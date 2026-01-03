/*
  CavRogue (Arduino/ESP32) — port of the Python prototype (v19).

  Target: ESP32 / ESP32-S3 + Nokia 5110 (PCD8544) + 6 buttons
  Display: 84x48 monochrome

  What’s included in this version:
  - Menus: Title / Slots / Scores / Story
  - New game: seed (random/custom) + difficulty
  - Core gameplay: cave generation, enemies, bullets, pause, game over, save slots/scores (NVS Preferences)
  - Animations: Intro / GameOver / Ending (NORMAL+EASY) / Ending (HARD)

  IMPORTANT
  - Button inputs are ACTIVE LOW with INPUT_PULLUP.
  - Change the GPIO pin mapping below to match your wiring.
*/

#include <Arduino.h>
#include <Preferences.h>

// Sprites (Arduino C++ headers)
#include "sprite_types.h"
#include "sprites_player.h"
#include "sprites_enemies.h"

#include "anim_intro.h"
#include "anim_gameover.h"
#include "anim_story.h"
#include "anim_ending_normaleasy.h"
#undef CAVROGUE_ENDING_FRAMES
#include "anim_ending_hard.h"

// =======================
// Pins (CHANGE THESE)
// =======================
// Nokia 5110 (PCD8544) pins (software SPI / bitbang)
#ifndef PIN_PCD8544_DIN
  #define PIN_PCD8544_DIN  23
#endif
#ifndef PIN_PCD8544_CLK
  #define PIN_PCD8544_CLK  18
#endif
#ifndef PIN_PCD8544_DC
  #define PIN_PCD8544_DC    2
#endif
#ifndef PIN_PCD8544_CE
  #define PIN_PCD8544_CE    5
#endif
#ifndef PIN_PCD8544_RST
  #define PIN_PCD8544_RST   4
#endif

// Buttons (active LOW)
#ifndef BTN_LEFT
  #define BTN_LEFT   32
#endif
#ifndef BTN_RIGHT
  #define BTN_RIGHT  33
#endif
#ifndef BTN_UP
  #define BTN_UP     25
#endif
#ifndef BTN_DOWN
  #define BTN_DOWN   26
#endif
#ifndef BTN_A
  #define BTN_A      27   // Shoot / Enter
#endif
#ifndef BTN_B
  #define BTN_B      14   // Back / Pause
#endif

// =======================
// Display: raw PCD8544 driver (bitbang)
// =======================
static constexpr int W = 84;
static constexpr int H = 48;

static inline void pcdPulseClock() {
  digitalWrite(PIN_PCD8544_CLK, HIGH);
  // a few NOPs helps with very fast MCUs
  __asm__ __volatile__("nop\nnop\nnop\n");
  digitalWrite(PIN_PCD8544_CLK, LOW);
}

static inline void pcdShiftOut(uint8_t b) {
  for (int i = 0; i < 8; i++) {
    digitalWrite(PIN_PCD8544_DIN, (b & 0x80) ? HIGH : LOW);
    pcdPulseClock();
    b <<= 1;
  }
}

static inline void pcdCommand(uint8_t c) {
  digitalWrite(PIN_PCD8544_DC, LOW);
  digitalWrite(PIN_PCD8544_CE, LOW);
  pcdShiftOut(c);
  digitalWrite(PIN_PCD8544_CE, HIGH);
}

static inline void pcdDataByte(uint8_t d) {
  digitalWrite(PIN_PCD8544_DC, HIGH);
  digitalWrite(PIN_PCD8544_CE, LOW);
  pcdShiftOut(d);
  digitalWrite(PIN_PCD8544_CE, HIGH);
}

static inline void pcdDataBlock(const uint8_t* data, size_t n) {
  digitalWrite(PIN_PCD8544_DC, HIGH);
  digitalWrite(PIN_PCD8544_CE, LOW);
  for (size_t i = 0; i < n; i++) pcdShiftOut(data[i]);
  digitalWrite(PIN_PCD8544_CE, HIGH);
}

static inline void pcdReset() {
  digitalWrite(PIN_PCD8544_RST, LOW);
  delay(10);
  digitalWrite(PIN_PCD8544_RST, HIGH);
}

static inline void pcdInit(uint8_t contrast = 55, bool invert = false) {
  pinMode(PIN_PCD8544_DIN, OUTPUT);
  pinMode(PIN_PCD8544_CLK, OUTPUT);
  pinMode(PIN_PCD8544_DC,  OUTPUT);
  pinMode(PIN_PCD8544_CE,  OUTPUT);
  pinMode(PIN_PCD8544_RST, OUTPUT);

  digitalWrite(PIN_PCD8544_CE, HIGH);
  digitalWrite(PIN_PCD8544_CLK, LOW);

  pcdReset();

  // Extended instruction set
  pcdCommand(0x21);                   // PD=0, V=0, H=1
  pcdCommand(0x80 | (contrast & 0x7F)); // Vop
  pcdCommand(0x04);                   // Temp coeff
  pcdCommand(0x14);                   // Bias mode 1:48
  // Basic instruction set
  pcdCommand(0x20);                   // H=0
  pcdCommand(invert ? 0x0D : 0x0C);   // Display control: inverse/normal
  // Clear
  pcdCommand(0x40);
  pcdCommand(0x80);
  for (int i = 0; i < 504; i++) pcdDataByte(0x00);
}

static inline void pcdShowFrame504(const uint8_t* frame504, bool invert = false) {
  // Set address to (0,0)
  pcdCommand(0x20);                 // basic
  pcdCommand(invert ? 0x0D : 0x0C); // inverse or normal
  pcdCommand(0x40);                 // y = 0
  pcdCommand(0x80);                 // x = 0
  pcdDataBlock(frame504, 504);
}

// =======================
// Simple text (5x7) renderer for PCD8544
// We use a tiny font table (subset) to keep sketch small.
// =======================
static uint8_t fb[504]; // frame buffer in RAM (PCD8544 native: 6 pages x 84 columns)

static inline void fbClear(uint8_t v=0x00){ memset(fb, v, sizeof(fb)); }

static inline void fbSetPixel(int x,int y,bool on){
  if (x<0||y<0||x>=W||y>=H) return;
  int page = y >> 3;
  int idx = page * W + x;
  uint8_t bit = uint8_t(1u << (y & 7));
  if (on) fb[idx] |= bit; else fb[idx] &= ~bit;
}


static inline bool fbGetPixel(int x,int y){
  if (x<0||y<0||x>=W||y>=H) return false;
  int page = y >> 3;
  int idx = page * W + x;
  uint8_t bit = uint8_t(1u << (y & 7));
  return (fb[idx] & bit) != 0;
}
static inline void fbXorPixel(int x,int y){
  fbSetPixel(x, y, !fbGetPixel(x,y));
}

static inline uint8_t RD(const uint8_t* p, int i) {
  return pgm_read_byte(p + i);
}

// XBM 1-bit (LSB-first), rowBytes = (w+7)/8
static void drawXbmXor(int x, int y, const uint8_t* data, int w, int h) {
  int rowBytes = (w + 7) / 8;
  for (int yy = 0; yy < h; yy++) {
    for (int xx = 0; xx < w; xx++) {
      int sx = x + xx;
      int sy = y + yy;
      if (sx < 0 || sy < 0 || sx >= W || sy >= H) continue;
      int byteIndex = yy * rowBytes + (xx >> 3);
      uint8_t b = RD(data, byteIndex);
      bool on = (b >> (xx & 7)) & 1;   // LSB first
      if (on) fbXorPixel(sx, sy);
    }
  }
}

static inline void drawSpriteXor(int x, int y, const SPR::Sprite& s) {
  drawXbmXor(x, y, s.data, s.w, s.h);
}


// Minimal 5x7 font for ASCII 32..90 (' ' to 'Z') (only what we need: digits, A-Z, '-' and some symbols)
static const uint8_t font5x7[][5] = {
  // 32 ' '
  {0x00,0x00,0x00,0x00,0x00},
  // 33 '!'
  {0x00,0x00,0x5F,0x00,0x00},
  // 34 '"'
  {0x00,0x07,0x00,0x07,0x00},
  // 35 '#'
  {0x14,0x7F,0x14,0x7F,0x14},
  // 36 '$'
  {0x24,0x2A,0x7F,0x2A,0x12},
  // 37 '%'
  {0x23,0x13,0x08,0x64,0x62},
  // 38 '&'
  {0x36,0x49,0x55,0x22,0x50},
  // 39 '''
  {0x00,0x05,0x03,0x00,0x00},
  // 40 '('
  {0x00,0x1C,0x22,0x41,0x00},
  // 41 ')'
  {0x00,0x41,0x22,0x1C,0x00},
  // 42 '*'
  {0x14,0x08,0x3E,0x08,0x14},
  // 43 '+'
  {0x08,0x08,0x3E,0x08,0x08},
  // 44 ','
  {0x00,0x50,0x30,0x00,0x00},
  // 45 '-'
  {0x08,0x08,0x08,0x08,0x08},
  // 46 '.'
  {0x00,0x60,0x60,0x00,0x00},
  // 47 '/'
  {0x20,0x10,0x08,0x04,0x02},
  // 48 '0'
  {0x3E,0x51,0x49,0x45,0x3E},
  // 49 '1'
  {0x00,0x42,0x7F,0x40,0x00},
  // 50 '2'
  {0x42,0x61,0x51,0x49,0x46},
  // 51 '3'
  {0x21,0x41,0x45,0x4B,0x31},
  // 52 '4'
  {0x18,0x14,0x12,0x7F,0x10},
  // 53 '5'
  {0x27,0x45,0x45,0x45,0x39},
  // 54 '6'
  {0x3C,0x4A,0x49,0x49,0x30},
  // 55 '7'
  {0x01,0x71,0x09,0x05,0x03},
  // 56 '8'
  {0x36,0x49,0x49,0x49,0x36},
  // 57 '9'
  {0x06,0x49,0x49,0x29,0x1E},
  // 58 ':'
  {0x00,0x36,0x36,0x00,0x00},
  // 59 ';'
  {0x00,0x56,0x36,0x00,0x00},
  // 60 '<'
  {0x08,0x14,0x22,0x41,0x00},
  // 61 '='
  {0x14,0x14,0x14,0x14,0x14},
  // 62 '>'
  {0x00,0x41,0x22,0x14,0x08},
  // 63 '?'
  {0x02,0x01,0x51,0x09,0x06},
  // 64 '@'
  {0x32,0x49,0x79,0x41,0x3E},
  // 65 'A'
  {0x7E,0x11,0x11,0x11,0x7E},
  // 66 'B'
  {0x7F,0x49,0x49,0x49,0x36},
  // 67 'C'
  {0x3E,0x41,0x41,0x41,0x22},
  // 68 'D'
  {0x7F,0x41,0x41,0x22,0x1C},
  // 69 'E'
  {0x7F,0x49,0x49,0x49,0x41},
  // 70 'F'
  {0x7F,0x09,0x09,0x09,0x01},
  // 71 'G'
  {0x3E,0x41,0x49,0x49,0x7A},
  // 72 'H'
  {0x7F,0x08,0x08,0x08,0x7F},
  // 73 'I'
  {0x00,0x41,0x7F,0x41,0x00},
  // 74 'J'
  {0x20,0x40,0x41,0x3F,0x01},
  // 75 'K'
  {0x7F,0x08,0x14,0x22,0x41},
  // 76 'L'
  {0x7F,0x40,0x40,0x40,0x40},
  // 77 'M'
  {0x7F,0x02,0x0C,0x02,0x7F},
  // 78 'N'
  {0x7F,0x04,0x08,0x10,0x7F},
  // 79 'O'
  {0x3E,0x41,0x41,0x41,0x3E},
  // 80 'P'
  {0x7F,0x09,0x09,0x09,0x06},
  // 81 'Q'
  {0x3E,0x41,0x51,0x21,0x5E},
  // 82 'R'
  {0x7F,0x09,0x19,0x29,0x46},
  // 83 'S'
  {0x46,0x49,0x49,0x49,0x31},
  // 84 'T'
  {0x01,0x01,0x7F,0x01,0x01},
  // 85 'U'
  {0x3F,0x40,0x40,0x40,0x3F},
  // 86 'V'
  {0x1F,0x20,0x40,0x20,0x1F},
  // 87 'W'
  {0x7F,0x20,0x18,0x20,0x7F},
  // 88 'X'
  {0x63,0x14,0x08,0x14,0x63},
  // 89 'Y'
  {0x03,0x04,0x78,0x04,0x03},
  // 90 'Z'
  {0x61,0x51,0x49,0x45,0x43},
};

static inline void fbChar(int x,int y,char c){
  if (c < 32 || c > 90) c = ' ';
  const uint8_t* col = font5x7[c-32];
  for(int cx=0; cx<5; cx++){
    uint8_t bits = col[cx];
    for(int cy=0; cy<7; cy++){
      bool on = (bits >> cy) & 1;
      fbSetPixel(x+cx, y+cy, on);
    }
  }
}
static inline void fbText(int x,int y,const char* s){
  int xx=x;
  for(int i=0; s[i] && xx < W-5; i++){
    char c = s[i];
    if (c>='a' && c<='z') c = char(c - 'a' + 'A');
    fbChar(xx, y, c);
    xx += 6;
  }
}
static inline void fbCenter(int y,const char* s){
  int len = (int)strlen(s);
  if (len > 14) len = 14;
  int wpx = len * 6;
  int x = (W - wpx) / 2;
  if (x < 0) x = 0;
  char tmp[16]; memset(tmp,0,sizeof(tmp));
  strncpy(tmp, s, len);
  fbText(x, y, tmp);
}

// Push our RAM framebuffer to the LCD
static inline void fbShow(bool invert=false){
  pcdShowFrame504(fb, invert);
}

// =======================
// Input (edge + hold)
// =======================
struct Buttons {
  uint8_t now=0, prev=0;
  static constexpr uint8_t L=1<<0, R=1<<1, U=1<<2, D=1<<3, A=1<<4, B=1<<5;

  void begin() {
    pinMode(BTN_LEFT,  INPUT_PULLUP);
    pinMode(BTN_RIGHT, INPUT_PULLUP);
    pinMode(BTN_UP,    INPUT_PULLUP);
    pinMode(BTN_DOWN,  INPUT_PULLUP);
    pinMode(BTN_A,     INPUT_PULLUP);
    pinMode(BTN_B,     INPUT_PULLUP);
  }

  void poll() {
    prev = now;
    now = 0;
    if (!digitalRead(BTN_LEFT))  now |= L;
    if (!digitalRead(BTN_RIGHT)) now |= R;
    if (!digitalRead(BTN_UP))    now |= U;
    if (!digitalRead(BTN_DOWN))  now |= D;
    if (!digitalRead(BTN_A))     now |= A;
    if (!digitalRead(BTN_B))     now |= B;
  }

  inline bool held(uint8_t m) const { return (now & m) != 0; }
  inline bool pressed(uint8_t m) const { return ((now & m) != 0) && ((prev & m) == 0); }
};
Buttons btn;

// =======================
// Save data (ESP32 NVS)
// =======================
Preferences prefs;

enum Difficulty : uint8_t { DIFF_EASY=0, DIFF_NORMAL=1, DIFF_HARD=2 };

struct SlotData {
  uint8_t used = 0;
  char name[4] = "AAA";
  uint8_t difficulty = DIFF_NORMAL;
  uint32_t seed = 0;
  uint16_t level = 1;
  int32_t score = 0;

  float cooldown_factor = 1.0f;
  uint16_t max_hp = 3;
  uint8_t score_boost = 0;
  uint8_t skip_next_level = 0;

  uint8_t enemy_arrow = 0;
  uint8_t pierce_forever = 0;
  uint8_t exit_half_kills = 0;
  uint8_t doubled_once = 0;

  // checkpoint
  uint16_t cp_level = 0;
  int32_t  cp_score = 0;
  float    cp_cooldown_factor = 1.0f;
  uint16_t cp_max_hp = 3;
  uint8_t  cp_score_boost = 0;
  uint8_t  cp_enemy_arrow = 0;
  uint8_t  cp_pierce_forever = 0;
  uint8_t  cp_exit_half_kills = 0;
  uint8_t  cp_doubled_once = 0;
};

struct ScoreEntry {
  char name[4] = "AAA";
  int32_t score = 0;
  uint8_t difficulty = DIFF_NORMAL;
  uint32_t seed = 0;
};

static constexpr int SLOT_COUNT = 3;
static constexpr int SCORE_COUNT = 10;

SlotData slots[SLOT_COUNT];
ScoreEntry scores[SCORE_COUNT];

static inline uint8_t clampU8(int v,int a,int b){ return (uint8_t)(v<a?a:(v>b?b:v)); }
template<typename T> static inline T clampT(T v, T a, T b) { return v < a ? a : (v > b ? b : v); }

// SplitMix32
static inline uint32_t splitmix32(uint32_t x) {
  x += 0x9E3779B9u;
  x = (x ^ (x >> 16)) * 0x85EBCA6Bu;
  x = (x ^ (x >> 13)) * 0xC2B2AE35u;
  x = (x ^ (x >> 16));
  return x;
}
static inline uint32_t level_seed(uint32_t run_seed, int level) {
  return splitmix32(run_seed ^ (uint32_t(level) * 0xA511E9B3u));
}

// xorshift32 rng
struct RNG {
  uint32_t s;
  explicit RNG(uint32_t seed=1) : s(seed ? seed : 1) {}
  inline uint32_t nextU32() {
    uint32_t x = s;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    s = x;
    return x;
  }
  inline int nextInt(int n) { return (n <= 0) ? 0 : int(nextU32() % uint32_t(n)); }
  inline int range(int a, int b_excl) { return a + nextInt(b_excl - a); }
  inline float nextF() { return (nextU32() & 0xFFFFFF) / float(0x1000000); }
  inline bool chance(float p) { return nextF() < p; }
};

static void loadSave() {
  prefs.begin("cavrogue", true);
  prefs.getBytes("slots", slots, sizeof(slots));
  prefs.getBytes("scores", scores, sizeof(scores));
  prefs.end();

  bool any = false;
  for (int i=0;i<SLOT_COUNT;i++) if (slots[i].used) any = true;
  if (!any) {
    for (int i=0;i<SLOT_COUNT;i++) {
      slots[i] = SlotData();
      slots[i].used = 0;
      strcpy(slots[i].name, "AAA");
      slots[i].difficulty = DIFF_NORMAL;
      slots[i].seed = 0;
      slots[i].level = 1;
      slots[i].score = 0;
    }
    for (int i=0;i<SCORE_COUNT;i++) scores[i] = ScoreEntry();

    prefs.begin("cavrogue", false);
    prefs.putBytes("slots", slots, sizeof(slots));
    prefs.putBytes("scores", scores, sizeof(scores));
    prefs.end();
  }
}
static void saveSlots() {
  prefs.begin("cavrogue", false);
  prefs.putBytes("slots", slots, sizeof(slots));
  prefs.end();
}
static void saveScores() {
  prefs.begin("cavrogue", false);
  prefs.putBytes("scores", scores, sizeof(scores));
  prefs.end();
}
static const char* diffShort(uint8_t d) {
  switch (d) {
    case DIFF_EASY: return "EASY";
    case DIFF_HARD: return "HARD";
    default: return "NORM";
  }
}
static void addScore(const SlotData& s) {
  ScoreEntry e;
  strncpy(e.name, s.name, 3); e.name[3]=0;
  e.score = s.score;
  e.difficulty = s.difficulty;
  e.seed = s.seed;

  int pos = SCORE_COUNT;
  for (int i=0;i<SCORE_COUNT;i++) {
    if (e.score > scores[i].score) { pos = i; break; }
  }
  if (pos >= SCORE_COUNT) return;
  for (int i=SCORE_COUNT-1;i>pos;i--) scores[i] = scores[i-1];
  scores[pos] = e;
  saveScores();
}

// =======================
// World grid (bitset) + cave generation (same idea as python)
// =======================
static constexpr int MAX_MAP_W = 1400;
static constexpr int MAX_MAP_H = 900;

struct GridBits {
  int w=0, h=0;
  uint8_t* bits=nullptr; // bit=1 => FLOOR
  size_t bytes=0;

  void freeMem() { if (bits) free(bits); bits=nullptr; w=h=0; bytes=0; }

  bool alloc(int Wm, int Hm) {
    freeMem();
    w=Wm; h=Hm;
    size_t nbits = size_t(w)*size_t(h);
    bytes = (nbits + 7)/8;
    bits = (uint8_t*)malloc(bytes);
    if (!bits) { freeMem(); return false; }
    memset(bits, 0, bytes);
    return true;
  }
  inline bool get(int x,int y) const {
    if (x<0||y<0||x>=w||y>=h) return false;
    size_t idx = size_t(y)*size_t(w) + size_t(x);
    return (bits[idx>>3] >> (idx & 7)) & 1;
  }
  inline void set(int x,int y,bool v) {
    if (x<0||y<0||x>=w||y>=h) return;
    size_t idx = size_t(y)*size_t(w) + size_t(x);
    uint8_t &b = bits[idx>>3];
    uint8_t m = uint8_t(1u << (idx & 7));
    if (v) b |= m; else b &= ~m;
  }
};

struct Pt { int16_t x=0,y=0; };

static inline uint32_t dist2i(int ax,int ay,int bx,int by){
  int dx=ax-bx, dy=ay-by;
  return uint32_t(dx*dx + dy*dy);
}

GridBits grid;
int map_w=0, map_h=0;
Pt startp, exitp;

static inline bool is_floor(int x,int y){ return grid.get(x,y); }

static void stamp_circle(GridBits& g, int cx, int cy, int r) {
  int r2 = r*r;
  int x0 = max(0, cx-r), x1 = min(g.w-1, cx+r);
  int y0 = max(0, cy-r), y1 = min(g.h-1, cy+r);
  for (int y=y0;y<=y1;y++){
    int dy = y-cy;
    int dy2 = dy*dy;
    for (int x=x0;x<=x1;x++){
      int dx = x-cx;
      if (dx*dx + dy2 <= r2) g.set(x,y,true);
    }
  }
}
static void carve_line(GridBits& g, int x0,int y0,int x1,int y1,int r){
  int steps = max(1, (abs(x1-x0)+abs(y1-y0))/8);
  for(int i=0;i<=steps;i++){
    int x = x0 + (x1-x0)*i/steps;
    int y = y0 + (y1-y0)*i/steps;
    stamp_circle(g, x, y, r);
  }
}
static void worm_connect(RNG& rng, int ax,int ay,int bx,int by,int margin,int w,int h,int step,
                         Pt* sample, int& sampleN, int sampleMax) {
  int x=ax, y=ay, tx=bx, ty=by;
  int dirx=0, diry=0;
  auto addSample=[&](int sx,int sy){
    if (sampleN < sampleMax) sample[sampleN++] = Pt{(int16_t)sx,(int16_t)sy};
    else sample[rng.nextInt(sampleMax)] = Pt{(int16_t)sx,(int16_t)sy};
  };

  addSample(x,y);
  for (int iter=0; iter<20000; iter++){
    if (abs(tx-x)+abs(ty-y) <= step) break;
    int dx = tx-x, dy = ty-y;
    bool wantX = abs(dx) >= abs(dy);

    if (rng.chance(0.58f)) {
      if (wantX) { dirx=0; diry = rng.chance(0.5f)?1:-1; }
      else       { diry=0; dirx = rng.chance(0.5f)?1:-1; }
    } else {
      if (wantX) { dirx = (dx>0)?1:-1; diry=0; }
      else       { diry = (dy>0)?1:-1; dirx=0; }
    }

    int run = rng.chance(0.55f) ? 1 : 2;
    for (int r=0;r<run;r++){
      x += dirx*step;
      y += diry*step;
      x = clampT(x, margin, w-1-margin);
      y = clampT(y, margin, h-1-margin);
      stamp_circle(g, x, y, rng.range(9, 16));
      if (rng.chance(0.08f)) stamp_circle(g, x, y, rng.range(16, 29));
      if ((iter & 7) == 0) addSample(x,y);
      if (abs(tx-x)+abs(ty-y) <= step) break;
    }
  }
  stamp_circle(g, tx, ty, 14);
  addSample(tx,ty);
}

static bool generate_cave(uint32_t run_seed, int level) {
  RNG rng(level_seed(run_seed, level));

  int map_step = min((level + 1)/2, 30);
  int target_w = W * map_step;
  int target_h = H * map_step;
  int w = clampT(target_w, W, MAX_MAP_W);
  int h = clampT(target_h, H, MAX_MAP_H);

  if (!grid.alloc(w,h)) return false;
  map_w=w; map_h=h;

  int ori = rng.nextInt(4);
  int margin = 14;
  if (ori == 0) { startp = { (int16_t)margin, (int16_t)rng.range(margin, h-margin) }; exitp = { (int16_t)(w-1-margin), (int16_t)rng.range(margin, h-margin) }; }
  if (ori == 1) { startp = { (int16_t)(w-1-margin), (int16_t)rng.range(margin, h-margin) }; exitp = { (int16_t)margin, (int16_t)rng.range(margin, h-margin) }; }
  if (ori == 2) { startp = { (int16_t)rng.range(margin, w-margin), (int16_t)margin }; exitp = { (int16_t)rng.range(margin, w-margin), (int16_t)(h-1-margin) }; }
  if (ori == 3) { startp = { (int16_t)rng.range(margin, w-margin), (int16_t)(h-1-margin) }; exitp = { (int16_t)rng.range(margin, w-margin), (int16_t)margin }; }

  int k = clampT(2 + level/8, 2, 8);
  Pt samples[600]; int sampleN=0;
  const int step = 8;

  stamp_circle(grid, startp.x, startp.y, 14);

  int prevx=startp.x, prevy=startp.y;
  for(int i=1;i<=k;i++){
    float t = float(i)/float(k);
    int mx = int(startp.x + (exitp.x-startp.x)*t);
    int my = int(startp.y + (exitp.y-startp.y)*t);
    int amp = clampT(int(14 + level*0.55f), 14, 90);
    if (abs(exitp.x-startp.x) >= abs(exitp.y-startp.y)) my += rng.range(-amp, amp+1);
    else mx += rng.range(-amp, amp+1);
    mx = clampT(mx, margin, w-1-margin);
    my = clampT(my, margin, h-1-margin);

    worm_connect(rng, prevx, prevy, mx, my, margin, w, h, step, samples, sampleN, 600);
    prevx=mx; prevy=my;
  }
  worm_connect(rng, prevx, prevy, exitp.x, exitp.y, margin, w, h, step, samples, sampleN, 600);

  int branches = clampT(3 + level/2, 3, 70);
  int branch_len = clampT(4 + level/10, 4, 26);
  for(int b=0;b<branches;b++){
    if (sampleN == 0) break;
    Pt bp = samples[rng.nextInt(sampleN)];
    int dirpick = rng.nextInt(4);
    int dirx = (dirpick==0)?1:(dirpick==1)?-1:0;
    int diry = (dirpick==2)?1:(dirpick==3)?-1:0;
    int cx=bp.x, cy=bp.y;
    for(int s=0;s<branch_len;s++){
      if (rng.chance(0.64f)) {
        if (dirx!=0){ dirx=0; diry = rng.chance(0.5f)?1:-1; }
        else { diry=0; dirx = rng.chance(0.5f)?1:-1; }
      }
      cx += dirx*step;
      cy += diry*step;
      cx = clampT(cx, margin, w-1-margin);
      cy = clampT(cy, margin, h-1-margin);
      stamp_circle(grid, cx, cy, rng.range(9, 16));
    }
    stamp_circle(grid, cx, cy, rng.range(14, 31));
  }

  carve_line(grid, startp.x, startp.y, exitp.x, exitp.y, 14);
  stamp_circle(grid, exitp.x, exitp.y, 14);
  return true;
}

// =======================
// Gameplay tuning
// =======================
static constexpr int ACTIVE_MARGIN = 24;
static constexpr float PLAYER_BASE_COOLDOWN = 2.0f;
static constexpr float PLAYER_COOLDOWN_MIN  = 0.22f;
static constexpr float PLAYER_IFRAME_SEC    = 3.0f;

static constexpr int AGGRO_R_12 = 26;
static constexpr int AGGRO_R_34 = 48;

static const uint8_t ENEMY_TICKS_NORMAL[5] = {0,12,8,14,8};
static const uint8_t ENEMY_TICKS_AGGRO [5] = {0,6,4,12,4};
static const uint8_t ENEMY_HP         [5] = {0,1,1,2,5};
static const int SCORE_KILL           [5] = {0,10,18,45,120};

static inline void enemy_bounds(uint8_t t, int& w, int& h){
  // Visual sprites: 1..3 are 8x8, 4 is 16x16 (matches sprites_enemies.h)
  if (t==4){ w=16; h=16; return; }
  w=8; h=8;
}

// =======================
// Entities
// =======================
struct Bullet {
  int16_t x=0,y=0;
  int8_t dx=0,dy=0;
  int16_t owner_id=0;
  int8_t damage=1;
  uint8_t alive=1;
};

struct Enemy {
  int16_t eid=0;
  uint8_t etype=1;
  int16_t x=0,y=0;
  int8_t hp=1;
  uint16_t move_tick=0;
  int8_t dirx=0, diry=0;
  int8_t step_left=0;
  int8_t pause_left=0;
  float shoot_timer=0;
  int8_t burst_left=0;
  float burst_delay=0;
  int8_t burst_dx=0, burst_dy=0;
};

static constexpr int MAX_ENEMIES = 180;
static constexpr int MAX_BULLETS = 96;
Enemy enemies[MAX_ENEMIES]; int enemyCount=0; int nextEid=1;
Bullet bullets[MAX_BULLETS]; int bulletCount=0;

static bool can_place_rect(int x,int y,int w,int h){
  for(int yy=0;yy<h;yy++) for(int xx=0;xx<w;xx++){
    if (!is_floor(x+xx,y+yy)) return false;
  }
  return true;
}

static inline void reset_enemy_shoot(Enemy& e, RNG& rng){
  float a=2.0f,b=2.0f;
  if (e.etype==1){a=b=2.8f;}
  else if (e.etype==2){a=1.0f;b=1.6f;}
  else if (e.etype==3){a=2.1f;b=2.7f;}
  else {a=2.6f;b=3.2f;}
  e.shoot_timer = (a==b)?a:(a + rng.nextF()*(b-a));
}

static inline uint8_t pick_enemy_type(RNG& rng, int L){
  int w1=140;
  int w2=clampT((L-4)*7, 0, 140);
  int w3=clampT((L-18)*6,0,110);
  int w4=clampT((L-45)*5,0,90);
  int total=w1+w2+w3+w4;
  int r=rng.nextInt(total);
  if (r < w1) return 1;
  r-=w1; if (r < w2) return 2;
  r-=w2; if (r < w3) return 3;
  return 4;
}

// =======================
// Game state
// =======================
enum GameState : uint8_t {
  ST_TITLE,
  ST_SLOTS,
  ST_SCORES,
  ST_STORY,
  ST_NEW_SEEDMENU,
  ST_NEW_CODE,
  ST_NEW_DIFF,
  ST_CONFIRM_ERASE,
  ST_BONUS,
  ST_PLAY,
  ST_PAUSE,
  ST_GAME_OVER
};

GameState state = ST_TITLE;
uint8_t sel = 0;
uint8_t slotIndex = 0;
SlotData cur;

// new game flow
uint8_t seedSel = 0; // 0 random, 1 custom
char newCode[9] = "00000000";
uint8_t codePos = 0;
uint8_t diffSel = 1;

// runtime
int16_t player_x=0, player_y=0;
int16_t player_hp=3;
bool exit_unlocked=true;
int enemies_total=0;
int kills_this_level=0;

// timers / scoring
uint32_t lastShotMs=0;
uint32_t iframesUntilMs=0;
uint32_t levelStartMs=0;
int shotsFired=0;
int damageTaken=0;

static inline float currentCooldownSec() {
  float cd = PLAYER_BASE_COOLDOWN * cur.cooldown_factor;
  if (cd < PLAYER_COOLDOWN_MIN) cd = PLAYER_COOLDOWN_MIN;
  return cd;
}

static void writeCheckpoint() {
  cur.cp_level = cur.level;
  cur.cp_score = cur.score;
  cur.cp_cooldown_factor = cur.cooldown_factor;
  cur.cp_max_hp = cur.max_hp;
  cur.cp_score_boost = cur.score_boost;
  cur.cp_enemy_arrow = cur.enemy_arrow;
  cur.cp_pierce_forever = cur.pierce_forever;
  cur.cp_exit_half_kills = cur.exit_half_kills;
  cur.cp_doubled_once = cur.doubled_once;
}
static void restoreFromCheckpoint() {
  if (cur.cp_level == 0) {
    cur.level = 1; cur.score = 0; cur.cooldown_factor = 1.0f; cur.max_hp = 3;
    cur.score_boost = 0; cur.skip_next_level=0; cur.enemy_arrow=0; cur.pierce_forever=0; cur.exit_half_kills=0; cur.doubled_once=0;
  } else {
    cur.level = cur.cp_level; cur.score = cur.cp_score; cur.cooldown_factor = cur.cp_cooldown_factor; cur.max_hp = cur.cp_max_hp;
    cur.score_boost = cur.cp_score_boost; cur.enemy_arrow = cur.cp_enemy_arrow; cur.pierce_forever = cur.cp_pierce_forever;
    cur.exit_half_kills = cur.cp_exit_half_kills; cur.doubled_once = cur.cp_doubled_once;
    cur.skip_next_level=0;
  }
}
static void saveCurSlot() { slots[slotIndex] = cur; saveSlots(); }

// =======================
// Animations (blocking player, skippable with A/B)
// =======================
static void playAnimFrames(const uint8_t (*frames)[504], int nFrames, uint16_t msPerFrame, bool skippable=true) {
  uint32_t next = millis();
  for (int i=0; i<nFrames; i++) {
    // allow skipping
    btn.poll();
    if (skippable && (btn.pressed(Buttons::A) || btn.pressed(Buttons::B))) break;

    pcdShowFrame504(frames[i], false); // animations are generated for normal (non-inverted) display
    next += msPerFrame;
    while ((int32_t)(millis() - next) < 0) {
      btn.poll();
      if (skippable && (btn.pressed(Buttons::A) || btn.pressed(Buttons::B))) return;
      delay(1);
    }
  }
}

// =======================
// Level load & entity placement
// =======================
static void clearEntities() {
  enemyCount=0; bulletCount=0; nextEid=1; enemies_total=0; kills_this_level=0;
}

static void placeEnemies(int count) {
  clearEntities();
  RNG rng(level_seed(cur.seed, cur.level) ^ 0x13579BDFu);

  const int R_spawn = 70;
  const int R_exit  = 70;
  const int R_min   = 44;

  int tries_limit = 20000;
  int attempts=0, placed=0;
  int e4_cap = 1 + cur.level/25;
  int e4_count=0;

  while (placed < count && attempts < tries_limit && enemyCount < MAX_ENEMIES) {
    attempts++;
    uint8_t et = pick_enemy_type(rng, cur.level);
    if (et==4 && e4_count>=e4_cap) et = uint8_t(1 + rng.nextInt(3));

    int bw,bh; enemy_bounds(et,bw,bh);
    int x = rng.nextInt(max(1, map_w - bw - 2));
    int y = rng.nextInt(max(1, map_h - bh - 2));

    if (dist2i(x,y,startp.x,startp.y) < uint32_t(R_spawn*R_spawn)) continue;
    if (dist2i(x,y,exitp.x,exitp.y)  < uint32_t(R_exit*R_exit)) continue;

    bool ok=true;
    for (int i=0;i<enemyCount;i++){
      if (dist2i(x,y,enemies[i].x,enemies[i].y) < uint32_t(R_min*R_min)) { ok=false; break; }
    }
    if (!ok) continue;
    if (!can_place_rect(x,y,bw,bh)) continue;

    Enemy& e = enemies[enemyCount++];
    e.eid = nextEid++;
    e.etype = et;
    e.x = x; e.y = y;
    e.hp = ENEMY_HP[et];
    e.move_tick = 0;
    e.dirx = e.diry = 0;
    e.step_left = 0;
    e.pause_left=0;
    e.burst_left=0;
    e.burst_delay=0;
    reset_enemy_shoot(e, rng);

    placed++;
    if (et==4) e4_count++;
  }
  enemies_total = enemyCount;
}

static void loadLevel(int level) {
  cur.level = level;
  player_hp = cur.max_hp;
  iframesUntilMs = 0;
  exit_unlocked = (cur.difficulty == DIFF_EASY);

  generate_cave(cur.seed, cur.level);

  player_x = startp.x; player_y = startp.y;
  lastShotMs=0;

  int enemy_count = clampT(level, 1, 180);
  placeEnemies(enemy_count);

  // In Python version: EASY/NORMAL auto checkpoint at start of each level
  if (cur.difficulty == DIFF_EASY || cur.difficulty == DIFF_NORMAL) {
    writeCheckpoint();
  }

  if (cur.difficulty == DIFF_EASY || cur.difficulty == DIFF_NORMAL) writeCheckpoint();
  saveCurSlot();

  levelStartMs = millis();
  shotsFired = 0;
  damageTaken = 0;
}

// =======================
// Rendering (game) in our RAM framebuffer
// =======================
int cam_x=0, cam_y=0;

static inline void xorFromWorld(int sx,int sy) {
  int wx = cam_x + sx;
  int wy = cam_y + sy;
  bool bgIsFloor = is_floor(wx,wy);
  fbSetPixel(sx, sy, bgIsFloor ? true : false);
}


// -----------------------
// Arrows (same patterns as Python: 4x3)
// -----------------------
static const int8_t ARROW_R_OFFS[][2] = {{2,0},{0,1},{1,1},{2,1},{3,1},{2,2}};
static const int8_t ARROW_L_OFFS[][2] = {{1,0},{0,1},{1,1},{2,1},{3,1},{1,2}};
static const int8_t ARROW_U_OFFS[][2] = {{2,0},{1,1},{2,1},{3,1},{0,2},{1,2},{2,2},{3,2}};
static const int8_t ARROW_D_OFFS[][2] = {{0,0},{1,0},{2,0},{3,0},{1,1},{2,1},{3,1},{2,2}};
static constexpr uint8_t ARROW_W = 4;
static constexpr uint8_t ARROW_H = 3;

static void renderExitArrow() {
  if (!exit_unlocked) return;
  int dx = exitp.x - player_x;
  int dy = exitp.y - player_y;

  const int8_t (*offs)[2] = ARROW_R_OFFS;
  uint8_t n = sizeof(ARROW_R_OFFS)/sizeof(ARROW_R_OFFS[0]);
  int sx0=1, sy0=1;

  if (abs(dx) >= abs(dy)) {
    if (dx > 0) { offs = ARROW_R_OFFS; n = sizeof(ARROW_R_OFFS)/sizeof(ARROW_R_OFFS[0]); sx0 = W - ARROW_W - 1; }
    else        { offs = ARROW_L_OFFS; n = sizeof(ARROW_L_OFFS)/sizeof(ARROW_L_OFFS[0]); sx0 = 1; }
    sy0 = H/2 - ARROW_H/2;
  } else {
    if (dy > 0) { offs = ARROW_D_OFFS; n = sizeof(ARROW_D_OFFS)/sizeof(ARROW_D_OFFS[0]); sy0 = H - ARROW_H - 1; }
    else        { offs = ARROW_U_OFFS; n = sizeof(ARROW_U_OFFS)/sizeof(ARROW_U_OFFS[0]); sy0 = 1; }
    sx0 = W/2 - ARROW_W/2;
  }

  for (uint8_t i=0;i<n;i++){
    xorFromWorld(sx0 + offs[i][0], sy0 + offs[i][1]);
  }
}

static void renderEnemyArrow() {
  if (!cur.enemy_arrow) return;

  int bestIdx = -1;
  int32_t bestD = 0x7fffffff;
  for (int i=0;i<enemyCount;i++){
    if (enemies[i].hp <= 0) continue;
    int dx = enemies[i].x - player_x;
    int dy = enemies[i].y - player_y;
    int32_t d = int32_t(dx)*dx + int32_t(dy)*dy;
    if (d < bestD) { bestD = d; bestIdx = i; }
  }
  if (bestIdx < 0) return;

  int dx = enemies[bestIdx].x - player_x;
  int dy = enemies[bestIdx].y - player_y;

  const int8_t (*offs)[2] = ARROW_R_OFFS;
  uint8_t n = sizeof(ARROW_R_OFFS)/sizeof(ARROW_R_OFFS[0]);
  int sx0=1, sy0=1;

  if (abs(dx) >= abs(dy)) {
    offs = (dx > 0) ? ARROW_R_OFFS : ARROW_L_OFFS;
    n = (dx > 0) ? sizeof(ARROW_R_OFFS)/sizeof(ARROW_R_OFFS[0]) : sizeof(ARROW_L_OFFS)/sizeof(ARROW_L_OFFS[0]);
    sx0 = (dx > 0) ? (W - ARROW_W - 1) : 1;
    sy0 = 1;
  } else {
    offs = (dy > 0) ? ARROW_D_OFFS : ARROW_U_OFFS;
    n = (dy > 0) ? sizeof(ARROW_D_OFFS)/sizeof(ARROW_D_OFFS[0]) : sizeof(ARROW_U_OFFS)/sizeof(ARROW_U_OFFS[0]);
    sy0 = (dy > 0) ? (H - ARROW_H - 1) : 1;
    sx0 = 1;
  }

  for (uint8_t i=0;i<n;i++){
    xorFromWorld(sx0 + offs[i][0], sy0 + offs[i][1]);
  }
}

static void renderPlay() {
  cam_x = player_x - W/2;
  cam_y = player_y - H/2;
  cam_x = clampT(cam_x, 0, max(0, map_w - W));
  cam_y = clampT(cam_y, 0, max(0, map_h - H));

  fbClear(0x00);

  // background: walls as black pixels
  for (int sy=0; sy<H; sy++){
    int wy = cam_y + sy;
    for (int sx=0; sx<W; sx++){
      int wx = cam_x + sx;
      if (!is_floor(wx,wy)) fbSetPixel(sx, sy, true);
    }
  }

  // exit: just a box marker (you can replace with your sprite)
  int ex0 = exitp.x - 3;
  int ey0 = exitp.y - 3;
  for(int yy=0;yy<6;yy++) for(int xx=0;xx<7;xx++){
    int sx = ex0 + xx - cam_x;
    int sy = ey0 + yy - cam_y;
    if (sx>=0&&sx<W&&sy>=0&&sy<H) xorFromWorld(sx,sy);
  }

  // enemies: sprites (XOR)
  for (int i=0;i<enemyCount;i++){
    const Enemy& e = enemies[i];
    uint8_t sid = SPR::E_PING;
    if (e.etype==2) sid = SPR::E_SCAN;
    else if (e.etype==3) sid = SPR::E_DAEMON;
    else if (e.etype==4) sid = SPR::E_FIREWALL;

    int sx = e.x - cam_x;
    int sy = e.y - cam_y;
    drawSpriteXor(sx, sy, SPR::ENEMY[sid]);
  }

  // bullets
  for (int i=0;i<bulletCount;i++){
    const Bullet& b = bullets[i];
    if (!b.alive) continue;
    int sx = b.x - cam_x;
    int sy = b.y - cam_y;
    if (sx>=0&&sx<W&&sy>=0&&sy<H) xorFromWorld(sx,sy);
  }

  // player: sprite (XOR) — collision stays 2x2, this is visual only
  uint8_t pid = SPR::P_IDLE;
  if (btn.held(Buttons::L|Buttons::R|Buttons::U|Buttons::D)) {
    pid = ((millis()/200) & 1) ? SPR::P_WALK1 : SPR::P_WALK2;
  }
  int cx = (player_x + 1) - cam_x; // center of 2x2 hitbox
  int cy = (player_y + 1) - cam_y;
  int psx = cx - (int)SPR::PLAYER[pid].w/2;
  int psy = cy - (int)SPR::PLAYER[pid].h/2;
  drawSpriteXor(psx, psy, SPR::PLAYER[pid]);

  // arrows (same behavior as Python)
  renderExitArrow();
  renderEnemyArrow();
}


static void renderTitle() {
  fbClear(0x00);
  fbCenter(0, "CAVROGUE");
  const char* items[3] = {"JOUER","SCORES","STORY"};
  for(int i=0;i<3;i++){
    char line[18];
    snprintf(line, sizeof(line), "%s%s", (sel==i)?"> ":"  ", items[i]);
    fbCenter(14 + i*10, line);
  }
  fbText(0, 40, "A=OK");
}

static void renderSlots() {
  fbClear(0x00);
  fbCenter(0, "SLOTS");
  for(int i=0;i<3;i++){
    const SlotData& s = slots[i];
    char line[20];
    if (!s.used) snprintf(line, sizeof(line), "%sVIDE", (sel==i)?"> ":"  ");
    else snprintf(line, sizeof(line), "%s%.3s %s", (sel==i)?"> ":"  ", s.name, diffShort(s.difficulty));
    fbText(0, 14 + i*10, line);
  }
  fbText(0, 40, "A=OK B=RET");
}

static void renderScores() {
  fbClear(0x00);
  fbCenter(0, "TOP SCORES");
  int off = (int)sel;
  off = clampT(off, 0, SCORE_COUNT-4);
  for(int i=0;i<4;i++){
    const ScoreEntry& e = scores[off+i];
    char line[20];
    snprintf(line, sizeof(line), "%d.%.3s %ld", off+i+1, e.name, (long)e.score);
    fbText(0, 10 + i*9, line);
  }
  fbText(0, 40, "B=RET");
}

static int storyLine = 0;
static void renderStory() {
  fbClear(0x00);
  fbCenter(0, "STORY");
  // show 5 lines per page (y=10..)
  for(int i=0;i<5;i++){
    int idx = storyLine + i;
    if (idx >= STORY_COUNT) break;
    fbText(0, 10 + i*8, STORY_LINES[idx]);
  }
  char footer[20];
  snprintf(footer, sizeof(footer), "%d/%d", storyLine+1, STORY_COUNT);
  fbText(60, 40, footer);
}


// =======================
// Bonus system (same rules/order as Python v19)
// =======================
enum BonusKind : uint8_t {
  B_UNQ_ENEMY_ARROW,
  B_UNQ_PIERCE,
  B_UNQ_EXIT_HALF,
  B_UNQ_SCORE_X2,
  B_HP_PLUS3,
  B_CADENCE,
  B_SCORE_BOOST,
  B_SKIP_LEVEL,
  B_SAVE_CP
};

struct BonusOpt { BonusKind kind; const char* label; };

static bool hasUniqueAvailable(BonusKind &outKind, const char* &outLabel) {
  // order is important (matches Python)
  if (cur.level >= 10 && !cur.enemy_arrow && cur.difficulty != DIFF_EASY) {
    outKind = B_UNQ_ENEMY_ARROW; outLabel = "UNQ:FLECHE ENN"; return true;
  }
  if (cur.level >= 20 && !cur.pierce_forever) {
    outKind = B_UNQ_PIERCE; outLabel = "UNQ:BALLES PERCE"; return true;
  }
  if (cur.level >= 30 && !cur.exit_half_kills && cur.difficulty != DIFF_EASY) {
    outKind = B_UNQ_EXIT_HALF; outLabel = "UNQ:SORTIE 50%"; return true;
  }
  if (cur.level >= 40 && !cur.doubled_once) {
    outKind = B_UNQ_SCORE_X2; outLabel = "UNQ:SCORE X2"; return true;
  }
  return false;
}

static int buildBonusOptions(BonusOpt *out, int maxOut) {
  int n = 0;
  BonusKind uk; const char* ul;
  if (hasUniqueAvailable(uk, ul) && n < maxOut) out[n++] = { uk, ul };

  if (n < maxOut) out[n++] = { B_HP_PLUS3, "PV +3 MAX" };

  // cadence only if still improvable
  if (currentCooldownSec() > (PLAYER_COOLDOWN_MIN + 0.05f) && n < maxOut) {
    out[n++] = { B_CADENCE, "CADENCE X2/3" };
  }

  if (n < maxOut) out[n++] = { B_SCORE_BOOST, "SCORE +50%" };
  if (n < maxOut) out[n++] = { B_SKIP_LEVEL, "SAUTER 1 NIV" };

  if (cur.difficulty == DIFF_HARD && n < maxOut) {
    out[n++] = { B_SAVE_CP, "SAUVEGARDER" };
  }

  return n;
}

static int bonusCount() {
  BonusOpt tmp[8];
  return buildBonusOptions(tmp, 8);
}

static void applyBonusChoice(int idx) {
  BonusOpt opts[8];
  int n = buildBonusOptions(opts, 8);
  if (n <= 0) return;
  idx = (idx % n + n) % n;

  switch (opts[idx].kind) {
    case B_UNQ_ENEMY_ARROW: cur.enemy_arrow = 1; break;
    case B_UNQ_PIERCE:      cur.pierce_forever = 1; break;
    case B_UNQ_EXIT_HALF:   cur.exit_half_kills = 1; break;
    case B_UNQ_SCORE_X2:    cur.score *= 2; cur.doubled_once = 1; break;
    case B_HP_PLUS3:        cur.max_hp = (uint16_t)clampT<int>(cur.max_hp + 3, 3, 300); break;
    case B_CADENCE: {
      // matches Python: factor = max(0.12, factor * 2/3)
      float f = cur.cooldown_factor * (2.0f/3.0f);
      if (f < 0.12f) f = 0.12f;
      cur.cooldown_factor = f;
    } break;
    case B_SCORE_BOOST:     cur.score_boost = 1; break;
    case B_SKIP_LEVEL:      cur.skip_next_level = 1; break;
    case B_SAVE_CP:         writeCheckpoint(); break;
  }
}


static void renderBonus() {
  fbClear(0x00);
  char t[18];
  snprintf(t, sizeof(t), "BONUS N%d", (int)cur.level);
  fbCenter(0, t);

  BonusOpt opts[8];
  int n = buildBonusOptions(opts, 8);
  if (n <= 0) { fbCenter(16, "AUCUN"); return; }

  // show 4 lines with scrolling window
  int window = 4;
  int start = 0;
  if (n > window) {
    if (sel >= (uint8_t)window) start = sel - (window-1);
    if (start > n - window) start = n - window;
  }
  int y = 8;
  for (int i=0;i<window;i++){
    int idx = start + i;
    if (idx >= n) break;
    char line[18];
    snprintf(line, sizeof(line), "%c%.14s", (idx == sel)?'>':' ', opts[idx].label);
    fbText(0, y, line);
    y += 8;
  }
}

static void renderPause() {
  fbClear(0x00);
  fbCenter(0, "PAUSE");
  char l1[18], l2[18], l3[18];
  snprintf(l1, sizeof(l1), "%s S%ld", diffShort(cur.difficulty), (long)cur.score);
  snprintf(l2, sizeof(l2), "N%d HP%d/%d", (int)cur.level, (int)player_hp, (int)cur.max_hp);

  // alive enemies count
  int alive = 0;
  for (int i=0;i<enemyCount;i++) if (enemies[i].hp > 0) alive++;
  snprintf(l3, sizeof(l3), "E%d/%d", alive, enemies_total);

  fbText(0, 8, l1);
  fbText(0, 16, l2);
  fbText(0, 24, l3);

  fbCenter(32, (sel==0)?"> REPR":"  REPR");
  fbCenter(40, (sel==1)?"> MENU":"  MENU");
}


static void renderGameOverMenu() {
  fbClear(0x00);
  fbCenter(0, "GAME OVER");
  char b1[18], b2[18];
  snprintf(b1, sizeof(b1), "SCOR %ld", (long)cur.score);
  snprintf(b2, sizeof(b2), "NIV %d %s", cur.level, diffShort(cur.difficulty));
  fbCenter(12, b1);
  fbCenter(20, b2);
  fbCenter(32, (sel==0)?"> CONT":"  CONT");
  fbCenter(40, (sel==1)?"> QUIT":"  QUIT");
}

// =======================
// Gameplay update
// =======================
static inline bool playerInExit() {
  int ex0 = exitp.x - 3, ey0 = exitp.y - 3;
  int px = player_x, py = player_y;
  int pw=2, ph=2;
  return !(px+pw < ex0 || px > ex0+7 || py+ph < ey0 || py > ey0+6);
}

static void damagePlayer(int dmg) {
  uint32_t now = millis();
  if (now < iframesUntilMs) return;
  damageTaken += dmg;
  player_hp -= dmg;
  iframesUntilMs = now + uint32_t(PLAYER_IFRAME_SEC * 1000.0f);
}

static void tryMovePlayer(int dx,int dy) {
  int nx=player_x+dx, ny=player_y+dy;
  if (can_place_rect(nx,ny,2,2)) { player_x=nx; player_y=ny; }
}

static void shootPlayer() {
  uint32_t now = millis();
  uint32_t cdMs = uint32_t(currentCooldownSec() * 1000.0f);
  if (now - lastShotMs < cdMs) return;
  lastShotMs = now;
  shotsFired++;

  int dx=0,dy=0;
  if (btn.held(Buttons::L)) dx=-1;
  else if (btn.held(Buttons::R)) dx=1;
  else if (btn.held(Buttons::U)) dy=-1;
  else if (btn.held(Buttons::D)) dy=1;
  else dx=1;

  if (bulletCount >= MAX_BULLETS) return;
  Bullet& b = bullets[bulletCount++];
  b.x = player_x + 1;
  b.y = player_y + 1;
  b.dx = dx; b.dy = dy;
  b.owner_id = 0;
  b.damage = 1;
  b.alive = 1;
}

static bool enemyActive(const Enemy& e) {
  int rx = (player_x - W/2) - ACTIVE_MARGIN;
  int ry = (player_y - H/2) - ACTIVE_MARGIN;
  int rw = W + 2*ACTIVE_MARGIN;
  int rh = H + 2*ACTIVE_MARGIN;
  int bw,bh; enemy_bounds(e.etype,bw,bh);
  return !(e.x + bw <= rx || rx + rw <= e.x || e.y + bh <= ry || ry + rh <= e.y);
}

static void enemyShootSingle(const Enemy& e, int dx,int dy, int dmg=1) {
  if (bulletCount >= MAX_BULLETS) return;
  int bw,bh; enemy_bounds(e.etype,bw,bh);
  Bullet& b = bullets[bulletCount++];
  b.x = e.x + bw/2;
  b.y = e.y + bh/2;
  b.dx = dx; b.dy = dy;
  b.owner_id = e.eid;
  b.damage = dmg;
  b.alive = 1;
}

static void startEnemyFire(Enemy& e, RNG& rng) {
  int dx = player_x - e.x;
  int dy = player_y - e.y;
  int adx = abs(dx), ady = abs(dy);
  int sx=0, sy=0;
  if (adx >= ady) { sx = (dx>0)?1:-1; sy=0; }
  else { sy = (dy>0)?1:-1; sx=0; }

  if (e.etype == 4) {
    enemyShootSingle(e, 1,0,2);
    enemyShootSingle(e,-1,0,2);
    enemyShootSingle(e, 0,1,2);
    enemyShootSingle(e, 0,-1,2);
    reset_enemy_shoot(e, rng);
    return;
  }
  if (e.etype == 3) {
    e.burst_left = 3;
    e.burst_delay = 0;
    e.burst_dx = sx; e.burst_dy = sy;
    reset_enemy_shoot(e, rng);
    return;
  }
  enemyShootSingle(e, sx, sy, 1);
  reset_enemy_shoot(e, rng);
}

static void updateEnemies(float dt) {
  RNG rng(0xC0FFEEu ^ (cur.seed) ^ uint32_t(cur.level*1337));
  for(int i=0;i<enemyCount;i++){
    Enemy& e = enemies[i];
    if (e.hp<=0) continue;
    if (!enemyActive(e)) continue;

    e.move_tick++;

    if (e.etype==3 && e.burst_left>0) {
      e.burst_delay -= dt;
      if (e.burst_delay <= 0.0f) {
        int dx=e.burst_dx, dy=e.burst_dy;
        enemyShootSingle(e,dx,dy,1);
        e.burst_left--;
        e.burst_delay = 0.12f;
      }
    }

    uint32_t d2p = dist2i(e.x,e.y, player_x, player_y);
    bool aggro12 = d2p <= uint32_t(AGGRO_R_12*AGGRO_R_12);
    bool aggro34 = d2p <= uint32_t(AGGRO_R_34*AGGRO_R_34);

    if (e.step_left <= 0) {
      auto chooseAxis=[&](){
        int dx = player_x - e.x;
        int dy = player_y - e.y;
        if (abs(dx) >= abs(dy)) { e.dirx = (dx>0)?1:-1; e.diry=0; }
        else { e.diry = (dy>0)?1:-1; e.dirx=0; }
      };
      if ((e.etype==1 || e.etype==2) && aggro12) {
        chooseAxis(); e.step_left = rng.range(7,17); e.pause_left = rng.range(0,2);
      } else if ((e.etype==3 || e.etype==4) && aggro34) {
        chooseAxis(); e.step_left = rng.range(3,8); e.pause_left = rng.range(0,4);
      } else {
        int d = rng.nextInt(4);
        e.dirx = (d==0)?1:(d==1)?-1:0;
        e.diry = (d==2)?1:(d==3)?-1:0;
        e.step_left = rng.range(2,8); e.pause_left = rng.range(0,9);
      }
    }

    if (e.pause_left > 0) e.pause_left--;
    else {
      bool ag = ((e.etype==1||e.etype==2)?aggro12:aggro34);
      uint8_t ticks = ag ? ENEMY_TICKS_AGGRO[e.etype] : ENEMY_TICKS_NORMAL[e.etype];
      if (ticks && (e.move_tick % ticks)==0) {
        int bw,bh; enemy_bounds(e.etype,bw,bh);
        int nx = e.x + e.dirx;
        int ny = e.y + e.diry;
        if (can_place_rect(nx,ny,bw,bh)) { e.x=nx; e.y=ny; e.step_left--; }
        else { e.step_left=0; e.pause_left = rng.range(1,9); }
      }
    }

    e.shoot_timer -= dt;
    bool canShoot = ((e.etype==1||e.etype==2)?aggro12:aggro34);
    if (canShoot && e.shoot_timer <= 0.0f) startEnemyFire(e, rng);
  }
}

static void updateBullets() {
  for(int i=0;i<bulletCount;i++){
    Bullet& b = bullets[i];
    if (!b.alive) continue;

    b.x += b.dx; b.y += b.dy;

    if (!is_floor(b.x,b.y)) { b.alive=0; continue; }

    int rx = (player_x - W/2) - ACTIVE_MARGIN;
    int ry = (player_y - H/2) - ACTIVE_MARGIN;
    int rw = W + 2*ACTIVE_MARGIN;
    int rh = H + 2*ACTIVE_MARGIN;
    if (!(b.x>=rx && b.x<rx+rw && b.y>=ry && b.y<ry+rh)) { b.alive=0; continue; }

    if (b.owner_id != 0) {
      if (b.x>=player_x && b.x<player_x+2 && b.y>=player_y && b.y<player_y+2) {
        damagePlayer(b.damage);
        b.alive=0;
      }
      continue;
    }

    for(int ei=0; ei<enemyCount; ei++){
      Enemy& e = enemies[ei];
      if (e.hp<=0) continue;
      int bw,bh; enemy_bounds(e.etype,bw,bh);
      if (b.x>=e.x && b.x<e.x+bw && b.y>=e.y && b.y<e.y+bh) {
        e.hp -= b.damage;
        if (e.hp <= 0) {
          int add = SCORE_KILL[e.etype];
          if (cur.score_boost) add = int(round(add * 1.5f));
          cur.score += add;
          kills_this_level++;
          int dead = e.eid;
          for(int j=0;j<bulletCount;j++) if (bullets[j].alive && bullets[j].owner_id==dead) bullets[j].alive=0;
        }
        if (!cur.pierce_forever) b.alive=0;
        break;
      }
    }
  }

  int w=0;
  for(int i=0;i<bulletCount;i++) if (bullets[i].alive) bullets[w++] = bullets[i];
  bulletCount = w;

  int eW=0;
  for(int i=0;i<enemyCount;i++) if (enemies[i].hp>0) enemies[eW++] = enemies[i];
  enemyCount = eW;
}

static void updateTouchDamage() {
  for(int i=0;i<enemyCount;i++){
    const Enemy& e = enemies[i];
    int bw,bh; enemy_bounds(e.etype,bw,bh);
    bool overlap = !(player_x+2 <= e.x || e.x+bw <= player_x || player_y+2 <= e.y || e.y+bh <= player_y);
    if (overlap) { damagePlayer(1); return; }
  }
}

static void awardLevelCompletionScore() {
  uint32_t now = millis();
  float elapsed = (now - levelStartMs) / 1000.0f;
  float target = 60.0f + 10.0f * float(max(0, enemies_total));
  float time_saved = max(0.0f, target - elapsed);
  int time_score = int(time_saved * 3.0f);
  int completion = 50 + int(cur.level * 2);
  int mhp = max(1, int(cur.max_hp));
  int hp_score = int((float(player_hp) / float(mhp)) * 80.0f);
  int denom = max(1, enemies_total);
  int acc_pen = int((shotsFired * 15) / denom);
  int acc_score = max(0, 200 - acc_pen);
  int dmg_pen = damageTaken * 20;
  int bonus = completion + time_score + hp_score + acc_score - dmg_pen;
  if (bonus < 0) bonus = 0;
  int add = bonus;
  if (cur.score_boost) add = int(round(add * 1.5f));
  cur.score += add;
}

// =======================
// Menus / transitions
// =======================
static void startNewGameFromSlot(uint8_t idx) {
  slotIndex = idx;
  cur = slots[idx];
  seedSel = 0;
  strcpy(newCode, "00000000");
  codePos = 0;
  diffSel = 1;
  state = ST_NEW_SEEDMENU;
}

static void commitNewGame() {
  SlotData s;
  s.used = 1;
  strcpy(s.name, "AAA");
  s.difficulty = diffSel==0?DIFF_EASY:diffSel==1?DIFF_NORMAL:DIFF_HARD;
  if (seedSel == 0) s.seed = splitmix32((uint32_t)esp_random());
  else s.seed = (uint32_t)strtoul(newCode, nullptr, 10);

  s.level = 1; s.score = 0; s.cooldown_factor = 1.0f; s.max_hp = 3;

  slots[slotIndex] = s;
  saveSlots();
  cur = s;

  loadLevel(1);
  state = ST_PLAY;
}

static void clearSlot(uint8_t idx) {
  slots[idx] = SlotData();
  slots[idx].used = 0;
  strcpy(slots[idx].name, "AAA");
  slots[idx].difficulty = DIFF_NORMAL;
  slots[idx].seed = 0;
  slots[idx].level = 1;
  slots[idx].score = 0;
  saveSlots();
}

// =======================
// Arduino loop
// =======================
uint32_t lastMs=0;

void setup() {
  btn.begin();
  pcdInit(55, false);

  loadSave();

  // Intro animation at boot (skippable)
  playAnimFrames(CavRogueIntro, CAVROGUE_INTRO_FRAMES, 70, true);

  lastMs = millis();
}

void loop() {
  uint32_t now = millis();
  float dt = (now - lastMs) / 1000.0f;
  lastMs = now;

  btn.poll();

  // ===== state machine =====
  if (state == ST_PLAY) {
    if (btn.pressed(Buttons::B)) { state = ST_PAUSE; sel=0; }
    else {
      if (btn.held(Buttons::L)) tryMovePlayer(-1,0);
      else if (btn.held(Buttons::R)) tryMovePlayer(1,0);
      else if (btn.held(Buttons::U)) tryMovePlayer(0,-1);
      else if (btn.held(Buttons::D)) tryMovePlayer(0,1);

      if (btn.held(Buttons::A)) shootPlayer();

      updateEnemies(dt);
      updateBullets();
      updateTouchDamage();

      if (!exit_unlocked && cur.difficulty != DIFF_EASY) {
        if (enemyCount == 0) exit_unlocked = true;
        else if (cur.exit_half_kills && enemies_total>0) {
          if (kills_this_level >= (enemies_total+1)/2) exit_unlocked = true;
        }
      }

      if (exit_unlocked && playerInExit()) {
        awardLevelCompletionScore();
        if ((cur.level % 5) == 0) { state = ST_BONUS; sel = 0; }
        int inc = cur.skip_next_level ? 2 : 1;
        cur.skip_next_level = 0;
        int nextLevel = cur.level + inc;
        saveCurSlot();

        if (nextLevel > 100) {
          // Ending animation depends on difficulty
          if (cur.difficulty == DIFF_HARD) playAnimFrames(CavRogueEndingHard, 220, 70, true);
          else playAnimFrames(CavRogueEnding, 162, 70, true);

          addScore(cur);
          state = ST_TITLE;
          sel = 0;
        } else {
          loadLevel(nextLevel);
        }
      }

      if (player_hp <= 0) {
        // Game over animation + score insert
        playAnimFrames(CavRogueGameOver, CAVROGUE_GAMEOVER_FRAMES, 70, true);
        addScore(cur);
        state = ST_GAME_OVER;
        sel=0;
      }
    }
  } else if (state == ST_PAUSE) {
    if (btn.pressed(Buttons::U) || btn.pressed(Buttons::D)) sel = 1 - sel;
    if (btn.pressed(Buttons::A)) {
      if (sel==0) state = ST_PLAY;
      else { state = ST_TITLE; sel=0; }
    }
    if (btn.pressed(Buttons::B)) state = ST_PLAY;
  }
  } else if (state == ST_BONUS) {
    int n = bonusCount();
    if (n <= 0) { state = ST_PLAY; }
    else {
      if (btn.pressed(Buttons::U)) sel = (sel + n - 1) % n;
      if (btn.pressed(Buttons::D)) sel = (sel + 1) % n;
      if (btn.pressed(Buttons::A) || btn.pressed(Buttons::B)) {
        applyBonusChoice(sel);
        saveCurSlot();
        state = ST_PLAY;
      }
    }

  } else if (state == ST_GAME_OVER) {
    if (btn.pressed(Buttons::U) || btn.pressed(Buttons::D)) sel = 1 - sel;
    if (btn.pressed(Buttons::A)) {
      if (sel==0) { restoreFromCheckpoint(); saveCurSlot(); loadLevel(cur.level); state=ST_PLAY; }
      else { state = ST_TITLE; sel=0; }
    }
  } else if (state == ST_TITLE) {
    if (btn.pressed(Buttons::U)) sel = (sel + 2) % 3;
    if (btn.pressed(Buttons::D)) sel = (sel + 1) % 3;
    if (btn.pressed(Buttons::A)) {
      if (sel==0) { state = ST_SLOTS; sel=0; }
      else if (sel==1) { state = ST_SCORES; sel=0; }
      else { state = ST_STORY; storyLine=0; }
    }
  } else if (state == ST_SLOTS) {
    if (btn.pressed(Buttons::U)) sel = (sel + 2) % 3;
    if (btn.pressed(Buttons::D)) sel = (sel + 1) % 3;
    if (btn.pressed(Buttons::B)) { state = ST_TITLE; sel=0; }
    if (btn.pressed(Buttons::A)) {
      if (slots[sel].used) {
        slotIndex = sel;
        cur = slots[slotIndex];
        loadLevel(cur.level);
        state = ST_PLAY;
      } else {
        startNewGameFromSlot(sel);
      }
    }
    // erase on LEFT (simple)
    if (btn.pressed(Buttons::L) && slots[sel].used) {
      slotIndex = sel;
      state = ST_CONFIRM_ERASE;
      sel = 0;
    }
  } else if (state == ST_CONFIRM_ERASE) {
    if (btn.pressed(Buttons::U) || btn.pressed(Buttons::D)) sel = 1 - sel;
    if (btn.pressed(Buttons::A)) {
      if (sel==1) clearSlot(slotIndex);
      state = ST_SLOTS;
      sel = 0;
    }
    if (btn.pressed(Buttons::B)) { state = ST_SLOTS; sel=0; }
  } else if (state == ST_SCORES) {
    if (btn.pressed(Buttons::U) && sel>0) sel--;
    if (btn.pressed(Buttons::D) && sel < SCORE_COUNT-4) sel++;
    if (btn.pressed(Buttons::B)) { state=ST_TITLE; sel=0; }
  } else if (state == ST_STORY) {
    if (btn.pressed(Buttons::U) && storyLine>0) storyLine--;
    if (btn.pressed(Buttons::D) && storyLine < STORY_COUNT-5) storyLine++;
    if (btn.pressed(Buttons::B) || btn.pressed(Buttons::A)) { state=ST_TITLE; sel=0; }
  } else if (state == ST_NEW_SEEDMENU) {
    if (btn.pressed(Buttons::U) || btn.pressed(Buttons::D)) seedSel = 1 - seedSel;
    if (btn.pressed(Buttons::A)) {
      if (seedSel == 0) commitNewGame();
      else { state = ST_NEW_CODE; codePos=0; }
    }
    if (btn.pressed(Buttons::B)) { state = ST_SLOTS; sel=0; }
  } else if (state == ST_NEW_CODE) {
    if (btn.pressed(Buttons::L)) codePos = (codePos + 7) % 8;
    if (btn.pressed(Buttons::R)) codePos = (codePos + 1) % 8;
    if (btn.pressed(Buttons::U)) { newCode[codePos] = char('0' + ((newCode[codePos]-'0'+1)%10)); }
    if (btn.pressed(Buttons::D)) { newCode[codePos] = char('0' + ((newCode[codePos]-'0'+9)%10)); }
    if (btn.pressed(Buttons::A)) { state = ST_NEW_DIFF; }
    if (btn.pressed(Buttons::B)) { state = ST_NEW_SEEDMENU; }
  } else if (state == ST_NEW_DIFF) {
    if (btn.pressed(Buttons::U)) diffSel = (diffSel + 2) % 3;
    if (btn.pressed(Buttons::D)) diffSel = (diffSel + 1) % 3;
    if (btn.pressed(Buttons::A)) commitNewGame();
    if (btn.pressed(Buttons::B)) state = ST_NEW_CODE;
  }

  // ===== render =====
  switch(state) {
    case ST_TITLE: renderTitle(); fbShow(false); break;
    case ST_SLOTS: renderSlots(); fbShow(false); break;
    case ST_SCORES: renderScores(); fbShow(false); break;
    case ST_STORY: renderStory(); fbShow(false); break;
    case ST_CONFIRM_ERASE:
      fbClear(0x00);
      fbCenter(0, "EFFACER?");
      fbCenter(18, (sel==0)?"> ANNUL":"  ANNUL");
      fbCenter(30, (sel==1)?"> EFFACE":"  EFFACE");
      fbShow(false);
      break;
    case ST_NEW_SEEDMENU:
      fbClear(0x00);
      fbCenter(0, "SEED");
      fbCenter(18, seedSel==0?"> ALEATOIRE":"  ALEATOIRE");
      fbCenter(30, seedSel==1?"> PERSO":"  PERSO");
      fbShow(false);
      break;
    case ST_NEW_CODE: {
      fbClear(0x00);
      fbCenter(0, "CODE");
      fbCenter(18, newCode);
      // caret
      int x = (W - 8*6) / 2;
      if (x < 0) x = 0;
      for(int i=0;i<5;i++) fbSetPixel(x + codePos*6 + i, 28, true);
      fbShow(false);
      break;
    }
    case ST_NEW_DIFF:
      fbClear(0x00);
      fbCenter(0, "DIFF");
      fbCenter(14, diffSel==0?"> EASY":"  EASY");
      fbCenter(24, diffSel==1?"> NORM":"  NORM");
      fbCenter(34, diffSel==2?"> HARD":"  HARD");
      fbShow(false);
      break;
    case ST_PAUSE: renderPause(); fbShow(false); break;
    case ST_BONUS: renderBonus(); fbShow(false); break;
    case ST_GAME_OVER: renderGameOverMenu(); fbShow(false); break;
    case ST_PLAY: renderPlay(); fbShow(false); break;
    default: break;
  }

  delay(10); // small frame cap
}
