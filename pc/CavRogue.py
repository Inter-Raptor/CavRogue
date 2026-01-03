"""
CavRogue Prototype v8 — Nokia 5110 accurate UI (84x48 everywhere)

Requested changes:
- Title: "CavRogue" (no "v7 nokia", no ESC quit text)
- Scores: show 3 entries at a time; use UP/DOWN to scroll through 1..10 (no extra hints)
- Slots: remove on-screen button hints
- Seed flow: choose "ALEATOIRE" or "PERSONALISER" before seed edit
- Difficulty labels: "Easy / Norm / Hard" (short)
- In-game: remove HUD (no level/score displayed during gameplay)
- Damage feedback: full-screen flash (invert) 3 times on hit + 3 seconds invincibility
  (player also blinks during invincibility)

Controls
- Move: Arrow keys
- Shoot: Space
- Pause: Esc
- Menus: Arrows + Enter
- Slots: Backspace = erase slot (with confirmation)
"""

from __future__ import annotations
import json, random, time, os, re
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import List, Tuple, Optional, Dict

import pygame

# ===== Display (Nokia 5110) =====
W, H = 84, 48
SCALE = 10
FPS = 60

SAVE_PATH = Path(os.environ.get("LOCALAPPDATA", ".")) / "CavRogue" / "cave_rogue_save.json"
SAVE_PATH.parent.mkdir(parents=True, exist_ok=True)

WHITE = (255, 255, 255)
BLACK = (0, 0, 0)

# ===== Animation loader (from .h files) =====
def _find_asset_file(filename: str) -> Optional[str]:
    """Try to locate an asset near this script or in common asset folders.

    Supported folders (relative to the .py or to the current working directory):
      - ./ (same folder)
      - ./H/
      - ./Annimation/   (your current folder name)
      - ./Animation/    (common spelling)
    """
    here = Path(__file__).resolve().parent
    candidates = [
        here / filename,
        here / "H" / filename,
        here / "Annimation" / filename,
        here / "Animation" / filename,
        Path.cwd() / filename,
        Path.cwd() / "H" / filename,
        Path.cwd() / "Annimation" / filename,
        Path.cwd() / "Animation" / filename,
    
        here / "Sprites" / filename,
        Path.cwd() / "Sprites" / filename,
    ]
    for c in candidates:
        if c.exists():
            return str(c)
    return None

def load_h_frames_84x48(path: str) -> List[List[int]]:
    """Parse a C header containing hex bytes and return frames grouped by 504 bytes."""
    t = Path(path).read_text(encoding="utf-8", errors="ignore")
    hex_bytes = re.findall(r"0x[0-9A-Fa-f]{2}", t)
    data = [int(h, 16) for h in hex_bytes]
    if len(data) < 504:
        raise ValueError(f"Not enough hex bytes in {path}")
    n_frames = len(data) // 504
    data = data[: n_frames * 504]
    return [data[i*504:(i+1)*504] for i in range(n_frames)]

def frame504_to_surface(frame: List[int]) -> pygame.Surface:
    """Convert 504 bytes (PCD8544 format) to a pygame Surface 84x48.
    Note: In this game, 'on' pixels are WHITE on BLACK background.
    """
    surf = pygame.Surface((W, H))
    surf.fill(BLACK)
    for page in range(6):
        base = page * W
        for x in range(W):
            b = frame[base + x]
            if b == 0:
                continue
            for bit in range(8):
                if (b >> bit) & 1:
                    y = page * 8 + bit
                    if 0 <= y < H:
                        surf.set_at((x, y), WHITE)
    return surf

def load_story_lines_from_h(path: str) -> List[str]:
    """Parse anim_story.h exported by our tooling."""
    t = Path(path).read_text(encoding="utf-8", errors="ignore")
    return re.findall(r'"([^"]*)"', t)

def clamp(v, a, b): return a if v < a else b if v > b else v

def splitmix32(x: int) -> int:
    x = (x + 0x9E3779B9) & 0xFFFFFFFF
    x = (x ^ (x >> 16)) & 0xFFFFFFFF
    x = (x * 0x85EBCA6B) & 0xFFFFFFFF
    x = (x ^ (x >> 13)) & 0xFFFFFFFF
    x = (x * 0xC2B2AE35) & 0xFFFFFFFF
    x = (x ^ (x >> 16)) & 0xFFFFFFFF
    return x

def level_seed(run_seed: int, level: int) -> int:
    return splitmix32(run_seed ^ (level * 0xA511E9B3 & 0xFFFFFFFF))

def dist2(a: Tuple[int,int], b: Tuple[int,int]) -> int:
    dx = a[0]-b[0]; dy = a[1]-b[1]
    return dx*dx + dy*dy

def pattern_to_offsets(lines: List[str]) -> List[Tuple[int,int]]:
    offs: List[Tuple[int,int]] = []
    for y, row in enumerate(lines):
        for x, ch in enumerate(row):
            if ch == "0":
                offs.append((x, y))
    return offs

def sprite_bounds(offs: List[Tuple[int,int]]) -> Tuple[int,int]:
    mx = max(dx for dx,_ in offs)
    my = max(dy for _,dy in offs)
    return mx+1, my+1

# ===== 5x7 bitmap font =====
FONT: Dict[str, List[str]] = {
" ": ["00000","00000","00000","00000","00000","00000","00000"],
"-": ["00000","00000","00000","11111","00000","00000","00000"],
".": ["00000","00000","00000","00000","00000","01100","01100"],
":": ["00000","01100","01100","00000","01100","01100","00000"],
"/": ["00001","00010","00100","01000","10000","00000","00000"],
">": ["10000","01000","00100","00010","00100","01000","10000"],
"<": ["00001","00010","00100","01000","00100","00010","00001"],
"!": ["00100","00100","00100","00100","00100","00000","00100"],
"?": ["01110","10001","00001","00010","00100","00000","00100"],
}

FONT.update({
"0": ["01110","10001","10011","10101","11001","10001","01110"],
"1": ["00100","01100","00100","00100","00100","00100","01110"],
"2": ["01110","10001","00001","00010","00100","01000","11111"],
"3": ["11110","00001","00001","01110","00001","00001","11110"],
"4": ["00010","00110","01010","10010","11111","00010","00010"],
"5": ["11111","10000","11110","00001","00001","10001","01110"],
"6": ["00110","01000","10000","11110","10001","10001","01110"],
"7": ["11111","00001","00010","00100","01000","01000","01000"],
"8": ["01110","10001","10001","01110","10001","10001","01110"],
"9": ["01110","10001","10001","01111","00001","00010","01100"],
})

FONT.update({
"A":["01110","10001","10001","11111","10001","10001","10001"],
"B":["11110","10001","10001","11110","10001","10001","11110"],
"C":["01110","10001","10000","10000","10000","10001","01110"],
"D":["11100","10010","10001","10001","10001","10010","11100"],
"E":["11111","10000","10000","11110","10000","10000","11111"],
"F":["11111","10000","10000","11110","10000","10000","10000"],
"G":["01110","10001","10000","10111","10001","10001","01110"],
"H":["10001","10001","10001","11111","10001","10001","10001"],
"I":["01110","00100","00100","00100","00100","00100","01110"],
"J":["00111","00010","00010","00010","00010","10010","01100"],
"K":["10001","10010","10100","11000","10100","10010","10001"],
"L":["10000","10000","10000","10000","10000","10000","11111"],
"M":["10001","11011","10101","10101","10001","10001","10001"],
"N":["10001","11001","10101","10011","10001","10001","10001"],
"O":["01110","10001","10001","10001","10001","10001","01110"],
"P":["11110","10001","10001","11110","10000","10000","10000"],
"Q":["01110","10001","10001","10001","10101","10010","01101"],
"R":["11110","10001","10001","11110","10100","10010","10001"],
"S":["01111","10000","10000","01110","00001","00001","11110"],
"T":["11111","00100","00100","00100","00100","00100","00100"],
"U":["10001","10001","10001","10001","10001","10001","01110"],
"V":["10001","10001","10001","10001","10001","01010","00100"],
"W":["10001","10001","10001","10101","10101","11011","10001"],
"X":["10001","10001","01010","00100","01010","10001","10001"],
"Y":["10001","10001","01010","00100","00100","00100","00100"],
"Z":["11111","00001","00010","00100","01000","10000","11111"],
})

def draw_char(buf: pygame.Surface, x: int, y: int, ch: str, color=WHITE):
    glyph = FONT.get(ch, FONT.get("?"))
    if glyph is None:
        return
    for ry, row in enumerate(glyph):
        yy = y + ry
        if 0 <= yy < H:
            for rx, bit in enumerate(row):
                if bit == "1":
                    xx = x + rx
                    if 0 <= xx < W:
                        buf.set_at((xx, yy), color)

def draw_text(buf: pygame.Surface, x: int, y: int, text: str, color=WHITE):
    """Draw text in 5x7 font. Automatically clips at the right edge (84px)."""
    cx = x
    for ch in text:
        if cx > W - 6:  # next glyph would go off-screen
            break
        draw_char(buf, cx, y, ch, color)
        cx += 6

def text_width(text: str) -> int:
    return max(0, len(text) * 6 - 1)

def draw_center(buf: pygame.Surface, y: int, text: str, color=WHITE):
    """Draw centered text, clipped to 14 chars so it never overflows."""
    if text is None:
        return
    text = str(text)[:14]
    x = (W - text_width(text)) // 2
    if x < 0:
        x = 0
    draw_text(buf, x, y, text, color)

# ===== Gameplay tuning =====
ACTIVE_MARGIN = 24  # px margin around the 84x48 camera where enemies are 'active'
PLAYER_BASE_COOLDOWN = 2.0
PLAYER_COOLDOWN_MIN = 0.22
PLAYER_IFRAME_SEC = 3.0  # requested

AGGRO_R_12 = 26
AGGRO_R_34 = 48

MAX_MAP_W = 1400
MAX_MAP_H = 900

# ---------- Save models ----------
@dataclass
class Slot:
    used: bool = False
    name: str = "AAA"
    difficulty: str = "NORMAL"  # EASY / NORMAL / HARDCORE
    seed: int = 0
    level: int = 1
    score: int = 0

    cooldown_factor: float = 1.0
    max_hp: int = 3
    score_boost: bool = False
    skip_next_level: bool = False

    enemy_arrow: bool = False
    pierce_forever: bool = False
    exit_half_kills: bool = False
    doubled_once: bool = False

    checkpoint_level: int = 0
    checkpoint_score: int = 0
    checkpoint_cooldown_factor: float = 1.0
    checkpoint_max_hp: int = 3
    checkpoint_score_boost: bool = False
    checkpoint_enemy_arrow: bool = False
    checkpoint_pierce_forever: bool = False
    checkpoint_exit_half_kills: bool = False
    checkpoint_doubled_once: bool = False

@dataclass
class ScoreEntry:
    name: str
    score: int
    difficulty: str
    seed: int

def default_save() -> Dict:
    return {"slots": [asdict(Slot()) for _ in range(3)], "scores": []}

def load_save() -> Dict:
    if not SAVE_PATH.exists():
        return default_save()
    try:
        return json.loads(SAVE_PATH.read_text(encoding="utf-8"))
    except Exception:
        return default_save()

def save_save(data: Dict) -> None:
    try:
        SAVE_PATH.write_text(json.dumps(data, indent=2), encoding="utf-8")
    except Exception:
        pass

# ---------- Cave generation ----------
def stamp_circle(grid: bytearray, w: int, h: int, cx: int, cy: int, r: int) -> None:
    r2 = r*r
    x0 = max(0, cx - r); x1 = min(w-1, cx + r)
    y0 = max(0, cy - r); y1 = min(h-1, cy + r)
    for y in range(y0, y1+1):
        dy = y - cy
        dy2 = dy*dy
        row = y*w
        for x in range(x0, x1+1):
            dx = x - cx
            if dx*dx + dy2 <= r2:
                grid[row + x] = 1

def flood_fill_reachable(grid: bytearray, w: int, h: int, start: Tuple[int,int]) -> bytearray:
    vis = bytearray(w*h)
    sx, sy = start
    if not (0 <= sx < w and 0 <= sy < h) or grid[sy*w+sx] == 0:
        return vis
    q = [(sx, sy)]
    vis[sy*w+sx] = 1
    head = 0
    while head < len(q):
        x, y = q[head]; head += 1
        for nx, ny in ((x+1,y),(x-1,y),(x,y+1),(x,y-1)):
            if 0 <= nx < w and 0 <= ny < h:
                idx = ny*w+nx
                if vis[idx] == 0 and grid[idx] == 1:
                    vis[idx] = 1
                    q.append((nx, ny))
    return vis

def worm_connect_points(rng: random.Random, a: Tuple[int,int], b: Tuple[int,int], margin: int,
                        w: int, h: int, step: int) -> List[Tuple[int,int]]:
    x, y = a
    tx, ty = b
    pts = [(x, y)]
    dirx, diry = 0, 0
    for _ in range(25000):
        if abs(tx-x) + abs(ty-y) <= step:
            break
        dx = tx - x
        dy = ty - y
        want_axis = "x" if abs(dx) >= abs(dy) else "y"

        if rng.random() < 0.58:
            if want_axis == "x":
                dirx, diry = 0, 1 if rng.random() < 0.5 else -1
            else:
                dirx, diry = 1 if rng.random() < 0.5 else -1, 0
        else:
            if want_axis == "x":
                dirx, diry = (1 if dx > 0 else -1), 0
            else:
                dirx, diry = 0, (1 if dy > 0 else -1)

        run = 1 if rng.random() < 0.55 else 2
        for _r in range(run):
            x += dirx * step
            y += diry * step
            x = int(clamp(x, margin, w-1-margin))
            y = int(clamp(y, margin, h-1-margin))
            pts.append((x, y))
            if abs(tx-x) + abs(ty-y) <= step:
                break
    pts.append((tx, ty))
    return pts

def generate_cave(run_seed: int, level: int) -> Tuple[bytearray, int, int, Tuple[int,int], Tuple[int,int]]:
    rng = random.Random(level_seed(run_seed, level))

    map_step = min((level + 1)//2, 30)
    target_w = W * map_step
    target_h = H * map_step
    w = int(clamp(target_w, W, MAX_MAP_W))
    h = int(clamp(target_h, H, MAX_MAP_H))
    grid = bytearray(w*h)

    orientation = rng.choice(["LR","RL","TB","BT"])
    margin = 14
    if orientation == "LR":
        start = (margin, rng.randrange(margin, h-margin))
        exitp = (w-1-margin, rng.randrange(margin, h-margin))
        along = (1, 0)
    elif orientation == "RL":
        start = (w-1-margin, rng.randrange(margin, h-margin))
        exitp = (margin, rng.randrange(margin, h-margin))
        along = (-1, 0)
    elif orientation == "TB":
        start = (rng.randrange(margin, w-margin), margin)
        exitp = (rng.randrange(margin, w-margin), h-1-margin)
        along = (0, 1)
    else:
        start = (rng.randrange(margin, w-margin), h-1-margin)
        exitp = (rng.randrange(margin, w-margin), margin)
        along = (0, -1)

    r_min, r_max = 9, 15
    big_room_chance = 0.08 + min(0.14, level/250)

    step = 8
    waypoints = [start]
    k = int(clamp(2 + level//8, 2, 8))
    for i in range(1, k):
        t = i / k
        mx = int(start[0] + (exitp[0]-start[0]) * t)
        my = int(start[1] + (exitp[1]-start[1]) * t)
        amp = int(clamp(14 + level*0.55, 14, 90))
        if along[0] != 0:
            my += rng.randint(-amp, amp)
        else:
            mx += rng.randint(-amp, amp)
        mx = int(clamp(mx, margin, w-1-margin))
        my = int(clamp(my, margin, h-1-margin))
        waypoints.append((mx, my))
    waypoints.append(exitp)

    pts: List[Tuple[int,int]] = []
    for a, b in zip(waypoints, waypoints[1:]):
        pts.extend(worm_connect_points(rng, a, b, margin, w, h, step))

    for (px, py) in pts:
        stamp_circle(grid, w, h, px, py, rng.randint(r_min, r_max))
        if rng.random() < big_room_chance:
            stamp_circle(grid, w, h, px, py, rng.randint(16, 28))

    branches = int(clamp(3 + level//2, 3, 70))
    branch_len = int(clamp(4 + level//10, 4, 26))
    for _ in range(branches):
        bx, by = rng.choice(pts[:: max(1, len(pts)//500) ])
        dirx, diry = rng.choice([(1,0),(-1,0),(0,1),(0,-1)])
        cx, cy = bx, by
        for _s in range(branch_len):
            if rng.random() < 0.64:
                if dirx != 0:
                    dirx = 0; diry = rng.choice([-1, 1])
                else:
                    diry = 0; dirx = rng.choice([-1, 1])
            cx += dirx * step
            cy += diry * step
            cx = int(clamp(cx, margin, w-1-margin))
            cy = int(clamp(cy, margin, h-1-margin))
            stamp_circle(grid, w, h, cx, cy, rng.randint(r_min, r_max))
        stamp_circle(grid, w, h, cx, cy, rng.randint(14, 30))

    vis = flood_fill_reachable(grid, w, h, start)
    if vis[exitp[1]*w + exitp[0]] == 0:
        cx, cy = start; tx, ty = exitp
        steps = max(3, int((abs(tx-cx)+abs(ty-cy)) / 10))
        for i in range(steps+1):
            ix = int(cx + (tx-cx)*i/steps)
            iy = int(cy + (ty-cy)*i/steps)
            stamp_circle(grid, w, h, ix, iy, 14)

    stamp_circle(grid, w, h, start[0], start[1], 14)
    stamp_circle(grid, w, h, exitp[0], exitp[1], 14)
    return grid, w, h, start, exitp


# ===== External sprite headers loader (optional) =====
# Allows CavRogue to use the same sprite assets you plan to compile on ESP/Arduino.
# If headers are missing or incomplete, the game falls back to the built-in tiny sprites.

# Keep gameplay identical: only accept the original tiny sprite sizes by default.
STRICT_SPRITE_SIZES = True

_C_UINT8_ARRAY_RE = re.compile(
    r"(?:static\s+)?(?:const\s+)?(?:uint8_t|unsigned\s+char)\s+(\w+)\s*\[\s*\]\s*(?:PROGMEM\s*)?=\s*\{([^}]*)\};",
    re.S,
)

def _strip_c_comments(s: str) -> str:
    s = re.sub(r"/\*.*?\*/", "", s, flags=re.S)
    s = re.sub(r"//.*?$", "", s, flags=re.M)
    return s

def _parse_c_int_tokens(body: str) -> List[int]:
    tokens = re.findall(r"0x[0-9a-fA-F]+|0b[01]+|\d+", body)
    out: List[int] = []
    for tok in tokens:
        if tok.startswith(("0x", "0X")):
            v = int(tok, 16)
        elif tok.startswith(("0b", "0B")):
            v = int(tok, 2)
        else:
            v = int(tok, 10)
        out.append(v & 0xFF)
    return out

def _extract_uint8_arrays_from_header(path: Path) -> Dict[str, List[int]]:
    try:
        raw = path.read_text(encoding="utf-8", errors="ignore")
    except Exception:
        return {}
    s = _strip_c_comments(raw)
    arrays: Dict[str, List[int]] = {}
    for name, body in _C_UINT8_ARRAY_RE.findall(s):
        vals = _parse_c_int_tokens(body)
        if vals:
            arrays[name] = vals
    return arrays

def _infer_size_from_name(name: str) -> Optional[Tuple[int, int]]:
    m = re.search(r"_(\d+)x(\d+)\b", name)
    if not m:
        return None
    return int(m.group(1)), int(m.group(2))

def _xbm_to_offsets(data: List[int], w: int, h: int, *, msb_first: bool = False) -> List[Tuple[int, int]]:
    """Convert XBM-like packed 1bpp bytes to offsets where bit=1."""
    bpr = (w + 7) // 8
    if len(data) < bpr * h:
        # pad if needed
        data = data + [0] * (bpr * h - len(data))
    offs: List[Tuple[int, int]] = []
    for y in range(h):
        row0 = y * bpr
        for x in range(w):
            b = data[row0 + (x // 8)]
            bit = 7 - (x % 8) if msb_first else (x % 8)
            if (b >> bit) & 1:
                offs.append((x, y))
    return offs

def _find_sprite_headers() -> Tuple[Optional[Path], Optional[Path]]:
    """Find sprites_player.h and sprites_enemies.h near the script/cwd."""
    here = Path(__file__).resolve().parent
    candidates = [
        here / "Sprites",
        here / "sprites",
        Path.cwd() / "Sprites",
        Path.cwd() / "sprites",
        here,
        Path.cwd(),
    ]
    ph = eh = None
    for base in candidates:
        p = base / "sprites_player.h"
        e = base / "sprites_enemies.h"
        if ph is None and p.exists():
            ph = p
        if eh is None and e.exists():
            eh = e
    return ph, eh

def _build_player_frames_from_arrays(arrays: Dict[str, List[int]]) -> Optional[Tuple[Dict[str, List[List[Tuple[int, int]]]], Tuple[int, int]]]:
    """Expected (recommended) names:
      PLAYER_L_0_4x4 ... PLAYER_L_3_4x4
      PLAYER_R_0_4x4 ... etc, U, D
    If only one frame exists per direction, it's repeated.
    """
    dirs = ["L", "R", "U", "D"]
    frames_out: Dict[str, List[List[Tuple[int, int]]]] = {}
    bounds: Optional[Tuple[int, int]] = None

    def frame_index(name_up: str, d: str) -> int:
        # PLAYER_L_2_4x4 or PLAYER_L2_4x4
        m = re.search(rf"PLAYER[_]?{d}[_]?(\d+)", name_up)
        if m:
            return int(m.group(1))
        # fallback: _2_ pattern
        m = re.search(r"_(\d+)_", name_up)
        if m:
            return int(m.group(1))
        return 0

    for d in dirs:
        cand = []
        for n in arrays.keys():
            up = n.upper()
            if up.startswith(f"PLAYER_{d}") or up.startswith(f"PLAYER{d}"):
                cand.append(n)
        if not cand:
            continue

        # sort by frame index
        cand.sort(key=lambda n: frame_index(n.upper(), d))

        # infer size from first candidate
        wh = _infer_size_from_name(cand[0])
        if wh is None:
            # if no size in name, assume 4x4 (keeps gameplay very close)
            wh = (4, 4)
        w, h = wh
        if STRICT_SPRITE_SIZES and (w, h) != (4, 4):
            return None

        # build frames
        frames: List[List[Tuple[int, int]]] = []
        for n in cand:
            wh2 = _infer_size_from_name(n) or (w, h)
            if wh2 != (w, h):
                continue
            frames.append(_xbm_to_offsets(arrays[n], w, h, msb_first=False))

        if not frames:
            continue

        # normalize to FEET count
        need = len(FEET)
        if len(frames) < need:
            frames = (frames * (need // len(frames) + 1))[:need]
        else:
            frames = frames[:need]

        frames_out[d] = frames
        bounds = (w, h)

    if len(frames_out) == 0:
        return None
    # if some directions missing, mirror a found one
    any_frames = next(iter(frames_out.values()))
    for d in dirs:
        if d not in frames_out:
            frames_out[d] = any_frames
    if bounds is None:
        bounds = (4, 4)
    return frames_out, bounds

def _build_enemy_shapes_from_arrays(arrays: Dict[str, List[int]]) -> Optional[Tuple[Dict[int, List[Tuple[int, int]]], Dict[int, Tuple[int, int]]]]:
    """Expected (recommended) names:
      ENEMY_PING_5x5  -> type 1
      ENEMY_SCAN_7x8  -> type 2
      ENEMY_DAEMON_7x6-> type 3
      ENEMY_FIREWALL_8x8 -> type 4
    """
    def pick(prefixes: List[str]) -> Optional[str]:
        for p in prefixes:
            for n in arrays.keys():
                if n.upper().startswith(p):
                    return n
        # second pass: contains
        for p in prefixes:
            for n in arrays.keys():
                if p in n.upper():
                    return n
        return None

    mapping = {
        1: ["ENEMY_PING", "PING", "ENEMY1", "E1"],
        2: ["ENEMY_SCAN", "SCAN", "ENEMY2", "E2"],
        3: ["ENEMY_DAEMON", "DAEMON", "ENEMY3", "E3"],
        4: ["ENEMY_FIREWALL", "FIREWALL", "ENEMY4", "E4"],
    }

    shapes: Dict[int, List[Tuple[int, int]]] = {}
    bounds: Dict[int, Tuple[int, int]] = {}

    for et, prefs in mapping.items():
        name = pick(prefs)
        if not name:
            continue
        wh = _infer_size_from_name(name)
        if wh is None:
            # fallback sizes matching the built-in shapes
            wh = {1:(5,5), 2:(7,8), 3:(7,6), 4:(8,8)}[et]
        w, h = wh
        if STRICT_SPRITE_SIZES:
            expected = {1:(5,5), 2:(7,8), 3:(7,6), 4:(8,8)}[et]
            if (w, h) != expected:
                return None
        offs = _xbm_to_offsets(arrays[name], w, h, msb_first=False)
        if offs:
            shapes[et] = offs
            bounds[et] = (w, h)

    if len(shapes) == 0:
        return None

    # ensure all types present
    for et in (1,2,3,4):
        if et not in shapes:
            return None
    return shapes, bounds

def try_load_external_sprites(
    player_frames_fallback: Dict[str, List[List[Tuple[int,int]]]],
    player_bounds_fallback: Tuple[int,int],
    enemy_shapes_fallback: Dict[int, List[Tuple[int,int]]],
    enemy_bounds_fallback: Dict[int, Tuple[int,int]],
):
    ph, eh = _find_sprite_headers()
    if not ph and not eh:
        return player_frames_fallback, player_bounds_fallback, enemy_shapes_fallback, enemy_bounds_fallback

    # load arrays
    p_arrays = _extract_uint8_arrays_from_header(ph) if ph else {}
    e_arrays = _extract_uint8_arrays_from_header(eh) if eh else {}

    # build
    pf = _build_player_frames_from_arrays(p_arrays) if p_arrays else None
    ef = _build_enemy_shapes_from_arrays(e_arrays) if e_arrays else None

    if pf:
        player_frames, player_bounds = pf
        print(f"[SPRITES] Loaded player sprites from: {ph.name}")
    else:
        player_frames, player_bounds = player_frames_fallback, player_bounds_fallback
        if ph:
            print(f"[SPRITES] Could not load player sprites from {ph.name} (names/sizes missing) -> fallback")

    if ef:
        enemy_shapes, enemy_bounds = ef
        print(f"[SPRITES] Loaded enemy sprites from: {eh.name}")
    else:
        enemy_shapes, enemy_bounds = enemy_shapes_fallback, enemy_bounds_fallback
        if eh:
            print(f"[SPRITES] Could not load enemy sprites from {eh.name} (names/sizes missing) -> fallback")

    return player_frames, player_bounds, enemy_shapes, enemy_bounds

# ---------- Sprites ----------
FEET = ["0--0", "-0-0", "0-0-", "0--0"]
def make_player_frames(direction: str) -> List[List[Tuple[int,int]]]:
    frames: List[List[Tuple[int,int]]] = []
    if direction == "L":
        top = ["-00-","000-","-00-"]
    elif direction == "R":
        top = ["-00-","-000","-00-"]
    elif direction == "U":
        top = ["--0-","-00-","-00-"]
    else:
        top = ["-00-","-00-","-00-"]
    for feet in FEET:
        lines = top + [feet]
        frames.append(pattern_to_offsets(lines))
    return frames

PLAYER_FRAMES = {d: make_player_frames(d) for d in ["L","R","U","D"]}
PLAYER_BOUNDS = (4, 4)

EXIT_DOOR = [
    "---0---",
    "-00000-",
    "0-----0",
    "0-----0",
    "0-----0",
    "0000000",
]
EXIT_OFFS = pattern_to_offsets(EXIT_DOOR)
EXIT_BOUNDS = (7, 6)

E1_LINES = ["00000","0-0-0","00000","-0-0-","00-00"]
E2_LINES = ["-00000-","-0-0-0-","-00000-","---0---","0000000","0-0-0-0","--0-0--","-00-00-"]
E3_LINES = ["--000--","--0-0--","0-000-0","0000000","--0--0--","-00--00-"]
E4_LINES = ["--0--0--","--0000--","--0000--","-0-00-0-","0-0000-0","--0000--","-00--00-","-0----0-"]
ENEMY_SHAPES = {1: pattern_to_offsets(E1_LINES), 2: pattern_to_offsets(E2_LINES), 3: pattern_to_offsets(E3_LINES), 4: pattern_to_offsets(E4_LINES)}
ENEMY_BOUNDS = {k: sprite_bounds(v) for k,v in ENEMY_SHAPES.items()}

# Try to load external sprite headers (Sprites/sprites_player.h + sprites_enemies.h)
# This lets you keep assets out of the main .py and re-use them later on ESP/Arduino.
PLAYER_FRAMES, PLAYER_BOUNDS, ENEMY_SHAPES, ENEMY_BOUNDS = try_load_external_sprites(
    PLAYER_FRAMES, PLAYER_BOUNDS,
    ENEMY_SHAPES, ENEMY_BOUNDS,
)


ARROW_R = pattern_to_offsets(["--0-", "0000", "--0-"])
ARROW_L = pattern_to_offsets(["-0--", "0000", "-0--"])
ARROW_U = pattern_to_offsets(["--0-", "-000", "0000"])
ARROW_D = pattern_to_offsets(["0000", "-000", "--0-"])
ARROW_BOUNDS = (4, 3)

DIRS = [(1,0),(-1,0),(0,1),(0,-1)]
ENEMY_TICKS_NORMAL = {1: 12, 2: 8, 3: 14, 4: 8}
ENEMY_TICKS_AGGRO  = {1: 6,  2: 4, 3: 12, 4: 4}
ENEMY_HP = {1: 1, 2: 1, 3: 2, 4: 5}
ENEMY_SHOOT_RANGE = {1: (2.8, 2.8), 2: (1.0, 1.6), 3: (2.1, 2.7), 4: (2.6, 3.2)}
ENEMY_SHOT_DAMAGE = {1: 1, 2: 1, 3: 1, 4: 2}
SCORE_KILL = {1: 10, 2: 18, 3: 45, 4: 120}

@dataclass
class Bullet:
    x: int
    y: int
    dx: int
    dy: int
    owner_id: int
    damage: int
    alive: bool = True

@dataclass
class Enemy:
    eid: int
    etype: int
    x: int
    y: int
    hp: int
    move_tick: int = 0
    dir: Tuple[int,int] = (0,0)
    step_left: int = 0
    pause_left: int = 0
    shoot_timer: float = 0.0
    burst_left: int = 0
    burst_delay: float = 0.0
    burst_dx: int = 0
    burst_dy: int = 0

class Game:
    def __init__(self):
        self.win = pygame.display.set_mode((W*SCALE, H*SCALE))
        pygame.display.set_caption("CavRogue")
        self.clock = pygame.time.Clock()
        self.fb = pygame.Surface((W, H))

        self.save = load_save()
        self.running = True


        # --- animations / story ---
        self.anim_bank: Dict[str, List[pygame.Surface]] = {}
        self.anim_name: str = ""
        self.anim_frames: List[pygame.Surface] = []
        self.anim_i: int = 0
        self.anim_acc: float = 0.0
        self.anim_frame_time: float = 0.07  # seconds per frame
        self.anim_next_state: str = "TITLE"

        self.story_lines: List[str] = []
        self.story_y: float = 0.0
        self.story_speed: float = 0.0
        self.story_line_h: int = 8
        self.story_total_h: int = 0
        self.state = "TITLE"
        self.sel = 0
        self.slot_index = 0

        self.new_name = ["A","A","A"]
        self.new_code_digits = ["0"]*8
        self.new_code_pos = 0
        self.diff_sel = 1
        self.seed_sel = 0  # 0 random / 1 custom

        self.slot: Optional[Slot] = None

        # map
        self.grid = bytearray()
        self.map_w = 0
        self.map_h = 0
        self.start = (0,0)
        self.exitp = (0,0)

        # level state
        self.exit_unlocked = True
        self.enemies_total = 0
        self.kills_this_level = 0


        # scoring (level metrics)
        self.level_start_time = time.time()
        self.shots_fired_level = 0
        self.damage_taken_level = 0
        self.level_bonus_last = 0
        # player
        self.player_x = 0
        self.player_y = 0
        self.player_hp = 3
        self.player_face = "R"
        self.step_phase = 0
        self.last_shot_time = 0.0
        self.iframes_until = 0.0

        # flash effect on damage
        self.flash_steps_left = 0
        self.flash_next_t = 0.0
        self.flash_on = False
        self.flash_step_dt = 0.10  # toggles quickly

        # entities
        self.enemies: List[Enemy] = []
        self.next_eid = 1
        self.bullets: List[Bullet] = []

        self.message: Optional[str] = None
        self.message_t = 0.0

        # for game over display
        self.go_level = 1
        self.go_score = 0
        self.go_diff = "NORM"

        # score scroll
        self.score_scroll = 0

        # camera
        self.cam_x = 0
        self.cam_y = 0

        # Load assets (optional). If not found, the game still runs.
        self._load_assets()

        # Start with intro animation if available
        if "intro" in self.anim_bank and self.anim_bank["intro"]:
            self.start_anim("intro", next_state="TITLE", frame_time=0.07)
        else:
            self.state = "TITLE"
            self.sel = 0


    # ----- save helpers -----
    def get_slots(self) -> List[Slot]:
        slots: List[Slot] = []
        for s in self.save.get("slots", []):
            tmp = Slot()
            tmp.__dict__.update(s)
            slots.append(tmp)
        while len(slots) < 3:
            slots.append(Slot())
        return slots

    def set_slot(self, idx: int, slot: Slot) -> None:
        slots = self.get_slots()
        slots[idx] = slot
        self.save["slots"] = [asdict(s) for s in slots]
        save_save(self.save)

    def clear_slot(self, idx: int) -> None:
        slots = self.get_slots()
        slots[idx] = Slot()
        self.save["slots"] = [asdict(s) for s in slots]
        save_save(self.save)

    def add_score(self, entry: ScoreEntry) -> None:
        scores = [ScoreEntry(**e) for e in self.save.get("scores", [])]
        scores.append(entry)
        scores.sort(key=lambda e: e.score, reverse=True)
        scores = scores[:10]
        self.save["scores"] = [asdict(e) for e in scores]
        save_save(self.save)

    # ----- checkpoints -----
    def write_checkpoint_from_current(self):
        self.slot.checkpoint_level = self.slot.level
        self.slot.checkpoint_score = self.slot.score
        self.slot.checkpoint_cooldown_factor = self.slot.cooldown_factor
        self.slot.checkpoint_max_hp = self.slot.max_hp
        self.slot.checkpoint_score_boost = self.slot.score_boost
        self.slot.checkpoint_enemy_arrow = self.slot.enemy_arrow
        self.slot.checkpoint_pierce_forever = self.slot.pierce_forever
        self.slot.checkpoint_exit_half_kills = self.slot.exit_half_kills
        self.slot.checkpoint_doubled_once = self.slot.doubled_once
        self.set_slot(self.slot_index, self.slot)

    def restore_from_checkpoint(self):
        if self.slot.checkpoint_level <= 0:
            self.slot.level = 1
            self.slot.score = 0
            self.slot.cooldown_factor = 1.0
            self.slot.max_hp = 3
            self.slot.score_boost = False
            self.slot.skip_next_level = False
            self.slot.enemy_arrow = False
            self.slot.pierce_forever = False
            self.slot.exit_half_kills = False
            self.slot.doubled_once = False
        else:
            self.slot.level = self.slot.checkpoint_level
            self.slot.score = self.slot.checkpoint_score
            self.slot.cooldown_factor = self.slot.checkpoint_cooldown_factor
            self.slot.max_hp = self.slot.checkpoint_max_hp
            self.slot.score_boost = self.slot.checkpoint_score_boost
            self.slot.enemy_arrow = self.slot.checkpoint_enemy_arrow
            self.slot.pierce_forever = self.slot.checkpoint_pierce_forever
            self.slot.exit_half_kills = self.slot.checkpoint_exit_half_kills
            self.slot.doubled_once = self.slot.checkpoint_doubled_once
            self.slot.skip_next_level = False
        self.set_slot(self.slot_index, self.slot)
        self.load_level(self.slot.level)

    # ----- helpers -----
    def diff_short(self, d: str) -> str:
        if d == "EASY": return "EASY"
        if d == "HARDCORE": return "HARD"
        return "NORM"

    # ----- assets / animations -----
    def _load_assets(self):
        # Load animation headers if present (placed next to this .py, or in a sibling 'H' folder).
        # Filenames expected:
        #  - anim_intro.h
        #  - anim_gameover.h
        #  - anim_ending_normaleasy.h
        #  - anim_ending_hard.h
        #  - anim_story.h
        anim_files = {
            "intro": "anim_intro.h",
            "gameover": "anim_gameover.h",
            "ending_normal": "anim_ending_normaleasy.h",
            "ending_hard": "anim_ending_hard.h",
        }

        for key, fn in anim_files.items():
            p = _find_asset_file(fn)
            if not p:
                continue
            try:
                frames = load_h_frames_84x48(p)
                self.anim_bank[key] = [frame504_to_surface(fr) for fr in frames]
            except Exception:
                self.anim_bank[key] = []

        p_story = _find_asset_file("anim_story.h")
        if p_story:
            try:
                self.story_lines = load_story_lines_from_h(p_story)
            except Exception:
                self.story_lines = []
        else:
            self.story_lines = []

    def start_anim(self, name: str, next_state: str = "TITLE", frame_time: float = 0.06):
        self.anim_name = name
        self.anim_frames = self.anim_bank.get(name, [])
        self.anim_i = 0
        self.anim_acc = 0.0
        self.anim_frame_time = frame_time
        self.anim_next_state = next_state
        self.state = "ANIM"

    def _finish_anim(self):
        self.state = self.anim_next_state
        if self.state == "TITLE":
            self.sel = 0

    def update_anim(self, dt: float):
        if not self.anim_frames:
            self._finish_anim()
            return
        self.anim_acc += dt
        while self.anim_acc >= self.anim_frame_time:
            self.anim_acc -= self.anim_frame_time
            self.anim_i += 1
            if self.anim_i >= len(self.anim_frames):
                self._finish_anim()
                return

    def start_story(self):
        if not self.story_lines:
            self.state = "TITLE"
            self.sel = 0
            return

        self.story_line_h = 8
        self.story_total_h = len(self.story_lines) * self.story_line_h
        self.story_y = float(H)  # start below
        duration_s = 60.0
        total_scroll = H + self.story_total_h + 6
        self.story_speed = total_scroll / duration_s  # px/s
        self.state = "STORY"

    def update_story(self, dt: float):
        if not self.story_lines:
            self.state = "TITLE"
            self.sel = 0
            return
        self.story_y -= self.story_speed * dt
        if self.story_y < -self.story_total_h - 6:
            self.state = "TITLE"
            self.sel = 0

    def render_story(self):
        self.fb.fill(BLACK)
        x = 2
        for i, line in enumerate(self.story_lines):
            y = int(self.story_y + i * self.story_line_h)
            if y < -self.story_line_h or y > H:
                continue
            draw_text(self.fb, x, y, line[:14], WHITE)


    def current_cooldown(self) -> float:
        return max(PLAYER_COOLDOWN_MIN, PLAYER_BASE_COOLDOWN * self.slot.cooldown_factor)

    def score_add(self, amount: int) -> None:
        if self.slot.score_boost:
            amount = int(round(amount * 1.5))
        self.slot.score += amount

    def is_floor(self, x: int, y: int) -> bool:
        if 0 <= x < self.map_w and 0 <= y < self.map_h:
            return self.grid[y*self.map_w + x] == 1
        return False

    def can_place(self, x: int, y: int, offs: List[Tuple[int,int]]) -> bool:
        for dx, dy in offs:
            if not self.is_floor(x+dx, y+dy):
                return False
        return True

    # FIX: rectangle collision for spawn/move
    def can_place_rect(self, x: int, y: int, w: int, h: int) -> bool:
        for yy in range(h):
            for xx in range(w):
                if not self.is_floor(x+xx, y+yy):
                    return False
        return True


    def active_rect(self) -> tuple[int,int,int,int]:
        # Camera rect expanded by margin (world coordinates)
        # cam is computed in render_play(), but we compute it here too for logic
        cam_x = self.player_x - W//2
        cam_y = self.player_y - H//2
        m = ACTIVE_MARGIN
        return cam_x - m, cam_y - m, W + 2*m, H + 2*m

    def rects_intersect(self, ax:int, ay:int, aw:int, ah:int, bx:int, by:int, bw:int, bh:int) -> bool:
        return not (ax+aw <= bx or bx+bw <= ax or ay+ah <= by or by+bh <= ay)

    def enemy_is_active(self, e: Enemy) -> bool:
        rx, ry, rw, rh = self.active_rect()
        ew, eh = ENEMY_BOUNDS[e.etype]
        return self.rects_intersect(e.x, e.y, ew, eh, rx, ry, rw, rh)
    def player_offsets(self) -> List[Tuple[int,int]]:
        return PLAYER_FRAMES[self.player_face][self.step_phase]

    def enemy_offsets(self, etype: int) -> List[Tuple[int,int]]:
        return ENEMY_SHAPES[etype]

    def choose_axis_dir_to_player(self, ex: int, ey: int) -> Tuple[int,int]:
        dx = self.player_x - ex
        dy = self.player_y - ey
        if abs(dx) >= abs(dy):
            return (1,0) if dx > 0 else (-1,0)
        return (0,1) if dy > 0 else (0,-1)

    def try_move_player(self, dx: int, dy: int):
        nx = self.player_x + dx
        ny = self.player_y + dy
        face = "L" if dx < 0 else "R" if dx > 0 else "U" if dy < 0 else "D"
        if self.can_place(nx, ny, PLAYER_FRAMES[face][self.step_phase]):
            self.player_x, self.player_y = nx, ny
            self.player_face = face
            self.step_phase = (self.step_phase + 1) % len(FEET)

    def shoot_player(self):
        now = time.time()
        if now - self.last_shot_time < self.current_cooldown():
            return
        self.last_shot_time = now
        self.shots_fired_level += 1
        dx, dy = {"L":(-1,0), "R":(1,0), "U":(0,-1), "D":(0,1)}[self.player_face]
        bx = self.player_x + 1
        by = self.player_y + 1
        self.bullets.append(Bullet(bx, by, dx, dy, 0, 1))

    def reset_enemy_shoot(self, e: Enemy, rng: random.Random):
        a, b = ENEMY_SHOOT_RANGE[e.etype]
        e.shoot_timer = a if a == b else (a + rng.random() * (b - a))

    def shoot_enemy_single(self, e: Enemy, dx: int, dy: int, ox: int = 0, oy: int = 0, damage: int = 1):
        base_x = e.x + ENEMY_BOUNDS[e.etype][0]//2 + ox
        base_y = e.y + ENEMY_BOUNDS[e.etype][1]//2 + oy
        self.bullets.append(Bullet(base_x, base_y, dx, dy, e.eid, damage))

    def start_enemy_fire(self, e: Enemy, rng: random.Random):
        et = e.etype
        dx, dy = self.choose_axis_dir_to_player(e.x, e.y)

        if et == 4:
            for ddx, ddy in [(1,0),(-1,0),(0,1),(0,-1)]:
                self.shoot_enemy_single(e, ddx, ddy, damage=ENEMY_SHOT_DAMAGE[et])
            self.reset_enemy_shoot(e, rng)
            return

        if et == 3:
            e.burst_left = 3
            e.burst_delay = 0.0
            e.burst_dx, e.burst_dy = dx, dy
            self.reset_enemy_shoot(e, rng)
            return

        self.shoot_enemy_single(e, dx, dy, damage=ENEMY_SHOT_DAMAGE[et])
        self.reset_enemy_shoot(e, rng)

    def enemy_weights(self, L: int) -> Tuple[int,int,int,int]:
        w1 = 140
        w2 = clamp((L-4)*7, 0, 140)
        w3 = clamp((L-18)*6, 0, 110)
        w4 = clamp((L-45)*5, 0, 90)
        return int(w1), int(w2), int(w3), int(w4)

    def pick_enemy_type(self, rng: random.Random, L: int) -> int:
        w1,w2,w3,w4 = self.enemy_weights(L)
        total = w1+w2+w3+w4
        r = rng.randrange(total)
        if r < w1: return 1
        r -= w1
        if r < w2: return 2
        r -= w2
        if r < w3: return 3
        return 4

    def place_enemies(self, rng: random.Random, count: int):
        self.enemies = []
        self.next_eid = 1
        self.bullets = []
        self.enemies_total = 0
        self.kills_this_level = 0

        R_spawn = 70
        R_exit = 70
        R_min = 44

        tries_limit = 28000
        attempts = 0
        placed = 0

        e4_cap = 1 + self.slot.level//25
        e4_count = 0

        while placed < count and attempts < tries_limit:
            attempts += 1
            et = self.pick_enemy_type(rng, self.slot.level)
            if et == 4 and e4_count >= e4_cap:
                et = rng.choice([1,2,3])

            bw, bh = ENEMY_BOUNDS[et]
            x = rng.randrange(0, max(1, self.map_w - bw - 2))
            y = rng.randrange(0, max(1, self.map_h - bh - 2))

            if dist2((x,y), self.start) < R_spawn*R_spawn:
                continue
            if dist2((x,y), self.exitp) < R_exit*R_exit:
                continue

            ok = True
            for ee in self.enemies:
                if dist2((x,y), (ee.x,ee.y)) < R_min*R_min:
                    ok = False; break
            if not ok:
                continue

            if not self.can_place_rect(x, y, bw, bh):
                continue

            e = Enemy(self.next_eid, et, x, y, ENEMY_HP[et])
            self.reset_enemy_shoot(e, rng)
            self.next_eid += 1
            self.enemies.append(e)
            placed += 1
            if et == 4:
                e4_count += 1

        self.enemies_total = len(self.enemies)

    def load_level(self, level: int):
        self.slot.level = level
        self.player_hp = self.slot.max_hp
        self.iframes_until = 0.0
        self.exit_unlocked = (self.slot.difficulty == "EASY")

        self.grid, self.map_w, self.map_h, self.start, self.exitp = generate_cave(self.slot.seed, self.slot.level)

        ex0 = self.exitp[0] - EXIT_BOUNDS[0]//2
        ey0 = self.exitp[1] - EXIT_BOUNDS[1]//2
        for dx, dy in EXIT_OFFS:
            x = ex0 + dx; y = ey0 + dy
            if 0 <= x < self.map_w and 0 <= y < self.map_h:
                self.grid[y*self.map_w + x] = 1

        self.player_x, self.player_y = self.start
        self.player_face = "R"
        self.step_phase = 0
        self.last_shot_time = 0.0

        enemy_count = int(clamp(level, 1, 180))
        rng = random.Random(level_seed(self.slot.seed, self.slot.level) ^ 0x13579BDF)
        self.place_enemies(rng, enemy_count)

        if self.slot.difficulty in ("EASY","NORMAL"):
            self.write_checkpoint_from_current()

        self.set_slot(self.slot_index, self.slot)
        self.message = f"NIV {level}"
        self.message_t = time.time()


        # scoring metrics reset per level
        self.level_start_time = time.time()
        self.shots_fired_level = 0
        self.damage_taken_level = 0
        self.level_bonus_last = 0
    # ----- bonuses -----
    def unique_bonus_available(self) -> Optional[str]:
        L = self.slot.level
        if L >= 20 and (not self.slot.enemy_arrow) and self.slot.difficulty in ("NORMAL","HARDCORE"):
            return "UNQ:FLECHE ENN"
        if L >= 40 and (not self.slot.pierce_forever):
            return "UNQ:BALLES PERCE"
        if L >= 60 and (not self.slot.exit_half_kills):
            return "UNQ:SORTIE 50%"
        if L >= 90 and (not self.slot.doubled_once):
            return "UNQ:SCORE X2"
        return None

    def bonus_options(self) -> List[str]:
        opts: List[str] = []
        u = self.unique_bonus_available()
        if u:
            opts.append(u)
        # Permanent bonuses
        opts += [
            "PV +3 MAX",
        ]
        # Offer CADENCE only if it still has an effect (not at min cap)
        if self.current_cooldown() > PLAYER_COOLDOWN_MIN + 1e-6:
            opts.append("CADENCE X2/3")
        opts += [
            "SCORE +50%" if not self.slot.score_boost else "SCORE +50% OK",
            "SAUTER 1 NIV",
        ]
        if self.slot.difficulty == "HARDCORE":
            opts.append("SAUVEGARDER")
        return opts

    def apply_bonus_choice(self, idx: int):
        choice = self.bonus_options()[idx]
        now = time.time()
        if choice.startswith("UNQ:"):
            if "FLECHE" in choice:
                self.slot.enemy_arrow = True
                self.message = "FLECHE ENN OK"
            elif "PERCE" in choice:
                self.slot.pierce_forever = True
                self.message = "PERCE OK"
            elif "SORTIE" in choice:
                self.slot.exit_half_kills = True
                self.message = "SORTIE 50% OK"
            elif "X2" in choice:
                self.slot.score *= 2
                self.slot.doubled_once = True
                self.message = "SCORE X2!"
            self.message_t = now
        elif choice.startswith("PV +3"):
            self.slot.max_hp = int(clamp(self.slot.max_hp + 3, 3, 300))
            self.message = "PV MAX +3"
            self.message_t = now
        elif choice.startswith("CADENCE"):
            self.slot.cooldown_factor = max(0.12, self.slot.cooldown_factor * (2.0/3.0))
            self.message = "CADENCE +"
            self.message_t = now
        elif choice.startswith("SCORE +50%"):
            if not self.slot.score_boost:
                self.slot.score_boost = True
                self.message = "SCORE BOOST!"
            else:
                self.message = "DEJA OK"
            self.message_t = now
        elif choice.startswith("SAUTER"):
            self.slot.skip_next_level = True
            self.message = "SAUTER OK"
            self.message_t = now
        elif choice.startswith("SAUVEGARDER"):
            self.write_checkpoint_from_current()
            self.message = "CHECKPOINT OK"
            self.message_t = now

        self.set_slot(self.slot_index, self.slot)
        self.state = "PLAY"

    # ----- damage + flashes -----
    def start_damage_flash(self):
        # 3 flashes => 6 toggles (on/off)
        self.flash_steps_left = 6
        self.flash_next_t = time.time()
        self.flash_on = True

    def damage_player(self, dmg: int, now: float, msg: str):
        if now < self.iframes_until:
            return
        self.damage_taken_level += dmg
        self.player_hp -= dmg
        self.iframes_until = now + PLAYER_IFRAME_SEC
        self.message = msg
        self.message_t = now
        self.start_damage_flash()

    def update_flash(self):
        if self.flash_steps_left <= 0:
            self.flash_on = False
            return
        now = time.time()
        if now >= self.flash_next_t:
            self.flash_on = not self.flash_on
            self.flash_steps_left -= 1
            self.flash_next_t = now + self.flash_step_dt

    # ----- collisions -----
    def update_touch_damage(self):
        now = time.time()
        pset = set((self.player_x+dx, self.player_y+dy) for dx,dy in self.player_offsets())
        for e in self.enemies:
            for dx, dy in self.enemy_offsets(e.etype):
                if (e.x+dx, e.y+dy) in pset:
                    self.damage_player(1, now, "AIE!")
                    return

    def update_bullets(self):
        now = time.time()
        for b in self.bullets:
            if not b.alive:
                continue
            b.x += b.dx
            b.y += b.dy
            if not self.is_floor(b.x, b.y):
                b.alive = False
                continue

            # ESP-like: don't simulate projectiles too far from camera
            rx, ry, rw, rh = self.active_rect()
            if not (rx <= b.x < rx+rw and ry <= b.y < ry+rh):
                b.alive = False
                continue

            if b.owner_id != 0:
                for dx, dy in self.player_offsets():
                    if b.x == self.player_x+dx and b.y == self.player_y+dy:
                        self.damage_player(b.damage, now, "TOUCHE!")
                        b.alive = False
                        break
                continue

            hit: Optional[Enemy] = None
            for e in self.enemies:
                for dx, dy in self.enemy_offsets(e.etype):
                    if b.x == e.x+dx and b.y == e.y+dy:
                        hit = e
                        break
                if hit:
                    break
            if hit:
                hit.hp -= b.damage
                if hit.hp <= 0:
                    self.score_add(SCORE_KILL[hit.etype])
                    self.kills_this_level += 1
                    dead_id = hit.eid
                    self.enemies = [e for e in self.enemies if e.eid != dead_id]
                    for bb in self.bullets:
                        if bb.owner_id == dead_id:
                            bb.alive = False
                if not self.slot.pierce_forever:
                    b.alive = False

        self.bullets = [b for b in self.bullets if b.alive]

    def try_move_enemy(self, e: Enemy, dx: int, dy: int) -> bool:
        nx, ny = e.x + dx, e.y + dy
        bw, bh = ENEMY_BOUNDS[e.etype]
        if self.can_place_rect(nx, ny, bw, bh):
            e.x, e.y = nx, ny
            return True
        return False

    def update_enemies(self, dt: float):
        rng = random.Random(0xC0FFEE ^ (self.slot.seed & 0xFFFFFFFF) ^ (self.slot.level*1337))
        for e in self.enemies:
            # ESP-like optimization: only enemies near/visible are active
            if not self.enemy_is_active(e):
                continue
            e.move_tick += 1
            d2p = dist2((e.x,e.y), (self.player_x,self.player_y))
            aggro12 = d2p <= AGGRO_R_12*AGGRO_R_12
            aggro34 = d2p <= AGGRO_R_34*AGGRO_R_34

            if e.etype == 3 and e.burst_left > 0:
                e.burst_delay -= dt
                if e.burst_delay <= 0.0:
                    dx, dy = e.burst_dx, e.burst_dy
                    if dx != 0:
                        self.shoot_enemy_single(e, dx, dy, oy=-1, damage=ENEMY_SHOT_DAMAGE[3])
                        self.shoot_enemy_single(e, dx, dy, oy=+1, damage=ENEMY_SHOT_DAMAGE[3])
                    else:
                        self.shoot_enemy_single(e, dx, dy, ox=-1, damage=ENEMY_SHOT_DAMAGE[3])
                        self.shoot_enemy_single(e, dx, dy, ox=+1, damage=ENEMY_SHOT_DAMAGE[3])
                    e.burst_left -= 1
                    e.burst_delay = 0.12

            if e.step_left <= 0:
                if e.etype in (1,2) and aggro12:
                    e.dir = self.choose_axis_dir_to_player(e.x, e.y)
                    e.step_left = rng.randint(7, 16)
                    e.pause_left = rng.randint(0, 1)
                elif e.etype in (3,4) and aggro34:
                    e.dir = self.choose_axis_dir_to_player(e.x, e.y)
                    e.step_left = rng.randint(3, 7)
                    e.pause_left = rng.randint(0, 3)
                else:
                    e.dir = rng.choice(DIRS)
                    e.step_left = rng.randint(2, 7)
                    e.pause_left = rng.randint(0, 8)

            if e.pause_left > 0:
                e.pause_left -= 1
            else:
                ticks = ENEMY_TICKS_AGGRO[e.etype] if ((e.etype in (1,2) and aggro12) or (e.etype in (3,4) and aggro34)) else ENEMY_TICKS_NORMAL[e.etype]
                if e.move_tick % ticks == 0:
                    moved = self.try_move_enemy(e, e.dir[0], e.dir[1])
                    e.step_left -= 1
                    if not moved:
                        e.step_left = 0
                        e.pause_left = rng.randint(1, 8)

            e.shoot_timer -= dt
            can_shoot = ((e.etype in (1,2) and aggro12) or (e.etype in (3,4) and aggro34))
            if can_shoot and e.shoot_timer <= 0.0:
                self.start_enemy_fire(e, rng)

    # ----- gameplay update -----
    def player_in_exit(self) -> bool:
        ex0 = self.exitp[0] - EXIT_BOUNDS[0]//2
        ey0 = self.exitp[1] - EXIT_BOUNDS[1]//2
        px, py = self.player_x, self.player_y
        pw, ph = PLAYER_BOUNDS
        return not (px+pw < ex0 or px > ex0+EXIT_BOUNDS[0] or py+ph < ey0 or py > ey0+EXIT_BOUNDS[1])

    def award_level_completion_score(self) -> None:
        """Adds the end-of-level bonus score (kills are already awarded on each kill).
        Tries to reward speed/efficiency/survival without any single factor ruining a run."""
        now = time.time()
        elapsed = max(0.0, now - self.level_start_time)

        # Time budget: 60s base + 10s per enemy intended for this level.
        target = 60.0 + 10.0 * max(0, self.enemies_total)

        # Speed bonus: points per second saved (never negative).
        time_saved = max(0.0, target - elapsed)
        time_score = int(time_saved * 3.0)

        # Completion bonus scales with level.
        completion = 50 + int(self.slot.level * 2)

        # Survival bonus (based on HP left at the end of the level).
        mhp = max(1, int(self.slot.max_hp))
        hp_score = int((self.player_hp / mhp) * 80)

        # Efficiency bonus: mild penalty for spamming shots, normalized by enemy count.
        shots = int(self.shots_fired_level)
        denom = max(1, int(self.enemies_total))
        acc_pen = int((shots * 15) / denom)
        acc_score = max(0, 200 - acc_pen)

        # Damage taken penalty (mild).
        dmg_pen = int(self.damage_taken_level) * 20

        bonus = completion + time_score + hp_score + acc_score - dmg_pen
        if bonus < 0:
            bonus = 0

        self.level_bonus_last = bonus
        self.score_add(bonus)


    # ----- loop -----
    def run(self):
        while self.running:
            dt = self.clock.tick(FPS) / 1000.0
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False
                elif event.type == pygame.KEYDOWN:
                    self.on_key(event.key)
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    self.on_mouse(event)
            self.update(dt)
            self.render()
        pygame.quit()

    def on_key(self, key):
        if self.state == "PLAY":
            if key == pygame.K_ESCAPE:
                self.state = "PAUSE"; self.sel = 0
            return

        if self.state == "PAUSE":
            if key in (pygame.K_UP, pygame.K_DOWN):
                self.sel = (self.sel + (1 if key == pygame.K_DOWN else -1)) % 2
            elif key == pygame.K_RETURN:
                if self.sel == 0:
                    self.state = "PLAY"
                else:
                    self.state = "TITLE"; self.sel = 0
            elif key == pygame.K_ESCAPE:
                self.state = "PLAY"
            return

        if self.state == "GAME_OVER":
            if key in (pygame.K_UP, pygame.K_DOWN):
                self.sel = 1 - self.sel
            elif key == pygame.K_RETURN:
                if self.sel == 0:
                    self.restore_from_checkpoint()
                    self.state = "PLAY"
                else:
                    self.state = "TITLE"; self.sel = 0
            return

        # Animation: allow skip (SPACE/ENTER/ESC)
        if self.state == "ANIM":
            if key in (pygame.K_SPACE, pygame.K_RETURN, pygame.K_ESCAPE):
                self._finish_anim()
            return

        # Story: allow exit
        if self.state == "STORY":
            if key in (pygame.K_SPACE, pygame.K_RETURN, pygame.K_ESCAPE):
                self.state = "TITLE"; self.sel = 0
            return

        # global back
        if key == pygame.K_ESCAPE:
            if self.state == "TITLE":
                self.running = False
            elif self.state in ("SLOTS","SCORES","STORY"):
                self.state = "TITLE"; self.sel = 0
            elif self.state in ("NEW_NAME","SEED_MENU","NEW_CODE","NEW_DIFF","CONFIRM_ERASE"):
                self.state = "SLOTS"; self.sel = 0
            return

        if self.state == "TITLE":
            if key in (pygame.K_UP, pygame.K_DOWN):
                self.sel = (self.sel + (1 if key == pygame.K_DOWN else -1)) % len(self.get_slots())
            elif key == pygame.K_RETURN:
                if self.sel == 0:
                    self.state = "SLOTS"; self.sel = 0
                elif self.sel == 1:
                    self.state = "SCORES"; self.score_scroll = 0
                else:
                    self.start_story()
            return

        if self.state == "SCORES":
            scores = [ScoreEntry(**e) for e in self.save.get("scores", [])]
            max_off = max(0, len(scores) - 4)
            if key == pygame.K_DOWN:
                self.score_scroll = min(max_off, self.score_scroll + 1)
            elif key == pygame.K_UP:
                self.score_scroll = max(0, self.score_scroll - 1)
            return

        if self.state == "SLOTS":
            if key in (pygame.K_UP, pygame.K_DOWN):
                self.sel = (self.sel + (1 if key == pygame.K_DOWN else -1)) % 3
            elif key in (pygame.K_RETURN, pygame.K_RIGHT):
                # Validate / Right: open slot (load) or start new
                self.slot_index = self.sel
                s = self.get_slots()[self.slot_index]
                if not s.used:
                    self.new_name = ["A","A","A"]
                    self.new_code_digits = ["0"]*8
                    self.new_code_pos = 0
                    self.diff_sel = 1
                    self.seed_sel = 0
                    self.state = "NEW_NAME"; self.sel = 0
                else:
                    self.slot = s
                    self.load_level(self.slot.level)
                    self.state = "PLAY"
            elif key == pygame.K_LEFT:
                # Left: request erase on used slot
                s = self.get_slots()[self.sel]
                if s.used:
                    self.slot_index = self.sel
                    self.state = "CONFIRM_ERASE"; self.sel = 0
            return

            return

        if self.state == "CONFIRM_ERASE":
            if key in (pygame.K_LEFT, pygame.K_RIGHT):
                self.sel = 1 - self.sel
            elif key in (pygame.K_RETURN, pygame.K_RIGHT):
                if self.sel == 1:
                    self.clear_slot(self.slot_index)
                self.state = "SLOTS"; self.sel = 0
            elif key == pygame.K_BACKSPACE:
                # optional fallback: backspace toggles too
                self.sel = 1 - self.sel
            return

        if self.state == "NEW_NAME":
            if key in (pygame.K_LEFT, pygame.K_RIGHT):
                self.sel = (self.sel + (1 if key == pygame.K_RIGHT else -1)) % 3
            elif key in (pygame.K_UP, pygame.K_DOWN):
                c = ord(self.new_name[self.sel])
                c += 1 if key == pygame.K_UP else -1
                if c < ord('A'): c = ord('Z')
                if c > ord('Z'): c = ord('A')
                self.new_name[self.sel] = chr(c)
            elif key == pygame.K_RETURN:
                self.state = "SEED_MENU"; self.sel = 0
            return

        if self.state == "SEED_MENU":
            if key in (pygame.K_UP, pygame.K_DOWN):
                self.seed_sel = 1 - self.seed_sel
            elif key == pygame.K_RETURN:
                if self.seed_sel == 0:
                    num = random.getrandbits(32) % 100000000
                    self.new_code_digits = list(f"{num:08d}")
                    self.state = "NEW_DIFF"
                else:
                    self.state = "NEW_CODE"
            return

        if self.state == "NEW_CODE":
            if key in (pygame.K_LEFT, pygame.K_RIGHT):
                self.new_code_pos = (self.new_code_pos + (1 if key == pygame.K_RIGHT else -1)) % 8
            elif key in (pygame.K_UP, pygame.K_DOWN):
                d = int(self.new_code_digits[self.new_code_pos])
                d = (d + (1 if key == pygame.K_UP else -1)) % 10
                self.new_code_digits[self.new_code_pos] = str(d)
            elif key == pygame.K_RETURN:
                self.state = "NEW_DIFF"
            return

        if self.state == "NEW_DIFF":
            if key in (pygame.K_UP, pygame.K_DOWN):
                self.diff_sel = (self.diff_sel + (1 if key == pygame.K_DOWN else -1)) % 3
            elif key == pygame.K_RETURN:
                name = "".join(self.new_name)
                seed = int("".join(self.new_code_digits))
                diff = ["EASY","NORMAL","HARDCORE"][self.diff_sel]
                slot = Slot(
                    used=True, name=name, difficulty=diff, seed=seed, level=1, score=0,
                    cooldown_factor=1.0, max_hp=3, score_boost=False, skip_next_level=False,
                    enemy_arrow=False, pierce_forever=False, exit_half_kills=False, doubled_once=False,
                    checkpoint_level=0
                )
                self.set_slot(self.slot_index, slot)
                self.slot = slot
                self.load_level(1)
                self.state = "PLAY"
            return

        if self.state == "BONUS":
            if key in (pygame.K_UP, pygame.K_DOWN):
                self.sel = (self.sel + (1 if key == pygame.K_DOWN else -1)) % len(self.bonus_options())
            elif key == pygame.K_RETURN:
                self.apply_bonus_choice(self.sel)
            return

    def on_mouse(self, event):
        # Mouse support (useful for desktop testing)
        # Left click on a slot -> if used: confirm erase ; if empty: start new game flow
        if event.button != 1:
            return
        mx, my = event.pos
        fx = mx // SCALE
        fy = my // SCALE

        if self.state == "SLOTS":
            # slots list lines start at y=14, step 10, 3 entries
            if 14 <= fy < 14 + 10*3:
                idx = (fy - 14) // 10
                if 0 <= idx < 3:
                    self.sel = int(idx)
                    s = self.get_slots()[self.sel]
                    if s.used:
                        self.slot_index = self.sel
                        self.state = "CONFIRM_ERASE"
                        self.sel = 0
                    else:
                        # start new game flow (same as Enter)
                        self.slot_index = self.sel
                        self.new_name = ["A","A","A"]
                        self.new_code_digits = ["0"]*8
                        self.new_code_pos = 0
                        self.diff_sel = 1
                        self.seed_sel = 0
                        self.state = "NEW_NAME"
                        self.sel = 0
            return

        if self.state == "CONFIRM_ERASE":
            # Two options centered at y=26 and y=36 (approx), make them clickable
            # We'll map click to nearest option by y.
            if 24 <= fy <= 46:
                opt = 0 if fy < 35 else 1  # ANNUL / EFFACE
                self.sel = opt
                if self.sel == 1:
                    self.clear_slot(self.slot_index)
                self.state = "SLOTS"
                self.sel = 0
            return

        if self.state == "TITLE":
            # Click on menu lines (centered). Simple mapping by y band.
            if 24 <= fy <= 46:
                opt = 0 if fy < 35 else 1
                self.sel = opt
                if self.sel == 0:
                    self.state = "SLOTS"; self.sel = 0
                else:
                    self.state = "SCORES"; self.score_scroll = 0
            return

        if self.state == "GAME_OVER":
            if 36 <= fy <= 47:
                opt = 0 if fy < 42 else 1
                self.sel = opt
                if self.sel == 0:
                    self.restore_from_checkpoint()
                    self.state = "PLAY"
                else:
                    self.state = "TITLE"; self.sel = 0
            return

        if self.state == "PAUSE":
            if 38 <= fy <= 47:
                opt = 0 if fy < 43 else 1
                self.sel = opt
                if self.sel == 0:
                    self.state = "PLAY"
                else:
                    self.state = "TITLE"; self.sel = 0
            return

    def update(self, dt: float):
        if self.state == "PLAY":
            self.update_flash()
            
            keys = pygame.key.get_pressed()
            if keys[pygame.K_LEFT]:
                self.try_move_player(-1, 0)
            elif keys[pygame.K_RIGHT]:
                self.try_move_player(1, 0)
            elif keys[pygame.K_UP]:
                self.try_move_player(0, -1)
            elif keys[pygame.K_DOWN]:
                self.try_move_player(0, 1)
            
            if keys[pygame.K_SPACE] or keys[pygame.K_RETURN]:
                self.shoot_player()
            
            self.update_enemies(dt)
            self.update_bullets()
            self.update_touch_damage()
            
            # exit unlock
            if not self.exit_unlocked and self.slot.difficulty != "EASY":
                if len(self.enemies) == 0:
                    self.exit_unlocked = True
                    self.message = "SORTIE OK"
                    self.message_t = time.time()
                elif self.slot.exit_half_kills and self.enemies_total > 0:
                    if self.kills_this_level >= (self.enemies_total + 1)//2:
                        self.exit_unlocked = True
                        self.message = "SORTIE 50%"
                        self.message_t = time.time()
            
            # exit reached
            if self.exit_unlocked and self.player_in_exit():
                self.award_level_completion_score()
                # brief feedback
                if self.level_bonus_last:
                    self.message = f"+{self.level_bonus_last}"
                    self.message_t = time.time()
            
                if self.slot.level % 5 == 0:
                    self.state = "BONUS"; self.sel = 0
            
                inc = 2 if self.slot.skip_next_level else 1
                self.slot.skip_next_level = False
                next_level = self.slot.level + inc
            
                self.set_slot(self.slot_index, self.slot)

                # Victory condition: finishing level 100 ends the run
                if next_level > 100:
                    # Save to scoreboard on victory as well
                    self.go_level = self.slot.level
                    self.go_score = self.slot.score
                    self.go_diff = self.diff_short(self.slot.difficulty)
                    self.add_score(ScoreEntry(self.slot.name, self.slot.score, self.slot.difficulty, self.slot.seed))

                    if self.slot.difficulty == "HARDCORE":
                        self.start_anim("ending_hard", next_state="TITLE", frame_time=0.045)
                    else:
                        self.start_anim("ending_normal", next_state="TITLE", frame_time=0.055)
                    return

                self.load_level(next_level)
            
            if self.player_hp <= 0:
                self.go_level = self.slot.level
                self.go_score = self.slot.score
                self.go_diff = self.diff_short(self.slot.difficulty)
                self.add_score(ScoreEntry(self.slot.name, self.slot.score, self.slot.difficulty, self.slot.seed))
                self.state = "GAME_OVER"
                self.sel = 0
            
            return

        if self.state == "ANIM":
            self.update_anim(dt)
            return

        if self.state == "STORY":
            self.update_story(dt)
            return

        return

    # ----- rendering helpers -----
    def fb_set(self, sx: int, sy: int, col):
        if 0 <= sx < W and 0 <= sy < H:
            self.fb.set_at((sx, sy), col)

    def fb_xor_from_world(self, sx: int, sy: int):
        wx = self.cam_x + sx
        wy = self.cam_y + sy
        col = BLACK if self.is_floor(wx, wy) else WHITE
        self.fb_set(sx, sy, col)

    def invert_fb(self):
        # 84x48 = 4032 px: simple loop is fine
        for y in range(H):
            for x in range(W):
                self.fb_set(x, y, WHITE if self.fb.get_at((x, y)) == BLACK else BLACK)

    def render_exit_arrow(self):
        if not self.exit_unlocked:
            return
        dx = self.exitp[0] - self.player_x
        dy = self.exitp[1] - self.player_y
        if abs(dx) >= abs(dy):
            if dx > 0:
                offs = ARROW_R; sx0 = W - ARROW_BOUNDS[0] - 1
            else:
                offs = ARROW_L; sx0 = 1
            sy0 = H//2 - ARROW_BOUNDS[1]//2
        else:
            if dy > 0:
                offs = ARROW_D; sy0 = H - ARROW_BOUNDS[1] - 1
            else:
                offs = ARROW_U; sy0 = 1
            sx0 = W//2 - ARROW_BOUNDS[0]//2
        for ox, oy in offs:
            self.fb_xor_from_world(sx0+ox, sy0+oy)

    def render_enemy_arrow(self):
        if not self.slot.enemy_arrow or len(self.enemies) == 0:
            return
        pe = (self.player_x, self.player_y)
        nearest = min(self.enemies, key=lambda e: dist2((e.x,e.y), pe))
        dx = nearest.x - self.player_x
        dy = nearest.y - self.player_y
        if abs(dx) >= abs(dy):
            offs = ARROW_R if dx > 0 else ARROW_L
            sx0 = W - ARROW_BOUNDS[0] - 1 if dx > 0 else 1
            sy0 = 1
        else:
            offs = ARROW_D if dy > 0 else ARROW_U
            sy0 = H - ARROW_BOUNDS[1] - 1 if dy > 0 else 1
            sx0 = 1
        for ox, oy in offs:
            self.fb_xor_from_world(sx0+ox, sy0+oy)

    def render(self):
        self.fb.fill(BLACK)

        if self.state == "ANIM":
            if self.anim_frames:
                self.fb.blit(self.anim_frames[self.anim_i], (0, 0))
            self.blit_fb()
            return

        if self.state == "STORY":
            self.render_story()
            self.blit_fb()
            return

        if self.state == "TITLE":
            draw_center(self.fb, 0, "CAVROGUE", WHITE)
            items = ["JOUER", "SCORES", "STORY"]
            y0 = 12
            line_h = 8
            for i, t in enumerate(items):
                prefix = ">" if i == self.sel else " "
                draw_center(self.fb, y0 + i*line_h, f"{prefix} {t}", WHITE)
            self.blit_fb()
            return

        if self.state == "SCORES":
            draw_center(self.fb, 0, "TOP SCORES", WHITE)
            scores = [ScoreEntry(**e) for e in self.save.get("scores", [])]
            off = self.score_scroll
            view = scores[off:off+4]
            y = 8
            for i, s in enumerate(view):
                idx = off + i + 1
                line = f"{idx}.{s.name} {s.score}"
                draw_text(self.fb, 0, y, line[:14], WHITE)
                y += 8
            self.blit_fb()
            return

        if self.state in ("SLOTS","CONFIRM_ERASE","NEW_NAME","SEED_MENU","NEW_CODE","NEW_DIFF"):
            self.render_slots_and_new()
            self.blit_fb()
            return

        if self.state == "PAUSE":
            self.render_pause()
            self.blit_fb()
            return

        if self.state == "BONUS":
            self.render_bonus()
            self.blit_fb()
            return

        if self.state == "GAME_OVER":
            self.render_game_over()
            self.blit_fb()
            return

        # PLAY
        self.render_play()
        # flash effect (invert fb)
        if self.flash_on:
            self.invert_fb()
        self.blit_fb()

    def blit_fb(self):
        scaled = pygame.transform.scale(self.fb, (W*SCALE, H*SCALE))
        self.win.blit(scaled, (0,0))
        pygame.display.flip()

    # ----- menu renders -----
    def render_slots_and_new(self):
        slots = self.get_slots()

        if self.state == "SLOTS":
            draw_center(self.fb, 0, "SLOTS", WHITE)
            y = 8
            for i, s in enumerate(slots):
                prefix = ">" if i == self.sel else " "
                if not s.used:
                    line = f"{prefix} {i+1}:VIDE"
                else:
                    line = f"{prefix}{i+1}:{s.name} N{s.level}"
                draw_text(self.fb, 0, y, line[:14], WHITE)
                y += 8
            return

        if self.state == "CONFIRM_ERASE":
            draw_center(self.fb, 6, "EFFACER?", WHITE)
            opts = ["ANNUL", "EFFACE"]
            y0 = 22
            for i, t in enumerate(opts):
                prefix = ">" if i == self.sel else " "
                draw_center(self.fb, y0 + i*9, f"{prefix}{t}", WHITE)
            return

        if self.state == "NEW_NAME":
            draw_center(self.fb, 0, "NEW GAME", WHITE)
            draw_center(self.fb, 12, "NOM", WHITE)
            s = "".join(self.new_name)
            draw_center(self.fb, 24, s, WHITE)
            cx = (W - text_width(s))//2 + self.sel*6
            for xx in range(5):
                self.fb_set(cx+xx, 32, WHITE)
            return

        if self.state == "SEED_MENU":
            draw_center(self.fb, 0, "SEED", WHITE)
            opts = ["ALEATOIRE", "PERSO"]
            for i, t in enumerate(opts):
                prefix = ">" if i == self.seed_sel else " "
                draw_center(self.fb, 20 + i*10, f"{prefix}{t}", WHITE)
            return

        if self.state == "NEW_CODE":
            draw_center(self.fb, 0, "SEED PERSO", WHITE)
            digits = "".join(self.new_code_digits)
            draw_center(self.fb, 20, digits, WHITE)
            cx = (W - text_width(digits))//2 + self.new_code_pos*6
            for xx in range(5):
                self.fb_set(cx+xx, 28, WHITE)
            return

        if self.state == "NEW_DIFF":
            draw_center(self.fb, 0, "DIFF", WHITE)
            opts = ["EASY","NORM","HARD"]
            y0 = 16
            for i, t in enumerate(opts):
                prefix = ">" if i == self.diff_sel else " "
                draw_center(self.fb, y0 + i*9, f"{prefix}{t}", WHITE)
            return

    def render_pause(self):
        draw_center(self.fb, 0, "PAUSE", WHITE)
        d = self.diff_short(self.slot.difficulty)
        draw_text(self.fb, 0, 8,  f"{d} S{self.slot.score}", WHITE)
        draw_text(self.fb, 0, 16, f"N{self.slot.level} HP{self.player_hp}/{self.slot.max_hp}", WHITE)
        draw_text(self.fb, 0, 24, f"E{len(self.enemies)}/{self.enemies_total}", WHITE)
        items = ["REPR", "MENU"]
        y0 = 32
        for i, t in enumerate(items):
            prefix = ">" if i == self.sel else " "
            draw_center(self.fb, y0 + i*8, f"{prefix}{t}", WHITE)

    def render_bonus(self):
        draw_center(self.fb, 0, f"BONUS N{self.slot.level}", WHITE)
        opts = self.bonus_options()
        y = 8
        for i, t in enumerate(opts[:4]):  # fit
            prefix = ">" if i == self.sel else " "
            draw_text(self.fb, 0, y, (prefix + t)[:14], WHITE)
            y += 8

    def render_game_over(self):
        draw_center(self.fb, 0, "GAME OVER", WHITE)
        draw_center(self.fb, 10, f"SCORE {self.go_score}", WHITE)
        draw_center(self.fb, 18, f"{self.go_diff} N{self.go_level}", WHITE)
        opts = ["CONT", "QUIT"]
        y0 = 32
        for i, t in enumerate(opts):
            prefix = ">" if i == self.sel else " "
            draw_center(self.fb, y0 + i*8, f"{prefix}{t}", WHITE)

    def render_play(self):
        self.cam_x = self.player_x - W//2
        self.cam_y = self.player_y - H//2

        for sy in range(H):
            wy = self.cam_y + sy
            if 0 <= wy < self.map_h:
                row = wy * self.map_w
                for sx in range(W):
                    wx = self.cam_x + sx
                    if 0 <= wx < self.map_w and self.grid[row + wx]:
                        self.fb_set(sx, sy, WHITE)

        if self.exit_unlocked:
            blink = (int(time.time()*3) % 2) == 0
            ex0 = self.exitp[0] - EXIT_BOUNDS[0]//2
            ey0 = self.exitp[1] - EXIT_BOUNDS[1]//2
            for dx, dy in EXIT_OFFS:
                sx = (ex0+dx) - self.cam_x
                sy = (ey0+dy) - self.cam_y
                if 0 <= sx < W and 0 <= sy < H:
                    self.fb_set(sx, sy, BLACK if blink else WHITE)

        for e in self.enemies:
            for dx, dy in self.enemy_offsets(e.etype):
                sx = (e.x+dx) - self.cam_x
                sy = (e.y+dy) - self.cam_y
                if 0 <= sx < W and 0 <= sy < H:
                    self.fb_set(sx, sy, BLACK)

        for b in self.bullets:
            sx = b.x - self.cam_x
            sy = b.y - self.cam_y
            if 0 <= sx < W and 0 <= sy < H:
                self.fb_xor_from_world(sx, sy)

        # player blink during iframes
        now = time.time()
        show_player = True
        if now < self.iframes_until:
            show_player = (int(now * 10) % 2) == 0
        if show_player:
            for dx, dy in self.player_offsets():
                sx = (self.player_x+dx) - self.cam_x
                sy = (self.player_y+dy) - self.cam_y
                if 0 <= sx < W and 0 <= sy < H:
                    self.fb_set(sx, sy, BLACK)

        self.render_exit_arrow()
        self.render_enemy_arrow()

        # message (brief)
        if self.message and (time.time() - self.message_t) < 0.9:
            draw_center(self.fb, 40, self.message[:14], WHITE)

def main():
    pygame.init()
    Game().run()

if __name__ == "__main__":
    main()
