#pragma once
#include <stdint.h>
#include <pgmspace.h>

namespace SPR {

  // ---------- Enemy sprites ----------
  // Exemple: PING (8x8)
  static const uint8_t ENEMY_PING_8x8[] PROGMEM = {
    // 8x8 => 1 byte/row * 8 rows = 8 bytes
    0b00111100,
    0b01000010,
    0b10100101,
    0b10000001,
    0b10100101,
    0b10011001,
    0b01000010,
    0b00111100,
  };

  // Exemple: SCAN (8x8)
  static const uint8_t ENEMY_SCAN_8x8[] PROGMEM = {
    0b00111100,
    0b01000010,
    0b10011001,
    0b10111101,
    0b10111101,
    0b10011001,
    0b01000010,
    0b00111100,
  };

  // Exemple: DAEMON (8x8)
  static const uint8_t ENEMY_DAEMON_8x8[] PROGMEM = {
    0b00111100,
    0b01111110,
    0b11111111,
    0b11011011,
    0b11111111,
    0b11100111,
    0b01111110,
    0b00111100,
  };

  // Exemple: FIREWALL (16x16)
  static const uint8_t ENEMY_FIREWALL_16x16[] PROGMEM = {
    // 16x16 => 32 bytes
    0xFF,0xFF,
    0x81,0x81,
    0xBD,0xBD,
    0xA5,0xA5,
    0xBD,0xBD,
    0x81,0x81,
    0xFF,0xFF,
    0x00,0x00,
    0xFF,0xFF,
    0x81,0x81,
    0xBD,0xBD,
    0xA5,0xA5,
    0xBD,0xBD,
    0x81,0x81,
    0xFF,0xFF,
    0x00,0x00,
  };

  // ---------- Registry ----------
  struct Sprite {
    const uint8_t* data;
    uint8_t w;
    uint8_t h;
  };

  enum EnemySpriteId : uint8_t {
    E_PING = 0,
    E_SCAN,
    E_DAEMON,
    E_FIREWALL,
    E_COUNT
  };

  static const Sprite ENEMY_SPRITES[E_COUNT] PROGMEM = {
    { ENEMY_PING_8x8,      8,  8  },
    { ENEMY_SCAN_8x8,      8,  8  },
    { ENEMY_DAEMON_8x8,    8,  8  },
    { ENEMY_FIREWALL_16x16,16, 16 },
  };

} // namespace SPR
