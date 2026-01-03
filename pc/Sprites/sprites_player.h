#pragma once
#include <stdint.h>
#include <pgmspace.h>

// Format sprite (1-bit):
// - bits rangés par lignes (row-major)
// - 1 octet = 8 pixels horizontaux
// - taille en pixels: W x H
// - bytes_per_row = (W + 7) / 8
//
// Exemple: W=16 => bytes_per_row=2
// data size = bytes_per_row * H

namespace SPR {

  // ---------- Player sprites ----------
  // Exemple: player idle 16x16
  static const uint8_t PLAYER_IDLE_16x16[] PROGMEM = {
    // 16x16 => 2 bytes/row * 16 rows = 32 bytes
    // Row 0..15 (2 octets par ligne)
    0b00000000,0b00000000,
    0b00011000,0b00000000,
    0b00111100,0b00000000,
    0b01111110,0b00000000,
    0b01100110,0b00000000,
    0b01111110,0b00000000,
    0b00111100,0b00000000,
    0b00011000,0b00000000,
    0b00011000,0b00000000,
    0b00100100,0b00000000,
    0b01000010,0b00000000,
    0b01000010,0b00000000,
    0b00100100,0b00000000,
    0b00011000,0b00000000,
    0b00011000,0b00000000,
    0b00000000,0b00000000,
  };

  // Exemple: player walk frame 1 (16x16)
  static const uint8_t PLAYER_WALK1_16x16[] PROGMEM = {
    0,0,
    0x18,0x00,
    0x3C,0x00,
    0x7E,0x00,
    0x66,0x00,
    0x7E,0x00,
    0x3C,0x00,
    0x18,0x00,
    0x18,0x00,
    0x24,0x00,
    0x42,0x00,
    0x42,0x00,
    0x14,0x00,
    0x08,0x00,
    0x18,0x00,
    0x00,0x00,
  };

  // Exemple: player walk frame 2 (16x16)
  static const uint8_t PLAYER_WALK2_16x16[] PROGMEM = {
    0,0,
    0x18,0x00,
    0x3C,0x00,
    0x7E,0x00,
    0x66,0x00,
    0x7E,0x00,
    0x3C,0x00,
    0x18,0x00,
    0x18,0x00,
    0x24,0x00,
    0x42,0x00,
    0x42,0x00,
    0x28,0x00,
    0x10,0x00,
    0x18,0x00,
    0x00,0x00,
  };

  // ---------- Registry (pratique) ----------
  struct Sprite {
    const uint8_t* data;
    uint8_t w;
    uint8_t h;
  };

  enum PlayerSpriteId : uint8_t {
    P_IDLE = 0,
    P_WALK1,
    P_WALK2,
    P_COUNT
  };

  static const Sprite PLAYER_SPRITES[P_COUNT] PROGMEM = {
    { PLAYER_IDLE_16x16,  16, 16 },
    { PLAYER_WALK1_16x16, 16, 16 },
    { PLAYER_WALK2_16x16, 16, 16 },
  };

} // namespace SPR
