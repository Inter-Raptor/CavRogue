# CavRogue

**CavRogue** is a minimalist roguelike made in **Python (Pygame)**, built as a **PC prototype** before an
**ESP32-S3 (N16R8) port** using a **Nokia 5110 / PCD8544 (84×48, 1-bit)** display and **at least 6 buttons**.

The PC version helps iterate quickly on gameplay and balancing, while keeping assets and rendering constraints close to the future microcontroller target.

---

## Features

- Minimalist roguelike gameplay (short runs, replayable)
- Designed around **1-bit sprites** and small-screen readability
- Assets stored as `.h` headers (player/enemies/animations) for easier embedded reuse
- Intended controls: **D-Pad + 2 actions** (6 buttons total)

---

## Controls (PC prototype)

Recommended mapping (you can adapt it in code):
- **Up / Down / Left / Right**: move
- **Action / Shoot**: interact / fire
- Optional: **Start/Menu** if you have an extra button on hardware

---

## Project structure (recommended)

```
pc/     -> Pygame prototype (Windows / Raspberry Pi)
esp32/  -> ESP32-S3 port experiments (Arduino/PlatformIO)
```

---

## Run on PC / Raspberry Pi

### Requirements
- Python 3.10+ recommended
- Pygame

### Install & run
```bash
pip install -r requirements.txt
python pc/CavRogue.py
```

If your main script is at repo root instead of `pc/`, run:
```bash
python CavRogue.py
```

---

## Build a Windows EXE (PyInstaller)

### One-folder build (easier to troubleshoot)
```bash
py -m pip install pyinstaller pygame
py -m PyInstaller --onedir --windowed --name CavRogue pc/CavRogue.py
```

### Single-file EXE (portable)
```bash
py -m pip install pyinstaller pygame
py -m PyInstaller --onefile --windowed --name CavRogue pc/CavRogue.py
```

The output is in `dist/`.

> Note: the `.exe` is not committed into the repository.  
> Download builds from **Releases** instead.

---

## Assets (`.h` headers)

The game can use external `.h` files for 1-bit sprites/animations, for example:
- `sprites_player.h`
- `sprites_enemies.h`
- `anim_intro.h`, `anim_gameover.h`, `anim_ending_*.h`, etc.

These headers are kept compatible with an embedded workflow (ESP32).

---

## ESP32-S3 Port (Work in progress)

Target:
- **ESP32-S3 N16R8**
- **Nokia 5110 / PCD8544 (84×48, 1-bit)**
- **6 buttons minimum**

Planned steps:
- Replace Pygame loop with an embedded main loop
- Replace rendering with a 1-bit framebuffer for PCD8544
- Replace keyboard input with GPIO buttons
- Keep save system lightweight (EEPROM/NVS or small file on flash)

---

## License

MIT (recommended).
