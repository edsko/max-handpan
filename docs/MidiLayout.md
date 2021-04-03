# MIDI layout

The frontend generates MIDI notes using the following layout:

```
-------------------------------
PERCUSSION  36 | Bass
            37 | Tak
            38 | Slap 1 (Left)
            39 | Slap 2 (Right)
-------------------------------
MELODIC     48 | Doum
            49 | Tonefield 1
            50 | ..
            51 | ..
            52 | ..
            53 | ..
            54 | ..
            55 | ..
            56 | ..
            57 | Tonefield 9
-------------------------------
GHOST       60 | Tak
            61 | Tonefield 1
            62 | ..
            63 | ..
            64 | ..
            65 | ..
            66 | ..
            67 | ..
            68 | ..
            69 | Tonefield 9
-------------------------------
```
