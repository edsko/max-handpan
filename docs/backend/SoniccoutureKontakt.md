# Backend: Soniccouture Pan Drum (Kontakt version)

## Why the Kontakt version

Ableton Live pack:

- Octave map only provides a single octave per articulation; insufficient range
- Precision map works, but Ableton has no support for key switching; we can drive the chain selector from within Max, but not with reliable ordering. (Multiple instances of the instrument would have the same problem: that too would require a chain selector to choose between them; exactly the same setup.)
- Our custom key-switched sampler works, but we cannot load the samples from the Live pack directly into Max (copyright protection?), and while we can drag them into wave file recepticles in Max, that requires loading 9 samples. Worse, such a setup does not reliable save and reload, so that every time we start we have to reload them again.

The Kontakt version _does_ support proper key switching. So we can use the chromatic map (avoids having to do our own retuning), and we can use key-switching to select between articulations.

Below is specific to the mk1, for now.

## Used articulations

```
-------------------------------
PERCUSSION  | Bass           |
            | Tak (Left)     |
            | Tak (Right)    |
            | Slap 1 (Left)  |
            | Slap 2 (Right) |
-------------------------------
MELODIC     | Doum           |
            | Tonefield 1    |
            | ..             |
            | ..             |
            | ..             |
            | ..             |
            | ..             |
            | ..             |
            | ..             |
            | Tonefield 9    |
-------------------------------
GHOST       | Tak            |
            | Tonefield 1    |
            | ..             |
            | ..             |
            | ..             |
            | ..             |
            | ..             |
            | ..             |
            | ..             |
            | Tonefield 9    |
-------------------------------
```

## Doum

Natural doum: F3
Transposed range: A2 - F#3
Manually extended to G2 - F#3 for a full octave (just drag the Kontakt ranges)





