# MIDI layout

## mk1

### Doum and Tone Fields

```
Doum Tonefields
     1  2   3  4  5  6  7   8
F3   A3 B♭3 C4 E4 F4 A4 B♭4 C5
65   69 70  72 76 77 81 82  84
```

### Percussion

```
Bass Tak    Slaps
(Gu) (Doum) 1     2
F2   F3     (B♭3) (C4)
53   65     70    72
```

### MIDI Maps

*Notes*:

```
0, 53 65 70 72;
1, 65 69 70 72 76 77 81 82 84 84;
2, 65 69 70 72 76 77 81 82 84 84;
```

(Note that we repeat the final note in order to get a maximum of 9 tone fields.)

*Articulations* (0 Mid, 1 Edge, 2 Slap, 3 Between, 4 Knuckle):

```
0, 0 1 2 2;
1, 0 0 0 0 0 0 0 0 0 0;
2, 3 3 3 3 3 3 3 3 3 3;
```

## mk2

### Doum and Tone Fields

```
Doum Tonefields
     1  2   3  4  5  6  7
D3   A3 B♭3 C4 D4 E4 F4 A4
62   69 70  72 74 76 77 81
```

### Percussion

```
Bass Tak    Slaps
(Gu) (Doum) 1     2
D2   D3     (B♭3) (C4)
50   62     70    72
```

### MIDI Maps

*Notes*:

```
0, 50 62 70 72;
1, 62 69 70 72 74 76 77 81 81 81;
2, 62 69 70 72 74 76 77 81 81 81;
```

(Since this drum has only 7 tone fields, we repeat the last note twice.)

Articulation mapping identical to the `mk1`.
