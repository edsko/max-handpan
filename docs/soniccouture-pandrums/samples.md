# Soniccouture Pan Drums

https://www.soniccouture.com/en/products/35-rare-and-unique/g29-pan-drums/

## First generation hangdrum (`mk1`)

### Gu (`F2`)

`F2` is mapped to the gu-side of the drum. It comes in two articulations:

* `Edge`, recorded at 8 velocities, with a total of 15 samples.
* non-`Edge` (for want of a better word), recorded at 10 velocities, for a total
  of 19 samples.

All of the `Between`, `Knuckle`, `Mid` and `Slap` articulations are all mapped
to that same "non-`Edge`" articulation.

### Doum (`F3`)

`F3` is mapped to the central note of the drum ("doum" or "ding"). Like the gu,
it comes in two articulations:

* `Edge`, recorded at 16 velocities, for a total of 45 samples.
* non-`Edge`, recorded at 15 velocities, for a total of 27 samples.

Like `F2`, other articulations are mapped to `Edge`.

### Tone fields (`A3`, `B♭3`, `C4`, `E4`, `F4`, `A4`, `B♭4`, `C5`)

The tone fields all come in one of 5 articulations (`Between`, `Edge`,
`Knuckle`, `Mid` and `Slap`). These articulations are a combination of _where_
and _how_ the instrument is hit. The manual specifies:

> There are many ways to hit the Hang to achieve different sounds... with the
> fingertips, the palm of the hand, or with your knuckles

and moreover that

> Also, where the Hang is struck results in a different type of sound quality.
> We have sampled our Hang Drums at three different zones around each “tone
> field”... the centre, the edge, and in-between.

but it does not explicitly specify how the _combinations_ of "where" and "how"
result in the various articulations provided. FWIW, my guess is as follows:

```
Articulation  Where       How
-------------------------------------
Mid           Center      Fingertips?
Edge          Edge        Fingertips?
Between       In-between  Fingertips?
Knuckle       Edge?       Knuckle
Slap          Edge?       Palm
```

For the exact number of velocities and samples see the reference below,
they vary per articulation.

## Second generation (`mk2`)

### Gu (`D2`)

Unlike the gu on the `mk1`, the gu on the `mk3` is available in 3 articulations.

* `Edge` (`Between` is mapped here as well). 5 velocities, 9 samples.
* `Mid` (`Knuckle` is mapped here as well). 8 velocities, 23 samples.
* `Slap`. 5 velocities, 9 samples.

### Doum (`D3`)

This comes in two articulations

* `Edge` (`Between` and `Slap` are mapped here as well). 12 velocities, 33 samples.
* `Mid` (`Knuckle` is mapped here as well). 18 velocities, 49 samples.

Although both the `mk1` and `mk2` have two articulations for the central note,
the mapping is a bit different.

### Tonefields (`A3`, `B♭3`, `C4`, `D4`, `E4`, `F4`, `A4`)

The discussion of the tonefields for the `mk1` applies here as well.
As for the `mk1` the number of velocities and samples per velocity varies
per note/articulation, see reference below. On oddity however is that `A4`
does not have a "between" articulation; `Pan Drum Mk2-Between.aif` contains
samples for `F4` down to `A3`; the sampler instrument maps `A4` to `A3` for
this articulation (even in the precision map).

## Reference

### Number of velocities and samples for the `mk1`

```
Note Articulation #Velocities #Samples
--------------------------------------

A3   Between      11          20
B♭3  Between       8          15
C4   Between      10          19
E4   Between       9          17
F4   Between      10          19
A4   Between      11          21
B♭4  Between      10          20
C5   Between       9          17

A3   Edge         11          31
B♭3  Edge         13          36
C4   Edge         14          38
E4   Edge         13          35
F4   Edge         16          45
A4   Edge         13          38
B♭4  Edge         12          34
C5   Edge         12          34

A3   Knuckle       7           7
B♭3  Knuckle       6           6
C4   Knuckle       4           4
E4   Knuckle       5           5
F4   Knuckle       4           4
A4   Knuckle       6           6
B♭4  Knuckle       8           8
C5   Knuckle       8           8

A3   Mid          12          35
B♭3  Mid          19          50
C4   Mid          13          39
E4   Mid          13          33
F4   Mid          12          33
A4   Mid          14          39
B♭4  Mid          13          37
C5   Mid          14          41

A3   Slap         19          35
B♭3  Slap         13          25
C4   Slap         14          27
E4   Slap         18          35
F4   Slap         13          26
A4   Slap         12          23
B♭4  Slap         12          22
C5   Slap         16          29
```

### Number of velocities and samples for the `mk2`

```
Note Articulation #Velocities #Samples
--------------------------------------

A3   Between       6          12
B♭3  Between       6          11
C4   Between       7          13
D4   Between       6          12
E4   Between       7          14
F4   Between       6          12
A4   Between       0           0   (see discussion above)

A3   Edge         20          57
B♭3  Edge         15          43
C4   Edge         14          37
D4   Edge         14          39
E4   Edge         13          35
F4   Edge         14          39
A4   Edge         11          29

A3   Knuckle       3           3
B♭3  Knuckle       3           3
C4   Knuckle       3           3
D4   Knuckle       3           3
E4   Knuckle       3           3
F4   Knuckle       3           3
A4   Knuckle       3           3

A3   Mid          19          53
B♭3  Mid          16          44
C4   Mid          17          47
D4   Mid          18          50
E4   Mid          20          55
F4   Mid          19          51
A4   Mid          17          47

A3   Slap         18          34
B♭3  Slap         18          34
C4   Slap         18          35
D4   Slap         19          34
E4   Slap         18          34
F4   Slap         16          31
A4   Slap         19          35
```
