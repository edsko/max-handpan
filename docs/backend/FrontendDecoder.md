# Frontend decoder

The frontend decoder is entirely implemented in Max; it's a straight-forward
patcher:

![](/docs/img/frontend-decode.png)

This just recognizes the [MIDI layout](/docs/frontend/MidiLayout.md]) used by
the Max Handpan frontend; we are careful to first output a retune index and then
a `(category, index)` pair; for example, we might output `(2, 1)` to mean
"ghost note at tone field 1". See the description of the
[Soniccouture backend](/docs/docs/backend/Soniccouture.md)
for an example use case.
