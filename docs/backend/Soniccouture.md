# Backend: Soniccouture Pan Drum

The Soniccouture Backend consists of some parts that are generic to any
backend, along with some instrument-specific logic:

![](/docs/img/BackendSoniccouture.png)

The red patch cabels are patcher configuration; the blue patch cables are MIDI
processing. We will consider both in turn.

## Configuration

The [scale selector](/docs/backend/ScaleSelector.md) needs to know the
"natural" (non-transposed) value of the doum and of the root of the scale;
for the `mk1` this is 65 (F3) and 69 (A3) respectively, and for the `mk2` this
is 62 (D3) and 69 (A3). The output of the scale selector is a list of MIDI
notes, starting with the doum and followed by the notes of the selected scale,
with any transpositions applied.

The [retuner](/docs/backend/Retuner.md) needs to know the selected scale,
as well as the natural tuning of the selected handpan. It uses this to map
incoming MIDI notes to a transposition value: "if the natural tuning of the
second tone field is a D, but we want it to be an E, then better transpose
that tone field up two semitones".

## Processing incoming MIDI notes

MIDI notes are processed in three steps:

1. Parsing
2. Issue key-switching commands
3. Issue the note itself.

### Parsing

Incoming MIDI notes are first parsed:

![](/docs/img/BackendSoniccouture-parse.png)

We separate out the MIDI note and it's velocity, and then run the note through
the [frontend decoder](/docs/backend/FrontendDecoder.md), which recognizes
the [MIDI layout](/docs/frontend/MidiLayout.md) generated in the frontend.
The decoder outputs two things:

1. On the second output, it outputs a "retuning index", which is passed to the
   retuner to look up how much we should transpose the note. The index will be
   0 for the doum, 1..9 for the tone fields, and 0 for the percussive elements
   (which are therefore transposed along with the doum).

2. On the first output, it outputs a `(category, index)` pair, which corresponds
   directory to the MIDI layout: the category is 0 for percussion, 1 for melodic
   elements and 2 for ghost notes; the index is the index within that category
   as per the layout specification.

   For a pair `(x, y)`, the `p nth` subpatcher then turns this into an
   `nth x (y + 1)` message, which will be used to index a `coll` (see below).
   The `(y + 1)` is necessary because `coll` uses 1-based indexing for lists.

### Key-switching commands

The velocity coming from the parser is routed straight to a `pack` object, to
be combined in the final stage with the note will we will issue.

Then the retuning index is sent to the `retuner`, which will output a
transposition value. This value is then turned into a MIDI "transpose"
key-switch command for our
[custom Soniccouture instrument](/docs/instrument/Soniccouture.md):
the key-switch command to transpose by `n` is the pair `(3, 64 + n)`, i.e.,
MIDI note 3 ("D#-2") with velocity `64 + n`.

Finally, the `(category, index)` pair is sent two two collections: one that
tells us which `strike` articulation we should be using, and which MIDI note
by the instrument for this doum/tonefield/percussive element. We have one
such pair of collections for both sampled instruments (the first generation
hang and the second generation hang).

We index the `strike` collection first, turning this into another key-switch
command `(1, c)`, where the "volume" `c` denotes the value we want to
chain selector to have.

### Issue the note

When both key-switch commands have been issued, we know that the instrument will
correctly interpret the note we will send it, and so we can now issue the actual
note.
