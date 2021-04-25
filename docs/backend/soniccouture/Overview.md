# Instrument: Soniccouture Pan Drum (Ableton Live pack)

## Overview

The
[Ableton Live pack](https://www.ableton.com/en/packs/pan-drum/)
of the Soniccouture Pan Drums contains samples of the first and second
generation hang drums (the
[Kontakt Player version](https://www.soniccouture.com/en/products/35-rare-and-unique/g29-pan-drums/)
is more expensive and has three additional instruments). The description below
is for the Live pack; I don't have a copy of the Kontakt Player version.

The instrument is basically
an
[instrument rack](https://www.ableton.com/en/manual/instrument-drum-and-effect-racks/)
containing 5
[samplers](https://www.ableton.com/en/manual/live-instrument-reference/#24-7-sampler),
one for each supported articulation: hitting the handpan

* in the middle of a tone field,
* near the edge of a tone field,
* slapping it,
* in between tone fields,
* and hitting it with a knuckle.

These samplers are assigned to different
[chain select zones](https://www.ableton.com/en/manual/instrument-drum-and-effect-racks/#18-5-4-chain-select-zones),
which is mapped to the `Strike` macro in the instrument rack.

Rather than storing each sample in its own file, the instrument comes with a few
very long samples which each contain multiple records of the handpan being hit
in various ways, in various zones, and with various velocities. The `sampler` is
setup to choose the right part of the sample based on key zones and velocity
zones; an LFO, set to
[sample and hold](https://www.ableton.com/en/manual/live-instrument-reference/#24-7-9-the-modulation-tab),
is used drive the sample selector to choose randomly between multiple recordings
of the same key/velocity/zone.

See
[Samples.md](/docs/backend/soniccouture/Samples.md)
for a detailed  description of the available samples, and
[Layout.md](/docs/backend/soniccouture/Layout.md)
for a description of the MIDI layout of the (precision) instrument
(which is the one we use as the basis for our
[custom Soniccouture instrument](/docs/instrument/Soniccouture.md)).

### Suitability for real handpan playing

This instrument is deeply sampled, and the Precision mapping gives us access
to the unmodified samples, which is what we need. It also provides plenty of
percussive samples. On the whole, this is an excellent instrument to emulate
real handpan playing.

It does seem however that there are two percussive elements missing:

1. There isn't a clear "tak" sound. I'm currently using the highest two tone
   fields for this in the "between" articulation. This kind of works as a
   percussive element, but it's not a perfect match.

   (There _are_ two articulations of the doum, but it seems they are just two
   different ways of playing the doum: with or without palm; see
   https://github.com/edsko/max-handpan/issues/3.)

2. There is no recording of a palm base; I'm using the gu side tone as a
   substitute as
   [they serve a similar purpose](https://www.markdambrosiomusic.com/post/the-top-ten-traits-of-a-great-handpan):
   both activate the helmholtz resonance of the instrument.
