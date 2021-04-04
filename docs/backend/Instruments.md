# Instruments

## Supported

Currently the only supported instrument is the
[Soniccouture Pan Drum](https://www.soniccouture.com/en/products/35-rare-and-unique/g29-pan-drums/),
in particular, the
[Pan Drum Ableton Live pack](https://www.ableton.com/en/packs/pan-drum/#?).
This instrument contains samples for a first generation hang and a second
generation hang; we support both. For a detailed description of this instrument,
see
[Instrument: Soniccouture Pan Drum (Ableton Live pack)](/docs/backend/soniccouture/Overview.md).

Unfortunately,
[as described in the architecture overview](/docs/Architecture.md#instrument),
we cannot use the instrument itself (which is a series of configured Ableton
`Sampler` instances), but use a
[custom instrument](/docs/instrument/Soniccouture.md) instead; we do of course
use the sample data from Soniccouture.

## Currently unsupported

There are many more handpan instruments/VSTs available, and over time more
are bound to come out. The list below are all the instruments I am
currently aware of.

* [Cinematique Instruments Pandrum](https://www.cinematique-instruments.com/pages_instr/inst_pandrum.php)

  _Possible implementation notes_:
  Tak and doum only available when a scale is selected (not available in
  chromatic mode), so this will need to be taken into account in the backend.
  Could perhaps use Zephyr Perc to get access to other percussive sounds.

* [8Dio New Alien Drum](https://8dio.com/instrument/alien-drum/)

  _Possible implementation notes_:
  [Seems to](https://www.youtube.com/watch?v=_D2lhwtXbUQ)
  have quite a few percussive sounds.

* [EastWest Stormdrum 2](http://www.soundsonline.com/sd2)

  _Possible implementation notes_:
  It's listed under "Ethnic Metals".

* [Sonixinema - Hang Drum](https://www.sonixinema.com/products/hang-drum)

It might perhaps also be fun to try and build a synthesized hand pan at some
point. Wikipedia says that the
[hang drum](https://en.wikipedia.org/wiki/Hang_(instrument))
is primarily based on
[Helmholtz resonance](https://en.wikipedia.org/wiki/Helmholtz_resonance)
which suggests we _might_ have some hope of being able to construct some kind of
useable/passable sound. Moreover, there an an MSc thesis on the topic:
[Analysis and Synthesis of the Handpan Sound](http://etheses.whiterose.ac.uk/12260/1/EyalMSc.pdf)
by Eyal Alon.
