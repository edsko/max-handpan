# max-handpan

Max for Live Handpan Instrument for the Ableton Live.

## Goal

The goal of this project is to provide a custom instrument for the
[Ableton Push](https://www.ableton.com/en/push/)
(and possibly other controllers), turning the Push into a digital
[handpan](https://en.wikipedia.org/wiki/Handpan)
(such as the
[hang drum](https://en.wikipedia.org/wiki/Hang_(instrument)))
that is meant to be played like a handpan is: not just access to the tone
fields/doum, but ghost notes, taks, slaps, etc. too, as well as a layout that is
remiscent of an actual handpan. It should also be possible to configure the
handpan in various ways; as an obvious example, it should be easy to change the
scale. In fact, I am building this instrument as I am making my way through the
excellent handpan course at David Kuckhermann's
[Handpan Dojo](https://courses.handpandojo.com/).

The project is split into two parts: an _interface_, which defines the layout
of the handpan and defines how the user actually "plays" the handpan, and a
_backend_, which uses an Ableton instrument to actually produce sound;
the backend also allows to select a scale.

In other words, the architecture looks
something like this:

<table>
<tr><td/><th>Interface</th><td/><th>Backend</th><td/><th>Scale</th><td/></tr>
<tr>
<td><pre><code>
/
\
</pre></code></td>
<td>
<a href="https://www.ableton.com/en/manual/using-push-2/">Push 2</a> (*) <br/>
<a href="https://www.ableton.com/en/manual/using-push/">Push 1</a> <br/>
<a href="https://novationmusic.com/en/launch">Launchpad</a> <br/>
...
</td>
<td><pre><code>
\/
/\
</pre></code></td>
<td>
<a href="https://www.soniccouture.com/en/products/35-rare-and-unique/g29-pan-drums/">Soniccouture 1st gen Hang</a><sup>1</sup> (*) <br/>
<a href="https://www.soniccouture.com/en/products/35-rare-and-unique/g29-pan-drums/">Soniccouture 2nd gen Hang</a> <br/>
<a href="https://www.cinematique-instruments.com/pages_instr/inst_pandrum.php">Cinematique Instruments Pandrum</a><sup>2</sup> <br/>
<a href="https://8dio.com/instrument/alien-drum/">8Dio New Alien Drum</a><sup>3</sup> <br/>
<a href="http://www.soundsonline.com/sd2">EastWest Stormdrum 2</a><sup>4</sup> <br/>
Perhaps a free synthesized hang?<sup>5</sup> <br/>
...
</td>
<td><pre><code>
\/
/\
</pre></code></td>
<td>
<a href="https://www.haganenote.com/store/kurd/">Kurd 9</a> (*) <br/>
<a href="https://en.wikipedia.org/wiki/Phrygian_dominant_scale">Hijaz</a> (*) <br/>
<a href="https://www.hangblog.org/hang-sound-models/">Many others..</a>
</td>
<td><pre><code>
\
/
</pre></code></td>
</tr>
</table>

where the lines marked `(*)` are currently actually implemented.

_Features of the interface_:

* Configurable number of tone fields
* Per zone velocity curve; in other words, configure drive, compand, min, and
  max velocity separately for center zone fields and for taks/ghost notes.

_Features of the backend_:   

The backend interfaces with a particular instrument (see table above for
supported instruments). It provides some user-facing settings as well, although
currently this is limited to just scale selection.

## TODOs

This is work in progress.

Currently known TODOs/bug (apart from simply adding support for whatever is
marked as not yet implemented in the table above):

* `OurTrack` recognition does not seem to work inside an instrument rack.
* We might want to allow to explicitly set a root note for the scale.
  Not sure it's necessary (can use the standard Ableton pitch effect), but
  if we want some notes to be shifted and other notes stay where they are
  (for example, if a particular instrument has some pitches dedicated to
  percussive sounds), that will become important.
* Related: we might want to be able to choose the doum separate from the
  scale; for instance, a D Hijaz over a root of G versus a D Hijaz over
  a root of A.
* Need to tune the sensitivity of the pads.
* Need to make sure that multiple instances of the instrument can coexist
  within the same session.
* It might be nice to allow to rotate the handpan.

# Notes per supported backend instrument

## Soniccouture Pan Drums

### Overview

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

### Suitability for real handpan playing

There does not seem to be a clear recording of a tak in this instrument?

Currently using the "slapped" articulation of the doum (but on octave up)
for taks; similarly, using the "slapped" articulation of the tone fields
for ghost notes.

However, the "slapped" articulation of the doum comes in two recordings, one
of which is useful but the other one less so. We therefore control sample
selection from within Max Handpan; see setup instructions, below.

Using the gu side tone as a substitute for the palm bass, as
[they serve a similar purpose](https://www.markdambrosiomusic.com/post/the-top-ten-traits-of-a-great-handpan): both activate the helmholtz resonance of the
instrument.

### Setup

Load the instrument:

* Load the `Pan Drum Mk1 Chromatic Map` into a MIDI track.
* We don't need most of this; select the instrument and "Ungroup" it,
  then delete the Arpeggiator (not at all useful for emulating "real" handpan
  playing) and the velocity effect (the Max Handpan has per-zone velocity
  control built in).

We do want random sample selection mostly, but we don't want it for all zones
(see Suitability, above). We therefore want to be able to control sample
selection from within Max Handpan. To make this possible requires two steps.
Select the _Slap_ chain, then

* Under Modulation, turn off LFO 2 which is controlling sample selection.
* Under MIDI, set the `Mod Wheel` to `Sample Selector`, and set the amount to
  100%.
* Optional: if you wanted to test whether this works, create a short clip with
  some D4s in it, and set "Strike" to 50 (selecting the "slap" articulation).
  You should only hear a single sample. Now create a MIDI control automation
  envelope for CC channel 1 (modulation). You should be able to select a sample
  by changing the modulation.
* Now set up the Max Handpan as for any other instrument.

(If it did not work, make sure you updated the Modulation and MIDI settings for
right articulation.)

# Footnotes

2. Tak and doum only available when a scale is selected (not available
   in chromatic mode), so this will need to be taken into account in the
   backend. Could perhaps use Zephyr Perc to get access to other percussive
   sounds.
3. [Seems to](https://www.youtube.com/watch?v=_D2lhwtXbUQ) have quite a few
   percussive sounds.
4. "Hangdrum" under "Ethnic Metals".
5. Wikipedia says that the
   [hang drum](https://en.wikipedia.org/wiki/Hang_(instrument))
   is primarily based on
   [Helmholtz resonance](https://en.wikipedia.org/wiki/Helmholtz_resonance)
   which suggests we _might_ have some hope of being able to construct
   some kind of useable/passable sound.
   Moreover, there an an MSc thesis on the topic?
   [Analysis and Synthesis of the Handpan Sound](http://etheses.whiterose.ac.uk/12260/1/EyalMSc.pdf)
   by Eyal Alon.
