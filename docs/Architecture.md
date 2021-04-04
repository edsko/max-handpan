# Architecture

The Max Handpan architecture consists of three parts:

![](/docs/img/pipeline.png)

## Frontend

The frontend interfaces with the control surface. Currently the only control
supported control face is the
[Ableton Push 2](https://www.ableton.com/en/manual/using-push-2/),
but support for other controllers such as the
[Ableton Push 1](https://www.ableton.com/en/manual/using-push-1/),
[Novation Launchpad](https://novationmusic.com/en/launch)
or the
[Akai Pro Force](https://www.akaipro.com/force-forcexus)
could in principle be added as well.

The frontend has the following responsiblities:

1. Configure the layout and colors of the controller. For example, on the
   Push 2 it currently looks something like this:

   ![](/docs/img/push2-small.jpg)

   The layout here is an attempt to mimick a real-life handpan as close as
   possible: tone fields around the outside, doum and tak in the middle, slaps
   (red) on the very outside<sup>1</sup>, and a bass note at the bottom. (The
   bass note is normally played on the shoulder of the instrument, but the
   placement on the bottom of the button matrix of the Push means we can play
   the base with a fist, as it is normally done.)

2. Since a handpan's sensitivity to force is non-uniform, the frontend provides
   multiple velocity curves so that different areas of the handpan can be
   configured differently.

3. Generate MIDI data to interface with the next stage in the pipeline, the
   backend. The MIDI data here reflects the physical layout of the handpan
   rather than refer to specific _notes_; it is reminiscent of the MIDI data for
   a drum controller. See
   [MidiLayout.md](/docs/frontend/MidiLayout.md)
   for a specification of the MIDI layout used.

See [Frontend: Ableton Push 2](/docs/frontend/Push2.md) for a technical
description of the frontend.

## Backend

The backend has the following responsibilities:

1. As mentioned, the MIDI data coming from the frontend reflects the physical
   layout of the handpan; for example, it might record something like "tone
   field 3, tone field 1, doum". A real-life handpan is of course tuned to a
   specific scale, so on _particular_ drum this sequence might sound like "C4,
   A4, D3". One advantage of the virtual handpan over a real one is that we can
   change the scale on the fly: the first responsibility of the backend is to
   take the MIDI data from the front end and interpret it with respect to a
   choice of scale.

   The backend allows a choice of scale (Kurd 9, Hijaz, etc.; see the
   [full list of scales](/docs/backend/Scales.md)).
   The root of the scale and the note of the doum can be chosen independently;
   for example, there are some handpans with a D Hijaz scale over a doum of G,
   and some handpans with a D Hijaz scale over a doum of A.

 2. While the scale selection part is instrument independent, the backend's
    second main responsibility is not: it must translate the MIDI data from the
    frontend, as interpreted through the scale options, into MIDI data that can
    then be turned into actual audio by a choice of Ableton instrument.
    Currently the only instrument supported is the
    [Soniccouture Pan Drum](https://www.soniccouture.com/en/products/35-rare-and-unique/g29-pan-drums/),
    in particular, the
    [Pan Drum Ableton Live pack](https://www.ableton.com/en/packs/pan-drum/#?).

    This Live pack actually contains two sampled hand pans, a first generation
    hang tuned in A Pelog with a doum of F, and a second generation hang tuned
    in A Integral with a doum of D. The Max Handpan backend supports both
    (select `mk1` or `mk2`).

    The backend translates the incoming MIDI data into MIDI data for the
    instrument, translating tone fields not just into notes, but also into
    key-switch commands that set parameters such as articulation; for example,
    when playing a tone field, the "Mid" articulation is chosen, but when
    playing ghost notes, typically played in between tone fields on the shoulder
    of the instrument, the "Between" articulation is chosen.

    Many other instruments could in principle be supported; see
    [list of instruments](/docs/backend/Instruments.md).

3. Since the sampled handpan is tuned to a _specific_ scale, any deviation
   from this scale will require transposition. The slides shown by the backend
   show how much transposition is required to emulate the selected scale; the
   first slider represents the doum, the other slides the tone fields. It also
   shows the minimum and maximum transition used, in semi-tones. (It is useful
   to remember that samples
   [sound better when transposed down](https://www.youtube.com/watch?v=YbbBGYKucHY)
   then when transposed up.)

See [Backend: Soniccouture Pan Drum](/docs/backend/Soniccouture.md) for a
technical description of the backend.

## Instrument

The instrument takes the MIDI data created by the backend and turns it into
actual sound. Ideally, the instrument is not part of the Max Handpan
infrastructure at all, but is an
[off-the-shelf](/docs/backend/Instruments.md)
Ableton Instrument or VST.

Unfortunately, unlike many VSTs, Ableton Instruments do not typically support
key-switching. Specifically, the Soniccouture Pan Drum
[is essentially just a collection of configured Sampler instances](/docs/backend/soniccouture/Overview.md).
To choose a particular articulation, we must set an instrument rack Chain slider
instead of sending key-switch commands.

While that works fine for MIDI programming melodies, it does not work for the
"just in time" articulation that we need to do when playing the handpan: we
don't want to first chose an articulation, and then play it; we just want to
strike the corresponding part of the instrument and have the articulation
chosen correspondingly. While it _is_ possible to control parameters such as
the Chain Selector from within a Max for Live device, this unfortunately has a
[fundamental limitation](https://cycling74.com/forums/updating-device-parameters-*synchronously*-guaranteeing-ordering):
while we _can_ change a parameter, we do not know when the parameter change
takes effect, and so when we send a MIDI note, we have no guarantee if new
parameter setting will be applied to that note or not. Max for Live devices
that emulate key-switching such as the excellent
[KeySwitch & Expression Map for Ableton Live
](https://www.swub.de/en/software/keyswitch-expression-map-ableton-live/)
suffer from the same limitation.

Consequently, for the specific case of the Soniccouture Pan Drum, Max Handpan
additionally includes a custom instrument that uses the sample data from the
Soniccouture Ableton Live pack, but does not use Sampler.
See [Instrument: Soniccouture Pan Drum](/docs/instrument/Soniccouture.md)
for a technical description of this instrument.

**Footnotes**

1. Currently we only have two areas for slaps, one on the left and one on the
   right. I posted the following question in the Handpan Dojo group:

   <blockquote>
   For percussive slaps, hitting the handpan near different zone fields (in
   between different tonefields) results in a different sound. Do you use this
   to your advantage when playing, or do you just play what is convenient? If
   the former, how do you think about it? Do you associate a particular slap
   with a particular tone field (like, "this is a D slap" or whatever), or do
   you just gradually learn how the different slaps sound, and you don't really
   think of them as notes as such?
   </blockquote>

   and David Kuckhermann answered:

   <blockquote>
   I check out the various spots on an instrument for slaps and usually choose
   the ones that give me the strongest accent with as little "tone" from the
   neighbouring tone fields as possible.
   </blockquote>

   So having two slaps available will probably suffice. As an potential
   improvement, we could use the additional sample data for further round robin
   sample selection, or make it configurable which slaps to use.
