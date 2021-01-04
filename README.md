# max-handpan

Max for Live Handpan Instrument for the Ableton Live.

## Goal

The goal of this project is to provide a custom instrument for the
[Ableton Push](https://www.ableton.com/en/push/),
turning the Push into a digital
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

The project is split into two parts:

1. The "interface", which defines how the handpan is played: the layout on the
   push, configurable sensitivity per zone on the handpan (i.e. different
   sensitivity for tone fields as for ghost notes), etc.  Although this is
   currently implemented for the Ableton Push, it could potentially be
   implemented for other controllers as well (such as the Launchpad).
2. The "backend", which uses a particular instrument to actually produce sound.
   Currently this is using the
   [first generation hang from Soniccouture](https://www.soniccouture.com/en/products/35-rare-and-unique/g29-pan-drums/),
   although other backends should be possible as well. The backend itself is
   then parameterised over a choice of scale.

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
<a href="https://www.hangblog.org/hang-sound-models/">Many others..</a>
</td>
<td><pre><code>
\
/
</pre></code></td>
</tr>
</table>

where the lines marked `(*)` are currently actually implemented.

## TODOs

This is work in progress.

Currently known TODOs/bug (apart from simply adding support for whatever is
marked as not yet implemented in the table above):

* `OurTrack` recognition does not seem to work inside an instrument rack.
* Backend scale selection is not yet implemented.
* We might want to allow to explicitly set a root note for the scale.
  Not sure it's necessary (can use the standard Ableton pitch effect), but
  if we want some notes to be shifted and other notes stay where they are
  (for example, if a particular instrument has some pitches dedicated to
  percussive sounds), that will become important.

# Footnotes

1. There does not seem to be a clear recording of a tak in this instrument?
   Currently using the "slapped" articulation of the doum (but on octave up)
   for taks; similarly, using the "slapped" articulation of the tone fields
   for ghost notes.
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
