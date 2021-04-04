# Key-switched Sampler

The key-switched sampler is a relatively bare-bones version of an Ableton
instrument rack containing multiple `Sampler` instances, but with support for
key-switching; the following  key-switch commands are supported:

1. **Chain selector**

   A MIDI note with value 1 ("C#-2") and velocity `c` sets the value of the
   chain selector to `c` (corresponding to which `Sampler` the instrument rack
   would route MIDI to).

2. **Sample selector**

   A MIDI note with value 2 ("D-2") and velocity `s` set the value of the sample
   selectors to `s`. Ableton's `Sampler` uses this for random sample selection.
   We give value "0" a special meaning: when the value is set to 0, the
   instrument will _internally_ choose a random selector value for each
   ("normal") incoming note.

3. **Transposition**

   A MIDI note with value 3 ("D#-2") and velocity `64 + t` transposes the next
   incoming notes (until changed again) by `t` semi-tones. Already playing notes
   are unaffected.

Using volume in this way is possibly non-standard as far as key-switched
instruments go, but it results in quite a simple design (see below) and could
conceivably also be combined in useful ways with Ableton 11's support for
probibalistic volume selection.

Existing `Sampler` instances can be translated to the format required by
the key-switched sampler either by hand or in code; this repo includes a
[tool to do the conversion](/parse-ableton), though so far it's only been
tested with the Soniccouture Pan Drum. YMMV.

## Top-level instrument

A top-level instrument would consist of an instance of the `ks-driver`,
described below, along with buffers for each of the required samples. See
[Instrument: Soniccouture Pan Drum](/docs/instrument/Soniccouture.md)
for an example.

## Driver

The driver looks like this:

![](/docs/img/ks-driver.png)

We will describe this patch "in topological order" below.

### Parse incoming notes

After parsing the incoming MIDI notes, we first filter out any note-off
commands; they are not relevant to the sampler (we let every sample "ring out"
naturally). After this, we separate out key-switching commands from normal
MIDI commands; the format we chose for the key-switching commands makes this
straight-forward:

![](/docs/img/ks-driver-interpret-KS.png)

If the incoming note was in fact a key-switch command, the corresponding
`live.dial` is changed; this provides the user with visual feedback, and the
user can also modify these dials manually if so desired, using the sampler
in a more conventional way within Ableton.

Only non-key-switch commands make it further down the pipeline.

### Apply random sample selection

_If_ the current value of the selector dial is zero, we then pick a random
selection in the `RoundRobin` subpatch (perhaps a poor name), before sending
it into the LUTs.

### LUTs

The lookup-tables (LUTs) form the heart of the sampler. The idea is as follows:
the Ableton `Sampler` allows to assign each sample to a particular subrange of
the Key, Velocity and Selector ranges; along with the Chain range for the
outer instrument rack, this makes four ranges. Proceed as follows:

1. Divide each range into non-overlapping chunks, such that each used range
   in the original sample data corresponds to one or multiple of these chunks.
   For example, suppose the original ranges for the `Selector` look like this:

   ![](/docs/img/ableton-selector-ranges-before.png)

   then split it into four ranges like as follows:

   ![](/docs/img/ableton-selector-ranges-after.png)

2. Create `table` input files that maps each value (0..127) to the corresponding
   "range ID"; in this example, it would map values 0..41 to 1, 42..64 to 2,
   65..84 to 3 and 85..127 to 4. See
   [sc_mk1_range_id_selector.table](/ks-sampler/sc_mk1_range_id_selector.table)
   for an example of what such a file looks like.

   Do the same for all four ranges (chain, key, velocity, selector).

3. Decide for each range ID which samples should be mapped to that range ID.
   For our running example, range ID 1 would include samples 1 and 3,
   range ID 2 would include 1 and 4, range ID 3 would include 2 and 4 and
   range ID 4 would include 2 and 5.

   Then create `coll` input files that map each range ID to a list of sample
   IDs. For this example, `1` would map to the list `1 3`, `2` to the list
   `1 4`, and so forth. See
   [sc_mk1_samples_selector.coll](/ks-sampler/sc_mk1_samples_selector.coll)
   for an example of what such a file looks like.

   Again, do teh same for all four ranges.

4. Finally, create another `coll` input file with the sample data. Each sample
   ID should be mapped to four values: start, end, sample, and volume. For
   example,

   ```
   2, 1536.6666666666665 3435.374149659864 pan_drum_mk1_gu_a 0.7018175721;
   ```

   means that sample with ID 2 should play from the sample in buffer
   `pan_drum_mk1_gu_a` from 1536ms to 3435ms, with volume 0.7. See
   [sc_mk1_samples.coll](/ks-sampler/sc_mk1_samples.coll)
   for an example.

   Note that it is the responsibility of the top-level instrument to have a
   buffer with this name available.

The instrument uses all this data as follows:

![](/docs/img/ks-driver-LUTs.png)

Each incoming note is processed in 4 steps:

1. First, it is mapped to the various range IDs for each range
   (chain, key, velocity and selector). This results in 4 range IDs.
2. We then lookup which samples are assigned to each of these range IDs;
   this 4 list of sample IDs.
3. We take the intersection of these 4 lists; this gives us a list of samples
   that are suitable for this particular note.
4. Finally, we take the head of this list. We normally expect the final list
   to have exactly one element; if the list is empty, there are no samples
   assigned to this particular combination of chain/key/velocity/selector and we
   don't play anthing; if the list has more than one element, there are multiple
   such samples (the setup is ambiguous) and we just play the first.

Setting up the instrument this way means that very little processing has to
be done while the instrument is being played, thus minimizing latency. Most
processing (the computation of the LUTs) happens off-line.

As mentioned, [parse-ableton](/parse-ableton) can be used to compute all of
these LUTs from an existing Ableton instrument group containing multiple
`Sampler` instances, though this has not been tested on many different kinds
of examples. Alternatively, it should not be difficult to write some code
to generate these LUTs for other specific use cases.

### Issue the note

We pack the information coming from the LUTs together with any required
transposition, and send this to `poly~` so that it can be handled by a voice
(see below). Note that this information is now self-contained; when any of the
parameters are changed (either through further key-switch commands or because
the user manually turns the `live.dial`s), already issued notes are unaffected.

### Count the number of active voices

Finally, the driver monitors the output of the voices to count how many
voices are active; this is just used to provide the player with some visual
feedback on the state of the sampler.

## Voice

The key-switched sampler is a polyphonic sampler, with each note handled by
individual voices. (Key-switch commands are handled in the driver, not in a
voice, in order to guarantee that key-switch commands are fully handled before
the next note is interpreted.) The voice itself is simple:

![](/docs/img/ks-voice.png)

Incoming data is processed as follows:

1. Before we do anything else, we send a `1` to `thispoly~` to indicate that
   this voice is busy; we send this also on the (only) output, so that the
   voice count in the driver can be incremented (see above).

2. We unpack the command.

3. The volume is sent to a `sig~` object, which is multiplied (`*~`) with
   the sample output.

4. The `play~` object is told which buffer to use.

5. Finally, we compute a `start` command for the `play~` object, which needs
   to know three things: start and end of the sample to play, and how much
   time to take to do so. The latter we compute by computing the length and
   then scaling the result if we need to transpose:

   ![](/docs/img/ks-voice-Start.png)

When the `play~` object tells us the sample finished playing, it sends a `bang`
on its third outlet; we use this to send a 0 to `thispoly~`, indicating the
voice is available again, and to our output, so that the voice count can be
decremented.
