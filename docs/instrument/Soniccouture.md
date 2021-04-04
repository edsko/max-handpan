# Instrument: Soniccouture Pan Drum

We have two custom instruments for the Soniccouture Pan Drum: one is setup
for the samples of the first generation Hang, and one for the samples of the
second generation Hang. See
[the architecture overview](/docs/Architecture.md#instrument) for an
explanation of why we cannot use the original instrument and must use a
custom instrument.

The sample data of course _is_ the original Soniccouture sample data; the
`Sampler` setup is translated to the LUTs required by our custom
[key-switched sampler](/docs/instrument/KeySwitchedSampler.md) by parsing
the original Ableton instrument group and generating LUTs in the right format;
I wrote a
[custom tool specifically to do that](/parse-ableton).

As would any instance of the key-switched sampler, the top-level patcher is
just an instance of `ks-driver` along with buffers for each of the samples
used. For the first generation hang, this looks like

![](/docs/img/SoniccoutureMk1.png)

where each of those subpatches connects the `live.drop` object to a `buffer~`:

![](/docs/img/SoniccoutureMk1-buffer.png)

The second generation hang instance looks very similar.

Requiring the user to drag each and every sample into the instrument is
extremely annoying and tedious, but it seems that there is no way around this:
[referring directly to samples in Live Factory Packs from a Max for Live device is apparently impossible](https://cycling74.com/forums/how-to-load-sample-from-ableton-factory-packs-into-buffer~).

See the discussion of the
[key-switched sampler](/docs/instrument/KeySwitchedSampler.md)
itself for details on how it works.
