# Scale selection

The scale selector has UI elements to select the scale and to transpose the doum
and the scale independently. The only input is to configure the natural tuning
of the doum and the root of the scale of whatever backend is using the scale
selector.

![](/docs/img/scaleselector.png)

Apart from the UI elements, the scale selector is entirely [implemented in
PureScript](/ps/src/ScaleSelector.purs). Whenever any input is changed, it
outputs a series of MIDI note values that describe the selected scale with the
specified transpositions applied.
