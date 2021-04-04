# Velocity curve

The velocity curve is implemented as a single LUT:

![](/docs/img/velocity.png)

The curve itself is
[computed in PureScript](/ps/src/Velocity.purs);
actual note processing does not involve any PureScript/JavaScript at all. For
some background on the mathematics behind the curve, see
[Reconstructing the Ableton velocity compand curve in Max for Live](http://edsko.net/2021/01/03/velocity-curve/).
