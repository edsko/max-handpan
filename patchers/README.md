# Patchers

NOTE: The JavaScript in this code is generated by the PureScript compiler.

## Monitor if the track is selected (`trackselected`)

This patcher monitors the track on which the device lives, and provides
notifications when this changes. This allows for example to grab control
over the Push controller when the track is selected, and release control when
the track is deselected. The basic approach taken in this patcher is
described in
[Designing a custom Push2 instrument in Max for Live/JavaScript: Part 1][trichords1],
but the details are a bit different.

In particular, rather than setting up monitoring threads within JavaScript,
we instead handle this Max side. This has two advantages:

* It is visible at a glance what the patcher is monitoring
  (this is an example where the visual layout of Max is really helpful)
* We avoid the problems described in the blog post with deleting observers.

We are careful to defer dealing with notifications; see comments within the
patcher itself. For some background on this, see

* https://docs.cycling74.com/max8/vignettes/live_api_overview#Notifications
* https://cycling74.com/articles/event-priority-in-max-scheduler-vs-queue
* https://cycling74.com/tutorials/advanced-max-learning-about-threading

## Velocity curve (`velocity`)

This is intended for use with `bpatcher`. Max for Live is a bit finicky when
it comes to using [parameter names][hashvars] with dials etc., but if you add the patcher
using

```
bpatcher velocity @args SomeName
```

and then save the patcher, you should see controls `SomeName.Drive` etc.
appear in a `live.banks` instance.

[hashvars]: https://docs.cycling74.com/max8/vignettes/dollar_sign_and_pound_sign
[trichords1]: http://edsko.net/2020/12/26/trichords-part1/