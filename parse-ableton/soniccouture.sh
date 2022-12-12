#!/bin/bash

PARSE="cabal run parse-ableton --"

$PARSE examples/mk1-precision.adg invert-msp | friendly >examples/inverted.mk1
$PARSE examples/mk2-precision.adg invert-msp | friendly >examples/inverted.mk2

$PARSE examples/mk1-precision.adg summarise-msp --collate-velocities | friendly >examples/summary.mk1
$PARSE examples/mk2-precision.adg summarise-msp --collate-velocities | friendly >examples/summary.mk2
