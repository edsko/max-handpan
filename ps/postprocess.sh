#!/bin/bash

## Post-process the output of psc

cat max-module-prefix.js $1-purescript max-module-suffix.js > $1
