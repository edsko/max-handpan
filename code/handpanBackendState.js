/**
 * Max Handpan
 *
 * @module handpanBackendState
 * @description Handpan backend state
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2020-2021
 * @see {@link https://github.com/edsko/max-handpan}
 * @license BSD-3-Clause
*/

/*******************************************************************************
  Imports
*******************************************************************************/

var Handpan = require("handpanGeneric");

/*******************************************************************************
  Public API
*******************************************************************************/

/**
 * Initial backend state
 *
 * @constructor
 */
exports.HandpanBackendState = function() {
  /**
   * Selected scale
   *
   * @member {module:handpanGeneric.Scale}
   */
  this.scale = Handpan.Scale.KURD_9;
}

exports.HandpanBackendState.prototype = {
  /**
   * Interpret currently selected scale for a particular instrument
   *
   * @param {number} lo Lowest available MIDI pitch (0-127) on the instrument
   */
  interpretScale: function(lo) {
    // Translate note to pitch
    //
    // We start with everything initialized to their lowest possible value.
    var pitches = [];
    with (Handpan) {
      for(var i = 0; i < 12; i++) {
        pitches[pitchToNote(lo + i)] = lo + i;
      }
    }

    // Interpret the scale
    //
    // We always try the lowest possible pitch for each note that is still
    // higher then the previous. This places the scale as low as possible on the
    // mapping, but means that the scale is still monotonically increasing.
    // (Players can of course use a standard +12 effect if desired.)
    var scale       = Handpan.scales[this.scale];
    var interpreted = [];
    var offset      = 0;
    var prev        = -1;
    for(step in scale) {
      var note  = scale[step];
      var pitch = pitches[note];
      if (pitch + offset <= prev) {
        offset += 12;
      }
      interpreted[step] = pitch + offset;
      prev = interpreted[step];
    }

    return interpreted;
  }
}
