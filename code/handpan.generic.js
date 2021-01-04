/**
 * Max Handpan
 *
 * @module handpan.generic
 * @description Generic definitons (independent of interface or backend)
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2020-2021
 * @see {@link https://github.com/edsko/max-handpan}
 * @license BSD-3-Clause
 */

/**
 * Different ways of hitting the handpan
 *
 * @enum {number}
 */
exports.Articulation = {
  /**
   * Hit a tone field in the center (default)
   */
  MID: 0

  /**
   * Slap near the edge (useful for taks/ghost notes)
   */
, SLAP: 1
};

/**
 * Different zones of the handpan
 *
 * @enum {number}
 */
exports.Zone = {
  DOUM: 0
, TONEFIELD_1: 1
, TONEFIELD_2: 2
, TONEFIELD_3: 3
, TONEFIELD_4: 4
, TONEFIELD_5: 5
, TONEFIELD_6: 6
, TONEFIELD_7: 7
, TONEFIELD_8: 8
, TONEFIELD_9: 9
};

/**
 * Convert zone/artificulation to MIDI note.
 *
 * This is how the handpan interface and the handpan backend communicate.
 *
 * @see fromMIDI
 */
exports.toMIDI = function(articulation, zone) {
  // We don't use the first 4 octaves (0-47)
  return 48 + (articulation * 12) + zone;
}

/**
 * Convert MIDI note to zone/articulation
 *
 * @see toMIDI
 */
exports.fromMIDI = function(pitch) {
  var articulation = Math.floor(pitch / 12) - 4;
  var zone         = pitch % 12;
  return [articulation, zone];
}


/**
 * Scale
 *
 * @enum {number}
 */
exports.Scale = {
  KURD_9: 0
, MAJOR: 1
}

// String representation of the above for use in the dial config
// TODO: Ideally we'd set that programatically.
// "Kurd 9" Major

/**
 * Scale degree
 *
 * @enum {number}
 */
exports.Note = {
    C : 0
  , Cs: 1
  , Db: 1
  , D : 2
  , Ds: 3
  , Eb: 3
  , E : 4
  , F : 5
  , Fs: 6
  , Gb: 6
  , G : 7
  , Gs: 8
  , Ab: 8
  , A : 9
  , As: 10
  , Bb: 10
  , B : 11
  };

/**
 * Notes in supported scales
 *
 * TODO: For now these are all defined with 9 notes. We might need to rethink
 * the relation between the number of tonefields and the scales.
 */
exports.scales = {};

with(exports.Note) {
  with(exports.Scale) {
    exports.scales[KURD_9] = [D, A, Bb, C, D, E, F, G, A, C];

    // No idea if there even is a handpan with a major scale or how it's
    // laid out. This is just a test for now.
    exports.scales[MAJOR]  = [C, C, D, E, F, G, A, B, C];
  }
}
