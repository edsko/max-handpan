/**
 * Max Handpan
 *
 * @module handpanGeneric
 * @description Generic definitons (independent of interface or backend)
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2020-2021
 * @see {@link https://github.com/edsko/max-handpan}
 * @license BSD-3-Clause
 */

/**
 * Different ways of hitting the handpan
 *
 * TODO: We should probably rephrase this in terms of position on the
 * handpan: mid, rim, shoulder, etc.
 *
 * https://www.youtube.com/watch?v=dCWX1JbxJi0 calls it
 *
 * - Inner shoulder ("tak")
 * - Outer shoulder ("ghost notes")
 * - Rim ("slaps")
 *
 * @enum {number}
 */
exports.Articulation = {
  /**
   * Hit a tone field in the center (default)
   */
  MID: 0

  /**
   * Hit near the edge of a tone field/doum. Useful for taks/ghost notes.
   */
, GHOST: 1

  /**
   * Hit in between tone fields. Useful for slaps.
   */
, SLAP: 2
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
  // Kurd 9
  KURD_9: 0

  // Hijaz, also known as Hitzaz
  // NOTE: We're using a Hijaz in D over a root of G.
, HIJAZ: 1
}

// String representation of the above for use in the dial config
// TODO: Ideally we'd set that programatically.
// "Kurd 9" Hijaz

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
 * Translate MIDI pitch to note
 *
 * @param {number} pitch MIDI pitch (0-127)
 * @returns {module:handpanGeneric.Note}
 */
exports.pitchToNote = function(pitch) {
  return (pitch % 12);
}

/**
 * Notes in supported scales
 *
 * TODO: For now these are all defined with 9 notes. We might need to rethink
 * the relation between the number of tonefields and the scales.
 */
exports.scales = {};

with(exports.Note) {
  with(exports.Scale) {
    exports.scales[KURD_9] = [D, A, Bb, C,  D, E, F,  G, A, C];
    exports.scales[HIJAZ]  = [G, D, Eb, Fs, G, A, Bb, C, D];
  }
}
