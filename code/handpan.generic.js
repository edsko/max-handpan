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
 * Different parts of the handpan
 *
 * @enum {number}
 */
exports.Note = {
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
