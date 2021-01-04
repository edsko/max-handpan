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
  this.scale = 0;
}
