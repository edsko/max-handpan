/**
 * Max Handpan
 *
 * @module handpan.iface.state
 * @description Handpan interface state
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2020-2021
 * @see {@link https://github.com/edsko/max-handpan}
 * @license BSD-3-Clause
*/

/*******************************************************************************
  Public API
*******************************************************************************/

 /**
  * Initial state
  *
  * TODO: Currently both the colors and the positions of the buttons are
  * entirely static. At some point we may want to make this configurable.
  *
  * @constructor
  */
exports.HandpanIfaceState = function() {
  /**
   * Number of tone fields
   *
   * @member {number}
   */
  this.fields = 9;

  /**
   * Colors
   */
  this.colors = {
    doum: 3
  , tak: 6
  , tonefieldMid: 16
  , tonefieldSlap: 20
  };
}

exports.HandpanIfaceState.prototype = {
  /**
   * Position of the doum and the two taks
   */
  positionOfDoum: function() {
    return {col: 3, row: 4};
  }

  /**
   * Position of the taks
   */
, positionOfTaks: function() {
    return [{col: 2, row: 4}, {col: 5, row: 4}];
  }

  /**
   * Position of the various tonefields
   *
   * @param {number} tonefield Tonefield (1-9)
   */
, positionOfTonefield: function(tonefield) {
    switch(tonefield) {
      // Bottom tonefield (1)
      case 1: return [{col: 4, row: 6}, {col: 4, row: 5}];
      // Left tonefields (2, 4, 6)
      case 2: return [{col: 1, row: 5}, {col: 2, row: 5}];
      case 4: return [{col: 1, row: 3}, {col: 2, row: 3}];
      case 6: return [{col: 1, row: 1}, {col: 2, row: 1}];
      // Right tonefields (3, 5, 7)
      case 3: return [{col: 6, row: 5}, {col: 5, row: 5}];
      case 5: return [{col: 6, row: 3}, {col: 5, row: 3}];
      case 7: return [{col: 6, row: 1}, {col: 5, row: 1}];
      // Top tonefields (8, 9)
      case 8: return [{col: 3, row: 1}, {col: 3, row: 2}];
      case 9: return [{col: 4, row: 1}, {col: 4, row: 2}];
      default:
        error("Unknown tonefield " + tonefield + "\n");
        break;
    }
  }
}
