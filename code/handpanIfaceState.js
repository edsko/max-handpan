/**
 * Max Handpan
 *
 * @module handpanIfaceState
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
  , tonefieldGhost: 20
  , tonefieldSlap: 68
  , gu: 74
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
   *
   * Returns multiple positions, all of which should be mapped to the
   * same sound.
   */
, positionOfTaks: function() {
    return [{col: 2, row: 4}, {col: 5, row: 4}];
  }

  /**
   * Position of the gu
   *
   * Returns multiple positions, all of which should be mapped to the
   * same sound.
   */
, positionOfGu: function() {
    return [ {col: 0, row: 7}
           , {col: 1, row: 7}
           , {col: 6, row: 7}
           , {col: 7, row: 7}
           ];
  }

  /**
   * Position of the slaps.
   *
   * We associate one slap with each tonefield.
   *
   * @param {number} tonefield Tonefield (1-9)
   */
, positionOfSlap: function(tonefield) {
    switch(tonefield) {
      // Bottom tonefield (1)
      case 1: return {col: 5, row: 7};
      // Left tonefields (2, 4, 6)
      case 2: return {col: 0, row: 6};
      case 4: return {col: 0, row: 4};
      case 6: return {col: 0, row: 2};
      // Right tonefields (3, 5, 7)
      case 3: return {col: 7, row: 6};
      case 5: return {col: 7, row: 4};
      case 7: return {col: 7, row: 2};
      // Top tonefields (8, 9)
      case 8: return {col: 2, row: 0};
      case 9: return {col: 5, row: 0};
      default:
        error("Unknown tonefield " + tonefield + "\n");
        break;
    }
  }


  /**
   * Position of the various tonefields
   *
   * @param {number} tonefield Tonefield (1-9)
   */
, positionOfTonefield: function(tonefield) {
    switch(tonefield) {
      // Bottom tonefield (1)
      case 1: return [{col: 4, row: 7}, {col: 4, row: 6}];
      // Left tonefields (2, 4, 6)
      case 2: return [{col: 0, row: 5}, {col: 1, row: 5}];
      case 4: return [{col: 0, row: 3}, {col: 1, row: 3}];
      case 6: return [{col: 0, row: 1}, {col: 1, row: 1}];
      // Right tonefields (3, 5, 7)
      case 3: return [{col: 7, row: 5}, {col: 6, row: 5}];
      case 5: return [{col: 7, row: 3}, {col: 6, row: 3}];
      case 7: return [{col: 7, row: 1}, {col: 6, row: 1}];
      // Top tonefields (8, 9)
      case 8: return [{col: 3, row: 0}, {col: 3, row: 1}];
      case 9: return [{col: 4, row: 0}, {col: 4, row: 1}];
      default:
        error("Unknown tonefield " + tonefield + "\n");
        break;
    }
  }
}
