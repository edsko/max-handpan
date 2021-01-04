/**
 * Max Handpan
 *
 * @module utilGrid
 * @description Simple 2D array
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2020-2021
 * @see {@link https://github.com/edsko/max-handpan}
 * @license BSD-3-Clause
 */

/**
 * Callback used by {@link module:utilGrid.Grid#init}.
 *
 * @callback initCallback
 * @param {number} col Column
 * @param {number} row Row
 * @returns {*} New value for the cell
 */

/**
 * Callback used by {@link module:utilGrid.Grid#traverse}.
 *
 * @callback traverseCallback
 * @param {number} col Column
 * @param {number} row Row
 * @param {*} value Value of the cell
 */

/*******************************************************************************
  Public API
*******************************************************************************/

/**
 * Simple 2D array of values
 *
 * @constructor
 * @param {number} cols Number of columns
 * @param {number} rows Number of rows
 * @param {*} init Initial value for each cell
 */
exports.Grid = function(cols, rows, init) {
  this.grid = new Array();

  for(var i = 0; i < cols; i++) {
    var col = new Array();

    for(var j = 0; j < rows; j++) {
      col[j] = init;
    }

    this.grid[i] = col;
  }
}

exports.Grid.prototype = {
  /**
   * Get the value of a cell in the grid
   *
   * @param {number} col Column
   * @param {number} row Row
   * @returns {*} Value of the selected cell
   */
  get: function(col, row) {
    return this.grid[col][row];
  }

  /**
   * Set a cell in the grid
   *
   * @param {number} col Column
   * @param {number} row Row
   * @param {*} value New value
   */
, set: function(col, row, value) {
    this.grid[col][row] = value;
  }

  /**
   * Call a function for each cell in the grid.
   *
   * @param {Object} object Object to invoke the callback on
   * @param {module:grid~traverseCallback} callback Callback
   */
, traverse: function(object, callback) {
    for(var i in this.grid) {
      var col = this.grid[i];

      for(var j in col) {
        callback.call(object, i, j, col[j]);
      }
    }
  }

  /**
   * Initialize each cell in the grid using the callback.
   *
   * @param {module:grid~initCallback} callback Callback
   */
, init: function(callback) {
    for(var i in this.grid) {
      var col = this.grid[i];

      for(var j in col) {
        col[j] = callback(i, j);
      }
    }
  }

  /**
   * Fill the grid with consecutive numbers.
   *
   * @param {number} from Starting value
   */
, fill: function(from) {
    this.init(function(col, row) { return from++; });
  }
}
