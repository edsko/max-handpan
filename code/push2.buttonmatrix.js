/**
 * Max Handpan
 *
 * @module push2.buttonmatrix
 * @description Interface to the button matrix of the Push2 controller.
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2020-2021
 * @see {@link https://github.com/edsko/max-handpan}
 * @license BSD-3-Clause
 */

/**
 * Function called whenever a button on the Push2 is pressed.
 *
 * @callback buttonCallback
 * @see {module:buttonmatrix.ButtonMatrix}
 * @param {number} col Column
 * @param {number} row Row
 * @param {number} velocity Velocity
 */

/*******************************************************************************
  Public API
*******************************************************************************/

/**
 * Interface to the Push2 button matrix.
 *
 * @constructor
 * @param {Object} push Reference to the Push2 controller
 * @param {Object} object Object to call the callback on
 * @param {module:buttonmatrix~buttonCallback} callback Callback
 */
exports.ButtonMatrix = function(push, object, callback) {
  this.buttonMatrix = findButtonMatrix(push, function(args) {
    if(args[0] == "value" && args.length == 5) {
      callback.call(object, args[2], args[3], args[1]);
    }
  });
}

exports.ButtonMatrix.prototype = {
  /**
   * Set the color of one of the buttons
   *
   * @param {number} col Column
   * @param {number} row Row
   * @param {number} color New color
   */
  setColor: function(col, row, color) {
    this.buttonMatrix.call("send_value", col, row, color);
  }

  /**
   * Delete all callbacks.
   *
   * This will cause the <code>ButtonMatrix</code> to stop listening for key
   * presses. Should should be called before the button matrix falls out of
   * scope.
   */
, deleteObservers: function() {
    this.buttonMatrix.property = "";
  }
}

/*******************************************************************************
  Private functions
*******************************************************************************/

/**
 * Find the button matrix on the Push2 controller
 *
 * @private
 */
function findButtonMatrix(push, callback) {
  var buttonMatrixId = push.call("get_control", "Button_Matrix");
  var buttonMatrix   = new LiveAPI(callback, buttonMatrixId);
  buttonMatrix.property = "value"; // Monitor the buttons
  return buttonMatrix;
}
