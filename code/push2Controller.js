/**
 * Max Handpan
 *
 * @module push2Controller
 * @description Interface to the Push2 controller.
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2020-2021
 * @see {@link https://github.com/edsko/max-handpan}
 * @license BSD-3-Clause
 */

/**
 * Function to be called when a button is pressed.
 *
 * @callback actionCallback
 * @see module:push2Controller.Push#setAction
 * @param {number} col Column
 * @param {number} row Row
 * @param {number} color Current color of the button
 * @param {number} velocity Velocity
 */

/*******************************************************************************
  Imports
*******************************************************************************/

var ButtonMatrix = require("push2Buttonmatrix").ButtonMatrix;
var Grid         = require("utilGrid").Grid;

/*******************************************************************************
  Public API
*******************************************************************************/

/**
 * Interface to the Push2 controller.
 * Should not be called until the M4L device is fully initialized.
 *
 * @constructor
 * @param {Object} object
 *   Object for action callbacks (see {@link module:push2Controller.Push#setAction})
 */
exports.Push = function(object) {
  this.actionObject = object;
  this.controller   = findPush();
  this.colorGrid    = new Grid(8, 8, 0);
  this.actionGrid   = new Grid(8, 8, null);
  this.buttonMatrix = new ButtonMatrix(this.controller, this, buttonPressed);
};

exports.Push.prototype = {
  /**
   * Check if the Push2 controller was found
   *
   * @returns {boolean}
   *   <code>true</code> if the push was found
   */
  checkFound: function() {
    return (this.controller != null);
  }

  /**
   * Set control of the button matrix
   *
   * @param {boolean} control
   *   <code>true</code> if we want to control the button matrix
   */
, controlButtonMatrix: function(control) {
    if(!this.checkFound()) return;

    if(control) {
      this.controller.call("grab_control", "Button_Matrix");

      // We initialize the colors after a short delay. If we initialize them
      // right here, switching between tracks works just fine within Ableton
      // itself, but for some reason it does not work if we switch track using
      // the buttons on the Push.
      var initColorsTask = new Task(initColors, this);
      initColorsTask.schedule(10);
    } else {
      this.controller.call("release_control", "Button_Matrix");
    }
  }

  /**
   * Set the color of one of the buttons
   *
   * @param {number} col Column
   * @param {number} row Row
   * @param {number} color New color
   */
, setColor: function(col, row, color) {
    if(!this.checkFound()) return;
    this.colorGrid.set(col, row, color);
    this.buttonMatrix.setColor(col, row, color);
  }

  /**
   * Set action for one of the buttons
   *
   * @param {number} col Column
   * @param {number} row Row
   * @param {module:push~actionCallback} callback Callback
   */
, setAction: function(col, row, callback) {
    this.actionGrid.set(col, row, callback);
  }

  /**
   * Show all possible colors
   *
   * @param {number} page Which page of colors (0 or 1)
   */
, showColors: function(page) {
    this.colorGrid.fill(page * 64, 1);
    initColors.call(this);
  }

  /**
   * Delete all observers.
   *
   * Should be called before the object falls out of scope.
   */
, deleteObservers: function() {
    this.buttonMatrix.deleteObservers();
  }
};

/*******************************************************************************
  Private functions
*******************************************************************************/

/**
 * Find the Push2 controller
 *
 * NOTE: We do not take into account that there might be more than one.
 *
 * @private
 */
function findPush() {
  var liveApp            = new LiveAPI(null, "live_app")
  var numControlSurfaces = liveApp.getcount("control_surfaces");

  for (var i = 0; i < numControlSurfaces; i++) {
    var controlSurface = new LiveAPI(null, "control_surfaces " + i);
    if (controlSurface.type == "Push2") {
      return controlSurface;
    }
  }

  return null;
}

/**
 * Initialize the colors of the button matrix after grabbing control
 *
 * @private
 */
function initColors() {
  this.colorGrid.traverse(this.buttonMatrix, this.buttonMatrix.setColor);
}

/**
 * Handle button presses
 *
 * @private
 */
function buttonPressed(col, row, velocity) {
  var action = this.actionGrid.get(col, row);
  var color  = this.colorGrid.get(col, row);
  post("buttonPressed", col, row, velocity, color, "\n");
  if (action != null) {
    action.call(this.actionObject, col, row, color, velocity);
  }
}
