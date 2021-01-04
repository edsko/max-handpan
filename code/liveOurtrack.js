/**
 * Max Handpan
 *
 * @module liveOurtrack
 * @description Interface to the track on which the M4L device is loaded.
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2020-2021
 * @see {@link https://github.com/edsko/max-handpan}
 * @license BSD-3-Clause
 */

/**
 * Callback used by {@link module:liveOurtrack.OurTrack}
 *
 * @callback observerCallback
 * @param {number} trackNo The number of the track we're on
 * @param {boolean} selected Whether or not the track was selected
 */

/*******************************************************************************
  Public API
*******************************************************************************/

/**
 * Interface to the track the Max for Live device is located on.
 * Should not be called until the M4L device is fully initialized.
 *
 * @constructor
 * @param {Object} object Object to call the callbak on
 * @param {module:ourtrack~observerCallback} callback Callback
 */
exports.OurTrack = function(object, callback) {
  var outerThis = this;

  // Set up our initial state
  this.update(object, callback);

  // We can use the relative path "this_device canonical_parent" to get the
  // track the device is currently on. However, we cannot observe relative
  // paths. We therefore monitor this indirectly:

  // 1. We first get the initial canonical path to this device, something like
  // "live_set tracks 0 devices 2"
  var initialPath = new LiveAPI(null, "this_device").unquotedpath;

  // 2. This gives us a canonical path that we can monitor. We need to set the
  // mode to '1', because if our device is moved to a different track, we want
  // to be notified.
  //
  // We make this part of 'this' so that it doesn't get GCed.
  this.monitorTrack = new LiveAPI(function(args) {
    var currentPath = new LiveAPI(null, "this_device").unquotedpath;

    if(currentPath != outerThis.monitorTrack.unquotedpath) {
      // 3. If the device is moved, we must then update the path we monitor
      outerThis.monitorTrack.path = currentPath;
      outerThis.update(object, callback);
    }
  });
  this.monitorTrack.path = initialPath;
  this.monitorTrack.mode = 1;

  // In addition to monitoring which track we are on, we also want to monitor
  // which track is currently selected. Fortunately, this is a little easier.
  this.monitorSelected = new LiveAPI(function(args) {
    if(args[0] == "selected_track") {
      outerThis.update(object, callback);
    }
  });
  this.monitorSelected.path     = "live_set view";
  this.monitorSelected.property = "selected_track";
}

/**
 * Class
 */
exports.OurTrack.prototype = {
  /**
   * Report if our track is currently selected
   *
   * @returns {boolean} <code>true</code> if the track is currently selected.
   */
  getIsSelected: function() {
    return this.selected;
  }

  /**
   * Report the number of the track this device lives on
   *
   * @returns {number} track number
   */
, getTrackNo: function() {
    return this.trackNo;
  }

, /**
   * Check again which track we are on, and whether it is selected.
   *
   * There should normally be no need to call this function manually.
   *
   * @param {Object} object Object to call the callbak on
   * @param {module:ourtrack~observerCallback} callback Callback
   */
  update: function(object, callback) {
    var ourTrack      = new LiveAPI(null, "this_device canonical_parent");
    var selectedTrack = new LiveAPI(null, "live_set view selected_track");
    var selected      = ourTrack.id == selectedTrack.id;
    var trackNo       = parseInt(ourTrack.unquotedpath.split(" ")[2]);

    if(this.selected != selected || this.trackNo != trackNo) {
      this.selected = selected;
      this.trackNo  = trackNo;
      callback.call(object, trackNo, selected);
    }
  }

  /**
   * Find parameter by name (on any device) in current track
   */
, findParameter: function(name) {
    var track      = new LiveAPI("live_set tracks " + this.trackNo);
    var numDevices = track.getcount("devices");

    for(var i = 0; i < numDevices; i++) {
      var device    = new LiveAPI("live_set tracks " + this.trackNo + " devices " + i);
      var numParams = device.getcount("parameters");

      for(var j = 0; j < numParams; j++) {
        var parameter = LiveAPI("live_set tracks " + this.trackNo + " devices " + i + " parameters " + j);

        if(parameter.get("name") == name) {
          return parameter.id;
        }
      }
    }

    return null;
  }

  /**
   * Delete all callbacks.
   *
   * This means we will stop watching the track.
   *
   * It is important to call this function before allowing the 'OurTrack' to
   * fall out of scope, otherwise these views and their callbacks will not
   * be GCed and the callback will continue to be called.
   */
, deleteObservers: function() {
    this.monitorTrack.mode = 0;
    this.monitorSelected.path = ""; // Setting to 'null' does not work!
  }
};
