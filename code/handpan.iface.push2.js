/**
 * Max Handpan
 *
 * @module handpan.iface.push2
 * @description Handpan interface on the Ableton Push 2
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2020-2021
 * @see {@link https://github.com/edsko/max-handpan}
 * @license BSD-3-Clause
 */

/*******************************************************************************
  Device initialization

  Disable automatch because before the device is disabled, the 'deleteObservers'
  message should be sent.
*******************************************************************************/

inlets    = 2;
outlets   = 3;
autowatch = 0;

/*******************************************************************************
  Imports
*******************************************************************************/

var Push     = require("push2.controller").Push;
var OurTrack = require("live.ourtrack").OurTrack;
var State    = require("handpan.iface.state").HandpanCtrlState;
var Handpan  = require("handpan.generic");

/*******************************************************************************
  Global variables
*******************************************************************************/

var push     = null;
var ourTrack = null;
var state    = new State();

/*******************************************************************************
  Handle M4L messages
*******************************************************************************/

/**
 * Initialize the device.
 *
 * Should be called when the M4L device is fully loaded
 * (use <code>live.thisdevice</code>).
 */
function init() {
  deleteObservers();

  push     = new Push(this);
  ourTrack = new OurTrack(this, function(trackNo, selected) {
    push.controlButtonMatrix(selected);
  });

  with (Handpan) {
    /*
     * Set up central doum area
     */
    var pos = state.positionOfDoum();
    push.setColor(pos.col, pos.row, state.colors.doum);
    push.setAction(pos.col, pos.row, sendNote(Articulation.MID, Zone.DOUM));

    var taks = state.positionOfTaks();
    for(tak in taks) {
      pos = taks[tak];
      push.setColor(pos.col, pos.row, state.colors.tak);
      push.setAction(pos.col, pos.row, sendNote(Articulation.SLAP, Zone.DOUM));
    }

    /*
     * Set up the actions for the tonefields
     *
     * The colors are set up in 'updatePush' because they vary depending on
     * settings.
     */
    for(var i = 1; i <= 9; i++) {
      pos  = state.positionOfTonefield(i);
      zone = Zone.TONEFIELD_1 + (i - 1);
      push.setAction(pos[0].col, pos[0].row, sendNote(Articulation.MID, zone));
      push.setAction(pos[1].col, pos[1].row, sendNote(Articulation.SLAP, zone));
    }
  }

  // Update the push with our current state
  // The state will have been updated in response to the messages from
  // 'live.dial' (or 'pattr') before 'live.thisdevice' calls 'init'.
  updatePush();
}

/**
 * Delete all observers.
 *
 * Should be called when the device is reloaded or removed.
 */
function deleteObservers() {
  if(ourTrack != null) ourTrack.deleteObservers();
  if(push     != null) push.deleteObservers();
}

/**
 * Dispatch other messages
 */
function anything() {
  switch(messagename) {
    // Messages that update the state
    case 'fields':
      state[messagename] = arguments[0];
      updatePush();
      break;

    // Messages we just forward directly to the push object
    case 'showColors':
    case 'controlButtonMatrix':
      if(push != null) {
        push[messagename].apply(push, arguments);
      }
      break;

    default:
      error("Message '" + messagename + "' not understood\n");
      break;
  }
}

/*******************************************************************************
  Internal functions
*******************************************************************************/

/**
 * Update the Push to reflect our internal state
 *
 * @private
 */
function updatePush() {
  for(var i = 1; i <= 9; i++) {
    var pos = state.positionOfTonefield(i);
    if(i <= state.fields) {
      push.setColor(pos[0].col, pos[0].row, state.colors.tonefieldMid);
      push.setColor(pos[1].col, pos[1].row, state.colors.tonefieldSlap);
    } else {
      for(var j = 0; j < 2; j++) {
        push.setColor(pos[j].col, pos[j].row, 0);
      }
    }
  }
}
updatePush.local = 1;

/**
 * Send the played note to the appropriate outlet
 *
 * @param {number} articulation Articulation
 * @param {number} zone Zone
 * @private
 */
function sendNote(articulation, zone) {
  var outletNo;

  switch(articulation) {
    case Handpan.Articulation.MID:
      outletNo = 0;
      break;
    case Handpan.Articulation.SLAP:
      outletNo = 1;
      break;
    default:
      error("sendNote: unknown articulation " + articulation + "\n");
      break;
  }

  return function(col, row, color, velocity) {
    outlet(outletNo, [Handpan.toMIDI(articulation, zone), velocity]);
  }
}
sendNote.local = 1;
