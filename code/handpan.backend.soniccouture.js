/**
 * Max Handpan
 *
 * @module handpan.backend.soniccouture
 * @description Background for {@link https://www.soniccouture.com/en/products/35-rare-and-unique/g29-pan-drums/ | Soniccouture Pan Drums}
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2020-2021
 * @see {@link https://github.com/edsko/max-handpan}
 * @license BSD-3-Clause
 */

/*******************************************************************************
  Device initialization
*******************************************************************************/

inlets    = 3;
outlets   = 2;
autowatch = 0;

/*******************************************************************************
  Imports
*******************************************************************************/

var OurTrack = require("live.ourtrack").OurTrack;

/*******************************************************************************
  Global variables
*******************************************************************************/

var ourTrack = null;

/*******************************************************************************
  Handle M4L messages
*******************************************************************************/

/**
 * Initialize device
 */
function init() {
  post("init\n");

  ourTrack = new OurTrack(this, function(trackNo, selected) {
    post("our track changed! :-o\n");
  });

  var strikeId = ourTrack.findParameter("Strike");
  var strike   = new LiveAPI(null, ["id", strikeId]);
  strike.set("value", 5);
}

/**
 * Delete observers
 */
function deleteObservers() {
  if(ourTrack != null) ourTrack.deleteObservers();
}

/**
 * Handle incoming MIDI messages
 */
function list() {
  if(arguments.length != 2) {
    error("list: message not understood\n");
    return;
  }

  var pitch    = arguments[0];
  var velocity = arguments[1];

  post("list", pitch, velocity, "\n");

  outlet(0, [pitch, velocity]);
}

/**
 * Route other messages
 */
function anything() {
  switch(messagename) {
    // Messages that update the state
    case 'scale':
      // TODO
      break;

    default:
      error("Message '" + messagename + "' not understood\n");
      break;
  }
}
