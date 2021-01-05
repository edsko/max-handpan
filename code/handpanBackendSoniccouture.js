/**
 * Max Handpan
 *
 * @module handpanBackendSoniccouture
 * @description Backend for {@link https://www.soniccouture.com/en/products/35-rare-and-unique/g29-pan-drums/ | Soniccouture Pan Drums}
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2020-2021
 * @see {@link https://github.com/edsko/max-handpan}
 * @license BSD-3-Clause
 */

/*******************************************************************************
  Device initialization
*******************************************************************************/

inlets    = 3;
outlets   = 3;
autowatch = 0;

/*******************************************************************************
  Imports
*******************************************************************************/

var OurTrack = require("liveOurtrack").OurTrack;
var State    = require("handpanBackendState").HandpanBackendState;
var Handpan  = require("handpanGeneric");

/*******************************************************************************
  Global variables
*******************************************************************************/

var ourTrack    = null;
var strike      = null;
var interpreted = null;
var state       = new State();

/*******************************************************************************
  Handle M4L messages
*******************************************************************************/

/**
 * Initialize device
 */
function init() {
  outerThis = this;

  ourTrack = new OurTrack(this, function(trackNo, selected) {
    // TODO: Handle changes
  });

  strike = new LiveAPI(null, ["id", ourTrack.findParameter("Strike")]);

  stateUpdated();
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

  var pitchIn      = arguments[0];
  var velocity     = arguments[1];
  var fromMIDI     = Handpan.fromMIDI(pitchIn);
  var articulation = fromMIDI[0];
  var zone         = fromMIDI[1];

  if(zone >= interpreted.length) {
    // Not all scales are long enough for the number of tone fields
    // We might want to reconsider how to deal with this at some point.
    return;
  }

  var pitchOut = interpreted[zone];

  // Pick a sample (assuming mod wheel is mapped to sample selection)
  var randomSample;
  with (Math) {
    randomSample = Math.round(1 + random() * 126);
  }

  with (Handpan.Articulation) {
    switch(articulation) {
      case MID:
        strike.set("value", 0);
        outlet(1, [1, randomSample]);
        break;

      case SLAP:
        strike.set("value", 50);

        // For the taks, we also use an octave higher
        if(zone == Handpan.Zone.DOUM) {
          pitchOut += 12;
        }

        // For the slaps we don't use random sample selection for low velocity
        // This makes ghost notes more reliable
        if(velocity < 50) {
          outlet(1, [1, 1]);
        } else {
          outlet(1, [1, randomSample]);
        }
        break;

      default:
        error("Unknown articulation\n");
        break;
    }
  }

  outlet(0, [pitchOut, velocity]);
}

/**
 * Route other messages
 */
function anything() {
  switch(messagename) {
    // Messages that update the state
    case 'scale':
      state[messagename] = arguments[0];
      stateUpdated();
      break;

    default:
      error("Message '" + messagename + "' not understood\n");
      break;
  }
}

/*******************************************************************************
  Internal
*******************************************************************************/

/**
 * Respond to updates to the state
 *
 * @private
 */
function stateUpdated() {
  // Lowest note on the mk1 is 57 (A).
  // TODO: It would be nice if we could support both mk1 and mk2.
  interpreted = state.interpretScale(57);
}
stateUpdated.local = 1;
