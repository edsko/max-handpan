/**
 * Max Handpan
 *
 * @module handpan.backend.soniccouture
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
outlets   = 2;
autowatch = 0;

/*******************************************************************************
  Imports
*******************************************************************************/

var OurTrack = require("live.ourtrack").OurTrack;
var Handpan  = require("handpan.generic");

/*******************************************************************************
  Global variables
*******************************************************************************/

var ourTrack    = null;
var strike      = null;
var interpreted = interpretScale(Handpan.scales[Handpan.Scale.KURD_9]);

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
  var pitchOut     = interpreted[zone];

  with (Handpan.Articulation) {
    switch(articulation) {
      case MID:
        strike.set("value", 0);
        break;
      case SLAP:
        strike.set("value", 50);
        // For the taks, we also use an octave higher
        if(zone == Handpan.Zone.DOUM) {
          pitchOut += 12;
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
      post("I should be changing the scale..\n");
      break;

    default:
      error("Message '" + messagename + "' not understood\n");
      break;
  }
}

/*******************************************************************************
  Internal

  TODO: We should try to generalize this so that we can use the mk2 as well.
  (_Detect_ mk1 versus mk2? More generally, detect backend?)
*******************************************************************************/

function interpretScale(scale) {
  // Translate note to pitch
  //
  // We start with everything initialized to their lowest possible value;
  // for the Soniccounture chromatic mapping for mk1, this is an A at 57.
  var pitches = [];
  for(var i = 0; i < 12; i++) {
    pitches[(Handpan.Note.A + i) % 12] = 57 + i;
  }

  // Intepret the scale
  //
  // We always try the lowest possible pitch for each note that is still higher
  // then the previous. This places the scale as low as possible on the mapping,
  // but means that the scale is still monotonically increasing.
  // (Players can of course use a standard +12 effect if desired.)
  var interpreted = [];
  var offset      = 0;
  var prev        = -1;
  for(step in scale) {
    var note  = scale[step];
    var pitch = pitches[note];
    if (pitch + offset <= prev) {
      offset += 12;
    }
    interpreted[step] = pitch + offset;
    prev = interpreted[step];
  }

  return interpreted;
}
interpretScale.local = 1;
