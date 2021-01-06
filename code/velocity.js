/**
 * Max Handpan
 *
 * @description Velocity compression/expansion curve
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2021
 * @see {@link https://github.com/edsko/max-handpan}
 * @license BSD-3-Clause
 */

/*******************************************************************************
  Setup device
*******************************************************************************/

inlets  = 4;
outlets = 1;

setinletassist(0, "Drive");
setinletassist(1, "Compand");
setinletassist(2, "Min");
setinletassist(3, "Max");

/*******************************************************************************
  Global state
*******************************************************************************/

var drive = 0;
var comp  = 0;
var min   = 0;
var max   = 127;

/*******************************************************************************
  Handle M4L messages
*******************************************************************************/

function msg_float(f) {
  var x, y;

  switch(inlet) {
    case 0:
      drive = f;
      break;
    case 1:
      comp = f;
      break;
    case 2:
      min = f;
      break;
    case 3:
      max = f;
      break;
    default:
      error("Message on unexpected inlet");
      break;
  }

  var rangeMin, rangeMax;

  if(max >= min) {
    rangeMin = min;
    rangeMax = max;
  } else {
    // If user sets min to be higher the max, just flip their meaning
    rangeMin = max;
    rangeMax = min;
  }

  for(x = 0; x <= 127; x++) {
    if(x == 0) {
      // Velocity 0 (note-off) we always leave at 0.
      y = 0;
    } else {
      // Start assuming no transformations are applied
      y = x;

      // Apply drive
      var mu_drive = fn_mu(drive);
      if(drive == 0) {
        // Nothing to do
      } else if(drive > 0) {
        // Map to the positive part of the compression curve
        with (Math) {
          y = log(1 + mu_drive * (y / 127)) / log(1 + mu_drive) * 127;
        }
      } else {
        // Map to the positive part of the expansion curve
        with (Math) {
          y = (1.0/mu_drive) * (pow(1.0 + mu_drive, x/127) - 1.0) * 127;
        }
      }

      // Apply compansion
      if(comp == 0) {
        // Nothing to do
      } else if(comp > 0) {
        y = fn_compress(comp, y);
      } else {
        y = fn_expand(comp, y);
      }

      // Apply min and max range
      y = rangeMin + (y / 127) * (rangeMax - rangeMin);
    }

    outlet(0, [x, y]);
  }
}

/*******************************************************************************
  Functions for boosting the signal
*******************************************************************************/

function fn_rescale(f, x) {
  return (x / 127) * f * 127;
}

/*******************************************************************************
  Direct translation of the math functions described in
  {@link http://edsko.net/2021/01/03/velocity-curve/}
*******************************************************************************/

function fn_in(x) {
  return -1 + (x / 127) * 2;
}

function fn_out(y) {
  return (y + 1) / 2 * 127;
}

function fn_mu(f) {
  with (Math) {
    return pow(10.0, 2 * abs(f));
  }
}

/**
 * Sign of the argument
 *
 * Apparently this is not available in M4L's JavaScript runtime.
 */
function fn_sign(x) {
  if(x == 0) {
    return 0;
  } else if(x > 0) {
    return 1;
  } else {
    return -1;
  }
}

function fn_compress(f, x) {
  var mu_f = fn_mu(f);
  var in_x = fn_in(x);

  with (Math) {
    return fn_out(
        fn_sign(in_x)
      * log(1 + mu_f * abs(in_x))
      / log(1 + mu_f)
      );
  }
}

function fn_expand(f, x) {
  var mu_f = fn_mu(f);
  var in_x = fn_in(x);

  with (Math) {
    return fn_out(
        fn_sign(in_x)
      * (1.0/mu_f)
      * (pow(1.0 + mu_f, abs(in_x)) - 1.0)
      );
  }
}
