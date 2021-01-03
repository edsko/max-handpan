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

inlets  = 2;
outlets = 1;

/*******************************************************************************
  Global state
*******************************************************************************/

var comp  = 0;
var boost = 0;

/*******************************************************************************
  Handle M4L messages
*******************************************************************************/

function msg_float(f) {
  var x, y;

  switch(inlet) {
    case 0:
      comp = f;
      break;
    case 1:
      boost = f;
      break;
    default:
      error("Message on unexpected inlet");
      break;
  }

  for(x = 0; x <= 127; x++) {
    if(x == 0) {
      // Velocity 0 (note-off) we always leave at 0.
      y = 0;
    } else {
      if(comp == 0) {
        y = x;
      } else if(comp > 0) {
        y = fn_compress(comp, x);
      } else {
        y = fn_expand(Math.abs(comp), x);
      }

      if(boost == 0) {
        // Nothing to do
      } else if (boost < 0) {
        y = fn_rescale(1 + boost, y);
      } else {
        y = fn_rescale(1 - boost, y) + (boost * 127);
      }
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
    return pow(10.0, 2 * f);
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
