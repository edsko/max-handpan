/**
 * @description Velocity compression/expansion curve
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2021
 * @license BSD-3-Clause
 */

/*******************************************************************************
  Setup device
*******************************************************************************/

inlets  = 1;
outlets = 1;

/*******************************************************************************
  Handle M4L messages
*******************************************************************************/

function msg_float(f) {
  var x, y;

  for(x = 0; x <= 127; x++) {
    if(f == 0) {
      y = x;
    } else if(f > 0) {
      y = fn_compress(f, x);
    } else {
      y = fn_expand(Math.abs(f), x);
    }

    outlet(0, [x, y]);
  }
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
