/**
 * Instead of using <code>EffectFn</code>, we do the wrapping manually.
 * The reason is that Max's JavaScript environment is a little weird, and it
 * it treats <code>post</code> special; this works just fine:
 *
 * <code>
 * var fn = function(x) {
 *   post("This is some other function", x, "\n");
 * }
 *
 * var fnRenamed = fn;
 * fnRenamed(2);
 * </code>
 *
 * but this does not:
 *
 * <code>
 * var postRenamed = post;
 * postRenamed("bye\n");
 * </code>
 */
exports.post = function(msg) {
  return function() {
    if(typeof post === 'function')
      post(msg);
    else if(typeof console.log === 'function')
      console.log(msg);
  }
}

exports.setInlets = function(numInlets) {
  return function() {
    inlets = numInlets;
  }
}

exports.setOutlets = function(numOutlets) {
  return function() {
    outlets = numOutlets;
  }
}

exports.outletImpl = function(i, x) {
  outlet(i, x);
}

exports.numJsArgs = jsarguments.length;

exports.jsArgImpl = function(i) {
  if(i < jsarguments.length) {
    return jsarguments[i];
  } else {
    throw ( "jsArgImpl: Argument "
          + i
          + " out of range ("
          + arguments.length
          + ")\n"
          );
  }
}
