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
