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
