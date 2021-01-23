exports.setHandlersImpl = function(handlers) {
  anything = function() {
    var i, h;
    for(i in handlers) {
      h = handlers[i];
      if(h.message === messagename) {
        h.handler(arguments);
        return;
      }
    }
    throw ("Unexpected message " + messagename + "\n");
  }
}

exports.getArgIntImpl = function(arguments, i) {
  if(i >= arguments.length) {
    throw ( "Argument "
          + i
          + " out of range ("
          + arguments.length
          + ")\n"
          );
  } else if(typeof(arguments[i]) !== 'number') {
    throw ( "Argument "
          + i
          + " has unexpected type "
          + typeof(arguments[i])
          + " (expected Int)\n"
          );
  } else {
    return arguments[i];
  }
}
