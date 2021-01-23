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
