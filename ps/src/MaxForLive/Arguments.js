exports.getArgImpl = function(arguments, i) {
  if(i < arguments.length) {
    return arguments[i];
  } else {
    throw ( "Argument "
          + i
          + " out of range ("
          + arguments.length
          + ")\n"
          );
  }
}
