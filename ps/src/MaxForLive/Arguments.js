exports.getArgImpl = function(arguments, i) {
  if(i < arguments.length) {
    return arguments[i];
  } else {
    throw ( "getArgImpl: Argument "
          + i
          + " out of range ("
          + arguments.length
          + ")\n"
          );
  }
}
