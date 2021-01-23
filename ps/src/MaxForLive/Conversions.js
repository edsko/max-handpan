exports.fromMaxIntImpl = function(x) {
  if(typeof(x) === 'number') {
    return x;
  } else {
    throw ( "Argument "
          + i
          + " has unexpected type "
          + typeof(x)
          + " (expected Int)\n"
          );
  }
}
