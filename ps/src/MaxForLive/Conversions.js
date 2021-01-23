exports.fromMaxIntImpl = function(x) {
  if(typeof(x) === 'number') {
    return x;
  } else {
    throw ( "fromMaxIntImpl: Argument "
          + x
          + " has unexpected type "
          + typeof(x)
          + " (expected Int)\n"
          );
  }
}

exports.fromMaxStringImpl = function(x) {
  if(typeof(x) === 'string') {
    return x;
  } else {
    throw ( "fromMaxStringImpl: Argument "
          + x
          + " has unexpected type "
          + typeof(x)
          + " (expected String)\n"
          );
  }
}
