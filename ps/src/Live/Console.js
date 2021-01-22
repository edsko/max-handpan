exports.post = function(msg) {
  return function() {
    post(msg);
  }
}
