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
