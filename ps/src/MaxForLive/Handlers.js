exports.setHandlersImpl = function(handlers) {
  post("going to set handlers\n");
  anything = function() {
    var i, h;
    for(i in handlers) {
      h = handlers[i];
      if(h.message === messagename) {
        h.handler(arguments);
        return;
      }
    }
    throw ("setHandlersImpl: Unexpected message " + messagename + "\n");
  }
}
