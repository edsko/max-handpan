exports.setHandlersImpl = function(handlers) {
  return function() {
    anything = function() {
      var i, h;
      for(i in handlers) {
        h = handlers[i];
        if(h.message === messagename) {
          h.handler();
          return;
        }
      }
      error("Unhandled message", messagename, "\n");
    }
  }
}
