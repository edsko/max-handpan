exports.registerHandlerImpl = function(message, handler) {
  messageHandlers[message] = handler;
}
