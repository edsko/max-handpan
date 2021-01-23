/**
 * Functions that max expects to be present when the file is loaded
 */

/**
 * Registered message handlers
 *
 * @see anything
 */
var messageHandlers = {};

/**
 * Respond to messages
 *
 * Max expects <code>anything</code> to be present when the JavaScript is
 * loaded. We <i>can</i> override it later, but since it must be present, we
 * might as well make use of it. On the PureScript side, we will update
 * <code>messageHandlers</code>, and we will read it here.
 */
function anything() {
  post("This is anything, JavaScript side", messagename, "\n");
  if(typeof(messageHandlers[messagename]) === 'function') {
    messageHandlers[messagename](arguments);
  } else {
    throw ("anything: Unexpected message " + messagename + "\n");    
  }
}
