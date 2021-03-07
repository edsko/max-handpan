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
  if(messageHandlers[inlet] !== undefined && typeof(messageHandlers[inlet][messagename]) === 'function') {
    messageHandlers[inlet][messagename](arguments);
  } else {
    throw ("anything: Unexpected message " + messagename + " on inlet " + inlet + "\n");
  }
}
// Generated by purs bundle 0.13.8
var PS = {};
(function(exports) {
  "use strict";

  var refEq = function (r1) {
    return function (r2) {
      return r1 === r2;
    };
  };

  exports.eqBooleanImpl = refEq;
})(PS["Data.Eq"] = PS["Data.Eq"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Eq"] = $PS["Data.Eq"] || {};
  var exports = $PS["Data.Eq"];
  var $foreign = $PS["Data.Eq"];
  var Eq = function (eq) {
      this.eq = eq;
  };                                       
  var eqBoolean = new Eq($foreign.eqBooleanImpl);
  var eq = function (dict) {
      return dict.eq;
  };
  var notEq = function (dictEq) {
      return function (x) {
          return function (y) {
              return eq(eqBoolean)(eq(dictEq)(x)(y))(false);
          };
      };
  };
  exports["Eq"] = Eq;
  exports["eq"] = eq;
  exports["notEq"] = notEq;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Function"] = $PS["Data.Function"] || {};
  var exports = $PS["Data.Function"];
  var $$const = function (a) {
      return function (v) {
          return a;
      };
  };
  exports["const"] = $$const;
})(PS);
(function(exports) {
  "use strict";

  exports.runFn2 = function (fn) {
    return function (a) {
      return function (b) {
        return fn(a, b);
      };
    };
  };
})(PS["Data.Function.Uncurried"] = PS["Data.Function.Uncurried"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Function.Uncurried"] = $PS["Data.Function.Uncurried"] || {};
  var exports = $PS["Data.Function.Uncurried"];
  var $foreign = $PS["Data.Function.Uncurried"];
  exports["runFn2"] = $foreign.runFn2;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Maybe"] = $PS["Data.Maybe"] || {};
  var exports = $PS["Data.Maybe"];
  var Data_Eq = $PS["Data.Eq"];                    
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  var eqMaybe = function (dictEq) {
      return new Data_Eq.Eq(function (x) {
          return function (y) {
              if (x instanceof Nothing && y instanceof Nothing) {
                  return true;
              };
              if (x instanceof Just && y instanceof Just) {
                  return Data_Eq.eq(dictEq)(x.value0)(y.value0);
              };
              return false;
          };
      });
  };
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
  exports["eqMaybe"] = eqMaybe;
})(PS);
(function(exports) {
  "use strict";

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Unit"] = $PS["Data.Unit"] || {};
  var exports = $PS["Data.Unit"];
  var $foreign = $PS["Data.Unit"];
  exports["unit"] = $foreign.unit;
})(PS);
(function(exports) {
  "use strict";

  exports.new = function (val) {
    return function () {
      return { value: val };
    };
  };

  exports.read = function (ref) {
    return function () {
      return ref.value;
    };
  };

  exports.write = function (val) {
    return function (ref) {
      return function () {
        ref.value = val;
        return {};
      };
    };
  };
})(PS["Effect.Ref"] = PS["Effect.Ref"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Effect.Ref"] = $PS["Effect.Ref"] || {};
  var exports = $PS["Effect.Ref"];
  var $foreign = $PS["Effect.Ref"];
  exports["new"] = $foreign["new"];
  exports["read"] = $foreign.read;
  exports["write"] = $foreign.write;
})(PS);
(function(exports) {
  "use strict";

  exports.mkEffectFn1 = function mkEffectFn1(fn) {
    return function(x) {
      return fn(x)();
    };
  };

  exports.runEffectFn4 = function runEffectFn4(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(d) {
            return function() {
              return fn(a, b, c, d);
            };
          };
        };
      };
    };
  };
})(PS["Effect.Uncurried"] = PS["Effect.Uncurried"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Effect.Uncurried"] = $PS["Effect.Uncurried"] || {};
  var exports = $PS["Effect.Uncurried"];
  var $foreign = $PS["Effect.Uncurried"];
  exports["mkEffectFn1"] = $foreign.mkEffectFn1;
  exports["runEffectFn4"] = $foreign.runEffectFn4;
})(PS);
(function(exports) {
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
})(PS["MaxForLive.Arguments"] = PS["MaxForLive.Arguments"] || {});
(function(exports) {
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
})(PS["MaxForLive.Conversions"] = PS["MaxForLive.Conversions"] || {});
(function(exports) {
  "use strict";

  // module Unsafe.Coerce

  exports.unsafeCoerce = function (x) {
    return x;
  };
})(PS["Unsafe.Coerce"] = PS["Unsafe.Coerce"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Unsafe.Coerce"] = $PS["Unsafe.Coerce"] || {};
  var exports = $PS["Unsafe.Coerce"];
  var $foreign = $PS["Unsafe.Coerce"];
  exports["unsafeCoerce"] = $foreign.unsafeCoerce;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["MaxForLive.Conversions"] = $PS["MaxForLive.Conversions"] || {};
  var exports = $PS["MaxForLive.Conversions"];
  var $foreign = $PS["MaxForLive.Conversions"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];                
  var ToMax = function (toMax) {
      this.toMax = toMax;
  };
  var FromMax = function (fromMax) {
      this.fromMax = fromMax;
  };
  var toMaxString = new ToMax(Unsafe_Coerce.unsafeCoerce);                              
  var toMaxInt = new ToMax(Unsafe_Coerce.unsafeCoerce);
  var toMax = function (dict) {
      return dict.toMax;
  };
  var toMaxBoolean = new ToMax((function () {
      var conv = function (v) {
          if (!v) {
              return 0;
          };
          if (v) {
              return 1;
          };
          throw new Error("Failed pattern match at MaxForLive.Conversions (line 64, column 7 - line 64, column 29): " + [ v.constructor.name ]);
      };
      var $7 = toMax(toMaxInt);
      return function ($8) {
          return $7(conv($8));
      };
  })());                                                      
  var fromMaxInt = new FromMax($foreign.fromMaxIntImpl);
  var fromMax = function (dict) {
      return dict.fromMax;
  };
  var fromMaxBoolean = new FromMax((function () {
      var conv = function (v) {
          if (v === 0) {
              return false;
          };
          return true;
      };
      var $9 = fromMax(fromMaxInt);
      return function ($10) {
          return conv($9($10));
      };
  })());
  exports["FromMax"] = FromMax;
  exports["fromMax"] = fromMax;
  exports["ToMax"] = ToMax;
  exports["toMax"] = toMax;
  exports["fromMaxBoolean"] = fromMaxBoolean;
  exports["toMaxBoolean"] = toMaxBoolean;
  exports["toMaxString"] = toMaxString;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["MaxForLive.Arguments"] = $PS["MaxForLive.Arguments"] || {};
  var exports = $PS["MaxForLive.Arguments"];
  var $foreign = $PS["MaxForLive.Arguments"];
  var Data_Function_Uncurried = $PS["Data.Function.Uncurried"];
  var MaxForLive_Conversions = $PS["MaxForLive.Conversions"];                
  var getArg = function (dictFromMax) {
      return function (xs) {
          var $1 = MaxForLive_Conversions.fromMax(dictFromMax);
          var $2 = Data_Function_Uncurried.runFn2($foreign.getArgImpl)(xs);
          return function ($3) {
              return $1($2($3));
          };
      };
  };
  exports["getArg"] = getArg;
})(PS);
(function(exports) {
   

  exports.setInlets = function(numInlets) {
    return function() {
      inlets = numInlets;
    }
  }

  exports.setOutlets = function(numOutlets) {
    return function() {
      outlets = numOutlets;
    }
  }

  exports.outletImpl = function(i, x) {
    outlet(i, x);
  }

  exports.setInletAssist = function(inlet) {
    return function(assist) {
      return function() {
        setinletassist(inlet, assist);
      }
    }
  }

  exports.setOutletAssist = function(inlet) {
    return function(assist) {
      return function() {
        setoutletassist(inlet, assist);
      }
    }
  }
})(PS["MaxForLive.Global"] = PS["MaxForLive.Global"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["MaxForLive.Global"] = $PS["MaxForLive.Global"] || {};
  var exports = $PS["MaxForLive.Global"];
  var $foreign = $PS["MaxForLive.Global"];
  var MaxForLive_Conversions = $PS["MaxForLive.Conversions"];
  var outlet = function (dictToMax) {
      return function (i) {
          return function (x) {
              return function () {
                  return $foreign.outletImpl(i, MaxForLive_Conversions.toMax(dictToMax)(x));
              };
          };
      };
  };
  exports["outlet"] = outlet;
  exports["setInlets"] = $foreign.setInlets;
  exports["setOutlets"] = $foreign.setOutlets;
  exports["setInletAssist"] = $foreign.setInletAssist;
  exports["setOutletAssist"] = $foreign.setOutletAssist;
})(PS);
(function(exports) {
  exports.setHandlerImpl = function(inlet, message, handler) {
    if(messageHandlers[inlet] === undefined) {
      messageHandlers[inlet] = {};
    }

    messageHandlers[inlet][message] = handler;
  }
})(PS["MaxForLive.Handlers"] = PS["MaxForLive.Handlers"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["MaxForLive.Handlers"] = $PS["MaxForLive.Handlers"] || {};
  var exports = $PS["MaxForLive.Handlers"];
  var $foreign = $PS["MaxForLive.Handlers"];
  var Data_Function = $PS["Data.Function"];
  var Effect_Uncurried = $PS["Effect.Uncurried"];
  var MaxForLive_Arguments = $PS["MaxForLive.Arguments"];                
  var InvokeHandler = function (invokeHandler) {
      this.invokeHandler = invokeHandler;
  };
  var invokeNoArgs = new InvokeHandler(function (_i) {
      return Data_Function["const"];
  });
  var invokeHandler = function (dict) {
      return dict.invokeHandler;
  };
  var invokeWithArg = function (dictFromMax) {
      return function (dictInvokeHandler) {
          return new InvokeHandler(function (i) {
              return function (f) {
                  return function (xs) {
                      return invokeHandler(dictInvokeHandler)(i + 1 | 0)(f(MaxForLive_Arguments.getArg(dictFromMax)(xs)(i)))(xs);
                  };
              };
          });
      };
  };
  var setHandler = function (dictInvokeHandler) {
      return function (v) {
          return function () {
              return $foreign.setHandlerImpl(v.inlet, v.msg, Effect_Uncurried.mkEffectFn1(invokeHandler(dictInvokeHandler)(0)(v.handler)));
          };
      };
  };
  exports["setHandler"] = setHandler;
  exports["invokeNoArgs"] = invokeNoArgs;
  exports["invokeWithArg"] = invokeWithArg;
})(PS);
(function(exports) {
  exports.withPath = function(path) {
    return function() {
      return new LiveAPI(null, path);
    }
  }

  exports.parentOfType = function(just, nothing, parentType, path) {
    var parentObj  = null;
    var parentPath = path;

    var i = 0;

    // We're a bit paranoid here; for justification, see
    // https://cycling74.com/forums/livemax-hang-live-at-100-cpu-when-saving-patch
    var loopProtection = 20;

    do {
      loopProtection--;
      parentPath = parentPath + " canonical_parent";
      parentObj  = new LiveAPI(null, parentPath);
    } while( (parentObj.type !== parentType)
          && (parentObj.id != 0)
          && loopProtection > 0
           );

    if(loopProtection == 0 || parentObj.id == 0) {
      return nothing;
    } else {
      return just(parentObj);
    }
  }

  exports.id = function(obj) {
    var id = obj.id;

    if(typeof(id) === 'string') {
      // For some reason, IDs are returned as strings 🤦‍♂️
      return parseInt(id);
    } else if(typeof(id) === 'number') {
      return id;
    } else {
      error("id: unexpected ID", id, "of type", typeof(id), "\n");
    }
  }

  exports.unquotedPath = function(obj) {
    return obj.unquotedpath;
  }

  exports.idFromMax = function(id) {
    if(typeof(id) === 'number') {
      return id;
    } else {
      error("idFromMax: unexpected ID", id, "of type", typeof(id), "\n");
    }
  }
})(PS["MaxForLive.LiveAPI"] = PS["MaxForLive.LiveAPI"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["MaxForLive.LiveAPI"] = $PS["MaxForLive.LiveAPI"] || {};
  var exports = $PS["MaxForLive.LiveAPI"];
  var $foreign = $PS["MaxForLive.LiveAPI"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Effect_Uncurried = $PS["Effect.Uncurried"];
  var MaxForLive_Conversions = $PS["MaxForLive.Conversions"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var toMaxPath = new MaxForLive_Conversions.ToMax(Unsafe_Coerce.unsafeCoerce);
  var thisDevice = "this_device";
  var sameId = function (v) {
      return function (v1) {
          return v === v1;
      };
  };
  var fromMaxId = new MaxForLive_Conversions.FromMax($foreign.idFromMax);
  var eqId = new Data_Eq.Eq(sameId);
  var deviceTrack = Effect_Uncurried.runEffectFn4($foreign.parentOfType)(Data_Maybe.Just.create)(Data_Maybe.Nothing.value)("Track");
  exports["thisDevice"] = thisDevice;
  exports["deviceTrack"] = deviceTrack;
  exports["toMaxPath"] = toMaxPath;
  exports["eqId"] = eqId;
  exports["fromMaxId"] = fromMaxId;
  exports["withPath"] = $foreign.withPath;
  exports["id"] = $foreign.id;
  exports["unquotedPath"] = $foreign.unquotedPath;
})(PS);
(function(exports) {
  exports.mkMaxMessage = function(msg, payload) {
    // If the payload is an array, insert the message at the front
    // This is the format that `route` and co expect
    if(Array.isArray(payload)) {
      return [msg].concat(payload);
    } else {
      return [msg, payload];
    }
  }
})(PS["MaxForLive.Message"] = PS["MaxForLive.Message"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["MaxForLive.Message"] = $PS["MaxForLive.Message"] || {};
  var exports = $PS["MaxForLive.Message"];
  var $foreign = $PS["MaxForLive.Message"];
  var MaxForLive_Conversions = $PS["MaxForLive.Conversions"];                
  var Message = (function () {
      function Message(value0) {
          this.value0 = value0;
      };
      Message.create = function (value0) {
          return new Message(value0);
      };
      return Message;
  })();
  var toMaxMessage = function (dictToMax) {
      return new MaxForLive_Conversions.ToMax(function (v) {
          return $foreign.mkMaxMessage(v.value0.messageName, MaxForLive_Conversions.toMax(dictToMax)(v.value0.messagePayload));
      });
  };
  exports["Message"] = Message;
  exports["toMaxMessage"] = toMaxMessage;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["TrackSelected.State"] = $PS["TrackSelected.State"] || {};
  var exports = $PS["TrackSelected.State"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Maybe = $PS["Data.Maybe"];
  var MaxForLive_LiveAPI = $PS["MaxForLive.LiveAPI"];                
  var isSelected = function (v) {
      return v.enabled && (Data_Eq.eq(Data_Maybe.eqMaybe(MaxForLive_LiveAPI.eqId))(v.parent)(v.selected) && v.preview);
  };
  var init = {
      ourId: Data_Maybe.Nothing.value,
      parent: Data_Maybe.Nothing.value,
      selected: Data_Maybe.Nothing.value,
      enabled: true,
      preview: true
  };
  exports["init"] = init;
  exports["isSelected"] = isSelected;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["TrackSelected"] = $PS["TrackSelected"] || {};
  var exports = $PS["TrackSelected"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect_Ref = $PS["Effect.Ref"];
  var MaxForLive_Conversions = $PS["MaxForLive.Conversions"];
  var MaxForLive_Global = $PS["MaxForLive.Global"];
  var MaxForLive_Handlers = $PS["MaxForLive.Handlers"];
  var MaxForLive_LiveAPI = $PS["MaxForLive.LiveAPI"];
  var MaxForLive_Message = $PS["MaxForLive.Message"];
  var TrackSelected_State = $PS["TrackSelected.State"];                
  var updateState = function (ref) {
      return function (f) {
          return function __do() {
              var oldState = Effect_Ref.read(ref)();
              var newState = f(oldState);
              Effect_Ref.write(newState)(ref)();
              var $5 = TrackSelected_State.isSelected(oldState) === TrackSelected_State.isSelected(newState);
              if ($5) {
                  return Data_Unit.unit;
              };
              return MaxForLive_Global.outlet(MaxForLive_Conversions.toMaxBoolean)(0)(TrackSelected_State.isSelected(newState))();
          };
      };
  };
  var toggleEnabled = function (ref) {
      return function (enabled) {
          return updateState(ref)(function (v) {
              return {
                  ourId: v.ourId,
                  parent: v.parent,
                  selected: v.selected,
                  enabled: enabled,
                  preview: v.preview
              };
          });
      };
  };
  var setSelectedId = function (ref) {
      return function (selected) {
          return updateState(ref)(function (v) {
              return {
                  ourId: v.ourId,
                  parent: v.parent,
                  selected: new Data_Maybe.Just(selected),
                  enabled: v.enabled,
                  preview: v.preview
              };
          });
      };
  };
  var previewDisable = function (ref) {
      return updateState(ref)(function (v) {
          return {
              ourId: v.ourId,
              parent: v.parent,
              selected: v.selected,
              enabled: v.enabled,
              preview: false
          };
      });
  };
  var init = function (ref) {
      return function __do() {
          var us = MaxForLive_LiveAPI.withPath(MaxForLive_LiveAPI.thisDevice)();
          var mOurTrack = MaxForLive_LiveAPI.deviceTrack(MaxForLive_LiveAPI.thisDevice)();
          if (mOurTrack instanceof Data_Maybe.Just) {
              updateState(ref)(function (st) {
                  return {
                      ourId: new Data_Maybe.Just(MaxForLive_LiveAPI.id(us)),
                      parent: new Data_Maybe.Just(MaxForLive_LiveAPI.id(mOurTrack.value0)),
                      selected: st.selected,
                      enabled: st.enabled,
                      preview: true
                  };
              })();
              MaxForLive_Global.outlet(MaxForLive_Conversions.toMaxString)(1)("Off")();
              return MaxForLive_Global.outlet(MaxForLive_Message.toMaxMessage(MaxForLive_LiveAPI.toMaxPath))(2)(new MaxForLive_Message.Message({
                  messageName: "path",
                  messagePayload: MaxForLive_LiveAPI.unquotedPath(us)
              }))();
          };
          if (mOurTrack instanceof Data_Maybe.Nothing) {
              MaxForLive_Global.outlet(MaxForLive_Conversions.toMaxString)(1)("Error")();
              return updateState(ref)(function (st) {
                  return {
                      ourId: Data_Maybe.Nothing.value,
                      parent: Data_Maybe.Nothing.value,
                      selected: st.selected,
                      enabled: st.enabled,
                      preview: true
                  };
              })();
          };
          throw new Error("Failed pattern match at TrackSelected (line 53, column 5 - line 72, column 11): " + [ mOurTrack.constructor.name ]);
      };
  };
  var previewEnable = function (ref) {
      return function __do() {
          var oldState = Effect_Ref.read(ref)();
          var $8 = !oldState.preview;
          if ($8) {
              return init(ref)();
          };
          return Data_Unit.unit;
      };
  };
  var setDeviceId = function (ref) {
      return function (deviceId) {
          return function __do() {
              var oldState = Effect_Ref.read(ref)();
              var $9 = Data_Eq.notEq(Data_Maybe.eqMaybe(MaxForLive_LiveAPI.eqId))(oldState.ourId)(new Data_Maybe.Just(deviceId));
              if ($9) {
                  return init(ref)();
              };
              return Data_Unit.unit;
          };
      };
  };
  var main = function __do() {
      MaxForLive_Global.setInlets(6)();
      MaxForLive_Global.setOutlets(3)();
      MaxForLive_Global.setInletAssist(0)("Bang to (re)initialise")();
      MaxForLive_Global.setInletAssist(1)("Device enabled/disabled (from live.thisdevice)")();
      MaxForLive_Global.setInletAssist(2)("Bang when preview state disabled")();
      MaxForLive_Global.setInletAssist(3)("Bang when preview state enabled")();
      MaxForLive_Global.setInletAssist(4)("ID of selected track")();
      MaxForLive_Global.setInletAssist(5)("ID of device at our path")();
      MaxForLive_Global.setOutletAssist(0)("'selected' or 'deselected'")();
      MaxForLive_Global.setOutletAssist(1)("Device path (on init and when device moved)")();
      var st = Effect_Ref["new"](TrackSelected_State.init)();
      MaxForLive_Handlers.setHandler(MaxForLive_Handlers.invokeNoArgs)({
          inlet: 0,
          msg: "bang",
          handler: init(st)
      })();
      MaxForLive_Handlers.setHandler(MaxForLive_Handlers.invokeWithArg(MaxForLive_Conversions.fromMaxBoolean)(MaxForLive_Handlers.invokeNoArgs))({
          inlet: 1,
          msg: "msg_int",
          handler: toggleEnabled(st)
      })();
      MaxForLive_Handlers.setHandler(MaxForLive_Handlers.invokeNoArgs)({
          inlet: 2,
          msg: "bang",
          handler: previewDisable(st)
      })();
      MaxForLive_Handlers.setHandler(MaxForLive_Handlers.invokeNoArgs)({
          inlet: 3,
          msg: "bang",
          handler: previewEnable(st)
      })();
      MaxForLive_Handlers.setHandler(MaxForLive_Handlers.invokeWithArg(MaxForLive_LiveAPI.fromMaxId)(MaxForLive_Handlers.invokeNoArgs))({
          inlet: 4,
          msg: "id",
          handler: setSelectedId(st)
      })();
      return MaxForLive_Handlers.setHandler(MaxForLive_Handlers.invokeWithArg(MaxForLive_LiveAPI.fromMaxId)(MaxForLive_Handlers.invokeNoArgs))({
          inlet: 5,
          msg: "id",
          handler: setDeviceId(st)
      })();
  };
  exports["main"] = main;
})(PS);
PS["TrackSelected"].main();
/* END */
