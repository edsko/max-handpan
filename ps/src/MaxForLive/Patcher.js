exports.patcher = patcher;
exports.box     = box;

exports.filepath = function(p) {
  return p.filepath;
}

exports.newDefaultImpl = function(patcher, args) {
  return patcher.newdefault.apply(patcher, args);
}

exports.mkNewDefaultArgsToggle = function(left, top) {
  return [left, top, "toggle"];
}

exports.remove = function(patcher) {
  return function(obj) {
    return function() {
      patcher.remove(obj);
    }
  }
}

exports.connect = function(patcher) {
  return function(args) {
    return function() {
      patcher.connect(args.fromObject, args.inlet, args.toObject, args.outlet);
    }
  }
}
