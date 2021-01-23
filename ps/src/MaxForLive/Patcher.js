exports.patcher = patcher;
exports.box     = box;

exports.filepath = function(p) {
  return p.filepath;
}

exports.newDefaultImpl = function(patcher, args) {
  return patcher.newdefault.apply(patcher, args);
}

exports.setAttr = function(obj, attrs) {
  var i, attr, value;

  for(i in attrs) {
    attr  = attrs[i].attr;
    value = attrs[i].value;
    obj.setattr(attr, value);
  }
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
