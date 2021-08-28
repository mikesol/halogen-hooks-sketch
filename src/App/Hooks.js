exports.unhedgeAtFFI = function(nothing) {
  return function(just) {
    return function(key) {
      return function(hedged) {
        return hedged.hasOwnProperty(key) ? just(hedged[key]) : nothing;
      }
    }
  }
}

exports.setHedgedAtFFI = function(key) {
  return function(val) {
    return function(hedged) {
      var o = Object.assign({}, hedged);
      o[key] = val;
      return o;
    }
  }
}

exports.unsafeSetViaVariant = function(vrep) {
  return function(row) {
    var o = Object.assign({}, row);
    o[vrep.type] = vrep.value;
    return o;
  }
}