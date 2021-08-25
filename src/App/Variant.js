exports.unsafeSetViaVariant = function(vrep) {
  return function(row) {
    var o = Object.assign({}, row);
    o[vrep.type] = vrep.value;
    return o;
  }
}