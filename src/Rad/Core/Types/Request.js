"use strict";

// module Rad.Core.Types.Request

exports.mkRequestExists = function(dict) {
  return function(fa) {
    return { rep: fa, dict: dict };
  }
};

exports.runRequestExists = function(f) {
  return function(fa) {
    return f(fa.dict)(fa.rep);
  }
};
