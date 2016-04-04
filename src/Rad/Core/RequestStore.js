"use strict";

// module Rad.Core.RequestStore

exports.mkBlockedRequests = function(dict) {
  return function(fa) {
    return { rep: fa, dict: dict };
  }
};

exports.runBlockedRequests = function(f) {
  return function(fa) {
    return f(fa.dict)(fa.rep);
  }
};

exports.applyBlockedRequests = function(flags) {
  return function(u) {
    return function(fa) {
      return fa.dict.fetch(flags)(u)(fa.rep)
    }
  }
}
