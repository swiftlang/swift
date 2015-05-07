// RUN: %target-swift-frontend %s -O -emit-sil

// Make sure we are not crashing on this one.

var a : [String] = ["foo"]

_preconditionFailure("unreachable")
for i in 0...a.count() {
  let x = 0
}

