// RUN: %target-swift-frontend -emit-sil %s -swift-version 6 -target %target-future-triple -enable-experimental-feature NoncopyableCasting -o /dev/null

// REQUIRES: concurrency
// REQUIRES: swift_feature_NoncopyableCasting

// RegionAnalysis reads the destination of a `checked_cast_addr_br` as operand 1.
// A `test_only` cast has no destination, so its operand list is just the source
// plus any type-dependent operands -- reading operand 1 walks off the end.
//
// Nothing here needs to be diagnosed; the point is that requesting region
// analysis over an `is` on a noncopyable existential does not crash. Strict
// concurrency is what makes the pass run at all.

protocol P: ~Copyable {}

struct NC: ~Copyable, P {
  var t: Int
}

struct Other: ~Copyable, P {}

func isNC(_ box: borrowing any P & ~Copyable) -> Bool {
  box is NC
}

// Two tests on the same subject, so the translator sees the instruction more
// than once in a single function.
func classify(_ box: borrowing any P & ~Copyable) -> Int {
  if box is NC { return 1 }
  if box is Other { return 2 }
  return 0
}

func isNCInPattern(_ box: borrowing any P & ~Copyable) -> Bool {
  switch box {
  case is NC: return true
  default: return false
  }
}

// An opened archetype target gives the instruction type-dependent operands, so
// the operand list is longer than one even without a destination.
func isOpened(_ box: borrowing any P & ~Copyable, _ q: borrowing any P & ~Copyable) -> Bool {
  box is NC
}

// Inside an actor, so the isolation machinery has something to track.
actor A {
  func check(_ box: borrowing any P & ~Copyable) -> Bool {
    box is NC
  }
}
