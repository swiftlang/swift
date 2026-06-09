// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature Lifetimes \
// RUN:   -disable-availability-checking -primary-file %s -o /dev/null -verify

// REQUIRES: swift_feature_Lifetimes

// This is a dummy struct. Only the interface matters for exclusivity checks.
// An example implementation is shown as comments only to motivate the interface.
struct InoutPair: ~Escapable, ~Copyable {
  //var x: Inout<Int>? = nil
  //var x: Inout<Int>? = nil

  @_lifetime(self: &x)
  mutating func insertX(_ x: inout Int) {
    //self.x = Inout(x)
  }

  @_lifetime(self: &y)
  mutating func insertY(_ y: inout Int) {
    //self.y = Inout(y)
  }
}

func takeFunction(_ inoutPair: consuming InoutPair) {}

struct Pair {
  var x: Int
  var y: Int

  mutating func testDistinctSubobjects() {
    var inoutPair = InoutPair()
    inoutPair.insertX(&x)
    inoutPair.insertY(&y)

    takeFunction(inoutPair) // OK: no overlapping access.
    // The access of both &x and &y are extended up to the last use of 'inoutPair'.
    // Exclusivity diagnostics determines that &x and &y are distinct sub-objects
    // so their simultaneous access does not overlap.
  }

  func takeMethod(_ pair: consuming InoutPair) {}

  // A 'modify' access of 'self' extends over the lifetime of 'inoutPair'. But, since the call to 'self.takeMethod()'
  // simply copies 'self' before calling 'takeMethod()', it is an immutable read scope which does not conflict with the
  // overlapping 'modify'.
  mutating func testInoutOverlapsWithMethod() {
    var inoutPair = InoutPair()
    inoutPair.insertX(&x)
    inoutPair.insertY(&y)

    self.takeMethod(inoutPair) // OK: treated like a nested read.
  }
}

// Verify that reading the array count does not interfere with a mutable span.
func testMutableSpan(array: inout [Int]) {
  var ms = array.mutableSpan
  _ = array.count // OK
  ms[0] = 3
}
