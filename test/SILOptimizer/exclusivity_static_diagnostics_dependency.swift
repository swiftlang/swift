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

func take(_ inoutPair: consuming InoutPair) {}

struct Pair {
  var x: Int
  var y: Int

  mutating func run() {
    var inoutPair = InoutPair()
    inoutPair.insertX(&x)
    inoutPair.insertY(&y)

    take(inoutPair) // OK: no overlapping access.
    // The access of both &x and &y are extended up to the last use of 'inoutPair'.
    // Exclusivity diagnostics determines that &x and &y are distinct sub-objects
    // so their simultaneous access does not overlap.
  }
}
