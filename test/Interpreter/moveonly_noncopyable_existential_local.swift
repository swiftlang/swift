// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all)
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all)

// REQUIRES: executable_test

// The move-only address checker treats a noncopyable existential as a single
// opaque leaf of the type tree. Check that the liveness it computes from that is
// actually right: each payload must be destroyed exactly once, at the right
// point, whether it is consumed, borrowed, reassigned, or left to go out of
// scope.

import StdlibUnittest

defer { runAllTests() }

var Tests = TestSuite("NoncopyableExistentialLocal")

protocol P: ~Copyable {}

// Payloads with differing leaf counts, since the leaf count of the payload is
// what used to be mistaken for the leaf count of the existential.
struct TwoLeaves: ~Copyable, P {
  var tracked = LifetimeTracked(0)
  deinit {}
}

struct ThreeFields: ~Copyable, P {
  var a = LifetimeTracked(0)
  var b = LifetimeTracked(0)
  var c = LifetimeTracked(0)
}

struct DeinitOnly: ~Copyable, P {
  deinit { DeinitOnly.deinitCount += 1 }
  static var deinitCount = 0
}

func consume<T: ~Copyable>(_: consuming T) {}
func borrow<T: ~Copyable>(_: borrowing T) {}

Tests.test("let binding destroyed once at scope exit") {
  do {
    let box: any P & ~Copyable = TwoLeaves()
    borrow(box)
    expectEqual(1, LifetimeTracked.instances)
  }
  expectEqual(0, LifetimeTracked.instances)
}

Tests.test("multi-leaf payload destroyed once") {
  do {
    let box: any P & ~Copyable = ThreeFields()
    borrow(box)
    expectEqual(3, LifetimeTracked.instances)
  }
  expectEqual(0, LifetimeTracked.instances)
}

Tests.test("consuming the binding destroys the payload once") {
  do {
    let box: any P & ~Copyable = ThreeFields()
    expectEqual(3, LifetimeTracked.instances)
    consume(box)
    // `consume` took ownership and dropped it.
    expectEqual(0, LifetimeTracked.instances)
  }
  expectEqual(0, LifetimeTracked.instances)
}

Tests.test("reassignment destroys the old payload") {
  do {
    var box: any P & ~Copyable = ThreeFields()
    expectEqual(3, LifetimeTracked.instances)
    box = TwoLeaves()
    // Old payload's three trackers are gone, new payload has one.
    expectEqual(1, LifetimeTracked.instances)
    borrow(box)
    consume(box)
    expectEqual(0, LifetimeTracked.instances)
  }
  expectEqual(0, LifetimeTracked.instances)
}

Tests.test("deinit runs exactly once") {
  DeinitOnly.deinitCount = 0
  do {
    let box: any P & ~Copyable = DeinitOnly()
    borrow(box)
    expectEqual(0, DeinitOnly.deinitCount)
  }
  expectEqual(1, DeinitOnly.deinitCount)

  DeinitOnly.deinitCount = 0
  do {
    var box: any P & ~Copyable = DeinitOnly()
    box = DeinitOnly() // destroys the first
    expectEqual(1, DeinitOnly.deinitCount)
    consume(box)
    expectEqual(2, DeinitOnly.deinitCount)
  }
  expectEqual(2, DeinitOnly.deinitCount)
}

// The existential sitting between two other fields: an over-wide leaf range
// would have overlapped `after`, so check the neighbours survive correctly.
struct Wrapper: ~Copyable {
  var before = LifetimeTracked(0)
  var box: any P & ~Copyable
  var after = LifetimeTracked(0)
}

Tests.test("existential nested in an aggregate") {
  do {
    let w = Wrapper(box: ThreeFields())
    borrow(w)
    // 1 before + 3 payload + 1 after
    expectEqual(5, LifetimeTracked.instances)
  }
  expectEqual(0, LifetimeTracked.instances)
}

Tests.test("reassigning the nested existential field") {
  do {
    var w = Wrapper(box: ThreeFields())
    expectEqual(5, LifetimeTracked.instances)
    w.box = TwoLeaves()
    // 1 before + 1 payload + 1 after
    expectEqual(3, LifetimeTracked.instances)
    consume(w)
    expectEqual(0, LifetimeTracked.instances)
  }
  expectEqual(0, LifetimeTracked.instances)
}

Tests.test("conditional consume destroys once on both paths") {
  func run(_ takeIt: Bool) {
    let box: any P & ~Copyable = ThreeFields()
    if takeIt {
      consume(box)
    } else {
      borrow(box)
    }
  }

  run(true)
  expectEqual(0, LifetimeTracked.instances)
  run(false)
  expectEqual(0, LifetimeTracked.instances)
}
