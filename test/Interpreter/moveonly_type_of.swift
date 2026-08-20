// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all)
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all)

// REQUIRES: executable_test

// `type(of:)` reads the dynamic type of its operand without consuming it, so it
// must neither destroy the operand early nor perturb its lifetime.

import StdlibUnittest

defer { runAllTests() }

var Tests = TestSuite("TypeOfNoncopyable")

protocol P: ~Copyable {}

struct NC: ~Copyable, P {
  var tracked = LifetimeTracked(0)
}

struct OtherNC: ~Copyable, P {
  var tracked = LifetimeTracked(0)
}

func borrow<T: ~Copyable>(_: borrowing T) {}

Tests.test("concrete operand is not consumed") {
  do {
    let v = NC()
    expectTrue(type(of: v) == NC.self)
    expectEqual(1, LifetimeTracked.instances)
    borrow(v)
  }
  expectEqual(0, LifetimeTracked.instances)
}

Tests.test("generic operand is not consumed") {
  func check<T: ~Copyable>(_ v: consuming T, is expected: T.Type) {
    expectTrue(type(of: v) == expected)
    // The operand is still live here, and only destroyed on return.
    expectEqual(1, LifetimeTracked.instances)
    borrow(v)
  }

  do {
    check(NC(), is: NC.self)
  }
  expectEqual(0, LifetimeTracked.instances)
}

Tests.test("existential operand reports its dynamic type") {
  func dynamicType(of box: consuming any P & ~Copyable) -> any (P & ~Copyable).Type {
    let t = type(of: box)
    expectEqual(1, LifetimeTracked.instances)
    borrow(box)
    return t
  }

  do {
    let t = dynamicType(of: NC())
    expectEqual(0, LifetimeTracked.instances)
    expectTrue(t == NC.self)
  }

  do {
    let t = dynamicType(of: OtherNC())
    expectEqual(0, LifetimeTracked.instances)
    expectTrue(t == OtherNC.self)
  }
}

Tests.test("existential metatype outlives the operand") {
  func consumeAfterQuery(_ box: consuming any P & ~Copyable) -> any (P & ~Copyable).Type {
    let t = type(of: box)
    // Consume the operand; the metatype must remain valid.
    let moved = consume box
    borrow(moved)
    return t
  }

  do {
    expectTrue(consumeAfterQuery(NC()) == NC.self)
    expectTrue(consumeAfterQuery(OtherNC()) == OtherNC.self)
  }
  expectEqual(0, LifetimeTracked.instances)
}

Tests.test("repeated queries do not consume") {
  func check<T: ~Copyable>(_ v: consuming T, is expected: T.Type) {
    for _ in 0 ..< 3 {
      expectTrue(type(of: v) == expected)
    }
    expectEqual(1, LifetimeTracked.instances)
    borrow(v)
  }

  do {
    check(NC(), is: NC.self)
  }
  expectEqual(0, LifetimeTracked.instances)
}
