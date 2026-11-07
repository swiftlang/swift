// RUN: %target-run-simple-swift
// RUN: %target-run-simple-swift(-Xfrontend -enable-lifetime-resolution)

// REQUIRES: executable_test

// A derived class initializer that fails cleans up `self` according to how far it got: before `super.init`,
// its initialized stored properties are destroyed and `self` is deallocated without running a deinit; once
// `super.init` fails, `self` was already released; after `super.init`, `self` is released as usual.

import StdlibUnittest

var DerivedInitTestSuite = TestSuite("DerivedInit")

struct E: Error {}

var baseDeinitCalled = 0
var derivedDeinitCalled = 0

func resetDeinitCounts() {
  baseDeinitCalled = 0
  derivedDeinitCalled = 0
}

class Base {
  var b: LifetimeTracked

  init() {
    b = LifetimeTracked(0)
  }

  init(throwing: Bool) throws {
    if throwing { throw E() }
    b = LifetimeTracked(0)
  }

  init?(failing: Bool) {
    if failing { return nil }
    b = LifetimeTracked(0)
  }

  deinit {
    baseDeinitCalled += 1
  }
}

enum Stage {
  case beforeAnyField, beforeSecondField, beforeSuperInit, superInit, afterSuperInit, never
}

class Derived: Base {
  var x: LifetimeTracked
  var y: LifetimeTracked

  init(throwingAt stage: Stage) throws {
    if stage == .beforeAnyField { throw E() }
    x = LifetimeTracked(1)
    if stage == .beforeSecondField { throw E() }
    y = LifetimeTracked(2)
    if stage == .beforeSuperInit { throw E() }
    try super.init(throwing: stage == .superInit)
    if stage == .afterSuperInit { throw E() }
  }

  init?(failingAt stage: Stage) {
    if stage == .beforeAnyField { return nil }
    x = LifetimeTracked(1)
    if stage == .beforeSecondField { return nil }
    y = LifetimeTracked(2)
    if stage == .beforeSuperInit { return nil }
    super.init(failing: stage == .superInit)
    if stage == .afterSuperInit { return nil }
  }

  deinit {
    derivedDeinitCalled += 1
  }
}

DerivedInitTestSuite.test("Throwing") {
  for stage in [Stage.beforeAnyField, .beforeSecondField, .beforeSuperInit, .superInit] {
    resetDeinitCounts()
    expectNil(try? Derived(throwingAt: stage))
    expectEqual(0, baseDeinitCalled)
    expectEqual(0, derivedDeinitCalled)
  }

  resetDeinitCounts()
  expectNil(try? Derived(throwingAt: .afterSuperInit))
  expectEqual(1, baseDeinitCalled)
  expectEqual(1, derivedDeinitCalled)

  resetDeinitCounts()
  expectNotNil(try? Derived(throwingAt: .never))
  expectEqual(1, baseDeinitCalled)
  expectEqual(1, derivedDeinitCalled)
}

DerivedInitTestSuite.test("Failable") {
  for stage in [Stage.beforeAnyField, .beforeSecondField, .beforeSuperInit, .superInit] {
    resetDeinitCounts()
    expectNil(Derived(failingAt: stage))
    expectEqual(0, baseDeinitCalled)
    expectEqual(0, derivedDeinitCalled)
  }

  resetDeinitCounts()
  expectNil(Derived(failingAt: .afterSuperInit))
  expectEqual(1, baseDeinitCalled)
  expectEqual(1, derivedDeinitCalled)

  resetDeinitCounts()
  expectNotNil(Derived(failingAt: .never))
  expectEqual(1, baseDeinitCalled)
  expectEqual(1, derivedDeinitCalled)
}

// Without stored properties of its own, only whether `super.init` was called and succeeded tells the
// failure paths apart.
class DerivedEmpty: Base {
  init(throwingAt stage: Stage) throws {
    if stage == .beforeSuperInit { throw E() }
    try super.init(throwing: stage == .superInit)
    if stage == .afterSuperInit { throw E() }
  }

  init?(failingAt stage: Stage) {
    if stage == .beforeSuperInit { return nil }
    super.init(failing: stage == .superInit)
    if stage == .afterSuperInit { return nil }
  }

  deinit {
    derivedDeinitCalled += 1
  }
}

DerivedInitTestSuite.test("ThrowingEmpty") {
  for stage in [Stage.beforeSuperInit, .superInit] {
    resetDeinitCounts()
    expectNil(try? DerivedEmpty(throwingAt: stage))
    expectEqual(0, baseDeinitCalled)
    expectEqual(0, derivedDeinitCalled)
  }

  resetDeinitCounts()
  expectNil(try? DerivedEmpty(throwingAt: .afterSuperInit))
  expectEqual(1, baseDeinitCalled)
  expectEqual(1, derivedDeinitCalled)
}

DerivedInitTestSuite.test("FailableEmpty") {
  for stage in [Stage.beforeSuperInit, .superInit] {
    resetDeinitCounts()
    expectNil(DerivedEmpty(failingAt: stage))
    expectEqual(0, baseDeinitCalled)
    expectEqual(0, derivedDeinitCalled)
  }

  resetDeinitCounts()
  expectNil(DerivedEmpty(failingAt: .afterSuperInit))
  expectEqual(1, baseDeinitCalled)
  expectEqual(1, derivedDeinitCalled)
}

// A use of `self` after `super.init` threw is rejected, but `catch` may still throw or reinitialize nothing.
class CatchRethrow: Base {
  var x: LifetimeTracked

  override init(throwing: Bool) throws {
    x = LifetimeTracked(1)
    do {
      try super.init(throwing: throwing)
    } catch {
      throw E()
    }
  }

  deinit {
    derivedDeinitCalled += 1
  }
}

DerivedInitTestSuite.test("CatchRethrow") {
  resetDeinitCounts()
  expectNil(try? CatchRethrow(throwing: true))
  expectEqual(0, baseDeinitCalled)
  expectEqual(0, derivedDeinitCalled)

  resetDeinitCounts()
  expectNotNil(try? CatchRethrow(throwing: false))
  expectEqual(1, baseDeinitCalled)
  expectEqual(1, derivedDeinitCalled)
}

class GenericDerived<T>: Base {
  var t: T
  var x: LifetimeTracked

  init(_ t: T, throwingAt stage: Stage) throws {
    self.t = t
    if stage == .beforeSecondField { throw E() }
    x = LifetimeTracked(1)
    try super.init(throwing: stage == .superInit)
    if stage == .afterSuperInit { throw E() }
  }

  deinit {
    derivedDeinitCalled += 1
  }
}

DerivedInitTestSuite.test("Generic") {
  for stage in [Stage.beforeSecondField, .superInit] {
    resetDeinitCounts()
    expectNil(try? GenericDerived(LifetimeTracked(3), throwingAt: stage))
    expectEqual(0, baseDeinitCalled)
    expectEqual(0, derivedDeinitCalled)
  }

  resetDeinitCounts()
  expectNil(try? GenericDerived(LifetimeTracked(3), throwingAt: .afterSuperInit))
  expectEqual(1, baseDeinitCalled)
  expectEqual(1, derivedDeinitCalled)
}

runAllTests()
