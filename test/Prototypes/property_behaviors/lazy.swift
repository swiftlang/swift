// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -enable-experimental-property-behaviors %s -o %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest

/// A lazily-initialized, mutable, unsynchronized property. The property's
/// parameter closure is evaluated the first time the property is read, if
/// it has not been written to beforehand. No synchronization is provided
/// for the lazy initialization.
protocol lazy {
  associatedtype Value
  var storage: Value? { get set }
  func parameter() -> Value
}
extension lazy {
  var value: Value {
    mutating get {
      if let existing = storage {
        return existing
      }
      let value = parameter()
      storage = value
      return value
    }
    
    set {
      storage = newValue
    }
  }

  static func initStorage() -> Value? {
    return nil
  }
}

var lazyEvaluated = false
func evaluateLazy() -> Int {
  lazyEvaluated = true
  return 1738
}

class Foo {
  var y: Int __behavior lazy { evaluateLazy() }

  // FIXME: Hack because we can't find the synthesized associated type witness
  // during witness matching.
  typealias Value = Int
}

var Lazy = TestSuite("Lazy")

Lazy.test("usage") {
  let foo = Foo()

  expectFalse(lazyEvaluated)
  expectEqual(foo.y, 1738)
  expectTrue(lazyEvaluated)

  lazyEvaluated = false
  expectEqual(foo.y, 1738)
  expectFalse(lazyEvaluated)

  foo.y = 36
  expectEqual(foo.y, 36)
  expectFalse(lazyEvaluated)

  let foo2 = Foo()
  expectFalse(lazyEvaluated)
  foo2.y = 36
  expectEqual(foo2.y, 36)
  expectFalse(lazyEvaluated)

  let foo3 = Foo()
  expectFalse(lazyEvaluated)
  expectEqual(foo3.y, 1738)
  expectTrue(lazyEvaluated)
}

runAllTests()
