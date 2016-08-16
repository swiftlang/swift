// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift -Xfrontend -enable-experimental-property-behaviors %s -o %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest

protocol delayedImmutable {
  associatedtype Value
  var storage: Value? { get set }
}
extension delayedImmutable {
  var value: Value {
    // The property can only be read after it's been initialized.
    get {
      guard let theValue = storage else {
        fatalError("delayedImmutable property read before initialization")
      }
      return theValue
    }
    
    // The property can only be written once to initialize it.
    set {
      guard storage == nil else {
        fatalError("delayedImmutable property rewritten after initialization")
      }
      storage = newValue
    }
  }

  static func initStorage() -> Value? {
    return nil
  }
}

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
  var x: Int __behavior delayedImmutable
  var y: Int __behavior lazy { evaluateLazy() }
}

var DelayedImmutable = TestSuite("DelayedImmutable")

DelayedImmutable.test("correct usage") {
  let foo = Foo()
  foo.x = 679
  expectEqual(foo.x, 679)
}

DelayedImmutable.test("read before initialization") {
  let foo = Foo()
  expectCrashLater()
  _ = foo.x
}

DelayedImmutable.test("write after initialization") {
  let foo = Foo()
  foo.x = 679
  expectCrashLater()
  foo.x = 680
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
