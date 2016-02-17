// RUN: %target-run-simple-swift
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

class Foo {
  var [delayedImmutable] x: Int
}

var tests = TestSuite("DelayedImmutable")

tests.test("correct usage") {
  let foo = Foo()
  foo.x = 679
  expectEqual(foo.x, 679)
}

tests.test("read before initialization") {
  let foo = Foo()
  expectCrashLater()
  _ = foo.x
}

tests.test("write after initialization") {
  let foo = Foo()
  foo.x = 679
  expectCrashLater()
  foo.x = 680
}

runAllTests()
