// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -enable-experimental-property-behaviors %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest

/// An immutable property with "delayed" initialization semantics. The property
/// may be set at most once, after which it is not allowed to be mutated.
/// The property must also be set before it is ever read.
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
  var x: Int __behavior delayedImmutable

  // FIXME: Hack because we can't find the synthesized associated type witness
  // during witness matching.
  typealias Value = Int
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

runAllTests()
