// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test

import StdlibUnittest

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T
}

struct TestWrappedValueLeak {
  @Wrapper var wrapped: LifetimeTracked = LifetimeTracked(0)
  var str: String

  init() {
    wrapped = LifetimeTracked(42)
    str = ""
    wrapped = LifetimeTracked(27)
  }

  init(conditionalInit: Bool) {
    if (conditionalInit) {
      wrapped = LifetimeTracked(42)
    }
    str = ""
    wrapped = LifetimeTracked(27)
  }
}

var propertyWrapperTests = TestSuite("Property Wrapper DI")

propertyWrapperTests.test("test wrapped value leak") {
  _ = TestWrappedValueLeak()
  _ = TestWrappedValueLeak(conditionalInit: true)
  _ = TestWrappedValueLeak(conditionalInit: false)
}

protocol IntInitializable {
  init(_ i: Int)
}

extension LifetimeTracked : IntInitializable {
  convenience init(_ i: Int) {
    self.init(i, identity: 0)
  }
}

struct TestWrappedValueLeakGeneric<T : IntInitializable> {
  @Wrapper var wrapped: T = T(0)
  var str: String

  init() {
    wrapped = T(42)
    str = ""
    wrapped = T(27)
  }

  init(conditionalInit: Bool) {
    if (conditionalInit) {
      wrapped = T(42)
    }
    str = ""
    wrapped = T(27)
  }
}

propertyWrapperTests.test("test wrapped value leak - generic") {
  _ = TestWrappedValueLeakGeneric<LifetimeTracked>()
  _ = TestWrappedValueLeakGeneric<LifetimeTracked>(conditionalInit: true)
  _ = TestWrappedValueLeakGeneric<LifetimeTracked>(conditionalInit: false)
}

runAllTests()
