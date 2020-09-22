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

TestSuite("Property Wrapper DI").test("test wrapped value leak") {
  _ = TestWrappedValueLeak()
  _ = TestWrappedValueLeak(conditionalInit: true)
  _ = TestWrappedValueLeak(conditionalInit: false)
}

runAllTests()
