// RUN: %target-run-simple-swift

// REQUIRES: executable_test

import StdlibUnittest

var SwitchDefaultTestSuite = TestSuite("Switch.Default")
defer { runAllTests() }

class Klass {}
protocol Protocol {}

enum Enum {
  case value1(LifetimeTracked)
  case value2(Protocol)
}

SwitchDefaultTestSuite.test("do not leak default case payload") {
  // We are passing in Enum.value1 so we go down the default and leak our
  // lifetime tracked.
  func f(_ e: Enum?) {
    switch (e) {
    case .value2: return
    default: return
    }
  }
  f(.value1(LifetimeTracked(0)))
}
