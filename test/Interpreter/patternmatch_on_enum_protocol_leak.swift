// RUN: %target-run-simple-swift

// REQUIRES: executable_test

import StdlibUnittest

// Make sure that in the following code we do not leak the case of the enum.

protocol MyProtocol {}

// An enum that wraps LeakingClass
enum LeakingEnum1: MyProtocol {
case eNone1
case eLeakingClass1(LifetimeTracked)
}

// An enum that wraps LeakingClass
enum LeakingEnum2 : MyProtocol {
case eNone2
case eLeakingClass2(LifetimeTracked)
}

var Tests = TestSuite("patternmatch_on_enum_protocol_leak")

Tests.test("dontLeak") {
  do {
    let leakingClass = LifetimeTracked(0)
    let leakEnum = LeakingEnum1.eLeakingClass1(leakingClass)
    let control: MyProtocol = leakEnum

    // This switch case order, interleaving LeakingEnum1 and LeakingEnum2 cases triggers the leak.
    switch control
    {
    case LeakingEnum1.eNone1:               break
    case LeakingEnum2.eNone2:               break

    case LeakingEnum1.eLeakingClass1:       break
    case LeakingEnum2.eLeakingClass2:       break

    default:                                break
    }
  }
  expectEqual(0, LifetimeTracked.instances)
}

runAllTests()
