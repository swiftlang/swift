// RUN: %target-run-stdlib-swift | FileCheck %s

import Swift
import StdlibUnittest

_setOverrideOSVersion(.iOSSimulator)
_setTestSuiteFailedCallback() { print("abort()") }

var XFailsIOSSimulator = TestSuite("XFailsIOSSimulator")

// CHECK: [   UXPASS ] XFailsIOSSimulator.xfail iOS Simulator passes{{$}}
XFailsIOSSimulator.test("xfail iOS Simulator passes")
  .xfail(.iOSSimulatorAny("")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsIOSSimulator.xfail iOS Simulator fails{{$}}
XFailsIOSSimulator.test("xfail iOS Simulator fails")
  .xfail(.iOSSimulatorAny("")).code {
  expectEqual(1, 2)
}

// CHECK: XFailsIOSSimulator: Some tests failed, aborting
// CHECK: abort()

runAllTests()

