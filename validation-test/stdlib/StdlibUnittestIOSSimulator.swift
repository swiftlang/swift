// RUN: %target-run-stdlib-swift | FileCheck %s
// REQUIRES: executable_test

import Swift
import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

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

