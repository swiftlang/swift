// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %s -o %t.out
// RUN: %target-run %t.out | FileCheck %s

import StdlibUnittest

_setOverrideOSVersion(.iOSSimulator)
_setTestCaseFailedCallback() { println("abort()") }

var XFailsIOSSimulator = TestCase("XFailsIOSSimulator")

// CHECK: [   UXPASS ] XFailsIOSSimulator.xfail iOS Simulator passes{{$}}
XFailsIOSSimulator.testXFail("xfail iOS Simulator passes", xfail: [.iOSSimulatorAny("")]) {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsIOSSimulator.xfail iOS Simulator fails{{$}}
XFailsIOSSimulator.testXFail("xfail iOS Simulator fails", xfail: [.iOSSimulatorAny("")]) {
  expectEqual(1, 2)
}

// CHECK: XFailsIOSSimulator: Some tests failed, aborting
// CHECK: abort()

runAllTests()

