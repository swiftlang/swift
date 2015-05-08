// RUN: %target-run-stdlib-swift | FileCheck %s

import Swift
import StdlibUnittest

_setOverrideOSVersion(.iOS(major: 10, minor: 9, bugFix: 3))
_setTestSuiteFailedCallback() { print("abort()") }

var XFailsIOS = TestSuite("XFailsIOS")

// CHECK: [   UXPASS ] XFailsIOS.xfail iOS passes{{$}}
XFailsIOS.test("xfail iOS passes").xfail(.iOSAny("")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsIOS.xfail iOS fails{{$}}
XFailsIOS.test("xfail iOS fails").xfail(.iOSAny("")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsIOS.xfail 9.*{{$}}
XFailsIOS.test("xfail 9.*").xfail(.iOSMajor(9, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsIOS.xfail 10.*{{$}}
XFailsIOS.test("xfail 10.*").xfail(.iOSMajor(10, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsIOS.xfail 10.8{{$}}
XFailsIOS.test("xfail 10.8").xfail(.iOSMinor(10, 8, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsIOS.xfail 10.9{{$}}
XFailsIOS.test("xfail 10.9").xfail(.iOSMinor(10, 9, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsIOS.xfail 10.[7-8]{{$}}
XFailsIOS.test("xfail 10.[7-8]")
  .xfail(.iOSMinorRange(10, 7...8, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsIOS.xfail 10.[9-10]{{$}}
XFailsIOS.test("xfail 10.[9-10]")
  .xfail(.iOSMinorRange(10, 9...10, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsIOS.xfail 10.9.2{{$}}
XFailsIOS.test("xfail 10.9.2")
  .xfail(.iOSBugFix(10, 9, 2, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsIOS.xfail 10.9.3{{$}}
XFailsIOS.test("xfail 10.9.3")
  .xfail(.iOSBugFix(10, 9, 3, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsIOS.xfail 10.9.[1-2]{{$}}
XFailsIOS.test("xfail 10.9.[1-2]")
  .xfail(.iOSBugFixRange(10, 9, 1...2, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsIOS.xfail 10.9.[3-4]{{$}}
XFailsIOS.test("xfail 10.9.[3-4]")
  .xfail(.iOSBugFixRange(10, 9, 3...4, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: XFailsIOS: Some tests failed, aborting
// CHECK: abort()

runAllTests()

