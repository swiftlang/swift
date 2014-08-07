// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %s -o %t.out
// RUN: %target-run %t.out | FileCheck %s

import StdlibUnittest

_setOverrideOSVersion(.iOS(major: 10, minor: 9, bugFix: 3))
_setTestCaseFailedCallback() { println("abort()") }

var XFailsIOS = TestCase("XFailsIOS")

// CHECK: [   UXPASS ] XFailsIOS.xfail iOS passes{{$}}
XFailsIOS.testXFail("xfail iOS passes", xfail: [.iOSAny("")]) {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsIOS.xfail iOS fails{{$}}
XFailsIOS.testXFail("xfail iOS fails", xfail: [.iOSAny("")]) {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsIOS.xfail 9.*{{$}}
XFailsIOS.testXFail("xfail 9.*", xfail: [.iOSMajor(9, reason: "")]) {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsIOS.xfail 10.*{{$}}
XFailsIOS.testXFail("xfail 10.*", xfail: [.iOSMajor(10, reason: "")]) {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsIOS.xfail 10.8{{$}}
XFailsIOS.testXFail("xfail 10.8", xfail: [.iOSMinor(10, 8, reason: "")]) {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsIOS.xfail 10.9{{$}}
XFailsIOS.testXFail("xfail 10.9", xfail: [.iOSMinor(10, 9, reason: "")]) {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsIOS.xfail 10.[7-8]{{$}}
XFailsIOS.testXFail("xfail 10.[7-8]", xfail: [.iOSMinorRange(10, 7...8, reason: "")]) {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsIOS.xfail 10.[9-10]{{$}}
XFailsIOS.testXFail("xfail 10.[9-10]", xfail: [.iOSMinorRange(10, 9...10, reason: "")]) {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsIOS.xfail 10.9.2{{$}}
XFailsIOS.testXFail("xfail 10.9.2", xfail: [.iOSBugFix(10, 9, 2, reason: "")]) {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsIOS.xfail 10.9.3{{$}}
XFailsIOS.testXFail("xfail 10.9.3", xfail: [.iOSBugFix(10, 9, 3, reason: "")]) {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsIOS.xfail 10.9.[1-2]{{$}}
XFailsIOS.testXFail("xfail 10.9.[1-2]", xfail: [.iOSBugFixRange(10, 9, 1...2, reason: "")]) {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsIOS.xfail 10.9.[3-4]{{$}}
XFailsIOS.testXFail("xfail 10.9.[3-4]", xfail: [.iOSBugFixRange(10, 9, 3...4, reason: "")]) {
  expectEqual(1, 2)
}

// CHECK: XFailsIOS: Some tests failed, aborting
// CHECK: abort()

runAllTests()

