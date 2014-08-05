// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %s -o %t.out
// RUN: %target-run %t.out | FileCheck %s

import Darwin
import StdlibUnittest

//
// Test OS version parsing
//

// CHECK: (10, 0, 0)
println(_parseDottedVersionTriple("10"))

// CHECK: (10, 9, 0)
println(_parseDottedVersionTriple("10.9"))

// CHECK: (10, 9, 3)
println(_parseDottedVersionTriple("10.9.3"))

//
// Test that harness aborts when a test fails
//

var TestCasePasses = TestCase("TestCasePasses")
TestCasePasses.test("passes") {
  expectEqual(1, 1)
}
_setTestCaseFailedCallback() { abort() }
TestCasePasses.run()
// CHECK: [       OK ] TestCasePasses.passes{{$}}
// CHECK: TestCasePasses: All tests passed

var TestCaseUXPasses = TestCase("TestCaseUXPasses")
TestCaseUXPasses.testXFail("uxpasses", xfail: [.OSXAny("")]) {
  expectEqual(1, 1)
}
_setTestCaseFailedCallback() { println("abort()") }
_setOverrideOSVersion(.OSX(major: 10, minor: 9, bugFix: 3))
TestCaseUXPasses.run()
// CHECK: [   UXPASS ] TestCaseUXPasses.uxpasses{{$}}
// CHECK: TestCaseUXPasses: Some tests failed, aborting
// CHECK: UXPASS: [uxpasses]
// CHECK: FAIL: []
// CHECK: SKIP: []
// CHECK: abort()

var TestCaseFails = TestCase("TestCaseFails")
TestCaseFails.test("fails") {
  expectEqual(1, 2)
}
_setTestCaseFailedCallback() { println("abort()") }
_setOverrideOSVersion(.OSX(major: 10, minor: 9, bugFix: 3))
TestCaseFails.run()
// CHECK: [     FAIL ] TestCaseFails.fails{{$}}
// CHECK: TestCaseFails: Some tests failed, aborting
// CHECK: UXPASS: []
// CHECK: FAIL: [fails]
// CHECK: SKIP: []
// CHECK: abort()

var TestCaseXFails = TestCase("TestCaseXFails")
TestCaseXFails.testXFail("xfails", xfail: [.OSXAny("")]) {
  expectEqual(1, 2)
}
_setTestCaseFailedCallback() { abort() }
TestCaseXFails.run()
// CHECK: [    XFAIL ] TestCaseXFails.xfails{{$}}
// CHECK: TestCaseXFails: All tests passed

//
// Test 'xfail:' and 'skip:' annotations
//

var XFailsAndSkips = TestCase("XFailsAndSkips")

// CHECK: [       OK ] XFailsAndSkips.passes{{$}}
XFailsAndSkips.test("passes") {
  expectEqual(1, 1)
}

// CHECK: [     FAIL ] XFailsAndSkips.fails{{$}}
XFailsAndSkips.test("fails") {
  expectEqual(1, 2)
}

// CHECK: [    XFAIL ] XFailsAndSkips.xfail 10.9.3 passes{{$}}
XFailsAndSkips.testXFail("xfail 10.9.3 passes", xfail: [.OSXBugFix(10, 9, 3, reason: "")]) {
  expectEqual(1, 2)
}

// CHECK: [    XFAIL ] XFailsAndSkips.xfail 10.9.3 fails{{$}}
XFailsAndSkips.testXFail("xfail 10.9.3 fails", xfail: [.OSXBugFix(10, 9, 3, reason: "")]) {
  expectEqual(1, 2)
}

// CHECK: [     FAIL ] XFailsAndSkips.skip 10.9.2 passes{{$}}
XFailsAndSkips.testSkip("skip 10.9.2 passes", skip: [.OSXBugFix(10, 9, 2, reason: "")]) {
  expectEqual(1, 2)
}

// CHECK: [     FAIL ] XFailsAndSkips.skip 10.9.2 fails{{$}}
XFailsAndSkips.testSkip("skip 10.9.2 fails", skip: [.OSXBugFix(10, 9, 2, reason: "")]) {
  expectEqual(1, 2)
}

// CHECK: [ SKIP     ] XFailsAndSkips.skip 10.9.3 (skip: [OSX(10.9.3, reason: )]){{$}}
XFailsAndSkips.testSkip("skip 10.9.3", skip: [.OSXBugFix(10, 9, 3, reason: "")]) {
  expectEqual(1, 2)
  fatalError("should not be executed")
}

_setTestCaseFailedCallback() { println("abort()") }
_setOverrideOSVersion(.OSX(major: 10, minor: 9, bugFix: 3))
XFailsAndSkips.run()
// CHECK: XFailsAndSkips: Some tests failed, aborting
// CHECK: abort()

//
// Test custom XFAIL predicates
//

var XFailsCustomPredicates = TestCase("XFailsCustomPredicates")

// CHECK: [    XFAIL ] XFailsCustomPredicates.matches{{$}}
XFailsCustomPredicates.testXFail("matches", xfail: [.Custom({ true }, reason: "")]) {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsCustomPredicates.not matches{{$}}
XFailsCustomPredicates.testXFail("not matches", xfail: [.Custom({ false }, reason: "")]) {
  expectEqual(1, 1)
}

_setTestCaseFailedCallback() { abort() }
XFailsCustomPredicates.run()
// CHECK: XFailsCustomPredicates: All tests passed

//
// Test version comparison rules
//

var XFailsOSX = TestCase("XFailsOSX")

// CHECK: [   UXPASS ] XFailsOSX.xfail OSX passes{{$}}
XFailsOSX.testXFail("xfail OSX passes", xfail: [.OSXAny("")]) {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail OSX fails{{$}}
XFailsOSX.testXFail("xfail OSX fails", xfail: [.OSXAny("")]) {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 9.*{{$}}
XFailsOSX.testXFail("xfail 9.*", xfail: [.OSXMajor(9, reason: "")]) {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.*{{$}}
XFailsOSX.testXFail("xfail 10.*", xfail: [.OSXMajor(10, reason: "")]) {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 10.8{{$}}
XFailsOSX.testXFail("xfail 10.8", xfail: [.OSXMinor(10, 8, reason: "")]) {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.9{{$}}
XFailsOSX.testXFail("xfail 10.9", xfail: [.OSXMinor(10, 9, reason: "")]) {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 10.[7-8]{{$}}
XFailsOSX.testXFail("xfail 10.[7-8]", xfail: [.OSXMinorRange(10, 7...8, reason: "")]) {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.[9-10]{{$}}
XFailsOSX.testXFail("xfail 10.[9-10]", xfail: [.OSXMinorRange(10, 9...10, reason: "")]) {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 10.9.2{{$}}
XFailsOSX.testXFail("xfail 10.9.2", xfail: [.OSXBugFix(10, 9, 2, reason: "")]) {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.9.3{{$}}
XFailsOSX.testXFail("xfail 10.9.3", xfail: [.OSXBugFix(10, 9, 3, reason: "")]) {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 10.9.[1-2]{{$}}
XFailsOSX.testXFail("xfail 10.9.[1-2]", xfail: [.OSXBugFixRange(10, 9, 1...2, reason: "")]) {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.9.[3-4]{{$}}
XFailsOSX.testXFail("xfail 10.9.[3-4]", xfail: [.OSXBugFixRange(10, 9, 3...4, reason: "")]) {
  expectEqual(1, 2)
}

_setTestCaseFailedCallback() { println("abort()") }
_setOverrideOSVersion(.OSX(major: 10, minor: 9, bugFix: 3))
XFailsOSX.run()
// CHECK: XFailsOSX: Some tests failed, aborting
// CHECK: abort()

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

_setTestCaseFailedCallback() { println("abort()") }
_setOverrideOSVersion(.iOS(major: 10, minor: 9, bugFix: 3))
XFailsIOS.run()
// CHECK: XFailsIOS: Some tests failed, aborting
// CHECK: abort()

