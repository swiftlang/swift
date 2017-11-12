// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// FIXME: this test is failing for watchos <rdar://problem/29996991>
// UNSUPPORTED: OS=watchos

import SwiftPrivate
import StdlibUnittest


_setOverrideOSVersion(.osx(major: 10, minor: 9, bugFix: 3))
_setTestSuiteFailedCallback() { print("abort()") }

//
// Test that harness aborts when a test fails
//

var TestSuitePasses = TestSuite("TestSuitePasses")

// CHECK: {{^}}[ RUN      ] TestSuitePasses.passes{{$}}
// CHECK: {{^}}[       OK ] TestSuitePasses.passes{{$}}
TestSuitePasses.test("passes") {
  expectEqual(1, 1)
}

// CHECK: {{^}}[ RUN      ] TestSuitePasses.passes/parameterized/0{{$}}
// CHECK: {{^}}stdout>>> 1010{{$}}
// CHECK: {{^}}[       OK ] TestSuitePasses.passes/parameterized/0{{$}}
// CHECK: {{^}}[ RUN      ] TestSuitePasses.passes/parameterized/1{{$}}
// CHECK: {{^}}stdout>>> 2020{{$}}
// CHECK: {{^}}[       OK ] TestSuitePasses.passes/parameterized/1{{$}}
TestSuitePasses.test("passes/parameterized").forEach(in: [1010, 2020]) {
  (parameter) in

  print(parameter)
  expectEqual(1, 1)
}
// CHECK: TestSuitePasses: All tests passed

var TestSuiteUXPasses = TestSuite("TestSuiteUXPasses")

// CHECK: {{^}}[   UXPASS ] TestSuiteUXPasses.uxpasses{{$}}
TestSuiteUXPasses.test("uxpasses").xfail(.osxAny("")).code {
  expectEqual(1, 1)
}

// CHECK: {{^}}[   UXPASS ] TestSuiteUXPasses.uxpasses/parameterized/0{{$}}
// CHECK: {{^}}[    XFAIL ] TestSuiteUXPasses.uxpasses/parameterized/1{{$}}
TestSuiteUXPasses.test("uxpasses/parameterized")
  .xfail(.osxAny(""))
  .forEach(in: [1010, 2020]) {
  (parameter) in

  if parameter == 1010 {
    expectEqual(1, 1)
  } else {
    expectEqual(1, 2)
  }
}

// CHECK: TestSuiteUXPasses: Some tests failed, aborting
// CHECK: UXPASS: ["uxpasses", "uxpasses/parameterized/0"]
// CHECK: FAIL: []
// CHECK: SKIP: []
// CHECK: abort()

var TestSuiteFails = TestSuite("TestSuiteFails")

// CHECK: {{^}}[     FAIL ] TestSuiteFails.fails{{$}}
TestSuiteFails.test("fails") {
  expectEqual(1, 2)
}

// CHECK: {{^}}[       OK ] TestSuiteFails.fails/parameterized/0{{$}}
// CHECK: {{^}}[     FAIL ] TestSuiteFails.fails/parameterized/1{{$}}
TestSuiteFails.test("fails/parameterized").forEach(in: [1010, 2020]) {
  (parameter) in

  if parameter == 1010 {
    expectEqual(1, 1)
  } else {
    expectEqual(1, 2)
  }
}

// CHECK: TestSuiteFails: Some tests failed, aborting
// CHECK: UXPASS: []
// CHECK: FAIL: ["fails", "fails/parameterized/1"]
// CHECK: SKIP: []
// CHECK: abort()

var TestSuiteXFails = TestSuite("TestSuiteXFails")

// CHECK: {{^}}[    XFAIL ] TestSuiteXFails.xfails{{$}}
TestSuiteXFails.test("xfails").xfail(.osxAny("")).code {
  expectEqual(1, 2)
}

// CHECK: {{^}}[   UXPASS ] TestSuiteXFails.xfails/parameterized/0{{$}}
// CHECK: {{^}}[    XFAIL ] TestSuiteXFails.xfails/parameterized/1{{$}}
TestSuiteXFails.test("xfails/parameterized")
  .xfail(.osxAny(""))
  .forEach(in: [1010, 2020]) {
  (parameter) in

  if parameter == 1010 {
    expectEqual(1, 1)
  } else {
    expectEqual(1, 2)
  }
}

// CHECK: TestSuiteXFails: Some tests failed, aborting
// CHECK: UXPASS: ["xfails/parameterized/0"]
// CHECK: FAIL: []
// CHECK: SKIP: []

//
// Test 'xfail:' and 'skip:' annotations
//

var XFailsAndSkips = TestSuite("XFailsAndSkips")

// CHECK: [       OK ] XFailsAndSkips.passes{{$}}
XFailsAndSkips.test("passes") {
  expectEqual(1, 1)
}

// CHECK: [     FAIL ] XFailsAndSkips.fails{{$}}
XFailsAndSkips.test("fails") {
  expectEqual(1, 2)
}

// CHECK: [    XFAIL ] XFailsAndSkips.fails-always{{$}}
XFailsAndSkips.test("fails-always")
  .xfail(.always("must always fail")).code {
  expectEqual(1, 2)
}

// CHECK: [      OK ] XFailsAndSkips.fails-never{{$}}
XFailsAndSkips.test("fails-never")
  .xfail(.never).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsAndSkips.xfail 10.9.3 passes{{$}}
XFailsAndSkips.test("xfail 10.9.3 passes")
  .xfail(.osxBugFix(10, 9, 3, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [    XFAIL ] XFailsAndSkips.xfail 10.9.3 fails{{$}}
XFailsAndSkips.test("xfail 10.9.3 fails")
  .xfail(.osxBugFix(10, 9, 3, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [     SKIP ] XFailsAndSkips.skipAlways (skip: [Always(reason: skip)]){{$}}
XFailsAndSkips.test("skipAlways")
  .skip(.always("skip")).code {
    fatalError("should not happen")
}

// CHECK: [       OK ] XFailsAndSkips.skipNever{{$}}
XFailsAndSkips.test("skipNever")
  .skip(.never).code {
    expectEqual(1, 1)
}

// CHECK: [     FAIL ] XFailsAndSkips.skip 10.9.2 passes{{$}}
XFailsAndSkips.test("skip 10.9.2 passes")
  .skip(.osxBugFix(10, 9, 2, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [     FAIL ] XFailsAndSkips.skip 10.9.2 fails{{$}}
XFailsAndSkips.test("skip 10.9.2 fails")
  .skip(.osxBugFix(10, 9, 2, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [ SKIP     ] XFailsAndSkips.skip 10.9.3 (skip: [osx(10.9.3, reason: )]){{$}}
XFailsAndSkips.test("skip 10.9.3")
  .skip(.osxBugFix(10, 9, 3, reason: "")).code {
  expectEqual(1, 2)
  fatalError("should not be executed")
}

// CHECK: XFailsAndSkips: Some tests failed, aborting
// CHECK: abort()

//
// Test custom XFAIL predicates
//

var XFailsCustomPredicates = TestSuite("XFailsCustomPredicates")

// CHECK: [    XFAIL ] XFailsCustomPredicates.matches{{$}}
XFailsCustomPredicates.test("matches")
  .xfail(.custom({ true }, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsCustomPredicates.not matches{{$}}
XFailsCustomPredicates.test("not matches")
  .xfail(.custom({ false }, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: XFailsCustomPredicates: All tests passed

//
// Test version comparison rules
//

var XFailsOSX = TestSuite("XFailsOSX")

// CHECK: [   UXPASS ] XFailsOSX.xfail OSX passes{{$}}
XFailsOSX.test("xfail OSX passes").xfail(.osxAny("")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail OSX fails{{$}}
XFailsOSX.test("xfail OSX fails").xfail(.osxAny("")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 9.*{{$}}
XFailsOSX.test("xfail 9.*").xfail(.osxMajor(9, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.*{{$}}
XFailsOSX.test("xfail 10.*").xfail(.osxMajor(10, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 10.8{{$}}
XFailsOSX.test("xfail 10.8").xfail(.osxMinor(10, 8, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.9{{$}}
XFailsOSX.test("xfail 10.9").xfail(.osxMinor(10, 9, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 10.[7-8]{{$}}
XFailsOSX.test("xfail 10.[7-8]")
  .xfail(.osxMinorRange(10, 7...8, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.[9-10]{{$}}
XFailsOSX.test("xfail 10.[9-10]")
  .xfail(.osxMinorRange(10, 9...10, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 10.9.2{{$}}
XFailsOSX.test("xfail 10.9.2")
  .xfail(.osxBugFix(10, 9, 2, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.9.3{{$}}
XFailsOSX.test("xfail 10.9.3")
  .xfail(.osxBugFix(10, 9, 3, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [       OK ] XFailsOSX.xfail 10.9.[1-2]{{$}}
XFailsOSX.test("xfail 10.9.[1-2]")
  .xfail(.osxBugFixRange(10, 9, 1...2, reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsOSX.xfail 10.9.[3-4]{{$}}
XFailsOSX.test("xfail 10.9.[3-4]")
  .xfail(.osxBugFixRange(10, 9, 3...4, reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: XFailsOSX: Some tests failed, aborting
// CHECK: abort()

//
// Check that we pass through stdout and stderr
//

var PassThroughStdoutStderr = TestSuite("PassThroughStdoutStderr")

PassThroughStdoutStderr.test("hasNewline") {
  print("stdout first")
  print("stdout second")
  print("stdout third")

  var stderr = _Stderr()
  print("stderr first", to: &stderr)
  print("stderr second", to: &stderr)
  print("stderr third", to: &stderr)
}
// CHECK: [ RUN      ] PassThroughStdoutStderr.hasNewline
// CHECK-DAG: stdout>>> stdout first
// CHECK-DAG: stdout>>> stdout second
// CHECK-DAG: stdout>>> stdout third
// CHECK-DAG: stderr>>> stderr first
// CHECK-DAG: stderr>>> stderr second
// CHECK-DAG: stderr>>> stderr third
// CHECK: [       OK ] PassThroughStdoutStderr.hasNewline

PassThroughStdoutStderr.test("noNewline") {
  print("stdout first")
  print("stdout second")
  print("stdout third", terminator: "")

  var stderr = _Stderr()
  print("stderr first", to: &stderr)
  print("stderr second", to: &stderr)
  print("stderr third", terminator: "", to: &stderr)
}
// CHECK: [ RUN      ] PassThroughStdoutStderr.noNewline
// CHECK-DAG: stdout>>> stdout first
// CHECK-DAG: stdout>>> stdout second
// CHECK-DAG: stdout>>> stdout third
// CHECK-DAG: stderr>>> stderr first
// CHECK-DAG: stderr>>> stderr second
// CHECK-DAG: stderr>>> stderr third
// CHECK: [       OK ] PassThroughStdoutStderr.noNewline
// CHECK: PassThroughStdoutStderr: All tests passed

//
// Test 'setUp' and 'tearDown'
//

var TestSuiteWithSetUp = TestSuite("TestSuiteWithSetUp")
var TestSuiteWithSetUpTimesCalled = 0

TestSuiteWithSetUp.setUp {
  print("setUp")
  if TestSuiteWithSetUpTimesCalled == 1 || TestSuiteWithSetUpTimesCalled == 3 {
    expectEqual(1, 2)
  }
  TestSuiteWithSetUpTimesCalled += 1
}

// CHECK: [ RUN      ] TestSuiteWithSetUp.passes
// CHECK: stdout>>> setUp
// CHECK: stdout>>> test body
// CHECK: [       OK ] TestSuiteWithSetUp.passes
TestSuiteWithSetUp.test("passes") {
  print("test body")
}

// CHECK: [ RUN      ] TestSuiteWithSetUp.fails
// CHECK: stdout>>> setUp
// CHECK-NEXT: stdout>>> check failed at {{.*}}/StdlibUnittest/Common.swift, line
// CHECK: stdout>>> test body
// CHECK: [     FAIL ] TestSuiteWithSetUp.fails
TestSuiteWithSetUp.test("fails") {
  print("test body")
}

// CHECK: [ RUN      ] TestSuiteWithSetUp.passesFails/parameterized/0
// CHECK: stdout>>> setUp
// CHECK: stdout>>> test body
// CHECK: [       OK ] TestSuiteWithSetUp.passesFails/parameterized/0
// CHECK: [ RUN      ] TestSuiteWithSetUp.passesFails/parameterized/1
// CHECK: stdout>>> setUp
// CHECK-NEXT: stdout>>> check failed at {{.*}}/StdlibUnittest/Common.swift, line
// CHECK: stdout>>> test body
// CHECK: [     FAIL ] TestSuiteWithSetUp.passesFails/parameterized/1
TestSuiteWithSetUp.test("passesFails/parameterized")
  .forEach(in: [1010, 2020]) {
  (parameter) in

  print("test body")
}

var TestSuiteWithTearDown = TestSuite("TestSuiteWithTearDown")
var TestSuiteWithTearDownShouldFail = false

TestSuiteWithTearDown.tearDown {
  print("tearDown")
  if TestSuiteWithTearDownShouldFail {
    expectEqual(1, 2)
    TestSuiteWithTearDownShouldFail = false
  }
}

// CHECK: [ RUN      ] TestSuiteWithTearDown.passes
// CHECK: stdout>>> test body
// CHECK: stdout>>> tearDown
// CHECK: [       OK ] TestSuiteWithTearDown.passes
TestSuiteWithTearDown.test("passes") {
  print("test body")
}

// CHECK: [ RUN      ] TestSuiteWithTearDown.fails
// CHECK: stdout>>> test body
// CHECK: stdout>>> tearDown
// CHECK-NEXT: stdout>>> check failed at {{.*}}/StdlibUnittest/Common.swift, line
// CHECK: [     FAIL ] TestSuiteWithTearDown.fails
TestSuiteWithTearDown.test("fails") {
  print("test body")
  TestSuiteWithTearDownShouldFail = true
}

// CHECK: [ RUN      ] TestSuiteWithTearDown.passesFails/parameterized/0
// CHECK: stdout>>> test body
// CHECK: stdout>>> tearDown
// CHECK: [       OK ] TestSuiteWithTearDown.passesFails/parameterized/0
// CHECK: [ RUN      ] TestSuiteWithTearDown.passesFails/parameterized/1
// CHECK: stdout>>> test body
// CHECK: stdout>>> tearDown
// CHECK-NEXT: stdout>>> check failed at {{.*}}/StdlibUnittest/Common.swift, line
// CHECK: [     FAIL ] TestSuiteWithTearDown.passesFails/parameterized/1
TestSuiteWithTearDown.test("passesFails/parameterized")
  .forEach(in: [1010, 2020]) {
  (parameter) in

  print("test body")
  if parameter != 1010 {
    TestSuiteWithTearDownShouldFail = true
  }
}

//
// Test assertions
//

var AssertionsTestSuite = TestSuite("Assertions")

AssertionsTestSuite.test("expectFailure/Pass") {
  expectFailure {
    expectEqual(1, 2)
    return ()
  }
}
// CHECK: [ RUN      ] Assertions.expectFailure/Pass
// CHECK-NEXT: stdout>>> check failed at {{.*}}/StdlibUnittest/Common.swift, line
// CHECK: stdout>>> expected: 1 (of type Swift.Int)
// CHECK: stdout>>> actual: 2 (of type Swift.Int)
// CHECK: [       OK ] Assertions.expectFailure/Pass

AssertionsTestSuite.test("expectFailure/UXPass")
  .xfail(.custom({ true }, reason: "test"))
  .code {
  expectFailure {
    expectEqual(1, 2)
    return ()
  }
}
// CHECK: [ RUN      ] Assertions.expectFailure/UXPass ({{X}}FAIL: [Custom(reason: test)])
// CHECK-NEXT: stdout>>> check failed at {{.*}}/StdlibUnittest/Common.swift, line
// CHECK: stdout>>> expected: 1 (of type Swift.Int)
// CHECK: stdout>>> actual: 2 (of type Swift.Int)
// CHECK: [   UXPASS ] Assertions.expectFailure/UXPass

AssertionsTestSuite.test("expectFailure/Fail") {
  expectFailure {
    return ()
  }
}
// CHECK: [ RUN      ] Assertions.expectFailure/Fail
// CHECK-NEXT: stdout>>> check failed at {{.*}}/StdlibUnittest/Common.swift, line
// CHECK: stdout>>> expected: true
// CHECK: stdout>>> running `body` should produce an expected failure
// CHECK: [     FAIL ] Assertions.expectFailure/Fail

AssertionsTestSuite.test("expectFailure/XFail")
  .xfail(.custom({ true }, reason: "test"))
  .code {
  expectFailure {
    return ()
  }
}
// CHECK: [ RUN      ] Assertions.expectFailure/XFail ({{X}}FAIL: [Custom(reason: test)])
// CHECK-NEXT: stdout>>> check failed at {{.*}}/StdlibUnittest/Common.swift, line
// CHECK: stdout>>> expected: true
// CHECK: stdout>>> running `body` should produce an expected failure
// CHECK: [    XFAIL ] Assertions.expectFailure/XFail

AssertionsTestSuite.test("expectFailure/AfterFailure/Fail") {
  expectEqual(1, 2)
  expectFailure {
    expectEqual(3, 4)
    return ()
  }
}
// CHECK: [ RUN      ] Assertions.expectFailure/AfterFailure/Fail
// CHECK-NEXT: stdout>>> check failed at {{.*}}/StdlibUnittest/Common.swift, line
// CHECK: stdout>>> expected: 1 (of type Swift.Int)
// CHECK: stdout>>> actual: 2 (of type Swift.Int)
// CHECK: stdout>>> check failed at {{.*}}/StdlibUnittest/Common.swift, line
// CHECK: stdout>>> expected: 3 (of type Swift.Int)
// CHECK: stdout>>> actual: 4 (of type Swift.Int)
// CHECK: [     FAIL ] Assertions.expectFailure/AfterFailure/Fail

AssertionsTestSuite.test("expectFailure/AfterFailure/XFail")
  .xfail(.custom({ true }, reason: "test"))
  .code {
  expectEqual(1, 2)
  expectFailure {
    expectEqual(3, 4)
    return ()
  }
}
// CHECK: [ RUN      ] Assertions.expectFailure/AfterFailure/XFail ({{X}}FAIL: [Custom(reason: test)])
// CHECK-NEXT: stdout>>> check failed at {{.*}}/StdlibUnittest/Common.swift, line
// CHECK: stdout>>> expected: 1 (of type Swift.Int)
// CHECK: stdout>>> actual: 2 (of type Swift.Int)
// CHECK: stdout>>> check failed at {{.*}}/StdlibUnittest/Common.swift, line
// CHECK: stdout>>> expected: 3 (of type Swift.Int)
// CHECK: stdout>>> actual: 4 (of type Swift.Int)
// CHECK: [    XFAIL ] Assertions.expectFailure/AfterFailure/XFail

AssertionsTestSuite.test("expectUnreachable") {
  expectUnreachable()
}
// CHECK: [ RUN      ] Assertions.expectUnreachable
// CHECK-NEXT: stdout>>> check failed at {{.*}}/StdlibUnittest/Common.swift, line
// CHECK: stdout>>> this code should not be executed
// CHECK: [     FAIL ] Assertions.expectUnreachable

AssertionsTestSuite.test("expectCrashLater/Pass") {
  let array: [Int] = _opaqueIdentity([])
  expectCrashLater()
  _blackHole(array[0])
}
// CHECK: [ RUN      ] Assertions.expectCrashLater/Pass
// CHECK: stderr>>> OK: saw expected "crashed: sig{{.*}}
// CHECK: [       OK ] Assertions.expectCrashLater/Pass

AssertionsTestSuite.test("expectCrashLater/UXPass")
  .xfail(.custom({ true }, reason: "test"))
  .code {
  let array: [Int] = _opaqueIdentity([])
  expectCrashLater()
  _blackHole(array[0])
}
// CHECK: [ RUN      ] Assertions.expectCrashLater/UXPass ({{X}}FAIL: [Custom(reason: test)])
// CHECK: stderr>>> OK: saw expected "crashed: sig{{.*}}
// CHECK: [   UXPASS ] Assertions.expectCrashLater/UXPass

AssertionsTestSuite.test("expectCrashLater/Fail") {
  expectCrashLater()
}
// CHECK: [ RUN      ] Assertions.expectCrashLater/Fail
// CHECK: expecting a crash, but the test did not crash
// CHECK: [     FAIL ] Assertions.expectCrashLater/Fail

AssertionsTestSuite.test("expectCrashLater/XFail")
  .xfail(.custom({ true }, reason: "test"))
  .code {
  expectCrashLater()
}
// CHECK: [ RUN      ] Assertions.expectCrashLater/XFail ({{X}}FAIL: [Custom(reason: test)])
// CHECK: expecting a crash, but the test did not crash
// CHECK: [    XFAIL ] Assertions.expectCrashLater/XFail

AssertionsTestSuite.test("UnexpectedCrash/RuntimeTrap") {
  let array: [Int] = _opaqueIdentity([])
  _blackHole(array[0])
}
// CHECK: [ RUN      ] Assertions.UnexpectedCrash/RuntimeTrap
// CHECK: stderr>>> CRASHED: SIG
// CHECK: the test crashed unexpectedly
// CHECK: [     FAIL ] Assertions.UnexpectedCrash/RuntimeTrap

AssertionsTestSuite.test("UnexpectedCrash/NullPointerDereference") {
  let nilValue: UnsafePointer<Int>? = nil
  let ptr: UnsafePointer<Int> =
    _opaqueIdentity(unsafeBitCast(nilValue, to: UnsafePointer.self))
  _blackHole(ptr.pointee)
}
// CHECK: [ RUN      ] Assertions.UnexpectedCrash/NullPointerDereference
// CHECK: stderr>>> CRASHED: SIG
// CHECK: the test crashed unexpectedly
// CHECK: [     FAIL ] Assertions.UnexpectedCrash/NullPointerDereference

AssertionsTestSuite.test("expectTrapping(_: Bound, in: RangeProtocol)") {
  expectTrapping(0, in: 1..<10)
}
// CHECK: [ RUN      ] Assertions.expectTrapping(_: Bound, in: RangeProtocol)
// CHECK-NEXT: stdout>>> check failed at {{.*}}.swift, line [[@LINE-3]]
// CHECK: stdout>>> 0 in 1..<10{{$}}
// CHECK: the test crashed unexpectedly
// CHECK: [     FAIL ] Assertions.expectTrapping(_: Bound, in: RangeProtocol)

AssertionsTestSuite.test("expectTrapping(_: RangeProtocol, in: RangeProtocol)") {
  expectTrapping(0..<5, in: 1..<10)
}
// CHECK: [ RUN      ] Assertions.expectTrapping(_: RangeProtocol, in: RangeProtocol)
// CHECK-NEXT: stdout>>> check failed at {{.*}}.swift, line [[@LINE-3]]
// CHECK: stdout>>> 0..<5 in 1..<10{{$}}
// CHECK: the test crashed unexpectedly
// CHECK: [     FAIL ] Assertions.expectTrapping(_: RangeProtocol, in: RangeProtocol)

var TestSuiteLifetimeTracked = TestSuite("TestSuiteLifetimeTracked")
var leakMe: LifetimeTracked? = nil
TestSuiteLifetimeTracked.test("failsIfLifetimeTrackedAreLeaked") {
  leakMe = LifetimeTracked(0)
}
// CHECK: [ RUN      ] TestSuiteLifetimeTracked.failsIfLifetimeTrackedAreLeaked
// CHECK-NEXT: stdout>>> check failed at {{.*}}.swift, line [[@LINE-4]]
// CHECK: stdout>>> expected: 0 (of type Swift.Int)
// CHECK: stdout>>> actual: 1 (of type Swift.Int)
// CHECK: [     FAIL ] TestSuiteLifetimeTracked.failsIfLifetimeTrackedAreLeaked

TestSuiteLifetimeTracked.test("passesIfLifetimeTrackedAreResetAfterFailure") {}
// CHECK: [ RUN      ] TestSuiteLifetimeTracked.passesIfLifetimeTrackedAreResetAfterFailure
// CHECK: [       OK ] TestSuiteLifetimeTracked.passesIfLifetimeTrackedAreResetAfterFailure

runAllTests()

