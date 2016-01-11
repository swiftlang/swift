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

_setOverrideOSVersion(.FreeBSD)
_setTestSuiteFailedCallback() { print("abort()") }

var XFailsFreeBSD = TestSuite("XFailsFreeBSD")

// CHECK: [   UXPASS ] XFailsFreeBSD.xfail FreeBSD passes{{$}}
XFailsFreeBSD.test("xfail FreeBSD passes").xfail(.FreeBSDAny(reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsFreeBSD.xfail FreeBSD fails{{$}}
XFailsFreeBSD.test("xfail FreeBSD fails").xfail(.FreeBSDAny(reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: XFailsFreeBSD: Some tests failed, aborting
// CHECK: abort()

runAllTests()

