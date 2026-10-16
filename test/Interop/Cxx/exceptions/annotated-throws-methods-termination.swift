// RUN: %empty-directory(%t)
// RUN: split-file %S/annotated-throws-methods.swift %t
// RUN: %target-build-swift %s -I %t -o %t/test -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging -Xfrontend -disable-availability-checking
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

import AnnotatedThrowingMethods
import StdlibUnittest

var tests = TestSuite("CxxExceptionBridgingMethodTermination")

// The base method is not annotated, so its Swift type doesn't throw, and an
// exception from the annotated override terminates.
tests.test("UnannotatedBaseMethod") {
  expectCrashLater()
  _ = getUnannotatedReferenceBase().mismatched(true)
}

tests.test("UnannotatedOverride") {
  expectCrashLater()
  _ = UnannotatedReferenceOverride.create().mismatched(true)
}

runAllTests()
