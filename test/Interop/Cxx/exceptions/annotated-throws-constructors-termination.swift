// RUN: %empty-directory(%t)
// RUN: split-file %S/annotated-throws-constructors.swift %t
// RUN: %target-build-swift %s -I %t -o %t/test -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test
// RUN: %target-build-swift %s -I %t -o %t/test-opt -O -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

import AnnotatedThrowingConstructors
import StdlibUnittest

var tests = TestSuite("CxxConstructorExceptionTermination")
tests.test("NoexceptStillTerminates") {
  expectCrashLater()
  do {
    _ = try NoexceptConstruction(true)
  } catch {
    // Recovering here would violate the C++ noexcept contract.
  }
}
runAllTests()
