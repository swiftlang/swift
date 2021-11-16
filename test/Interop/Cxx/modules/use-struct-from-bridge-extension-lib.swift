// RUN: %empty-directory(%t)
// RUN: cd %t
// RUN: %target-build-swift    -import-objc-header %S/Inputs/bridge-header.h -Xfrontend -enable-cxx-interop %S/Inputs/struct-from-bridge-extension-lib.swift -emit-module -emit-library -module-name BridgeExtensionLib
// RUN: %target-build-swift %s -import-objc-header %S/Inputs/bridge-header.h -Xfrontend -enable-cxx-interop -o %t/run -I %t/ -L %t/ -lBridgeExtensionLib
// RUN: %target-codesign %t/run
// RUN: %target-run %t/run
//
// REQUIRES: executable_test

// https://bugs.swift.org/browse/SR-15488
// REQUIRES: SR15488

import StdlibUnittest
import BridgeExtensionLib

var BridgeTestSuite = TestSuite("Extension in library on nested types in bridge header")

BridgeTestSuite.test("Call functions from extension") {
  expectEqual(Parent.Child.test(), 42)
  expectEqual(Namespace.InNamespace().test(), 52)
}

runAllTests()

