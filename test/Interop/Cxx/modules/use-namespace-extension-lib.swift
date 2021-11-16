// RUN: %empty-directory(%t)
// RUN: cd %t
// RUN: %target-build-swift    -I %S/Inputs -Xfrontend -enable-cxx-interop %S/Inputs/namespace-extension-lib.swift -emit-module -emit-library -module-name NamespaceExtensionLib
// RUN: %target-build-swift %s -I %S/Inputs -Xfrontend -enable-cxx-interop -o %t/run -I %t/ -L %t/ -lNamespaceExtensionLib
// RUN: %target-codesign %t/run
// RUN: %target-run %t/run
//
// REQUIRES: executable_test

// https://bugs.swift.org/browse/SR-15488
// REQUIRES: SR15488

import StdlibUnittest
import Namespace
import NamespaceExtensionLib

var NamespacesTestSuite = TestSuite("Extension in library on namespace")

NamespacesTestSuite.test("Call functions from extension") {
  expectEqual(Namespace.Parent.test(), 42)
  expectEqual(Namespace.Parent.Child.test(), 52)
  expectEqual(Namespace.NestedNamespace.NestedStruct().test(), 62)
}

runAllTests()

