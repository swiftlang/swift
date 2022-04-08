// RUN: %empty-directory(%t)
// RUN: cd %t
// RUN: %target-build-swift    -I %S/Inputs -Xfrontend -enable-experimental-cxx-interop %S/Inputs/namespace-extension-lib.swift -emit-module -emit-library -static -module-name NamespaceExtensionLib
// RUN: %target-build-swift %s -I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -o %t/run -I %t/ -L %t/ -lNamespaceExtensionLib
// RUN: %target-codesign %t/run
// RUN: %target-run %t/run
//
// REQUIRES: executable_test
// XFAIL: OS=windows-msvc

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

