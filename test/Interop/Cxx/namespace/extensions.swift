// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import Extensions

extension EmptyNamespace {
  static var a = "a"
}

extension EmptyNamespace {
  static var b = "b"
}

fileprivate extension EmptyRedeclaredNamespace {
  static var c = "c"
}

extension ParentNamespace.EmptyChildNamespace {
  static var d = "d"

  static func e() -> String { "e" }
}

var NamespacesTestSuite = TestSuite("Extensions on namespaces")

NamespacesTestSuite.test("EmptyNamespace") {
  expectEqual(EmptyNamespace.a, "a")
  expectEqual(EmptyNamespace.b, "b")
}

NamespacesTestSuite.test("EmptyRedeclaredNamespace") {
  expectEqual(EmptyRedeclaredNamespace.c, "c")
}

NamespacesTestSuite.test("Nested namespace") {
  expectEqual(ParentNamespace.EmptyChildNamespace.d, "d")
  expectEqual(ParentNamespace.EmptyChildNamespace.e(), "e")
}

runAllTests()

