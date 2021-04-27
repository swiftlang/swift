// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-cxx-interop)
// REQUIRES: executable_test

import StdlibUnittest
import Extensions

var CxxClassExtensionsTestSuite = TestSuite("CxxClassExtensions")

extension Outer.Space.Foo {
  func bar() -> Int32 { return a + 1 }
}

CxxClassExtensionsTestSuite.test("calling extension method") {
  var foo = Outer.Space.Foo(a: 123)
  expectEqual(124, foo.bar())
}

runAllTests()
