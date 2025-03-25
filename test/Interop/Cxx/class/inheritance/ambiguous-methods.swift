// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import Methods

var AmbiguousMethodsTestSuite = TestSuite("Ambiguous methods in classes")

AmbiguousMethodsTestSuite.test("Simple") { }

struct Foo {
  let bar: C3
}

func c3(x: C3) {
  _ = x.GetPrim()
}

runAllTests()
