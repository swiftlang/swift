// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=default)

// REQUIRES: executable_test

import StdlibUnittest
import MemberwiseInitializer

var Suite = TestSuite("Memberwise Initializer")

Suite.test("ClassWithConstexprStatic") {
  let instance = ClassWithConstexprStatic(x: 123)
  expectEqual(123, instance.x)
}

runAllTests()
