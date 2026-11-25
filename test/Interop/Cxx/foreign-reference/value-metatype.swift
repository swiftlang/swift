// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=default -target %target-swift-5.8-abi-triple)
//
// REQUIRES: executable_test

import StdlibUnittest
import POD

func dynamicType<T>(_ x: T) -> Any.Type { type(of: x) }

var ValueMetatypeTestSuite = TestSuite("type(of:) on foreign reference types")

ValueMetatypeTestSuite.test("IntPair") {
  let x = IntPair.create()
  expectTrue(dynamicType(x) == IntPair.self)
}

runAllTests()
