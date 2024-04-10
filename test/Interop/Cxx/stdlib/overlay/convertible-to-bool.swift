// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)

// REQUIRES: executable_test

import StdlibUnittest
import ConvertibleToBool

var CxxConvertibleToBoolTestSuite = TestSuite("CxxConvertibleToBool")

CxxConvertibleToBoolTestSuite.test("BoolBox as CxxConvertibleToBool") {
  let b1 = BoolBox(value: true)
  expectTrue(Bool(fromCxx: b1))

  let b2 = BoolBox(value: false)
  expectFalse(Bool(fromCxx: b2))
}

CxxConvertibleToBoolTestSuite.test("DualOverloadBoolBox as CxxConvertibleToBool") {
  let b1 = DualOverloadBoolBox(value: true)
  expectTrue(Bool(fromCxx: b1))

  let b2 = DualOverloadBoolBox(value: false)
  expectFalse(Bool(fromCxx: b2))
}

runAllTests()
