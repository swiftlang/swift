// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test
//
// We can't yet call member functions correctly on Windows (SR-13129).
// XFAIL: OS=windows-msvc
// REQUIRES: fixing-after-30630

import ClassTemplateVariadic
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("variadic-class-template") {
  let a = IntWrapper(value: 10)
  let b = IntWrapper(value: 20)

  var pair = Pair()
  pair.set(a, b)

  var pairA = pair.first()
  var restB = pair.rest()
  var pairB = restB.first()
  expectEqual(pairA.getValue(), 10)
  expectEqual(pairB.getValue(), 20)
}

runAllTests()
