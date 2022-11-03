// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xfrontend -disable-availability-checking)
//
// REQUIRES: executable_test

// Please don't add tests into this test case - its setup is quite delicate.
import ClassTemplateInstantiationExistingSpecialization
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

// This test covers the case where Clang has to create specialization eagerly
// because the code in the header forces it to. Swift has to reuse this
// specialization, it must not create its own.
TemplatesTestSuite.test("existing-specialization") {
  let myInt = IntWrapper(value: 18)
  var magicInt = MagicWrapper<IntWrapper>(t: myInt)
  expectEqual(magicInt.getValuePlusArg(12), 30)
}

runAllTests()
