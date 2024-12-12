// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test
import InheritedLookup
import StdlibUnittest

var InheritedMemberTestSuite = TestSuite("Test if inherited lookup works")

InheritedMemberTestSuite.test("IIBase1::method() resolves to grandparent") {
  let iibase1 = IIBase1()
  expectEqual(iibase1.method(), 1)
}

runAllTests()
