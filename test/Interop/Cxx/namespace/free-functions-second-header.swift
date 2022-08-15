// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import FreeFunctionsSecondHeader

var NamespacesTestSuite = TestSuite("Functions second header in namespaces")

NamespacesTestSuite.test("Defined and declared in different headers") {
  let definedInDefsCString = FunctionsNS1.definedInDefs()
  expectEqual(String(cString: definedInDefsCString!), "FunctionsNS1::definedInDefs")
}

runAllTests()

