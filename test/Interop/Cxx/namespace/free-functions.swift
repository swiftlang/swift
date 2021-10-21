// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import FreeFunctions

var NamespacesTestSuite = TestSuite("Functions in namespaces")

NamespacesTestSuite.test("Basic functions") {
  let basicFunctionTopLevelCString = FunctionsNS1.basicFunctionTopLevel()
  expectEqual(String(cString: basicFunctionTopLevelCString!),
              "FunctionsNS1::basicFunctionTopLevel")

  let basicFunctionSecondLevelCString = FunctionsNS1.FunctionsNS2.basicFunctionSecondLevel()
  expectEqual(String(cString: basicFunctionSecondLevelCString!),
              "FunctionsNS1::FunctionsNS2::basicFunctionSecondLevel")

  let basicFunctionLowestLevelCString = FunctionsNS1.FunctionsNS2.FunctionsNS3.basicFunctionLowestLevel()
  expectEqual(String(cString: basicFunctionLowestLevelCString!),
              "FunctionsNS1::FunctionsNS2::FunctionsNS3::basicFunctionLowestLevel")

  let x = FunctionsNS1.X()
  expectEqual(String(cString: x + x),
              "FunctionsNS1::operator+(X, X)")
}

NamespacesTestSuite.test("Forward declared functions") {
  let forwardDeclaredCString = FunctionsNS1.forwardDeclared()
  expectEqual(String(cString: forwardDeclaredCString!), "FunctionsNS1::forwardDeclared")

  let definedOutOfLineCString = FunctionsNS1.definedOutOfLine()
  expectEqual(String(cString: definedOutOfLineCString!), "FunctionsNS1::definedOutOfLine")
}

NamespacesTestSuite.test("Functions with the same name") {
  let sameNameInChildCString = FunctionsNS1.sameNameInChild()
  expectEqual(String(cString: sameNameInChildCString!), "FunctionsNS1::sameNameInChild")

  let sameNameInSiblingCString = FunctionsNS1.sameNameInSibling()
  expectEqual(String(cString: sameNameInSiblingCString!), "FunctionsNS1::sameNameInSibling")

  let ns2SameNameInChildCString = FunctionsNS1.FunctionsNS2.sameNameInChild()
  expectEqual(String(cString: ns2SameNameInChildCString!),
              "FunctionsNS1::FunctionsNS2::sameNameInChild")

  let ns4SameNameInSiblingCString = FunctionsNS4.sameNameInSibling()
  expectEqual(String(cString: ns4SameNameInSiblingCString!), "FunctionsNS4::sameNameInSibling")
}

runAllTests()

