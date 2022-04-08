// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import ClassesSecondHeader

var NamespacesTestSuite = TestSuite("Classes second header in namespaces")

NamespacesTestSuite.test("Declared and defined in different headers") {
  var basicStructInst = ClassesNS1.ClassesNS2.DefinedInDefs()
  let basicMemberCString = basicStructInst.basicMember()
  expectEqual(String(cString: basicMemberCString!),
              "ClassesNS1::ClassesNS2::DefinedInDefs::basicMember")
}

runAllTests()

