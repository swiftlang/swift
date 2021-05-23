// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import Classes

var NamespacesTestSuite = TestSuite("Classes in namespaces")

NamespacesTestSuite.test("Basic classes") {
  var basicStructInst = ClassesNS1.BasicStruct()
  let basicMemberCString = basicStructInst.basicMember()
  expectEqual(String(cString: basicMemberCString!), "ClassesNS1::BasicStruct::basicMember")
  
  var nestedBasicStructInst = ClassesNS1.ClassesNS2.BasicStruct()
  let nestedBasicMemberCString = nestedBasicStructInst.basicMember()
  expectEqual(String(cString: nestedBasicMemberCString!),
              "ClassesNS1::ClassesNS2::BasicStruct::basicMember")
  
  var siblingBasicStruct = ClassesNS3.BasicStruct()
  let siblingMemberCString = siblingBasicStruct.basicMember()
  expectEqual(String(cString: siblingMemberCString!), "ClassesNS3::BasicStruct::basicMember")

  var basicStructViaAlias = ClassesNS4.AliasToGlobalNS1.BasicStruct()
  let basicMemberViaAliasCString = basicStructViaAlias.basicMember()
  expectEqual(String(cString: basicMemberViaAliasCString!), "ClassesNS1::BasicStruct::basicMember")
}

NamespacesTestSuite.test("Forward declared classes") {
  var forwardDeclaredStruct = ClassesNS1.ForwardDeclaredStruct()
  let basicMemberCString = forwardDeclaredStruct.basicMember()
  expectEqual(String(cString: basicMemberCString!), "ClassesNS1::ForwardDeclaredStruct::basicMember")

  var nestedForwardDeclaredStruct = ClassesNS1.ClassesNS2.ForwardDeclaredStruct()
  let nestedBasicMemberCString = nestedForwardDeclaredStruct.basicMember()
  expectEqual(String(cString: nestedBasicMemberCString!),
              "ClassesNS1::ClassesNS2::ForwardDeclaredStruct::basicMember")
}

runAllTests()

