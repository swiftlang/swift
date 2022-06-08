// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import TemplatesSecondHeader

var NamespacesTestSuite = TestSuite("Templates in namespaces")

NamespacesTestSuite.test("Defined and declared in different headers") {
  let basicFunctionTemplateCString = TemplatesNS1.basicFunctionTemplateDefinedInDefs(0)
  expectEqual(String(cString: basicFunctionTemplateCString!),
              "TemplatesNS1::basicFunctionTemplateDefinedInDefs")

  var basicClassTemplateInst = BasicClassTemplateDefinedInDefsChar()
  let basicClassTemplateCString = basicClassTemplateInst.basicMember()
  expectEqual(String(cString: basicClassTemplateCString!), "TemplatesNS1::BasicClassTemplateDefinedInDefs::basicMember")
}

runAllTests()
