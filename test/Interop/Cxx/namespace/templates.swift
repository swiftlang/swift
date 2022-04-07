// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import Templates

var NamespacesTestSuite = TestSuite("Templates in namespaces")

NamespacesTestSuite.test("Basic classes") {
  let basicFunctionTemplateCString = TemplatesNS1.basicFunctionTemplate(0)
  expectEqual(String(cString: basicFunctionTemplateCString!),
              "TemplatesNS1::basicFunctionTemplate")
  
  var basicClassTemplateInst = TemplatesNS1.BasicClassTemplateChar()
  let basicClassTemplateCString = basicClassTemplateInst.basicMember()
  expectEqual(String(cString: basicClassTemplateCString!),
              "TemplatesNS1::BasicClassTemplate::basicMember")
  
  let takesClassTemplateFromSiblingCString = TemplatesNS1.TemplatesNS2.takesClassTemplateFromSibling(
    TemplatesNS1.TemplatesNS2.BasicClassTemplateChar())
  expectEqual(String(cString: takesClassTemplateFromSiblingCString!), "TemplatesNS1::TemplatesNS2::takesClassTemplateFromSibling")
}

NamespacesTestSuite.test("Forward declared") {
  let forwardDeclaredFunctionTemplateCString = TemplatesNS1.TemplatesNS2.forwardDeclaredFunctionTemplate(0)
  expectEqual(String(cString: forwardDeclaredFunctionTemplateCString!),
              "TemplatesNS1::TemplatesNS2::forwardDeclaredFunctionTemplate")
  
  var forwardDeclaredClassTemplateInst = TemplatesNS1.ForwardDeclaredClassTemplateChar()
  let forwardDeclaredClassTemplateCString = forwardDeclaredClassTemplateInst.basicMember()
  expectEqual(String(cString: forwardDeclaredClassTemplateCString!),
              "TemplatesNS1::TemplatesNS2::ForwardDeclaredClassTemplate::basicMember")
  
  let forwardDeclaredFunctionTemplateOutOfLineCString = TemplatesNS1.TemplatesNS2.forwardDeclaredFunctionTemplateOutOfLine(0)
  expectEqual(String(cString: forwardDeclaredFunctionTemplateOutOfLineCString!),
              "TemplatesNS1::TemplatesNS2::forwardDeclaredFunctionTemplateOutOfLine")
  
  var forwardDeclaredClassTemplateOutOfLineInst = ForwardDeclaredClassTemplateOutOfLineChar()
  let forwardDeclaredClassTemplateOutOfLineCString = forwardDeclaredClassTemplateOutOfLineInst.basicMember()
  expectEqual(String(cString: forwardDeclaredClassTemplateOutOfLineCString!),
              "TemplatesNS1::TemplatesNS2::ForwardDeclaredClassTemplateOutOfLine::basicMember")
}

runAllTests()
