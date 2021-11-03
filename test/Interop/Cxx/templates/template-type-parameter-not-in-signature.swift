// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xfrontend -validate-tbd-against-ir=none)
//
// REQUIRES: executable_test

import TemplateTypeParameterNotInSignature
import StdlibUnittest

var TemplateNotInSignatureTestSuite = TestSuite("Template Type Parameters Not in Function Signature")

TemplateNotInSignatureTestSuite.test("Function with defaulted template type parameters") {
  templateTypeParamNotUsedInSignature(T: Int.self)
  multiTemplateTypeParamNotUsedInSignature(T: Float.self, U: Int.self)
  let x: Int = multiTemplateTypeParamOneUsedInSignature(1, T: Int.self)
  expectEqual(x, 1)
  multiTemplateTypeParamNotUsedInSignatureWithUnrelatedParams(1, 1, T: Int32.self, U: Int.self)
  let y: Int = templateTypeParamUsedInReturnType(10)
  expectEqual(y, 10)
}

TemplateNotInSignatureTestSuite.test("Pointer types") {
  var x = 1
  x = templateTypeParamUsedInReferenceParam(&x)
  expectEqual(x, 1)
}

runAllTests()
