// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none)
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

TemplateNotInSignatureTestSuite.test("Instantiate the same function template twice.") {
  // Intentionally test the same thing twice.
  templateTypeParamNotUsedInSignature(T: Int.self)
  templateTypeParamNotUsedInSignature(T: Int.self)
}

TemplateNotInSignatureTestSuite.test("Pointer types") {
  var x = 1
  x = templateTypeParamUsedInReferenceParam(&x)
  expectEqual(x, 1)
}

TemplateNotInSignatureTestSuite.test("Member function templates") {
  let s = Struct()
  s.templateTypeParamNotUsedInSignature(T: Int.self)
  let x: Int = templateTypeParamUsedInReturnType(42)
  expectEqual(x, 42)
}

TemplateNotInSignatureTestSuite.test("Member function templates (mutable)") {
  var s = Struct()
  s.templateTypeParamNotUsedInSignatureMutable(T: Int.self)
}

TemplateNotInSignatureTestSuite.test("Member function templates (static)") {
  Struct.templateTypeParamNotUsedInSignatureStatic(T: Int.self)
}

TemplateNotInSignatureTestSuite.test("Type not used in signature and takes an inout parameter.") {
  var x = 42
  let out = templateTypeParamNotUsedInSignatureWithRef(&x, U: Int.self)
  expectEqual(out, 42)
}

runAllTests()
