// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)

// REQUIRES: executable_test

import FunctionTemplates
import StdlibUnittest

var FunctionTemplateTestSuite = TestSuite("Function Templates")

FunctionTemplateTestSuite.test("passThrough<T> where T == Int") {
  let result = passThrough(42)
  expectEqual(42, result)
}

FunctionTemplateTestSuite.test("addSameTypeParams<T> where T == Int") {
  let result = addSameTypeParams(42, 23)
  expectEqual(65, result)
}

FunctionTemplateTestSuite.test("addSameTypeParams<T, U> where T, U == Int") {
  let result = addMixedTypeParams(42, 23)
  expectEqual(65, result)
}

FunctionTemplateTestSuite.test("lvalueReference<T> where T == Int") {
  var value = 0
  lvalueReference(&value)
  expectEqual(value, 42)
}

FunctionTemplateTestSuite.test("lvalueReferenceZero<T> where T == Bool") {
  var value = true
  lvalueReferenceZero(&value)
  expectEqual(value, false)
}

FunctionTemplateTestSuite.test("constLvalueReferenceToBool<T> where T == Bool") {
  expectTrue(constLvalueReferenceToBool(true))
  expectFalse(constLvalueReferenceToBool(false))
}

// TODO: Generics, Any, and Protocols should be tested here but need to be
// better supported in ClangTypeConverter first.

runAllTests()
