// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import FunctionTemplates
import StdlibUnittest

var FunctionTemplateTestSuite = TestSuite("Function Templates")

FunctionTemplateTestSuite.test("passThrough<T> where T == Int") {
  let result = passThrough(42)
  expectEqual(42, result)
}

FunctionTemplateTestSuite.test("add<T> where T == Int") {
  let result = add(42, 23)
  expectEqual(65, result)
}

FunctionTemplateTestSuite.test("add<T, U> where T, U == Int") {
  let result = addTwoTemplates(42, 23)
  expectEqual(65, result)
}

FunctionTemplateTestSuite.test("lvalueReference<T> where T == Int") {
  var value = 0
  lvalueReference(&value)
  expectEqual(value, 42)
}

// TODO: currently "Any" is imported as an Objective-C "id".
// This doesn't work without the Objective-C runtime. 
#if _runtime(_ObjC)
FunctionTemplateTestSuite.test("passThrough<T> where T == Any") {
  let result = passThrough(42 as Any)
  expectEqual(42, result as! Int)
}
#endif

// TODO: Generics, Any, and Protocols should be tested here but need to be
// better supported in ClangTypeConverter first.

runAllTests()
