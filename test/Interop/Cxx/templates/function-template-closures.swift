// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -cxx-interoperability-mode=default)

// REQUIRES: executable_test
// REQUIRES: objc_interop

import FunctionTemplates
import StdlibUnittest

var FunctionTemplateClosuresTestSuite = TestSuite("Function Templates with Closures")

FunctionTemplateClosuresTestSuite.test("callFunction<F> where F == () -> ()") {
  var value = false
  callFunction({() in value = true})
  expectTrue(value)
}

FunctionTemplateClosuresTestSuite.test("callFunction<F> where F == () -> Bool") {
  var value = false
  callFunction({() in value = true; return true})
  expectTrue(value)
}

FunctionTemplateClosuresTestSuite.test("callFunctionWithParam<F, T> where F == (Int) -> (), T == Int") {
  var value = 42
  callFunctionWithParam({(x: Int) in value = x}, 24)
  expectEqual(value, 24)
}

FunctionTemplateClosuresTestSuite.test("callFunctionWithParam<F, T> where F == (Int) -> Bool, T == Int") {
  var value = 42
  callFunctionWithParam({(x: Int) in value = x; return true}, 24)
  expectEqual(value, 24)
}

FunctionTemplateClosuresTestSuite.test("callFunctionWithParam<F, T> where F == (CxxClass) -> (), T == CxxClass") {
  var value: CInt = 42

  // Read x via direct field access
  callFunctionWithParam({(c: CxxClass) in value = c.x}, CxxClass(x: 24))
  expectEqual(value, 24)

  // Read x via getter method
  callFunctionWithParam({(c: CxxClass) in value = c.getX()}, CxxClass(x: 22))
  expectEqual(value, 22)
}

// TODO: this likely needs more design work
// FunctionTemplateClosuresTestSuite.test("callFunctionWithParam<F, T> where F == (()) -> (), T == ()") {
//   var value = 42
//   // None of these are quite right:
//   // callFunctionWithParam({(_: ()) in value = 24}, ())
//   // callFunctionWithParam({(_: Void) in value = 24}, ())
//   // callFunctionWithParam({() in value = 24}, ())
//   expectEqual(value, 24)
// }

FunctionTemplateClosuresTestSuite.test("callFunctionWithReturn<F, T> where F == () -> Int, T == Int") {
  let val1: Int = callFunctionWithReturn({() in 111})
  expectEqual(val1, 111)
  let val2: Int = callFunctionWithReturn({() -> Int in 222})
  expectEqual(val2, 222)
  // Cannot infer return type without val3 explicitly annotated as Int
  // let val3 = callFunctionWithReturn({() -> Int in 333})
  // expectEqual(val3, 333)

  // This works:
  expectEqual(callFunctionWithReturn({() in 444}), 444)
  // This too:
  expectEqual(callFunctionWithReturn({() in true}), true)
  // But this does not (probably a quirk of expectTrue())
  // expectTrue(callFunctionWithReturn({() in true}))
}

FunctionTemplateClosuresTestSuite.test("callFunctionWithReturn<F, T> where F == () -> CxxClass, T == CxxClass") {
  let c: CxxClass = callFunctionWithReturn({() in CxxClass(x: 42)})
  expectEqual(c.x, 42)

  let d: CxxClass = callFunctionWithReturn({() in c})
  expectEqual(d.x, 42)
}

FunctionTemplateClosuresTestSuite.test("callFunctionWithPassthrough<F, T> where F == (T) -> T, T == Int") {
  expectEqual(callFunctionWithPassthrough({(x: Int) in x}, 42), 42)
  expectEqual(callFunctionWithPassthrough({(x: Int) in 24}, 42), 24)
  expectEqual(callFunctionWithPassthrough({(x: Int) in x + 42}, 42), 84)
}

FunctionTemplateClosuresTestSuite.test("indirectlyCallFunction<T> where T == () -> ()") {
  var value = 42

  indirectlyCallFunction({() in value = 111});
  expectEqual(value, 111)

  indirectlyCallFunctionTemplate({() in value = 222});
  expectEqual(value, 222)
}

FunctionTemplateClosuresTestSuite.test("indirectlyCallFunction<T> where T == () -> ()") {
  var value: CInt = 0

  indirectlyCallFunctionWith42({(x: CInt) in value = x});
  expectEqual(value, 42)

  indirectlyCallFunctionWithCxxClass24({(c: CxxClass) in value = c.x});
  expectEqual(value, 24)
}

FunctionTemplateClosuresTestSuite.test("callFunctionWithReturn<F, T> where F == void (^)(int), T == CInt") {
  var value: CInt = 0

  value = callFunctionWithReturn(blockReturns111!)
  expectEqual(value, 111)

  value = callFunctionWithReturn(nonNullBlockReturns222)
  expectEqual(value, 222)

  getConstantIntBlock(333, {cb in value = callFunctionWithReturn(cb) })
  expectEqual(value, 333)
}

FunctionTemplateClosuresTestSuite.test("callFunctionWithPassthrough<F, T> where F == int (^)(int), T == CInt") {
  expectEqual(callFunctionWithPassthrough(blockTripleInt!, 7), 21)
  expectEqual(callFunctionWithPassthrough(nonNullBlockTripleInt, 8), 24)
  expectEqual(getMultiplyIntBlock(9, {cb in callFunctionWithPassthrough(cb, 3)}), 27)
}

FunctionTemplateClosuresTestSuite.test("callFunctionWithReturn<T, B> where T == C function ptr, B == CInt") {
  var value: CInt = 0

  value = callFunctionWithReturn(get42)
  expectEqual(value, 42)

  value = callFunctionWithReturn(functionPtrGet42!)
  expectEqual(value, 42)

  value = callFunctionWithReturn(nonNullFunctionPtrGet42)
  expectEqual(value, 42)
}

FunctionTemplateClosuresTestSuite.test("callFunctionWithPassthrough<F, T> where C function ptr, T == CInt") {
  expectEqual(callFunctionWithPassthrough(tripleInt, 4), 12)
  expectEqual(callFunctionWithPassthrough(functionPtrTripleInt!, 5), 15)
  expectEqual(callFunctionWithPassthrough(nonNullFunctionPtrTripleInt, 6), 18)
}

runAllTests()
