// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++14)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++17)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20)

// REQUIRES: executable_test

import StdlibUnittest
import StdFunction
import CxxStdlib

var StdFunctionTestSuite = TestSuite("StdFunction")

StdFunctionTestSuite.test("FunctionIntToInt.init()") {
  let f = FunctionIntToInt()
  expectTrue(isEmptyFunction(f))

  let copied = f
  expectTrue(isEmptyFunction(copied))
}

StdFunctionTestSuite.test("FunctionIntToInt.callAsFunction") {
  let f = getIdentityFunction()
  expectEqual(123, f(123))
}

StdFunctionTestSuite.test("FunctionIntToInt retrieve and pass back as parameter") {
  let res = invokeFunction(getIdentityFunction(), 456)
  expectEqual(456, res)
}

#if !os(Windows) // FIXME: rdar://103979602
StdFunctionTestSuite.test("FunctionIntToInt init from closure and call") {
  let cClosure: @convention(c) (Int32) -> Int32 = { $0 + 1 }
  let f = FunctionIntToInt(cClosure)
  expectEqual(1, f(0))
  expectEqual(124, f(123))
  expectEqual(0, f(-1))

  let f2 = FunctionIntToInt({ $0 * 2 })
  expectEqual(0, f2(0))
  expectEqual(246, f2(123))
}

StdFunctionTestSuite.test("FunctionIntToInt init from closure and pass as parameter") {
  let res = invokeFunction(.init({ $0 * 2 }), 111)
  expectEqual(222, res)
}

StdFunctionTestSuite.test("FunctionStringToString init from closure and pass as parameter") {
  let res = invokeFunctionTwice(.init({ $0 + std.string("abc") }),
                                std.string("prefix"))
  expectEqual(std.string("prefixabcabc"), res)
}

StdFunctionTestSuite.test("FunctionStringToStringConstRef init from closure and pass as parameter") {
  let res = invokeFunctionTwiceConstRef(.init({ $0 + std.string("abc") }),
                                        std.string("prefix"))
  expectEqual(std.string("prefixabcabc"), res)
}
#endif

StdFunctionTestSuite.test("FunctionVoidToVoid init from thick closure and call") {
  var counter = 0
  let f1 = FunctionVoidToVoid { counter += 1 }
  expectEqual(0, counter)
  f1()
  expectEqual(1, counter)
  f1()
  expectEqual(2, counter)
}


StdFunctionTestSuite.test("FunctionVoidToInt init from thick closure and call") {
  let external: Int32 = 123 
  let f1 = FunctionVoidToInt { external }
  expectEqual(123, f1())
}

StdFunctionTestSuite.test("FunctionIntToVoid init from thick closure and call") {
  var counter: Int32 = 0
  let f1 = FunctionIntToVoid { counter += $0 }
  expectEqual(0, counter)
  f1(5)
  expectEqual(5, counter)
  f1(10)
  expectEqual(15, counter)
}

StdFunctionTestSuite.test("FunctionIntToInt init from thick closure and call") {
  let external: Int32 = 123 
  let f1 = FunctionIntToInt { $0 + external }
  expectEqual(123, f1(0))
  expectEqual(124, f1(1))
}

StdFunctionTestSuite.test("FunctionConstIntToInt init from thick closure and call") {
  let external: Int32 = 321 
  let f1 = FunctionConstIntToInt { $0 + external }
  expectEqual(321, f1(0))
  expectEqual(322, f1(1))
}

StdFunctionTestSuite.test("FunctionConstRefIntToInt init from thick closure and call") {
  let external: Int32 = 456 
  let f1 = FunctionConstIntToInt { $0 + external }
  expectEqual(456, f1(0))
  expectEqual(457, f1(1))
}

StdFunctionTestSuite.test("FunctionIntIntToInt init from thick closure and call") {
  let immutableExternal: Int32 = 10
  let f1 = FunctionIntIntToInt({ a, b in
    a - b + immutableExternal
  })
  expectEqual(11, f1(3, 2))
  expectEqual(9, f1(2, 3))
  
  var mutableExternal: Int32 = -100
  let f2 = FunctionIntIntToInt({ a, b in
    a - b + mutableExternal
  })
  mutableExternal = 20
  expectEqual(21, f2(3, 2))
  mutableExternal = 0
  expectEqual(1, f2(3, 2))

  var modifiedExternal: Int32 = 20
  let f3 = FunctionIntIntToInt({ a, b in
    let result = a - b + modifiedExternal
    modifiedExternal = a + b
    return result
  })
  expectEqual(21, f3(3, 2))
  expectEqual(6, f3(3, 2))
  expectEqual(4, f3(1, 2))
  expectEqual(2, f3(1, 2))

  struct MyContext {
    var x: Int32
    var y: Int32
    var z: Int32
  }
  var external = MyContext(x: 123, y: 456, z: 789)
  let closure: (Int32, Int32) -> Int32 = {
    let r = $0 + $1 + external.x
    external.y = r
    return r
  }
  let f4 = FunctionIntIntToInt(closure)
  expectEqual(246, f4(123, 0))
  expectEqual(124, f4(1, 0))
  expectEqual(122, f4(-1, 0))
}

StdFunctionTestSuite.test("FunctionIntToInt init from thick closure and pass as parameter") {
  let immutableExternal: Int32 = 10
  let res1 = invokeFunctionIntToIntTwice(.init({ $0 + immutableExternal }),
                                         123)
  expectEqual(143, res1)

  var modifiedExternal: Int32 = 20
  let res2 = invokeFunctionIntToIntTwice(.init({ a in
                                           modifiedExternal += a
                                           return a + modifiedExternal
                                         }),
                                         7)
  expectEqual(95, res2)
}

StdFunctionTestSuite.test("FunctionStringToString init from thick closure and pass as parameter") {
  var modifiedExternal = std.string()
  let res = invokeFunctionTwice(.init({
                                  let result = $0 + std.string("abc")
                                  modifiedExternal = result
                                  return result
                                }),
                                std.string("prefix_"))
  expectEqual(std.string("prefix_abcabc"), res)
  expectEqual(std.string("prefix_abcabc"), modifiedExternal)
}

runAllTests()
