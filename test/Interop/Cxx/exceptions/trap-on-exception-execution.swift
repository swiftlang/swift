// RUN: %empty-directory(%t)
// RUN: split-file %S/trap-on-exception-irgen-itanium.swift %t

// RUN: %target-build-swift %s -I %t/Inputs -o %t/trap-exceptions -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none -g
// RUN: %target-codesign %t/trap-exceptions
// RUN: %target-run %t/trap-exceptions

// RUN: %target-build-swift %s -I %t/Inputs -o %t/trap-exceptions-no-debug -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none
// RUN: %target-codesign %t/trap-exceptions-no-debug
// RUN: %target-run %t/trap-exceptions-no-debug

// RUN: %target-build-swift %s -I %t/Inputs -o %t/trap-exceptions-opt -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none -O
// RUN: %target-codesign %t/trap-exceptions-opt
// RUN: %target-run %t/trap-exceptions-opt

// REQUIRES: executable_test

// FIXME: Support MSVC exceptions.
// UNSUPPORTED: OS=windows-msvc

import CxxModule
import StdlibUnittest

func makeCInt() -> CInt {
  return 42
}

var TrapOnExecutionTestSuite = TestSuite("TrapOnExecution")

TrapOnExecutionTestSuite.test("freeFunctionNoThrow") {
  expectEqual(freeFunctionNoThrow(makeCInt()), -42)
}

TrapOnExecutionTestSuite.test("freeFunctionThrows") {
  expectCrashLater()
  let _ = freeFunctionThrows(2)
}

TrapOnExecutionTestSuite.test("freeFunctionThrowsNoException") {
  expectEqual(freeFunctionThrows(-1), 1)
}

TrapOnExecutionTestSuite.test("freeFunctionCatchesException") {
  expectEqual(freeFunctionCatchesException(-1), 1)
  expectEqual(freeFunctionCatchesException(2), 2)
}

TrapOnExecutionTestSuite.test("TestClassMethodThrows") {
  expectCrashLater()
  let v = TestClass()
  v.method(2)
}

TrapOnExecutionTestSuite.test("TestClassNoExceptMethod") {
  var v = TestClass()
  expectEqual(v.noExceptMethod(1), -1)
}

TrapOnExecutionTestSuite.test("TestTemplateIntDependentNoExceptMethod") {
  var v = TestTemplateInt()
  v.dependentNoExceptMethod()
}

TrapOnExecutionTestSuite.test("TestTemplateBoolDependentNoExceptMethod") {
  expectCrashLater()
  var v = TestTemplateBool()
  v.dependentNoExceptMethod()
}

protocol MethodProtocol {
  func method(_ x: CInt) -> CInt
}

extension TestClass: MethodProtocol {
}

TrapOnExecutionTestSuite.test("TestProtocolConformanceThunkInvoke") {
  let v = TestClass()
  let p: MethodProtocol = v
  expectCrashLater()
  let _ = p.method(2)
}

TrapOnExecutionTestSuite.test("TestClassWithSubscript") {
  let v = ClassWithSubscript()
  expectEqual(v[0], 0)
  expectCrashLater()
  let _ = v[1]
}

TrapOnExecutionTestSuite.test("TestClassWithThrowingDestructor") {
  expectCrashLater()
  let _ = ClassWithThrowingDestructor()
}

TrapOnExecutionTestSuite.test("TestClassWithThrowingCopyConstructor") {
  expectCrashLater()
  let p1 = ClassWithThrowingCopyConstructor()
  var p2 = p1
  expectEqual(p2.m, 0)
  p2.m = 1
  expectEqual(p2.m, 1)
  expectEqual(p1.m, 0)
}

TrapOnExecutionTestSuite.test("ClassWithThrowingConstructor") {
  expectCrashLater()
  let _ = ClassWithThrowingConstructor()
}

TrapOnExecutionTestSuite.test("ClassWithNoThrowingConstructor") {
  let obj = ClassWithNoThrowingConstructor()
  expectEqual(obj.m, 0)
}

runAllTests()
