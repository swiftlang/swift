// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/reference.cpp -I %S/Inputs -o %t/reference.o -std=c++17
// RUN: %target-build-swift %s -I %S/Inputs -o %t/reference %t/reference.o -Xfrontend -enable-cxx-interop
// RUN: %target-codesign %t/reference
// RUN: %target-run %t/reference
//
// REQUIRES: executable_test

import Reference
import StdlibUnittest

var ReferenceTestSuite = TestSuite("Reference")

ReferenceTestSuite.test("read-lvalue-reference") {
  expectNotEqual(13, getStaticInt())
  setStaticInt(13)
  expectEqual(13, getStaticIntRef().pointee)
  expectEqual(13, getConstStaticIntRef().pointee)
}

ReferenceTestSuite.test("read-rvalue-reference") {
  expectNotEqual(32, getStaticInt())
  setStaticInt(32)
  expectEqual(32, getStaticIntRvalueRef().pointee)
  expectEqual(32, getConstStaticIntRvalueRef().pointee)
}

ReferenceTestSuite.test("write-lvalue-reference") {
  expectNotEqual(14, getStaticInt())
  getStaticIntRef().pointee = 14
  expectEqual(14, getStaticInt())
}

ReferenceTestSuite.test("write-rvalue-reference") {
  expectNotEqual(41, getStaticInt())
  getStaticIntRvalueRef().pointee = 41
  expectEqual(41, getStaticInt())
}

ReferenceTestSuite.test("pass-lvalue-reference") {
  expectNotEqual(21, getStaticInt())
  var val: CInt = 21
  withUnsafeMutablePointer(to: &val) {
    setStaticIntRef($0)
  }
  expectEqual(21, getStaticInt())
}

ReferenceTestSuite.test("pass-const-lvalue-reference") {
  expectNotEqual(22, getStaticInt())
  var val: CInt = 22
  withUnsafePointer(to: &val) {
    setConstStaticIntRef($0)
  }
  expectEqual(22, getStaticInt())
}

ReferenceTestSuite.test("pass-rvalue-reference") {
  expectNotEqual(52, getStaticInt())
  var val: CInt = 52
  withUnsafeMutablePointer(to: &val) {
    setStaticIntRvalueRef($0)
  }
  expectEqual(52, getStaticInt())
}

ReferenceTestSuite.test("pass-const-rvalue-reference") {
  expectNotEqual(53, getStaticInt())
  var val: CInt = 53
  withUnsafePointer(to: &val) {
    setConstStaticIntRvalueRef($0)
  }
  expectEqual(53, getStaticInt())
}

ReferenceTestSuite.test("func-reference") {
  let cxxF: @convention(c) () -> Int32 = getFuncRef()

  expectNotEqual(15, getStaticInt())
  setStaticInt(15)
  expectEqual(15, cxxF())
}

ReferenceTestSuite.test("func-rvalue-reference") {
  let cxxF: @convention(c) () -> Int32 = getFuncRvalueRef()

  expectNotEqual(61, getStaticInt())
  setStaticInt(61)
  expectEqual(61, cxxF())
}

runAllTests()
