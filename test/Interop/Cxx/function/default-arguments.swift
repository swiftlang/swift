// RUN: %empty-directory(%t)
// RUN: %target-clangxx -c %S/Inputs/default-arguments.cpp -I %S/Inputs -o %t/default-arguments.o
// RUN: %target-build-swift %s -I %S/Inputs -o %t/default-arguments %t/default-arguments.o -cxx-interoperability-mode=upcoming-swift
// RUN: %target-codesign %t/default-arguments
// RUN: %target-run %t/default-arguments

// REQUIRES: executable_test

import StdlibUnittest
import DefaultArguments

var DefaultArgTestSuite = TestSuite("Functions with Default Arguments")

DefaultArgTestSuite.test("func with integer parameter") {
  let z0 = isZero()
  expectTrue(z0)

  let z1 = isZero(1)
  expectFalse(z1)

  let z2 = isNonZero()
  expectFalse(z2)

  let s0 = sum(1)
  expectEqual(2, s0)

  let s1 = sum(3, 4)
  expectEqual(7, s1)

  let s2 = subtract()
  expectEqual(122, s2)

  let s3 = subtract(55)
  expectEqual(54, s3)

  let s4 = subtract(55, 44)
  expectEqual(11, s4)
}

DefaultArgTestSuite.test("func with pointer parameter") {
  let z0 = isNil()
  expectTrue(z0)

  let z1 = isGlobalNonNil()
  expectFalse(z1)
}

DefaultArgTestSuite.test("func with cString parameter") {
  let z0 = isStrNil()
  expectFalse(z0)
}

DefaultArgTestSuite.test("func with mutating integer argument") {
  let z0 = isZeroCounter()
  expectTrue(z0)

  let z1 = isZeroCounter()
  expectFalse(z1)
}

DefaultArgTestSuite.test("func with non-trailing argument") {
  let t0 = nonTrailing()
  expectEqual(3, t0)
  
  let t1 = nonTrailing(5, 6)
  expectEqual(11, t1)
}

DefaultArgTestSuite.test("method with integer parameter") {
  let s = HasMethodWithDefaultArg()
  let z0 = s.isZero()
  expectTrue(z0)

  let z1 = s.isNonZero()
  expectTrue(z1)
}

// TODO: support default args of constructors
// (https://github.com/apple/swift/issues/70124)
DefaultArgTestSuite.test("constructor with integer parameter") {
  let a = HasCtorWithDefaultArg(1, 2, 3)
  expectEqual(a.value, 6)

//  let b = HasCtorWithDefaultArg(1, 2)
//  expectEqual(b.value, 126)

//  let c = HasCtorWithDefaultArg(1)
//  expectEqual(c.value, 580)
}

DefaultArgTestSuite.test("method in a templated class") {
  let t = TemplatedHasMethodWithDefaultArgFloat()
  let z0 = t.isZero()
  expectTrue(z0)

  let z1 = t.isZero(0.0 as Float)
  expectTrue(z1)

  let z2 = t.isNonZero()
  expectFalse(z2)
}

DefaultArgTestSuite.test("derived method with integer parameter") {
  let s = DerivedFromHasMethodWithDefaultArg()
  let z0 = s.isZero()
  expectTrue(z0)

  let z1 = s.isZero(0)
  expectTrue(z1)

  let s1 = DerivedFromDerivedFromHasMethodWithDefaultArg()
  let z2 = s1.isZero()
  expectTrue(z2)

  let z3 = s1.isZero(0)
  expectTrue(z3)
}

DefaultArgTestSuite.test("derived method from a templated class") {
  let s = DerivedFromTemplatedHasMethodWithDefaultArgFloat()
  let z0 = s.isZero()
  expectTrue(z0)

  let z1 = s.isNonZero()
  expectFalse(z1)
}

DefaultArgTestSuite.test("func with record parameter") {
  let z0 = isArgZero()
  expectTrue(z0)

  let z1 = isArgZero(ArgTy(value: 0))
  expectTrue(z1)

  let z2 = isArgZero(ArgTy(value: 1))
  expectFalse(z2)

  let z3 = isArgNonZero()
  expectTrue(z3)

  let z4 = isArgZeroOutOfLine()
  expectTrue(z4)

  let z5 = isArgViewNull(ArgTyView(ptr: nil))
  expectTrue(z5)

  let z6 = isArgOwnedPtrNull()
  expectTrue(z6)
}

DefaultArgTestSuite.test("func with immortal FRT parameter") {
  createFRT()

  let z0 = isArgFRTNull()
  expectTrue(z0)

  let v0 = getArgFRTValue()
  expectEqual(123, v0)

  globalFRT.value = 567
  let v1 = getArgFRTValue()
  expectEqual(567, v1)

  deleteFRT()
}

DefaultArgTestSuite.test("func with ref-counted FRT parameter") {
  let v0 = getArgRefCountedValue()
  expectEqual(321, v0)
}

DefaultArgTestSuite.test("func with const reference parameter") {
  let z0 = isArgZeroConstRef(ArgTy(value: 0))
  expectTrue(z0)
}

DefaultArgTestSuite.test("static method with parameter referring to field") {
  HasStaticMethodWithDefaultArg.value = 0
  let z0 = HasStaticMethodWithDefaultArg.isNonZero()
  expectFalse(z0)

  HasStaticMethodWithDefaultArg.value = 123
  let z1 = HasStaticMethodWithDefaultArg.isNonZero()
  expectTrue(z1)

  HasStaticMethodWithDefaultArg.counter = 0
  let z2 = HasStaticMethodWithDefaultArg.isNonZeroCounter()
  expectFalse(z2)
  let z3 = HasStaticMethodWithDefaultArg.isNonZeroCounter()
  expectTrue(z3)
}

runAllTests()
