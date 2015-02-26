// RUN: rm -rf %t &&mkdir %t
// RUN: cp %s %t/main.swift

// RUN: echo "typealias TestFloat = Float" > %t/float_type.swift
// RUN: %target-build-swift %t/main.swift %t/float_type.swift -o %t/float.out
// RUN: %target-run %t/float.out | FileCheck %s

// RUN: echo "typealias TestFloat = Double" > %t/double_type.swift
// RUN: %target-build-swift %t/main.swift %t/double_type.swift -o %t/double.out
// RUN: %target-run %t/double.out | FileCheck %s

// XFAIL: linux

import Darwin

//===---
// Helpers
//===---

func noinlinePlusZero() -> TestFloat {
  return 0.0
}

func noinlineMinusZero() -> TestFloat {
  return -0.0
}

//===---
// Normals
//===---

func checkNormal(normal: TestFloat) {
  _precondition(normal.isNormal)
  _precondition(normal.isFinite)
  _precondition(!normal.isZero)
  _precondition(!normal.isSubnormal)
  _precondition(!normal.isInfinite)
  _precondition(!normal.isNaN)
  _precondition(!normal.isSignaling)
}

func testNormal() {
  var positiveNormal: TestFloat = 42.0
  checkNormal(positiveNormal)
  _precondition(!positiveNormal.isSignMinus)
  _precondition(positiveNormal.floatingPointClass == .PositiveNormal)

  var negativeNormal: TestFloat = -42.0
  checkNormal(negativeNormal)
  _precondition(negativeNormal.isSignMinus)
  _precondition(negativeNormal.floatingPointClass == .NegativeNormal)

  _precondition(positiveNormal == positiveNormal)
  _precondition(negativeNormal == negativeNormal)
  _precondition(positiveNormal != negativeNormal)
  _precondition(negativeNormal != positiveNormal)
  _precondition(positiveNormal == -negativeNormal)
  _precondition(negativeNormal == -positiveNormal)

  println("testNormal done")
}
testNormal()
// CHECK: testNormal done

//===---
// Zeroes
//===---

func checkZero(zero: TestFloat) {
  _precondition(!zero.isNormal)
  _precondition(zero.isFinite)
  _precondition(zero.isZero)
  _precondition(!zero.isSubnormal)
  _precondition(!zero.isInfinite)
  _precondition(!zero.isNaN)
  _precondition(!zero.isSignaling)
}

func testZero() {
  var plusZero = noinlinePlusZero()
  checkZero(plusZero)
  _precondition(!plusZero.isSignMinus)
  _precondition(plusZero.floatingPointClass == .PositiveZero)

  var minusZero = noinlineMinusZero()
  checkZero(minusZero)
  _precondition(minusZero.isSignMinus)
  _precondition(minusZero.floatingPointClass == .NegativeZero)

  _precondition(plusZero == 0.0)
  _precondition(plusZero == plusZero)
  _precondition(plusZero == minusZero)
  _precondition(minusZero == -0.0)
  _precondition(minusZero == plusZero)
  _precondition(minusZero == minusZero)

  println("testZero done")
}
testZero()
// CHECK: testZero done

//===---
// Subnormals
//===---

func checkSubnormal(subnormal: TestFloat) {
  _precondition(!subnormal.isNormal)
  _precondition(subnormal.isFinite)
  _precondition(!subnormal.isZero)
  _precondition(subnormal.isSubnormal)
  _precondition(!subnormal.isInfinite)
  _precondition(!subnormal.isNaN)
  _precondition(!subnormal.isSignaling)
}

func asUInt64(a: UInt64) -> UInt64 {
  return a
}

func asUInt64(a: UInt32) -> UInt64 {
  return UInt64(a)
}

func testSubnormal() {
  var iterations: Int
  switch asUInt64(TestFloat._BitsType.max) {
    case UInt64.max:
      iterations = 1023
    case asUInt64(UInt32.max):
      iterations = 127
    default:
      _preconditionFailure("unhandled float kind")
  }
  var positiveSubnormal: TestFloat = 1.0
  for var i = 0; i < iterations; i++ {
    positiveSubnormal /= 2.0 as TestFloat
  }
  checkSubnormal(positiveSubnormal)
  _precondition(!positiveSubnormal.isSignMinus)
  _precondition(positiveSubnormal.floatingPointClass == .PositiveSubnormal)
  _precondition(positiveSubnormal != 0.0)

  var negativeSubnormal: TestFloat = -1.0
  for var i = 0; i < iterations; i++ {
    negativeSubnormal /= 2.0 as TestFloat
  }
  checkSubnormal(negativeSubnormal)
  _precondition(negativeSubnormal.isSignMinus)
  _precondition(negativeSubnormal.floatingPointClass == .NegativeSubnormal)
  _precondition(negativeSubnormal != -0.0)

  println("testSubnormal done")
}
testSubnormal()
// CHECK: testSubnormal done

//===---
// Infinities
//===---

func checkInf(inf: TestFloat) {
  _precondition(!inf.isNormal)
  _precondition(!inf.isFinite)
  _precondition(!inf.isZero)
  _precondition(!inf.isSubnormal)
  _precondition(inf.isInfinite)
  _precondition(!inf.isNaN)
  _precondition(!inf.isSignaling)
}

func testInf() {
  var stdlibPlusInf = TestFloat.infinity
  checkInf(stdlibPlusInf)
  _precondition(!stdlibPlusInf.isSignMinus)
  _precondition(stdlibPlusInf.floatingPointClass == .PositiveInfinity)

  var stdlibMinusInf = -TestFloat.infinity
  checkInf(stdlibMinusInf)
  _precondition(stdlibMinusInf.isSignMinus)
  _precondition(stdlibMinusInf.floatingPointClass == .NegativeInfinity)

  var computedPlusInf = 1.0 / noinlinePlusZero()
  checkInf(computedPlusInf)
  _precondition(!computedPlusInf.isSignMinus)
  _precondition(computedPlusInf.floatingPointClass == .PositiveInfinity)

  var computedMinusInf = -1.0 / noinlinePlusZero()
  checkInf(computedMinusInf)
  _precondition(computedMinusInf.isSignMinus)
  _precondition(computedMinusInf.floatingPointClass == .NegativeInfinity)

  _precondition(stdlibPlusInf == computedPlusInf)
  _precondition(stdlibMinusInf == computedMinusInf)

  _precondition(stdlibPlusInf != computedMinusInf)
  _precondition(stdlibMinusInf != computedPlusInf)

  println("testInf done")
}
testInf()
// CHECK: testInf done

//===---
// NaNs
//===---

func checkNaN(nan: TestFloat) {
  _precondition(!nan.isSignMinus)
  _precondition(!nan.isNormal)
  _precondition(!nan.isFinite)
  _precondition(!nan.isZero)
  _precondition(!nan.isSubnormal)
  _precondition(!nan.isInfinite)
  _precondition(nan.isNaN)
}

func checkQNaN(qnan: TestFloat) {
  checkNaN(qnan)
  _precondition(!qnan.isSignaling)
  _precondition(qnan.floatingPointClass == .QuietNaN)
}

func testNaN() {
  var stdlibDefaultNaN = TestFloat.NaN
  checkQNaN(stdlibDefaultNaN)

  var stdlibQNaN = TestFloat.quietNaN
  checkQNaN(stdlibQNaN)

  println("testNaN done")
}
testNaN()
// CHECK: testNaN done

println("all done.")
// CHECK: all done.

