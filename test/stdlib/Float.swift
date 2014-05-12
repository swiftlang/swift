// RUN: rm -rf %t
// RUN: mkdir %t
//
// RUN: echo "typealias TestFloat = Float" > %t/float_type.swift
// RUN: %target-build-swift %s -I %t -Xfrontend -enable-source-import -o %t/float.out
// RUN: %target-run %t/float.out | FileCheck %s

// RUN: echo "typealias TestFloat = Double" > %t/float_type.swift
// RUN: %target-build-swift %s -I %t -Xfrontend -enable-source-import -o %t/double.out
// RUN: %target-run %t/double.out | FileCheck %s

import Darwin
import float_type

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
  securityCheck(normal.isNormal())
  securityCheck(normal.isFinite())
  securityCheck(!normal.isZero())
  securityCheck(!normal.isSubnormal())
  securityCheck(!normal.isInfinite())
  securityCheck(!normal.isNaN())
  securityCheck(!normal.isSignaling())
}

func testNormal() {
  var positiveNormal: TestFloat = 42.0
  checkNormal(positiveNormal)
  securityCheck(!positiveNormal.isSignMinus())
  securityCheck(positiveNormal.floatingPointClass == .PositiveNormal)

  var negativeNormal: TestFloat = -42.0
  checkNormal(negativeNormal)
  securityCheck(negativeNormal.isSignMinus())
  securityCheck(negativeNormal.floatingPointClass == .NegativeNormal)

  securityCheck(positiveNormal == positiveNormal)
  securityCheck(negativeNormal == negativeNormal)
  securityCheck(positiveNormal != negativeNormal)
  securityCheck(negativeNormal != positiveNormal)
  securityCheck(positiveNormal == -negativeNormal)
  securityCheck(negativeNormal == -positiveNormal)

  println("testNormal done")
}
testNormal()
// CHECK: testNormal done

//===---
// Zeroes
//===---

func checkZero(zero: TestFloat) {
  securityCheck(!zero.isNormal())
  securityCheck(zero.isFinite())
  securityCheck(zero.isZero())
  securityCheck(!zero.isSubnormal())
  securityCheck(!zero.isInfinite())
  securityCheck(!zero.isNaN())
  securityCheck(!zero.isSignaling())
}

func testZero() {
  var plusZero = noinlinePlusZero()
  checkZero(plusZero)
  securityCheck(!plusZero.isSignMinus())
  securityCheck(plusZero.floatingPointClass == .PositiveZero)

  var minusZero = noinlineMinusZero()
  checkZero(minusZero)
  securityCheck(minusZero.isSignMinus())
  securityCheck(minusZero.floatingPointClass == .NegativeZero)

  securityCheck(plusZero == 0.0)
  securityCheck(plusZero == plusZero)
  securityCheck(plusZero == minusZero)
  securityCheck(minusZero == -0.0)
  securityCheck(minusZero == plusZero)
  securityCheck(minusZero == minusZero)

  println("testZero done")
}
testZero()
// CHECK: testZero done

//===---
// Subnormals
//===---

func checkSubnormal(subnormal: TestFloat) {
  securityCheck(!subnormal.isNormal())
  securityCheck(subnormal.isFinite())
  securityCheck(!subnormal.isZero())
  securityCheck(subnormal.isSubnormal())
  securityCheck(!subnormal.isInfinite())
  securityCheck(!subnormal.isNaN())
  securityCheck(!subnormal.isSignaling())
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
      fatal("unhandled float kind")
  }
  var positiveSubnormal: TestFloat = 1.0
  for var i = 0; i < iterations; i++ {
    positiveSubnormal /= 2.0 as TestFloat
  }
  checkSubnormal(positiveSubnormal)
  securityCheck(!positiveSubnormal.isSignMinus())
  securityCheck(positiveSubnormal.floatingPointClass == .PositiveSubnormal)
  securityCheck(positiveSubnormal != 0.0)

  var negativeSubnormal: TestFloat = -1.0
  for var i = 0; i < iterations; i++ {
    negativeSubnormal /= 2.0 as TestFloat
  }
  checkSubnormal(negativeSubnormal)
  securityCheck(negativeSubnormal.isSignMinus())
  securityCheck(negativeSubnormal.floatingPointClass == .NegativeSubnormal)
  securityCheck(negativeSubnormal != -0.0)

  println("testSubnormal done")
}
testSubnormal()
// CHECK: testSubnormal done

//===---
// Infinities
//===---

func checkInf(inf: TestFloat) {
  securityCheck(!inf.isNormal())
  securityCheck(!inf.isFinite())
  securityCheck(!inf.isZero())
  securityCheck(!inf.isSubnormal())
  securityCheck(inf.isInfinite())
  securityCheck(!inf.isNaN())
  securityCheck(!inf.isSignaling())
}

func testInf() {
  var stdlibPlusInf = TestFloat.inf()
  checkInf(stdlibPlusInf)
  securityCheck(!stdlibPlusInf.isSignMinus())
  securityCheck(stdlibPlusInf.floatingPointClass == .PositiveInfinity)

  var stdlibMinusInf = -TestFloat.inf()
  checkInf(stdlibMinusInf)
  securityCheck(stdlibMinusInf.isSignMinus())
  securityCheck(stdlibMinusInf.floatingPointClass == .NegativeInfinity)

  var computedPlusInf = 1.0 / noinlinePlusZero()
  checkInf(computedPlusInf)
  securityCheck(!computedPlusInf.isSignMinus())
  securityCheck(computedPlusInf.floatingPointClass == .PositiveInfinity)

  var computedMinusInf = -1.0 / noinlinePlusZero()
  checkInf(computedMinusInf)
  securityCheck(computedMinusInf.isSignMinus())
  securityCheck(computedMinusInf.floatingPointClass == .NegativeInfinity)

  securityCheck(stdlibPlusInf == computedPlusInf)
  securityCheck(stdlibMinusInf == computedMinusInf)

  securityCheck(stdlibPlusInf != computedMinusInf)
  securityCheck(stdlibMinusInf != computedPlusInf)

  println("testInf done")
}
testInf()
// CHECK: testInf done

//===---
// NaNs
//===---

func checkNaN(nan: TestFloat) {
  securityCheck(!nan.isSignMinus())
  securityCheck(!nan.isNormal())
  securityCheck(!nan.isFinite())
  securityCheck(!nan.isZero())
  securityCheck(!nan.isSubnormal())
  securityCheck(!nan.isInfinite())
  securityCheck(nan.isNaN())
}

func checkQNaN(qnan: TestFloat) {
  checkNaN(qnan)
  securityCheck(!qnan.isSignaling())
  securityCheck(qnan.floatingPointClass == .QuietNaN)
}

func testNaN() {
  var stdlibDefaultNaN = TestFloat.NaN()
  checkQNaN(stdlibDefaultNaN)
  securityCheck(stdlibDefaultNaN != stdlibDefaultNaN)

  var stdlibQNaN = TestFloat.quietNaN()
  checkQNaN(stdlibQNaN)
  securityCheck(stdlibQNaN != stdlibQNaN)

  println("testNaN done")
}
testNaN()
// CHECK: testNaN done

println("all done.")
// CHECK: all done.

