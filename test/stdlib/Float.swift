// RUN: rm -rf %t
// RUN: mkdir %t
//
// RUN: echo "typealias TestFloat = Float" > %t/float_type.swift
// RUN: %swift -I %t -i %s | FileCheck %s
//
// RUN: echo "typealias TestFloat = Double" > %t/float_type.swift
// RUN: %swift -I %t -i %s | FileCheck %s
//
// REQUIRES: swift_interpreter

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

func noinlineSNaN() -> TestFloat {
  return TestFloat.signalingNaN()
}

//===---
// Normals
//===---

func checkNormal(normal: TestFloat) {
  alwaysTrap(normal.isNormal())
  alwaysTrap(normal.isFinite())
  alwaysTrap(!normal.isZero())
  alwaysTrap(!normal.isSubnormal())
  alwaysTrap(!normal.isInfinite())
  alwaysTrap(!normal.isNaN())
  alwaysTrap(!normal.isSignaling())
}

func testNormal() {
  var positiveNormal: TestFloat = 42.0
  checkNormal(positiveNormal)
  alwaysTrap(!positiveNormal.isSignMinus())
  alwaysTrap(positiveNormal.floatingPointClass == .PositiveNormal)

  var negativeNormal: TestFloat = -42.0
  checkNormal(negativeNormal)
  alwaysTrap(negativeNormal.isSignMinus())
  alwaysTrap(negativeNormal.floatingPointClass == .NegativeNormal)

  alwaysTrap(positiveNormal == positiveNormal)
  alwaysTrap(negativeNormal == negativeNormal)
  alwaysTrap(positiveNormal != negativeNormal)
  alwaysTrap(negativeNormal != positiveNormal)
  alwaysTrap(positiveNormal == -negativeNormal)
  alwaysTrap(negativeNormal == -positiveNormal)

  println("testNormal done")
}
testNormal()
// CHECK: testNormal done

//===---
// Zeroes
//===---

func checkZero(zero: TestFloat) {
  alwaysTrap(!zero.isNormal())
  alwaysTrap(zero.isFinite())
  alwaysTrap(zero.isZero())
  alwaysTrap(!zero.isSubnormal())
  alwaysTrap(!zero.isInfinite())
  alwaysTrap(!zero.isNaN())
  alwaysTrap(!zero.isSignaling())
}

func testZero() {
  var plusZero = noinlinePlusZero()
  checkZero(plusZero)
  alwaysTrap(!plusZero.isSignMinus())
  alwaysTrap(plusZero.floatingPointClass == .PositiveZero)

  var minusZero = noinlineMinusZero()
  checkZero(minusZero)
  alwaysTrap(minusZero.isSignMinus())
  alwaysTrap(minusZero.floatingPointClass == .NegativeZero)

  alwaysTrap(plusZero == 0.0)
  alwaysTrap(plusZero == plusZero)
  alwaysTrap(plusZero == minusZero)
  alwaysTrap(minusZero == -0.0)
  alwaysTrap(minusZero == plusZero)
  alwaysTrap(minusZero == minusZero)

  println("testZero done")
}
testZero()
// CHECK: testZero done

//===---
// Subnormals
//===---

func checkSubnormal(subnormal: TestFloat) {
  alwaysTrap(!subnormal.isNormal())
  alwaysTrap(subnormal.isFinite())
  alwaysTrap(!subnormal.isZero())
  alwaysTrap(subnormal.isSubnormal())
  alwaysTrap(!subnormal.isInfinite())
  alwaysTrap(!subnormal.isNaN())
  alwaysTrap(!subnormal.isSignaling())
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
      alwaysTrap("unhandled float kind")
  }
  var positiveSubnormal: TestFloat = 1.0
  for var i = 0; i < iterations; i++ {
    positiveSubnormal /= 2.0 as TestFloat
  }
  checkSubnormal(positiveSubnormal)
  alwaysTrap(!positiveSubnormal.isSignMinus())
  alwaysTrap(positiveSubnormal.floatingPointClass == .PositiveSubnormal)
  alwaysTrap(positiveSubnormal != 0.0)

  var negativeSubnormal: TestFloat = -1.0
  for var i = 0; i < iterations; i++ {
    negativeSubnormal /= 2.0 as TestFloat
  }
  checkSubnormal(negativeSubnormal)
  alwaysTrap(negativeSubnormal.isSignMinus())
  alwaysTrap(negativeSubnormal.floatingPointClass == .NegativeSubnormal)
  alwaysTrap(negativeSubnormal != -0.0)

  println("testSubnormal done")
}
testSubnormal()
// CHECK: testSubnormal done

//===---
// Infinities
//===---

func checkInf(inf: TestFloat) {
  alwaysTrap(!inf.isNormal())
  alwaysTrap(!inf.isFinite())
  alwaysTrap(!inf.isZero())
  alwaysTrap(!inf.isSubnormal())
  alwaysTrap(inf.isInfinite())
  alwaysTrap(!inf.isNaN())
  alwaysTrap(!inf.isSignaling())
}

func testInf() {
  var stdlibPlusInf = TestFloat.inf()
  checkInf(stdlibPlusInf)
  alwaysTrap(!stdlibPlusInf.isSignMinus())
  alwaysTrap(stdlibPlusInf.floatingPointClass == .PositiveInfinity)

  var stdlibMinusInf = -TestFloat.inf()
  checkInf(stdlibMinusInf)
  alwaysTrap(stdlibMinusInf.isSignMinus())
  alwaysTrap(stdlibMinusInf.floatingPointClass == .NegativeInfinity)

  var computedPlusInf = 1.0 / noinlinePlusZero()
  checkInf(computedPlusInf)
  alwaysTrap(!computedPlusInf.isSignMinus())
  alwaysTrap(computedPlusInf.floatingPointClass == .PositiveInfinity)

  var computedMinusInf = -1.0 / noinlinePlusZero()
  checkInf(computedMinusInf)
  alwaysTrap(computedMinusInf.isSignMinus())
  alwaysTrap(computedMinusInf.floatingPointClass == .NegativeInfinity)

  alwaysTrap(stdlibPlusInf == computedPlusInf)
  alwaysTrap(stdlibMinusInf == computedMinusInf)

  alwaysTrap(stdlibPlusInf != computedMinusInf)
  alwaysTrap(stdlibMinusInf != computedPlusInf)

  println("testInf done")
}
testInf()
// CHECK: testInf done

//===---
// NaNs
//===---

func checkNaN(nan: TestFloat) {
  alwaysTrap(!nan.isSignMinus())
  alwaysTrap(!nan.isNormal())
  alwaysTrap(!nan.isFinite())
  alwaysTrap(!nan.isZero())
  alwaysTrap(!nan.isSubnormal())
  alwaysTrap(!nan.isInfinite())
  alwaysTrap(nan.isNaN())
}

func checkQNaN(qnan: TestFloat) {
  checkNaN(qnan)
  alwaysTrap(!qnan.isSignaling())
  alwaysTrap(qnan.floatingPointClass == .QuietNaN)
}

func checkSNaN(snan: TestFloat) {
  checkNaN(snan)
  alwaysTrap(snan.isSignaling())
  alwaysTrap(snan.floatingPointClass == .SignalingNaN)
}

func testNaN() {
  var stdlibDefaultNaN = TestFloat.NaN()
  checkQNaN(stdlibDefaultNaN)
  alwaysTrap(stdlibDefaultNaN != stdlibDefaultNaN)

  var stdlibQNaN = TestFloat.quietNaN()
  checkQNaN(stdlibQNaN)
  alwaysTrap(stdlibQNaN != stdlibQNaN)

  var stdlibSNaN = noinlineSNaN()
  checkSNaN(stdlibSNaN)
  alwaysTrap(stdlibSNaN != stdlibSNaN)

  feclearexcept(FE_ALL_EXCEPT)
  var previousExceptions = fetestexcept(FE_ALL_EXCEPT)
  println("\(Float.signalingNaN() + 0.0)")
  alwaysTrap(fetestexcept(FE_ALL_EXCEPT) != previousExceptions)

  feclearexcept(FE_ALL_EXCEPT)
  checkSNaN(stdlibSNaN)
  alwaysTrap(fetestexcept(FE_ALL_EXCEPT) == previousExceptions)

  println("testNaN done")
}
testNaN()
// CHECK: testNaN done

println("all done.")
// CHECK: all done.

