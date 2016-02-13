// RUN: rm -rf %t &&mkdir %t
// RUN: cp %s %t/main.swift

// RUN: echo "typealias TestFloat = Float" > %t/float_type.swift
// RUN: %target-build-swift %t/main.swift %t/float_type.swift -o %t/float.out
// RUN: %target-run %t/float.out | FileCheck %s

// RUN: echo "typealias TestFloat = Double" > %t/double_type.swift
// RUN: %target-build-swift %t/main.swift %t/double_type.swift -o %t/double.out
// RUN: %target-run %t/double.out | FileCheck %s
// REQUIRES: executable_test

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
  _require(normal.isNormal)
  _require(normal.isFinite)
  _require(!normal.isZero)
  _require(!normal.isSubnormal)
  _require(!normal.isInfinite)
  _require(!normal.isNaN)
  _require(!normal.isSignaling)
}

func testNormal() {
  let positiveNormal: TestFloat = 42.0
  checkNormal(positiveNormal)
  _require(!positiveNormal.isSignMinus)
  _require(positiveNormal.floatingPointClass == .positiveNormal)

  let negativeNormal: TestFloat = -42.0
  checkNormal(negativeNormal)
  _require(negativeNormal.isSignMinus)
  _require(negativeNormal.floatingPointClass == .negativeNormal)

  _require(positiveNormal == positiveNormal)
  _require(negativeNormal == negativeNormal)
  _require(positiveNormal != negativeNormal)
  _require(negativeNormal != positiveNormal)
  _require(positiveNormal == -negativeNormal)
  _require(negativeNormal == -positiveNormal)

  print("testNormal done")
}
testNormal()
// CHECK: testNormal done

//===---
// Zeroes
//===---

func checkZero(zero: TestFloat) {
  _require(!zero.isNormal)
  _require(zero.isFinite)
  _require(zero.isZero)
  _require(!zero.isSubnormal)
  _require(!zero.isInfinite)
  _require(!zero.isNaN)
  _require(!zero.isSignaling)
}

func testZero() {
  let plusZero = noinlinePlusZero()
  checkZero(plusZero)
  _require(!plusZero.isSignMinus)
  _require(plusZero.floatingPointClass == .positiveZero)

  let minusZero = noinlineMinusZero()
  checkZero(minusZero)
  _require(minusZero.isSignMinus)
  _require(minusZero.floatingPointClass == .negativeZero)

  _require(plusZero == 0.0)
  _require(plusZero == plusZero)
  _require(plusZero == minusZero)
  _require(minusZero == -0.0)
  _require(minusZero == plusZero)
  _require(minusZero == minusZero)

  print("testZero done")
}
testZero()
// CHECK: testZero done

//===---
// Subnormals
//===---

func checkSubnormal(subnormal: TestFloat) {
  _require(!subnormal.isNormal)
  _require(subnormal.isFinite)
  _require(!subnormal.isZero)
  _require(subnormal.isSubnormal)
  _require(!subnormal.isInfinite)
  _require(!subnormal.isNaN)
  _require(!subnormal.isSignaling)
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
      _requirementFailure("unhandled float kind")
  }
  var positiveSubnormal: TestFloat = 1.0
  for var i = 0; i < iterations; i += 1 {
    positiveSubnormal /= 2.0 as TestFloat
  }
  checkSubnormal(positiveSubnormal)
  _require(!positiveSubnormal.isSignMinus)
  _require(positiveSubnormal.floatingPointClass == .positiveSubnormal)
  _require(positiveSubnormal != 0.0)

  var negativeSubnormal: TestFloat = -1.0
  for var i = 0; i < iterations; i += 1{
    negativeSubnormal /= 2.0 as TestFloat
  }
  checkSubnormal(negativeSubnormal)
  _require(negativeSubnormal.isSignMinus)
  _require(negativeSubnormal.floatingPointClass == .negativeSubnormal)
  _require(negativeSubnormal != -0.0)

  print("testSubnormal done")
}

#if arch(arm)
  print("testSubnormal done")
#else
  testSubnormal()
#endif
// CHECK: testSubnormal done

//===---
// Infinities
//===---

func checkInf(inf: TestFloat) {
  _require(!inf.isNormal)
  _require(!inf.isFinite)
  _require(!inf.isZero)
  _require(!inf.isSubnormal)
  _require(inf.isInfinite)
  _require(!inf.isNaN)
  _require(!inf.isSignaling)
}

func testInf() {
  var stdlibPlusInf = TestFloat.infinity
  checkInf(stdlibPlusInf)
  _require(!stdlibPlusInf.isSignMinus)
  _require(stdlibPlusInf.floatingPointClass == .positiveInfinity)

  var stdlibMinusInf = -TestFloat.infinity
  checkInf(stdlibMinusInf)
  _require(stdlibMinusInf.isSignMinus)
  _require(stdlibMinusInf.floatingPointClass == .negativeInfinity)

  var computedPlusInf = 1.0 / noinlinePlusZero()
  checkInf(computedPlusInf)
  _require(!computedPlusInf.isSignMinus)
  _require(computedPlusInf.floatingPointClass == .positiveInfinity)

  var computedMinusInf = -1.0 / noinlinePlusZero()
  checkInf(computedMinusInf)
  _require(computedMinusInf.isSignMinus)
  _require(computedMinusInf.floatingPointClass == .negativeInfinity)

  _require(stdlibPlusInf == computedPlusInf)
  _require(stdlibMinusInf == computedMinusInf)

  _require(stdlibPlusInf != computedMinusInf)
  _require(stdlibMinusInf != computedPlusInf)

  print("testInf done")
}
testInf()
// CHECK: testInf done

//===---
// NaNs
//===---

func checkNaN(nan: TestFloat) {
  _require(!nan.isSignMinus)
  _require(!nan.isNormal)
  _require(!nan.isFinite)
  _require(!nan.isZero)
  _require(!nan.isSubnormal)
  _require(!nan.isInfinite)
  _require(nan.isNaN)
}

func checkQNaN(qnan: TestFloat) {
  checkNaN(qnan)
  _require(!qnan.isSignaling)
  _require(qnan.floatingPointClass == .quietNaN)
}

func testNaN() {
  var stdlibDefaultNaN = TestFloat.NaN
  checkQNaN(stdlibDefaultNaN)

  var stdlibQNaN = TestFloat.quietNaN
  checkQNaN(stdlibQNaN)

  print("testNaN done")
}
testNaN()
// CHECK: testNaN done

print("all done.")
// CHECK: all done.

