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

func checkNormal(_ normal: TestFloat) {
  _precondition(normal.isNormal)
  _precondition(normal.isFinite)
  _precondition(!normal.isZero)
  _precondition(!normal.isSubnormal)
  _precondition(!normal.isInfinite)
  _precondition(!normal.isNaN)
  _precondition(!normal.isSignalingNaN)
}

func testNormal() {
  let positiveNormal: TestFloat = 42.0
  checkNormal(positiveNormal)
  _precondition(positiveNormal.sign == .plus)
  _precondition(positiveNormal.floatingPointClass == .positiveNormal)

  let negativeNormal: TestFloat = -42.0
  checkNormal(negativeNormal)
  _precondition(negativeNormal.sign == .minus)
  _precondition(negativeNormal.floatingPointClass == .negativeNormal)

  _precondition(positiveNormal == positiveNormal)
  _precondition(negativeNormal == negativeNormal)
  _precondition(positiveNormal != negativeNormal)
  _precondition(negativeNormal != positiveNormal)
  _precondition(positiveNormal == -negativeNormal)
  _precondition(negativeNormal == -positiveNormal)

  print("testNormal done")
}
testNormal()
// CHECK: testNormal done

//===---
// Zeroes
//===---

func checkZero(_ zero: TestFloat) {
  _precondition(!zero.isNormal)
  _precondition(zero.isFinite)
  _precondition(zero.isZero)
  _precondition(!zero.isSubnormal)
  _precondition(!zero.isInfinite)
  _precondition(!zero.isNaN)
  _precondition(!zero.isSignalingNaN)
}

func testZero() {
  let plusZero = noinlinePlusZero()
  checkZero(plusZero)
  _precondition(plusZero.sign == .plus)
  _precondition(plusZero.floatingPointClass == .positiveZero)

  let minusZero = noinlineMinusZero()
  checkZero(minusZero)
  _precondition(minusZero.sign == .minus)
  _precondition(minusZero.floatingPointClass == .negativeZero)

  _precondition(plusZero == 0.0)
  _precondition(plusZero == plusZero)
  _precondition(plusZero == minusZero)
  _precondition(minusZero == -0.0)
  _precondition(minusZero == plusZero)
  _precondition(minusZero == minusZero)

  print("testZero done")
}
testZero()
// CHECK: testZero done

//===---
// Subnormals
//===---

func checkSubnormal(_ subnormal: TestFloat) {
  _precondition(!subnormal.isNormal)
  _precondition(subnormal.isFinite)
  _precondition(!subnormal.isZero)
  _precondition(subnormal.isSubnormal)
  _precondition(!subnormal.isInfinite)
  _precondition(!subnormal.isNaN)
  _precondition(!subnormal.isSignalingNaN)
}

func asUInt64(_ a: UInt64) -> UInt64 {
  return a
}

func asUInt64(_ a: UInt32) -> UInt64 {
  return UInt64(a)
}

func testSubnormal() {
  var iterations: Int
  switch asUInt64(TestFloat.RawSignificand.max) {
    case UInt64.max:
      iterations = 1023
    case asUInt64(UInt32.max):
      iterations = 127
    default:
      _preconditionFailure("unhandled float kind")
  }
  var positiveSubnormal: TestFloat = 1.0
  for i in 0 ..< iterations {
    positiveSubnormal /= 2.0 as TestFloat
  }
  checkSubnormal(positiveSubnormal)
  _precondition(positiveSubnormal.sign == .plus)
  _precondition(positiveSubnormal.floatingPointClass == .positiveSubnormal)
  _precondition(positiveSubnormal != 0.0)

  var negativeSubnormal: TestFloat = -1.0
  for i in 0 ..< iterations {
    negativeSubnormal /= 2.0 as TestFloat
  }
  checkSubnormal(negativeSubnormal)
  _precondition(negativeSubnormal.sign == .minus)
  _precondition(negativeSubnormal.floatingPointClass == .negativeSubnormal)
  _precondition(negativeSubnormal != -0.0)

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

func checkInf(_ inf: TestFloat) {
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
  _precondition(stdlibPlusInf.floatingPointClass == .positiveInfinity)

  var stdlibMinusInf = -TestFloat.infinity
  checkInf(stdlibMinusInf)
  _precondition(stdlibMinusInf.isSignMinus)
  _precondition(stdlibMinusInf.floatingPointClass == .negativeInfinity)

  var computedPlusInf = 1.0 / noinlinePlusZero()
  checkInf(computedPlusInf)
  _precondition(!computedPlusInf.isSignMinus)
  _precondition(computedPlusInf.floatingPointClass == .positiveInfinity)

  var computedMinusInf = -1.0 / noinlinePlusZero()
  checkInf(computedMinusInf)
  _precondition(computedMinusInf.isSignMinus)
  _precondition(computedMinusInf.floatingPointClass == .negativeInfinity)

  _precondition(stdlibPlusInf == computedPlusInf)
  _precondition(stdlibMinusInf == computedMinusInf)

  _precondition(stdlibPlusInf != computedMinusInf)
  _precondition(stdlibMinusInf != computedPlusInf)

  print("testInf done")
}
testInf()
// CHECK: testInf done

//===---
// NaNs
//===---

func checkNaN(_ nan: TestFloat) {
  _precondition(!nan.isSignMinus)
  _precondition(!nan.isNormal)
  _precondition(!nan.isFinite)
  _precondition(!nan.isZero)
  _precondition(!nan.isSubnormal)
  _precondition(!nan.isInfinite)
  _precondition(nan.isNaN)
}

func checkQNaN(_ qnan: TestFloat) {
  checkNaN(qnan)
  _precondition(!qnan.isSignaling)
  _precondition(qnan.floatingPointClass == .quietNaN)
}

func checkSNaN(_ snan: TestFloat) {
  checkNaN(snan)
// sNaN cannot be fully supported on i386.
#if !arch(i386)
  _precondition(snan.isSignaling)
  _precondition(snan.floatingPointClass == .signalingNaN)
#endif
}

func testNaN() {
  var stdlibDefaultNaN = TestFloat.nan
  checkQNaN(stdlibDefaultNaN)

  var stdlibQNaN = TestFloat.quietNaN
  checkQNaN(stdlibQNaN)

  var stdlibSNaN = TestFloat.signalingNaN
  checkSNaN(stdlibSNaN)
  print("testNaN done")
}
testNaN()
// CHECK: testNaN done

print("all done.")
// CHECK: all done.

