// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift

// RUN: echo "typealias TestFloat = Float" > %t/float_type.swift
// RUN: %target-build-swift %t/main.swift %t/float_type.swift -o %t/float.out
// RUN: %target-codesign %t/float.out
// RUN: %target-run %t/float.out

// RUN: echo "typealias TestFloat = Double" > %t/double_type.swift
// RUN: %target-build-swift %t/main.swift %t/double_type.swift -o %t/double.out
// RUN: %target-codesign %t/double.out
// RUN: %target-run %t/double.out
// REQUIRES: executable_test

import StdlibUnittest

var FloatTests = TestSuite("Float")

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
  precondition(normal.isNormal)
  precondition(normal.isFinite)
  precondition(!normal.isZero)
  precondition(!normal.isSubnormal)
  precondition(!normal.isInfinite)
  precondition(!normal.isNaN)
  precondition(!normal.isSignalingNaN)
}

FloatTests.test("normal") {
  let positiveNormal: TestFloat = 42.0
  checkNormal(positiveNormal)
  precondition(positiveNormal.sign == .plus)
  precondition(positiveNormal.floatingPointClass == .positiveNormal)

  let negativeNormal: TestFloat = -42.0
  checkNormal(negativeNormal)
  precondition(negativeNormal.sign == .minus)
  precondition(negativeNormal.floatingPointClass == .negativeNormal)

  precondition(positiveNormal == positiveNormal)
  precondition(negativeNormal == negativeNormal)
  precondition(positiveNormal != negativeNormal)
  precondition(negativeNormal != positiveNormal)
  precondition(positiveNormal == -negativeNormal)
  precondition(negativeNormal == -positiveNormal)
}

//===---
// Zeroes
//===---

func checkZero(_ zero: TestFloat) {
  precondition(!zero.isNormal)
  precondition(zero.isFinite)
  precondition(zero.isZero)
  precondition(!zero.isSubnormal)
  precondition(!zero.isInfinite)
  precondition(!zero.isNaN)
  precondition(!zero.isSignalingNaN)
}

FloatTests.test("zero") {
  let plusZero = noinlinePlusZero()
  checkZero(plusZero)
  precondition(plusZero.sign == .plus)
  precondition(plusZero.floatingPointClass == .positiveZero)

  let minusZero = noinlineMinusZero()
  checkZero(minusZero)
  precondition(minusZero.sign == .minus)
  precondition(minusZero.floatingPointClass == .negativeZero)

  precondition(plusZero == 0.0)
  precondition(plusZero == plusZero)
  precondition(plusZero == minusZero)
  precondition(minusZero == -0.0)
  precondition(minusZero == plusZero)
  precondition(minusZero == minusZero)
}

//===---
// Subnormals
//===---

func checkSubnormal(_ subnormal: TestFloat) {
  precondition(!subnormal.isNormal)
  precondition(subnormal.isFinite)
  precondition(!subnormal.isZero)
  precondition(subnormal.isSubnormal)
  precondition(!subnormal.isInfinite)
  precondition(!subnormal.isNaN)
  precondition(!subnormal.isSignalingNaN)
}

func asUInt64(_ a: UInt64) -> UInt64 {
  return a
}

func asUInt64(_ a: UInt32) -> UInt64 {
  return UInt64(a)
}

#if !arch(arm)
FloatTests.test("subnormal") {
  var iterations: Int
  switch asUInt64(TestFloat.RawSignificand.max) {
    case UInt64.max:
      iterations = 1023
    case asUInt64(UInt32.max):
      iterations = 127
    default:
      preconditionFailure("unhandled float kind")
  }
  var positiveSubnormal: TestFloat = 1.0
  for i in 0 ..< iterations {
    positiveSubnormal /= 2.0 as TestFloat
  }
  checkSubnormal(positiveSubnormal)
  precondition(positiveSubnormal.sign == .plus)
  precondition(positiveSubnormal.floatingPointClass == .positiveSubnormal)
  precondition(positiveSubnormal != 0.0)

  var negativeSubnormal: TestFloat = -1.0
  for i in 0 ..< iterations {
    negativeSubnormal /= 2.0 as TestFloat
  }
  checkSubnormal(negativeSubnormal)
  precondition(negativeSubnormal.sign == .minus)
  precondition(negativeSubnormal.floatingPointClass == .negativeSubnormal)
  precondition(negativeSubnormal != -0.0)
}
#endif

//===---
// Infinities
//===---

func checkInf(_ inf: TestFloat) {
  precondition(!inf.isNormal)
  precondition(!inf.isFinite)
  precondition(!inf.isZero)
  precondition(!inf.isSubnormal)
  precondition(inf.isInfinite)
  precondition(!inf.isNaN)
  precondition(!inf.isSignalingNaN)
}

FloatTests.test("infinity") {
  var stdlibPlusInf = TestFloat.infinity
  checkInf(stdlibPlusInf)
  precondition(stdlibPlusInf.sign == .plus)
  precondition(stdlibPlusInf.floatingPointClass == .positiveInfinity)

  var stdlibMinusInf = -TestFloat.infinity
  checkInf(stdlibMinusInf)
  precondition(stdlibMinusInf.sign == .minus)
  precondition(stdlibMinusInf.floatingPointClass == .negativeInfinity)

  var computedPlusInf = 1.0 / noinlinePlusZero()
  checkInf(computedPlusInf)
  precondition(computedPlusInf.sign == .plus)
  precondition(computedPlusInf.floatingPointClass == .positiveInfinity)

  var computedMinusInf = -1.0 / noinlinePlusZero()
  checkInf(computedMinusInf)
  precondition(computedMinusInf.sign == .minus)
  precondition(computedMinusInf.floatingPointClass == .negativeInfinity)

  precondition(stdlibPlusInf == computedPlusInf)
  precondition(stdlibMinusInf == computedMinusInf)

  precondition(stdlibPlusInf != computedMinusInf)
  precondition(stdlibMinusInf != computedPlusInf)
}

//===---
// NaNs
//===---

func checkNaN(_ nan: TestFloat) {
  precondition(nan.sign == .plus)
  precondition(!nan.isNormal)
  precondition(!nan.isFinite)
  precondition(!nan.isZero)
  precondition(!nan.isSubnormal)
  precondition(!nan.isInfinite)
  precondition(nan.isNaN)
}

func checkQNaN(_ qnan: TestFloat) {
  checkNaN(qnan)
  precondition(!qnan.isSignalingNaN)
  precondition(qnan.floatingPointClass == .quietNaN)
}

func checkSNaN(_ snan: TestFloat) {
  checkNaN(snan)
// sNaN cannot be fully supported on i386.
#if !arch(i386)
  precondition(snan.isSignalingNaN)
  precondition(snan.floatingPointClass == .signalingNaN)
#endif
}

FloatTests.test("nan") {
  var stdlibDefaultNaN = TestFloat.nan
  checkQNaN(stdlibDefaultNaN)

  var stdlibQNaN = TestFloat.nan
  checkQNaN(stdlibQNaN)

  var stdlibSNaN = TestFloat.signalingNaN
  checkSNaN(stdlibSNaN)
}

runAllTests()

