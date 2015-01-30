// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out -Ounchecked
// RUN: %target-run %t/a.out

// XFAIL: linux

import Darwin
import StdlibUnittest

var TGMathTestSuite = TestSuite("tgmath")

let minusOneDouble = Double(-1.0)
let minusOneFloat = Float(-1.0)

@inline(never)
func minusOneDoubleFunction() -> Double{
  return minusOneDouble
}

@inline(never)
func minusOneFloatFunction() -> Float {
  return minusOneFloat
}

TGMathTestSuite.test("sqrt") {
  expectTrue(isnan(sqrt(minusOneFloat)))
  expectTrue(isnan(sqrt(minusOneFloatFunction())))
  expectTrue(isnan(sqrt(minusOneDouble)))
  expectTrue(isnan(sqrt(minusOneDoubleFunction())))
}

runAllTests()
