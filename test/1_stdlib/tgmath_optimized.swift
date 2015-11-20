// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out -Ounchecked
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

// XFAIL: linux

import Darwin
import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

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
