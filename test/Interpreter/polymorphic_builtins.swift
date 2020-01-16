// This is an end-to-end test case of a speculative design for SIMD types using
// polymorphic builtins. It is just intended to exercise the functional intended
// behavior of _isConcrete + polymorphic builtins and that this functionality
// keeps on working. It is not intended to push any future simd designs in any
// direction, just validate that this speculative design keeps working as
// expected.

// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(mysimd)) -enable-library-evolution %S/../Inputs/polymorphic_builtins.swift -emit-module -emit-module-path %t/mysimd.swiftmodule -module-name mysimd -parse-stdlib
// RUN: %target-codesign %t/%target-library-name(mysimd)

// RUN: %target-build-swift %s -L %t -I %t -lmysimd -parse-stdlib -Xfrontend -disable-access-control -Xfrontend -sil-verify-all %target-rpath(%t) -o %t/a.out
// RUN: %target-codesign %t/a.out

// RUN: %target-run %t/a.out %t/%target-library-name(mysimd)

// REQUIRES: executable_test

// TODO: Re-enable this when optimization is enabled.
//
// REQUIRES: swift_test_mode_optimize_none

import Swift
import StdlibUnittest
import mysimd

// Defer running all of the tests to end of file.
defer { runAllTests() }

// Implement the full specialization scheme, checking where we crash.
var Tests = TestSuite("Polymorphic Builtins")

Tests.test("Direct Dispatch + Transparent => Get right result") {
  let inputs: [Int32] = [5,6,7,8]
  let expectedOutputs: [Int32] = inputs.map { $0 &+ $0 }

  let x = mysimd.SIMD4<Int32>(inputs)
  let y = x &+ x

  expectEqual(expectedOutputs, y.asArray)
}

public protocol Bar {
  associatedtype SIMDTy : SIMD
  func callAddGuarded(_ t: SIMDTy) -> SIMDTy
  func callMulUnguarded(_ t: SIMDTy) -> SIMDTy
}

public struct Foo<T : SIMD> where T.Scalar : FixedWidthInteger {
}

extension Foo : Bar {
  typealias SIMDTy = T

  @_transparent
  func callAddGuarded(_ t: T) -> T {
    return t &+ t
  }

  @_transparent
  func callMulUnguarded(_ t: T) -> T {
    return t &* t
  }
}

func callBarAdd<T: Bar, U : SIMD>(_ t: T, _ u: U) -> U where T.SIMDTy == U {
  return t.callAddGuarded(u)
}

func callBarMul<T: Bar, U : SIMD>(_ t: T, _ u: U) -> U where T.SIMDTy == U {
  return t.callMulUnguarded(u)
}

// In this case, we have guarded inside the add function using _isConcrete. So
// we should go down the slow path even though we are calling through a vtable
// (and thus are going along the generic code path).
Tests.test("Indirect Dispatch + Transparent + _isConcrete Guard == OK") {
  let inputs: [Int32] = [5,6,7,8]
  let expectedOutputs: [Int32] = inputs.map { $0 &+ $0 }

  let x = mysimd.SIMD4<Int32>(inputs)
  let f = Foo<mysimd.SIMD4<Int32>>()
  let y = callBarAdd(f, x)
  expectEqual(expectedOutputs, y.asArray)
}

// In this case, we call mul which is unguarded with _isConcrete and thus
// crashes. The stdlib maintainer should have guarded this usage to ensure that
// we did not go down this code path.

Tests.test("Indirect Dispatch + Transparent + No _isConcrete Guard == Crash") {
  expectCrashLater()

  let inputs: [Int32] = [5,6,7,8]
  let expectedOutputs: [Int32] = inputs.map { $0 &* $0 }

  let x = mysimd.SIMD4<Int32>(inputs)
  let f = Foo<mysimd.SIMD4<Int32>>()
  let y = callBarMul(f, x)
  expectEqual(expectedOutputs, y.asArray)
}
