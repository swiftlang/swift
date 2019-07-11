// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(mysimd)) -enable-library-evolution %S/../Inputs/polymorphic_builtins.swift -emit-module -emit-module-path %t/mysimd.swiftmodule -module-name mysimd -parse-stdlib
// RUN: %target-codesign %t/%target-library-name(mysimd)

// RUN: %target-build-swift %s -L %t -I %t -lmysimd -parse-stdlib -Xfrontend -disable-access-control -Xfrontend -sil-verify-all %target-rpath(%t) -o %t/a.out 
// RUN: %target-codesign %t/a.out

// RUN: %target-run %t/a.out %t/%target-library-name(mysimd)

// REQUIRES: executable_test

// End to end test around using polymorphic builtins using a small MYSIMD
// prototype.

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

Tests.test("Indirect Dispatch + Transparent + POD Guard == OK") {
  // TODO: Once we get a, is this call IRGened in a transparent function
  // builtin, this will crash. Once we have that, we should always delegate to
  // the old slow implementation.
  expectCrashLater()

  let inputs: [Int32] = [5,6,7,8]
  let expectedOutputs: [Int32] = inputs.map { $0 &+ $0 }

  let x = mysimd.SIMD4<Int32>(inputs)
  let f = Foo<mysimd.SIMD4<Int32>>()
  let y = callBarAdd(f, x)
  expectEqual(expectedOutputs, y.asArray)
}

// In this case, we call sub which is unguarded and thus crashes.
Tests.test("Indirect Dispatch + Transparent + No POD Guard == Crash") {
  // In this case since we are calling through a vtable, we get a crash. The
  // stdlib maintainer should have guarded this usage with a Builtin.isPOD.
  expectCrashLater()
 
  let inputs: [Int32] = [5,6,7,8]
  let expectedOutputs: [Int32] = inputs.map { $0 &* $0 }

  let x = mysimd.SIMD4<Int32>(inputs)
  let f = Foo<mysimd.SIMD4<Int32>>()
  let y = callBarMul(f, x)
  expectEqual(expectedOutputs, y.asArray)
}

