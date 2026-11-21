// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-Onone)
// RUN: %target-run-simple-swift(-O)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name src %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation
import StdlibUnittest

// CHECK:     enum [[INT_BTE:_AD__[^ ]+branch[^ ]+_spec[^ <]+]] {
// CHECK-NOT:   }
// CHECK:       case bb1((predecessor: {{_AD__[^ ]+}}<Int>, (Float, Float)))
// CHECK:     }
// CHECK:     enum [[STRING_BTE:_AD__[^ ]+branch[^ ]+_spec[^ <]+]] {
// CHECK-NOT:   }
// CHECK:       case bb1((predecessor: {{_AD__[^ ]+}}<String>, (Float, Float)))
// CHECK:     }

// CHECK-DAG:   switch_enum %[[#]], case #[[STRING_BTE]].
// CHECK-DAG:   switch_enum %[[#]], case #[[INT_BTE]].

// The concrete VJP returns a pullback that captures an instantiation of the
// original generic branch tracing enum.
@differentiable(reverse, wrt: x)
@inline(never)
func branch<T>(_ x: Float, _ type: T.Type) -> Float {
  if x > 0 { return x * x }
  return x + x
}

var tests = TestSuite("ADCSGenericBTEContext")
tests.test("InstantiatedBranchTracingEnums") {
  expectEqual(Float(6), gradient(at: Float(3), of: { branch($0, Int.self) }))
  expectEqual(Float(2), gradient(at: Float(-3), of: { branch($0, Int.self) }))
  expectEqual(Float(8), gradient(at: Float(4), of: { branch($0, String.self) }))
  expectEqual(Float(2), gradient(at: Float(-4), of: { branch($0, String.self) }))
}
runAllTests()
