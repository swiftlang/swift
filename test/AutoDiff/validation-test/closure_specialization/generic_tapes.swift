// REQUIRES: executable_test
// RUN: %target-run-simple-swift
// RUN: %target-run-simple-swift(-O)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name src %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation
import StdlibUnittest

var tests = TestSuite("GenericTapes")

// MARK: - Concrete instantiations of a generic branch tape

// CHECK-LABEL:     {{^}}enum _AD__$s3src6branchyS2f_xmtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_c861720d29dca92d6bf6c68c23b46431 {
// CHECK-NEXT:        case bb2((predecessor: _AD__$s3src6branchyS2f_xmtlF_bb2__Pred__src_0_wrt_0_l<Int>, ()))
// CHECK:             case bb1((predecessor: _AD__$s3src6branchyS2f_xmtlF_bb1__Pred__src_0_wrt_0_l<Int>, (Float, Float)))
// CHECK-LABEL:     {{^}}}
// CHECK-LABEL:     {{^}}enum _AD__$s3src6branchyS2f_xmtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_45186266a52d34461f2256dfc117e4be {
// CHECK-NEXT:        case bb2((predecessor: _AD__$s3src6branchyS2f_xmtlF_bb2__Pred__src_0_wrt_0_l<String>, ()))
// CHECK:             case bb1((predecessor: _AD__$s3src6branchyS2f_xmtlF_bb1__Pred__src_0_wrt_0_l<String>, (Float, Float)))
// CHECK-LABEL:     {{^}}}

// The concrete VJP returns a pullback that captures an instantiation of the
// original generic branch tracing enum.
@differentiable(reverse, wrt: x)
@inline(never)
func branch<T>(_ x: Float, _ type: T.Type) -> Float {
  if x > 0 { return x * x }
  return x + x
}

tests.test("InstantiatedBranchTracingEnums") {
  expectEqual(Float(6), gradient(at: Float(3), of: { branch($0, Int.self) }))
  expectEqual(Float(2), gradient(at: Float(-3), of: { branch($0, Int.self) }))
  expectEqual(Float(8), gradient(at: Float(4), of: { branch($0, String.self) }))
  expectEqual(Float(2), gradient(at: Float(-4), of: { branch($0, String.self) }))
}

// MARK: - Nested tapes from different instantiations

// Early inlining makes the String and Double VJPs capture the Int instantiation
// of the same generic function's branch tracing enum.
@differentiable(reverse, wrt: x)
@inline(never)
public func nestedTape<T>(_ x: Float, _ type: T.Type, _ flag: Bool) -> Float {
  if T.self == Int.self { return x + x }
  if flag { return nestedTape(x * x, Int.self, false) }
  return x * x
}

// CHECK-LABEL:     {{^}}// specialized pullback of stringNestedTape(_:_:)
// CHECK:             switch_enum %[[#]], case #_AD__$s3src10nestedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l_spec_bb4_1_bb3_1_2_a8778c0bda02f1c55b2813a5ab390874.
// CHECK:           bb[[#]](%[[#]] : $(predecessor: _AD__$s3src10nestedTapeyS2f_xmSbtlF_bb3__Pred__src_0_wrt_0_l<String>, (Float, Float), (_: _AD__$s3src10nestedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l<Int>))):
// CHECK:             switch_enum %[[#]], case #_AD__$s3src10nestedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l.
// CHECK-LABEL:     {{^}}// specialized pullback of doubleNestedTape(_:_:)
// CHECK:             switch_enum %[[#]], case #_AD__$s3src10nestedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l_spec_bb4_1_bb3_1_2_a72cfdc37860cf59e53ca55a56d8ae78.
// CHECK:           bb[[#]](%[[#]] : $(predecessor: _AD__$s3src10nestedTapeyS2f_xmSbtlF_bb3__Pred__src_0_wrt_0_l<Double>, (Float, Float), (_: _AD__$s3src10nestedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l<Int>))):
// CHECK:             switch_enum %[[#]], case #_AD__$s3src10nestedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l.
// The concrete instantiations dispatch on their own tape types.
// CHECK-DAG:         switch_enum %[[#]], case #_AD__$s3src6branchyS2f_xmtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_45186266a52d34461f2256dfc117e4be.
// CHECK-DAG:         switch_enum %[[#]], case #_AD__$s3src6branchyS2f_xmtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_c861720d29dca92d6bf6c68c23b46431.

@differentiable(reverse, wrt: x)
@inline(never)
public func stringNestedTape(_ x: Float, _ flag: Bool) -> Float {
  nestedTape(x, String.self, flag)
}

@differentiable(reverse, wrt: x)
@inline(never)
public func doubleNestedTape(_ x: Float, _ flag: Bool) -> Float {
  nestedTape(x, Double.self, flag)
}

tests.test("RepeatedNestedInstantiation") {
  for flag in [false, true] {
    for x: Float in [-3, 0, 3] {
      let stringResult = valueWithPullback(at: x, of: { stringNestedTape($0, flag) })
      let doubleResult = valueWithPullback(at: x, of: { doubleNestedTape($0, flag) })
      let intResult = valueWithPullback(at: x, of: { nestedTape($0, Int.self, flag) })
      let scale: Float = flag ? 2 : 1
      let expectedValue = scale * x * x
      let expectedSlope = scale * 2 * x
      precondition(stringResult.value == expectedValue)
      precondition(doubleResult.value == expectedValue)
      precondition(intResult.value == 2 * x)

      for seed: Float in [0, 1, -2, 1] {
        precondition(stringResult.pullback(seed) == expectedSlope * seed)
        precondition(doubleResult.pullback(seed) == expectedSlope * seed)
        precondition(intResult.pullback(seed) == 2 * seed)
      }
    }
  }
}

runAllTests()
