// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-Onone)
// RUN: %target-run-simple-swift(-O)
// RUN: %target-run-simple-swift(-Osize)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name NestedTape %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation

// Early inlining makes the String and Double VJPs capture the Int instantiation
// of the same generic function's branch tracing enum.
@differentiable(reverse, wrt: x)
@inline(never)
public func nestedTape<T>(_ x: Float, _ type: T.Type, _ flag: Bool) -> Float {
  if T.self == Int.self { return x + x }
  if flag { return nestedTape(x * x, Int.self, false) }
  return x * x
}

// CHECK-LABEL: {{^}}// specialized pullback of stringNestedTape(_:_:)
// CHECK:         switch_enum %[[#]], case #{{_AD__[^.]+_spec[^.]+}}.
// CHECK:       bb[[#]](%[[#]] : $(predecessor: {{_AD__[^,]+}}<String>, (Float, Float), (_: [[INNER:_AD__[^ <]+]]<Int>))):
// CHECK:         switch_enum %[[#]], case #[[INNER]].
// CHECK-LABEL: {{^}}// specialized pullback of doubleNestedTape(_:_:)
// CHECK:         switch_enum %[[#]], case #{{_AD__[^.]+_spec[^.]+}}.
// CHECK:       bb[[#]](%[[#]] : $(predecessor: {{_AD__[^,]+}}<Double>, (Float, Float), (_: [[INNER]]<Int>))):
// CHECK:         switch_enum %[[#]], case #[[INNER]].
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
