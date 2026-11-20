// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-Onone)
// RUN: %target-run-simple-swift(-O)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name adcs_coroutine_tape %s -o %t.sil
// RUN: %FileCheck %s -DFUNCTION=mutateAndBranch -DMODEL=Accumulator < %t.sil
// RUN: %FileCheck %s -DFUNCTION=mutateSelected -DMODEL=Pair < %t.sil

// CHECK:       sil private @{{[^ ]*}}[[FUNCTION]]{{[^ ]*TJp[^ ]*_spec[^ ]*}} :
// CHECK-NOT:   } // end sil function
// CHECK:       bb[[#]]({{.*}}, %[[#PAYLOAD:]] : $((), @callee_guaranteed (@inout [[MODEL]].TangentVector) -> (), (Float, Float))):
// CHECK-NEXT:    %[[#TAPED:]] = tuple_extract %[[#PAYLOAD]], 1
// CHECK-NEXT:    %[[#CAPTURES:]] = tuple_extract %[[#PAYLOAD]], 2
// CHECK-NEXT:    %[[#ACCESSOR:]] = convert_function %[[#TAPED]] to $@yield_once
// CHECK-NEXT:    (%[[#YIELD:]], %[[#TOKEN:]]) = begin_apply %[[#ACCESSOR]]
// CHECK-NEXT:    %[[#]] = tuple_extract %[[#CAPTURES]], 0
// CHECK-NEXT:    %[[#]] = tuple_extract %[[#CAPTURES]], 1
// CHECK:         builtin "fmul_FPIEEE32"
// CHECK:         store %[[#]] to %[[#YIELD]]
// CHECK:         end_apply %[[#TOKEN]]
// CHECK:       } // end sil function

import _Differentiation

public struct Accumulator: Differentiable {
  public var value: Float
  public init(_ value: Float) { self.value = value }

  public var state: Float {
    get { value }
    set { value = newValue }
    @inline(never) _modify { yield &value }
  }
}

// The accessor pullback is formed by partial_apply and invoked as a coroutine.
@differentiable(reverse, wrt: x)
public func mutateAndBranch(_ x: Float, _ flag: Bool) -> Float {
  var model = Accumulator(x)
  model.state *= x
  if flag { return model.state * x }
  return model.state
}

struct Pair: Differentiable {
  var first: Float
  var second: Float
  @noDerivative var chooseFirst: Bool

  var selected: Float {
    get { chooseFirst ? first : second }
    @inline(never) _modify {
      if chooseFirst { yield &first } else { yield &second }
    }
  }
}

// With two yields, end_apply produces the accessor pullback. It shares a
// payload with the statically known multiply-assign pullback.
@differentiable(reverse, wrt: x)
public func mutateSelected(_ x: Float, _ chooseFirst: Bool, _ scale: Bool) -> Float {
  var pair = Pair(first: x, second: x, chooseFirst: chooseFirst)
  pair.selected *= x
  if scale { return pair.first * x }
  return pair.second
}

for flag in [false, true] {
  let (value, pullback) = valueWithPullback(at: Float(3), of: {
    mutateAndBranch($0, flag)
  })
  precondition(value == (flag ? 27 : 9))
  precondition(pullback(1) == (flag ? 27 : 6))
  precondition(pullback(2) == (flag ? 54 : 12))
}
for (chooseFirst, scale, expected, slope): (Bool, Bool, Float, Float) in [
  (true, true, 27, 27), (true, false, 3, 1),
  (false, true, 9, 6), (false, false, 9, 6)
] {
  let (value, pullback) = valueWithPullback(at: Float(3)) {
    mutateSelected($0, chooseFirst, scale)
  }
  precondition(value == expected)
  precondition(pullback(1) == slope)
  precondition(pullback(2) == 2 * slope)
}
