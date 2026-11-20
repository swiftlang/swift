// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name adcs_coroutine_tape %s -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s --check-prefix=INPUT < %t.passes
// RUN: %FileCheck %s --check-prefix=PULLBACK -DFUNCTION=mutateAndBranch -DMODEL=Accumulator < %t.passes
// RUN: %FileCheck %s --check-prefix=PULLBACK -DFUNCTION=mutateSelected -DMODEL=Pair < %t.passes

// INPUT-LABEL: {{^}}// reverse-mode derivative of mutateAndBranch(_:_:)
// INPUT:         // function_ref pullback of Accumulator.state.modify
// INPUT-NEXT:    %[[#MODIFY:]] = function_ref
// INPUT-NEXT:    %[[#CANDIDATE:]] = partial_apply {{.*}} %[[#MODIFY]]
// INPUT-NEXT:    %[[#CONVERTED:]] = convert_function %[[#CANDIDATE]] to $@callee_guaranteed (@inout Accumulator.TangentVector) -> ()
// INPUT:         %[[#SAVED:]] = tuple (%[[#]], %[[#CONVERTED]], %[[#]])
// INPUT:       {{.*}} = enum $_AD__{{.*}}, %[[#SAVED]]
// INPUT:         %[[#STATIC:]] = function_ref @{{.*}}_vjpMultiply
// INPUT-NEXT:    %[[#CLOSURE:]] = partial_apply {{.*}} %[[#STATIC]]
// INPUT:         %[[#KNOWN:]] = tuple $(predecessor: {{.*}}) ({{.*}}, %[[#CLOSURE]])
// INPUT-NEXT:  {{.*}} = enum $_AD__{{.*}}, %[[#KNOWN]]

// INPUT-LABEL: {{^}}// reverse-mode derivative of mutateSelected(_:_:_:)
// INPUT:         // function_ref closure #1 in static Float._vjpMultiplyAssign
// INPUT-NEXT:    %[[#ASSIGN:]] = function_ref
// INPUT-NEXT:    %[[#ASSIGN_CLOSURE:]] = partial_apply {{.*}} %[[#ASSIGN]]
// INPUT:         %[[#RETURNED:]] = end_apply {{.*}} as $@yield_once
// INPUT-NEXT:    %[[#OPAQUE:]] = convert_function %[[#RETURNED]] to $@callee_guaranteed (@inout Pair.TangentVector) -> ()
// INPUT:       tuple (%[[#]], %[[#OPAQUE]], %[[#ASSIGN_CLOSURE]])

// PULLBACK:      sil private [ossa] @{{[^ ]*}}[[FUNCTION]]{{[^ ]*TJp[^ ]*_spec[^ ]*}} :
// PULLBACK-NOT:  } // end sil function
// PULLBACK:      bb[[#]]({{.*}}, %[[#PAYLOAD:]] : @owned $({{.*}}, @callee_guaranteed (@inout [[MODEL]].TangentVector) -> (), (Float, Float))):
// PULLBACK-NEXT:   (%[[#]], %[[#TAPED:]], %[[#CAPTURES:]]) = destructure_tuple %[[#PAYLOAD]]
// PULLBACK-NEXT:   %[[#ACCESSOR:]] = convert_function %[[#TAPED]] to $@yield_once
// PULLBACK-NEXT:   (%[[#YIELD:]], %[[#TOKEN:]]) = begin_apply %[[#ACCESSOR]]
// PULLBACK:        %[[#LHS:]] = tuple_extract %[[#CAPTURES]], 0
// PULLBACK:        %[[#RHS:]] = tuple_extract %[[#CAPTURES]], 1
// PULLBACK:        %[[#STATIC:]] = function_ref @{{[^ ]*_vjpMultiplyAssign[^ ]*}}
// PULLBACK-NEXT: {{.*}} = apply %[[#STATIC]](%[[#YIELD]], %[[#LHS]], %[[#RHS]])
// PULLBACK:      end_apply %[[#TOKEN]]
// PULLBACK:      } // end sil function

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

// The single-yield accessor exposes a partial_apply for its pullback.
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

// The two-yield accessor produces its pullback dynamically from end_apply.
@differentiable(reverse, wrt: x)
public func mutateSelected(_ x: Float, _ chooseFirst: Bool, _ scale: Bool) -> Float {
  var pair = Pair(first: x, second: x, chooseFirst: chooseFirst)
  pair.selected *= x
  if scale { return pair.first * x }
  return pair.second
}
