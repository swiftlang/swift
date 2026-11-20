// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name payload_flow %s -Xllvm -sil-disable-skipping-passes -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s --check-prefix=INPUT < %t.passes
// RUN: %FileCheck %s --check-prefix=EARLY < %t.passes
// RUN: %FileCheck %s --check-prefix=LATE < %t.passes
// RUN: %FileCheck %s --check-prefix=MIXED < %t.passes
// RUN: %FileCheck %s --check-prefix=WHOLE < %t.passes
// RUN: %FileCheck %s --check-prefix=FINAL < %t.sil
// RUN: %FileCheck %s --check-prefix=ORDER < %t.sil

// Emit specialized predecessor declarations in the order of the join's cases.
// ORDER:      enum [[FIRST:_AD__[^ ]*9branching[^ ]*_spec[^ ]*]] {
// ORDER:      enum [[SECOND:_AD__[^ ]*9branching[^ ]*_spec[^ ]*]] {
// ORDER:      enum {{_AD__[^ ]*9branching[^ ]*_spec[^ ]*}} {
// ORDER-NEXT: case bb[[#]]((predecessor: [[FIRST]],
// ORDER-NOT:  }
// ORDER:      case bb[[#]]((predecessor: [[SECOND]],

// INPUT-LABEL: {{^}}// reverse-mode derivative of branching(_:_:)
// INPUT:         %[[#SUBTRACT:]] = function_ref @{{.*}}_vjpSubtract
// INPUT-NEXT:    %[[#NO_CAPTURES:]] = thin_to_thick_function %[[#SUBTRACT]]
// INPUT:         %[[#PAYLOAD:]] = tuple $(predecessor: {{.*}}) (%[[#]], %[[#]], %[[#NO_CAPTURES]])
// INPUT-NEXT:  {{.*}} = enum {{.*}}, %[[#PAYLOAD]]

// INPUT-LABEL: {{^}}// pullback of branching(_:_:)
// INPUT:       bb[[#]](%[[#]] : @owned $(predecessor: {{.*}}, @callee_guaranteed (Float) -> (Float, Float))):
// INPUT:         %[[#PAYLOAD1:]] = unchecked_enum_data
// INPUT-NEXT:    %[[#FIELD1:]] = destructure_tuple %[[#PAYLOAD1]]
// INPUT-NEXT:    br [[JOIN:bb[0-9]+]](%[[#]], %[[#FIELD1]], %[[#]])
// INPUT:         %[[#PAYLOAD2:]] = unchecked_enum_data
// INPUT-NEXT:    %[[#FIELD2:]] = destructure_tuple %[[#PAYLOAD2]]
// INPUT-NEXT:    br [[JOIN]](%[[#]], %[[#FIELD2]], %[[#]])
// INPUT:       [[JOIN]](%[[#]] : $Builtin.FPIEEE32, %[[#CALLBACK:]] : @owned $@callee_guaranteed (Float) -> (Float, Float), %[[#]] : $Builtin.FPIEEE32):
// INPUT:         apply %[[#CALLBACK]]

// INPUT-LABEL: {{^}}// pullback of generic<A>(_:_:)
// INPUT:         %[[#PREDPAYLOAD1:]] = unchecked_enum_data
// INPUT-NEXT:    %[[#PRED1:]] = tuple_extract %[[#PREDPAYLOAD1]], 0
// INPUT-NEXT:    br [[PREDJOIN:bb[0-9]+]](%[[#]], %[[#PRED1]])
// INPUT:         %[[#PREDPAYLOAD2:]] = unchecked_enum_data
// INPUT-NEXT:    %[[#PRED2:]] = tuple_extract %[[#PREDPAYLOAD2]], 0
// INPUT-NEXT:    br [[PREDJOIN]](%[[#]], %[[#PRED2]])
// INPUT:       [[PREDJOIN]](%[[#]] : $Float, %[[#PRED:]] : $_AD__{{[^,]+}}):
// INPUT:         switch_enum %[[#PRED]], case {{[^:]+}}: [[UNUSED:bb[0-9]+]]
// INPUT:       [[UNUSED]](%[[#UNUSED_PAYLOAD:]] : $(predecessor: {{_AD__[^)]+}})):
// INPUT-NOT:     %[[#UNUSED_PAYLOAD]]
// INPUT:         br bb[[#]] //

// EARLY:      *** New SIL function in {{.*}}stage HighLevel,Module+StackPromote, pass: AutodiffClosureSpecialization {{.*}}, Function: $s12payload_flow9branchingyS2f_SbtFTJp
// EARLY-NEXT: // specialized pullback of branching(_:_:)
// EARLY:      // specialized pullback of branching(_:_:)
// EARLY:      bb[[#]](%[[#]] : $(predecessor: {{.*}}, (), ())):
// EARLY:        %[[#DIRECT_SUBTRACT:]] = function_ref @{{.*}}_vjpSubtract
// EARLY-NEXT: {{.*}} = apply %[[#DIRECT_SUBTRACT]](%[[#]]) : $@convention(thin) (Float) -> (Float, Float)
// EARLY:      bb[[#]](%[[#]] : $Builtin.FPIEEE32, %[[#]] : $(Float, Float), %[[#]] : $Builtin.FPIEEE32):
// EARLY-NOT:    apply {{.*}} : $@callee_guaranteed
// EARLY:      } // end sil function

// EARLY:      *** New SIL function in {{.*}}stage HighLevel,Module+StackPromote, pass: AutodiffClosureSpecialization {{.*}}, Function: $s12payload_flow7generic
// EARLY-NEXT: // specialized pullback of generic<A>(_:_:)
// EARLY:      // specialized pullback of generic<A>(_:_:)
// EARLY:      bb[[#]](%[[#]] : $Float, %[[#NEW_PRED:]] : $_AD__{{[^ ]+}}__Pred__src_0_wrt_0_l<τ_0_0>): // Preds:
// EARLY:        switch_enum %[[#NEW_PRED]], case {{[^:]+}}: [[NEW_UNUSED:bb[0-9]+]]
// EARLY:      [[NEW_UNUSED]](%[[#NEW_UNUSED_PAYLOAD:]] : $(predecessor: {{_AD__[^ ]+}}__Pred__src_0_wrt_0_l<τ_0_0>)):
// EARLY-NOT:    %[[#NEW_UNUSED_PAYLOAD]]
// EARLY:        br bb[[#]] //

// LATE-LABEL: {{^}}// reverse-mode derivative of lateBranch(_:_:)
// LATE:         // function_ref delayedSquareVJP(_:)
// LATE:         apply
// LATE-NOT:     function_ref specialized pullback of lateBranch
// LATE:         // function_ref pullback of lateBranch(_:_:)
// LATE:       } // end sil function
// LATE-LABEL: {{^}}// pullback of lateBranch(_:_:)
// LATE:       bb[[#]](%[[#]] : $Builtin.FPIEEE32, %[[#]] : $Builtin.FPIEEE32, %[[#OPAQUE:]] : @owned $@callee_guaranteed (Float) -> Float):
// LATE:         apply %[[#OPAQUE]]
// LATE:       } // end sil function
// LATE:       *** New SIL function in {{.*}}stage MidLevel,Function, pass: AutodiffClosureSpecialization {{.*}}, Function: $s12payload_flow10lateBranchyS2f_SbtFTJp
// LATE-NEXT:  // specialized pullback of lateBranch(_:_:)
// LATE:       // specialized pullback of lateBranch(_:_:)
// LATE:       bb[[#]](%[[#]] : $Builtin.FPIEEE32, %[[#]] : $Builtin.FPIEEE32, %[[#CAPTURES:]] : $(_: Float)):
// LATE:       tuple_extract %[[#CAPTURES]], 0
// LATE-NOT:     apply {{.*}} : $@callee_guaranteed
// LATE:       } // end sil function

// MIXED-LABEL: *** New SIL function in {{.*}}stage HighLevel,Module+StackPromote, pass: AutodiffClosureSpecialization {{.*}}, Function: $s12payload_flow14mixedCallbacks{{.*}}TJp
// MIXED-NEXT:  // specialized pullback of mixedCallbacks(_:_:_:)
// MIXED:       // specialized pullback of mixedCallbacks(_:_:_:)
// MIXED:       bb[[#]](%[[#]] : $(predecessor: {{.*}}, (Float, Float))):
// MIXED:         %[[#MULTIPLY:]] = function_ref @{{.*}}_vjpMultiply
// MIXED-NEXT:  {{.*}} = apply %[[#MULTIPLY]]({{.*}}) : $@convention(thin)
// MIXED:       bb[[#]](%[[#OPAQUE_PAYLOAD:]] : @owned $(predecessor: {{.*}}, @callee_guaranteed (Float) -> Float)):
// MIXED-NEXT:    (%[[#]], %[[#OPAQUE:]]) = destructure_tuple %[[#OPAQUE_PAYLOAD]]
// MIXED-NEXT:  {{.*}} = apply %[[#OPAQUE]]({{.*}}) : $@callee_guaranteed (Float) -> Float
// MIXED-NEXT:    destroy_value %[[#OPAQUE]]
// MIXED:       } // end sil function

// Disable pass skipping so the unchanged pullback is printed at the
// second invocation after late inlining has exposed its VJP's callback.
// WHOLE-LABEL: {{^}}// reverse-mode derivative of lateIdentity(_:_:_:)
// WHOLE:         // function_ref delayedSquareVJP(_:)
// WHOLE-NOT:     function_ref specialized pullback of lateIdentity
// WHOLE:         // function_ref pullback of lateIdentity
// WHOLE-LABEL: {{^}}// pullback of lateIdentity(_:_:_:)
// WHOLE:       bb[[#]](%[[#OWNED:]] : @owned $(predecessor: {{.*}}, @callee_guaranteed (Float) -> Float)):
// WHOLE-NEXT:    (%[[#]], %[[#]]) = destructure_tuple %[[#OWNED]]
// WHOLE:       bb[[#]](%[[#WHOLE:]] : $(predecessor: {{.*}})):
// WHOLE-NEXT:    %[[#PRED:]] = tuple_extract %[[#WHOLE]], 0
// WHOLE:         switch_enum %[[#PRED]]

// WHOLE:      *** New SIL function in {{.*}}stage MidLevel,Function, pass: AutodiffClosureSpecialization {{.*}}, Function: $s12payload_flow12lateIdentity
// WHOLE-NEXT: // specialized pullback of lateIdentity(_:_:_:)
// WHOLE:      // specialized pullback of lateIdentity(_:_:_:)
// WHOLE:      bb[[#]](%[[#NEW_WHOLE:]] : $(predecessor: {{_AD__[^ ]+}}__Pred__src_0_wrt_0)):
// WHOLE-NEXT:   %[[#NEW_PRED:]] = tuple_extract %[[#NEW_WHOLE]], 0
// WHOLE:        switch_enum %[[#NEW_PRED]]
// WHOLE:      } // end sil function
// WHOLE:      // specialized pullback of lateIdentity(_:_:_:)
// WHOLE:      } // end sil function
// WHOLE:      *** SIL function before {{.*}}stage MidLevel,Function, pass {{[0-9]+}}: AutodiffClosureSpecialization
// WHOLE-NEXT: // pullback of lateIdentity(_:_:_:)
// WHOLE:      bb[[#]](%[[#STILL_OWNED:]] : @owned $(predecessor: {{.*}}, @callee_guaranteed (Float) -> Float)):
// WHOLE-NEXT:   (%[[#]], %[[#]]) = destructure_tuple %[[#STILL_OWNED]]
// WHOLE:      bb[[#]](%[[#STILL_WHOLE:]] : $(predecessor: {{.*}})):
// WHOLE-NEXT:   %[[#STILL_PRED:]] = tuple_extract %[[#STILL_WHOLE]], 0
// WHOLE:        switch_enum %[[#STILL_PRED]]

// FINAL-LABEL: {{^}}// reverse-mode derivative of branching(_:_:)
// FINAL-NOT:   } // end sil function
// FINAL:         // function_ref specialized pullback of branching(_:_:)
// FINAL:         return
// FINAL:       } // end sil function
// FINAL-LABEL: {{^}}// specialized pullback of branching(_:_:)
// FINAL-NOT:     = partial_apply
// FINAL-NOT:     apply {{.*}} : $@callee_guaranteed
// FINAL:       } // end sil function
// FINAL-LABEL: {{^}}// reverse-mode derivative of nested(_:_:)
// FINAL-NOT:   } // end sil function
// FINAL:         // function_ref specialized pullback of nested(_:_:)
// FINAL:         return
// FINAL:       } // end sil function
// FINAL-LABEL: {{^}}// specialized pullback of nested(_:_:)
// FINAL-NOT:     = partial_apply
// FINAL-NOT:     apply {{.*}} : $@callee_guaranteed
// FINAL:       } // end sil function
// FINAL-LABEL: {{^}}// reverse-mode derivative of generic<A>(_:_:)
// FINAL-NOT:   } // end sil function
// FINAL:         // function_ref specialized pullback of generic<A>(_:_:)
// FINAL:         return
// FINAL:       } // end sil function
// FINAL-LABEL: {{^}}// specialized pullback of generic<A>(_:_:)
// FINAL-NOT:     = partial_apply
// FINAL-NOT:     apply {{.*}} : $@callee_guaranteed
// FINAL:       } // end sil function
// FINAL-LABEL: {{^}}// reverse-mode derivative of lateBranch(_:_:)
// FINAL-NOT:   } // end sil function
// FINAL:         // function_ref specialized pullback of lateBranch(_:_:)
// FINAL:         return
// FINAL:       } // end sil function
// FINAL-LABEL: {{^}}// specialized pullback of lateBranch(_:_:)
// FINAL-NOT:     = partial_apply
// FINAL-NOT:     apply {{.*}} : $@callee_guaranteed
// FINAL:       } // end sil function

import _Differentiation

// Aggregate unwrapping projects payload fields across branches before ADCS.
@inline(never)
@differentiable(reverse, wrt: x)
public func branching(_ x: Float, _ flag: Bool) -> Float {
  var result = x * x
  if flag {
    result = result + result
    result = result - x
  } else {
    result = result / x
  }
  return result
}

@inline(never)
@differentiable(reverse)
public func nested(_ x: Float, _ y: Float) -> Float {
  if x > 0 {
    if y > 10 {
      let z = x * y
      if z > 100 { return x + z }
      if y == 20 { return z + z }
    } else {
      return x + y
    }
  }
  return -y
}

@inline(never)
@differentiable(reverse, wrt: x)
public func generic<T>(_ x: Float, _ metatype: T.Type) -> Float {
  if metatype is Int.Type { return x + x }
  return x * x
}

@inline(never)
@differentiable(reverse)
public func delayedSquare(_ x: Float) -> Float { x * x }

// Delay exposing the callback until mid-level inlining, after the first ADCS run.
@_semantics("inline_late")
@derivative(of: delayedSquare)
public func delayedSquareVJP(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  (x * x, { seed in 2 * x * seed })
}

@inline(never)
@differentiable(reverse, wrt: x)
public func lateBranch(_ x: Float, _ flag: Bool) -> Float {
  let result = delayedSquare(x)
  if flag { return result }
  return x
}

// The identity and constant paths keep a live trivial predecessor payload
// that uses tuple_extract before ADCS.
@inline(never)
@differentiable(reverse, wrt: x)
public func lateIdentity(_ x: Float, _ first: Bool, _ second: Bool) -> Float {
  let result: Float
  if first {
    if second { result = x } else { result = 3 }
  } else {
    result = delayedSquare(x)
  }
  return result
}

@inline(never)
@differentiable(reverse, wrt: x)
public func mixedCallbacks(
  _ x: Float, _ useTransform: Bool,
  _ transform: @differentiable(reverse) (Float) -> Float
) -> Float {
  if useTransform { return transform(x) }
  return x * x
}

public func branchingGradient(_ x: Float, _ flag: Bool) -> Float {
  gradient(at: x, of: { branching($0, flag) })
}
public func nestedGradient(_ x: Float, _ y: Float) -> (Float, Float) {
  gradient(at: x, y, of: nested)
}
public func genericGradient<T>(_ x: Float, _ metatype: T.Type) -> Float {
  gradient(at: x, of: { generic($0, metatype) })
}

public func lateGradient(_ x: Float, _ flag: Bool) -> Float {
  gradient(at: x, of: { lateBranch($0, flag) })
}

public func lateIdentityGradient(_ x: Float, _ first: Bool, _ second: Bool) -> Float {
  gradient(at: x, of: { lateIdentity($0, first, second) })
}
