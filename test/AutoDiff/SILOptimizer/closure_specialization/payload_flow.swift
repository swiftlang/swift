// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name payload_flow %S/../../validation-test/closure_specialization/payload_flow.swift -Xllvm -sil-disable-skipping-passes -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s --check-prefix=INPUT < %t.passes
// RUN: %FileCheck %s --check-prefix=EARLY < %t.passes
// RUN: %FileCheck %s --check-prefix=LATE < %t.passes
// RUN: %FileCheck %s --check-prefix=MIXED < %t.passes
// RUN: %FileCheck %s --check-prefix=WHOLE < %t.passes
// RUN: %FileCheck %s --check-prefix=FINAL < %t.sil

// Compile the exact validation source. Textual SIL cannot faithfully round-trip
// its compiler-generated singleton tuple AST types, so source compilation also
// keeps the real ownership, enum-case and phi shapes intact.
//
// Both invocation points perform BTE specialization: branching is handled in
// HighLevel; lateBranch remains opaque until MidLevel inlining exposes its
// custom pullback. This checks actual late specialization, not just a second
// visit to a function already specialized by the first invocation.

// The VJP tapes a known callback with no captures.
// INPUT-LABEL: // reverse-mode derivative of branching(_:_:)
// INPUT: %[[SUBTRACT:[0-9]+]] = function_ref @{{.*}}_vjpSubtract
// INPUT-NEXT: %[[NO_CAPTURES:[0-9]+]] = thin_to_thick_function %[[SUBTRACT]]
// INPUT: %[[PAYLOAD:[0-9]+]] = tuple $(predecessor: {{.*}}) (%{{[0-9]+}}, %{{[0-9]+}}, %[[NO_CAPTURES]])
// INPUT-NEXT: {{.*}} = enum {{.*}}, %[[PAYLOAD]]

// The first input has both a whole multi-field payload argument and a scalar
// callback phi fed by singleton payload projections from two incoming edges.
// INPUT-LABEL: // pullback of branching(_:_:)
// INPUT: bb{{[0-9]+}}(%{{[0-9]+}} : @owned $(predecessor: {{.*}}, @callee_guaranteed (Float) -> (Float, Float))):
// INPUT: %[[PAYLOAD1:[0-9]+]] = unchecked_enum_data
// INPUT-NEXT: %[[FIELD1:[0-9]+]] = destructure_tuple %[[PAYLOAD1]]
// INPUT-NEXT: br [[JOIN:bb[0-9]+]](%{{[0-9]+}}, %[[FIELD1]], %{{[0-9]+}})
// INPUT: %[[PAYLOAD2:[0-9]+]] = unchecked_enum_data
// INPUT-NEXT: %[[FIELD2:[0-9]+]] = destructure_tuple %[[PAYLOAD2]]
// INPUT-NEXT: br [[JOIN]](%{{[0-9]+}}, %[[FIELD2]], %{{[0-9]+}})
// INPUT: [[JOIN]](%{{[0-9]+}} : $Builtin.FPIEEE32, %[[CALLBACK:[0-9]+]] : @owned $@callee_guaranteed (Float) -> (Float, Float), %{{[0-9]+}} : $Builtin.FPIEEE32):
// INPUT: apply %[[CALLBACK]]

// A projected predecessor-enum phi is trivial already at the first invocation;
// its incoming fields use tuple_extract rather than destructure_tuple.
// INPUT-LABEL: // pullback of generic<A>(_:_:)
// INPUT: %[[PREDPAYLOAD1:[0-9]+]] = unchecked_enum_data
// INPUT-NEXT: %[[PRED1:[0-9]+]] = tuple_extract %[[PREDPAYLOAD1]], 0
// INPUT-NEXT: br [[PREDJOIN:bb[0-9]+]](%{{[0-9]+}}, %[[PRED1]])
// INPUT: %[[PREDPAYLOAD2:[0-9]+]] = unchecked_enum_data
// INPUT-NEXT: %[[PRED2:[0-9]+]] = tuple_extract %[[PREDPAYLOAD2]], 0
// INPUT-NEXT: br [[PREDJOIN]](%{{[0-9]+}}, %[[PRED2]])
// INPUT: [[PREDJOIN]](%{{[0-9]+}} : $Float, %[[PRED:[0-9]+]] : $_AD__{{[^,]+}}):
// INPUT: switch_enum %[[PRED]], case {{[^:]+}}: [[UNUSED:bb[0-9]+]]
// INPUT: [[UNUSED]](%[[UNUSED_PAYLOAD:[0-9]+]] : $(predecessor: {{_AD__[^)]+}})):
// INPUT-NOT: %[[UNUSED_PAYLOAD]]
// INPUT: br bb{{[0-9]+}} //

// Creating this specialized pullback during the high-level invocation proves
// the first phase rewrote the projected phi. Its subsequent pass input has a
// capture-tuple phi and direct calls, rather than opaque callback applications.
// EARLY: *** New SIL function in {{.*}}stage HighLevel,Module+StackPromote, pass: AutodiffClosureSpecialization {{.*}}, Function: $s12payload_flow9branchingyS2f_SbtFTJp
// EARLY-NEXT: // specialized pullback of branching(_:_:)
// EARLY: // specialized pullback of branching(_:_:)
// The captureless callbacks become empty tuples. Invoke their thin functions
// directly, with only the original seed argument.
// EARLY: bb{{[0-9]+}}(%{{[0-9]+}} : $(predecessor: {{.*}}, (), ())):
// EARLY: %[[DIRECT_SUBTRACT:[0-9]+]] = function_ref @{{.*}}_vjpSubtract
// EARLY-NEXT: {{.*}} = apply %[[DIRECT_SUBTRACT]](%{{[0-9]+}}) : $@convention(thin) (Float) -> (Float, Float)
// EARLY: bb{{[0-9]+}}(%{{[0-9]+}} : $Builtin.FPIEEE32, %{{[0-9]+}} : $(Float, Float), %{{[0-9]+}} : $Builtin.FPIEEE32):
// EARLY-NOT: apply {{.*}} : $@callee_guaranteed
// EARLY: } // end sil function

// The nested dispatch supplies an unused whole payload. Its argument still
// gets the new predecessor enum type, although it has no projections to rewrite.
// EARLY: *** New SIL function in {{.*}}stage HighLevel,Module+StackPromote, pass: AutodiffClosureSpecialization {{.*}}, Function: $s12payload_flow7generic
// EARLY-NEXT: // specialized pullback of generic<A>(_:_:)
// EARLY: // specialized pullback of generic<A>(_:_:)
// EARLY: bb{{[0-9]+}}(%{{[0-9]+}} : $Float, %[[NEW_PRED:[0-9]+]] : $_AD__{{[^,]+_spec[^,]+}}): // Preds:
// EARLY: switch_enum %[[NEW_PRED]], case {{[^:]+}}: [[NEW_UNUSED:bb[0-9]+]]
// EARLY: [[NEW_UNUSED]](%[[NEW_UNUSED_PAYLOAD:[0-9]+]] : $(predecessor: {{_AD__[^)]+_spec[^)]+}})):
// EARLY-NOT: %[[NEW_UNUSED_PAYLOAD]]
// EARLY: br bb{{[0-9]+}} //

// The first invocation leaves the custom derivative call and the original
// pullback in place. The second invocation sees a known captured closure and
// creates a new specialized pullback in the actual mid-level stage.
// LATE-LABEL: // reverse-mode derivative of lateBranch(_:_:)
// LATE: // function_ref delayedSquareVJP(_:)
// LATE: apply
// LATE-NOT: function_ref specialized pullback of lateBranch
// LATE: // function_ref pullback of lateBranch(_:_:)
// LATE: } // end sil function
// LATE-LABEL: // pullback of lateBranch(_:_:)
// LATE: bb{{[0-9]+}}(%{{[0-9]+}} : $Builtin.FPIEEE32, %{{[0-9]+}} : $Builtin.FPIEEE32, %[[OPAQUE:[0-9]+]] : @owned $@callee_guaranteed (Float) -> Float):
// LATE: apply %[[OPAQUE]]
// LATE: } // end sil function
// LATE: *** New SIL function in {{.*}}stage MidLevel,Function, pass: AutodiffClosureSpecialization {{.*}}, Function: $s12payload_flow10lateBranchyS2f_SbtFTJp
// LATE-NEXT: // specialized pullback of lateBranch(_:_:)
// LATE: // specialized pullback of lateBranch(_:_:)
// LATE: bb{{[0-9]+}}(%{{[0-9]+}} : $Builtin.FPIEEE32, %{{[0-9]+}} : $Builtin.FPIEEE32, %[[CAPTURES:[0-9]+]] : $(_: Float)):
// LATE: tuple_extract %[[CAPTURES]], 0
// LATE-NOT: apply {{.*}} : $@callee_guaranteed
// LATE: } // end sil function

// The same specialized pullback directly invokes the known multiplication
// and retargets the opaque case's apply and destroy to its new tuple result.
// MIXED-LABEL: *** New SIL function in {{.*}}stage HighLevel,Module+StackPromote, pass: AutodiffClosureSpecialization {{.*}}, Function: $s12payload_flow14mixedCallbacks{{.*}}TJp
// MIXED-NEXT: // specialized pullback of mixedCallbacks(_:_:_:)
// MIXED: // specialized pullback of mixedCallbacks(_:_:_:)
// MIXED: bb{{[0-9]+}}(%{{[0-9]+}} : $(predecessor: {{.*}}, (Float, Float))):
// MIXED: %[[MULTIPLY:[0-9]+]] = function_ref @{{.*}}_vjpMultiply
// MIXED-NEXT: {{.*}} = apply %[[MULTIPLY]]({{.*}}) : $@convention(thin)
// MIXED: bb{{[0-9]+}}(%[[OPAQUE_PAYLOAD:[0-9]+]] : @owned $(predecessor: {{.*}}, @callee_guaranteed (Float) -> Float)):
// MIXED-NEXT: (%{{[0-9]+}}, %[[OPAQUE:[0-9]+]]) = destructure_tuple %[[OPAQUE_PAYLOAD]]
// MIXED-NEXT: {{.*}} = apply %[[OPAQUE]]({{.*}}) : $@callee_guaranteed (Float) -> Float
// MIXED-NEXT: destroy_value %[[OPAQUE]]
// MIXED: } // end sil function

// The original whole predecessor payload is trivial and already uses TEI at
// the first invocation. The callback-containing whole payload still uses DTI.
// Disable pass skipping above so the unchanged original PB is also printed
// at the second invocation, after its VJP's late derivative has been exposed.
// WHOLE-LABEL: // reverse-mode derivative of lateIdentity(_:_:_:)
// WHOLE: // function_ref delayedSquareVJP(_:)
// WHOLE-NOT: function_ref specialized pullback of lateIdentity
// WHOLE: // function_ref pullback of lateIdentity
// WHOLE-LABEL: // pullback of lateIdentity(_:_:_:)
// WHOLE: bb{{[0-9]+}}(%[[OWNED:[0-9]+]] : @owned $(predecessor: {{.*}}, @callee_guaranteed (Float) -> Float)):
// WHOLE-NEXT: (%{{[0-9]+}}, %{{[0-9]+}}) = destructure_tuple %[[OWNED]]
// WHOLE: bb{{[0-9]+}}(%[[WHOLE:[0-9]+]] : $(predecessor: {{.*}})):
// WHOLE-NEXT: %[[PRED:[0-9]+]] = tuple_extract %[[WHOLE]], 0
// WHOLE: switch_enum %[[PRED]]

// The second invocation creates a specialized PB and rebuilds its whole-tuple
// extraction against the new predecessor type.
// WHOLE: *** New SIL function in {{.*}}stage MidLevel,Function, pass: AutodiffClosureSpecialization {{.*}}, Function: $s12payload_flow12lateIdentity
// WHOLE-NEXT: // specialized pullback of lateIdentity(_:_:_:)
// WHOLE: // specialized pullback of lateIdentity(_:_:_:)
// WHOLE: bb{{[0-9]+}}(%[[NEW_WHOLE:[0-9]+]] : $(predecessor: {{.*}}_spec{{[^)]+}})):
// WHOLE-NEXT: %[[NEW_PRED:[0-9]+]] = tuple_extract %[[NEW_WHOLE]], 0
// WHOLE: switch_enum %[[NEW_PRED]]
// WHOLE: } // end sil function
// WHOLE: // specialized pullback of lateIdentity(_:_:_:)
// WHOLE: } // end sil function
// WHOLE: *** SIL function before {{.*}}stage MidLevel,Function, pass {{[0-9]+}}: AutodiffClosureSpecialization
// WHOLE-NEXT: // pullback of lateIdentity(_:_:_:)
// WHOLE: bb{{[0-9]+}}(%[[STILL_OWNED:[0-9]+]] : @owned $(predecessor: {{.*}}, @callee_guaranteed (Float) -> Float)):
// WHOLE-NEXT: (%{{[0-9]+}}, %{{[0-9]+}}) = destructure_tuple %[[STILL_OWNED]]
// WHOLE: bb{{[0-9]+}}(%[[STILL_WHOLE:[0-9]+]] : $(predecessor: {{.*}})):
// WHOLE-NEXT: %[[STILL_PRED:[0-9]+]] = tuple_extract %[[STILL_WHOLE]], 0
// WHOLE: switch_enum %[[STILL_PRED]]

// The final source-generated VJPs return specialized pullbacks. Their bodies
// have no materialized closures or opaque callback applications.
// FINAL-LABEL: // reverse-mode derivative of branching(_:_:)
// FINAL-NOT: } // end sil function
// FINAL: // function_ref specialized pullback of branching(_:_:)
// FINAL: return
// FINAL: } // end sil function
// FINAL-LABEL: // specialized pullback of branching(_:_:)
// FINAL-NOT: = partial_apply
// FINAL-NOT: apply {{.*}} : $@callee_guaranteed
// FINAL: } // end sil function
// FINAL-LABEL: // reverse-mode derivative of nested(_:_:)
// FINAL-NOT: } // end sil function
// FINAL: // function_ref specialized pullback of nested(_:_:)
// FINAL: return
// FINAL: } // end sil function
// FINAL-LABEL: // specialized pullback of nested(_:_:)
// FINAL-NOT: = partial_apply
// FINAL-NOT: apply {{.*}} : $@callee_guaranteed
// FINAL: } // end sil function
// FINAL-LABEL: // reverse-mode derivative of generic<A>(_:_:)
// FINAL-NOT: } // end sil function
// FINAL: // function_ref specialized pullback of generic<A>(_:_:)
// FINAL: return
// FINAL: } // end sil function
// FINAL-LABEL: // specialized pullback of generic<A>(_:_:)
// FINAL-NOT: = partial_apply
// FINAL-NOT: apply {{.*}} : $@callee_guaranteed
// FINAL: } // end sil function
// FINAL-LABEL: // reverse-mode derivative of lateBranch(_:_:)
// FINAL-NOT: } // end sil function
// FINAL: // function_ref specialized pullback of lateBranch(_:_:)
// FINAL: return
// FINAL: } // end sil function
// FINAL-LABEL: // specialized pullback of lateBranch(_:_:)
// FINAL-NOT: = partial_apply
// FINAL-NOT: apply {{.*}} : $@callee_guaranteed
// FINAL: } // end sil function
