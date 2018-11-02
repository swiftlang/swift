// RUN: %target-swift-frontend -O -emit-sil -sil-verify-all -Xllvm -sil-disable-pass=function-signature-opts %s | %FileCheck %s

//===----------------------------------------------------------------------===//
// testReturnSelf: Call to a protocol extension method with
// an existential self that can be type-propagated.
// sil-combine should bailout since it does not propagate
// type substitutions on the return value.
//
// <rdar://40555427> [SR-7773]:
// SILCombiner::propagateConcreteTypeOfInitExistential fails to full propagate
// type substitutions.
//===----------------------------------------------------------------------===//
public protocol P: class {}

extension P {
  public func returnSelf() -> Self {
    return self
  }
}

final class C: P {}
// CHECK-LABEL: sil @$S32sil_combine_concrete_existential14testReturnSelfAA1P_pyF : $@convention(thin) () -> @owned P {
// CHECK: [[E1:%.*]] = init_existential_ref %0 : $C : $C, $P
// CHECK: [[O1:%.*]] = open_existential_ref [[E1]] : $P to $@opened("{{.*}}") P
// CHECK: [[F1:%.*]] = function_ref @$S32sil_combine_concrete_existential1PPAAE10returnSelfxyF : $@convention(method) <τ_0_0 where τ_0_0 : P> (@guaranteed τ_0_0) -> @owned τ_0_0
// CHECK: [[C1:%.*]] = apply [[F1]]<@opened("{{.*}}") P>([[O1]]) : $@convention(method) <τ_0_0 where τ_0_0 : P> (@guaranteed τ_0_0) -> @owned τ_0_0
// CHECK: [[E2:%.*]] = init_existential_ref [[C1]] : $@opened("{{.*}}") P : $@opened("{{.*}}") P, $P
// CHECK: [[O2:%.*]] = open_existential_ref [[E2]] : $P to $@opened("{{.*}}") P
// CHECK: apply [[F1]]<@opened("{{.*}}") P>([[O2]]) : $@convention(method) <τ_0_0 where τ_0_0 : P> (@guaranteed τ_0_0) -> @owned τ_0_0
// CHECK-LABEL: } // end sil function '$S32sil_combine_concrete_existential14testReturnSelfAA1P_pyF'
public func testReturnSelf() -> P {
  let p: P = C()
  return p.returnSelf().returnSelf()
}

//===----------------------------------------------------------------------===//
// testWitnessReturnOptionalSelf: Call to a witness method with an existential
// self that can be type-propagated. sil-combine should bailout since it does
// not propagate type substitutions on the return value, and it must walk the
// Optional type to find Self in the return type.
//
// Although sil-combine will not replace the self operand, it will still
// rewrite the witness_method. The devirtualizer then handles the first call.
//===----------------------------------------------------------------------===//
public protocol PP: class {
  func returnOptionalSelf() -> Self?
}

final class CC: PP {
  init() {}
  func returnOptionalSelf() -> Self? {
    return self
  }
}

// The first apply has been devirtualized and inlined. The second remains unspecialized.
// CHECK-LABEL: sil @$S32sil_combine_concrete_existential29testWitnessReturnOptionalSelfAA2PP_pSgyF : $@convention(thin) () -> @owned Optional<PP> {
// CHECK: [[E1:%.*]] = init_existential_ref %0 : $CC : $CC, $PP
// CHECK: [[O1:%.*]] = open_existential_ref [[E1]] : $PP to $@opened("{{.*}}") PP
// CHECK: [[E2:%.*]] = init_existential_ref %{{.*}} : $@opened("{{.*}}") PP : $@opened("{{.*}}") PP, $PP
// CHECK: [[O2:%.*]] = open_existential_ref [[E2]] : $PP to $@opened("{{.*}}") PP
// CHECK: [[W:%.*]] = witness_method $@opened("{{.*}}") PP, #PP.returnOptionalSelf!1 : <Self where Self : PP> (Self) -> () -> @dynamic_self Self?, [[O1]] : $@opened("{{.*}}") PP : $@convention(witness_method: PP) <τ_0_0 where τ_0_0 : PP> (@guaranteed τ_0_0) -> @owned Optional<τ_0_0>
// CHECK: apply [[W]]<@opened("{{.*}}") PP>([[O2]]) : $@convention(witness_method: PP) <τ_0_0 where τ_0_0 : PP> (@guaranteed τ_0_0) -> @owned Optional<τ_0_0>
// CHECK-LABEL: } // end sil function '$S32sil_combine_concrete_existential29testWitnessReturnOptionalSelfAA2PP_pSgyF'
public func testWitnessReturnOptionalSelf() -> PP? {
  let p: PP = CC()
  return p.returnOptionalSelf()?.returnOptionalSelf()
}

//===----------------------------------------------------------------------===//
// testWitnessReturnOptionalIndirectSelf: Call to a witness method with an
// existential self that can be type-propagated. sil-combine should bailout
// since it does not propagate type substitutions on non-self arguments. It must
// walk the Optional type to find Self in the non-self argument.
//
// Although sil-combine will not replace the self operand, it will still
// rewrite the witness_method. The devirtualizer then handles the first call.
//===----------------------------------------------------------------------===//
protocol PPP {
  func returnsOptionalIndirect() -> Self?
}

struct S: PPP {
  func returnsOptionalIndirect() -> S? {
    return self
  }
}

// The first apply has been devirtualized and inlined. The second remains unspecialized.
// CHECK-LABEL: sil @$S32sil_combine_concrete_existential37testWitnessReturnOptionalIndirectSelfyyF : $@convention(thin) () -> () {
// CHECK: switch_enum_addr %{{.*}} : $*Optional<@opened("{{.*}}") PPP>, case #Optional.some!enumelt.1: bb{{.*}}, case #Optional.none!enumelt: bb{{.*}}
// CHECK: [[O:%.*]] = open_existential_addr immutable_access %{{.*}} : $*PPP to $*@opened("{{.*}}") PPP
// CHECK: [[W:%.*]] = witness_method $@opened("{{.*}}") PPP, #PPP.returnsOptionalIndirect!1 : <Self where Self : PPP> (Self) -> () -> @dynamic_self Self?, [[O]] : $*@opened("{{.*}}") PPP : $@convention(witness_method: PPP) <τ_0_0 where τ_0_0 : PPP> (@in_guaranteed τ_0_0) -> @out Optional<τ_0_0>
// CHECK: apply [[W]]<@opened("{{.*}}") PPP>(%{{.*}}, [[O]]) : $@convention(witness_method: PPP) <τ_0_0 where τ_0_0 : PPP> (@in_guaranteed τ_0_0) -> @out Optional<τ_0_0>
// CHECK-LABEL: } // end sil function '$S32sil_combine_concrete_existential37testWitnessReturnOptionalIndirectSelfyyF'
public func testWitnessReturnOptionalIndirectSelf() {
  let p: PPP = S()
  p.returnsOptionalIndirect()?.returnsOptionalIndirect()
}

//===----------------------------------------------------------------------===//
// testExtensionProtocolComposition: Call to a witness method with an
// existential self that can be type-propagated. Handle an existential with
// multiple conformances.
//
// Previously crashed with in SILCombiner::propagateConcreteTypeOfInitExistential
// with assertion failed: (proto == Conformance.getRequirement()).
// ===----------------------------------------------------------------------===//
public protocol Q {}

extension P where Self : Q {
  public func witnessComposition() {}
}
  
public class C_PQ: P & Q {}

// testExtensionProtocolComposition(c:)
// CHECK-LABEL: sil @$S32sil_combine_concrete_existential32testExtensionProtocolComposition1cyAA4C_PQC_tF : $@convention(thin) (@guaranteed C_PQ) -> () {
// CHECK-NOT: init_existential_ref
// CHECK-NOT: function_ref
// CHECK-NOT: apply
// CHECK: } // end sil function '$S32sil_combine_concrete_existential32testExtensionProtocolComposition1cyAA4C_PQC_tF'
public func testExtensionProtocolComposition(c: C_PQ) {
  let pp: P & Q = c
  pp.witnessComposition()
}
