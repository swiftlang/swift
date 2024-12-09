// RUN: %target-swift-frontend -O -Xllvm -sil-print-types -emit-sil -sil-verify-all -Xllvm -sil-disable-pass=function-signature-opts %s | %FileCheck %s
// RUN: %target-swift-frontend -O -Xllvm -sil-print-types -emit-sil -Xllvm -sil-verify-force-analysis-around-pass=devirtualizer -Xllvm -sil-disable-pass=function-signature-opts %s | %FileCheck %s

// REQUIRES: swift_in_compiler

//===----------------------------------------------------------------------===//
// testReturnSelf: Call to a protocol extension method with
// an existential self that can be type-propagated.
// sil-combine should bailout since it does not propagate
// type substitutions on the return value.
//
// rdar://40555427
// https://github.com/apple/swift/issues/50312
// 'SILCombiner::propagateConcreteTypeOfInitExistential' fails to fully
// propagate type substitutions.
//===----------------------------------------------------------------------===//
public protocol P: class {}

extension P {
  public func returnSelf() -> Self {
    return self
  }
}

final class C: P {}
// CHECK-LABEL: sil @$s32sil_combine_concrete_existential14testReturnSelfAA1P_pyF : $@convention(thin) () -> @owned any P {
// CHECK: [[EI:%.*]] = end_init_let_ref %0
// CHECK: [[E1:%.*]] = init_existential_ref [[EI]] : $C : $C, $any P
// CHECK: [[O1:%.*]] = open_existential_ref [[E1]] : $any P to $@opened("{{.*}}", any P) Self
// CHECK: [[F1:%.*]] = function_ref @$s32sil_combine_concrete_existential1PPAAE10returnSelfxyF : $@convention(method) <τ_0_0 where τ_0_0 : P> (@guaranteed τ_0_0) -> @owned τ_0_0
// CHECK: [[C1:%.*]] = apply [[F1]]<@opened("{{.*}}", any P) Self>([[O1]]) : $@convention(method) <τ_0_0 where τ_0_0 : P> (@guaranteed τ_0_0) -> @owned τ_0_0
// CHECK: [[E2:%.*]] = init_existential_ref [[C1]] : $@opened("{{.*}}", any P) Self : $@opened("{{.*}}", any P) Self, $any P
// CHECK: [[O2:%.*]] = open_existential_ref [[E2]] : $any P to $@opened("{{.*}}", any P) Self
// CHECK: apply [[F1]]<@opened("{{.*}}", any P) Self>([[O2]]) : $@convention(method) <τ_0_0 where τ_0_0 : P> (@guaranteed τ_0_0) -> @owned τ_0_0
// CHECK-LABEL: } // end sil function '$s32sil_combine_concrete_existential14testReturnSelfAA1P_pyF'
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
// CHECK-LABEL: sil @$s32sil_combine_concrete_existential29testWitnessReturnOptionalSelfAA2PP_pSgyF : $@convention(thin) () -> @owned Optional<any PP> {
// CHECK: [[EI:%.*]] = end_init_let_ref %0
// CHECK: [[E1:%.*]] = init_existential_ref [[EI]] : $CC : $CC, $any PP
// CHECK: [[O1:%.*]] = open_existential_ref [[E1]] : $any PP to $@opened("{{.*}}", any PP) Self
// CHECK: [[E2:%.*]] = init_existential_ref %{{.*}} : $@opened("{{.*}}", any PP) Self : $@opened("{{.*}}", any PP) Self, $any PP
// CHECK: [[O2:%.*]] = open_existential_ref [[E2]] : $any PP to $@opened("{{.*}}", any PP) Self
// CHECK: [[W:%.*]] = witness_method $@opened("{{.*}}", any PP) Self, #PP.returnOptionalSelf : <Self where Self : PP> (Self) -> () -> Self?, [[O1]] : $@opened("{{.*}}", any PP) Self : $@convention(witness_method: PP) <τ_0_0 where τ_0_0 : PP> (@guaranteed τ_0_0) -> @owned Optional<τ_0_0>
// CHECK: apply [[W]]<@opened("{{.*}}", any PP) Self>([[O2]]) : $@convention(witness_method: PP) <τ_0_0 where τ_0_0 : PP> (@guaranteed τ_0_0) -> @owned Optional<τ_0_0>
// CHECK-LABEL: } // end sil function '$s32sil_combine_concrete_existential29testWitnessReturnOptionalSelfAA2PP_pSgyF'
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

struct SS: PPP {
  func returnsOptionalIndirect() -> SS? {
    return self
  }
}

// The first apply has been devirtualized and inlined. The second remains unspecialized.
// CHECK-LABEL: sil @$s32sil_combine_concrete_existential37testWitnessReturnOptionalIndirectSelfyyF : $@convention(thin) () -> () {
// CHECK: [[O1:%.*]] = open_existential_addr immutable_access %{{.*}} : $*any PPP to $*@opened("{{.*}}", any PPP) Self
// CHECK: switch_enum_addr %{{.*}} : $*Optional<@opened("{{.*}}", any PPP) Self>, case #Optional.some!enumelt: bb{{.*}}, case #Optional.none!enumelt: bb{{.*}}
// CHECK: [[O2:%.*]] = open_existential_addr immutable_access %{{.*}} : $*any PPP to $*@opened("{{.*}}", any PPP) Self
// CHECK: [[W:%.*]] = witness_method $@opened("{{.*}}", any PPP) Self, #PPP.returnsOptionalIndirect : <Self where Self : PPP> (Self) -> () -> Self?, [[O1]] : $*@opened("{{.*}}", any PPP) Self : $@convention(witness_method: PPP) <τ_0_0 where τ_0_0 : PPP> (@in_guaranteed τ_0_0) -> @out Optional<τ_0_0>
// CHECK: apply [[W]]<@opened("{{.*}}", any PPP) Self>(%{{.*}}, [[O2]]) : $@convention(witness_method: PPP) <τ_0_0 where τ_0_0 : PPP> (@in_guaranteed τ_0_0) -> @out Optional<τ_0_0>
// CHECK-LABEL: } // end sil function '$s32sil_combine_concrete_existential37testWitnessReturnOptionalIndirectSelfyyF'
public func testWitnessReturnOptionalIndirectSelf() {
  let p: PPP = S()
  _ = p.returnsOptionalIndirect()?.returnsOptionalIndirect()
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
// CHECK-LABEL: sil {{.*}}@$s32sil_combine_concrete_existential32testExtensionProtocolComposition1cyAA4C_PQC_tF : $@convention(thin) (@guaranteed C_PQ) -> () {
// CHECK-NOT: init_existential_ref
// CHECK-NOT: function_ref
// CHECK-NOT: apply
// CHECK: } // end sil function '$s32sil_combine_concrete_existential32testExtensionProtocolComposition1cyAA4C_PQC_tF'
public func testExtensionProtocolComposition(c: C_PQ) {
  let pp: P & Q = c
  pp.witnessComposition()
}
