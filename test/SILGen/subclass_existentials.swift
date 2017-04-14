// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-silgen -parse-as-library -enable-experimental-subclass-existentials %s | %FileCheck %s

protocol Q {}

class Base<T> : Q {
  func classSelfReturn() -> Self {}
  static func classSelfReturn() -> Self {}
}

protocol P {
  func protocolSelfReturn() -> Self
  static func protocolSelfReturn() -> Self
}

class Derived : Base<Int>, P {
  func protocolSelfReturn() -> Self {}
  static func protocolSelfReturn() -> Self {}
}

protocol R {}

// CHECK-LABEL: sil hidden @_T021subclass_existentials11conversionsyAA1P_AA4BaseCySiGXc8baseAndP_AA7DerivedC7derivedAA1R_AIXc0hF1RAaC_AFXcXp0eF5PTypeAIm0H4TypeAaK_AIXcXp0hF5RTypetF : $@convention(thin) (@owned Base<Int> & P, @owned Derived, @owned Derived & R, @thick (Base<Int> & P).Type, @thick Derived.Type, @thick (Derived & R).Type) -> () {

func conversions(
  baseAndP: Base<Int> & P,
  derived: Derived,
  derivedAndR: Derived & R,

  baseAndPType: (Base<Int> & P).Type,
  derivedType: Derived.Type,
  derivedAndRType: (Derived & R).Type) {

  // Values

  // CHECK: [[BORROWED:%.*]] = begin_borrow %0 : $Base<Int> & P
  // CHECK: [[PAYLOAD:%.*]] = open_existential_ref [[BORROWED]] : $Base<Int> & P
  // CHECK: [[REF:%.*]] = copy_value [[PAYLOAD]]
  // CHECK: [[BASE:%.*]] = upcast [[REF]] : $@opened("{{.*}}") Base<Int> & P to $Base<Int>
  // CHECK: destroy_value [[BASE]] : $Base<Int>
  // CHECK: end_borrow [[BORROWED]] from %0 : $Base<Int> & P
  let _: Base<Int> = baseAndP

  // CHECK: [[BORROW:%.*]] = begin_borrow %0 : $Base<Int> & P
  // CHECK: [[PAYLOAD:%.*]] = open_existential_ref [[BORROW]] : $Base<Int> & P
  // CHECK: [[RESULT:%.*]] = alloc_stack $P
  // CHECK: [[RESULT_PAYLOAD:%.*]] = init_existential_addr [[RESULT]] : $*P, $@opened("{{.*}}") Base<Int> & P
  // CHECK: [[REF:%.*]] = copy_value [[PAYLOAD]]
  // CHECK: store [[REF]] to [init] [[RESULT_PAYLOAD]]
  // CHECK: destroy_addr [[RESULT]] : $*P
  // CHECK: dealloc_stack [[RESULT]] : $*P
  // CHECK: end_borrow [[BORROW]] from %0 : $Base<Int> & P
  let _: P = baseAndP

  // CHECK: [[BORROW:%.*]] = begin_borrow %0 : $Base<Int> & P
  // CHECK: [[PAYLOAD:%.*]] = open_existential_ref [[BORROW]] : $Base<Int> & P
  // CHECK: [[RESULT:%.*]] = alloc_stack $Q
  // CHECK: [[RESULT_PAYLOAD:%.*]] = init_existential_addr [[RESULT]] : $*Q, $@opened("{{.*}}") Base<Int> & P
  // CHECK: [[REF:%.*]] = copy_value [[PAYLOAD]]
  // CHECK: store [[REF]] to [init] [[RESULT_PAYLOAD]]
  // CHECK: destroy_addr [[RESULT]] : $*Q
  // CHECK: dealloc_stack [[RESULT]] : $*Q
  // CHECK: end_borrow [[BORROW]] from %0 : $Base<Int> & P
  let _: Q = baseAndP

  // CHECK: [[BORROW:%.*]] = begin_borrow %2 : $Derived & R
  // CHECK: [[PAYLOAD:%.*]] = open_existential_ref [[BORROW]] : $Derived & R
  // CHECK: [[REF:%.*]] = copy_value [[PAYLOAD]]
  // CHECK: [[RESULT:%.*]] = init_existential_ref [[REF]] : $@opened("{{.*}}") Derived & R : $@opened("{{.*}}") Derived & R, $Base<Int> & P
  // CHECK: destroy_value [[RESULT]]
  // CHECK: end_borrow [[BORROW]] from %2 : $Derived & R
  let _: Base<Int> & P = derivedAndR

  // Metatypes

  // CHECK: [[PAYLOAD:%.*]] = open_existential_metatype %3 : $@thick (Base<Int> & P).Type to $@thick (@opened("{{.*}}") (Base<Int> & P)).Type
  // CHECK: [[RESULT:%.*]] = upcast [[PAYLOAD]] : $@thick (@opened("{{.*}}") (Base<Int> & P)).Type to $@thick Base<Int>.Type
  let _: Base<Int>.Type = baseAndPType

  // CHECK: [[PAYLOAD:%.*]] = open_existential_metatype %3 : $@thick (Base<Int> & P).Type to $@thick (@opened("{{.*}}") (Base<Int> & P)).Type
  // CHECK: [[RESULT:%.*]] = init_existential_metatype [[PAYLOAD]] : $@thick (@opened("{{.*}}") (Base<Int> & P)).Type, $@thick P.Type
  let _: P.Type = baseAndPType

  // CHECK: [[PAYLOAD:%.*]] = open_existential_metatype %3 : $@thick (Base<Int> & P).Type to $@thick (@opened("{{.*}}") (Base<Int> & P)).Type
  // CHECK: [[RESULT:%.*]] = init_existential_metatype [[PAYLOAD]] : $@thick (@opened("{{.*}}") (Base<Int> & P)).Type, $@thick Q.Type
  let _: Q.Type = baseAndPType

  // CHECK: [[RESULT:%.*]] = init_existential_metatype %4 : $@thick Derived.Type, $@thick (Base<Int> & P).Type
  let _: (Base<Int> & P).Type = derivedType

  // CHECK: [[PAYLOAD:%.*]] = open_existential_metatype %5 : $@thick (Derived & R).Type to $@thick (@opened("{{.*}}") (Derived & R)).Type
  // CHECK: [[RESULT:%.*]] = init_existential_metatype [[PAYLOAD]] : $@thick (@opened("{{.*}}") (Derived & R)).Type, $@thick (Base<Int> & P).Type
  let _: (Base<Int> & P).Type = derivedAndRType

  // CHECK: return
}

// CHECK-LABEL: sil hidden @_T021subclass_existentials11methodCallsyAA1P_AA4BaseCySiGXc8baseAndP_AaC_AFXcXp0fG5PTypetF : $@convention(thin) (@owned Base<Int> & P, @thick (Base<Int> & P).Type) -> () {

func methodCalls(
  baseAndP: Base<Int> & P,
  baseAndPType: (Base<Int> & P).Type) {

  // CHECK: [[BORROW:%.*]] = begin_borrow %0 : $Base<Int> & P
  // CHECK: [[PAYLOAD:%.*]] = open_existential_ref [[BORROW]] : $Base<Int> & P to $@opened("{{.*}}") Base<Int> & P
  // CHECK: [[REF:%.*]] = copy_value [[PAYLOAD]] : $@opened("{{.*}}") Base<Int> & P
  // CHECK: [[CLASS_REF:%.*]] = upcast [[REF]] : $@opened("{{.*}}") Base<Int> & P to $Base<Int>
  // CHECK: [[METHOD:%.*]] = class_method [[CLASS_REF]] : $Base<Int>, #Base.classSelfReturn!1 : <T> (Base<T>) -> () -> @dynamic_self Base<T>, $@convention(method) <τ_0_0> (@guaranteed Base<τ_0_0>) -> @owned Base<τ_0_0>
  // CHECK: [[RESULT_CLASS_REF:%.*]] = apply [[METHOD]]<Int>([[CLASS_REF]]) : $@convention(method) <τ_0_0> (@guaranteed Base<τ_0_0>) -> @owned Base<τ_0_0>
  // CHECK: destroy_value [[CLASS_REF]] : $Base<Int>
  // CHECK: [[RESULT_REF:%.*]] = unchecked_ref_cast [[RESULT_CLASS_REF]] : $Base<Int> to $@opened("{{.*}}") Base<Int> & P
  // CHECK: [[RESULT:%.*]] = init_existential_ref [[RESULT_REF]] : $@opened("{{.*}}") Base<Int> & P : $@opened("{{.*}}") Base<Int> & P, $Base<Int> & P
  // CHECK: destroy_value [[RESULT]] : $Base<Int> & P
  // CHECK: end_borrow [[BORROW]] from %0 : $Base<Int> & P
  let _: Base<Int> & P = baseAndP.classSelfReturn()

  // CHECK: [[BORROW:%.*]] = begin_borrow %0 : $Base<Int> & P
  // CHECK: [[PAYLOAD:%.*]] = open_existential_ref [[BORROW]] : $Base<Int> & P to $@opened("{{.*}}") Base<Int> & P
  // CHECK: [[SELF_BOX:%.*]] = alloc_stack $@opened("{{.*}}") Base<Int> & P
  // CHECK: store [[PAYLOAD]] to [init] [[SELF_BOX]] : $*@opened("{{.*}}") Base<Int> & P
  // CHECK: [[METHOD:%.*]] = witness_method $@opened("{{.*}}") Base<Int> & P, #P.protocolSelfReturn!1 : <Self where Self : P> (Self) -> () -> @dynamic_self Self, %16 : $@opened("{{.*}}") Base<Int> & P : $@convention(witness_method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // CHECK: [[RESULT_BOX:%.*]] = alloc_stack $@opened("{{.*}}") Base<Int> & P
  // CHECK: apply [[METHOD]]<@opened("{{.*}}") Base<Int> & P>([[RESULT_BOX]], [[SELF_BOX]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // CHECK: [[RESULT_REF:%.*]] = load [take] %20 : $*@opened("{{.*}}") Base<Int> & P
  // CHECK: [[RESULT:%.*]] = init_existential_ref [[RESULT_REF]] : $@opened("{{.*}}") Base<Int> & P : $@opened("{{.*}}") Base<Int> & P, $Base<Int> & P
  // CHECK: destroy_value [[RESULT]] : $Base<Int> & P
  // CHECK: dealloc_stack [[RESULT_BOX]] : $*@opened("{{.*}}") Base<Int> & P
  // CHECK: dealloc_stack [[SELF_BOX]] : $*@opened("{{.*}}") Base<Int> & P
  // CHECK: end_borrow [[BORROW]] from %0 : $Base<Int> & P
  let _: Base<Int> & P = baseAndP.protocolSelfReturn()

  let _: Base<Int> & P = baseAndPType.classSelfReturn()
  let _: Base<Int> & P = baseAndPType.protocolSelfReturn()

  // Partial applications
  let _: () -> (Base<Int> & P) = baseAndP.classSelfReturn
  let _: () -> (Base<Int> & P) = baseAndP.protocolSelfReturn

  let _: () -> (Base<Int> & P) = baseAndPType.classSelfReturn
  let _: () -> (Base<Int> & P) = baseAndPType.protocolSelfReturn
}

// CHECK-LABEL: sil hidden @_T021subclass_existentials19functionConversionsyAA1P_AA4BaseCySiGXcyc07returnsE4AndP_AaC_AFXcXpyc0feG5PTypeAA7DerivedCyc0fI0AJmyc0fI4TypeAA1R_AJXcyc0fiG1RAaM_AJXcXpyc0fiG5RTypetF : $@convention(thin) (@owned @callee_owned () -> @owned Base<Int> & P, @owned @callee_owned () -> @thick (Base<Int> & P).Type, @owned @callee_owned () -> @owned Derived, @owned @callee_owned () -> @thick Derived.Type, @owned @callee_owned () -> @owned Derived & R, @owned @callee_owned () -> @thick (Derived & R).Type) -> () {

func functionConversions(
  returnsBaseAndP: @escaping () -> (Base<Int> & P),
  returnsBaseAndPType: @escaping () -> (Base<Int> & P).Type,
  returnsDerived: @escaping () -> Derived,
  returnsDerivedType: @escaping () -> Derived.Type,
  returnsDerivedAndR: @escaping () -> Derived & R,
  returnsDerivedAndRType: @escaping () -> (Derived & R).Type) {

  let _: () -> Base<Int> = returnsBaseAndP
  let _: () -> Base<Int>.Type = returnsBaseAndPType

  let _: () -> P = returnsBaseAndP
  let _: () -> P.Type = returnsBaseAndPType

  let _: () -> (Base<Int> & P) = returnsDerived
  let _: () -> (Base<Int> & P).Type = returnsDerivedType

  let _: () -> Base<Int> = returnsDerivedAndR
  let _: () -> Base<Int>.Type = returnsDerivedAndRType

  let _: () -> (Base<Int> & P) = returnsDerivedAndR
  let _: () -> (Base<Int> & P).Type = returnsDerivedAndRType

  let _: () -> P = returnsDerivedAndR
  let _: () -> P.Type = returnsDerivedAndRType
}
