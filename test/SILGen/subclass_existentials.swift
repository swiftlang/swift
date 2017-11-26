// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-silgen -parse-as-library -primary-file %s -verify | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -parse-as-library -primary-file %s

// Note: we pass -verify above to ensure there are no spurious
// compiler warnings relating to casts.

protocol Q {}

class Base<T> : Q {
  required init(classInit: ()) {}
  func classSelfReturn() -> Self {
    return self
  }
  static func classSelfReturn() -> Self {
    return self.init(classInit: ())
  }
}

protocol P {
  init(protocolInit: ())
  func protocolSelfReturn() -> Self
  static func protocolSelfReturn() -> Self
}

class Derived : Base<Int>, P {
  required init(protocolInit: ()) {
    super.init(classInit: ())
  }

  required init(classInit: ()) {
    super.init(classInit: ())
  }

  func protocolSelfReturn() -> Self {
    return self
  }
  static func protocolSelfReturn() -> Self {
    return self.init(classInit: ())
  }
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
  // CHECK: [[RESULT_BOX:%.*]] = alloc_stack $@opened("{{.*}}") Base<Int> & P
  // CHECK: [[SELF_BOX:%.*]] = alloc_stack $@opened("{{.*}}") Base<Int> & P
  // CHECK: store_borrow [[PAYLOAD]] to [[SELF_BOX]] : $*@opened("{{.*}}") Base<Int> & P
  // CHECK: [[METHOD:%.*]] = witness_method $@opened("{{.*}}") Base<Int> & P, #P.protocolSelfReturn!1 : <Self where Self : P> (Self) -> () -> @dynamic_self Self, [[PAYLOAD]] : $@opened("{{.*}}") Base<Int> & P : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // CHECK: apply [[METHOD]]<@opened("{{.*}}") Base<Int> & P>([[RESULT_BOX]], [[SELF_BOX]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // CHECK: dealloc_stack [[SELF_BOX]] : $*@opened("{{.*}}") Base<Int> & P
  // CHECK: [[RESULT_REF:%.*]] = load [take] [[RESULT_BOX]] : $*@opened("{{.*}}") Base<Int> & P
  // CHECK: [[RESULT:%.*]] = init_existential_ref [[RESULT_REF]] : $@opened("{{.*}}") Base<Int> & P : $@opened("{{.*}}") Base<Int> & P, $Base<Int> & P
  // CHECK: destroy_value [[RESULT]] : $Base<Int> & P
  // CHECK: dealloc_stack [[RESULT_BOX]] : $*@opened("{{.*}}") Base<Int> & P
  // CHECK: end_borrow [[BORROW]] from %0 : $Base<Int> & P
  let _: Base<Int> & P = baseAndP.protocolSelfReturn()

  // CHECK: [[METATYPE:%.*]] = open_existential_metatype %1 : $@thick (Base<Int> & P).Type to $@thick (@opened("{{.*}}") (Base<Int> & P)).Type
  // CHECK: [[METATYPE_REF:%.*]] = upcast [[METATYPE]] : $@thick (@opened("{{.*}}") (Base<Int> & P)).Type to $@thick Base<Int>.Type
  // CHECK: [[METHOD:%.*]] = function_ref @_T021subclass_existentials4BaseC15classSelfReturnACyxGXDyFZ : $@convention(method) <τ_0_0> (@thick Base<τ_0_0>.Type) -> @owned Base<τ_0_0>
  // CHECK: [[RESULT_REF2:%.*]] = apply [[METHOD]]<Int>([[METATYPE_REF]])
  // CHECK: [[RESULT_REF:%.*]] = unchecked_ref_cast [[RESULT_REF2]] : $Base<Int> to $@opened("{{.*}}") (Base<Int> & P)
  // CHECK: [[RESULT:%.*]] = init_existential_ref [[RESULT_REF]] : $@opened("{{.*}}") (Base<Int> & P) : $@opened("{{.*}}") (Base<Int> & P), $Base<Int> & P
  // CHECK: destroy_value [[RESULT]]
  let _: Base<Int> & P = baseAndPType.classSelfReturn()

  // CHECK: [[METATYPE:%.*]] = open_existential_metatype %1 : $@thick (Base<Int> & P).Type to $@thick (@opened("{{.*}}") (Base<Int> & P)).Type
  // CHECK: [[RESULT:%.*]] = alloc_stack $@opened("{{.*}}") (Base<Int> & P)
  // CHECK: [[METHOD:%.*]] = witness_method $@opened("{{.*}}") (Base<Int> & P), #P.protocolSelfReturn!1 : <Self where Self : P> (Self.Type) -> () -> @dynamic_self Self, [[METATYPE]] : $@thick (@opened("{{.*}}") (Base<Int> & P)).Type : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@thick τ_0_0.Type) -> @out τ_0_0
  // CHECK: apply [[METHOD]]<@opened("{{.*}}") (Base<Int> & P)>([[RESULT]], [[METATYPE]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@thick τ_0_0.Type) -> @out τ_0_0
  // CHECK: [[RESULT_REF:%.*]] = load [take] [[RESULT]] : $*@opened("{{.*}}") (Base<Int> & P)
  // CHECK: [[RESULT_VALUE:%.*]] = init_existential_ref [[RESULT_REF]] : $@opened("{{.*}}") (Base<Int> & P) : $@opened("{{.*}}") (Base<Int> & P), $Base<Int> & P
  // CHECK: destroy_value [[RESULT_VALUE]]
  // CHECK: dealloc_stack [[RESULT]]
  let _: Base<Int> & P = baseAndPType.protocolSelfReturn()

  // Partial applications
  let _: () -> (Base<Int> & P) = baseAndP.classSelfReturn
  let _: () -> (Base<Int> & P) = baseAndP.protocolSelfReturn

  let _: () -> (Base<Int> & P) = baseAndPType.classSelfReturn
  let _: () -> (Base<Int> & P) = baseAndPType.protocolSelfReturn

  let _: () -> (Base<Int> & P) = baseAndPType.init(classInit:)
  let _: () -> (Base<Int> & P) = baseAndPType.init(protocolInit:)

  // CHECK:      return
  // CHECK-NEXT: }
}

protocol PropertyP {
  var p: PropertyP & PropertyC { get set }

  subscript(key: Int) -> Int { get set }
}

class PropertyC {
  var c: PropertyP & PropertyC {
    get {
      return self as! PropertyP & PropertyC
    }
    set { }
  }

  subscript(key: (Int, Int)) -> Int {
    get {
      return 0
    } set { }
  }
}

// CHECK-LABEL: sil hidden @_T021subclass_existentials16propertyAccessesyAA9PropertyP_AA0E1CCXcF : $@convention(thin) (@owned PropertyC & PropertyP) -> () {
func propertyAccesses(_ x: PropertyP & PropertyC) {
  var xx = x
  xx.p.p = x
  xx.c.c = x

  propertyAccesses(xx.p)
  propertyAccesses(xx.c)

  _ = xx[1]
  xx[1] = 1
  xx[1] += 1

  _ = xx[(1, 2)]
  xx[(1, 2)] = 1
  xx[(1, 2)] += 1
}

// CHECK-LABEL: sil hidden @_T021subclass_existentials19functionConversionsyAA1P_AA4BaseCySiGXcyc07returnsE4AndP_AaC_AFXcXpyc0feG5PTypeAA7DerivedCyc0fI0AJmyc0fI4TypeAA1R_AJXcyc0fiG1RAaM_AJXcXpyc0fiG5RTypetF : $@convention(thin) (@owned @callee_guaranteed () -> @owned Base<Int> & P, @owned @callee_guaranteed () -> @thick (Base<Int> & P).Type, @owned @callee_guaranteed () -> @owned Derived, @owned @callee_guaranteed () -> @thick Derived.Type, @owned @callee_guaranteed () -> @owned Derived & R, @owned @callee_guaranteed () -> @thick (Derived & R).Type) -> () {
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

  // CHECK:      return %
  // CHECK-NEXT: }
}

// CHECK-LABEL: sil hidden @_T021subclass_existentials9downcastsyAA1P_AA4BaseCySiGXc8baseAndP_AA7DerivedC7derivedAaC_AFXcXp0eF5PTypeAIm0H4TypetF : $@convention(thin) (@owned Base<Int> & P, @owned Derived, @thick (Base<Int> & P).Type, @thick Derived.Type) -> () {
func downcasts(
  baseAndP: Base<Int> & P,
  derived: Derived,
  baseAndPType: (Base<Int> & P).Type,
  derivedType: Derived.Type) {

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %0 : $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<Int> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<Int> & P to $Derived
  let _ = baseAndP as? Derived

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %0 : $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<Int> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<Int> & P to $Derived
  let _ = baseAndP as! Derived

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %0 : $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<Int> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<Int> & P to $Derived & R
  let _ = baseAndP as? (Derived & R)

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %0 : $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<Int> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<Int> & P to $Derived & R
  let _ = baseAndP as! (Derived & R)

  // CHECK:      checked_cast_br %3 : $@thick Derived.Type to $@thick (Derived & R).Type
  let _ = derivedType as? (Derived & R).Type

  // CHECK:      unconditional_checked_cast %3 : $@thick Derived.Type to $@thick (Derived & R).Type
  let _ = derivedType as! (Derived & R).Type

  // CHECK:      checked_cast_br %2 : $@thick (Base<Int> & P).Type to $@thick Derived.Type
  let _ = baseAndPType as? Derived.Type

  // CHECK:      unconditional_checked_cast %2 : $@thick (Base<Int> & P).Type to $@thick Derived.Type
  let _ = baseAndPType as! Derived.Type

  // CHECK:      checked_cast_br %2 : $@thick (Base<Int> & P).Type to $@thick (Derived & R).Type
  let _ = baseAndPType as? (Derived & R).Type

  // CHECK:      unconditional_checked_cast %2 : $@thick (Base<Int> & P).Type to $@thick (Derived & R).Type
  let _ = baseAndPType as! (Derived & R).Type

  // CHECK:      return
  // CHECK-NEXT: }
}

// CHECK-LABEL: sil hidden @_T021subclass_existentials16archetypeUpcastsyq_9baseTAndP_q0_0E7IntAndPq1_7derivedtAA4BaseCyxGRb_AA1PR_AGySiGRb0_AaIR0_AA7DerivedCRb1_r2_lF : $@convention(thin) <T, BaseTAndP, BaseIntAndP, DerivedT where BaseTAndP : Base<T>, BaseTAndP : P, BaseIntAndP : Base<Int>, BaseIntAndP : P, DerivedT : Derived> (@owned BaseTAndP, @owned BaseIntAndP, @owned DerivedT) -> () {
func archetypeUpcasts<T,
                      BaseTAndP : Base<T> & P,
                      BaseIntAndP : Base<Int> & P,
                      DerivedT : Derived>(
  baseTAndP: BaseTAndP,
  baseIntAndP : BaseIntAndP,
  derived : DerivedT) {

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %0 : $BaseTAndP
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $BaseTAndP
  // CHECK-NEXT: init_existential_ref [[COPIED]] : $BaseTAndP : $BaseTAndP, $Base<T> & P
  let _: Base<T> & P = baseTAndP

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %1 : $BaseIntAndP
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $BaseIntAndP
  // CHECK-NEXT: init_existential_ref [[COPIED]] : $BaseIntAndP : $BaseIntAndP, $Base<Int> & P
  let _: Base<Int> & P = baseIntAndP

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %2 : $DerivedT
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $DerivedT
  // CHECK-NEXT: init_existential_ref [[COPIED]] : $DerivedT : $DerivedT, $Base<Int> & P
  let _: Base<Int> & P = derived

  // CHECK:      return
  // CHECK-NEXT: }
}

// CHECK-LABEL: sil hidden @_T021subclass_existentials18archetypeDowncastsyx1s_q_1tq0_2ptq1_5baseTq2_0F3Intq3_0f6TAndP_C0q4_0fg5AndP_C0q5_08derived_C0AA1R_AA7DerivedCXc0ji2R_C0AA1P_AA4BaseCyq_GXc0fH10P_concreteAaO_AQySiGXc0fgi2P_M0tAaOR0_ARRb1_ATRb2_ARRb3_AaOR3_ATRb4_AaOR4_AMRb5_r6_lF : $@convention(thin) <S, T, PT, BaseT, BaseInt, BaseTAndP, BaseIntAndP, DerivedT where PT : P, BaseT : Base<T>, BaseInt : Base<Int>, BaseTAndP : Base<T>, BaseTAndP : P, BaseIntAndP : Base<Int>, BaseIntAndP : P, DerivedT : Derived> (@in S, @in T, @in PT, @owned BaseT, @owned BaseInt, @owned BaseTAndP, @owned BaseIntAndP, @owned DerivedT, @owned Derived & R, @owned Base<T> & P, @owned Base<Int> & P) -> () {
func archetypeDowncasts<S,
                        T,
                        PT : P,
                        BaseT : Base<T>,
                        BaseInt : Base<Int>,
                        BaseTAndP : Base<T> & P,
                        BaseIntAndP : Base<Int> & P,
                        DerivedT : Derived>(
  s: S,
  t: T,
  pt: PT,
  baseT : BaseT,
  baseInt : BaseInt,

  baseTAndP_archetype: BaseTAndP,
  baseIntAndP_archetype : BaseIntAndP,
  derived_archetype : DerivedT,
  derivedAndR_archetype : Derived & R,

  baseTAndP_concrete: Base<T> & P,
  baseIntAndP_concrete: Base<Int> & P) {

  // CHECK:      [[COPY:%.*]] = alloc_stack $S
  // CHECK-NEXT: copy_addr %0 to [initialization] [[COPY]] : $*S
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Base<T> & P
  // CHECK-NEXT: checked_cast_addr_br take_always S in [[COPY]] : $*S to Base<T> & P in [[RESULT]] : $*Base<T> & P
  let _ = s as? (Base<T> & P)

  // CHECK:      [[COPY:%.*]] = alloc_stack $S
  // CHECK-NEXT: copy_addr %0 to [initialization] [[COPY]] : $*S
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Base<T> & P
  // CHECK-NEXT: unconditional_checked_cast_addr S in [[COPY]] : $*S to Base<T> & P in [[RESULT]] : $*Base<T> & P
  let _ = s as! (Base<T> & P)

  // CHECK:      [[COPY:%.*]] = alloc_stack $S
  // CHECK-NEXT: copy_addr %0 to [initialization] [[COPY]] : $*S
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Base<Int> & P
  // CHECK-NEXT: checked_cast_addr_br take_always S in [[COPY]] : $*S to Base<Int> & P in [[RESULT]] : $*Base<Int> & P
  let _ = s as? (Base<Int> & P)

  // CHECK:      [[COPY:%.*]] = alloc_stack $S
  // CHECK-NEXT: copy_addr %0 to [initialization] [[COPY]] : $*S
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Base<Int> & P
  // CHECK-NEXT: unconditional_checked_cast_addr S in [[COPY]] : $*S to Base<Int> & P in [[RESULT]] : $*Base<Int> & P
  let _ = s as! (Base<Int> & P)

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %5 : $BaseTAndP
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $BaseTAndP
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $BaseTAndP to $Derived & R
  let _ = baseTAndP_archetype as? (Derived & R)

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %5 : $BaseTAndP
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $BaseTAndP
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $BaseTAndP to $Derived & R
  let _ = baseTAndP_archetype as! (Derived & R)

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %9 : $Base<T> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<T> & P
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $Base<T> & P
  // CHECK-NEXT: store [[COPIED]] to [init] [[COPY]] : $*Base<T> & P
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Optional<S>
  // CHECK-NEXT: [[PAYLOAD:%.*]] = init_enum_data_addr [[RESULT]] : $*Optional<S>, #Optional.some
  // CHECK-NEXT: checked_cast_addr_br take_always Base<T> & P in [[COPY]] : $*Base<T> & P to S in [[PAYLOAD]] : $*S
  let _ = baseTAndP_concrete as? S

  // CHECK:      [[COPY:%.*]] = alloc_stack $Base<T> & P
  // CHECK-NEXT: [[BORROWED:%.*]] = begin_borrow %9 : $Base<T> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<T> & P
  // CHECK-NEXT: store [[COPIED]] to [init] [[COPY]] : $*Base<T> & P
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $S
  // CHECK-NEXT: unconditional_checked_cast_addr Base<T> & P in [[COPY]] : $*Base<T> & P to S in [[RESULT]] : $*S
  let _ = baseTAndP_concrete as! S

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %9 : $Base<T> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<T> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<T> & P to $BaseT
  let _ = baseTAndP_concrete as? BaseT

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %9 : $Base<T> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<T> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<T> & P to $BaseT
  let _ = baseTAndP_concrete as! BaseT

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %9 : $Base<T> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<T> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<T> & P to $BaseInt
  let _ = baseTAndP_concrete as? BaseInt

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %9 : $Base<T> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<T> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<T> & P to $BaseInt
  let _ = baseTAndP_concrete as! BaseInt

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %9 : $Base<T> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<T> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<T> & P to $BaseTAndP
  let _ = baseTAndP_concrete as? BaseTAndP

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %9 : $Base<T> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<T> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<T> & P to $BaseTAndP
  let _ = baseTAndP_concrete as! BaseTAndP

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %6 : $BaseIntAndP
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $BaseIntAndP
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $BaseIntAndP to $Derived & R
  let _ = baseIntAndP_archetype as? (Derived & R)

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %6 : $BaseIntAndP
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $BaseIntAndP
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $BaseIntAndP to $Derived & R
  let _ = baseIntAndP_archetype as! (Derived & R)

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %10 : $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<Int> & P
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $Base<Int> & P
  // CHECK-NEXT: store [[COPIED]] to [init] [[COPY]] : $*Base<Int> & P
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Optional<S>
  // CHECK-NEXT: [[PAYLOAD:%.*]] = init_enum_data_addr [[RESULT]] : $*Optional<S>, #Optional.some
  // CHECK-NEXT: checked_cast_addr_br take_always Base<Int> & P in [[COPY]] : $*Base<Int> & P to S in [[PAYLOAD]] : $*S
  let _ = baseIntAndP_concrete as? S

  // CHECK:      [[COPY:%.*]] = alloc_stack $Base<Int> & P
  // CHECK-NEXT: [[BORROWED:%.*]] = begin_borrow %10 : $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<Int> & P
  // CHECK-NEXT: store [[COPIED]] to [init] [[COPY]] : $*Base<Int> & P
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $S
  // CHECK-NEXT: unconditional_checked_cast_addr Base<Int> & P in [[COPY]] : $*Base<Int> & P to S in [[RESULT]] : $*S
  let _ = baseIntAndP_concrete as! S

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %10 : $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<Int> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<Int> & P to $DerivedT
  let _ = baseIntAndP_concrete as? DerivedT

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %10 : $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<Int> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<Int> & P to $DerivedT
  let _ = baseIntAndP_concrete as! DerivedT

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %10 : $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<Int> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<Int> & P to $BaseT
  let _ = baseIntAndP_concrete as? BaseT

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %10 : $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<Int> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<Int> & P to $BaseT
  let _ = baseIntAndP_concrete as! BaseT

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %10 : $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<Int> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<Int> & P to $BaseInt
  let _ = baseIntAndP_concrete as? BaseInt

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %10 : $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<Int> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<Int> & P to $BaseInt
  let _ = baseIntAndP_concrete as! BaseInt

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %10 : $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<Int> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<Int> & P to $BaseTAndP
  let _ = baseIntAndP_concrete as? BaseTAndP

  // CHECK:      [[BORROWED:%.*]] = begin_borrow %10 : $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[BORROWED]] : $Base<Int> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<Int> & P to $BaseTAndP
  let _ = baseIntAndP_concrete as! BaseTAndP

  // CHECK:      return
  // CHECK-NEXT: }
}
