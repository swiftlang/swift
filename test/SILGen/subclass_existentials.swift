
// RUN: %target-swift-emit-silgen -module-name subclass_existentials -Xllvm -sil-full-demangle -parse-as-library -primary-file %s -verify | %FileCheck %s
// RUN: %target-swift-emit-ir -module-name subclass_existentials -parse-as-library -primary-file %s

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

// CHECK-LABEL: sil hidden @$S21subclass_existentials11conversions8baseAndP7derived0fE1R0dE5PType0F4Type0fE5RTypeyAA1P_AA4BaseCySiGXc_AA7DerivedCAA1R_ANXcAaI_ALXcXpANmAaO_ANXcXptF : $@convention(thin) (@guaranteed Base<Int> & P, @guaranteed Derived, @guaranteed Derived & R, @thick (Base<Int> & P).Type, @thick Derived.Type, @thick (Derived & R).Type) -> () {
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $Base<Int> & P,

func conversions(
  baseAndP: Base<Int> & P,
  derived: Derived,
  derivedAndR: Derived & R,

  baseAndPType: (Base<Int> & P).Type,
  derivedType: Derived.Type,
  derivedAndRType: (Derived & R).Type) {

  // Values

  // CHECK: [[PAYLOAD:%.*]] = open_existential_ref [[ARG0]] : $Base<Int> & P
  // CHECK: [[REF:%.*]] = copy_value [[PAYLOAD]]
  // CHECK: [[BASE:%.*]] = upcast [[REF]] : $@opened("{{.*}}") Base<Int> & P to $Base<Int>
  // CHECK: destroy_value [[BASE]] : $Base<Int>
  let _: Base<Int> = baseAndP

  // CHECK: [[PAYLOAD:%.*]] = open_existential_ref [[ARG0]] : $Base<Int> & P
  // CHECK: [[RESULT:%.*]] = alloc_stack $P
  // CHECK: [[RESULT_PAYLOAD:%.*]] = init_existential_addr [[RESULT]] : $*P, $@opened("{{.*}}") Base<Int> & P
  // CHECK: [[REF:%.*]] = copy_value [[PAYLOAD]]
  // CHECK: store [[REF]] to [init] [[RESULT_PAYLOAD]]
  // CHECK: destroy_addr [[RESULT]] : $*P
  // CHECK: dealloc_stack [[RESULT]] : $*P
  let _: P = baseAndP

  // CHECK: [[PAYLOAD:%.*]] = open_existential_ref [[ARG0]] : $Base<Int> & P
  // CHECK: [[RESULT:%.*]] = alloc_stack $Q
  // CHECK: [[RESULT_PAYLOAD:%.*]] = init_existential_addr [[RESULT]] : $*Q, $@opened("{{.*}}") Base<Int> & P
  // CHECK: [[REF:%.*]] = copy_value [[PAYLOAD]]
  // CHECK: store [[REF]] to [init] [[RESULT_PAYLOAD]]
  // CHECK: destroy_addr [[RESULT]] : $*Q
  // CHECK: dealloc_stack [[RESULT]] : $*Q
  let _: Q = baseAndP

  // CHECK: [[PAYLOAD:%.*]] = open_existential_ref [[ARG2:%.*]] : $Derived & R
  // CHECK: [[REF:%.*]] = copy_value [[PAYLOAD]]
  // CHECK: [[RESULT:%.*]] = init_existential_ref [[REF]] : $@opened("{{.*}}") Derived & R : $@opened("{{.*}}") Derived & R, $Base<Int> & P
  // CHECK: destroy_value [[RESULT]]
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

// CHECK-LABEL: sil hidden @$S21subclass_existentials11methodCalls8baseAndP0eF5PTypeyAA1P_AA4BaseCySiGXc_AaE_AHXcXptF : $@convention(thin) (@guaranteed Base<Int> & P, @thick (Base<Int> & P).Type) -> () {

func methodCalls(
  baseAndP: Base<Int> & P,
  baseAndPType: (Base<Int> & P).Type) {
  // CHECK: bb0([[ARG0:%.*]] : @guaranteed $Base<Int> & P,
  // CHECK: [[PAYLOAD:%.*]] = open_existential_ref [[ARG0]] : $Base<Int> & P to $@opened("{{.*}}") Base<Int> & P
  // CHECK: [[REF:%.*]] = copy_value [[PAYLOAD]] : $@opened("{{.*}}") Base<Int> & P
  // CHECK: [[CLASS_REF:%.*]] = upcast [[REF]] : $@opened("{{.*}}") Base<Int> & P to $Base<Int>
  // CHECK: [[METHOD:%.*]] = class_method [[CLASS_REF]] : $Base<Int>, #Base.classSelfReturn!1 : <T> (Base<T>) -> () -> @dynamic_self Base<T>, $@convention(method) <τ_0_0> (@guaranteed Base<τ_0_0>) -> @owned Base<τ_0_0>
  // CHECK: [[RESULT_CLASS_REF:%.*]] = apply [[METHOD]]<Int>([[CLASS_REF]]) : $@convention(method) <τ_0_0> (@guaranteed Base<τ_0_0>) -> @owned Base<τ_0_0>
  // CHECK: destroy_value [[CLASS_REF]] : $Base<Int>
  // CHECK: [[RESULT_REF:%.*]] = unchecked_ref_cast [[RESULT_CLASS_REF]] : $Base<Int> to $@opened("{{.*}}") Base<Int> & P
  // CHECK: [[RESULT:%.*]] = init_existential_ref [[RESULT_REF]] : $@opened("{{.*}}") Base<Int> & P : $@opened("{{.*}}") Base<Int> & P, $Base<Int> & P
  // CHECK: destroy_value [[RESULT]] : $Base<Int> & P
  let _: Base<Int> & P = baseAndP.classSelfReturn()

  // CHECK: [[PAYLOAD:%.*]] = open_existential_ref [[ARG0]] : $Base<Int> & P to $@opened("{{.*}}") Base<Int> & P
  // CHECK: [[SELF_BOX:%.*]] = alloc_stack $@opened("{{.*}}") Base<Int> & P
  // CHECK: store_borrow [[PAYLOAD]] to [[SELF_BOX]] : $*@opened("{{.*}}") Base<Int> & P
  // CHECK: [[METHOD:%.*]] = witness_method $@opened("{{.*}}") Base<Int> & P, #P.protocolSelfReturn!1 : <Self where Self : P> (Self) -> () -> @dynamic_self Self, [[PAYLOAD]] : $@opened("{{.*}}") Base<Int> & P : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // CHECK: [[RESULT_BOX:%.*]] = alloc_box
  // CHECK: [[RESULT_BUF:%.*]] = project_box [[RESULT_BOX]]
  // CHECK: apply [[METHOD]]<@opened("{{.*}}") Base<Int> & P>([[RESULT_BUF]], [[SELF_BOX]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // CHECK: dealloc_stack [[SELF_BOX]] : $*@opened("{{.*}}") Base<Int> & P
  // CHECK: [[RESULT_REF:%.*]] = load [take] [[RESULT_BUF]] : $*@opened("{{.*}}") Base<Int> & P
  // CHECK: [[RESULT:%.*]] = init_existential_ref [[RESULT_REF]] : $@opened("{{.*}}") Base<Int> & P : $@opened("{{.*}}") Base<Int> & P, $Base<Int> & P
  // CHECK: destroy_value [[RESULT]] : $Base<Int> & P
  // CHECK: dealloc_box [[RESULT_BOX]]
  let _: Base<Int> & P = baseAndP.protocolSelfReturn()

  // CHECK: [[METATYPE:%.*]] = open_existential_metatype %1 : $@thick (Base<Int> & P).Type to $@thick (@opened("{{.*}}") (Base<Int> & P)).Type
  // CHECK: [[METATYPE_REF:%.*]] = upcast [[METATYPE]] : $@thick (@opened("{{.*}}") (Base<Int> & P)).Type to $@thick Base<Int>.Type
  // CHECK: [[METHOD:%.*]] = function_ref @$S21subclass_existentials4BaseC15classSelfReturnACyxGXDyFZ : $@convention(method) <τ_0_0> (@thick Base<τ_0_0>.Type) -> @owned Base<τ_0_0>
  // CHECK: [[RESULT_REF2:%.*]] = apply [[METHOD]]<Int>([[METATYPE_REF]])
  // CHECK: [[RESULT_REF:%.*]] = unchecked_ref_cast [[RESULT_REF2]] : $Base<Int> to $@opened("{{.*}}") (Base<Int> & P)
  // CHECK: [[RESULT:%.*]] = init_existential_ref [[RESULT_REF]] : $@opened("{{.*}}") (Base<Int> & P) : $@opened("{{.*}}") (Base<Int> & P), $Base<Int> & P
  // CHECK: destroy_value [[RESULT]]
  let _: Base<Int> & P = baseAndPType.classSelfReturn()

  // CHECK: [[METATYPE:%.*]] = open_existential_metatype %1 : $@thick (Base<Int> & P).Type to $@thick (@opened("{{.*}}") (Base<Int> & P)).Type
  // CHECK: [[METHOD:%.*]] = witness_method $@opened("{{.*}}") (Base<Int> & P), #P.protocolSelfReturn!1 : <Self where Self : P> (Self.Type) -> () -> @dynamic_self Self, [[METATYPE]] : $@thick (@opened("{{.*}}") (Base<Int> & P)).Type : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@thick τ_0_0.Type) -> @out τ_0_0
  // CHECK: [[RESULT_BOX:%.*]] = alloc_box
  // CHECK: [[RESULT_BUF:%.*]] = project_box
  // CHECK: apply [[METHOD]]<@opened("{{.*}}") (Base<Int> & P)>([[RESULT_BUF]], [[METATYPE]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@thick τ_0_0.Type) -> @out τ_0_0
  // CHECK: [[RESULT_REF:%.*]] = load [take] [[RESULT_BUF]] : $*@opened("{{.*}}") (Base<Int> & P)
  // CHECK: [[RESULT_VALUE:%.*]] = init_existential_ref [[RESULT_REF]] : $@opened("{{.*}}") (Base<Int> & P) : $@opened("{{.*}}") (Base<Int> & P), $Base<Int> & P
  // CHECK: destroy_value [[RESULT_VALUE]]
  // CHECK: dealloc_box [[RESULT_BOX]]
  let _: Base<Int> & P = baseAndPType.protocolSelfReturn()

  // Partial applications
  let _: () -> (Base<Int> & P) = baseAndP.classSelfReturn
  let _: () -> (Base<Int> & P) = baseAndP.protocolSelfReturn

  let _: () -> (Base<Int> & P) = baseAndPType.classSelfReturn
  let _: () -> (Base<Int> & P) = baseAndPType.protocolSelfReturn

  let _: (()) -> (Base<Int> & P) = baseAndPType.init(classInit:)
  let _: (()) -> (Base<Int> & P) = baseAndPType.init(protocolInit:)

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

// CHECK-LABEL: sil hidden @$S21subclass_existentials16propertyAccessesyyAA9PropertyP_AA0E1CCXcF : $@convention(thin) (@guaranteed PropertyC & PropertyP) -> () {
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

// CHECK-LABEL: sil hidden @$S21subclass_existentials19functionConversions15returnsBaseAndP0efG5PType0E7Derived0eI4Type0eiG1R0eiG5RTypeyAA1P_AA0F0CySiGXcyc_AaI_ALXcXpycAA0I0CycANmycAA1R_ANXcycAaO_ANXcXpyctF : $@convention(thin) (@guaranteed @callee_guaranteed () -> @owned Base<Int> & P, @guaranteed @callee_guaranteed () -> @thick (Base<Int> & P).Type, @guaranteed @callee_guaranteed () -> @owned Derived, @guaranteed @callee_guaranteed () -> @thick Derived.Type, @guaranteed @callee_guaranteed () -> @owned Derived & R, @guaranteed @callee_guaranteed () -> @thick (Derived & R).Type) -> () {
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

// CHECK-LABEL: sil hidden @$S21subclass_existentials9downcasts8baseAndP7derived0dE5PType0F4TypeyAA1P_AA4BaseCySiGXc_AA7DerivedCAaG_AJXcXpALmtF : $@convention(thin) (@guaranteed Base<Int> & P, @guaranteed Derived, @thick (Base<Int> & P).Type, @thick Derived.Type) -> () {
func downcasts(
  baseAndP: Base<Int> & P,
  derived: Derived,
  baseAndPType: (Base<Int> & P).Type,
  derivedType: Derived.Type) {
  // CHECK: bb0([[ARG0:%.*]] : @guaranteed $Base<Int> & P, [[ARG1:%.*]] : @guaranteed $Derived, [[ARG2:%.*]] : @trivial $@thick (Base<Int> & P).Type, [[ARG3:%.*]] : @trivial $@thick Derived.Type):
  // CHECK: [[COPIED:%.*]] = copy_value [[ARG0]] : $Base<Int> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<Int> & P to $Derived
  let _ = baseAndP as? Derived

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG0]] : $Base<Int> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<Int> & P to $Derived
  let _ = baseAndP as! Derived

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG0]] : $Base<Int> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<Int> & P to $Derived & R
  let _ = baseAndP as? (Derived & R)

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG0]] : $Base<Int> & P
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

// CHECK-LABEL: sil hidden @$S21subclass_existentials16archetypeUpcasts9baseTAndP0E7IntAndP7derivedyq__q0_q1_tAA4BaseCyxGRb_AA1PR_AGySiGRb0_AaIR0_AA7DerivedCRb1_r2_lF : $@convention(thin) <T, BaseTAndP, BaseIntAndP, DerivedT where BaseTAndP : Base<T>, BaseTAndP : P, BaseIntAndP : Base<Int>, BaseIntAndP : P, DerivedT : Derived> (@guaranteed BaseTAndP, @guaranteed BaseIntAndP, @guaranteed DerivedT) -> () {
func archetypeUpcasts<T,
                      BaseTAndP : Base<T> & P,
                      BaseIntAndP : Base<Int> & P,
                      DerivedT : Derived>(
  baseTAndP: BaseTAndP,
  baseIntAndP : BaseIntAndP,
  derived : DerivedT) {
  // CHECK: bb0([[ARG0:%.*]] : @guaranteed $BaseTAndP, [[ARG1:%.*]] : @guaranteed $BaseIntAndP, [[ARG2:%.*]] : @guaranteed $DerivedT)
  // CHECK: [[COPIED:%.*]] = copy_value [[ARG0]] : $BaseTAndP
  // CHECK-NEXT: init_existential_ref [[COPIED]] : $BaseTAndP : $BaseTAndP, $Base<T> & P
  let _: Base<T> & P = baseTAndP

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG1]] : $BaseIntAndP
  // CHECK-NEXT: init_existential_ref [[COPIED]] : $BaseIntAndP : $BaseIntAndP, $Base<Int> & P
  let _: Base<Int> & P = baseIntAndP

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG2]] : $DerivedT
  // CHECK-NEXT: init_existential_ref [[COPIED]] : $DerivedT : $DerivedT, $Base<Int> & P
  let _: Base<Int> & P = derived

  // CHECK:      return
  // CHECK-NEXT: }
}

// CHECK-LABEL: sil hidden @$S21subclass_existentials18archetypeDowncasts1s1t2pt5baseT0F3Int0f6TAndP_C00fg5AndP_C008derived_C00ji2R_C00fH10P_concrete0fgi2P_K0yx_q_q0_q1_q2_q3_q4_q5_AA1R_AA7DerivedCXcAA1P_AA4BaseCyq_GXcAaQ_ASySiGXctAaQR0_ATRb1_AURb2_ATRb3_AaQR3_AURb4_AaQR4_APRb5_r6_lF : $@convention(thin) <S, T, PT, BaseT, BaseInt, BaseTAndP, BaseIntAndP, DerivedT where PT : P, BaseT : Base<T>, BaseInt : Base<Int>, BaseTAndP : Base<T>, BaseTAndP : P, BaseIntAndP : Base<Int>, BaseIntAndP : P, DerivedT : Derived> (@in_guaranteed S, @in_guaranteed T, @in_guaranteed PT, @guaranteed BaseT, @guaranteed BaseInt, @guaranteed BaseTAndP, @guaranteed BaseIntAndP, @guaranteed DerivedT, @guaranteed Derived & R, @guaranteed Base<T> & P, @guaranteed Base<Int> & P) -> () {
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

  // CHECK: ([[ARG0:%.*]] : @trivial $*S, [[ARG1:%.*]] : @trivial $*T, [[ARG2:%.*]] : @trivial $*PT, [[ARG3:%.*]] : @guaranteed $BaseT, [[ARG4:%.*]] : @guaranteed $BaseInt, [[ARG5:%.*]] : @guaranteed $BaseTAndP, [[ARG6:%.*]] : @guaranteed $BaseIntAndP, [[ARG7:%.*]] : @guaranteed $DerivedT, [[ARG8:%.*]] : @guaranteed $Derived & R, [[ARG9:%.*]] : @guaranteed $Base<T> & P, [[ARG10:%.*]] : @guaranteed $Base<Int> & P)

  // CHECK:      [[COPY:%.*]] = alloc_stack $S
  // CHECK-NEXT: copy_addr %0 to [initialization] [[COPY]] : $*S
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Base<T> & P
  // CHECK-NEXT: checked_cast_addr_br take_always S in [[COPY]] : $*S to Base<T> & P in [[RESULT]] : $*Base<T> & P
  let _ = s as? (Base<T> & P)

  // CHECK:      [[COPY:%.*]] = alloc_stack $S
  // CHECK-NEXT: copy_addr [[ARG0]] to [initialization] [[COPY]] : $*S
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Base<T> & P
  // CHECK-NEXT: unconditional_checked_cast_addr S in [[COPY]] : $*S to Base<T> & P in [[RESULT]] : $*Base<T> & P
  let _ = s as! (Base<T> & P)

  // CHECK:      [[COPY:%.*]] = alloc_stack $S
  // CHECK-NEXT: copy_addr [[ARG0]] to [initialization] [[COPY]] : $*S
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Base<Int> & P
  // CHECK-NEXT: checked_cast_addr_br take_always S in [[COPY]] : $*S to Base<Int> & P in [[RESULT]] : $*Base<Int> & P
  let _ = s as? (Base<Int> & P)

  // CHECK:      [[COPY:%.*]] = alloc_stack $S
  // CHECK-NEXT: copy_addr [[ARG0]] to [initialization] [[COPY]] : $*S
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Base<Int> & P
  // CHECK-NEXT: unconditional_checked_cast_addr S in [[COPY]] : $*S to Base<Int> & P in [[RESULT]] : $*Base<Int> & P
  let _ = s as! (Base<Int> & P)

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG5]] : $BaseTAndP
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $BaseTAndP to $Derived & R
  let _ = baseTAndP_archetype as? (Derived & R)

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG5]] : $BaseTAndP
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $BaseTAndP to $Derived & R
  let _ = baseTAndP_archetype as! (Derived & R)

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG9]] : $Base<T> & P
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $Base<T> & P
  // CHECK-NEXT: store [[COPIED]] to [init] [[COPY]] : $*Base<T> & P
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Optional<S>
  // CHECK-NEXT: [[PAYLOAD:%.*]] = init_enum_data_addr [[RESULT]] : $*Optional<S>, #Optional.some
  // CHECK-NEXT: checked_cast_addr_br take_always Base<T> & P in [[COPY]] : $*Base<T> & P to S in [[PAYLOAD]] : $*S
  let _ = baseTAndP_concrete as? S

  // CHECK:      [[COPY:%.*]] = alloc_stack $Base<T> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[ARG9]] : $Base<T> & P
  // CHECK-NEXT: store [[COPIED]] to [init] [[COPY]] : $*Base<T> & P
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $S
  // CHECK-NEXT: unconditional_checked_cast_addr Base<T> & P in [[COPY]] : $*Base<T> & P to S in [[RESULT]] : $*S
  let _ = baseTAndP_concrete as! S

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG9]] : $Base<T> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<T> & P to $BaseT
  let _ = baseTAndP_concrete as? BaseT

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG9]] : $Base<T> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<T> & P to $BaseT
  let _ = baseTAndP_concrete as! BaseT

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG9]] : $Base<T> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<T> & P to $BaseInt
  let _ = baseTAndP_concrete as? BaseInt

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG9]] : $Base<T> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<T> & P to $BaseInt
  let _ = baseTAndP_concrete as! BaseInt

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG9]] : $Base<T> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<T> & P to $BaseTAndP
  let _ = baseTAndP_concrete as? BaseTAndP

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG9]] : $Base<T> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<T> & P to $BaseTAndP
  let _ = baseTAndP_concrete as! BaseTAndP

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG6]] : $BaseIntAndP
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $BaseIntAndP to $Derived & R
  let _ = baseIntAndP_archetype as? (Derived & R)

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG6]] : $BaseIntAndP
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $BaseIntAndP to $Derived & R
  let _ = baseIntAndP_archetype as! (Derived & R)

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG10]] : $Base<Int> & P
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $Base<Int> & P
  // CHECK-NEXT: store [[COPIED]] to [init] [[COPY]] : $*Base<Int> & P
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $Optional<S>
  // CHECK-NEXT: [[PAYLOAD:%.*]] = init_enum_data_addr [[RESULT]] : $*Optional<S>, #Optional.some
  // CHECK-NEXT: checked_cast_addr_br take_always Base<Int> & P in [[COPY]] : $*Base<Int> & P to S in [[PAYLOAD]] : $*S
  let _ = baseIntAndP_concrete as? S

  // CHECK:      [[COPY:%.*]] = alloc_stack $Base<Int> & P
  // CHECK-NEXT: [[COPIED:%.*]] = copy_value [[ARG10]] : $Base<Int> & P
  // CHECK-NEXT: store [[COPIED]] to [init] [[COPY]] : $*Base<Int> & P
  // CHECK-NEXT: [[RESULT:%.*]] = alloc_stack $S
  // CHECK-NEXT: unconditional_checked_cast_addr Base<Int> & P in [[COPY]] : $*Base<Int> & P to S in [[RESULT]] : $*S
  let _ = baseIntAndP_concrete as! S

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG10]] : $Base<Int> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<Int> & P to $DerivedT
  let _ = baseIntAndP_concrete as? DerivedT

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG10]] : $Base<Int> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<Int> & P to $DerivedT
  let _ = baseIntAndP_concrete as! DerivedT

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG10]] : $Base<Int> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<Int> & P to $BaseT
  let _ = baseIntAndP_concrete as? BaseT

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG10]] : $Base<Int> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<Int> & P to $BaseT
  let _ = baseIntAndP_concrete as! BaseT

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG10]] : $Base<Int> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<Int> & P to $BaseInt
  let _ = baseIntAndP_concrete as? BaseInt

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG10]] : $Base<Int> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<Int> & P to $BaseInt
  let _ = baseIntAndP_concrete as! BaseInt

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG10]] : $Base<Int> & P
  // CHECK-NEXT: checked_cast_br [[COPIED]] : $Base<Int> & P to $BaseTAndP
  let _ = baseIntAndP_concrete as? BaseTAndP

  // CHECK: [[COPIED:%.*]] = copy_value [[ARG10]] : $Base<Int> & P
  // CHECK-NEXT: unconditional_checked_cast [[COPIED]] : $Base<Int> & P to $BaseTAndP
  let _ = baseIntAndP_concrete as! BaseTAndP

  // CHECK:      return
  // CHECK-NEXT: }
}
