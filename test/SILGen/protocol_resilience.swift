// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-sil-ownership -enable-resilience -emit-module-path=%t/resilient_protocol.swiftmodule -module-name=resilient_protocol %S/../Inputs/resilient_protocol.swift
// RUN: %target-swift-frontend -I %t -emit-silgen -enable-sil-ownership -enable-resilience %s | %FileCheck %s

import resilient_protocol

prefix operator ~~~ {}
infix operator <*> {}
infix operator <**> {}
infix operator <===> {}

public protocol P {}


// Protocol is public -- needs resilient witness table
public protocol ResilientMethods {
  associatedtype AssocType : P

  func defaultWitness()
  func anotherDefaultWitness(_ x: Int) -> Self
  func defaultWitnessWithAssociatedType(_ a: AssocType)
  func defaultWitnessMoreAbstractThanRequirement(_ a: AssocType, b: Int)
  func defaultWitnessMoreAbstractThanGenericRequirement<T>(_ a: AssocType, t: T)

  func noDefaultWitness()
  func defaultWitnessIsNotPublic()

  static func staticDefaultWitness(_ x: Int) -> Self
}

extension ResilientMethods {

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientMethodsP14defaultWitnessyyF
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientMethodsPAAE14defaultWitnessyyF
  public func defaultWitness() {}

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientMethodsP21anotherDefaultWitnessxSiF
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientMethodsPAAE21anotherDefaultWitnessxSiF
  public func anotherDefaultWitness(_ x: Int) -> Self {}

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientMethodsP32defaultWitnessWithAssociatedTypey05AssocI0QzF
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientMethodsPAAE32defaultWitnessWithAssociatedTypey05AssocI0QzF
  public func defaultWitnessWithAssociatedType(_ a: AssocType) {}

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientMethodsP41defaultWitnessMoreAbstractThanRequirementy9AssocTypeQz_Si1btF
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientMethodsPAAE41defaultWitnessMoreAbstractThanRequirementyqd___qd_0_1btr0_lF
  public func defaultWitnessMoreAbstractThanRequirement<A, T>(_ a: A, b: T) {}

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientMethodsP48defaultWitnessMoreAbstractThanGenericRequirementy9AssocTypeQz_qd__1ttlF
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientMethodsPAAE48defaultWitnessMoreAbstractThanGenericRequirementyqd___qd_0_1ttr0_lF
  public func defaultWitnessMoreAbstractThanGenericRequirement<A, T>(_ a: A, t: T) {}

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientMethodsP20staticDefaultWitnessxSiFZ
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientMethodsPAAE20staticDefaultWitnessxSiFZ
  public static func staticDefaultWitness(_ x: Int) -> Self {}

// CHECK-LABEL: sil private @_T019protocol_resilience16ResilientMethodsPAAE25defaultWitnessIsNotPublic{{.*}}F
  private func defaultWitnessIsNotPublic() {}

}


public protocol ResilientConstructors {
  init(noDefault: ())
  init(default: ())
  init?(defaultIsOptional: ())
  init?(defaultNotOptional: ())
  init(optionalityMismatch: ())
}

extension ResilientConstructors {

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience21ResilientConstructorsPxyt7default_tcfC
// CHECK-LABEL: sil @_T019protocol_resilience21ResilientConstructorsPAAExyt7default_tcfC
  public init(default: ()) {
    self.init(noDefault: ())
  }

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience21ResilientConstructorsPxSgyt17defaultIsOptional_tcfC
// CHECK-LABEL: sil @_T019protocol_resilience21ResilientConstructorsPAAExSgyt17defaultIsOptional_tcfC
  public init?(defaultIsOptional: ()) {
    self.init(noDefault: ())
  }

// CHECK-LABEL: sil @_T019protocol_resilience21ResilientConstructorsPAAExyt20defaultIsNotOptional_tcfC
  public init(defaultIsNotOptional: ()) {
    self.init(noDefault: ())
  }

// CHECK-LABEL: sil @_T019protocol_resilience21ResilientConstructorsPAAExSgyt19optionalityMismatch_tcfC
  public init?(optionalityMismatch: ()) {
    self.init(noDefault: ())
  }
}


public protocol ResilientStorage {
  associatedtype T : ResilientConstructors

  var propertyWithDefault: Int { get }
  var propertyWithNoDefault: Int { get }
  var mutablePropertyWithDefault: Int { get set }
  var mutablePropertyNoDefault: Int { get set }
  var mutableGenericPropertyWithDefault: T { get set }

  subscript(x: T) -> T { get set }

  var mutatingGetterWithNonMutatingDefault: Int { mutating get set }
}

extension ResilientStorage {

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP19propertyWithDefaultSivg
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE19propertyWithDefaultSivg
  public var propertyWithDefault: Int {
    get { return 0 }
  }

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSivg
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE26mutablePropertyWithDefaultSivg
// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSivs
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE26mutablePropertyWithDefaultSivs
// CHECK-LABEL: sil private [transparent] @_T019protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSivmytfU_
// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSivm
  public var mutablePropertyWithDefault: Int {
    get { return 0 }
    set { }
  }

  public private(set) var mutablePropertyNoDefault: Int {
    get { return 0 }
    set { }
  }

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzvg
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE33mutableGenericPropertyWithDefault1TQzvg
// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzvs
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE33mutableGenericPropertyWithDefault1TQzvs
// CHECK-LABEL: sil private [transparent] @_T019protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzvmytfU_
// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzvm
  public var mutableGenericPropertyWithDefault: T {
    get {
      return T(default: ())
    }
    set { }
  }

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP1TQzAEcig
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE1TQzAEcig
// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP1TQzAEcis
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE1TQzAEcis
// CHECK-LABEL: sil private [transparent] @_T019protocol_resilience16ResilientStorageP1TQzAEcimytfU_
// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP1TQzAEcim
  public subscript(x: T) -> T {
    get {
      return x
    }
    set { }
  }

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSivg
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE36mutatingGetterWithNonMutatingDefaultSivg
// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSivs
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE36mutatingGetterWithNonMutatingDefaultSivs
// CHECK-LABEL: sil private [transparent] @_T019protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSivmytfU_
// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSivm
  public var mutatingGetterWithNonMutatingDefault: Int {
    get {
      return 0
    }
    set { }
  }
}


public protocol ResilientOperators {
  associatedtype AssocType : P

  static prefix func ~~~(s: Self)
  static func <*><T>(s: Self, t: T)
  static func <**><T>(t: T, s: Self) -> AssocType
  static func <===><T : ResilientOperators>(t: T, s: Self) -> T.AssocType
}

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience18ResilientOperatorsP3tttopyxFZ
// CHECK-LABEL: sil @_T019protocol_resilience3tttopyxlF
public prefix func ~~~<S>(s: S) {}

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience18ResilientOperatorsP3lmgoiyx_qd__tlFZ
// CHECK-LABEL: sil @_T019protocol_resilience3lmgoiyq__xtr0_lF
public func <*><T, S>(s: S, t: T) {}

// Swap the generic parameters to make sure we don't mix up our DeclContexts
// when mapping interface types in and out

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience18ResilientOperatorsP4lmmgoi9AssocTypeQzqd___xtlFZ
// CHECK-LABEL: sil @_T019protocol_resilience4lmmgoi9AssocTypeQzq__xtAA18ResilientOperatorsRzr0_lF
public func <**><S : ResilientOperators, T>(t: T, s: S) -> S.AssocType {}

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience18ResilientOperatorsP5leeegoi9AssocTypeQyd__qd___xtAaBRd__lFZ
// CHECK-LABEL: sil @_T019protocol_resilience5leeegoi9AssocTypeQzx_q_tAA18ResilientOperatorsRzAaER_r0_lF
public func <===><T : ResilientOperators, S : ResilientOperators>(t: T, s: S) -> T.AssocType {}


public protocol ReabstractSelfBase {
  // No requirements
}

public protocol ReabstractSelfRefined : class, ReabstractSelfBase {
  // A requirement with 'Self' abstracted as a class instance
  var callback: (Self) -> Self { get set }
}

func id<T>(_ t: T) -> T {}

extension ReabstractSelfBase {
  // A witness for the above requirement, but with 'Self' maximally abstracted
  public var callback: (Self) -> Self {
    get { return id }
    nonmutating set { }
  }
}

// CHECK-LABEL: sil private [transparent] [thunk] @_T019protocol_resilience21ReabstractSelfRefinedP8callbackxxcvg : $@convention(witness_method: ReabstractSelfRefined) <τ_0_0 where τ_0_0 : ReabstractSelfRefined> (@guaranteed τ_0_0) -> @owned @callee_guaranteed (@owned τ_0_0) -> @owned τ_0_0
// CHECK: [[SELF_BOX:%.*]] = alloc_stack $τ_0_0
// CHECK-NEXT: [[SELF_COPY:%.*]] = copy_value %0 : $τ_0_0
// CHECK-NEXT: store [[SELF_COPY]] to [init] [[SELF_BOX]] : $*τ_0_0
// CHECK: [[WITNESS:%.*]] = function_ref @_T019protocol_resilience18ReabstractSelfBasePAAE8callbackxxcvg
// CHECK-NEXT: [[RESULT:%.*]] = apply [[WITNESS]]<τ_0_0>([[SELF_BOX]])
// CHECK: [[THUNK_FN:%.*]] = function_ref @_T0xxIegir_xxIegxo_19protocol_resilience21ReabstractSelfRefinedRzlTR
// CHECK-NEXT: [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]<τ_0_0>([[RESULT]])
// CHECK-NEXT: destroy_addr [[SELF_BOX]]
// CHECK-NEXT: dealloc_stack [[SELF_BOX]]
// CHECK-NEXT: return [[THUNK]]

final class X : ReabstractSelfRefined {}

func inoutFunc(_ x: inout Int) {}

// CHECK-LABEL: sil hidden @_T019protocol_resilience22inoutResilientProtocoly010resilient_A005OtherdE0_pzF
func inoutResilientProtocol(_ x: inout OtherResilientProtocol) {
  // CHECK: function_ref @_T018resilient_protocol22OtherResilientProtocolPAAE19propertyInExtensionSivm
  inoutFunc(&x.propertyInExtension)
}

struct OtherConformingType : OtherResilientProtocol {}

// CHECK-LABEL: sil hidden @_T019protocol_resilience22inoutResilientProtocolyAA19OtherConformingTypeVzF
func inoutResilientProtocol(_ x: inout OtherConformingType) {
  // CHECK: function_ref @_T018resilient_protocol22OtherResilientProtocolPAAE19propertyInExtensionSivm
  inoutFunc(&x.propertyInExtension)

  // CHECK: function_ref @_T018resilient_protocol22OtherResilientProtocolPAAE25staticPropertyInExtensionSivmZ
  inoutFunc(&OtherConformingType.staticPropertyInExtension)
}

// CHECK-LABEL: sil_default_witness_table P {
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ResilientMethods {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientMethods.defaultWitness!1: {{.*}} : @_T019protocol_resilience16ResilientMethodsP14defaultWitnessyyF
// CHECK-NEXT:    method #ResilientMethods.anotherDefaultWitness!1: {{.*}} : @_T019protocol_resilience16ResilientMethodsP21anotherDefaultWitnessxSiF
// CHECK-NEXT:    method #ResilientMethods.defaultWitnessWithAssociatedType!1: {{.*}} : @_T019protocol_resilience16ResilientMethodsP32defaultWitnessWithAssociatedTypey05AssocI0QzF
// CHECK-NEXT:    method #ResilientMethods.defaultWitnessMoreAbstractThanRequirement!1: {{.*}} : @_T019protocol_resilience16ResilientMethodsP41defaultWitnessMoreAbstractThanRequirementy9AssocTypeQz_Si1btF
// CHECK-NEXT:    method #ResilientMethods.defaultWitnessMoreAbstractThanGenericRequirement!1: {{.*}} : @_T019protocol_resilience16ResilientMethodsP48defaultWitnessMoreAbstractThanGenericRequirementy9AssocTypeQz_qd__1ttlF
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientMethods.staticDefaultWitness!1: {{.*}} : @_T019protocol_resilience16ResilientMethodsP20staticDefaultWitnessxSiFZ
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ResilientConstructors {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientConstructors.init!allocator.1: {{.*}} : @_T019protocol_resilience21ResilientConstructorsPxyt7default_tcfC
// CHECK-NEXT:    method #ResilientConstructors.init!allocator.1: {{.*}} : @_T019protocol_resilience21ResilientConstructorsPxSgyt17defaultIsOptional_tcfC
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ResilientStorage {
// CHECK-NEXT:   no_default
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #ResilientStorage.propertyWithDefault!getter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP19propertyWithDefaultSivg
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #ResilientStorage.mutablePropertyWithDefault!getter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSivg
// CHECK-NEXT:   method #ResilientStorage.mutablePropertyWithDefault!setter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSivs
// CHECK-NEXT:   method #ResilientStorage.mutablePropertyWithDefault!materializeForSet.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSivm
// CHECK-NEXT:   no_default
// CHECK-NEXT:   no_default
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #ResilientStorage.mutableGenericPropertyWithDefault!getter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzvg
// CHECK-NEXT:   method #ResilientStorage.mutableGenericPropertyWithDefault!setter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzvs
// CHECK-NEXT:   method #ResilientStorage.mutableGenericPropertyWithDefault!materializeForSet.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzvm
// CHECK-NEXT:   method #ResilientStorage.subscript!getter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP1TQzAEcig
// CHECK-NEXT:   method #ResilientStorage.subscript!setter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP1TQzAEcis
// CHECK-NEXT:   method #ResilientStorage.subscript!materializeForSet.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP1TQzAEcim
// CHECK-NEXT:   method #ResilientStorage.mutatingGetterWithNonMutatingDefault!getter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSivg
// CHECK-NEXT:   method #ResilientStorage.mutatingGetterWithNonMutatingDefault!setter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSivs
// CHECK-NEXT:   method #ResilientStorage.mutatingGetterWithNonMutatingDefault!materializeForSet.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSivm
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ResilientOperators {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientOperators."~~~"!1: {{.*}} : @_T019protocol_resilience18ResilientOperatorsP3tttopyxFZ
// CHECK-NEXT:    method #ResilientOperators."<*>"!1: {{.*}} : @_T019protocol_resilience18ResilientOperatorsP3lmgoiyx_qd__tlFZ
// CHECK-NEXT:    method #ResilientOperators."<**>"!1: {{.*}} : @_T019protocol_resilience18ResilientOperatorsP4lmmgoi9AssocTypeQzqd___xtlFZ
// CHECK-NEXT:    method #ResilientOperators."<===>"!1: {{.*}} : @_T019protocol_resilience18ResilientOperatorsP5leeegoi9AssocTypeQyd__qd___xtAaBRd__lFZ
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ReabstractSelfRefined {
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #ReabstractSelfRefined.callback!getter.1: {{.*}} : @_T019protocol_resilience21ReabstractSelfRefinedP8callbackxxcvg
// CHECK-NEXT:   method #ReabstractSelfRefined.callback!setter.1: {{.*}} : @_T019protocol_resilience21ReabstractSelfRefinedP8callbackxxcvs
// CHECK-NEXT:   method #ReabstractSelfRefined.callback!materializeForSet.1: {{.*}} : @_T019protocol_resilience21ReabstractSelfRefinedP8callbackxxcvm
// CHECK-NEXT: }
