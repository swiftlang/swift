
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name protocol_resilience -emit-module -enable-sil-ownership -enable-resilience -emit-module-path=%t/resilient_protocol.swiftmodule -module-name=resilient_protocol %S/../Inputs/resilient_protocol.swift
// RUN: %target-swift-frontend -module-name protocol_resilience -I %t -emit-silgen -enable-sil-ownership -enable-resilience %s | %FileCheck %s

import resilient_protocol

prefix operator ~~~
infix operator <*>
infix operator <**>
infix operator <===>

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

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientMethodsP14defaultWitnessyyF
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientMethodsPAAE14defaultWitnessyyF
  public func defaultWitness() {}

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientMethodsP21anotherDefaultWitnessyxSiF
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientMethodsPAAE21anotherDefaultWitnessyxSiF
  public func anotherDefaultWitness(_ x: Int) -> Self {}

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientMethodsP32defaultWitnessWithAssociatedTypeyy05AssocI0QzF
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientMethodsPAAE32defaultWitnessWithAssociatedTypeyy05AssocI0QzF
  public func defaultWitnessWithAssociatedType(_ a: AssocType) {}

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientMethodsP41defaultWitnessMoreAbstractThanRequirement_1by9AssocTypeQz_SitF
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientMethodsPAAE41defaultWitnessMoreAbstractThanRequirement_1byqd___qd_0_tr0_lF
  public func defaultWitnessMoreAbstractThanRequirement<A, T>(_ a: A, b: T) {}

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientMethodsP48defaultWitnessMoreAbstractThanGenericRequirement_1ty9AssocTypeQz_qd__tlF
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientMethodsPAAE48defaultWitnessMoreAbstractThanGenericRequirement_1tyqd___qd_0_tr0_lF
  public func defaultWitnessMoreAbstractThanGenericRequirement<A, T>(_ a: A, t: T) {}

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientMethodsP20staticDefaultWitnessyxSiFZ
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientMethodsPAAE20staticDefaultWitnessyxSiFZ
  public static func staticDefaultWitness(_ x: Int) -> Self {}

// CHECK-LABEL: sil private @$S19protocol_resilience16ResilientMethodsPAAE25defaultWitnessIsNotPublic{{.*}}F
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

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience21ResilientConstructorsP7defaultxyt_tcfC
// CHECK-LABEL: sil @$S19protocol_resilience21ResilientConstructorsPAAE7defaultxyt_tcfC
  public init(default: ()) {
    self.init(noDefault: ())
  }

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience21ResilientConstructorsP17defaultIsOptionalxSgyt_tcfC
// CHECK-LABEL: sil @$S19protocol_resilience21ResilientConstructorsPAAE17defaultIsOptionalxSgyt_tcfC
  public init?(defaultIsOptional: ()) {
    self.init(noDefault: ())
  }

// CHECK-LABEL: sil @$S19protocol_resilience21ResilientConstructorsPAAE20defaultIsNotOptionalxyt_tcfC
  public init(defaultIsNotOptional: ()) {
    self.init(noDefault: ())
  }

// CHECK-LABEL: sil @$S19protocol_resilience21ResilientConstructorsPAAE19optionalityMismatchxSgyt_tcfC
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

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientStorageP19propertyWithDefaultSivg
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientStoragePAAE19propertyWithDefaultSivg
  public var propertyWithDefault: Int {
    get { return 0 }
  }

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSivg
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientStoragePAAE26mutablePropertyWithDefaultSivg
// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSivs
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientStoragePAAE26mutablePropertyWithDefaultSivs
// CHECK-LABEL: sil private [transparent] @$S19protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSivmytfU_
// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSivm
  public var mutablePropertyWithDefault: Int {
    get { return 0 }
    set { }
  }

  public private(set) var mutablePropertyNoDefault: Int {
    get { return 0 }
    set { }
  }

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzvg
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientStoragePAAE33mutableGenericPropertyWithDefault1TQzvg
// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzvs
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientStoragePAAE33mutableGenericPropertyWithDefault1TQzvs
// CHECK-LABEL: sil private [transparent] @$S19protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzvmytfU_
// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzvm
  public var mutableGenericPropertyWithDefault: T {
    get {
      return T(default: ())
    }
    set { }
  }

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientStoragePy1TQzAEcig
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientStoragePAAEy1TQzAEcig
// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientStoragePy1TQzAEcis
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientStoragePAAEy1TQzAEcis
// CHECK-LABEL: sil private [transparent] @$S19protocol_resilience16ResilientStoragePy1TQzAEcimytfU_
// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientStoragePy1TQzAEcim
  public subscript(x: T) -> T {
    get {
      return x
    }
    set { }
  }

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSivg
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientStoragePAAE36mutatingGetterWithNonMutatingDefaultSivg
// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSivs
// CHECK-LABEL: sil @$S19protocol_resilience16ResilientStoragePAAE36mutatingGetterWithNonMutatingDefaultSivs
// CHECK-LABEL: sil private [transparent] @$S19protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSivmytfU_
// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSivm
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

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience18ResilientOperatorsP3tttopyyxFZ
// CHECK-LABEL: sil @$S19protocol_resilience3tttopyyxlF
public prefix func ~~~<S>(s: S) {}

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience18ResilientOperatorsP3lmgoiyyx_qd__tlFZ
// CHECK-LABEL: sil @$S19protocol_resilience3lmgoiyyq__xtr0_lF
public func <*><T, S>(s: S, t: T) {}

// Swap the generic parameters to make sure we don't mix up our DeclContexts
// when mapping interface types in and out

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience18ResilientOperatorsP4lmmgoiy9AssocTypeQzqd___xtlFZ
// CHECK-LABEL: sil @$S19protocol_resilience4lmmgoiy9AssocTypeQzq__xtAA18ResilientOperatorsRzr0_lF
public func <**><S : ResilientOperators, T>(t: T, s: S) -> S.AssocType {}

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience18ResilientOperatorsP5leeegoiy9AssocTypeQyd__qd___xtAaBRd__lFZ
// CHECK-LABEL: sil @$S19protocol_resilience5leeegoiy9AssocTypeQzx_q_tAA18ResilientOperatorsRzAaER_r0_lF
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

// CHECK-LABEL: sil private [transparent] [thunk] @$S19protocol_resilience21ReabstractSelfRefinedP8callbackyxxcvg : $@convention(witness_method: ReabstractSelfRefined) <τ_0_0 where τ_0_0 : ReabstractSelfRefined> (@guaranteed τ_0_0) -> @owned @callee_guaranteed (@guaranteed τ_0_0) -> @owned τ_0_0
// CHECK: [[SELF_BOX:%.*]] = alloc_stack $τ_0_0
// CHECK-NEXT: [[SELF_COPY:%.*]] = copy_value %0 : $τ_0_0
// CHECK-NEXT: store [[SELF_COPY]] to [init] [[SELF_BOX]] : $*τ_0_0
// CHECK: [[WITNESS:%.*]] = function_ref @$S19protocol_resilience18ReabstractSelfBasePAAE8callbackyxxcvg
// CHECK-NEXT: [[RESULT:%.*]] = apply [[WITNESS]]<τ_0_0>([[SELF_BOX]])
// CHECK: [[THUNK_FN:%.*]] = function_ref @$SxxIegnr_xxIeggo_19protocol_resilience21ReabstractSelfRefinedRzlTR
// CHECK-NEXT: [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]<τ_0_0>([[RESULT]])
// CHECK-NEXT: destroy_addr [[SELF_BOX]]
// CHECK-NEXT: dealloc_stack [[SELF_BOX]]
// CHECK-NEXT: return [[THUNK]]

final class X : ReabstractSelfRefined {}

func inoutFunc(_ x: inout Int) {}

// CHECK-LABEL: sil hidden @$S19protocol_resilience22inoutResilientProtocolyy010resilient_A005OtherdE0_pzF
func inoutResilientProtocol(_ x: inout OtherResilientProtocol) {
  // CHECK: function_ref @$S18resilient_protocol22OtherResilientProtocolPAAE19propertyInExtensionSivm
  inoutFunc(&x.propertyInExtension)
}

struct OtherConformingType : OtherResilientProtocol {}

// CHECK-LABEL: sil hidden @$S19protocol_resilience22inoutResilientProtocolyyAA19OtherConformingTypeVzF
func inoutResilientProtocol(_ x: inout OtherConformingType) {
  // CHECK: function_ref @$S18resilient_protocol22OtherResilientProtocolPAAE19propertyInExtensionSivm
  inoutFunc(&x.propertyInExtension)

  // CHECK: function_ref @$S18resilient_protocol22OtherResilientProtocolPAAE25staticPropertyInExtensionSivmZ
  inoutFunc(&OtherConformingType.staticPropertyInExtension)
}

// CHECK-LABEL: sil_default_witness_table P {
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ResilientMethods {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientMethods.defaultWitness!1: {{.*}} : @$S19protocol_resilience16ResilientMethodsP14defaultWitnessyyF
// CHECK-NEXT:    method #ResilientMethods.anotherDefaultWitness!1: {{.*}} : @$S19protocol_resilience16ResilientMethodsP21anotherDefaultWitnessyxSiF
// CHECK-NEXT:    method #ResilientMethods.defaultWitnessWithAssociatedType!1: {{.*}} : @$S19protocol_resilience16ResilientMethodsP32defaultWitnessWithAssociatedTypeyy05AssocI0QzF
// CHECK-NEXT:    method #ResilientMethods.defaultWitnessMoreAbstractThanRequirement!1: {{.*}} : @$S19protocol_resilience16ResilientMethodsP41defaultWitnessMoreAbstractThanRequirement_1by9AssocTypeQz_SitF
// CHECK-NEXT:    method #ResilientMethods.defaultWitnessMoreAbstractThanGenericRequirement!1: {{.*}} : @$S19protocol_resilience16ResilientMethodsP48defaultWitnessMoreAbstractThanGenericRequirement_1ty9AssocTypeQz_qd__tlF
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientMethods.staticDefaultWitness!1: {{.*}} : @$S19protocol_resilience16ResilientMethodsP20staticDefaultWitnessyxSiFZ
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ResilientConstructors {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientConstructors.init!allocator.1: {{.*}} : @$S19protocol_resilience21ResilientConstructorsP7defaultxyt_tcfC
// CHECK-NEXT:    method #ResilientConstructors.init!allocator.1: {{.*}} : @$S19protocol_resilience21ResilientConstructorsP17defaultIsOptionalxSgyt_tcfC
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ResilientStorage {
// CHECK-NEXT:   no_default
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #ResilientStorage.propertyWithDefault!getter.1: {{.*}} : @$S19protocol_resilience16ResilientStorageP19propertyWithDefaultSivg
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #ResilientStorage.mutablePropertyWithDefault!getter.1: {{.*}} : @$S19protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSivg
// CHECK-NEXT:   method #ResilientStorage.mutablePropertyWithDefault!setter.1: {{.*}} : @$S19protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSivs
// CHECK-NEXT:   method #ResilientStorage.mutablePropertyWithDefault!materializeForSet.1: {{.*}} : @$S19protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSivm
// CHECK-NEXT:   no_default
// CHECK-NEXT:   no_default
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #ResilientStorage.mutableGenericPropertyWithDefault!getter.1: {{.*}} : @$S19protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzvg
// CHECK-NEXT:   method #ResilientStorage.mutableGenericPropertyWithDefault!setter.1: {{.*}} : @$S19protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzvs
// CHECK-NEXT:   method #ResilientStorage.mutableGenericPropertyWithDefault!materializeForSet.1: {{.*}} : @$S19protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzvm
// CHECK-NEXT:   method #ResilientStorage.subscript!getter.1: {{.*}} : @$S19protocol_resilience16ResilientStoragePy1TQzAEcig
// CHECK-NEXT:   method #ResilientStorage.subscript!setter.1: {{.*}} : @$S19protocol_resilience16ResilientStoragePy1TQzAEcis
// CHECK-NEXT:   method #ResilientStorage.subscript!materializeForSet.1: {{.*}} : @$S19protocol_resilience16ResilientStoragePy1TQzAEcim
// CHECK-NEXT:   method #ResilientStorage.mutatingGetterWithNonMutatingDefault!getter.1: {{.*}} : @$S19protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSivg
// CHECK-NEXT:   method #ResilientStorage.mutatingGetterWithNonMutatingDefault!setter.1: {{.*}} : @$S19protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSivs
// CHECK-NEXT:   method #ResilientStorage.mutatingGetterWithNonMutatingDefault!materializeForSet.1: {{.*}} : @$S19protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSivm
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ResilientOperators {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientOperators."~~~"!1: {{.*}} : @$S19protocol_resilience18ResilientOperatorsP3tttopyyxFZ
// CHECK-NEXT:    method #ResilientOperators."<*>"!1: {{.*}} : @$S19protocol_resilience18ResilientOperatorsP3lmgoiyyx_qd__tlFZ
// CHECK-NEXT:    method #ResilientOperators."<**>"!1: {{.*}} : @$S19protocol_resilience18ResilientOperatorsP4lmmgoiy9AssocTypeQzqd___xtlFZ
// CHECK-NEXT:    method #ResilientOperators."<===>"!1: {{.*}} : @$S19protocol_resilience18ResilientOperatorsP5leeegoiy9AssocTypeQyd__qd___xtAaBRd__lFZ
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ReabstractSelfRefined {
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #ReabstractSelfRefined.callback!getter.1: {{.*}} : @$S19protocol_resilience21ReabstractSelfRefinedP8callbackyxxcvg
// CHECK-NEXT:   method #ReabstractSelfRefined.callback!setter.1: {{.*}} : @$S19protocol_resilience21ReabstractSelfRefinedP8callbackyxxcvs
// CHECK-NEXT:   method #ReabstractSelfRefined.callback!materializeForSet.1: {{.*}} : @$S19protocol_resilience21ReabstractSelfRefinedP8callbackyxxcvm
// CHECK-NEXT: }
