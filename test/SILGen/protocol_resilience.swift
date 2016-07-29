// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_protocol.swiftmodule -module-name=resilient_protocol %S/../Inputs/resilient_protocol.swift
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -I %t -emit-silgen -enable-resilience %s | %FileCheck %s

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

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientMethodsP14defaultWitnessyyF
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientMethodsPAAE14defaultWitnessyyF
  public func defaultWitness() {}

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientMethodsP21anotherDefaultWitnessxSiF
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientMethodsPAAE21anotherDefaultWitnessxSiF
  public func anotherDefaultWitness(_ x: Int) -> Self {}

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientMethodsP32defaultWitnessWithAssociatedTypey05AssocI0QzF
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientMethodsPAAE32defaultWitnessWithAssociatedTypey05AssocI0QzF
  public func defaultWitnessWithAssociatedType(_ a: AssocType) {}

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientMethodsP41defaultWitnessMoreAbstractThanRequirementy9AssocTypeQz_Si1btF
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientMethodsPAAE41defaultWitnessMoreAbstractThanRequirementyqd___qd_0_1btr0_lF
  public func defaultWitnessMoreAbstractThanRequirement<A, T>(_ a: A, b: T) {}

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientMethodsP48defaultWitnessMoreAbstractThanGenericRequirementy9AssocTypeQz_qd__1ttlF
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientMethodsPAAE48defaultWitnessMoreAbstractThanGenericRequirementyqd___qd_0_1ttr0_lF
  public func defaultWitnessMoreAbstractThanGenericRequirement<A, T>(_ a: A, t: T) {}

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientMethodsP20staticDefaultWitnessxSiFZ
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

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience21ResilientConstructorsPxyt7default_tcfC
// CHECK-LABEL: sil @_T019protocol_resilience21ResilientConstructorsPAAExyt7default_tcfC
  public init(default: ()) {
    self.init(noDefault: ())
  }

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience21ResilientConstructorsPxSgyt17defaultIsOptional_tcfC
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

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP19propertyWithDefaultSifg
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE19propertyWithDefaultSifg
  public var propertyWithDefault: Int {
    get { return 0 }
  }

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSifg
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE26mutablePropertyWithDefaultSifg
// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSifs
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE26mutablePropertyWithDefaultSifs
// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSifm
// CHECK-LABEL: sil [transparent] @_T019protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSifmytfU_
  public var mutablePropertyWithDefault: Int {
    get { return 0 }
    set { }
  }

  public private(set) var mutablePropertyNoDefault: Int {
    get { return 0 }
    set { }
  }

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzfg
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE33mutableGenericPropertyWithDefault1TQzfg
// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzfs
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE33mutableGenericPropertyWithDefault1TQzfs
// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzfm
// CHECK-LABEL: sil [transparent] @_T019protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzfmytfU_
  public var mutableGenericPropertyWithDefault: T {
    get {
      return T(default: ())
    }
    set { }
  }

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP9subscript1TQzAFcfg
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE9subscript1TQzAFcfg
// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP9subscript1TQzAFcfs
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE9subscript1TQzAFcfs
// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP9subscript1TQzAFcfm
// CHECK-LABEL: sil [transparent] @_T019protocol_resilience16ResilientStorageP9subscript1TQzAFcfmytfU_
  public subscript(x: T) -> T {
    get {
      return x
    }
    set { }
  }

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSifg
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE36mutatingGetterWithNonMutatingDefaultSifg
// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSifs
// CHECK-LABEL: sil @_T019protocol_resilience16ResilientStoragePAAE36mutatingGetterWithNonMutatingDefaultSifs
// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSifm
// CHECK-LABEL: sil [transparent] @_T019protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSifmytfU_
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

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience18ResilientOperatorsP3tttopyxFZ
// CHECK-LABEL: sil @_T019protocol_resilience3tttopyxlF
public prefix func ~~~<S>(s: S) {}

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience18ResilientOperatorsP3lmgoiyx_qd__tlFZ
// CHECK-LABEL: sil @_T019protocol_resilience3lmgoiyq__xtr0_lF
public func <*><T, S>(s: S, t: T) {}

// Swap the generic parameters to make sure we don't mix up our DeclContexts
// when mapping interface types in and out

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience18ResilientOperatorsP4lmmgoi9AssocTypeQzqd___xtlFZ
// CHECK-LABEL: sil @_T019protocol_resilience4lmmgoi9AssocTypeQzq__xtAA18ResilientOperatorsRzr0_lF
public func <**><S : ResilientOperators, T>(t: T, s: S) -> S.AssocType {}

// CHECK-LABEL: sil [transparent] [thunk] @_T019protocol_resilience18ResilientOperatorsP5leeegoi9AssocTypeQyd__qd___xtAaBRd__lFZ
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

final class X : ReabstractSelfRefined {}

func inoutFunc(_ x: inout Int) {}

// CHECK-LABEL: sil hidden @_T019protocol_resilience22inoutResilientProtocoly010resilient_A005OtherdE0_pzF
func inoutResilientProtocol(_ x: inout OtherResilientProtocol) {
  // CHECK: function_ref @_T018resilient_protocol22OtherResilientProtocolPAAE19propertyInExtensionSifm
  inoutFunc(&x.propertyInExtension)
}

struct OtherConformingType : OtherResilientProtocol {}

// CHECK-LABEL: sil hidden @_T019protocol_resilience22inoutResilientProtocolyAA19OtherConformingTypeVzF
func inoutResilientProtocol(_ x: inout OtherConformingType) {
  // CHECK: function_ref @_T018resilient_protocol22OtherResilientProtocolPAAE19propertyInExtensionSifm
  inoutFunc(&x.propertyInExtension)

  // CHECK: function_ref @_T018resilient_protocol22OtherResilientProtocolPAAE25staticPropertyInExtensionSifmZ
  inoutFunc(&OtherConformingType.staticPropertyInExtension)
}

// Protocol is not public -- make sure default witnesses have the right linkage
protocol InternalProtocol {
  func noDefaultF()
  func defaultG()
}

extension InternalProtocol {

  // CHECK-LABEL: sil hidden [transparent] [thunk] @_T019protocol_resilience16InternalProtocolP8defaultGyyF
  // CHECK: return
  func defaultG() {}
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
// CHECK-NEXT:   method #ResilientStorage.propertyWithDefault!getter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP19propertyWithDefaultSifg
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #ResilientStorage.mutablePropertyWithDefault!getter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSifg
// CHECK-NEXT:   method #ResilientStorage.mutablePropertyWithDefault!setter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSifs
// CHECK-NEXT:   method #ResilientStorage.mutablePropertyWithDefault!materializeForSet.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP26mutablePropertyWithDefaultSifm
// CHECK-NEXT:   no_default
// CHECK-NEXT:   no_default
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #ResilientStorage.mutableGenericPropertyWithDefault!getter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzfg
// CHECK-NEXT:   method #ResilientStorage.mutableGenericPropertyWithDefault!setter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzfs
// CHECK-NEXT:   method #ResilientStorage.mutableGenericPropertyWithDefault!materializeForSet.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP33mutableGenericPropertyWithDefault1TQzfm
// CHECK-NEXT:   method #ResilientStorage.subscript!getter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP9subscript1TQzAFcfg
// CHECK-NEXT:   method #ResilientStorage.subscript!setter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP9subscript1TQzAFcfs
// CHECK-NEXT:   method #ResilientStorage.subscript!materializeForSet.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP9subscript1TQzAFcfm
// CHECK-NEXT:   method #ResilientStorage.mutatingGetterWithNonMutatingDefault!getter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSifg
// CHECK-NEXT:   method #ResilientStorage.mutatingGetterWithNonMutatingDefault!setter.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSifs
// CHECK-NEXT:   method #ResilientStorage.mutatingGetterWithNonMutatingDefault!materializeForSet.1: {{.*}} : @_T019protocol_resilience16ResilientStorageP36mutatingGetterWithNonMutatingDefaultSifm
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
// CHECK-NEXT:   method #ReabstractSelfRefined.callback!getter.1: {{.*}} : @_T019protocol_resilience21ReabstractSelfRefinedP8callbackxxcfg
// CHECK-NEXT:   method #ReabstractSelfRefined.callback!setter.1: {{.*}} : @_T019protocol_resilience21ReabstractSelfRefinedP8callbackxxcfs
// CHECK-NEXT:   method #ReabstractSelfRefined.callback!materializeForSet.1: {{.*}} : @_T019protocol_resilience21ReabstractSelfRefinedP8callbackxxcfm
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table hidden InternalProtocol {
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #InternalProtocol.defaultG!1: {{.*}} : @_T019protocol_resilience16InternalProtocolP8defaultGyyF
// CHECK-NEXT: }
