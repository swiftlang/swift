// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-silgen -enable-resilience %s | %FileCheck %s --check-prefix=CHECK

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

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientMethods14defaultWitnessfT_T_
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_16ResilientMethods14defaultWitnessfT_T_
  public func defaultWitness() {}

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientMethods21anotherDefaultWitnessfSix
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_16ResilientMethods21anotherDefaultWitnessfSix
  public func anotherDefaultWitness(_ x: Int) -> Self {}

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientMethods32defaultWitnessWithAssociatedTypefwx9AssocTypeT_
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_16ResilientMethods32defaultWitnessWithAssociatedTypefwx9AssocTypeT_
  public func defaultWitnessWithAssociatedType(_ a: AssocType) {}

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientMethods41defaultWitnessMoreAbstractThanRequirementfTwx9AssocType1bSi_T_
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_16ResilientMethods41defaultWitnessMoreAbstractThanRequirementu0_rfTqd__1bqd_0__T_
  public func defaultWitnessMoreAbstractThanRequirement<A, T>(_ a: A, b: T) {}

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientMethods48defaultWitnessMoreAbstractThanGenericRequirementurfTwx9AssocType1tqd___T_
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_16ResilientMethods48defaultWitnessMoreAbstractThanGenericRequirementu0_rfTqd__1tqd_0__T_
  public func defaultWitnessMoreAbstractThanGenericRequirement<A, T>(_ a: A, t: T) {}

// CHECK-LABEL: sil [transparent] [thunk] @_TZFP19protocol_resilience16ResilientMethods20staticDefaultWitnessfSix
// CHECK-LABEL: sil @_TZFE19protocol_resiliencePS_16ResilientMethods20staticDefaultWitnessfSix
  public static func staticDefaultWitness(_ x: Int) -> Self {}

// CHECK-LABEL: sil private @_TFE19protocol_resiliencePS_16ResilientMethodsP{{.*}}25defaultWitnessIsNotPublicfT_T_
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

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience21ResilientConstructorsCfT7defaultT__x
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_21ResilientConstructorsCfT7defaultT__x
  public init(default: ()) {
    self.init(noDefault: ())
  }

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience21ResilientConstructorsCfT17defaultIsOptionalT__GSqx_
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_21ResilientConstructorsCfT17defaultIsOptionalT__GSqx_
  public init?(defaultIsOptional: ()) {
    self.init(noDefault: ())
  }

// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_21ResilientConstructorsCfT20defaultIsNotOptionalT__x
  public init(defaultIsNotOptional: ()) {
    self.init(noDefault: ())
  }

// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_21ResilientConstructorsCfT19optionalityMismatchT__GSqx_
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

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientStorageg19propertyWithDefaultSi
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_16ResilientStorageg19propertyWithDefaultSi
  public var propertyWithDefault: Int {
    get { return 0 }
  }

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientStorageg26mutablePropertyWithDefaultSi
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_16ResilientStorageg26mutablePropertyWithDefaultSi
// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientStorages26mutablePropertyWithDefaultSi
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_16ResilientStorages26mutablePropertyWithDefaultSi
// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientStoragem26mutablePropertyWithDefaultSi
// CHECK-LABEL: sil [transparent] @_TFFP19protocol_resilience16ResilientStoragem26mutablePropertyWithDefaultSiU_T_
  public var mutablePropertyWithDefault: Int {
    get { return 0 }
    set { }
  }

  public private(set) var mutablePropertyNoDefault: Int {
    get { return 0 }
    set { }
  }

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientStorageg33mutableGenericPropertyWithDefaultwx1T
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_16ResilientStorageg33mutableGenericPropertyWithDefaultwx1T
// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientStorages33mutableGenericPropertyWithDefaultwx1T
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_16ResilientStorages33mutableGenericPropertyWithDefaultwx1T
// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientStoragem33mutableGenericPropertyWithDefaultwx1T
// CHECK-LABEL: sil [transparent] @_TFFP19protocol_resilience16ResilientStoragem33mutableGenericPropertyWithDefaultwx1TU_T_
  public var mutableGenericPropertyWithDefault: T {
    get {
      return T(default: ())
    }
    set { }
  }

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientStorageg9subscriptFwx1TwxS1_
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_16ResilientStorageg9subscriptFwx1TwxS1_
// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientStorages9subscriptFwx1TwxS1_
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_16ResilientStorages9subscriptFwx1TwxS1_
// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientStoragem9subscriptFwx1TwxS1_
// CHECK-LABEL: sil [transparent] @_TFFP19protocol_resilience16ResilientStoragem9subscriptFwx1TwxS1_U_T_
  public subscript(x: T) -> T {
    get {
      return x
    }
    set { }
  }

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientStorageg36mutatingGetterWithNonMutatingDefaultSi
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_16ResilientStorageg36mutatingGetterWithNonMutatingDefaultSi
// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientStorages36mutatingGetterWithNonMutatingDefaultSi
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_16ResilientStorages36mutatingGetterWithNonMutatingDefaultSi
// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience16ResilientStoragem36mutatingGetterWithNonMutatingDefaultSi
// CHECK-LABEL: sil [transparent] @_TFFP19protocol_resilience16ResilientStoragem36mutatingGetterWithNonMutatingDefaultSiU_T_
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

// CHECK-LABEL: sil [transparent] [thunk] @_TZFP19protocol_resilience18ResilientOperatorsop3tttfxT_
// CHECK-LABEL: sil @_TF19protocol_resilienceop3ttturFxT_
public prefix func ~~~<S>(s: S) {}

// CHECK-LABEL: sil [transparent] [thunk] @_TZFP19protocol_resilience18ResilientOperatorsoi3lmgurfTxqd___T_
// CHECK-LABEL: sil @_TF19protocol_resilienceoi3lmgu0_rFTq_x_T_
public func <*><T, S>(s: S, t: T) {}

// Swap the generic parameters to make sure we don't mix up our DeclContexts
// when mapping interface types in and out

// CHECK-LABEL: sil [transparent] [thunk] @_TZFP19protocol_resilience18ResilientOperatorsoi4lmmgurfTqd__x_wx9AssocType
// CHECK-LABEL: sil @_TF19protocol_resilienceoi4lmmgu0_RxS_18ResilientOperatorsrFTq_x_wx9AssocType
public func <**><S : ResilientOperators, T>(t: T, s: S) -> S.AssocType {}

// CHECK-LABEL: sil [transparent] [thunk] @_TZFP19protocol_resilience18ResilientOperatorsoi5leeeguRd__S0_rfTqd__x_wd__9AssocType
// CHECK-LABEL: sil @_TF19protocol_resilienceoi5leeegu0_RxS_18ResilientOperators_S0_rFTxq__wx9AssocType
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

// CHECK-LABEL: sil hidden @_TF19protocol_resilience22inoutResilientProtocolFRP18resilient_protocol22OtherResilientProtocol_T_
func inoutResilientProtocol(_ x: inout OtherResilientProtocol) {
  // CHECK: function_ref @_TFE18resilient_protocolPS_22OtherResilientProtocolm19propertyInExtensionSi
  inoutFunc(&x.propertyInExtension)
}

struct OtherConformingType : OtherResilientProtocol {}

// CHECK-LABEL: sil hidden @_TF19protocol_resilience22inoutResilientProtocolFRVS_19OtherConformingTypeT_
func inoutResilientProtocol(_ x: inout OtherConformingType) {
  // CHECK: function_ref @_TFE18resilient_protocolPS_22OtherResilientProtocolm19propertyInExtensionSi
  inoutFunc(&x.propertyInExtension)

  // CHECK: function_ref @_TZFE18resilient_protocolPS_22OtherResilientProtocolm25staticPropertyInExtensionSi
  inoutFunc(&OtherConformingType.staticPropertyInExtension)
}

// Protocol is not public -- make sure default witnesses have the right linkage
protocol InternalProtocol {
  func noDefaultF()
  func defaultG()
}

extension InternalProtocol {

  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TFP19protocol_resilience16InternalProtocol8defaultGfT_T_
  // CHECK: return
  func defaultG() {}
}

// CHECK-LABEL: sil_default_witness_table P {
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ResilientMethods {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientMethods.defaultWitness!1: @_TFP19protocol_resilience16ResilientMethods14defaultWitnessfT_T_
// CHECK-NEXT:    method #ResilientMethods.anotherDefaultWitness!1: @_TFP19protocol_resilience16ResilientMethods21anotherDefaultWitnessfSix
// CHECK-NEXT:    method #ResilientMethods.defaultWitnessWithAssociatedType!1: @_TFP19protocol_resilience16ResilientMethods32defaultWitnessWithAssociatedTypefwx9AssocTypeT_
// CHECK-NEXT:    method #ResilientMethods.defaultWitnessMoreAbstractThanRequirement!1: @_TFP19protocol_resilience16ResilientMethods41defaultWitnessMoreAbstractThanRequirementfTwx9AssocType1bSi_T_
// CHECK-NEXT:    method #ResilientMethods.defaultWitnessMoreAbstractThanGenericRequirement!1: @_TFP19protocol_resilience16ResilientMethods48defaultWitnessMoreAbstractThanGenericRequirementurfTwx9AssocType1tqd___T_
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientMethods.staticDefaultWitness!1: @_TZFP19protocol_resilience16ResilientMethods20staticDefaultWitnessfSix
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ResilientConstructors {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientConstructors.init!allocator.1: @_TFP19protocol_resilience21ResilientConstructorsCfT7defaultT__x
// CHECK-NEXT:    method #ResilientConstructors.init!allocator.1: @_TFP19protocol_resilience21ResilientConstructorsCfT17defaultIsOptionalT__GSqx_
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ResilientStorage {
// CHECK-NEXT:   no_default
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #ResilientStorage.propertyWithDefault!getter.1: @_TFP19protocol_resilience16ResilientStorageg19propertyWithDefaultSi
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #ResilientStorage.mutablePropertyWithDefault!getter.1: @_TFP19protocol_resilience16ResilientStorageg26mutablePropertyWithDefaultSi
// CHECK-NEXT:   method #ResilientStorage.mutablePropertyWithDefault!setter.1: @_TFP19protocol_resilience16ResilientStorages26mutablePropertyWithDefaultSi
// CHECK-NEXT:   method #ResilientStorage.mutablePropertyWithDefault!materializeForSet.1: @_TFP19protocol_resilience16ResilientStoragem26mutablePropertyWithDefaultSi
// CHECK-NEXT:   no_default
// CHECK-NEXT:   no_default
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #ResilientStorage.mutableGenericPropertyWithDefault!getter.1: @_TFP19protocol_resilience16ResilientStorageg33mutableGenericPropertyWithDefaultwx1T
// CHECK-NEXT:   method #ResilientStorage.mutableGenericPropertyWithDefault!setter.1: @_TFP19protocol_resilience16ResilientStorages33mutableGenericPropertyWithDefaultwx1T
// CHECK-NEXT:   method #ResilientStorage.mutableGenericPropertyWithDefault!materializeForSet.1: @_TFP19protocol_resilience16ResilientStoragem33mutableGenericPropertyWithDefaultwx1T
// CHECK-NEXT:   method #ResilientStorage.subscript!getter.1: @_TFP19protocol_resilience16ResilientStorageg9subscriptFwx1TwxS1_
// CHECK-NEXT:   method #ResilientStorage.subscript!setter.1: @_TFP19protocol_resilience16ResilientStorages9subscriptFwx1TwxS1_
// CHECK-NEXT:   method #ResilientStorage.subscript!materializeForSet.1: @_TFP19protocol_resilience16ResilientStoragem9subscriptFwx1TwxS1_
// CHECK-NEXT:   method #ResilientStorage.mutatingGetterWithNonMutatingDefault!getter.1: @_TFP19protocol_resilience16ResilientStorageg36mutatingGetterWithNonMutatingDefaultSi
// CHECK-NEXT:   method #ResilientStorage.mutatingGetterWithNonMutatingDefault!setter.1: @_TFP19protocol_resilience16ResilientStorages36mutatingGetterWithNonMutatingDefaultSi
// CHECK-NEXT:   method #ResilientStorage.mutatingGetterWithNonMutatingDefault!materializeForSet.1: @_TFP19protocol_resilience16ResilientStoragem36mutatingGetterWithNonMutatingDefaultSi
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ResilientOperators {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientOperators."~~~"!1: @_TZFP19protocol_resilience18ResilientOperatorsop3tttfxT_
// CHECK-NEXT:    method #ResilientOperators."<*>"!1: @_TZFP19protocol_resilience18ResilientOperatorsoi3lmgurfTxqd___T_
// CHECK-NEXT:    method #ResilientOperators."<**>"!1: @_TZFP19protocol_resilience18ResilientOperatorsoi4lmmgurfTqd__x_wx9AssocType
// CHECK-NEXT:    method #ResilientOperators."<===>"!1: @_TZFP19protocol_resilience18ResilientOperatorsoi5leeeguRd__S0_rfTqd__x_wd__9AssocType
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ReabstractSelfRefined {
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #ReabstractSelfRefined.callback!getter.1: @_TFP19protocol_resilience21ReabstractSelfRefinedg8callbackFxx
// CHECK-NEXT:   method #ReabstractSelfRefined.callback!setter.1: @_TFP19protocol_resilience21ReabstractSelfRefineds8callbackFxx
// CHECK-NEXT:   method #ReabstractSelfRefined.callback!materializeForSet.1: @_TFP19protocol_resilience21ReabstractSelfRefinedm8callbackFxx
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table hidden InternalProtocol {
// CHECK-NEXT:   no_default
// CHECK-NEXT:   method #InternalProtocol.defaultG!1: @_TFP19protocol_resilience16InternalProtocol8defaultGfT_T_
// CHECK-NEXT: }
