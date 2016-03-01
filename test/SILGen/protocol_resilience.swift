// RUN: %target-swift-frontend -emit-silgen -enable-resilience %s | FileCheck %s --check-prefix=CHECK --check-prefix=GLOBAL

prefix operator ~~~ {}
infix operator <*> {}
infix operator <**> {}
infix operator <===> {}

public protocol P {}

// Protocol is public -- needs resilient witness table
public protocol ResilientProtocol {
  associatedtype AssocType : P

  func defaultWitness()
  func anotherDefaultWitness(x: Int) -> Self
  func defaultWitnessWithAssociatedType(a: AssocType)
  func defaultWitnessMoreAbstractThanRequirement(a: AssocType, b: Int)
  func defaultWitnessMoreAbstractThanGenericRequirement<T>(a: AssocType, t: T)

  func noDefaultWitness()
  func defaultWitnessIsNotPublic()

  static func staticDefaultWitness(x: Int) -> Self

  init(noDefault: ())
  init(default: ())
  init?(defaultIsOptional: ())
  init?(defaultNotOptional: ())
  init(optionalityMismatch: ())

  prefix func ~~~(s: Self)
  func <*><T>(s: Self, t: T)
  func <**><T>(t: T, s: Self) -> AssocType
  func <===><T : ResilientProtocol>(t: T, s: Self) -> T.AssocType
}

extension ResilientProtocol {

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience17ResilientProtocol14defaultWitnessfT_T_
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_17ResilientProtocol14defaultWitnessfT_T_
  public func defaultWitness() {}

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience17ResilientProtocol21anotherDefaultWitnessfSix
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_17ResilientProtocol21anotherDefaultWitnessfSix
  public func anotherDefaultWitness(x: Int) -> Self {}

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience17ResilientProtocol32defaultWitnessWithAssociatedTypefwx9AssocTypeT_
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_17ResilientProtocol32defaultWitnessWithAssociatedTypefwx9AssocTypeT_
  public func defaultWitnessWithAssociatedType(a: AssocType) {}

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience17ResilientProtocol41defaultWitnessMoreAbstractThanRequirementfTwx9AssocType1bSi_T_
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_17ResilientProtocol41defaultWitnessMoreAbstractThanRequirementu0_rfTqd__1bqd_0__T_
  public func defaultWitnessMoreAbstractThanRequirement<A, T>(a: A, b: T) {}

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience17ResilientProtocol48defaultWitnessMoreAbstractThanGenericRequirementurfTwx9AssocType1tqd___T_
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_17ResilientProtocol48defaultWitnessMoreAbstractThanGenericRequirementu0_rfTqd__1tqd_0__T_
  public func defaultWitnessMoreAbstractThanGenericRequirement<A, T>(a: A, t: T) {}

// CHECK-LABEL: sil [transparent] [thunk] @_TZFP19protocol_resilience17ResilientProtocol20staticDefaultWitnessfSix
// CHECK-LABEL: sil @_TZFE19protocol_resiliencePS_17ResilientProtocol20staticDefaultWitnessfSix
  public static func staticDefaultWitness(x: Int) -> Self {}

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience17ResilientProtocolCfT7defaultT__x
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_17ResilientProtocolCfT7defaultT__x
  public init(default: ()) {
    self.init(noDefault: ())
  }

// CHECK-LABEL: sil [transparent] [thunk] @_TFP19protocol_resilience17ResilientProtocolCfT17defaultIsOptionalT__GSqx_
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_17ResilientProtocolCfT17defaultIsOptionalT__GSqx_
  public init?(defaultIsOptional: ()) {
    self.init(noDefault: ())
  }

  private func defaultWitnessIsNotPublic() {}

  public init(defaultIsNotOptional: ()) {
    self.init(noDefault: ())
  }

  public init?(optionalityMismatch: ()) {
    self.init(noDefault: ())
  }
}

// CHECK-LABEL: sil [transparent] [thunk] @_TZFP19protocol_resilience17ResilientProtocolop3tttfxT_
// CHECK-LABEL: sil @_TZF19protocol_resilienceop3ttturFxT_
public prefix func ~~~<S>(s: S) {}

// CHECK-LABEL: sil [transparent] [thunk] @_TZFP19protocol_resilience17ResilientProtocoloi3lmgurfTxqd___T_
// CHECK-LABEL: sil @_TZF19protocol_resilienceoi3lmgu0_rFTq_x_T_
public func <*><T, S>(s: S, t: T) {}

// Swap the generic parameters to make sure we don't mix up our DeclContexts
// when mapping interface types in and out

// CHECK-LABEL: sil [transparent] [thunk] @_TZFP19protocol_resilience17ResilientProtocoloi4lmmgurfTqd__x_wx9AssocType
// CHECK-LABEL: sil @_TZF19protocol_resilienceoi4lmmgu0_RxS_17ResilientProtocolrFTq_x_wx9AssocType
public func <**><S : ResilientProtocol, T>(t: T, s: S) -> S.AssocType {}

// CHECK-LABEL: sil [transparent] [thunk] @_TZFP19protocol_resilience17ResilientProtocoloi5leeeguRd__S0_rfTqd__x_wd__9AssocType
// CHECK-LABEL: sil @_TZF19protocol_resilienceoi5leeegu0_RxS_17ResilientProtocol_S0_rFTxq__wx9AssocType
public func <===><T : ResilientProtocol, S : ResilientProtocol>(t: T, s: S) -> T.AssocType {}

// Protocol is not public -- doesn't need default witness table
protocol InternalProtocol {
  func f()
}

// Not referenced from default witness table

// CHECK-LABEL: sil private @_TFE19protocol_resiliencePS_17ResilientProtocolP{{.*}}25defaultWitnessIsNotPublicfT_T_
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_17ResilientProtocolCfT20defaultIsNotOptionalT__x
// CHECK-LABEL: sil @_TFE19protocol_resiliencePS_17ResilientProtocolCfT19optionalityMismatchT__GSqx_


// CHECK-LABEL: sil_default_witness_table P {
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ResilientProtocol {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientProtocol.defaultWitness!1: @_TFP19protocol_resilience17ResilientProtocol14defaultWitnessfT_T_
// CHECK-NEXT:    method #ResilientProtocol.anotherDefaultWitness!1: @_TFP19protocol_resilience17ResilientProtocol21anotherDefaultWitnessfSix
// CHECK-NEXT:    method #ResilientProtocol.defaultWitnessWithAssociatedType!1: @_TFP19protocol_resilience17ResilientProtocol32defaultWitnessWithAssociatedTypefwx9AssocTypeT_
// CHECK-NEXT:    method #ResilientProtocol.defaultWitnessMoreAbstractThanRequirement!1: @_TFP19protocol_resilience17ResilientProtocol41defaultWitnessMoreAbstractThanRequirementfTwx9AssocType1bSi_T_
// CHECK-NEXT:    method #ResilientProtocol.defaultWitnessMoreAbstractThanGenericRequirement!1: @_TFP19protocol_resilience17ResilientProtocol48defaultWitnessMoreAbstractThanGenericRequirementurfTwx9AssocType1tqd___T_
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientProtocol.staticDefaultWitness!1: @_TZFP19protocol_resilience17ResilientProtocol20staticDefaultWitnessfSix
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientProtocol.init!allocator.1: @_TFP19protocol_resilience17ResilientProtocolCfT7defaultT__x
// CHECK-NEXT:    method #ResilientProtocol.init!allocator.1: @_TFP19protocol_resilience17ResilientProtocolCfT17defaultIsOptionalT__GSqx_
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ResilientProtocol."~~~"!1: @_TZFP19protocol_resilience17ResilientProtocolop3tttfxT_
// CHECK-NEXT:    method #ResilientProtocol."<*>"!1: @_TZFP19protocol_resilience17ResilientProtocoloi3lmgurfTxqd___T_
// CHECK-NEXT:    method #ResilientProtocol."<**>"!1: @_TZFP19protocol_resilience17ResilientProtocoloi4lmmgurfTqd__x_wx9AssocType
// CHECK-NEXT:    method #ResilientProtocol."<===>"!1: @_TZFP19protocol_resilience17ResilientProtocoloi5leeeguRd__S0_rfTqd__x_wd__9AssocType
// CHECK-NEXT: }

// GLOBAL-NOT: sil_default_witness_table InternalProtocol
