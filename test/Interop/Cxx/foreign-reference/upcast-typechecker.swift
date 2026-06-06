// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default -enable-experimental-feature ForeignReferenceTypeInheritance -disable-availability-checking -I %S/Inputs

// REQUIRES: swift_feature_ForeignReferenceTypeInheritance

import Upcast

func upcastDerived(_ d: Derived) -> Base {
  return d as Base
}

func upcastLeaf(_ l: LeafDerived) -> Derived {
  return l as Derived
}

func upcastLeafToBase(_ l: LeafDerived) -> Base {
  return l as Base
}

func takesBase(_ b: Base) -> Int32 {
  return b.getBaseValue()
}
func implicitConversion(_ d: Derived) -> Int32 {
  return takesBase(d)
}
func implicitConversionLeaf(_ l: LeafDerived) -> Int32 {
  return takesBase(l)
}

// MARK: - Overridden lifetime operations:

func upcastOverridesToDerived(_ c: OverridesLifetimeOps) -> RefCountedDerived {
  return c as RefCountedDerived // expected-error {{cannot convert value of type 'OverridesLifetimeOps' to type 'RefCountedDerived' in coercion}}
}

func upcastOverridesToBase(_ c: OverridesLifetimeOps) -> RefCountedBase {
  return c as RefCountedBase // expected-error {{cannot convert value of type 'OverridesLifetimeOps' to type 'RefCountedBase' in coercion}}
}

func upcastDerivedToOverrides(_ d: OverridesLifetimeOpsDerived) -> OverridesLifetimeOps {
  return d as OverridesLifetimeOps
}

func upcastDerivedToRefCountedDerived(_ d: OverridesLifetimeOpsDerived) -> RefCountedDerived {
  return d as RefCountedDerived // expected-error {{cannot convert value of type 'OverridesLifetimeOpsDerived' to type 'RefCountedDerived' in coercion}}
}

func unrelatedCast(_ u: Unrelated) -> Base {
  return u as Base // expected-error {{cannot convert value of type 'Unrelated' to type 'Base' in coercion}}
}

func virtualBaseCast(_ v: VirtualDerived) -> Base {
  return v as Base // expected-error {{cannot convert value of type 'VirtualDerived' to type 'Base' in coercion}}
}

func privateBaseCast(_ p: PrivateDerived) -> Base {
  return p as Base // expected-error {{cannot convert value of type 'PrivateDerived' to type 'Base' in coercion}}
}

func protectedBaseCast(_ p: ProtectedDerived) -> Base {
  return p as Base // expected-error {{cannot convert value of type 'ProtectedDerived' to type 'Base' in coercion}}
}

func arrayUpcast(_ arr: [Derived]) -> [Base] {
  return arr as [Base] // expected-error {{cannot convert value of type '[Derived]' to type '[Base]' in coercion}}
  // expected-note@-1 {{arguments to generic parameter 'Element' ('Derived' and 'Base') are expected to be equal}}
}

func arrayUpcastImplicit(_ arr: [Derived]) {
  let _: [Base] = arr // expected-error {{cannot assign value of type '[Derived]' to type '[Base]'}}
  // expected-note@-1 {{arguments to generic parameter 'Element' ('Derived' and 'Base') are expected to be equal}}
}

/// This conformance is needed to let us write Set<Derived>.
extension Base: @retroactive Hashable {
  public static func == (lhs: Base, rhs: Base) -> Bool {
    return lhs.getBaseValue() == rhs.getBaseValue()
  }
  public func hash(into hasher: inout Hasher) {
    hasher.combine(getBaseValue())
  }
}

func setUpcast(_ s: Set<Derived>) -> Set<Base> {
  return s as Set<Base> // expected-error {{cannot convert value of type 'Set<Derived>' to type 'Set<Base>' in coercion}}
  // expected-note@-1 {{arguments to generic parameter 'Element' ('Derived' and 'Base') are expected to be equal}}
}

func setUpcastImplicit(_ s: Set<Derived>) {
  let _: Set<Base> = s // expected-error {{cannot assign value of type 'Set<Derived>' to type 'Set<Base>'}}
  // expected-note@-1 {{arguments to generic parameter 'Element' ('Derived' and 'Base') are expected to be equal}}
}

func castToEmptyTag(_ d: DerivedFromEmptyAndBase) -> EmptyTag {
  return d as EmptyTag // expected-error {{cannot convert value of type 'DerivedFromEmptyAndBase' to type 'EmptyTag' in coercion}}
}

// MARK: - Upcasts with different optional levels:

func upcastOptionalToOptional(_ d: Derived?) -> Base? {
  return d as Base?
}

func upcastToOptional(_ d: Derived) -> Base? {
  return d as Base?
}

func upcastDoubleOptional(_ d: Derived??) -> Base?? {
  return d as Base??
}

func upcastOptionalToDoubleOptional(_ d: Derived?) -> Base?? {
  return d as Base??
}

func upcastImplicitOptional(_ d: Derived?) {
  let _: Base? = d
}

func upcastImplicitDoubleOptional(_ d: Derived??) {
  let _: Base?? = d
}

func upcastDoubleOptionalToSingleOptional(_ d: Derived??) -> Base? {
  return d as Base? // expected-error {{cannot convert value of type 'Derived??' to type 'Base?' in coercion}}
  // expected-note@-1 {{arguments to generic parameter 'Wrapped' ('Derived?' and 'Base') are expected to be equal}}
}

// MARK: - Unsupported downcasts:

func downcast(_ b: Base) -> Derived? {
  return b as Derived // expected-error {{cannot convert value of type 'Base' to type 'Derived' in coercion}}
}

func downcastConditional(_ b: Base) -> Derived? {
  return b as? Derived // expected-error {{downcast from 'Base' to 'Derived' is not supported for foreign reference types}}
  // expected-warning@-1 {{cast from 'Base' to unrelated type 'Derived' always fails}}
}

func downcastForced(_ b: Base) -> Derived {
  return b as! Derived // expected-error {{downcast from 'Base' to 'Derived' is not supported for foreign reference types}}
  // expected-warning@-1 {{cast from 'Base' to unrelated type 'Derived' always fails}}
}

func downcastIs(_ b: Base) -> Bool {
  return b is Derived // expected-error {{downcast from 'Base' to 'Derived' is not supported for foreign reference types}}
  // expected-warning@-1 {{cast from 'Base' to unrelated type 'Derived' always fails}}
}

// MARK: - CRTP (derived inherits from template base specialized on itself):

func upcastCRTP(_ d: CRTPDerived) -> CRTPBaseOfDerived {
  return d as CRTPBaseOfDerived
}

func takesCRTPBase(_ b: CRTPBaseOfDerived) -> Int32 {
  return b.crtpBaseValue
}

func implicitConversionCRTP(_ d: CRTPDerived) -> Int32 {
  return takesCRTPBase(d)
}

func crtpDerivedCallsBaseMethod(_ d: CRTPDerived) -> Int32 {
  return d.crtpBaseValue
}

func crtpSelf(_ d: CRTPDerived) -> CRTPDerived? {
  return d.derivedSelf()
}

func crtpDowncast(_ b: CRTPBaseOfDerived) -> CRTPDerived {
  return b as! CRTPDerived // expected-error {{downcast from 'CRTPBaseOfDerived' (aka 'CRTPBase<CRTPDerived>') to 'CRTPDerived' is not supported for foreign reference types}}
  // expected-warning@-1 {{cast from 'CRTPBaseOfDerived' (aka 'CRTPBase<CRTPDerived>') to unrelated type 'CRTPDerived' always fails}}
}
