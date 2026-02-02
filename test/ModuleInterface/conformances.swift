// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name conformances
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name conformances
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %FileCheck -check-prefix CHECK-END %s < %t.swiftinterface
// RUN: %FileCheck -check-prefix NEGATIVE %s < %t.swiftinterface

// NEGATIVE-NOT: BAD

// CHECK-LABEL: public protocol SimpleProto {
public protocol SimpleProto {
  // CHECK: associatedtype Element
  associatedtype Element
  // CHECK: associatedtype Inferred
  associatedtype Inferred
  func inference(_: Inferred)
} // CHECK: {{^}$}}

// CHECK-LABEL: public struct SimpleImpl<Element> : conformances.SimpleProto {
public struct SimpleImpl<Element>: SimpleProto {
  // NEGATIVE-NOT: typealias Element =
  // CHECK: public func inference(_: Swift.Int){{$}}
  public func inference(_: Int) {}
  // CHECK: public typealias Inferred = Swift.Int
} // CHECK: {{^}$}}


public protocol PublicProto {}
private protocol PrivateProto {}

// CHECK: public struct A1 : conformances.PublicProto {
// NEGATIVE-NOT: extension conformances.A1
public struct A1: PublicProto, PrivateProto {}
// CHECK: public struct A2 : conformances.PublicProto {
// NEGATIVE-NOT: extension conformances.A2
public struct A2: PrivateProto, PublicProto {}
// CHECK: public struct A3 {
// CHECK-END: extension conformances.A3 : conformances.PublicProto {}
public struct A3: PublicProto & PrivateProto {}
// CHECK: public struct A4 {
// CHECK-END: extension conformances.A4 : conformances.PublicProto {}
public struct A4: PrivateProto & PublicProto {}

public protocol PublicBaseProto {}
private protocol PrivateSubProto: PublicBaseProto {}

// CHECK: public struct B1 {
// CHECK-END: extension conformances.B1 : conformances.PublicBaseProto {}
public struct B1: PrivateSubProto {}
// CHECK: public struct B2 : conformances.PublicBaseProto {
// NEGATIVE-NOT: extension conformances.B2
public struct B2: PublicBaseProto, PrivateSubProto {}
// CHECK: public struct B3 {
// CHECK-END: extension conformances.B3 : conformances.PublicBaseProto {}
public struct B3: PublicBaseProto & PrivateSubProto {}
// CHECK: public struct B4 : conformances.PublicBaseProto {
// NEGATIVE-NOT: extension conformances.B4 {
// NEGATIVE-NOT: extension conformances.B4
public struct B4: PublicBaseProto {}
extension B4: PrivateSubProto {}
// CHECK: public struct B5 {
// CHECK: extension conformances.B5 : conformances.PublicBaseProto {
// NEGATIVE-NOT: extension B5
public struct B5: PrivateSubProto {}
extension B5: PublicBaseProto {}
// CHECK: public struct B6 {
// NEGATIVE-NOT: extension conformances.B6 : conformances.PrivateSubProto
// NEGATIVE-NOT: extension conformances.B6 {
// CHECK: extension conformances.B6 : conformances.PublicBaseProto {
public struct B6 {}
extension B6: PrivateSubProto {}
extension B6: PublicBaseProto {}
// CHECK: public struct B7 {
// CHECK: extension conformances.B7 : conformances.PublicBaseProto {
// NEGATIVE-NOT: extension conformances.B7 : conformances.PrivateSubProto {
// NEGATIVE-NOT: extension conformances.B7 {
public struct B7 {}
extension B7: PublicBaseProto {}
extension B7: PrivateSubProto {}

// CHECK-LABEL: public struct OuterGeneric<T> {
public struct OuterGeneric<T> {
  // CHECK-NEXT: public struct Inner {
  public struct Inner: PrivateSubProto {}
  // CHECK-NEXT: {{^  }$}}
}
// CHECK-NEXT: {{^}$}}

public protocol ConditionallyConformed {}
public protocol ConditionallyConformedAgain {}

// CHECK-END: @available(*, unavailable)
// CHECK-END-NEXT: extension conformances.OuterGeneric : conformances.ConditionallyConformed, conformances.ConditionallyConformedAgain where T : _ConstraintThatIsNotPartOfTheAPIOfThisLibrary {}
extension OuterGeneric: ConditionallyConformed where T: PrivateProto {}
extension OuterGeneric: ConditionallyConformedAgain where T == PrivateProto {}

// CHECK-END: extension conformances.OuterGeneric.Inner : conformances.PublicBaseProto {}
// CHECK-END: @available(*, unavailable)
// CHECK-END-NEXT: extension conformances.OuterGeneric.Inner : conformances.ConditionallyConformed, conformances.ConditionallyConformedAgain where T : _ConstraintThatIsNotPartOfTheAPIOfThisLibrary {}
extension OuterGeneric.Inner: ConditionallyConformed where T: PrivateProto {}
extension OuterGeneric.Inner: ConditionallyConformedAgain where T == PrivateProto {}

private protocol AnotherPrivateSubProto: PublicBaseProto {}

// CHECK: public struct C1 {
// CHECK-END: extension conformances.C1 : conformances.PublicBaseProto {}
public struct C1: PrivateSubProto, AnotherPrivateSubProto {}
// CHECK: public struct C2 {
// CHECK-END: extension conformances.C2 : conformances.PublicBaseProto {}
public struct C2: PrivateSubProto & AnotherPrivateSubProto {}
// CHECK: public struct C3 {
// NEGATIVE-NOT: extension conformances.C3 {
// CHECK-END: extension conformances.C3 : conformances.PublicBaseProto {}
public struct C3: PrivateSubProto {}
extension C3: AnotherPrivateSubProto {}

public protocol PublicSubProto: PublicBaseProto {}
public protocol APublicSubProto: PublicBaseProto {}

// CHECK: public struct D1 : conformances.PublicSubProto {
// NEGATIVE-NOT: extension conformances.D1
public struct D1: PublicSubProto, PrivateSubProto {}
// CHECK: public struct D2 : conformances.PublicSubProto {
// NEGATIVE-NOT: extension conformances.D2
public struct D2: PrivateSubProto, PublicSubProto {}
// CHECK: public struct D3 {
// CHECK-END: extension conformances.D3 : conformances.PublicBaseProto {}
// CHECK-END: extension conformances.D3 : conformances.PublicSubProto {}
public struct D3: PrivateSubProto & PublicSubProto {}
// CHECK: public struct D4 {
// CHECK-END: extension conformances.D4 : conformances.APublicSubProto {}
// CHECK-END: extension conformances.D4 : conformances.PublicBaseProto {}
public struct D4: APublicSubProto & PrivateSubProto {}
// CHECK: public struct D5 {
// NEGATIVE-NOT: extension conformances.D5 : conformances.PrivateSubProto {
// NEGATIVE-NOT: extension conformances.D5 {
// CHECK: extension conformances.D5 : conformances.PublicSubProto {
public struct D5: PrivateSubProto {}
extension D5: PublicSubProto {}
// CHECK: public struct D6 : conformances.PublicSubProto {
// NEGATIVE-NOT: extension conformances.D6 {
// NEGATIVE-NOT: extension conformances.D6
public struct D6: PublicSubProto {}
extension D6: PrivateSubProto {}

private typealias PrivateProtoAlias = PublicProto

// CHECK: public struct E1 {
// CHECK-END: extension conformances.E1 : conformances.PublicProto {}
public struct E1: PrivateProtoAlias {}

private typealias PrivateSubProtoAlias = PrivateSubProto

// CHECK: public struct F1 {
// CHECK-END: extension conformances.F1 : conformances.PublicBaseProto {}
public struct F1: PrivateSubProtoAlias {}

private protocol ClassConstrainedProto: PublicProto, AnyObject {}

public class G1: ClassConstrainedProto {}
// CHECK: public class G1 {
// CHECK-END: extension conformances.G1 : conformances.PublicProto {}

public class Base {}
private protocol BaseConstrainedProto: Base, PublicProto {}

public class H1: Base, ClassConstrainedProto {}
// CHECK: public class H1 : conformances.Base {
// CHECK-END: extension conformances.H1 : conformances.PublicProto {}

public struct MultiGeneric<T, U, V> {}
extension MultiGeneric: PublicProto where U: PrivateProto {}

// CHECK: public struct MultiGeneric<T, U, V> {
// CHECK-END: @available(*, unavailable)
// CHECK-END-NEXT: extension conformances.MultiGeneric : conformances.PublicProto where T : _ConstraintThatIsNotPartOfTheAPIOfThisLibrary {}


internal struct InternalImpl_BAD: PrivateSubProto {}
internal struct InternalImplConstrained_BAD<T> {}
extension InternalImplConstrained_BAD: PublicProto where T: PublicProto {}
internal struct InternalImplConstrained2_BAD<T> {}
extension InternalImplConstrained2_BAD: PublicProto where T: PrivateProto {}

public struct WrapperForInternal {
  internal struct InternalImpl_BAD: PrivateSubProto {}
  internal struct InternalImplConstrained_BAD<T> {}
  internal struct InternalImplConstrained2_BAD<T> {}
}
extension WrapperForInternal.InternalImplConstrained_BAD: PublicProto where T: PublicProto {}
extension WrapperForInternal.InternalImplConstrained2_BAD: PublicProto where T: PrivateProto {}


internal protocol ExtraHashable: Hashable {}
extension Bool: ExtraHashable {}

@available(iOS, unavailable)
@available(macOS, unavailable)
public struct CoolTVType: PrivateSubProto {}
// CHECK: public struct CoolTVType {
// CHECK-END: @available(iOS, unavailable)
// CHECK-END-NEXT: @available(macOS, unavailable)
// CHECK-END-NEXT: extension conformances.CoolTVType : conformances.PublicBaseProto {}

@available(macOS 10.99, *)
public struct VeryNewMacType: PrivateSubProto {}
// CHECK: public struct VeryNewMacType {
// CHECK-END: @available(macOS 10.99, *)
// CHECK-END-NEXT: extension conformances.VeryNewMacType : conformances.PublicBaseProto {}

public struct VeryNewMacProto {}
@available(macOS 10.98, *)
extension VeryNewMacProto: PrivateSubProto {}
// CHECK: public struct VeryNewMacProto {
// CHECK-END: @available(macOS 10.98, *)
// CHECK-END-NEXT: extension conformances.VeryNewMacProto : conformances.PublicBaseProto {}

public struct PrivateProtoConformer {}
extension PrivateProtoConformer : PrivateProto {
  public var member: Int { return 0 }
}
// CHECK: public struct PrivateProtoConformer {
// CHECK: extension conformances.PrivateProtoConformer {
// CHECK-NEXT: public var member: Swift.Int {
// CHECK-NEXT:   get
// CHECK-NEXT: }
// CHECK-NEXT: {{^}$}}
// NEGATIVE-NOT: extension conformances.PrivateProtoConformer : conformances.PrivateProto {

// NEGATIVE-NOT: extension {{(Swift.)?}}Bool{{.+}}Hashable
// NEGATIVE-NOT: extension {{(Swift.)?}}Bool{{.+}}Equatable


@available(macOS 10.97, iOS 13.22, *)
@available(tvOS, unavailable)
@available(swift 4.2.123)
public struct NestedAvailabilityOuter {
  @available(iOS 13.23, *)
  public struct Inner: PrivateSubProto {}
}

@_nonSendable public struct NonSendable {}
// NEGATIVE-NOT: @_nonSendable
// CHECK-LABEL: public struct NonSendable {
// CHECK: @available(*, unavailable)
// CHECK-NEXT: extension conformances.NonSendable : @unchecked Swift.Sendable {

// CHECK-END: @available(macOS 10.97, iOS 13.23, *)
// CHECK-END: @available(tvOS, unavailable)
// CHECK-END: extension conformances.NestedAvailabilityOuter.Inner : conformances.PublicBaseProto {}


// CHECK-END: @usableFromInline
// CHECK-END-NEXT: internal protocol _ConstraintThatIsNotPartOfTheAPIOfThisLibrary {}
