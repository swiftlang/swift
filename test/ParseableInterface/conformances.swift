// RUN: %target-swift-frontend-typecheck -emit-parseable-module-interface-path %t.swiftinterface %s
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %FileCheck -check-prefix CHECK-END %s < %t.swiftinterface
// RUN: %FileCheck -check-prefix NEGATIVE %s < %t.swiftinterface

// CHECK-LABEL: public protocol SimpleProto {
public protocol SimpleProto {
  // CHECK: associatedtype Element
  associatedtype Element
  // CHECK: associatedtype Inferred
  associatedtype Inferred
  func inference(_: Inferred)
} // CHECK: {{^}$}}

// CHECK-LABEL: public struct SimpleImpl<Element> : SimpleProto {
public struct SimpleImpl<Element>: SimpleProto {
  // NEGATIVE-NOT: typealias Element =
  // CHECK: public func inference(_: Int){{$}}
  public func inference(_: Int) {}
  // CHECK: public typealias Inferred = Swift.Int
} // CHECK: {{^}$}}


public protocol PublicProto {}
private protocol PrivateProto {}

// CHECK: public struct A1 : PublicProto {
// NEGATIVE-NOT: extension conformances.A1
public struct A1: PublicProto, PrivateProto {}
// CHECK: public struct A2 : PublicProto {
// NEGATIVE-NOT: extension conformances.A2
public struct A2: PrivateProto, PublicProto {}
// CHECK: public struct A3 {
// CHECK-END: extension conformances.A3 : PublicProto {}
public struct A3: PublicProto & PrivateProto {}
// CHECK: public struct A4 {
// CHECK-END: extension conformances.A4 : PublicProto {}
public struct A4: PrivateProto & PublicProto {}

public protocol PublicBaseProto {}
private protocol PrivateSubProto: PublicBaseProto {}

// CHECK: public struct B1 {
// CHECK-END: extension conformances.B1 : PublicBaseProto {}
public struct B1: PrivateSubProto {}
// CHECK: public struct B2 : PublicBaseProto {
// NEGATIVE-NOT: extension conformances.B2
public struct B2: PublicBaseProto, PrivateSubProto {}
// CHECK: public struct B3 {
// CHECK-END: extension conformances.B3 : PublicBaseProto {}
public struct B3: PublicBaseProto & PrivateSubProto {}
// CHECK: public struct B4 : PublicBaseProto {
// CHECK: extension B4 {
// NEGATIVE-NOT: extension conformances.B4
public struct B4: PublicBaseProto {}
extension B4: PrivateSubProto {}
// CHECK: public struct B5 {
// CHECK: extension B5 : PublicBaseProto {
// NEGATIVE-NOT: extension conformances.B5
public struct B5: PrivateSubProto {}
extension B5: PublicBaseProto {}
// CHECK: public struct B6 {
// CHECK: extension B6 {
// CHECK: extension B6 : PublicBaseProto {
// NEGATIVE-NOT: extension conformances.B6
public struct B6 {}
extension B6: PrivateSubProto {}
extension B6: PublicBaseProto {}
// CHECK: public struct B7 {
// CHECK: extension B7 : PublicBaseProto {
// CHECK: extension B7 {
// NEGATIVE-NOT: extension conformances.B7
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
// CHECK-END: extension conformances.OuterGeneric.Inner : PublicBaseProto {}

private protocol AnotherPrivateSubProto: PublicBaseProto {}

// CHECK: public struct C1 {
// CHECK-END: extension conformances.C1 : PublicBaseProto {}
public struct C1: PrivateSubProto, AnotherPrivateSubProto {}
// CHECK: public struct C2 {
// CHECK-END: extension conformances.C2 : PublicBaseProto {}
public struct C2: PrivateSubProto & AnotherPrivateSubProto {}
// CHECK: public struct C3 {
// CHECK: extension C3 {
// CHECK-END: extension conformances.C3 : PublicBaseProto {}
public struct C3: PrivateSubProto {}
extension C3: AnotherPrivateSubProto {}

public protocol PublicSubProto: PublicBaseProto {}
public protocol APublicSubProto: PublicBaseProto {}

// CHECK: public struct D1 : PublicSubProto {
// NEGATIVE-NOT: extension conformances.D1
public struct D1: PublicSubProto, PrivateSubProto {}
// CHECK: public struct D2 : PublicSubProto {
// NEGATIVE-NOT: extension conformances.D2
public struct D2: PrivateSubProto, PublicSubProto {}
// CHECK: public struct D3 {
// CHECK-END: extension conformances.D3 : PublicBaseProto, PublicSubProto {}
public struct D3: PrivateSubProto & PublicSubProto {}
// CHECK: public struct D4 {
// CHECK-END: extension conformances.D4 : APublicSubProto, PublicBaseProto {}
public struct D4: APublicSubProto & PrivateSubProto {}
// CHECK: public struct D5 {
// CHECK: extension D5 : PublicSubProto {
// NEGATIVE-NOT: extension conformances.D5
public struct D5: PrivateSubProto {}
extension D5: PublicSubProto {}
// CHECK: public struct D6 : PublicSubProto {
// CHECK: extension D6 {
// NEGATIVE-NOT: extension conformances.D6
public struct D6: PublicSubProto {}
extension D6: PrivateSubProto {}

private typealias PrivateProtoAlias = PublicProto

// CHECK: public struct E1 {
// CHECK-END: extension conformances.E1 : PublicProto {}
public struct E1: PrivateProtoAlias {}

private typealias PrivateSubProtoAlias = PrivateSubProto

// CHECK: public struct F1 {
// CHECK-END: extension conformances.F1 : PublicBaseProto {}
public struct F1: PrivateSubProtoAlias {}

private protocol ClassConstrainedProto: PublicProto, AnyObject {}

public class G1: ClassConstrainedProto {}
// CHECK: public class G1 {
// CHECK-END: extension conformances.G1 : PublicProto {}

public class Base {}
private protocol BaseConstrainedProto: Base, PublicProto {}

public class H1: Base, ClassConstrainedProto {}
// CHECK: public class H1 : Base {
// CHECK-END: extension conformances.H1 : PublicProto {}
