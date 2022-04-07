// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -module-name ParameterizedProtocols -emit-module-interface-path %t/ParameterizedProtocols.swiftinterface %s
// RUN: %FileCheck %s < %t/ParameterizedProtocols.swiftinterface

public protocol HasPrimaryAssociatedTypes<T, U> {
  associatedtype T : Collection where T.Element == U
  associatedtype U : Equatable
}

// CHECK: #if compiler(>=5.3) && $PrimaryAssociatedTypes
// CHECK-NEXT: public protocol HasPrimaryAssociatedTypes<T, U> {
// CHECK-NEXT:   associatedtype T : Swift.Collection
// CHECK-NEXT:   associatedtype U : Swift.Equatable where Self.U == Self.T.Element
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: public protocol HasPrimaryAssociatedTypes {
// CHECK-NEXT:   associatedtype T : Swift.Collection
// CHECK-NEXT:   associatedtype U : Swift.Equatable where Self.U == Self.T.Element
// CHECK-NEXT: }
// CHECK-NEXT: #endif
