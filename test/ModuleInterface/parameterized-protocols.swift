// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -module-name ParameterizedProtocols -emit-module-interface-path %t/ParameterizedProtocols.swiftinterface -enable-parameterized-protocol-types %s
// RUN: %FileCheck %s < %t/ParameterizedProtocols.swiftinterface

public protocol HasPrimaryAssociatedTypes<T, U : Collection, V : Equatable = Int> where U.Element == Int {}

// CHECK: #if compiler(>=5.3) && $PrimaryAssociatedTypes
// CHECK-NEXT: public protocol HasPrimaryAssociatedTypes<T, U : Swift.Collection, V : Swift.Equatable = Swift.Int> where Self.U.Element == Swift.Int {
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: public protocol HasPrimaryAssociatedTypes {
// CHECK-NEXT:   associatedtype T
// CHECK-NEXT:   associatedtype U : Swift.Collection where Self.U.Element == Swift.Int
// CHECK-NEXT:   associatedtype V : Swift.Equatable = Swift.Int
// CHECK-NEXT: }
// CHECK-NEXT: #endif
