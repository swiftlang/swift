// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/ParameterizedProtocols.swiftinterface) %s -module-name ParameterizedProtocols
// RUN: %target-swift-typecheck-module-from-interface(%t/ParameterizedProtocols.swiftinterface) -module-name ParameterizedProtocols
// RUN: %FileCheck %s < %t/ParameterizedProtocols.swiftinterface

public protocol HasPrimaryAssociatedTypes<T, U> {
  associatedtype T : Collection where T.Element == U
  associatedtype U : Equatable
}

// CHECK:      public protocol HasPrimaryAssociatedTypes<T, U> {
// CHECK-NEXT:   associatedtype T : Swift.Collection
// CHECK-NEXT:   associatedtype U : Swift.Equatable where Self.U == Self.T.Element
// CHECK-NEXT: }
