// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/ParameterizedProtocols.swiftinterface) %s -module-name ParameterizedProtocols
// RUN: %target-swift-typecheck-module-from-interface(%t/ParameterizedProtocols.swiftinterface) -module-name ParameterizedProtocols
// RUN: %FileCheck %s < %t/ParameterizedProtocols.swiftinterface

public struct S1 : P1 {
  public typealias T = Int
}

public struct S2 : Q1 {}

protocol P1 : P2 {}

public protocol P2<T> {
  associatedtype T
}

protocol Q1 : Q2 {}

public protocol Q2 {}

// CHECK: extension ParameterizedProtocols.S1 : ParameterizedProtocols.P2 {}
// CHECK-NEXT: extension ParameterizedProtocols.S2 : ParameterizedProtocols.Q2 {}
