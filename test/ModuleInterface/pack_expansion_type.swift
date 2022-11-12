// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/PackExpansionType.swiftinterface) %s -module-name PackExpansionType -enable-experimental-feature VariadicGenerics
// RUN: %FileCheck %s < %t/PackExpansionType.swiftinterface

// CHECK: public func variadicFunction<T..., U...>(t: T..., u: U...) -> ((T, U)...) where ((T, U)...) : Any
public func variadicFunction<T..., U...>(t: T..., u: U...) -> ((T, U)...) {}

// CHECK: public struct VariadicType<T...> {
public struct VariadicType<T...> {
  // CHECK: public func variadicMethod<U...>(t: T..., u: U...) -> ((T, U)...) where ((T, U)...) : Any
  public func variadicMethod<U...>(t: T..., u: U...) -> ((T, U)...) {}
}
// CHECK: }
