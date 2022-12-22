// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/PackExpansionType.swiftinterface) %s -module-name PackExpansionType -enable-experimental-feature VariadicGenerics
// RUN: %FileCheck %s < %t/PackExpansionType.swiftinterface

// Experimental features require an asserts compiler
// REQUIRES: asserts

// CHECK: public func variadicFunction<T..., U...>(t: T..., u: U...) -> ((T, U)...) where ((T, U)...) : Any
public func variadicFunction<T..., U...>(t: repeat each T, u: repeat each  U) -> (repeat (each T, each U)) {}

// CHECK: public struct VariadicType<T...> {
public struct VariadicType<T...> {
  // CHECK: public func variadicMethod<U...>(t: T..., u: U...) -> ((T, U)...) where ((T, U)...) : Any
  public func variadicMethod<U...>(t: repeat each T, u: repeat each  U) -> (repeat (each T, each U)) {}

  // CHECK: public func returnsSelf() -> PackExpansionType.VariadicType<T...>
  public func returnsSelf() -> Self {}
}
// CHECK: }

// CHECK: public func returnsVariadicType() -> PackExpansionType.VariadicType<>
public func returnsVariadicType() -> VariadicType< > {}

// CHECK: public func returnsVariadicType() -> PackExpansionType.VariadicType<Swift.Int, Swift.String, Swift.Float>
public func returnsVariadicType() -> VariadicType<Int, String, Float> {}

// CHECK: public func returnsVariadicType<T...>() -> PackExpansionType.VariadicType<T...>
public func returnsVariadicType<T...>() -> VariadicType<repeat each T> {}

// CHECK: public typealias VariadicAlias<T...> = PackExpansionType.VariadicType<Swift.Array<T>...>
public typealias VariadicAlias<T...> = VariadicType<repeat Array<each T>>

// CHECK: public func returnsVariadicAlias() -> PackExpansionType.VariadicAlias<>
public func returnsVariadicAlias() -> VariadicAlias< > {}

// CHECK: public func returnsVariadicAlias() -> PackExpansionType.VariadicAlias<Swift.Int, Swift.String, Swift.Float>
public func returnsVariadicAlias() -> VariadicAlias<Int, String, Float> {}

// CHECK: public func returnsVariadicAlias<T...>() -> PackExpansionType.VariadicAlias<T...>
public func returnsVariadicAlias<T...>() -> VariadicAlias<repeat each T> {}
