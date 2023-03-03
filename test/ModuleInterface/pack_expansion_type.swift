// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/PackExpansionType.swiftinterface) %s -module-name PackExpansionType -enable-experimental-feature VariadicGenerics
// RUN: %FileCheck %s < %t/PackExpansionType.swiftinterface

// Experimental features require an asserts compiler
// REQUIRES: asserts

// CHECK: public func variadicFunction<each T, each U>(t: repeat each T, u: repeat each U) -> (repeat (each T, each U)) where (repeat (each T, each U)) : Any
public func variadicFunction<each T, each U>(t: repeat each T, u: repeat each U) -> (repeat (each T, each U)) {}

// CHECK: public struct VariadicType<each T> {
public struct VariadicType<each T> {
  // CHECK: public func variadicMethod<each U>(t: repeat each T, u: repeat each U) -> (repeat (each T, each U)) where (repeat (each T, each U)) : Any
  public func variadicMethod<each U>(t: repeat each T, u: repeat each  U) -> (repeat (each T, each U)) {}

  // CHECK: public func returnsSelf() -> PackExpansionType.VariadicType<repeat each T>
  public func returnsSelf() -> Self {}
}
// CHECK: }

// CHECK: public func returnsVariadicType() -> PackExpansionType.VariadicType<>
public func returnsVariadicType() -> VariadicType< > {}

// CHECK: public func returnsVariadicType() -> PackExpansionType.VariadicType<Swift.Int, Swift.String, Swift.Float>
public func returnsVariadicType() -> VariadicType<Int, String, Float> {}

// CHECK: public func returnsVariadicType<each T>() -> PackExpansionType.VariadicType<repeat each T>
public func returnsVariadicType<each T>() -> VariadicType<repeat each T> {}

// CHECK: public typealias VariadicAlias<each T> = PackExpansionType.VariadicType<repeat Swift.Array<each T>>
public typealias VariadicAlias<each T> = VariadicType<repeat Array<each T>>

// CHECK: public func returnsVariadicAlias() -> PackExpansionType.VariadicAlias<>
public func returnsVariadicAlias() -> VariadicAlias< > {}

// CHECK: public func returnsVariadicAlias() -> PackExpansionType.VariadicAlias<Swift.Int, Swift.String, Swift.Float>
public func returnsVariadicAlias() -> VariadicAlias<Int, String, Float> {}

// CHECK: public func returnsVariadicAlias<each T>() -> PackExpansionType.VariadicAlias<repeat each T>
public func returnsVariadicAlias<each T>() -> VariadicAlias<repeat each T> {}
