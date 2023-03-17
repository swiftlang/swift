// RUN: %target-swift-emit-silgen -enable-experimental-feature VariadicGenerics %s | %FileCheck %s

// Because of -enable-experimental-feature VariadicGenerics
// REQUIRES: asserts

public struct G<T> {}

// CHECK-LABEL: sil [ossa] @$s4main6caller2fnyyAA1GVyxGxQpXE_tRvzlF : $@convention(thin) <each T> (@guaranteed @noescape @callee_guaranteed @substituted <each τ_0_0> (@pack_guaranteed Pack{repeat G<each τ_0_0>}) -> () for <Pack{repeat each T}>) -> () {
public func caller<each T>(fn: (repeat G<each T>) -> ()) {
  fn(repeat G<each T>())
}
