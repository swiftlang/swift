// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// This is a GenericSignatureBuilder bug fixed with the Requirement Machine, from
// https://bugs.swift.org/browse/SR-15917.

public struct Foo<Unused: CaseIterable> {
  public struct Nested {}

  public let closure: () -> Nested
}

// CHECK-LABEL: sil [transparent] [serialized] [ossa] @$s029type_lowering_subst_function_A13_caseiterable3FooV7closureAC6NestedVyx_Gycvg : $@convention(method) <Unused where Unused : CaseIterable> (@guaranteed Foo<Unused>) -> @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : CaseIterable, τ_0_0 == τ_0_1> () -> Foo<τ_0_0>.Nested for <Unused, Unused>