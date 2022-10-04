// RUN: %target-swift-frontend -typecheck -enable-experimental-variadic-generics %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P {
  associatedtype A
}

// CHECK-LABEL: inferSameShape(ts:us:)
// CHECK-NEXT: Generic signature: <@_typeSequence T, @_typeSequence U where T.count == U.count>
func inferSameShape<@_typeSequence T, @_typeSequence U>(ts t: T..., us u: U...) where ((T, U)...): Any {
}

// CHECK-LABEL: desugarSameShape(ts:us:)
// CHECK-NEXT: Generic signature: <@_typeSequence T, @_typeSequence U where T : P, T.count == U.count, U : P>
func desugarSameShape<@_typeSequence T, @_typeSequence U>(ts t: T..., us u: U...) where T: P, U: P, ((T.A, U.A)...): Any {
}
