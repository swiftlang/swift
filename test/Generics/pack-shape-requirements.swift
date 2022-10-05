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

// CHECK-LABEL: multipleSameShape1(ts:us:vs:)
// CHECK-NEXT: Generic signature: <@_typeSequence T, @_typeSequence U, @_typeSequence V where T.count == U.count, U.count == V.count>
func multipleSameShape1<@_typeSequence T, @_typeSequence U, @_typeSequence V>(ts t: T..., us u: U..., vs v: V...) where ((T, U, V)...): Any {
}

// CHECK-LABEL: multipleSameShape2(ts:us:vs:)
// CHECK-NEXT: Generic signature: <@_typeSequence T, @_typeSequence U, @_typeSequence V where T.count == U.count, U.count == V.count>
func multipleSameShape2<@_typeSequence T, @_typeSequence U, @_typeSequence V>(ts t: T..., us u: U..., vs v: V...) where ((V, T, U)...): Any {
}

// CHECK-LABEL: multipleSameShape3(ts:us:vs:)
// CHECK-NEXT: Generic signature: <@_typeSequence T, @_typeSequence U, @_typeSequence V where T.count == U.count, U.count == V.count>
func multipleSameShape3<@_typeSequence T, @_typeSequence U, @_typeSequence V>(ts t: T..., us u: U..., vs v: V...) where ((U, V, T)...): Any {
}

// CHECK-LABEL: multipleSameShape4(ts:us:vs:)
// CHECK-NEXT: Generic signature: <@_typeSequence T, @_typeSequence U, @_typeSequence V where T.count == U.count, U.count == V.count>
func multipleSameShape4<@_typeSequence T, @_typeSequence U, @_typeSequence V>(ts t: T..., us u: U..., vs v: V...) where ((U, T, V)...): Any {
}

// CHECK-LABEL: multipleSameShape5(ts:us:vs:)
// CHECK-NEXT: Generic signature: <@_typeSequence T, @_typeSequence U, @_typeSequence V where T.count == U.count, U.count == V.count>
func multipleSameShape5<@_typeSequence T, @_typeSequence U, @_typeSequence V>(ts t: T..., us u: U..., vs v: V...) where ((T, V, U)...): Any {
}

// CHECK-LABEL: multipleSameShape6(ts:us:vs:)
// CHECK-NEXT: Generic signature: <@_typeSequence T, @_typeSequence U, @_typeSequence V where T.count == U.count, U.count == V.count>
func multipleSameShape6<@_typeSequence T, @_typeSequence U, @_typeSequence V>(ts t: T..., us u: U..., vs v: V...) where ((V, U, T)...): Any {
}

struct Ts<@_typeSequence T> {
  struct Us<@_typeSequence U> {
    // CHECK-LABEL: Ts.Us.packEquality()
    // CHECK-NEXT: Generic signature: <@_typeSequence T, @_typeSequence U where T == U>
    func packEquality() where T == U, ((T, U)...): Any {
    }

    struct Vs<@_typeSequence V> {
      // CHECK-LABEL: Ts.Us.Vs.packEquality()
      // CHECK-NEXT: Generic signature: <@_typeSequence T, @_typeSequence U, @_typeSequence V where T == U, T.count == V.count>
      func packEquality() where T == U, ((U, V)...): Any {
      }
    }
  }
}
