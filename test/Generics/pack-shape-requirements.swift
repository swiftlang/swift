// RUN: %target-swift-frontend -typecheck -enable-experimental-feature VariadicGenerics %s -debug-generic-signatures 2>&1 | %FileCheck %s

// REQUIRES: asserts

protocol P {
  associatedtype A
}

// CHECK-LABEL: inferSameShape(ts:us:)
// CHECK-NEXT: Generic signature: <T..., U... where ((T, U)...) : Any>
func inferSameShape<T..., U...>(ts t: T..., us u: U...) where ((T, U)...): Any {
}

// CHECK-LABEL: desugarSameShape(ts:us:)
// CHECK-NEXT: Generic signature: <T..., U... where T : P, ((T, U)...) : Any, U : P>
func desugarSameShape<T..., U...>(ts t: T..., us u: U...) where T: P, U: P, ((T.A, U.A)...): Any {
}

// CHECK-LABEL: multipleSameShape1(ts:us:vs:)
// CHECK-NEXT: Generic signature: <T..., U..., V... where ((T, U)...) : Any, ((U, V)...) : Any>
func multipleSameShape1<T..., U..., V...>(ts t: T..., us u: U..., vs v: V...) where ((T, U, V)...): Any {
}

// CHECK-LABEL: multipleSameShape2(ts:us:vs:)
// CHECK-NEXT: Generic signature: <T..., U..., V... where ((T, U)...) : Any, ((U, V)...) : Any>
func multipleSameShape2<T..., U..., V...>(ts t: T..., us u: U..., vs v: V...) where ((V, T, U)...): Any {
}

// CHECK-LABEL: multipleSameShape3(ts:us:vs:)
// CHECK-NEXT: Generic signature: <T..., U..., V... where ((T, U)...) : Any, ((U, V)...) : Any>
func multipleSameShape3<T..., U..., V...>(ts t: T..., us u: U..., vs v: V...) where ((U, V, T)...): Any {
}

// CHECK-LABEL: multipleSameShape4(ts:us:vs:)
// CHECK-NEXT: Generic signature: <T..., U..., V... where ((T, U)...) : Any, ((U, V)...) : Any>
func multipleSameShape4<T..., U..., V...>(ts t: T..., us u: U..., vs v: V...) where ((U, T, V)...): Any {
}

// CHECK-LABEL: multipleSameShape5(ts:us:vs:)
// CHECK-NEXT: Generic signature: <T..., U..., V... where ((T, U)...) : Any, ((U, V)...) : Any>
func multipleSameShape5<T..., U..., V...>(ts t: T..., us u: U..., vs v: V...) where ((T, V, U)...): Any {
}

// CHECK-LABEL: multipleSameShape6(ts:us:vs:)
// CHECK-NEXT: Generic signature: <T..., U..., V... where ((T, U)...) : Any, ((U, V)...) : Any>
func multipleSameShape6<T..., U..., V...>(ts t: T..., us u: U..., vs v: V...) where ((V, U, T)...): Any {
}

struct Ts<T...> {
  struct Us<U...> {
    // CHECK-LABEL: Ts.Us.packEquality()
    // CHECK-NEXT: Generic signature: <T..., U... where T == U>
    func packEquality() where T == U, ((T, U)...): Any {
    }

    struct Vs<V...> {
      // CHECK-LABEL: Ts.Us.Vs.packEquality()
      // CHECK-NEXT: Generic signature: <T..., U..., V... where T == U, ((T, V)...) : Any>
      func packEquality() where T == U, ((U, V)...): Any {
      }
    }
  }
}

// CHECK-LABEL: expandedParameters(_:transform:)
// CHECK-NEXT: Generic signature: <T..., Result... where ((T, Result)...) : Any>
func expandedParameters<T..., Result...>(_ t: T..., transform: ((T) -> Result)...) -> (Result...) {
  fatalError()
}
