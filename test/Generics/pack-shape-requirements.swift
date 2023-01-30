// RUN: %target-swift-frontend -typecheck -enable-experimental-feature VariadicGenerics %s -debug-generic-signatures 2>&1 | %FileCheck %s

// REQUIRES: asserts

protocol P {
  associatedtype A
}

// CHECK-LABEL: inferSameShape(ts:us:)
// CHECK-NEXT: Generic signature: <T..., U... where ((T, U)...) : Any>
func inferSameShape<T..., U...>(ts t: repeat each T, us u: repeat each U) where (repeat (each T, each U)): Any {
}

// CHECK-LABEL: desugarSameShape(ts:us:)
// CHECK-NEXT: Generic signature: <T..., U... where T : P, ((T, U)...) : Any, U : P>
func desugarSameShape<T..., U...>(ts t: repeat each T, us u: repeat each U) where T: P, U: P, (repeat (each T.A, each U.A)): Any {
}

// CHECK-LABEL: multipleSameShape1(ts:us:vs:)
// CHECK-NEXT: Generic signature: <T..., U..., V... where ((T, U)...) : Any, ((U, V)...) : Any>
func multipleSameShape1<T..., U..., V...>(ts t: repeat each T, us u: repeat each U, vs v: repeat each V) where (repeat (each T, each U, each V)): Any {
}

// CHECK-LABEL: multipleSameShape2(ts:us:vs:)
// CHECK-NEXT: Generic signature: <T..., U..., V... where ((T, U)...) : Any, ((U, V)...) : Any>
func multipleSameShape2<T..., U..., V...>(ts t: repeat each T, us u: repeat each U, vs v: repeat each V) where (repeat (each V, each T, each U)): Any {
}

// CHECK-LABEL: multipleSameShape3(ts:us:vs:)
// CHECK-NEXT: Generic signature: <T..., U..., V... where ((T, U)...) : Any, ((U, V)...) : Any>
func multipleSameShape3<T..., U..., V...>(ts t: repeat each T, us u: repeat each U, vs v: repeat each V) where (repeat (each U, each V, each T)): Any {
}

// CHECK-LABEL: multipleSameShape4(ts:us:vs:)
// CHECK-NEXT: Generic signature: <T..., U..., V... where ((T, U)...) : Any, ((U, V)...) : Any>
func multipleSameShape4<T..., U..., V...>(ts t: repeat each T, us u: repeat each U, vs v: repeat each V) where (repeat (each U, each T, each V)): Any {
}

// CHECK-LABEL: multipleSameShape5(ts:us:vs:)
// CHECK-NEXT: Generic signature: <T..., U..., V... where ((T, U)...) : Any, ((U, V)...) : Any>
func multipleSameShape5<T..., U..., V...>(ts t: repeat each T, us u: repeat each U, vs v: repeat each V) where (repeat (each T, each V, each U)): Any {
}

// CHECK-LABEL: multipleSameShape6(ts:us:vs:)
// CHECK-NEXT: Generic signature: <T..., U..., V... where ((T, U)...) : Any, ((U, V)...) : Any>
func multipleSameShape6<T..., U..., V...>(ts t: repeat each T, us u: repeat each U, vs v: repeat each V) where (repeat (each V, each U, each T)): Any {
}

struct Ts<T...> {
  struct Us<U...> {
    // CHECK-LABEL: Ts.Us.packEquality()
    // CHECK-NEXT: Generic signature: <T..., U... where T == U>
    func packEquality() where each T == each U, (repeat (each T, each U)): Any {
    }

    struct Vs<V...> {
      // CHECK-LABEL: Ts.Us.Vs.packEquality()
      // CHECK-NEXT: Generic signature: <T..., U..., V... where T == U, ((T, V)...) : Any>
      func packEquality() where each T == each U, (repeat (each U, each V)): Any {
      }
    }
  }
}

// CHECK-LABEL: expandedParameters(_:transform:)
// CHECK-NEXT: Generic signature: <T..., Result... where ((T, Result)...) : Any>
func expandedParameters<T..., Result...>(_ t: repeat each T, transform: repeat (each T) -> each Result) -> (repeat each Result) {
  fatalError()
}
