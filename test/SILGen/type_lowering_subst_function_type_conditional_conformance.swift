// RUN: %target-swift-emit-silgen %s -requirement-machine=verify | %FileCheck %s

enum E<T : P> {
  case a(T.X)
}

struct S<T> {}

protocol P {
  associatedtype X
}

extension S : P where T : P {
  typealias X = T.X
}

func foo<T : P>(_ x: E<S<T>>) {
// Ensure that the lowered substituted SIL function type for `() -> E<S<T>>`
// preserves the T: P constraint necessary to allow for `S<T>` to substitute
// into `E<T: P>`
  bar({ return x })
}

// CHECK-LABEL: {{^}}sil {{.*}} @${{.*}}3bar
// CHECK-SAME: @substituted <τ_0_0 where τ_0_0 : P> () -> @out E<S<τ_0_0>> for <T>
func bar<T: P>(_: () -> E<S<T>>) {}

// ---

protocol Q: P {
  associatedtype Y
}

enum E2<T: Q> {
  case a(T.Y)
}

struct S2<T: P>: P {
  typealias X = T.X
}

extension S2: Q where T: Q {
  typealias Y = T.Y
}

func foo2<T : Q>(_ x: E2<S2<T>>) {
  bar2({ return x })
}

// CHECK-LABEL: {{^}}sil {{.*}} @${{.*}}4bar2
// CHECK-SAME: @substituted <τ_0_0 where τ_0_0 : Q> () -> @out E2<S2<τ_0_0>> for <T>
func bar2<T: Q>(_: () -> E2<S2<T>>) {}

