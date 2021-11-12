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
// CHECK-SAME: @substituted <τ_0_0, τ_0_1, τ_0_2 where τ_0_0 == S<τ_0_1>, τ_0_1 : P, τ_0_1 == τ_0_2> () -> @out E<S<τ_0_1>> for <S<T>, T, T>
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
// CHECK-SAME: @substituted <τ_0_0, τ_0_1, τ_0_2 where τ_0_0 == S2<τ_0_1>, τ_0_1 : Q, τ_0_1 == τ_0_2> () -> @out E2<S2<τ_0_1>> for <S2<T>, T, T>
func bar2<T: Q>(_: () -> E2<S2<T>>) {}

protocol P1 {
  associatedtype Element
  associatedtype Index
}

struct S3<Base> where Base: P1, Base.Element: P1 {
  let x: Base.Element.Index
}

struct S4<Base> where Base : P1, Base.Element: P1 {
// CHECK-LABEL: {{^}}sil {{.*}} @${{.*}}2S4{{.*}}3foo{{.*}}F :
// CHECK: @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : P1, τ_0_0 == τ_0_1, τ_0_0.Element : P1> (@in_guaranteed S3<τ_0_0>) -> () for <Base, Base>
  func foo(index: S3<Base>?) {
    _ = index.map({ _ = $0 })
  }
}
