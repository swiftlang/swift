// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s
protocol P {}

class C: P {}
class D: C {}

struct Butt<T: P> {}

func foo<T: P>(_: (Butt<T>) -> ()) {}

// CHECK-LABEL: sil{{.*}}3bar
func bar(_ f: (Butt<D>) -> ()) {
  // CHECK: convert_function {{.*}} $@noescape @callee_guaranteed (Butt<D>) -> () to $@noescape @callee_guaranteed @substituted <τ_0_0 where τ_0_0 : P> (Butt<τ_0_0>) -> () for <D>
  foo(f)
}
