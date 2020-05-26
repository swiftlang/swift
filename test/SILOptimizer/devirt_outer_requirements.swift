
// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s

public protocol P {}

public class C1<U> {
  // CHECK-LABEL: sil @$s25devirt_outer_requirements2C1C3fooyyqd__AA1PRzlF : $@convention(method) <U where U : P><V> (@in_guaranteed V, @guaranteed C1<U>) -> () {
  // CHECK: function_ref @$s25devirt_outer_requirements2C1C3baryyqd__AA1PRzlF : $@convention(method) <τ_0_0 where τ_0_0 : P><τ_1_0> (@in_guaranteed τ_1_0, @guaranteed C1<τ_0_0>) -> ()
  // CHECK: return
  public func foo<V>(_ z: V) where U : P {
    bar(z)
  }

  @_optimize(none)
  public func bar<V>(_ z: V) where U : P {}
}

public class Base<T> {
  public func foo<V>(_: V) where T : P {}
}

public protocol Q {}

public struct Foo<T> {}
extension Foo : P where T : Q {}

public class Derived<T> : Base<Foo<T>> {
  public override func foo<V>(_: V) where T : Q {}
}

@_transparent
public func takesBase<T, V>(_ b: Base<T>, _ v: V) where T : P {
  b.foo(v)
}

// CHECK-LABEL: sil @$s25devirt_outer_requirements12takesDerivedyyAA0E0CyxG_q_tAA1QRzr0_lF : $@convention(thin) <T, V where T : Q> (@guaranteed Derived<T>, @in_guaranteed V) -> () {
public func takesDerived<T, V>(_ d: Derived<T>, _ v: V) where T : Q {
  // CHECK: function_ref @$s25devirt_outer_requirements12takesDerivedyyAA0E0CyxG_q_tAA1QRzr0_lF : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Q> (@guaranteed Derived<τ_0_0>, @in_guaranteed τ_0_1) -> ()
  takesDerived(d, v)

  // CHECK: return
}
