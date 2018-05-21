
// RUN: %target-swift-frontend -module-name devirt_nested_class -emit-sil -O %s | %FileCheck %s

fileprivate class Outer<T> {
  class Inner<U> : Base<T, U> {
    @_optimize(none)
    override func method<V>(v: V) {}
  }
}

fileprivate class Base<T, U> {
  @_optimize(none)
  func method<V>(v: V) {}
}

fileprivate class Derived<T, U> : Outer<T>.Inner<U> {}

@_transparent
fileprivate func bar<T, U, V>(b: Base<T, U>, v: V) {
  b.method(v: v)
}

fileprivate func foo<T, U, V>(d: Outer<T>.Inner<U>, v: V) {
  bar(b: d, v: v)
}

foo(d: Outer<Int>.Inner<Int>(), v: 0)

// CHECK-LABEL: sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32
// CHECK: function_ref @$S19devirt_nested_class5Outer{{.*}}LC5InnerC6method1vyqd0___tlF : $@convention(method) <τ_0_0><τ_1_0><τ_2_0> (@in_guaranteed τ_2_0, @guaranteed Outer<τ_0_0>.Inner<τ_1_0>) -> ()
// CHECK: return
