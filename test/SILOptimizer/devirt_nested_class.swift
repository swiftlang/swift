// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s

fileprivate class Outer<T> {
  class Inner<U> : Base<T, U> {
    @_semantics("optimize.sil.never")
    override func method<V>(v: V) {}
  }
}

fileprivate class Base<T, U> {
  @_semantics("optimize.sil.never")
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
// CHECK: function_ref @_T019devirt_nested_class5Outer{{.*}}LC5InnerC6methodyqd0__1v_tlF : $@convention(method) <τ_0_0><τ_1_0><τ_2_0> (@in τ_2_0, @guaranteed Outer<τ_0_0>.Inner<τ_1_0>) -> ()
// CHECK: return
