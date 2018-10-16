// RUN: %target-swift-frontend -O -Xllvm -sil-inline-generics=false -Xllvm -sil-disable-pass=GlobalOpt %s -emit-sil -sil-verify-all | %FileCheck %s


public protocol Foo {
  func foo_method()
}
public protocol Bar {
  func bar_method()
}

@inline(never)
func bar_marker() {}

public struct Inner: Bar {
  public func bar_method() {
    bar_marker()
  }
}

public struct Outer<T> {
  var x: T
}

@inline(never)
func foo_marker() {}
extension Outer: Foo where T: Bar {
  public func foo_method() {
    foo_marker()
    x.bar_method()
  }
}

func callFoo<T: Foo>(_ x: T) {
  x.foo_method()
}

func genericLayer<T: Bar>(_ x: T) {
  callFoo(Outer(x: x))
}

// See that we devirtualize/inline enough to get down to the @inline(never)
// function calls.

// CHECK-LABEL: sil @$s30devirt_conditional_conformance12throughLayeryyF : $@convention(thin) () -> ()
// CHECK: function_ref @$s30devirt_conditional_conformance10foo_markeryyF
// CHECK: function_ref @$s30devirt_conditional_conformance10bar_markeryyF
public func throughLayer() {
  genericLayer(Inner())
}

// CHECK-LABEL: sil @$s30devirt_conditional_conformance6directyyF : $@convention(thin) () -> ()
// CHECK: function_ref @$s30devirt_conditional_conformance10foo_markeryyF
// CHECK: function_ref @$s30devirt_conditional_conformance10bar_markeryyF
public func direct() {
  callFoo(Outer(x: Inner()))
}
