// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

public struct Foo : Differentiable {
  public var a: Float
}

// CHECK-LABEL: public struct Foo : Differentiable {
// CHECK:   @sil_stored @differentiable()
// CHECK:   public var a: Float { get set }

// CHECK-LABEL: // Foo.a.getter
// CHECK: sil [transparent] [serialized] [differentiable source 0 wrt 0] @$s33derived_differentiable_properties3FooV1aSfvg : $@convention(method) (Foo) -> Float


