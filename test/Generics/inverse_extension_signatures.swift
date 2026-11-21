// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature LifetimeDependence  \
// RUN:   -verify -typecheck %s -debug-generic-signatures \
// RUN:   -debug-inverse-requirements 2>&1 | %FileCheck %s --implicit-check-not "error:"

// REQUIRES: swift_feature_LifetimeDependence

// CHECK-LABEL: .Outer@
// CHECK: Generic signature: <A where A : Deinitable, A : Escapable, A : Transferable>

// CHECK-LABEL: .Outer.innerFn@
// CHECK: Generic signature: <A, B where A : Deinitable, A : Escapable, A : Transferable, B : Deinitable, B : Escapable, B : Transferable>

// CHECK-LABEL: .Outer.InnerStruct@
// CHECK: Generic signature: <A, C where A : Deinitable, A : Escapable, A : Transferable, C : Deinitable, C : Escapable, C : Transferable>

// CHECK-LABEL: .Outer.InnerStruct.g@
// CHECK: Generic signature: <A, C, D where A : Deinitable, A : Escapable, A : Transferable, C : Deinitable, C : Escapable, C : Transferable, D : Deinitable, D : Escapable, D : Transferable>

// CHECK-LABEL: .Outer.InnerStruct.init()@
// CHECK: Generic signature: <A, C where A : Deinitable, A : Escapable, A : Transferable, C : Deinitable, C : Escapable, C : Transferable>

// CHECK: (builtin_conformance type="Outer<A>.InnerStruct<C>" protocol="Escapable"{{.*}})

// CHECK-LABEL: .Outer.InnerVariation1@
// CHECK: Generic signature: <A, D where A : Deinitable, A : Escapable, A : Transferable, D : Deinitable, D : Escapable, D : Transferable>

// CHECK-LABEL: .Outer.InnerVariation2@
// CHECK: Generic signature: <A, D where A : Deinitable, A : Escapable, A : Transferable, D : Copyable, D : Transferable>

// CHECK-LABEL: ExtensionDecl {{.*}} base=Outer.InnerStruct
// CHECK: Generic signature: <A, C where A : Copyable, A : Escapable, A : Transferable, C : Copyable, C : Escapable, C : Transferable>

// CHECK-LABEL: .InnerStruct extension.hello@
// CHECK: Generic signature: <A, C, T where A : Copyable, A : Escapable, A : Transferable, C : Copyable, C : Escapable, C : Transferable, T : Copyable, T : Transferable>

// CHECK-LABEL: .Freestanding@
// CHECK: Generic signature: <T where T : Deinitable, T : Transferable>

// CHECK-LABEL: ExtensionDecl {{.*}} base=Outer
// CHECK: Generic signature: <A where A : Copyable, A : Escapable, A : Transferable>

// CHECK-LABEL: ExtensionDecl {{.*}} base=Outer.InnerVariation1
// CHECK: Generic signature: <A, D where A : Deinitable, A : Escapable, A : Transferable, D : Copyable, D : Escapable, D : Transferable>

// CHECK-LABEL: ExtensionDecl {{.*}} base=Outer.InnerVariation2
// CHECK: Generic signature: <A, D where A : Deinitable, A : Escapable, A : Transferable, D : Copyable, D : Transferable>
  
public struct Outer<A: ~Copyable> {
  public func innerFn<B: ~Copyable>(_ b: borrowing B) {}
  public struct InnerStruct<C: ~Copyable> {
    public func g<D>(_ d: borrowing D) where D: ~Copyable {}
  }
  public struct InnerVariation1<D: ~Copyable>: ~Escapable {}
  public struct InnerVariation2<D: ~Escapable>: ~Copyable {}
}

extension Outer.InnerStruct {
    public func hello<T: ~Escapable>(_ t: T) {}
}

public struct Freestanding<T: ~Copyable> where T: ~Escapable {}

extension Outer {}
extension Outer.InnerVariation1 where A: ~Copyable {}
extension Outer.InnerVariation2 where D: ~Escapable, A: ~Copyable {}
