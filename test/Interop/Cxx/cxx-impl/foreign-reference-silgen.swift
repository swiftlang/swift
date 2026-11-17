// A `@cxx @implementation` function borrows a foreign reference type
// parameter from its C++ caller (`@guaranteed`), and a method borrows `this`,
// so neither copies it on entry. A `super` call passes the borrowed `self`
// without copying it either.

// RUN: %target-swift-emit-silgen \
// RUN:   -module-name main \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s

// REQUIRES: swift_feature_CxxImplementation

import ForeignReference
import ForeignReferenceFree

// int takesNode(Node *_Nonnull n);
// CHECK-LABEL: sil {{.*}}@$s4main9takesNodeys5Int32VSo0C0VFTo : $@convention(c) (@guaranteed Node) -> Int32 {
// CHECK:       bb0(%0 : @guaranteed $Node):
// CHECK-NOT:     copy_value
// CHECK-LABEL: } // end sil function '$s4main9takesNodeys5Int32VSo0C0VFTo'
@cxx @implementation
public func takesNode(_ n: Node) -> Int32 { return n.get() + n.get() }

// int takesNullableNode(Node *_Nullable n);
// CHECK-LABEL: sil {{.*}}@$s4main17takesNullableNodeys5Int32VSo0D0VSgFTo : $@convention(c) (@guaranteed Optional<Node>) -> Int32 {
// CHECK:       bb0(%0 : @guaranteed $Optional<Node>):
@cxx @implementation
public func takesNullableNode(_ n: Node?) -> Int32 { return n?.value ?? -1 }

// int takesNodeByRef(Node &n);
// CHECK-LABEL: sil {{.*}}@$s4main14takesNodeByRefys5Int32VSo0C0VFTo : $@convention(c) (@guaranteed Node) -> Int32 {
// CHECK:       bb0(%0 : @guaranteed $Node):
// CHECK-NOT:     copy_value
// CHECK-LABEL: } // end sil function '$s4main14takesNodeByRefys5Int32VSo0C0VFTo'
@cxx @implementation
public func takesNodeByRef(_ n: Node) -> Int32 { return n.value }

// int consumesConstNodeRef(const Node &n);
// CHECK-LABEL: sil {{.*}}@$s4main20consumesConstNodeRefys5Int32VSo0D0VFTo : $@convention(c) (@guaranteed Node) -> Int32 {
// CHECK:       bb0(%0 : @noImplicitCopy @guaranteed $Node):
@cxx @implementation
public func consumesConstNodeRef(_ n: borrowing Node) -> Int32 { return n.value }

extension Node {
  // int Node::get() const;
  // CHECK-LABEL: sil {{.*}}@$sSo4NodeV4mainE3gets5Int32VyFTo : $@convention(cxx_method) (@guaranteed Node) -> Int32 {
  // CHECK:       bb0(%0 : @guaranteed $Node):
  // CHECK-NOT:     copy_value
  // CHECK-LABEL: } // end sil function '$sSo4NodeV4mainE3gets5Int32VyFTo'
  @cxx @implementation
  public func get() -> Int32 { return value }
}

extension Derived {
  // void Derived::free() override;
  // CHECK-LABEL: sil {{.*}}@$sSo7DerivedV4mainE4freeyyFTo : $@convention(cxx_method) (@guaranteed Derived) -> () {
  // CHECK:       bb0(%0 : @guaranteed $Derived):
  // CHECK-NOT:     copy_value
  // CHECK:         [[SUPER:%.*]] = upcast %0 to $Object
  // CHECK-NOT:     copy_value
  // CHECK:         [[FREE:%.*]] = function_ref @$sSo6ObjectV17__staticCall_freeyyFTo : $@convention(cxx_method) (@guaranteed Object) -> ()
  // CHECK-NEXT:    apply [[FREE]]([[SUPER]]) : $@convention(cxx_method) (@guaranteed Object) -> ()
  // CHECK-NOT:     destroy_value
  // CHECK-LABEL: } // end sil function '$sSo7DerivedV4mainE4freeyyFTo'
  @cxx @implementation
  public override func free() {
    note("Derived::free")
    super.free()
  }
}
