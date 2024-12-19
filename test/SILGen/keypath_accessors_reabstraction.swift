// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

// https://github.com/apple/swift/issues/63682
// Test that we adjust to abstraction differences when assigning in generated
// key path accessors.

struct Foo<A> {
  var closure: () -> A?
}

// CHECK-LABEL: sil hidden [ossa] @{{.+}}physicalFunctionValue
func physicalFunctionValue() {
  // CHECK: keypath $WritableKeyPath<Foo<Bool>, () -> Optional<Bool>>, (root $Foo<Bool>; settable_property $() -> Optional<Bool>,  id ##Foo.closure, getter @$[[GETTER:[_a-zA-Z0-9]+]] {{.+}}, setter @$[[SETTER:[_a-zA-Z0-9]+]]
  let _ = \Foo<Bool>.closure
} // CHECK: // end sil function '{{.+}}physicalFunctionValue

// CHECK: sil shared [thunk] [ossa] @$[[GETTER]] : $@convention(keypath_accessor_getter) (@in_guaranteed Foo<Bool>) -> @out @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Optional<Bool>> {
// CHECK: bb0([[OUT_FN:%[0-9]+]] {{.+}}):
// CHECK: [[SRC_REABSTR:%[0-9]+]] = convert_function %{{[0-9]+}} : $@callee_guaranteed () -> @out Optional<Bool> to $@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Optional<Bool>>
// CHECK-NEXT: store [[SRC_REABSTR]] to [init] [[OUT_FN]] : $*@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Optional<Bool>>
// CHECK: } // end sil function '$[[GETTER]]'

// CHECK: sil shared [thunk] [ossa] @$[[SETTER]] : $@convention(keypath_accessor_setter) (@in_guaranteed @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Optional<Bool>>, @inout Foo<Bool>) -> () {
// CHECK: bb0({{.+}}, [[FOO:%[0-9]+]] : $*Foo<Bool>):
// CHECK: [[SRC_REABSTR:%[0-9]+]] = convert_function %{{[0-9]+}} : $@callee_guaranteed () -> @out Optional<Bool> to $@callee_guaranteed @substituted <τ_0_0> () -> @out Optional<τ_0_0> for <Bool>
// CHECK-NEXT: [[DEST:%[0-9]+]] = struct_element_addr [[FOO]] : $*Foo<Bool>, #Foo.closure
// CHECK-NEXT: assign [[SRC_REABSTR]] to [[DEST]] : $*@callee_guaranteed @substituted <τ_0_0> () -> @out Optional<τ_0_0> for <Bool>
// CHECK: } // end sil function '$[[SETTER]]'
