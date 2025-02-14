// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

// Make sure we do not call the getter to get the oldValue and pass it to didSet
// when the didSet does not reference the oldValue in its body.

class Foo<T> {
  var value: T {
    // CHECK-LABEL: sil private [ossa] @$s35didset_oldvalue_not_accessed_silgen3FooC5valuexvW : $@convention(method) <T> (@guaranteed Foo<T>) -> ()
    // CHECK: debug_value %0 : $Foo<T>, let, name "self", argno {{[0-9]+}}
    // CHECK-NOT: debug_value %0 : $*T, let, name "oldValue", argno {{[0-9]+}}, {{.*}} expr op_deref
    didSet { print("didSet called!") }
  }

  init(value: T) {
    self.value = value
  }
}

let foo = Foo(value: "Hello")
// Foo.value.setter //

// CHECK-LABEL: sil hidden [ossa] @$s35didset_oldvalue_not_accessed_silgen3FooC5valuexvs : $@convention(method) <T> (@in T, @guaranteed Foo<T>) -> ()
// CHECK: debug_value [[VALUE:%.*]] : $*T, let, name "value", argno {{[0-9+]}}, expr op_deref
// CHECK-NEXT: debug_value [[SELF:%.*]] : $Foo<T>, let, name "self", argno {{[0-9+]}}
// CHECK-NEXT: [[ALLOC_STACK:%.*]] = alloc_stack $T
// CHECK-NEXT: copy_addr [[VALUE]] to [init] [[ALLOC_STACK]] : $*T
// CHECK-NEXT: [[REF_ADDR:%.*]] = ref_element_addr [[SELF]] : $Foo<T>, #Foo.value
// CHECK-NEXT: [[BEGIN_ACCESS:%.*]] = begin_access [modify] [dynamic] [[REF_ADDR]] : $*T
// CHECK-NEXT: copy_addr [take] [[ALLOC_STACK]] to [[BEGIN_ACCESS]] : $*T
// CHECK-NEXT: end_access [[BEGIN_ACCESS]] : $*T
// CHECK-NEXT: dealloc_stack [[ALLOC_STACK]] : $*T

// CHECK: [[DIDSET:%.*]] = function_ref @$s35didset_oldvalue_not_accessed_silgen3FooC5valuexvW : $@convention(method) <τ_0_0> (@guaranteed Foo<τ_0_0>) -> ()
// CHECK-NEXT: [[RESULT:%.*]] = apply [[DIDSET]]<T>([[SELF]]) : $@convention(method) <τ_0_0> (@guaranteed Foo<τ_0_0>) -> ()

// Foo.value.modify //

// CHECK-LABEL: sil hidden [ossa] @$s35didset_oldvalue_not_accessed_silgen3FooC5valuexvM : $@yield_once @convention(method) <T> (@guaranteed Foo<T>) -> @yields @inout T
// CHECK: debug_value [[SELF:%.*]] : $Foo<T>, let, name "self", argno {{[0-9+]}}
// CHECK-NEXT: [[REF_ADDR:%.*]] = ref_element_addr [[SELF]] : $Foo<T>, #Foo.value
// CHECK-NEXT: [[BEGIN_ACCESS:%.*]] = begin_access [modify] [dynamic] [[REF_ADDR]] : $*T
// CHECK-NEXT: yield [[BEGIN_ACCESS]] : $*T, resume bb1, unwind bb2

// CHECK: bb1:
// CHECK-NEXT: end_access [[BEGIN_ACCESS]] : $*T
// CHECK-NEXT: // function_ref Foo.value.didset
// CHECK-NEXT: [[DIDSET:%.*]] = function_ref @$s35didset_oldvalue_not_accessed_silgen3FooC5valuexvW : $@convention(method) <τ_0_0> (@guaranteed Foo<τ_0_0>) -> ()
// CHECK-NEXT: [[RESULT:%.*]] = apply [[DIDSET]]<T>([[SELF]]) : $@convention(method) <τ_0_0> (@guaranteed Foo<τ_0_0>) -> ()

// CHECK: bb2:
// CHECK-NEXT: end_access [[BEGIN_ACCESS]] : $*T
// CHECK-NEXT: unwind
foo.value = "World"
