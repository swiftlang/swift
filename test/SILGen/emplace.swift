// RUN: %target-swift-frontend -enable-experimental-feature BuiltinModule -emit-silgen %s | %FileCheck %s

// REQUIRES: swift_feature_BuiltinModule

import Builtin

@_silgen_name("do_emplace")
func do_emplace(_: Builtin.RawPointer)

struct AO {
    var x: Any
}

struct Loadable {
    var x: AnyObject
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}_in_place_loadable
func emplace_in_place_loadable() -> Loadable {
    // CHECK: [[TMP:%.*]] = alloc_stack $Loadable
    // CHECK-NEXT: builtin "prepareInitialization"([[TMP]])
    // CHECK-NEXT: [[PTR:%.*]] = address_to_pointer {{.*}} [[TMP]]
    // CHECK-NEXT: apply {{.*}}([[PTR]])
    // CHECK-NEXT: load [take] [[TMP]]
    // CHECK-NEXT: dealloc_stack [[TMP]]
    return Builtin.emplace(do_emplace)
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}_in_place_ao
// CHECK: bb0([[OUT:%.*]] : $*AO):
func emplace_in_place_ao() -> AO {
    // CHECK: builtin "prepareInitialization"([[OUT]])
    // CHECK-NEXT: [[PTR:%.*]] = address_to_pointer {{.*}} [[OUT]]
    // CHECK-NEXT: apply [[FN:%.*]]([[PTR]])
    // CHECK-NEXT: destroy_value [[FN]]
    // CHECK-NEXT: tuple ()
    // CHECK-NEXT: return
    return Builtin.emplace(do_emplace)
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}_assign_loadable
// CHECK: bb0([[INOUT:%.*]] : $*Loadable):
func emplace_assign_loadable(_ x: inout Loadable) {
    // CHECK: [[TMP:%.*]] = alloc_stack $Loadable
    // CHECK-NEXT: builtin "prepareInitialization"([[TMP]])
    // CHECK-NEXT: [[PTR:%.*]] = address_to_pointer {{.*}} [[TMP]]
    // CHECK-NEXT: apply {{.*}}([[PTR]])
    // CHECK-NEXT: [[RESULT:%.*]] = load [take] [[TMP]]
    // CHECK-NEXT: [[WRITE:%.*]] = begin_access [modify] [unknown] [[INOUT]]
    // CHECK-NEXT: assign [[RESULT]] to [[WRITE]]
    // CHECK-NEXT: end_access [[WRITE]]
    // CHECK-NEXT: dealloc_stack [[TMP]]
    x = Builtin.emplace(do_emplace)
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}_assign_ao
// CHECK: bb0([[INOUT:%.*]] : $*AO):
func emplace_assign_ao(_ x: inout AO) {
    // CHECK: [[TMP:%.*]] = alloc_stack $AO
    // CHECK-NEXT: builtin "prepareInitialization"([[TMP]])
    // CHECK-NEXT: [[PTR:%.*]] = address_to_pointer {{.*}} [[TMP]]
    // CHECK-NEXT: apply {{.*}}([[PTR]])
    // CHECK-NEXT: [[WRITE:%.*]] = begin_access [modify] [unknown] [[INOUT]]
    // CHECK-NEXT: copy_addr [take] [[TMP]] to [[WRITE]]
    // CHECK-NEXT: end_access [[WRITE]]
    // CHECK-NEXT: dealloc_stack [[TMP]]
    x = Builtin.emplace(do_emplace)
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}_ignore_loadable
func emplace_ignore_loadable() {
    // CHECK: [[TMP:%.*]] = alloc_stack $Loadable
    // CHECK-NEXT: builtin "prepareInitialization"([[TMP]])
    // CHECK-NEXT: [[PTR:%.*]] = address_to_pointer {{.*}} [[TMP]]
    // CHECK-NEXT: apply {{.*}}([[PTR]])
    // CHECK-NEXT: [[RESULT:%.*]] = load [take] [[TMP]]
    // CHECK-NEXT: ignored_use [[RESULT]]
    // CHECK-NEXT: destroy_value [[RESULT]]
    // CHECK-NEXT: dealloc_stack [[TMP]]
    let _: Loadable = Builtin.emplace(do_emplace)
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}_ignore_ao
func emplace_ignore_ao() {
    // CHECK: [[TMP:%.*]] = alloc_stack $AO
    // CHECK-NEXT: builtin "prepareInitialization"([[TMP]])
    // CHECK-NEXT: [[PTR:%.*]] = address_to_pointer {{.*}} [[TMP]]
    // CHECK-NEXT: apply {{.*}}([[PTR]])
    // CHECK-NEXT: ignored_use [[TMP]]
    // CHECK-NEXT: destroy_addr [[TMP]]
    // CHECK-NEXT: dealloc_stack [[TMP]]
    let _: AO = Builtin.emplace(do_emplace)
}
