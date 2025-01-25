// RUN: %target-swift-frontend -emit-irgen %s | %FileCheck %s

// CHECK: [[BOX_1:@[A-Za-z0-9.]+]] = private constant %swift.full_boxmetadata { ptr [[DESTROY_BOX_1:@[A-Za-z0-9.]+]],
// CHECK: [[BOX_2:@[A-Za-z0-9.]+]] = private constant %swift.full_boxmetadata { ptr [[DESTROY_BOX_2:@[A-Za-z0-9.]+]],
// CHECK: [[BOX_3:@[A-Za-z0-9.]+]] = private constant %swift.full_boxmetadata { ptr [[DESTROY_BOX_3:@[A-Za-z0-9.]+]],
// CHECK: [[BOX_4:@[A-Za-z0-9.]+]] = private constant %swift.full_boxmetadata { ptr [[DESTROY_BOX_4:@[A-Za-z0-9.]+]],

// We don't really need to test arm64e, and doing so would mean tweaking the
// test to cope with ptrauth.
// UNSUPPORTED: CPU=arm64e

@_silgen_name("mystery_destroy")
func mystery_destroy() {}

@_silgen_name("mystery_destroy_generic")
func mystery_destroy_generic(_: Any.Type) {}

@_silgen_name("mystery_borrow")
func mystery_borrow<T: ~Copyable>(_: borrowing T) {}

struct FixedWithDeinit: ~Copyable {
    var field = 0
    deinit { mystery_destroy() }
}

// We can't treat the box as a stock empty box because we need to invoke
// the type's deinit.
// CHECK: define{{.*}} @"$s{{.*}}19capture_and_release
func capture_and_release() -> () -> () {
    let ewd = FixedWithDeinit()
    // CHECK: call{{.*}} @swift_allocObject({{.*}} [[BOX_1]],
    return { mystery_borrow(ewd) }
}

// CHECK: define{{.*}} void [[DESTROY_BOX_1]](
// CHECK:   call {{.*}}15FixedWithDeinitVfD

struct FixedWithDeinitGeneric<T>: ~Copyable {
    var field = 0
    deinit { mystery_destroy_generic(T.self) }
}

// The "empty" box needs to capture generic parameters in order
// to invoke the type's deinit.
// CHECK-LABEL: define{{.*}} @"$s{{.*}}27capture_and_release_generic
func capture_and_release_generic<T>(_: T.Type) -> () -> () {
    let ewd = FixedWithDeinitGeneric<T>()
    // CHECK: [[BOX:%.*]] = call{{.*}} @swift_allocObject({{.*}} [[BOX_2]],
    // CHECK: [[BINDINGS:%.*]] = getelementptr{{.*}} ptr [[BOX]], i32 0, i32 2
    // CHECK: store ptr %T, ptr [[BINDINGS]],
    return { mystery_borrow(ewd) }
}

// CHECK: define{{.*}} void [[DESTROY_BOX_2]](
// CHECK:   [[BINDINGS:%.*]] = getelementptr{{.*}} ptr %0, i32 0, i32 2
// CHECK:   [[T:%.*]] = load ptr, ptr [[BINDINGS]],
// CHECK:   call {{.*}}22FixedWithDeinitGenericVfD"({{.*}}, ptr [[T]])

// Ensure that we capture the generic parameters even if the type indirectly
// contains deinit-bearing fields, but has no deinit of its own.

struct FixedWithDeinitGenericIndirect<T>: ~Copyable {
    var value = FixedWithDeinitGeneric<T>()
}

// CHECK-LABEL: define{{.*}} @"$s{{.*}}36capture_and_release_generic_indirect
func capture_and_release_generic_indirect<T>(_: T.Type) -> () -> () {
    let ewd = FixedWithDeinitGenericIndirect<T>()
    // CHECK: [[BOX:%.*]] = call{{.*}} @swift_allocObject({{.*}} [[BOX_4]],
    // CHECK: [[BINDINGS:%.*]] = getelementptr{{.*}} ptr [[BOX]], i32 0, i32 2
    // CHECK: store ptr %T, ptr [[BINDINGS]],
    return { mystery_borrow(ewd) }
}

// CHECK: define{{.*}} void [[DESTROY_BOX_4]](
// CHECK:   [[BINDINGS:%.*]] = getelementptr{{.*}} ptr %0, i32 0, i32 2
// CHECK:   [[T:%.*]] = load ptr, ptr [[BINDINGS]],
// CHECK:   call {{.*}}22FixedWithDeinitGenericVfD"({{.*}}, ptr [[T]])

