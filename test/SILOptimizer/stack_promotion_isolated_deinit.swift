// RUN: %target-swift-frontend -parse-as-library -target %target-future-triple -O -module-name=test %s -Xllvm -sil-print-types -emit-sil | %FileCheck %s
// REQUIRES: swift_in_compiler

@globalActor actor AnotherActor: GlobalActor {
  static let shared = AnotherActor()
}

final class Inner {}

final class IsolatedDeinit {
    var inner: Inner?
    @AnotherActor deinit {}
}

final class Container {
    var ref: IsolatedDeinit?
}

// CHECK-LABEL: sil [noinline] {{.*}}@$s4test0A16ContainerOutsideyyF : $@convention(thin) () -> () {
// CHECK: [[C:%.*]] = alloc_ref [bare] [stack] $Container
// CHECK: [[ID:%.*]] = alloc_ref $IsolatedDeinit
// CHECK: dealloc_stack_ref [[C]] : $Container
// CHECK: return
@inline(never)
public func testContainerOutside() {
    // container can be promoted
    let container = Container()
    let obj = IsolatedDeinit()
    container.ref = obj
}

// CHECK-LABEL: sil [noinline] @$s4test0A15ContainerInsideyyF : $@convention(thin) () -> () {
// CHECK: [[D:%.*]] = alloc_ref $IsolatedDeinit
// CHECK: [[C:%.*]] = alloc_ref [bare] [stack] $Container
// CHECK: dealloc_stack_ref [[C]] : $Container
// CHECK: return
@inline(never)
public func testContainerInside() {
    let obj = IsolatedDeinit()
    // container can be promoted
    let container = Container()
    container.ref = obj
}

// CHECK-LABEL: sil [noinline] @$s4test0A12InnerOutsideyyF : $@convention(thin) () -> () {
// CHECK: [[I:%.*]] = alloc_ref $Inner
// CHECK: [[D:%.*]] = alloc_ref $IsolatedDeinit
// CHECK: [[DI:%.*]] = end_init_let_ref [[D]] : $IsolatedDeinit
// CHECK: strong_release [[DI]] : $IsolatedDeinit
// CHECK: return
@inline(never)
public func testInnerOutside() {
    // inner cannot be promoted, because it escapes to isolated deinit
    let inner = Inner()
    let obj = IsolatedDeinit()
    obj.inner = inner
}

// CHECK-LABEL: sil [noinline] @$s4test0A11InnerInsideyyF : $@convention(thin) () -> () {
// CHECK: [[D:%.*]] = alloc_ref $IsolatedDeinit
// CHECK: [[DI:%.*]] = end_init_let_ref [[D]] : $IsolatedDeinit
// CHECK: [[I:%.*]] = alloc_ref $Inner
// CHECK: strong_release [[DI]] : $IsolatedDeinit
// CHECK: return
@inline(never)
public func testInnerInside() {
    let obj = IsolatedDeinit()
    // inner cannot be promoted, because it escapes to isolated deinit
    let inner = Inner()
    obj.inner = inner
}
