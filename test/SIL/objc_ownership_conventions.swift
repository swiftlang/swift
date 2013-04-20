// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-sil | FileCheck %s
import gizmo

// CHECK: sil @test3
func test3() -> NSObject {
  // alloc is implicitly ns_returns_retained
  // init is implicitly ns_consumes_self and ns_returns_retained
  return Gizmo.alloc().init()
  // CHECK:      class_method {{.*}}, @alloc.1
  // CHECK-NEXT: apply
  // CHECK-NEXT: class_method {{.*}}, @init.1
  // CHECK-NEXT: apply
  // CHECK-NEXT: return
}

// CHECK: sil @test4
func test4() {
  var t = Gizmo
  // alloc is implicitly ns_returns_retained
  // init is implicitly ns_consumes_self and ns_returns_retained
  var obj = t.alloc().init()
  // CHECK:      class_method {{.*}}, @alloc.1
  // CHECK-NEXT: [[ALLOC_RESULT:%.*]] = apply
  // CHECK-NEXT: class_method {{.*}}, @init.1
  // CHECK-NEXT: [[INIT_RESULT:%.*]] = apply
  // CHECK-NOT:  release [[ALLOC_RESULT]]
  // CHECK-NOT:  release [[INIT_RESULT]]
  // CHECK:      return
}

// Normal message send with argument, no transfers.
// CHECK: sil @test5
func test5(g : Gizmo) {
  Gizmo.inspect(g)
  // CHECK:      [[CLASS:%.*]] = metatype $Gizmo.metatype
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [[CLASS]], @inspect.1
  // CHECK-NEXT: [[G:%.*]] = load
  // CHECK-NEXT: retain [[G]]
  // CHECK-NEXT: apply [[METHOD]]([[CLASS]], [[G]])
  // CHECK-NEXT: release [[G]]
}
// The argument to consume is __attribute__((ns_consumed)).
// CHECK: sil @test6
func test6(g : Gizmo) {
  Gizmo.consume(g)
  // CHECK:      [[CLASS:%.*]] = metatype $Gizmo.metatype
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [[CLASS]], @consume.1
  // CHECK-NEXT: [[G:%.*]] = load
  // CHECK-NEXT: retain [[G]]
  // CHECK-NEXT: apply [[METHOD]]([[CLASS]], [[G]])
  // CHECK-NOT:  release [[G]]
}
// fork is __attribute__((ns_consumes_self)).
// CHECK: sil @test7
func test7(g : Gizmo) {
  g.fork()
  // CHECK:      [[G:%.*]] = load
  // CHECK-NEXT: retain [[G]]
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [[G]], @fork.1
  // CHECK-NEXT: apply [[METHOD]]([[G]])
  // CHECK-NOT:  release [[G]]
}
// clone is __attribute__((ns_returns_retained)).
// CHECK: sil @test8
func test8(g : Gizmo) -> Gizmo {
  return g.clone()
  // CHECK:      [[G:%.*]] = load
  // CHECK-NEXT: retain [[G]]
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [[G]], @clone.1
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[G]])
  // CHECK-NEXT: release [[G]]
  // CHECK-NOT:  retain{{.*}} [[RESULT]]
  // CHECK-NOT:  release [[RESULT]]
  // CHECK:      return ([[RESULT]])
}
// duplicate returns an autoreleased object at +0.
// CHECK: sil @test9
func test9(g : Gizmo) -> Gizmo {
  return g.duplicate()
  // CHECK:      [[G:%.*]] = load
  // CHECK-NEXT: retain [[G]]
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [[G]], @duplicate.1
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[G]])
  // CHECK-NEXT: retain_autoreleased [[RESULT]]
  // CHECK-NEXT: release [[G]]
  // CHECK-NOT:  release [[RESULT]]
  // CHECK:      return ([[RESULT]])
}
