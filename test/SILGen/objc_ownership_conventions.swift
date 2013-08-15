// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-silgen | FileCheck %s
import gizmo

// CHECK: sil @_T26objc_ownership_conventions5test3FT_CSo8NSObject
func test3() -> NSObject {
  // alloc is implicitly ns_returns_retained
  // init is implicitly ns_consumes_self and ns_returns_retained
  return Gizmo.alloc().init()
  // CHECK:      class_method [volatile] {{.*}} : {{.*}}, #NSObject.alloc!1.objc
  // CHECK-NEXT: apply
  // CHECK-NEXT: class_method [volatile] {{.*}} : {{.*}}, #NSObject.init!1.objc
  // CHECK-NEXT: apply
  // CHECK-NEXT: return
}

// CHECK: sil @_T26objc_ownership_conventions5test4FT_T_
func test4() {
  var t = Gizmo
  // alloc is implicitly ns_returns_retained
  // init is implicitly ns_consumes_self and ns_returns_retained
  var obj = t.alloc().init()
  // CHECK:      class_method [volatile] {{.*}} : {{.*}}, #NSObject.alloc!1.objc
  // CHECK-NEXT: [[ALLOC_RESULT:%.*]] = apply
  // CHECK-NEXT: class_method [volatile] {{.*}} : {{.*}}, #NSObject.init!1.objc
  // CHECK-NEXT: [[INIT_RESULT:%.*]] = apply
  // CHECK-NOT:  release [[ALLOC_RESULT]]
  // CHECK-NOT:  release [[INIT_RESULT]]
  // CHECK:      return
}

// Normal message send with argument, no transfers.
// CHECK: sil @_T26objc_ownership_conventions5test5FT1gCSo5Gizmo_T_
func test5(g : Gizmo) {
  Gizmo.inspect(g)
  // CHECK:      [[CLASS:%.*]] = metatype $Gizmo.metatype
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [volatile] [[CLASS]] : {{.*}}, #Gizmo.inspect!1.objc
  // CHECK-NEXT: [[G:%.*]] = load
  // CHECK-NEXT: retain [[G]]
  // CHECK-NEXT: apply [[METHOD]]([[CLASS]], [[G]])
  // CHECK-NEXT: release [[G]]
}
// The argument to consume is __attribute__((ns_consumed)).
// CHECK: sil @_T26objc_ownership_conventions5test6FT1gCSo5Gizmo_T_
func test6(g : Gizmo) {
  Gizmo.consume(g)
  // CHECK:      [[CLASS:%.*]] = metatype $Gizmo.metatype
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [volatile] [[CLASS]] : {{.*}}, #Gizmo.consume!1.objc
  // CHECK-NEXT: [[G:%.*]] = load
  // CHECK-NEXT: retain [[G]]
  // CHECK-NEXT: apply [[METHOD]]([[CLASS]], [[G]])
  // CHECK-NOT:  release [[G]]
}
// fork is __attribute__((ns_consumes_self)).
// CHECK: sil @_T26objc_ownership_conventions5test7FT1gCSo5Gizmo_T_
func test7(g : Gizmo) {
  g.fork()
  // CHECK:      [[G:%.*]] = load
  // CHECK-NEXT: retain [[G]]
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [volatile] [[G]] : {{.*}}, #Gizmo.fork!1.objc
  // CHECK-NEXT: apply [[METHOD]]([[G]])
  // CHECK-NOT:  release [[G]]
}
// clone is __attribute__((ns_returns_retained)).
// CHECK: sil @_T26objc_ownership_conventions5test8FT1gCSo5Gizmo_S0_
func test8(g : Gizmo) -> Gizmo {
  return g.clone()
  // CHECK:      [[G:%.*]] = load
  // CHECK-NEXT: retain [[G]]
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [volatile] [[G]] : {{.*}}, #Gizmo.clone!1.objc
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[G]])
  // CHECK-NEXT: release [[G]]
  // CHECK-NOT:  retain{{.*}} [[RESULT]]
  // CHECK-NOT:  release [[RESULT]]
  // CHECK:      return [[RESULT]]
}
// duplicate returns an autoreleased object at +0.
// CHECK: sil @_T26objc_ownership_conventions5test9FT1gCSo5Gizmo_S0_
func test9(g : Gizmo) -> Gizmo {
  return g.duplicate()
  // CHECK:      [[G:%.*]] = load
  // CHECK-NEXT: retain [[G]]
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [volatile] [[G]] : {{.*}}, #Gizmo.duplicate!1.objc
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[G]])
  // CHECK-NEXT: retain_autoreleased [[RESULT]]
  // CHECK-NEXT: release [[G]]
  // CHECK-NOT:  release [[RESULT]]
  // CHECK:      return [[RESULT]]
}
