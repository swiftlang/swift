// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-sil | FileCheck %s
import gizmo

class Hoozit : Gizmo {
  func typical(x:Int, y:Gizmo) -> Gizmo { return y }
  // CHECK: sil @_TToCSo6Hoozit7typicalfS_FT1xSi1yCSo5Gizmo_S0_ : $[cc(objc_method), thin] (Hoozit, (x : Int64, y : Gizmo)) -> Gizmo {
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit, [[X:%.*]] : $Int64, [[Y:%.*]] : $Gizmo):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   retain [[Y]]
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TCSo6Hoozit7typicalfS_FT1xSi1yCSo5Gizmo_S0_ : $[cc(method), thin] ((x : Int64, y : Gizmo), Hoozit) -> Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[X]], [[Y]], [[THIS]])
  // CHECK-NEXT:   autorelease_return [[RES]]
  // CHECK-NEXT: }

  // NS_CONSUMES_SELF by inheritance
  func fork() { }
  // CHECK: sil @_TToCSo6Hoozit4forkfS_FT_T_ : $[cc(objc_method), thin] (Hoozit, ()) -> () {
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TCSo6Hoozit4forkfS_FT_T_ : $[cc(method), thin] ((), Hoozit) -> ()
  // CHECK-NEXT:   apply [[NATIVE]]([[THIS]])
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  // NS_CONSUMED 'gizmo' argument by inheritance
  func consume(gizmo:Gizmo) { }
  // CHECK: sil @_TToCSo6Hoozit7consumefS_FT5gizmoCSo5Gizmo_T_ : $[cc(objc_method), thin] (Hoozit, (gizmo : Gizmo)) -> () {
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit, [[GIZMO:%.*]] : $Gizmo):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NOT:    retain [[GIZMO]]
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TCSo6Hoozit7consumefS_FT5gizmoCSo5Gizmo_T_ : $[cc(method), thin] ((gizmo : Gizmo), Hoozit) -> ()
  // CHECK-NEXT:   apply [[NATIVE]]([[GIZMO]], [[THIS]])
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  // NS_RETURNS_RETAINED by family (-copy)
  func copyFoo() -> Gizmo { return this }
  // CHECK: sil @_TToCSo6Hoozit7copyFoofS_FT_CSo5Gizmo : $[cc(objc_method), thin] (Hoozit, ()) -> Gizmo
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TCSo6Hoozit7copyFoofS_FT_CSo5Gizmo : $[cc(method), thin] ((), Hoozit) -> Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[THIS]])
  // CHECK-NOT:    autorelease_return
  // CHECK-NOT:    release
  // CHECK-NEXT:   return [[RES]]
  // CHECK-NEXT: }

  var typicalProperty : Gizmo
  // -- getter
  // CHECK: sil @_TToCSo6Hoozit15typicalPropertyCSo5Gizmog : $[cc(objc_method), thin] (Hoozit, ()) -> Gizmo {
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   [[ADDR:%.*]] = ref_element_addr [[THIS]], @typicalProperty
  // CHECK-NEXT:   [[RES:%.*]] = load [[ADDR]]
  // CHECK-NEXT:   release [[THIS]]
  // CHECK-NEXT:   retain [[RES]]
  // CHECK-NEXT:   autorelease_return [[RES]]
  // CHECK-NEXT: }

  // -- setter
  // CHECK: sil @_TToCSo6Hoozit15typicalPropertyCSo5Gizmos : $[cc(objc_method), thin] (Hoozit, (value : Gizmo)) -> () {
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit, [[VALUE:%.*]] : $Gizmo):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   retain [[VALUE]]
  // CHECK-NEXT:   [[ADDR:%.*]] = ref_element_addr [[THIS]], @typicalProperty
  // CHECK-NEXT:   [[OLD:%.*]] = load [[ADDR]]
  // CHECK-NEXT:   store [[VALUE]] to [[ADDR]]
  // CHECK-NEXT:   release [[OLD]]
  // CHECK-NEXT:   release [[THIS]]

  // NS_RETURNS_RETAINED getter by family (-copy)
  var copyProperty : Gizmo
  // -- getter
  // CHECK: sil @_TToCSo6Hoozit12copyPropertyCSo5Gizmog : $[cc(objc_method), thin] (Hoozit, ()) -> Gizmo {
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   [[ADDR:%.*]] = ref_element_addr [[THIS]], @copyProperty
  // CHECK-NEXT:   [[RES:%.*]] = load [[ADDR]]
  // CHECK-NEXT:   release [[THIS]]
  // CHECK-NEXT:   retain [[RES]]
  // CHECK-NEXT:   return [[RES]]
  // CHECK-NEXT: }

  // -- setter is normal
  // CHECK: sil @_TToCSo6Hoozit12copyPropertyCSo5Gizmos : $[cc(objc_method), thin] (Hoozit, (value : Gizmo)) -> () {
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit, [[VALUE:%.*]] : $Gizmo):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   retain [[VALUE]]
  // CHECK-NEXT:   [[ADDR:%.*]] = ref_element_addr [[THIS]], @copyProperty
  // CHECK-NEXT:   [[OLD:%.*]] = load [[ADDR]]
  // CHECK-NEXT:   store [[VALUE]] to [[ADDR]]
  // CHECK-NEXT:   release [[OLD]]
  // CHECK-NEXT:   release [[THIS]]

  var roProperty : Gizmo { return this }
  // -- getter
  // CHECK: sil @_TToCSo6Hoozit10roPropertyCSo5Gizmog : $[cc(objc_method), thin] (Hoozit, ()) -> Gizmo {
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TCSo6Hoozit10roPropertyCSo5Gizmog : $[cc(method), thin] ((), Hoozit) -> Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[THIS]])
  // CHECK-NEXT:   autorelease_return [[RES]]
  // CHECK-NEXT: }

  // -- no setter
  // CHECK-NOT: sil @_TToCSo6Hoozit10roPropertyCSo5Gizmos

  var rwProperty : Gizmo {
    get: return this
    set:
  }
  // -- getter
  // CHECK: sil @_TToCSo6Hoozit10rwPropertyCSo5Gizmog : $[cc(objc_method), thin] (Hoozit, ()) -> Gizmo 

  // -- setter
  // CHECK: sil @_TToCSo6Hoozit10rwPropertyCSo5Gizmos : $[cc(objc_method), thin] (Hoozit, (value : Gizmo)) -> () {
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit, [[VALUE:%.*]] : $Gizmo):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   retain [[VALUE]]
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TCSo6Hoozit10rwPropertyCSo5Gizmos : $[cc(method), thin] ((value : Gizmo), Hoozit) -> ()
  // CHECK-NEXT:   apply [[NATIVE]]([[VALUE]], [[THIS]])
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  var copyRWProperty : Gizmo {
    get: return this
    set:
  }
  // -- getter
  // CHECK: sil @_TToCSo6Hoozit14copyRWPropertyCSo5Gizmog : $[cc(objc_method), thin] (Hoozit, ()) -> Gizmo {
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TCSo6Hoozit14copyRWPropertyCSo5Gizmog : $[cc(method), thin] ((), Hoozit) -> Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[THIS]])
  // CHECK-NOT:    release
  // CHECK-NOT:    autorelease_return
  // CHECK-NEXT:   return [[RES]]
  // CHECK-NEXT: }

  // -- setter is normal
  // CHECK: sil @_TToCSo6Hoozit14copyRWPropertyCSo5Gizmos : $[cc(objc_method), thin] (Hoozit, (value : Gizmo)) -> () {
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit, [[VALUE:%.*]] : $Gizmo):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   retain [[VALUE]]
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TCSo6Hoozit14copyRWPropertyCSo5Gizmos : $[cc(method), thin] ((value : Gizmo), Hoozit) -> ()
  // CHECK-NEXT:   apply [[NATIVE]]([[VALUE]], [[THIS]])
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  // Don't export generics to ObjC yet
  func generic<T>(x:T) {}
  // CHECK-NOT: sil @_TToCSo6Hoozit7genericfS_U__FT_T_
}

// Don't export generics to ObjC yet
class Wotsit<T> : Gizmo {
  func plain() { }

  func generic<U>(x:U) {}

  var property : T
}
// CHECK-NOT: sil @_TTo{{.*}}Wotsit{{.*}}

// Extension properties and methods need thunks too.

extension Hoozit {
  func foo() {}
  // CHECK: sil @_TToCSo6Hoozit3foofS_FT_T_ : $[cc(objc_method), thin] (Hoozit, ()) -> () {

  var extensionProperty : Int { return 0 }
  // CHECK: sil @_TCSo6Hoozit17extensionPropertySig : $[cc(method), thin] ((), Hoozit) -> Int64
}

// Calling objc methods of subclass should go through objc entry points
func useHoozit(h:Hoozit) {
// sil @_T11objc_thunks9useHoozitFT1hCSo6Hoozit_T_
  h.fork()
  // CHECK: class_method [volatile] {{%.*}} : {{.*}}, #Hoozit.fork!1.objc

  h.foo()
  // CHECK: class_method [volatile] {{%.*}} : {{.*}}, #Hoozit.foo!1.objc

  // Generic doesn't have an objc entry point
  h.generic(1)
  // CHECK-NOT: class_method [volatile] {{%.*}} : {{.*}}, #foo!1.objc
}

// Wotsit<T> is generic and doesn't have objc entry points for its methods
func useWotsit(w:Wotsit<String>) {
// sil @_T11objc_thunks9useWotsitFT1wGCSo6WotsitSS__T_
  w.plain()
  // CHECK-NOT: class_method [volatile] {{%.*}} : {{.*}}, #Wotsit.plain!1.objc
  w.generic(2)
  // CHECK-NOT: class_method [volatile] {{%.*}} : {{.*}}, #Wotsit.generic!1.objc

  // Inherited methods do have objc entry points
  // FIXME: Crashes type-checker
  // w.clone()
  // C/HECK: class_method [volatile] {{%.*}} : {{.*}}, @clone.1.objc
}
