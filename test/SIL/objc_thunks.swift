// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-sil | FileCheck %s
import gizmo

class Hoozit : Gizmo {
  func typical(x:Int, y:Gizmo) -> Gizmo { return y }
  // CHECK: sil @_TToCSo6Hoozit7typicalfS_FT1xSi1yCSo5Gizmo_S0_ : $[sil_cc=c, thin] (Hoozit)(Int64, Gizmo) -> Gizmo {
  // CHECK-NEXT: bb0([[THIS:%.*]] : Hoozit, [[X:%.*]] : Int64, [[Y:%.*]] : Gizmo):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   retain [[Y]]
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref $[sil_cc=method, thin] (Hoozit)(Int64, Gizmo) -> Gizmo, @_TCSo6Hoozit7typicalfS_FT1xSi1yCSo5Gizmo_S0_
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[THIS]], [[X]], [[Y]])
  // CHECK-NEXT:   autorelease_return ([[RES]])
  // CHECK-NEXT: }

  // NS_CONSUMES_SELF by inheritance
  func fork() { }
  // CHECK: sil @_TToCSo6Hoozit4forkfS_FT_T_ : $[sil_cc=c, thin] (Hoozit)() -> () {
  // CHECK-NEXT: bb0([[THIS:%.*]] : Hoozit):
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref $[sil_cc=method, thin] (Hoozit)() -> (), @_TCSo6Hoozit4forkfS_FT_T_
  // CHECK-NEXT:   apply [[NATIVE]]([[THIS]])
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  // NS_CONSUMED 'gizmo' argument by inheritance
  func consume(gizmo:Gizmo) { }
  // CHECK: sil @_TToCSo6Hoozit7consumefS_FT5gizmoCSo5Gizmo_T_ : $[sil_cc=c, thin] (Hoozit)(Gizmo) -> () {
  // CHECK-NEXT: bb0([[THIS:%.*]] : Hoozit, [[GIZMO:%.*]] : Gizmo):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NOT:    retain [[GIZMO]]
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref $[sil_cc=method, thin] (Hoozit)(Gizmo) -> (), @_TCSo6Hoozit7consumefS_FT5gizmoCSo5Gizmo_T_
  // CHECK-NEXT:   apply [[NATIVE]]([[THIS]], [[GIZMO]])
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  // NS_RETURNS_RETAINED by family (-copy)
  func copyFoo() -> Gizmo { return this }
  // CHECK: sil @_TToCSo6Hoozit7copyFoofS_FT_CSo5Gizmo : $[sil_cc=c, thin] (Hoozit)() -> Gizmo {
  // CHECK-NEXT: bb0([[THIS:%.*]] : Hoozit):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref $[sil_cc=method, thin] (Hoozit)() -> Gizmo, @_TCSo6Hoozit7copyFoofS_FT_CSo5Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[THIS]])
  // CHECK-NOT:    autorelease_return
  // CHECK-NOT:    release
  // CHECK-NEXT:   return ([[RES]])
  // CHECK-NEXT: }

  var typicalProperty : Gizmo
  // -- getter
  // CHECK: sil @_TToCSo6Hoozit15typicalPropertyCSo5Gizmog : $[sil_cc=c, thin] (Hoozit)() -> Gizmo {
  // CHECK-NEXT: bb0([[THIS:%.*]] : Hoozit):
  // CHECK-NEXT:   [[ADDR:%.*]] = ref_element_addr [[THIS]], @typicalProperty
  // CHECK-NEXT:   [[RES:%.*]] = load [[ADDR]]
  // CHECK-NOT:    retain
  // CHECK-NEXT:   return ([[RES]])
  // CHECK-NEXT: }

  // -- setter
  // CHECK: sil @_TToCSo6Hoozit15typicalPropertyCSo5Gizmos : $[sil_cc=c, thin] (Hoozit)(Gizmo) -> () {
  // CHECK-NEXT: bb0([[THIS:%.*]] : Hoozit, [[VALUE:%.*]] : Gizmo):
  // CHECK-NEXT:   [[ADDR:%.*]] = ref_element_addr [[THIS]], @typicalProperty
  // CHECK-NEXT:   [[OLD:%.*]] = load [[ADDR]]
  // CHECK-NEXT:   retain [[VALUE]]
  // CHECK-NEXT:   store [[VALUE]] to [[ADDR]]
  // CHECK-NEXT:   release [[OLD]]

  // NS_RETURNS_RETAINED getter by family (-copy)
  var copyProperty : Gizmo
  // -- getter
  // CHECK: sil @_TToCSo6Hoozit12copyPropertyCSo5Gizmog : $[sil_cc=c, thin] (Hoozit)() -> Gizmo {
  // CHECK-NEXT: bb0([[THIS:%.*]] : Hoozit):
  // CHECK-NEXT:   [[ADDR:%.*]] = ref_element_addr [[THIS]], @copyProperty
  // CHECK-NEXT:   [[RES:%.*]] = load [[ADDR]]
  // CHECK-NEXT:   retain [[RES]]
  // CHECK-NEXT:   return ([[RES]])
  // CHECK-NEXT: }

  // -- setter is normal
  // CHECK: sil @_TToCSo6Hoozit12copyPropertyCSo5Gizmos : $[sil_cc=c, thin] (Hoozit)(Gizmo) -> () {
  // CHECK-NEXT: bb0([[THIS:%.*]] : Hoozit, [[VALUE:%.*]] : Gizmo):
  // CHECK-NEXT:   [[ADDR:%.*]] = ref_element_addr [[THIS]], @copyProperty
  // CHECK-NEXT:   [[OLD:%.*]] = load [[ADDR]]
  // CHECK-NEXT:   retain [[VALUE]]
  // CHECK-NEXT:   store [[VALUE]] to [[ADDR]]
  // CHECK-NEXT:   release [[OLD]]

  var roProperty : Gizmo { return this }
  // -- getter
  // CHECK: sil @_TToCSo6Hoozit10roPropertyCSo5Gizmog : $[sil_cc=c, thin] (Hoozit)() -> Gizmo {
  // CHECK-NEXT: bb0([[THIS:%.*]] : Hoozit):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref $[sil_cc=method, thin] (Hoozit)() -> Gizmo, @_TCSo6Hoozit10roPropertyCSo5Gizmog
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[THIS]])
  // CHECK-NEXT:   autorelease_return ([[RES]])
  // CHECK-NEXT: }

  // -- no setter
  // CHECK-NOT: sil @_TToCSo6Hoozit10roPropertyCSo5Gizmos

  var rwProperty : Gizmo {
    get: return this
    set:
  }
  // -- getter
  // CHECK: sil @_TToCSo6Hoozit10rwPropertyCSo5Gizmog : $[sil_cc=c, thin] (Hoozit)() -> Gizmo {

  // -- setter
  // CHECK: sil @_TToCSo6Hoozit10rwPropertyCSo5Gizmos : $[sil_cc=c, thin] (Hoozit)(Gizmo) -> () {
  // CHECK-NEXT: bb0([[THIS:%.*]] : Hoozit, [[VALUE:%.*]] : Gizmo):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   retain [[VALUE]]
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref $[sil_cc=method, thin] (Hoozit)(Gizmo) -> (), @_TCSo6Hoozit10rwPropertyCSo5Gizmos
  // CHECK-NEXT:   apply [[NATIVE]]([[THIS]], [[VALUE]])
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  var copyRWProperty : Gizmo {
    get: return this
    set:
  }
  // -- getter
  // CHECK: sil @_TToCSo6Hoozit14copyRWPropertyCSo5Gizmog : $[sil_cc=c, thin] (Hoozit)() -> Gizmo {
  // CHECK-NEXT: bb0([[THIS:%.*]] : Hoozit):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref $[sil_cc=method, thin] (Hoozit)() -> Gizmo, @_TCSo6Hoozit14copyRWPropertyCSo5Gizmog
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[THIS]])
  // CHECK-NOT:    release
  // CHECK-NOT:    autorelease_return
  // CHECK-NEXT:   return ([[RES]])
  // CHECK-NEXT: }

  // -- setter is normal
  // CHECK: sil @_TToCSo6Hoozit14copyRWPropertyCSo5Gizmos : $[sil_cc=c, thin] (Hoozit)(Gizmo) -> () {
  // CHECK-NEXT: bb0([[THIS:%.*]] : Hoozit, [[VALUE:%.*]] : Gizmo):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   retain [[VALUE]]
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref $[sil_cc=method, thin] (Hoozit)(Gizmo) -> (), @_TCSo6Hoozit14copyRWPropertyCSo5Gizmos
  // CHECK-NEXT:   apply [[NATIVE]]([[THIS]], [[VALUE]])
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  // Don't export generics to ObjC yet
  func generic<T>() {}
  // CHECK-NOT: sil @_TToCSo6Hoozit7genericfS_U__FT_T_
}

// Don't export generics to ObjC yet
class Wotsit<T> : Gizmo {
  func plain() { }

  func generic<T>() {}

  var property : T
}
// CHECK-NOT: sil @_TTo{{.*}}Wotsit{{.*}}
