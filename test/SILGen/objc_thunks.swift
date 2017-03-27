// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen -emit-verbose-sil | %FileCheck %s

// REQUIRES: objc_interop

import gizmo
import ansible

class Hoozit : Gizmo {
  @objc func typical(_ x: Int, y: Gizmo) -> Gizmo { return y }
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC7typicalSo5GizmoCSi_AF1ytFTo : $@convention(objc_method) (Int, Gizmo, Hoozit) -> @autoreleased Gizmo {
  // CHECK: bb0([[X:%.*]] : $Int, [[Y:%.*]] : $Gizmo, [[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[Y_COPY:%.*]] = copy_value [[Y]]
  // CHECK-NEXT:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK-NEXT:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK-NEXT:   // function_ref objc_thunks.Hoozit.typical
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_T011objc_thunks6HoozitC7typicalSo5GizmoCSi_AF1ytF : $@convention(method) (Int, @owned Gizmo, @guaranteed Hoozit) -> @owned Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[X]], [[Y_COPY]], [[BORROWED_THIS_COPY]]) {{.*}} line:[[@LINE-8]]:14:auto_gen
  // CHECK-NEXT:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK-NEXT:   destroy_value [[THIS_COPY]] : $Hoozit
  // CHECK-NEXT:   return [[RES]] : $Gizmo{{.*}} line:[[@LINE-11]]:14:auto_gen
  // CHECK-NEXT: } // end sil function '_T011objc_thunks6HoozitC7typicalSo5GizmoCSi_AF1ytFTo'

  // NS_CONSUMES_SELF by inheritance
  override func fork() { }
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC4forkyyFTo : $@convention(objc_method) (@owned Hoozit) -> () {
  // CHECK: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[BORROWED_THIS:%.*]] = begin_borrow [[THIS]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_T011objc_thunks6HoozitC4forkyyF : $@convention(method) (@guaranteed Hoozit) -> ()
  // CHECK-NEXT:   apply [[NATIVE]]([[BORROWED_THIS]])
  // CHECK-NEXT:   end_borrow [[BORROWED_THIS]] from [[THIS]]
  // CHECK-NEXT:   destroy_value [[THIS]]
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  // NS_CONSUMED 'gizmo' argument by inheritance
  override class func consume(_ gizmo: Gizmo?) { }
   // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC7consumeySo5GizmoCSgFZTo : $@convention(objc_method) (@owned Optional<Gizmo>, @objc_metatype Hoozit.Type) -> () {
  // CHECK: bb0([[GIZMO:%.*]] : $Optional<Gizmo>, [[THIS:%.*]] : $@objc_metatype Hoozit.Type):
  // CHECK-NEXT: [[THICK_THIS:%[0-9]+]] = objc_to_thick_metatype [[THIS]] : $@objc_metatype Hoozit.Type to $@thick Hoozit.Type
  // CHECK:   [[NATIVE:%.*]] = function_ref @_T011objc_thunks6HoozitC7consumeySo5GizmoCSgFZ : $@convention(method) (@owned Optional<Gizmo>, @thick Hoozit.Type) -> ()
  // CHECK-NEXT:   apply [[NATIVE]]([[GIZMO]], [[THICK_THIS]])
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  // NS_RETURNS_RETAINED by family (-copy)
  @objc func copyFoo() -> Gizmo { return self }
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC7copyFooSo5GizmoCyFTo : $@convention(objc_method) (Hoozit) -> @owned Gizmo
  // CHECK: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK-NEXT:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_T011objc_thunks6HoozitC7copyFooSo5GizmoCyF : $@convention(method) (@guaranteed Hoozit) -> @owned Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[BORROWED_THIS_COPY]])
  // CHECK-NEXT:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK-NEXT:   destroy_value [[THIS_COPY]]
  // CHECK-NEXT:   return [[RES]]
  // CHECK-NEXT: }

  // Override the normal family conventions to make this non-consuming and
  // returning at +0.
  @objc func initFoo() -> Gizmo { return self }
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC7initFooSo5GizmoCyFTo : $@convention(objc_method) (Hoozit) -> @autoreleased Gizmo
  // CHECK: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK-NEXT:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_T011objc_thunks6HoozitC7initFooSo5GizmoCyF : $@convention(method) (@guaranteed Hoozit) -> @owned Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[BORROWED_THIS_COPY]])
  // CHECK-NEXT:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK-NEXT:   destroy_value [[THIS_COPY]]
  // CHECK-NEXT:   return [[RES]]
  // CHECK-NEXT: }

  @objc var typicalProperty: Gizmo
  // -- getter
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC15typicalPropertySo5GizmoCfgTo : $@convention(objc_method) (Hoozit) -> @autoreleased Gizmo {
  // CHECK: bb0([[SELF:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK-NEXT:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK-NEXT:   // function_ref objc_thunks.Hoozit.typicalProperty.getter
  // CHECK-NEXT:   [[GETIMPL:%.*]] = function_ref @_T011objc_thunks6HoozitC15typicalPropertySo5GizmoCfg
  // CHECK-NEXT:   [[RES:%.*]] = apply [[GETIMPL]]([[BORROWED_SELF_COPY]])
  // CHECK-NEXT:   end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK-NEXT:   destroy_value [[SELF_COPY]]
  // CHECK-NEXT:   return [[RES]] : $Gizmo
  // CHECK-NEXT: }
  
  // CHECK-LABEL: sil hidden @_T011objc_thunks6HoozitC15typicalPropertySo5GizmoCfg : $@convention(method) (@guaranteed Hoozit) -> @owned Gizmo
  // CHECK: bb0(%0 : $Hoozit):
  // CHECK-NEXT:   debug_value %0
  // CHECK-NEXT:   [[ADDR:%.*]] = ref_element_addr %0 : {{.*}}, #Hoozit.typicalProperty
  // CHECK-NEXT:   [[RES:%.*]] = load [copy] [[ADDR]] {{.*}}
  // CHECK-NEXT:   return [[RES]]

  // -- setter
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC15typicalPropertySo5GizmoCfsTo : $@convention(objc_method) (Gizmo, Hoozit) -> () {
  // CHECK: bb0([[VALUE:%.*]] : $Gizmo, [[THIS:%.*]] : $Hoozit):
  // CHECK:   [[VALUE_COPY:%.*]] = copy_value [[VALUE]] : $Gizmo
  // CHECK:   [[THIS_COPY:%.*]] = copy_value [[THIS]] : $Hoozit
  // CHECK:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK:   // function_ref objc_thunks.Hoozit.typicalProperty.setter
  // CHECK:   [[FR:%.*]] = function_ref @_T011objc_thunks6HoozitC15typicalPropertySo5GizmoCfs
  // CHECK:   [[RES:%.*]] = apply [[FR]]([[VALUE_COPY]], [[BORROWED_THIS_COPY]])
  // CHECK:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK:   destroy_value [[THIS_COPY]]
  // CHECK:   return [[RES]] : $(), scope {{.*}} // id: {{.*}} line:[[@LINE-32]]:13:auto_gen
  // CHECK: } // end sil function '_T011objc_thunks6HoozitC15typicalPropertySo5GizmoCfsTo'

  // CHECK-LABEL: sil hidden @_T011objc_thunks6HoozitC15typicalPropertySo5GizmoCfs
  // CHECK: bb0([[ARG0:%.*]] : $Gizmo, [[ARG1:%.*]] : $Hoozit):
  // CHECK:   [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
  // CHECK:   [[ARG0_COPY:%.*]] = copy_value [[BORROWED_ARG0]]
  // CHECK:   [[ADDR:%.*]] = ref_element_addr [[ARG1]] : {{.*}}, #Hoozit.typicalProperty
  // CHECK:   assign [[ARG0_COPY]] to [[ADDR]] : $*Gizmo
  // CHECK:   end_borrow [[BORROWED_ARG0]] from [[ARG0]]
  // CHECK:   destroy_value [[ARG0]]
  // CHECK: } // end sil function '_T011objc_thunks6HoozitC15typicalPropertySo5GizmoCfs'

  // NS_RETURNS_RETAINED getter by family (-copy)
  @objc var copyProperty: Gizmo
  // -- getter
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC12copyPropertySo5GizmoCfgTo : $@convention(objc_method) (Hoozit) -> @owned Gizmo {
  // CHECK: bb0([[SELF:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK-NEXT:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK-NEXT:   // function_ref objc_thunks.Hoozit.copyProperty.getter
  // CHECK-NEXT:   [[FR:%.*]] = function_ref @_T011objc_thunks6HoozitC12copyPropertySo5GizmoCfg
  // CHECK-NEXT:   [[RES:%.*]] = apply [[FR]]([[BORROWED_SELF_COPY]])
  // CHECK-NEXT:   end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK-NEXT:   destroy_value [[SELF_COPY]]
  // CHECK-NEXT:   return [[RES]]
  // CHECK-NEXT: }

  // CHECK-LABEL: sil hidden @_T011objc_thunks6HoozitC12copyPropertySo5GizmoCfg
  // CHECK: bb0(%0 : $Hoozit):
  // CHECK:        [[ADDR:%.*]] = ref_element_addr %0 : {{.*}}, #Hoozit.copyProperty
  // CHECK-NEXT:   [[RES:%.*]] = load [copy] [[ADDR]]
  // CHECK-NEXT:   return [[RES]]

  // -- setter is normal
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC12copyPropertySo5GizmoCfsTo : $@convention(objc_method) (Gizmo, Hoozit) -> () {
  // CHECK: bb0([[VALUE:%.*]] : $Gizmo, [[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[VALUE_COPY:%.*]] = copy_value [[VALUE]]
  // CHECK-NEXT:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK-NEXT:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK-NEXT:   // function_ref objc_thunks.Hoozit.copyProperty.setter
  // CHECK-NEXT:   [[FR:%.*]] = function_ref @_T011objc_thunks6HoozitC12copyPropertySo5GizmoCfs
  // CHECK-NEXT:   [[RES:%.*]] = apply [[FR]]([[VALUE_COPY]], [[BORROWED_THIS_COPY]])
  // CHECK-NEXT:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK-NEXT:   destroy_value [[THIS_COPY]]
  // CHECK-NEXT:   return [[RES]]

  // CHECK-LABEL: sil hidden @_T011objc_thunks6HoozitC12copyPropertySo5GizmoCfs
  // CHECK: bb0([[ARG1:%.*]] : $Gizmo, [[SELF:%.*]] : $Hoozit):
  // CHECK:   [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG1]]
  // CHECK:   [[ARG1_COPY:%.*]] = copy_value [[BORROWED_ARG1]]
  // CHECK:   [[ADDR:%.*]] = ref_element_addr [[SELF]] : {{.*}}, #Hoozit.copyProperty
  // CHECK:   assign [[ARG1_COPY]] to [[ADDR]]
  // CHECK:   end_borrow [[BORROWED_ARG1]] from [[ARG1]]
  // CHECK:   destroy_value [[ARG1]]
  // CHECK: } // end sil function '_T011objc_thunks6HoozitC12copyPropertySo5GizmoCfs'

  @objc var roProperty: Gizmo { return self }
  // -- getter
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC10roPropertySo5GizmoCfgTo : $@convention(objc_method) (Hoozit) -> @autoreleased Gizmo {
  // CHECK: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK-NEXT:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_T011objc_thunks6HoozitC10roPropertySo5GizmoCfg : $@convention(method) (@guaranteed Hoozit) -> @owned Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[BORROWED_THIS_COPY]])
  // CHECK-NEXT:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK-NEXT:   destroy_value [[THIS_COPY]] : $Hoozit
  // CHECK-NEXT:   return [[RES]] : $Gizmo
  // CHECK-NEXT: } // end sil function '_T011objc_thunks6HoozitC10roPropertySo5GizmoCfgTo'

  // -- no setter
  // CHECK-NOT: sil hidden [thunk] @_T011objc_thunks6HoozitC10roPropertySo5GizmoCfsTo

  @objc var rwProperty: Gizmo {
    get {
      return self
    }
    set {}
  }
  // -- getter
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC10rwPropertySo5GizmoCfgTo : $@convention(objc_method) (Hoozit) -> @autoreleased Gizmo 

  // -- setter
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC10rwPropertySo5GizmoCfsTo : $@convention(objc_method) (Gizmo, Hoozit) -> () {
  // CHECK: bb0([[VALUE:%.*]] : $Gizmo, [[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[VALUE_COPY:%.*]] = copy_value [[VALUE]]
  // CHECK-NEXT:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK-NEXT:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_T011objc_thunks6HoozitC10rwPropertySo5GizmoCfs : $@convention(method) (@owned Gizmo, @guaranteed Hoozit) -> ()
  // CHECK-NEXT:   apply [[NATIVE]]([[VALUE_COPY]], [[BORROWED_THIS_COPY]])
  // CHECK-NEXT:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK-NEXT:   destroy_value [[THIS_COPY]]
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  @objc var copyRWProperty: Gizmo {
    get {
      return self
    }
    set {}
  }
  // -- getter
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC14copyRWPropertySo5GizmoCfgTo : $@convention(objc_method) (Hoozit) -> @owned Gizmo {
  // CHECK: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK-NEXT:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_T011objc_thunks6HoozitC14copyRWPropertySo5GizmoCfg : $@convention(method) (@guaranteed Hoozit) -> @owned Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[BORROWED_THIS_COPY]])
  // CHECK-NEXT:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK-NEXT:   destroy_value [[THIS_COPY]]
  // CHECK-NOT:    return
  // CHECK-NEXT:   return [[RES]]
  // CHECK-NEXT: }

  // -- setter is normal
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC14copyRWPropertySo5GizmoCfsTo : $@convention(objc_method) (Gizmo, Hoozit) -> () {
  // CHECK: bb0([[VALUE:%.*]] : $Gizmo, [[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[VALUE_COPY:%.*]] = copy_value [[VALUE]]
  // CHECK-NEXT:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK-NEXT:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_T011objc_thunks6HoozitC14copyRWPropertySo5GizmoCfs : $@convention(method) (@owned Gizmo, @guaranteed Hoozit) -> ()
  // CHECK-NEXT:   apply [[NATIVE]]([[VALUE_COPY]], [[BORROWED_THIS_COPY]])
  // CHECK-NEXT:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK-NEXT:   destroy_value [[THIS_COPY]]
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  @objc var initProperty: Gizmo
  // -- getter
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC12initPropertySo5GizmoCfgTo : $@convention(objc_method) (Hoozit) -> @autoreleased Gizmo {
  // CHECK: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK-NEXT:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_T011objc_thunks6HoozitC12initPropertySo5GizmoCfg : $@convention(method) (@guaranteed Hoozit) -> @owned Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[BORROWED_THIS_COPY]])
  // CHECK-NEXT:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK-NEXT:   destroy_value [[THIS_COPY]]
  // CHECK-NEXT:   return [[RES]]
  // CHECK-NEXT: }

  // -- setter
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC12initPropertySo5GizmoCfsTo : $@convention(objc_method) (Gizmo, Hoozit) -> () {
  // CHECK: bb0([[VALUE:%.*]] : $Gizmo, [[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[VALUE_COPY:%.*]] = copy_value [[VALUE]]
  // CHECK-NEXT:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK-NEXT:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_T011objc_thunks6HoozitC12initPropertySo5GizmoCfs : $@convention(method) (@owned Gizmo, @guaranteed Hoozit) -> ()
  // CHECK-NEXT:   apply [[NATIVE]]([[VALUE_COPY]], [[BORROWED_THIS_COPY]])
  // CHECK-NEXT:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK-NEXT:   destroy_value [[THIS_COPY]]
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  @objc var propComputed: Gizmo {
    @objc(initPropComputedGetter) get { return self }
    @objc(initPropComputedSetter:) set {}
  }
  // -- getter
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC12propComputedSo5GizmoCfgTo : $@convention(objc_method) (Hoozit) -> @autoreleased Gizmo {
  // CHECK: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK-NEXT:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_T011objc_thunks6HoozitC12propComputedSo5GizmoCfg : $@convention(method) (@guaranteed Hoozit) -> @owned Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[BORROWED_THIS_COPY]])
  // CHECK-NEXT:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK-NEXT:   destroy_value [[THIS_COPY]]
  // CHECK-NEXT:   return [[RES]]
  // CHECK-NEXT: }

  // -- setter
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC12propComputedSo5GizmoCfsTo : $@convention(objc_method) (Gizmo, Hoozit) -> () {
  // CHECK: bb0([[VALUE:%.*]] : $Gizmo, [[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   [[VALUE_COPY:%.*]] = copy_value [[VALUE]]
  // CHECK-NEXT:   [[THIS_COPY:%.*]] = copy_value [[THIS]]
  // CHECK-NEXT:   [[BORROWED_THIS_COPY:%.*]] = begin_borrow [[THIS_COPY]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_T011objc_thunks6HoozitC12propComputedSo5GizmoCfs : $@convention(method) (@owned Gizmo, @guaranteed Hoozit) -> ()
  // CHECK-NEXT:   apply [[NATIVE]]([[VALUE_COPY]], [[BORROWED_THIS_COPY]])
  // CHECK-NEXT:   end_borrow [[BORROWED_THIS_COPY]] from [[THIS_COPY]]
  // CHECK-NEXT:   destroy_value [[THIS_COPY]]
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  // Don't export generics to ObjC yet
  func generic<T>(_ x: T) {}
  // CHECK-NOT: sil hidden [thunk] @_TToFC11objc_thunks6Hoozit7generic{{.*}}

  // Constructor.
  // CHECK-LABEL: sil hidden @_T011objc_thunks6HoozitCACSi7bellsOn_tcfc : $@convention(method) (Int, @owned Hoozit) -> @owned Hoozit {
  // CHECK: [[SELF_BOX:%[0-9]+]] = alloc_box ${ var Hoozit }
  // CHECK: [[PB:%.*]] = project_box [[SELF_BOX]]
  // CHECK: [[SELFMUI:%[0-9]+]] = mark_uninitialized [derivedself] [[PB]]
  // CHECK: [[GIZMO:%[0-9]+]] = upcast [[SELF:%[0-9]+]] : $Hoozit to $Gizmo
  // CHECK: [[SUPERMETHOD:%[0-9]+]] = super_method [volatile] [[SELF]] : $Hoozit, #Gizmo.init!initializer.1.foreign : (Gizmo.Type) -> (Int) -> Gizmo!, $@convention(objc_method) (Int, @owned Gizmo) -> @owned Optional<Gizmo>
  // CHECK-NEXT: [[SELF_REPLACED:%[0-9]+]] = apply [[SUPERMETHOD]](%0, [[X:%[0-9]+]]) : $@convention(objc_method) (Int, @owned Gizmo) -> @owned Optional<Gizmo>
  // CHECK-NOT: unconditional_checked_cast downcast [[SELF_REPLACED]] : $Gizmo to $Hoozit
  // CHECK: unchecked_ref_cast
  // CHECK: return
  override init(bellsOn x : Int) {
    super.init(bellsOn: x)
    other()
  }

  // Subscript
  @objc subscript (i: Int) -> Hoozit {
  // Getter
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC9subscriptACSicfgTo : $@convention(objc_method) (Int, Hoozit) -> @autoreleased Hoozit
  // CHECK: bb0([[I:%[0-9]+]] : $Int, [[SELF:%[0-9]+]] : $Hoozit):
  // CHECK-NEXT: [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Hoozit
  // CHECK-NEXT: [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[NATIVE:%[0-9]+]] = function_ref @_T011objc_thunks6HoozitC9subscriptACSicfg : $@convention(method) (Int, @guaranteed Hoozit) -> @owned Hoozit
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[NATIVE]]([[I]], [[BORROWED_SELF_COPY]]) : $@convention(method) (Int, @guaranteed Hoozit) -> @owned Hoozit
  // CHECK-NEXT: end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK-NEXT: destroy_value [[SELF_COPY]]
  // CHECK-NEXT: return [[RESULT]] : $Hoozit
  get {
    return self
  }

  // Setter
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC9subscriptACSicfsTo : $@convention(objc_method) (Hoozit, Int, Hoozit) -> ()
  // CHECK: bb0([[VALUE:%[0-9]+]] : $Hoozit, [[I:%[0-9]+]] : $Int, [[SELF:%[0-9]+]] : $Hoozit):
  // CHECK:   [[VALUE_COPY:%.*]] = copy_value [[VALUE]] : $Hoozit
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Hoozit
  // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:   [[NATIVE:%[0-9]+]] = function_ref @_T011objc_thunks6HoozitC9subscriptACSicfs : $@convention(method) (@owned Hoozit, Int, @guaranteed Hoozit) -> ()
  // CHECK:   [[RESULT:%[0-9]+]] = apply [[NATIVE]]([[VALUE_COPY]], [[I]], [[BORROWED_SELF_COPY]]) : $@convention(method) (@owned Hoozit, Int, @guaranteed Hoozit) -> ()
  // CHECK:   end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK:   destroy_value [[SELF_COPY]]
  // CHECK:   return [[RESULT]] : $()
  // CHECK: } // end sil function '_T011objc_thunks6HoozitC9subscriptACSicfsTo'
  set {}
  }
}

class Wotsit<T> : Gizmo {
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6WotsitC5plainyyFTo : $@convention(objc_method) <T> (Wotsit<T>) -> () {
  // CHECK: bb0([[SELF:%.*]] : $Wotsit<T>):
  // CHECK-NEXT: [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Wotsit<T>
  // CHECK-NEXT: [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[NATIVE:%.*]] = function_ref @_T011objc_thunks6WotsitC5plainyyF : $@convention(method) <τ_0_0> (@guaranteed Wotsit<τ_0_0>) -> ()
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[NATIVE]]<T>([[BORROWED_SELF_COPY]]) : $@convention(method) <τ_0_0> (@guaranteed Wotsit<τ_0_0>) -> ()
  // CHECK-NEXT: end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK-NEXT: destroy_value [[SELF_COPY]] : $Wotsit<T>
  // CHECK-NEXT: return [[RESULT]] : $()
  // CHECK-NEXT: }
  @objc func plain() { }

  func generic<U>(_ x: U) {}

  var property : T

  init(t: T) {
    self.property = t
    super.init()
  }

  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6WotsitC11descriptionSSfgTo : $@convention(objc_method) <T> (Wotsit<T>) -> @autoreleased NSString {
  // CHECK: bb0([[SELF:%.*]] : $Wotsit<T>):
  // CHECK-NEXT:   [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Wotsit<T>
  // CHECK-NEXT:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_T011objc_thunks6WotsitC11descriptionSSfg : $@convention(method) <τ_0_0> (@guaranteed Wotsit<τ_0_0>) -> @owned String
  // CHECK-NEXT:   [[RESULT:%.*]] = apply [[NATIVE:%.*]]<T>([[BORROWED_SELF_COPY]]) : $@convention(method) <τ_0_0> (@guaranteed Wotsit<τ_0_0>) -> @owned String
  // CHECK-NEXT:   end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK-NEXT:   destroy_value [[SELF_COPY]] : $Wotsit<T>
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[BRIDGE:%.*]] = function_ref @_T0SS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
  // CHECK-NEXT:   [[BORROWED_RESULT:%.*]] = begin_borrow [[RESULT]]
  // CHECK-NEXT:   [[NSRESULT:%.*]] = apply [[BRIDGE]]([[BORROWED_RESULT]]) : $@convention(method) (@guaranteed String) -> @owned NSString
  // CHECK-NEXT:   end_borrow [[BORROWED_RESULT]] from [[RESULT]]
  // CHECK-NEXT:   destroy_value [[RESULT]]
  // CHECK-NEXT:   return [[NSRESULT]] : $NSString
  // CHECK-NEXT: }
  override var description : String {
    return "Hello, world."
  }

  // Ivar destroyer
  // CHECK: sil hidden @_T011objc_thunks6WotsitCfETo

  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6WotsitCSQyACyxGGycfcTo : $@convention(objc_method) <T> (@owned Wotsit<T>) -> @owned Optional<Wotsit<T>>

  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6WotsitCSQyACyxGGSi7bellsOn_tcfcTo : $@convention(objc_method) <T> (Int, @owned Wotsit<T>) -> @owned Optional<Wotsit<T>>
}

// CHECK-NOT: sil hidden [thunk] @_TToF{{.*}}Wotsit{{.*}}

// Extension initializers, properties and methods need thunks too.
extension Hoozit {
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitCACSi3int_tcfcTo : $@convention(objc_method) (Int, @owned Hoozit) -> @owned Hoozit
  @objc dynamic convenience init(int i: Int) { self.init(bellsOn: i) }

  // CHECK-LABEL: sil hidden @_T011objc_thunks6HoozitCACSd6double_tcfc : $@convention(method) (Double, @owned Hoozit) -> @owned Hoozit
  convenience init(double d: Double) { 
    // CHECK: [[SELF_BOX:%[0-9]+]] = alloc_box ${ var Hoozit }
    // CHECK: [[PB:%.*]] = project_box [[SELF_BOX]]
    // CHECK: [[SELFMUI:%[0-9]+]] = mark_uninitialized [delegatingself] [[PB]]
    // CHECK: [[X_BOX:%[0-9]+]] = alloc_box ${ var X }
    var x = X()
    // CHECK: [[CTOR:%[0-9]+]] = class_method [volatile] [[SELF:%[0-9]+]] : $Hoozit, #Hoozit.init!initializer.1.foreign : (Hoozit.Type) -> (Int) -> Hoozit, $@convention(objc_method) (Int, @owned Hoozit) -> @owned Hoozit
    // CHECK: [[NEW_SELF:%[0-9]+]] = apply [[CTOR]]
    // CHECK: store [[NEW_SELF]] to [init] [[SELFMUI]] : $*Hoozit
    // CHECK: [[NONNULL:%[0-9]+]] = is_nonnull [[NEW_SELF]] : $Hoozit
    // CHECK-NEXT: cond_br [[NONNULL]], [[NONNULL_BB:bb[0-9]+]], [[NULL_BB:bb[0-9]+]]
    // CHECK: [[NULL_BB]]:
    // CHECK-NEXT: destroy_value [[X_BOX]] : ${ var X }
    // CHECK-NEXT: br [[EPILOG_BB:bb[0-9]+]]

    // CHECK: [[NONNULL_BB]]:
    // CHECK:   [[OTHER_REF:%[0-9]+]] = function_ref @_T011objc_thunks5otheryyF : $@convention(thin) () -> ()
    // CHECK-NEXT: apply [[OTHER_REF]]() : $@convention(thin) () -> ()
    // CHECK-NEXT: destroy_value [[X_BOX]] : ${ var X }
    // CHECK-NEXT: br [[EPILOG_BB]]
    
    // CHECK: [[EPILOG_BB]]:
    // CHECK-NOT: super_method
    // CHECK: return
    self.init(int:Int(d))
    other()
  }

  func foof() {}
  // CHECK-LABEL: sil hidden [thunk] @_T011objc_thunks6HoozitC4foofyyFTo : $@convention(objc_method) (Hoozit) -> () {

  var extensionProperty: Int { return 0 }
  // CHECK-LABEL: sil hidden  @_T011objc_thunks6HoozitC17extensionPropertySifg : $@convention(method) (@guaranteed Hoozit) -> Int
}

// Calling objc methods of subclass should go through native entry points
func useHoozit(_ h: Hoozit) {
// sil @_T011objc_thunks9useHoozityAA0D0C1h_tF
  // In the class decl, gets dynamically dispatched
  h.fork()
  // CHECK: class_method {{%.*}} : {{.*}}, #Hoozit.fork!1 :

  // In an extension, 'dynamic' was inferred.
  h.foof()
  // CHECK: class_method [volatile] {{%.*}} : {{.*}}, #Hoozit.foof!1.foreign
}

func useWotsit(_ w: Wotsit<String>) {
// sil @_T011objc_thunks9useWotsitySo0D0CySSG1w_tF
  w.plain()
  // CHECK: class_method {{%.*}} : {{.*}}, #Wotsit.plain!1 :
  w.generic(2)
  // CHECK: class_method {{%.*}} : {{.*}}, #Wotsit.generic!1 :

  // Inherited methods only have objc entry points
  w.clone()
  // CHECK: class_method [volatile] {{%.*}} : {{.*}}, #Gizmo.clone!1.foreign
}

func other() { }

class X { }

// CHECK-LABEL: sil hidden @_T011objc_thunks8propertySiSo5GizmoCF
func property(_ g: Gizmo) -> Int {
  // CHECK: bb0([[ARG:%.*]] : $Gizmo):
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   class_method [volatile] [[BORROWED_ARG]] : $Gizmo, #Gizmo.count!getter.1.foreign
  return g.count
}

// CHECK-LABEL: sil hidden @_T011objc_thunks13blockPropertyySo5GizmoCF
func blockProperty(_ g: Gizmo) {
  // CHECK: bb0([[ARG:%.*]] : $Gizmo):
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   class_method [volatile] [[BORROWED_ARG]] : $Gizmo, #Gizmo.block!setter.1.foreign
  g.block = { }
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   class_method [volatile] [[BORROWED_ARG]] : $Gizmo, #Gizmo.block!getter.1.foreign
  g.block()
}

class DesignatedStubs : Gizmo {
  var i: Int

  override init() { i = 5 }

  // CHECK-LABEL: sil hidden @_T011objc_thunks15DesignatedStubsCSQyACGSi7bellsOn_tcfc
  // CHECK: function_ref @_T0s25_unimplementedInitializers5NeverOs12StaticStringV9className_AE04initG0AE4fileSu4lineSu6columntF
  // CHECK: string_literal utf8 "objc_thunks.DesignatedStubs"
  // CHECK: string_literal utf8 "init(bellsOn:)"
  // CHECK: string_literal utf8 "{{.*}}objc_thunks.swift"
  // CHECK: return

  // CHECK-NOT: sil hidden @_TFCSo15DesignatedStubsc{{.*}}
}

class DesignatedOverrides : Gizmo {
  var i: Int = 5

  // CHECK-LABEL: sil hidden @_T011objc_thunks19DesignatedOverridesCSQyACGycfc
  // CHECK-NOT: return
  // CHECK: function_ref @_T011objc_thunks19DesignatedOverridesC1iSivfi : $@convention(thin) () -> Int
  // CHECK: super_method [volatile] [[SELF:%[0-9]+]] : $DesignatedOverrides, #Gizmo.init!initializer.1.foreign : (Gizmo.Type) -> () -> Gizmo!, $@convention(objc_method) (@owned Gizmo) -> @owned Optional<Gizmo>
  // CHECK: return

  // CHECK-LABEL: sil hidden @_T011objc_thunks19DesignatedOverridesCSQyACGSi7bellsOn_tcfc
  // CHECK: function_ref @_T011objc_thunks19DesignatedOverridesC1iSivfi : $@convention(thin) () -> Int
  // CHECK: super_method [volatile] [[SELF:%[0-9]+]] : $DesignatedOverrides, #Gizmo.init!initializer.1.foreign : (Gizmo.Type) -> (Int) -> Gizmo!, $@convention(objc_method) (Int, @owned Gizmo) -> @owned Optional<Gizmo>
  // CHECK: return
}

// Make sure we copy blocks passed in as IUOs - <rdar://problem/22471309>

func registerAnsible() {
  // CHECK: function_ref @_T011objc_thunks15registerAnsibleyyFyyycSgcfU_
  Ansible.anseAsync({ completion in completion!() })
}
