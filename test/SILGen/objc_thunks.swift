// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen -emit-verbose-sil | FileCheck %s

// REQUIRES: objc_interop

import gizmo

class Hoozit : Gizmo {
  func typical(x: Int, y: Gizmo) -> Gizmo { return y }
  // CHECK-LABEL: sil hidden  @_TToFC11objc_thunks6Hoozit7typicalfS0_FTSi1yCSo5Gizmo_S1_ : $@cc(objc_method) @thin (Int, Gizmo, Hoozit) -> @autoreleased Gizmo {
  // CHECK-NEXT: bb0([[X:%.*]] : $Int, [[Y:%.*]] : $Gizmo, [[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   retain [[Y]]
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TFC11objc_thunks6Hoozit7typicalfS0_FTSi1yCSo5Gizmo_S1_ : $@cc(method) @thin (Int, @owned Gizmo, @owned Hoozit) -> @owned Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[X]], [[Y]], [[THIS]]) {{.*}} line:[[@LINE-7]]:8:auto_gen
  // CHECK-NEXT:   autorelease_return [[RES]] : $Gizmo // {{.*}} line:[[@LINE-8]]:8:auto_gen
  // CHECK-NEXT: }

  // NS_CONSUMES_SELF by inheritance
  override func fork() { }
  // CHECK-LABEL: sil hidden  @_TToFC11objc_thunks6Hoozit4forkfS0_FT_T_ : $@cc(objc_method) @thin (@owned Hoozit) -> () {
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TFC11objc_thunks6Hoozit4forkfS0_FT_T_ : $@cc(method) @thin (@owned Hoozit) -> ()
  // CHECK-NEXT:   apply [[NATIVE]]([[THIS]])
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  // NS_CONSUMED 'gizmo' argument by inheritance
  override class func consume(gizmo: Gizmo?) { }
   // CHECK-LABEL: sil hidden @_TToZFC11objc_thunks6Hoozit7consumefMS0_FGSqCSo5Gizmo_T_ : $@cc(objc_method) @thin (@owned Optional<Gizmo>, @objc_metatype Hoozit.Type) -> () {
  // CHECK-NEXT: bb0([[GIZMO:%.*]] : $Optional<Gizmo>, [[THIS:%.*]] : $@objc_metatype Hoozit.Type):
  // CHECK-NEXT: [[THICK_THIS:%[0-9]+]] = objc_to_thick_metatype [[THIS]] : $@objc_metatype Hoozit.Type to $@thick Hoozit.Type
  // CHECK:   [[NATIVE:%.*]] = function_ref @_TZFC11objc_thunks6Hoozit7consumefMS0_FGSqCSo5Gizmo_T_ : $@thin (@owned Optional<Gizmo>, @thick Hoozit.Type) -> ()
  // CHECK-NEXT:   apply [[NATIVE]]([[GIZMO]], [[THICK_THIS]])
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  // NS_RETURNS_RETAINED by family (-copy)
  func copyFoo() -> Gizmo { return self }
  // CHECK-LABEL: sil hidden  @_TToFC11objc_thunks6Hoozit7copyFoofS0_FT_CSo5Gizmo : $@cc(objc_method) @thin (Hoozit) -> @owned Gizmo
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TFC11objc_thunks6Hoozit7copyFoofS0_FT_CSo5Gizmo : $@cc(method) @thin (@owned Hoozit) -> @owned Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[THIS]])
  // CHECK-NOT:    autorelease_return
  // CHECK-NOT:    release
  // CHECK-NEXT:   return [[RES]]
  // CHECK-NEXT: }

  var typicalProperty: Gizmo
  // -- getter
  // CHECK-LABEL: sil hidden [transparent]  @_TToFC11objc_thunks6Hoozitg15typicalPropertyCSo5Gizmo : $@cc(objc_method) @thin (Hoozit) -> @autoreleased Gizmo {
  // CHECK-NEXT: bb0(%0 : $Hoozit):
  // CHECK-NEXT:   strong_retain %0
  // CHECK-NEXT:   // function_ref objc_thunks.Hoozit.typicalProperty.getter
  // CHECK-NEXT:   [[GETIMPL:%.*]] = function_ref @_TFC11objc_thunks6Hoozitg15typicalPropertyCSo5Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [transparent] [[GETIMPL]](%0)
  // CHECK-NEXT:   autorelease_return [[RES]] : $Gizmo
  // CHECK-NEXT: }
  
  // CHECK-LABEL: sil hidden [transparent] @_TFC11objc_thunks6Hoozitg15typicalPropertyCSo5Gizmo
  // CHECK-NEXT: bb0(%0 : $Hoozit):
  // CHECK-NEXT:   debug_value %0
  // CHECK-NEXT:   [[ADDR:%.*]] = ref_element_addr %0 : {{.*}}, #Hoozit.typicalProperty {{.*}}
  // CHECK-NEXT:   [[RES:%.*]] = load [[ADDR]] {{.*}}
  // CHECK-NEXT:   strong_retain [[RES]] : $Gizmo
  // CHECK-NEXT:   strong_release %0 : $Hoozit
  // CHECK-NEXT:   return [[RES]]

  // -- setter
  // CHECK-LABEL: sil hidden [transparent]  @_TToFC11objc_thunks6Hoozits15typicalPropertyCSo5Gizmo : $@cc(objc_method) @thin (Gizmo, Hoozit) -> () {
  // CHECK-NEXT: bb0([[VALUE:%.*]] : $Gizmo, [[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   retain [[VALUE]] : $Gizmo
  // CHECK-NEXT:   retain [[THIS]] : $Hoozit
  // CHECK-NEXT:   // function_ref objc_thunks.Hoozit.typicalProperty.setter
  // CHECK-NEXT:   [[FR:%.*]] = function_ref @_TFC11objc_thunks6Hoozits15typicalPropertyCSo5Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [transparent] [[FR]](%0, %1)
  // CHECK_NEXT:   return [[RES]] line:[[@LINE-19]]:7:auto_gen

  // CHECK-LABEL: sil hidden [transparent] @_TFC11objc_thunks6Hoozits15typicalPropertyCSo5Gizmo
  // CHECK-NEXT: bb0(%0 : $Gizmo, %1 : $Hoozit):
  // CHECK:        [[ADDR:%.*]] = ref_element_addr %1 : {{.*}}, #Hoozit.typicalProperty
  // CHECK-NEXT:   assign %0 to [[ADDR]] : $*Gizmo

  // NS_RETURNS_RETAINED getter by family (-copy)
  var copyProperty: Gizmo
  // -- getter
  // CHECK-LABEL: sil hidden [transparent] @_TToFC11objc_thunks6Hoozitg12copyPropertyCSo5Gizmo : $@cc(objc_method) @thin (Hoozit) -> @owned Gizmo {
  // CHECK-NEXT: bb0(%0 : $Hoozit):
  // CHECK-NEXT:   strong_retain %0
  // CHECK-NEXT:   // function_ref objc_thunks.Hoozit.copyProperty.getter
  // CHECK-NEXT:   [[FR:%.*]] = function_ref @_TFC11objc_thunks6Hoozitg12copyPropertyCSo5Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [transparent] [[FR]](%0)
  // CHECK-NEXT:   return [[RES]]
  // CHECK-NEXT: }

  // CHECK-LABEL: sil hidden [transparent] @_TFC11objc_thunks6Hoozitg12copyPropertyCSo5Gizmo
  // CHECK-NEXT: bb0(%0 : $Hoozit):
  // CHECK:        [[ADDR:%.*]] = ref_element_addr %0 : {{.*}}, #Hoozit.copyProperty
  // CHECK-NEXT:   [[RES:%.*]] = load [[ADDR]]
  // CHECK-NEXT:   retain [[RES]]
  // CHECK-NEXT:   release %0
  // CHECK-NEXT:   return [[RES]]

  // -- setter is normal
  // CHECK-LABEL: sil hidden [transparent]  @_TToFC11objc_thunks6Hoozits12copyPropertyCSo5Gizmo : $@cc(objc_method) @thin (Gizmo, Hoozit) -> () {
  // CHECK-NEXT: bb0([[VALUE:%.*]] : $Gizmo, [[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   retain [[VALUE]]
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   // function_ref objc_thunks.Hoozit.copyProperty.setter
  // CHECK-NEXT:   [[FR:%.*]] = function_ref @_TFC11objc_thunks6Hoozits12copyPropertyCSo5Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [transparent] [[FR]](%0, %1)
  // CHECK-NEXT:   return [[RES]]

  // CHECK-LABEL: sil hidden [transparent] @_TFC11objc_thunks6Hoozits12copyPropertyCSo5Gizmo
  // CHECK:        bb0(%0 : $Gizmo, %1 : $Hoozit):
  // CHECK:   [[ADDR:%.*]] = ref_element_addr %1 : {{.*}}, #Hoozit.copyProperty
  // CHECK-NEXT:   assign %0 to [[ADDR]]

  var roProperty: Gizmo { return self }
  // -- getter
  // CHECK-LABEL: sil hidden  @_TToFC11objc_thunks6Hoozitg10roPropertyCSo5Gizmo : $@cc(objc_method) @thin (Hoozit) -> @autoreleased Gizmo {
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TFC11objc_thunks6Hoozitg10roPropertyCSo5Gizmo : $@cc(method) @thin (@owned Hoozit) -> @owned Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[THIS]])
  // CHECK-NEXT:   autorelease_return [[RES]] : $Gizmo
  // CHECK-NEXT: }

  // -- no setter
  // CHECK-NOT: sil hidden @_TToFC11objc_thunks6Hoozits10roPropertyCSo5Gizmo

  var rwProperty: Gizmo {
    get {
      return self
    }
    set {}
  }
  // -- getter
  // CHECK-LABEL: sil hidden  @_TToFC11objc_thunks6Hoozitg10rwPropertyCSo5Gizmo : $@cc(objc_method) @thin (Hoozit) -> @autoreleased Gizmo 

  // -- setter
  // CHECK-LABEL: sil hidden  @_TToFC11objc_thunks6Hoozits10rwPropertyCSo5Gizmo : $@cc(objc_method) @thin (Gizmo, Hoozit) -> () {
  // CHECK-NEXT: bb0([[VALUE:%.*]] : $Gizmo, [[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   retain [[VALUE]]
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TFC11objc_thunks6Hoozits10rwPropertyCSo5Gizmo : $@cc(method) @thin (@owned Gizmo, @owned Hoozit) -> ()
  // CHECK-NEXT:   apply [[NATIVE]]([[VALUE]], [[THIS]])
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  var copyRWProperty: Gizmo {
    get {
      return self
    }
    set {}
  }
  // -- getter
  // CHECK-LABEL: sil hidden  @_TToFC11objc_thunks6Hoozitg14copyRWPropertyCSo5Gizmo : $@cc(objc_method) @thin (Hoozit) -> @owned Gizmo {
  // CHECK-NEXT: bb0([[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TFC11objc_thunks6Hoozitg14copyRWPropertyCSo5Gizmo : $@cc(method) @thin (@owned Hoozit) -> @owned Gizmo
  // CHECK-NEXT:   [[RES:%.*]] = apply [[NATIVE]]([[THIS]])
  // CHECK-NOT:    release
  // CHECK-NOT:    autorelease_return
  // CHECK-NEXT:   return [[RES]]
  // CHECK-NEXT: }

  // -- setter is normal
  // CHECK-LABEL: sil hidden  @_TToFC11objc_thunks6Hoozits14copyRWPropertyCSo5Gizmo : $@cc(objc_method) @thin (Gizmo, Hoozit) -> () {
  // CHECK-NEXT: bb0([[VALUE:%.*]] : $Gizmo, [[THIS:%.*]] : $Hoozit):
  // CHECK-NEXT:   retain [[VALUE]]
  // CHECK-NEXT:   retain [[THIS]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   [[NATIVE:%.*]] = function_ref @_TFC11objc_thunks6Hoozits14copyRWPropertyCSo5Gizmo : $@cc(method) @thin (@owned Gizmo, @owned Hoozit) -> ()
  // CHECK-NEXT:   apply [[NATIVE]]([[VALUE]], [[THIS]])
  // CHECK-NEXT:   return
  // CHECK-NEXT: }

  // Don't export generics to ObjC yet
  func generic<T>(x: T) {}
  // CHECK-NOT: sil hidden @_TToFC11objc_thunks6Hoozit7genericfS_U__FT_T_

  // Constructor.
  // CHECK-LABEL: sil hidden @_TFC11objc_thunks6HoozitcfMS0_FT7bellsOnSi_S0_ : $@cc(method) @thin (Int, @owned Hoozit) -> @owned Hoozit {
  // CHECK: [[SELF_BOX:%[0-9]+]] = alloc_box $Hoozit
  // CHECK: [[SELFMUI:%[0-9]+]] = mark_uninitialized [derivedself] [[SELF_BOX]]#1
  // CHECK: [[GIZMO:%[0-9]+]] = upcast [[SELF:%[0-9]+]] : $Hoozit to $Gizmo
  // CHECK-NEXT: [[SUPERMETHOD:%[0-9]+]] = super_method [volatile] [[SELF]] : $Hoozit, #Gizmo.init!initializer.1.foreign : Gizmo.Type -> (bellsOn: Int) -> Gizmo! , $@cc(objc_method) @thin (Int, @owned Gizmo) -> @owned ImplicitlyUnwrappedOptional<Gizmo>
  // CHECK-NEXT: [[SELF_REPLACED:%[0-9]+]] = apply [[SUPERMETHOD]](%0, [[X:%[0-9]+]]) : $@cc(objc_method) @thin (Int, @owned Gizmo) -> @owned ImplicitlyUnwrappedOptional<Gizmo>
  // CHECK-NOT: unconditional_checked_cast downcast [[SELF_REPLACED]] : $Gizmo to $Hoozit
  // CHECK: unchecked_ref_cast
  // CHECK-NEXT: store [[SELF:%[0-9]+]] to [[SELFMUI]] : $*Hoozit
  // CHECK-NEXT: [[NONNULL:%[0-9]+]] = is_nonnull [[SELF]] : $Hoozit
  // CHECK-NEXT: cond_br [[NONNULL]], [[NONNULL_BB:bb[0-9]+]], [[NULL_BB:bb[0-9]+]]
  // CHECK: [[NULL_BB]]:
  // CHECK:   br [[EPILOG_BB:bb[0-9]+]]

  // CHECK: [[NONNULL_BB]]:
  // CHECK:   [[OTHER_REF:%[0-9]+]] = function_ref @_TF11objc_thunks5otherFT_T_ : $@thin () -> ()
  // CHECK-NEXT: apply [[OTHER_REF]]() : $@thin () -> ()
  // CHECK-NEXT: br [[EPILOG_BB]]

  // CHECK: [[EPILOG_BB]]:
  // CHECK-NOT: super_method
  // CHECK: return
  override init(bellsOn x : Int) {
    super.init(bellsOn: x)
    other()
  }

  // Subscript
  subscript (i: Int) -> Hoozit {
  // Getter
  // CHECK-LABEL: sil hidden @_TToFC11objc_thunks6Hoozitg9subscriptFSiS0_ : $@cc(objc_method) @thin (Int, Hoozit) -> @autoreleased Hoozit
  // CHECK-NEXT: bb0([[I:%[0-9]+]] : $Int, [[SELF:%[0-9]+]] : $Hoozit):
  // CHECK-NEXT:   strong_retain [[SELF]] : $Hoozit
  // CHECK: [[NATIVE:%[0-9]+]] = function_ref @_TFC11objc_thunks6Hoozitg9subscript{{.*}} : $@cc(method) @thin (Int, @owned Hoozit) -> @owned Hoozit
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[NATIVE]]([[I]], [[SELF]]) : $@cc(method) @thin (Int, @owned Hoozit) -> @owned Hoozit
  // CHECK-NEXT: autorelease_return [[RESULT]] : $Hoozit
  get {
    return self
  }

  // Setter
  // CHECK-LABEL: sil hidden @_TToFC11objc_thunks6Hoozits9subscriptFSiS0_ : $@cc(objc_method) @thin (Hoozit, Int, Hoozit) -> ()
  // CHECK-NEXT: bb0([[SELF:%[0-9]+]] : $Hoozit, [[I:%[0-9]+]] : $Int, [[VALUE:%[0-9]+]] : $Hoozit):
  // CHECK-NEXT: strong_retain [[SELF]] : $Hoozit
  // CHECK_NEXT: strong_retain [[VALUE]] : $Hoozit
  // CHECK: [[NATIVE:%[0-9]+]] = function_ref @_TFC11objc_thunks6Hoozits9subscript{{.*}} : $@cc(method) @thin (@owned Hoozit, Int, @owned Hoozit) -> ()
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[NATIVE]]([[SELF]], [[I]], [[VALUE]]) : $@cc(method) @thin (@owned Hoozit, Int, @owned Hoozit) -> ()
  // CHECK-NEXT: return [[RESULT]] : $()
  set {}
  }
}

// Don't export generics to ObjC yet
class Wotsit<T> : Gizmo {
  func plain() { }

  func generic<U>(x: U) {}

  var property : T

  init(t: T) {
    self.property = t
    super.init()
  }
}
// We should emit no @objc symbols except for the deallocator and ivar
// destroyer.
// CHECK-NOT: sil hidden @_TToF{{.*}}Wotsit{{.*}}
// CHECK: sil hidden @_TToF{{.*}}WotsitE
// CHECK-NOT: sil hidden @_TToF{{.*}}Wotsit{{.*}}

// Extension initializers, properties and methods need thunks too.
extension Hoozit {
  // CHECK-LABEL: sil hidden @_TToFC11objc_thunks6HoozitcfMS0_FT3intSi_S0_ : $@cc(objc_method) @thin (Int, @owned Hoozit) -> @owned Hoozit
  dynamic convenience init(int i: Int) { self.init(bellsOn: i) }

  // CHECK-LABEL: sil hidden @_TFC11objc_thunks6HoozitcfMS0_FT6doubleSd_S0_ : $@cc(method) @thin (Double, @owned Hoozit) -> @owned Hoozit
  convenience init(double d: Double) { 
    // CHECK: [[SELF_BOX:%[0-9]+]] = alloc_box $Hoozit
    // CHECK: [[SELFMUI:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1
    // CHECK: [[X_BOX:%[0-9]+]] = alloc_box $X
    var x = X()
    // CHECK: [[CTOR:%[0-9]+]] = class_method [volatile] [[SELF:%[0-9]+]] : $Hoozit, #Hoozit.init!initializer.1.foreign : Hoozit.Type -> (int: Int) -> Hoozit , $@cc(objc_method) @thin (Int, @owned Hoozit) -> @owned Hoozit
    // CHECK: [[NEW_SELF:%[0-9]+]] = apply [[CTOR]]
    // CHECK: store [[NEW_SELF]] to [[SELFMUI]] : $*Hoozit
    // CHECK: [[NONNULL:%[0-9]+]] = is_nonnull [[NEW_SELF]] : $Hoozit
    // CHECK-NEXT: cond_br [[NONNULL]], [[NONNULL_BB:bb[0-9]+]], [[NULL_BB:bb[0-9]+]]
    // CHECK: [[NULL_BB]]:
    // CHECK-NEXT: strong_release [[X_BOX]]#0 : $Builtin.NativeObject
    // CHECK-NEXT: br [[EPILOG_BB:bb[0-9]+]]

    // CHECK: [[NONNULL_BB]]:
    // CHECK:   [[OTHER_REF:%[0-9]+]] = function_ref @_TF11objc_thunks5otherFT_T_ : $@thin () -> ()
    // CHECK-NEXT: apply [[OTHER_REF]]() : $@thin () -> ()
    // CHECK-NEXT: strong_release [[X_BOX]]#0 : $Builtin.NativeObject
    // CHECK-NEXT: br [[EPILOG_BB]]
    
    // CHECK: [[EPILOG_BB]]:
    // CHECK-NOT: super_method
    // CHECK: return
    self.init(int:Int(d))
    other()
  }

  func foof() {}
  // CHECK-LABEL: sil hidden @_TToFC11objc_thunks6Hoozit4fooffS0_FT_T_ : $@cc(objc_method) @thin (Hoozit) -> () {

  var extensionProperty: Int { return 0 }
  // CHECK-LABEL: sil hidden  @_TFC11objc_thunks6Hoozitg17extensionPropertySi : $@cc(method) @thin (@owned Hoozit) -> Int
}

// Calling objc methods of subclass should go through native entry points
func useHoozit(h: Hoozit) {
// sil @_TF11objc_thunks9useHoozitFT1hC11objc_thunks6Hoozit_T_
  // In the class decl, gets dynamically dispatched
  h.fork()
  // CHECK: class_method {{%.*}} : {{.*}}, #Hoozit.fork!1 :

  // In an extension, 'dynamic' was inferred.
  h.foof()
  // CHECK: class_method [volatile] {{%.*}} : {{.*}}, #Hoozit.foof!1.foreign
}

func useWotsit(w: Wotsit<String>) {
// sil @_TF11objc_thunks9useWotsitFT1wGCSo6WotsitSS__T_
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

// CHECK-LABEL: sil hidden @_TF11objc_thunks8property
func property(g: Gizmo) -> Int {
  // CHECK: class_method [volatile] %0 : $Gizmo, #Gizmo.count!getter.1.foreign
  return g.count
}

// CHECK-LABEL: sil hidden @_TF11objc_thunks13blockProperty
func blockProperty(g: Gizmo) {
  // CHECK: class_method [volatile] %0 : $Gizmo, #Gizmo.block!setter.1.foreign
  g.block = { }
  // CHECK: class_method [volatile] %0 : $Gizmo, #Gizmo.block!getter.1.foreign
  g.block()
}

class DesignatedStubs : Gizmo {
  var i: Int

  override init() { i = 5 }

  // CHECK-LABEL: sil hidden @_TFC11objc_thunks15DesignatedStubscfMS0_FT7bellsOnSi_GSQS0__
  // CHECK: function_ref @_TFSs26_unimplemented_initializer
  // CHECK: string_literal utf8 "objc_thunks.DesignatedStubs"
  // CHECK: string_literal utf8 "init(bellsOn:)"
  // CHECK: string_literal utf8 "{{.*}}objc_thunks.swift"
  // CHECK: return

  // CHECK-NOT: sil hidden @_TFCSo15DesignatedStubscfMS_FT12withoutBellsSi_S_
}

class DesignatedOverrides : Gizmo {
  var i: Int = 5

  // CHECK-LABEL: sil hidden @_TFC11objc_thunks19DesignatedOverridescfMS0_FT_GSQS0__
  // CHECK-NOT: return
  // CHECK: function_ref @_TFSiCfMSiFT22_builtinIntegerLiteralBi2048__Si
  // CHECK: super_method [volatile] [[SELF:%[0-9]+]] : $DesignatedOverrides, #Gizmo.init!initializer.1.foreign : Gizmo.Type -> () -> Gizmo! , $@cc(objc_method) @thin (@owned Gizmo) -> @owned ImplicitlyUnwrappedOptional<Gizmo>
  // CHECK: return

  // CHECK-LABEL: sil hidden @_TFC11objc_thunks19DesignatedOverridescfMS0_FT7bellsOnSi_GSQS0__
  // CHECK: function_ref @_TFSiCfMSiFT22_builtinIntegerLiteralBi2048__Si
  // CHECK: super_method [volatile] [[SELF:%[0-9]+]] : $DesignatedOverrides, #Gizmo.init!initializer.1.foreign : Gizmo.Type -> (bellsOn: Int) -> Gizmo! , $@cc(objc_method) @thin (Int, @owned Gizmo) -> @owned ImplicitlyUnwrappedOptional<Gizmo>
  // CHECK: return
}
