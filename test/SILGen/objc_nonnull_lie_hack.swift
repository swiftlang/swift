// RUN: rm -rf %t/APINotes
// RUN: mkdir -p %t/APINotes
// RUN: %clang_apinotes -yaml-to-binary %S/Inputs/gizmo.apinotes -o %t/APINotes/gizmo.apinotesc
// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -I %t/APINotes -enable-source-import -primary-file %s | FileCheck -check-prefix=SILGEN %s
// RUN: %target-swift-frontend -emit-sil -O -sdk %S/Inputs -I %S/Inputs -I %t/APINotes -enable-source-import -primary-file %s | FileCheck -check-prefix=OPT %s

// REQUIRES: objc_interop

import Foundation
import gizmo

// SILGEN-LABEL: sil hidden @_TF21objc_nonnull_lie_hack10makeObjectFT_GSqCSo8NSObject_
// SILGEN:         [[INIT:%.*]] = function_ref @_TFCSo8NSObjectCfMS_FT_S_
// SILGEN:         [[NONOPTIONAL:%.*]] = apply [[INIT]]
// SILGEN:         [[OPTIONAL:%.*]] = unchecked_ref_bit_cast [[NONOPTIONAL]]

// OPT-LABEL: sil hidden @_TF21objc_nonnull_lie_hack10makeObjectFT_GSqCSo8NSObject_
// OPT:         [[OPT:%.*]] = unchecked_ref_bit_cast
// OPT:         switch_enum [[OPT]] : $Optional<NSObject>{{.*}} case #Optional.None!enumelt: [[NIL:bb[0-9]+]]
// OPT:       [[NIL]]:
// OPT:         string_literal utf8 "nil"

func makeObject() -> NSObject? {
  let foo: NSObject? = NSObject()
  if foo == nil {
    println("nil")
  }
  return foo
}

// OPT-LABEL: sil hidden @_TF21objc_nonnull_lie_hack18callInstanceMethod
// OPT: [[METHOD:%[0-9]+]] = class_method [volatile] [[OBJ:%[0-9]+]] : $Gizmo, #Gizmo.nonNilGizmo!1.foreign : Gizmo -> () -> Gizmo , $@convention(objc_method) (Gizmo) -> @autoreleased Gizmo
// OPT: [[NONOPTIONAL:%[0-9]+]] = apply [[METHOD]]([[OBJ]]) : $@convention(objc_method) (Gizmo) -> @autoreleased Gizmo
// OPT: [[OPTIONAL:%[0-9]+]] = unchecked_ref_bit_cast [[NONOPTIONAL]]
// OPT: switch_enum [[OPTIONAL]] : $Optional<Gizmo>
func callInstanceMethod(gizmo: Gizmo) -> Gizmo? {
  let foo: Gizmo? = gizmo.nonNilGizmo()

  if foo == nil {
    println("nil")
  }
  return foo
}

// OPT-LABEL: sil hidden @_TF21objc_nonnull_lie_hack15callClassMethod
// OPT: [[METATYPE:%[0-9]+]] = metatype $@thick Gizmo.Type
// OPT: [[METHOD:%[0-9]+]] = class_method [volatile] [[METATYPE]] : $@thick Gizmo.Type, #Gizmo.nonNilGizmo!1.foreign : Gizmo.Type -> () -> Gizmo , $@convention(objc_method) (@objc_metatype Gizmo.Type) -> @autoreleased Gizmo
// OPT: [[OBJC_METATYPE:%[0-9]+]] = metatype $@objc_metatype Gizmo.Type
// OPT: [[NONOPTIONAL:%[0-9]+]] = apply [[METHOD]]([[OBJC_METATYPE]]) : $@convention(objc_method) (@objc_metatype Gizmo.Type) -> @autoreleased Gizmo
// OPT: [[OPTIONAL:%[0-9]+]] = unchecked_ref_bit_cast [[NONOPTIONAL]] : $Gizmo to $Optional<Gizmo>
// OPT: switch_enum [[OPTIONAL]] : $Optional<Gizmo>
func callClassMethod() -> Gizmo? {
  let foo: Gizmo? = Gizmo.nonNilGizmo()
  if foo == nil {
    println("nil")
  }
  return foo  
}

// OPT-LABEL: sil hidden @_TF21objc_nonnull_lie_hack12loadPropertyFCSo5GizmoGSqS0__ : $@convention(thin) (@owned Gizmo) -> @owned Optional<Gizmo>
// OPT: [[GETTER:%[0-9]+]] = class_method [volatile] [[OBJ:%[0-9]+]] : $Gizmo, #Gizmo.nonNilGizmoProperty!getter.1.foreign : Gizmo -> () -> Gizmo , $@convention(objc_method) (Gizmo) -> @autoreleased Gizmo
// OPT: [[NONOPTIONAL:%[0-9]+]] = apply %1(%0) : $@convention(objc_method) (Gizmo) -> @autoreleased Gizmo
// OPT: [[OPTIONAL:%[0-9]+]] = unchecked_ref_bit_cast [[NONOPTIONAL]] : $Gizmo to $Optional<Gizmo>
// OPT: switch_enum [[OPTIONAL]] : $Optional<Gizmo>,
func loadProperty(gizmo: Gizmo) -> Gizmo? {
  let foo: Gizmo? = gizmo.nonNilGizmoProperty
  if foo == nil {
    println("nil")
  }
  return foo  
}

// OPT-LABEL: sil hidden @_TF21objc_nonnull_lie_hack19loadUnownedPropertyFCSo5GizmoGSqS0__
// OPT: [[GETTER:%[0-9]+]] = class_method [volatile] [[OBJ:%[0-9]+]] : $Gizmo, #Gizmo.unownedNonNilGizmoProperty!getter.1.foreign : Gizmo -> () -> Gizmo , $@convention(objc_method) (Gizmo) -> @autoreleased Gizmo
// OPT: [[NONOPTIONAL:%[0-9]+]] = apply [[GETTER]]([[OBJ]]) : $@convention(objc_method) (Gizmo) -> @autoreleased Gizmo
// OPT: [[OPTIONAL:%[0-9]+]] = unchecked_ref_bit_cast [[NONOPTIONAL]] : $Gizmo to $Optional<Gizmo>
// OPT: switch_enum [[OPTIONAL]] : $Optional<Gizmo>
func loadUnownedProperty(gizmo: Gizmo) -> Gizmo? {
  let foo: Gizmo? = gizmo.unownedNonNilGizmoProperty
  if foo == nil {
    println("nil")
  }
  return foo  
}
