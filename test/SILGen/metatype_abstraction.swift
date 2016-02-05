// RUN: %target-swift-frontend -emit-silgen -module-name Swift -parse-stdlib %s | FileCheck %s

enum Optional<T> {
  case Some(T), None
}

struct S {}
class C {}

struct Generic<T> {
  var value: T
}

struct GenericMetatype<T> {
  var value: T.Type
}

// CHECK-LABEL: sil hidden @_TFs26genericMetatypeFromGeneric
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*Generic<T.Type>, #Generic.value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick T.Type
// CHECK:         return [[META]] : $@thick T.Type
// CHECK:       }
func genericMetatypeFromGeneric<T>(x: Generic<T.Type>) -> T.Type {
  var x = x
  return x.value
}
// CHECK-LABEL: sil hidden @_TFs26dynamicMetatypeFromGeneric
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*Generic<C.Type>, #Generic.value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick C.Type
// CHECK:         return [[META]] : $@thick C.Type
// CHECK:       }
func dynamicMetatypeFromGeneric(x: Generic<C.Type>) -> C.Type {
  var x = x
  return x.value
}
// CHECK-LABEL: sil hidden @_TFs25staticMetatypeFromGeneric
// CHECK:         [[META:%.*]] = metatype $@thin S.Type
// CHECK:         return [[META]] : $@thin S.Type
// CHECK:       }
func staticMetatypeFromGeneric(x: Generic<S.Type>) -> S.Type {
  return x.value
}

// CHECK-LABEL: sil hidden @_TFs34genericMetatypeFromGenericMetatype
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*GenericMetatype<T>, #GenericMetatype.value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick T.Type
// CHECK:         return [[META]] : $@thick T.Type
// CHECK:       }
func genericMetatypeFromGenericMetatype<T>(x: GenericMetatype<T>)-> T.Type {
  var x = x
  return x.value
}
// CHECK-LABEL: sil hidden @_TFs34dynamicMetatypeFromGenericMetatypeFGVs15GenericMetatypeCs1C_MS0_
// CHECK:         [[XBOX:%[0-9]+]] = alloc_box $GenericMetatype<C>
// CHECK:         [[PX:%[0-9]+]] = project_box [[XBOX]]
// CHECK:         [[ADDR:%.*]] = struct_element_addr [[PX]] : $*GenericMetatype<C>, #GenericMetatype.value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick C.Type
// CHECK:         return [[META]] : $@thick C.Type
// CHECK:       }
func dynamicMetatypeFromGenericMetatype(x: GenericMetatype<C>) -> C.Type {
  var x = x
  return x.value
}

func takeGeneric<T>(x: T) {}
func takeGenericMetatype<T>(x: T.Type) {}

// CHECK-LABEL: sil hidden @_TFs23staticMetatypeToGeneric
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick S.Type
// CHECK:         [[META:%.*]] = metatype $@thick S.Type
// CHECK:         store [[META]] to [[MAT]] : $*@thick S.Type
// CHECK:         apply {{%.*}}<S.Type>([[MAT]])
func staticMetatypeToGeneric(x: S.Type) {
  takeGeneric(x)
}
// CHECK-LABEL: sil hidden @_TFs31staticMetatypeToGenericMetatype
// CHECK:         [[META:%.*]] = metatype $@thick S.Type
// CHECK:         apply {{%.*}}<S>([[META]])
func staticMetatypeToGenericMetatype(x: S.Type) {
  takeGenericMetatype(x)
}
// CHECK-LABEL: sil hidden @_TFs24dynamicMetatypeToGeneric
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick C.Type
// CHECK:         apply {{%.*}}<C.Type>([[MAT]]) : $@convention(thin) <τ_0_0> (@in τ_0_0) -> ()
func dynamicMetatypeToGeneric(x: C.Type) {
  var x = x
  takeGeneric(x)
}
// CHECK-LABEL: sil hidden @_TFs32dynamicMetatypeToGenericMetatypeFMCs1CT_
// CHECK:         [[XBOX:%[0-9]+]] = alloc_box $@thick C.Type
// CHECK:         [[PX:%[0-9]+]] = project_box [[XBOX]]
// CHECK:         [[META:%.*]] = load [[PX]] : $*@thick C.Type
// CHECK:         apply {{%.*}}<C>([[META]]) : $@convention(thin) <τ_0_0> (@thick τ_0_0.Type) -> ()
func dynamicMetatypeToGenericMetatype(x: C.Type) {
  var x = x
  takeGenericMetatype(x)
}
// CHECK-LABEL: sil hidden @_TFs24genericMetatypeToGeneric
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick U.Type
// CHECK:         apply {{%.*}}<U.Type>([[MAT]]) : $@convention(thin) <τ_0_0> (@in τ_0_0) -> ()
func genericMetatypeToGeneric<U>(x: U.Type) {
  var x = x
  takeGeneric(x)
}
func genericMetatypeToGenericMetatype<U>(x: U.Type) {
  takeGenericMetatype(x)
}

// CHECK-LABEL: sil hidden @_TFs27static_metatype_of_metatypeFVs1SMMS_
// CHECK:         metatype $@thin S.Type.Type
func static_metatype_of_metatype(x: S) -> S.Type.Type {
  return x.dynamicType.dynamicType
}

// CHECK-LABEL: sil hidden @_TFs26class_metatype_of_metatypeFCs1CMMS_
// CHECK:         [[METATYPE:%.*]] = value_metatype $@thick C.Type
// CHECK:         [[META_METATYPE:%.*]] = value_metatype $@thick C.Type.Type, [[METATYPE]]
func class_metatype_of_metatype(x: C) -> C.Type.Type {
  return x.dynamicType.dynamicType
}

// CHECK-LABEL: sil hidden @_TFs28generic_metatype_of_metatype
// CHECK:         [[METATYPE:%.*]] = value_metatype $@thick T.Type
// CHECK:         [[META_METATYPE:%.*]] = value_metatype $@thick T.Type.Type, [[METATYPE]]
func generic_metatype_of_metatype<T>(x: T) -> T.Type.Type {
  return x.dynamicType.dynamicType
}

// FIXME rdar://problem/18419772
/*
func existential_metatype_of_metatype(x: protocol<>) -> protocol<>.Type.Type {
  return x.dynamicType.dynamicType
}
 */

func function_metatype_of_metatype(x: () -> ()) -> (() -> ()).Type.Type {
  return x.dynamicType.dynamicType
}
