// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | FileCheck %s

struct S {}
class C {}

struct Generic<T> {
  var value: T
}

struct GenericMetatype<T> {
  var value: T.Type
}

// CHECK-LABEL: sil hidden @_TF20metatype_abstraction26genericMetatypeFromGeneric
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*Generic<T.Type>, #Generic.value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick T.Type
// CHECK:         return [[META]] : $@thick T.Type
// CHECK:       }
func genericMetatypeFromGeneric<T>(var x: Generic<T.Type>) -> T.Type {
  return x.value
}
// CHECK-LABEL: sil hidden @_TF20metatype_abstraction26dynamicMetatypeFromGeneric
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*Generic<C.Type>, #Generic.value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick C.Type
// CHECK:         return [[META]] : $@thick C.Type
// CHECK:       }
func dynamicMetatypeFromGeneric(var x: Generic<C.Type>) -> C.Type {
  return x.value
}
// CHECK-LABEL: sil hidden @_TF20metatype_abstraction25staticMetatypeFromGeneric
// CHECK:         [[META:%.*]] = metatype $@thin S.Type
// CHECK:         return [[META]] : $@thin S.Type
// CHECK:       }
func staticMetatypeFromGeneric(x: Generic<S.Type>) -> S.Type {
  return x.value
}

// CHECK-LABEL: sil hidden @_TF20metatype_abstraction34genericMetatypeFromGenericMetatype
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*GenericMetatype<T>, #GenericMetatype.value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick T.Type
// CHECK:         return [[META]] : $@thick T.Type
// CHECK:       }
func genericMetatypeFromGenericMetatype<T>(var x: GenericMetatype<T>)-> T.Type {
  return x.value
}
// CHECK-LABEL: sil hidden @_TF20metatype_abstraction34dynamicMetatypeFromGenericMetatype
// CHECK:         [[ADDR:%.*]] = struct_element_addr %1#1 : $*GenericMetatype<C>, #GenericMetatype.value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick C.Type
// CHECK:         return [[META]] : $@thick C.Type
// CHECK:       }
func dynamicMetatypeFromGenericMetatype(var x: GenericMetatype<C>) -> C.Type {
  return x.value
}
// CHECK-LABEL: sil hidden @_TF20metatype_abstraction33staticMetatypeFromGenericMetatype
// CHECK:         [[META:%.*]] = metatype $@thin S.Type
// CHECK:         return [[META]] : $@thin S.Type
// CHECK:       }
func staticMetatypeFromGenericMetatype(x: GenericMetatype<S>) -> S.Type {
  return x.value
}

func takeGeneric<T>(x: T) {}
func takeGenericMetatype<T>(x: T.Type) {}

// CHECK-LABEL: sil hidden @_TF20metatype_abstraction23staticMetatypeToGeneric
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick S.Type
// CHECK:         [[META:%.*]] = metatype $@thick S.Type
// CHECK:         store [[META]] to [[MAT]]#1 : $*@thick S.Type
// CHECK:         apply {{%.*}}<S.Type>([[MAT]]#1)
func staticMetatypeToGeneric(x: S.Type) {
  takeGeneric(x)
}
// CHECK-LABEL: sil hidden @_TF20metatype_abstraction31staticMetatypeToGenericMetatype
// CHECK:         [[META:%.*]] = metatype $@thick S.Type
// CHECK:         apply {{%.*}}<S>([[META]])
func staticMetatypeToGenericMetatype(x: S.Type) {
  takeGenericMetatype(x)
}
// CHECK-LABEL: sil hidden @_TF20metatype_abstraction24dynamicMetatypeToGeneric
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick C.Type
// CHECK:         apply {{%.*}}<C.Type>([[MAT]]#1) : $@convention(thin) <τ_0_0> (@in τ_0_0) -> ()
func dynamicMetatypeToGeneric(var x: C.Type) {
  takeGeneric(x)
}
// CHECK-LABEL: sil hidden @_TF20metatype_abstraction32dynamicMetatypeToGenericMetatype
// CHECK:         [[META:%.*]] = load %1#1 : $*@thick C.Type
// CHECK:         apply {{%.*}}<C>([[META]]) : $@convention(thin) <τ_0_0> (@thick τ_0_0.Type) -> ()
func dynamicMetatypeToGenericMetatype(var x: C.Type) {
  takeGenericMetatype(x)
}
// CHECK-LABEL: sil hidden @_TF20metatype_abstraction24genericMetatypeToGeneric
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick U.Type
// CHECK:         apply {{%.*}}<U.Type>([[MAT]]#1) : $@convention(thin) <τ_0_0> (@in τ_0_0) -> ()
func genericMetatypeToGeneric<U>(var x: U.Type) {
  takeGeneric(x)
}
func genericMetatypeToGenericMetatype<U>(x: U.Type) {
  takeGenericMetatype(x)
}

// CHECK-LABEL: sil hidden @_TF20metatype_abstraction27static_metatype_of_metatypeFVS_1SMMS0_
// CHECK:         metatype $@thin S.Type.Type
func static_metatype_of_metatype(x: S) -> S.Type.Type {
  return x.dynamicType.dynamicType
}

// CHECK-LABEL: sil hidden @_TF20metatype_abstraction26class_metatype_of_metatypeFCS_1CMMS0_
// CHECK:         [[METATYPE:%.*]] = value_metatype $@thick C.Type
// CHECK:         [[META_METATYPE:%.*]] = value_metatype $@thick C.Type.Type, [[METATYPE]]
func class_metatype_of_metatype(x: C) -> C.Type.Type {
  return x.dynamicType.dynamicType
}

// CHECK-LABEL: sil hidden @_TF20metatype_abstraction28generic_metatype_of_metatypeurFq_MMq_
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
