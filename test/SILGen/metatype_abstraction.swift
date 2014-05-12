// RUN: %swift -parse-stdlib -emit-silgen %s | FileCheck %s

struct S {}
class C {}

struct Generic<T> {
  var value: T
}

struct GenericMetatype<T> {
  var value: T.Type
}

// CHECK-LABEL: sil @_TF20metatype_abstraction26genericMetatypeFromGeneric
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*Generic<T.Type>, #Generic.value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick T.Type
// CHECK:         return [[META]] : $@thick T.Type
// CHECK:       }
func genericMetatypeFromGeneric<T>(var x: Generic<T.Type>) -> T.Type {
  return x.value
}
// CHECK-LABEL: sil @_TF20metatype_abstraction26dynamicMetatypeFromGeneric
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*Generic<C.Type>, #Generic.value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick C.Type
// CHECK:         return [[META]] : $@thick C.Type
// CHECK:       }
func dynamicMetatypeFromGeneric(var x: Generic<C.Type>) -> C.Type {
  return x.value
}
// CHECK-LABEL: sil @_TF20metatype_abstraction25staticMetatypeFromGeneric
// CHECK:         [[META:%.*]] = metatype $@thin S.Type
// CHECK:         return [[META]] : $@thin S.Type
// CHECK:       }
func staticMetatypeFromGeneric(x: Generic<S.Type>) -> S.Type {
  return x.value
}

// CHECK-LABEL: sil @_TF20metatype_abstraction34genericMetatypeFromGenericMetatype
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*GenericMetatype<T>, #GenericMetatype.value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick T.Type
// CHECK:         return [[META]] : $@thick T.Type
// CHECK:       }
func genericMetatypeFromGenericMetatype<T>(var x: GenericMetatype<T>)-> T.Type {
  return x.value
}
// CHECK-LABEL: sil @_TF20metatype_abstraction34dynamicMetatypeFromGenericMetatype
// CHECK:         [[ADDR:%.*]] = struct_element_addr %1#1 : $*GenericMetatype<C>, #GenericMetatype.value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick C.Type
// CHECK:         return [[META]] : $@thick C.Type
// CHECK:       }
func dynamicMetatypeFromGenericMetatype(var x: GenericMetatype<C>) -> C.Type {
  return x.value
}
// CHECK-LABEL: sil @_TF20metatype_abstraction33staticMetatypeFromGenericMetatype
// CHECK:         [[META:%.*]] = metatype $@thin S.Type
// CHECK:         return [[META]] : $@thin S.Type
// CHECK:       }
func staticMetatypeFromGenericMetatype(x: GenericMetatype<S>) -> S.Type {
  return x.value
}

func takeGeneric<T>(x: T) {}
func takeGenericMetatype<T>(x: T.Type) {}

// CHECK-LABEL: sil @_TF20metatype_abstraction23staticMetatypeToGeneric
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick S.Type
// CHECK:         [[META:%.*]] = metatype $@thick S.Type
// CHECK:         store [[META]] to [[MAT]]#1 : $*@thick S.Type
// CHECK:         apply {{%.*}}<S.Type>([[MAT]]#1)
func staticMetatypeToGeneric(x: S.Type) {
  takeGeneric(x)
}
// CHECK-LABEL: sil @_TF20metatype_abstraction31staticMetatypeToGenericMetatype
// CHECK:         [[META:%.*]] = metatype $@thick S.Type
// CHECK:         apply {{%.*}}<S>([[META]])
func staticMetatypeToGenericMetatype(x: S.Type) {
  takeGenericMetatype(x)
}
// CHECK-LABEL: sil @_TF20metatype_abstraction24dynamicMetatypeToGeneric
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick C.Type
// CHECK:         apply {{%.*}}<C.Type>([[MAT]]#1) : $@thin <τ_0_0> (@in τ_0_0) -> ()
func dynamicMetatypeToGeneric(var x: C.Type) {
  takeGeneric(x)
}
// CHECK-LABEL: sil @_TF20metatype_abstraction32dynamicMetatypeToGenericMetatype
// CHECK:         [[META:%.*]] = load %1#1 : $*@thick C.Type
// CHECK:         apply {{%.*}}<C>([[META]]) : $@thin <τ_0_0> (@thick τ_0_0.Type) -> ()
func dynamicMetatypeToGenericMetatype(var x: C.Type) {
  takeGenericMetatype(x)
}
// CHECK-LABEL: sil @_TF20metatype_abstraction24genericMetatypeToGeneric
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick U.Type
// CHECK:         apply {{%.*}}<U.Type>([[MAT]]#1) : $@thin <τ_0_0> (@in τ_0_0) -> ()
func genericMetatypeToGeneric<U>(var x: U.Type) {
  takeGeneric(x)
}
func genericMetatypeToGenericMetatype<U>(x: U.Type) {
  takeGenericMetatype(x)
}
