// RUN: %swift -parse-stdlib -emit-silgen %s | FileCheck %s

struct S {}
class C {}

struct Generic<T> {
  var value: T
}

struct GenericMetatype<T> {
  var value: T.metatype
}

// CHECK-LABEL: sil @_TF20metatype_abstraction26genericMetatypeFromGenericU__FT1xGVS_7GenericMQ___MQ_ : $@thin <T> (Generic<T.metatype>) -> @thick T.metatype {
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*Generic<T.metatype>, #value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick T.metatype
// CHECK:         return [[META]] : $@thick T.metatype
// CHECK:       }
func genericMetatypeFromGeneric<T>(x: Generic<T.metatype>) -> T.metatype {
  return x.value
}
// CHECK-LABEL: sil @_TF20metatype_abstraction26dynamicMetatypeFromGenericFT1xGVS_7GenericMCS_1C__MS1_ : $@thin (Generic<C.metatype>) -> @thick C.metatype {
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*Generic<C.metatype>, #value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick C.metatype
// CHECK:         return [[META]] : $@thick C.metatype
// CHECK:       }
func dynamicMetatypeFromGeneric(x: Generic<C.metatype>) -> C.metatype {
  return x.value
}
// CHECK-LABEL: sil @_TF20metatype_abstraction25staticMetatypeFromGenericFT1xGVS_7GenericMVS_1S__MS1_ : $@thin (Generic<S.metatype>) -> @thin S.metatype {
// CHECK:         [[META:%.*]] = metatype $@thin S.metatype
// CHECK:         return [[META]] : $@thin S.metatype
// CHECK:       }
func staticMetatypeFromGeneric(x: Generic<S.metatype>) -> S.metatype {
  return x.value
}

// CHECK-LABEL: sil @_TF20metatype_abstraction34genericMetatypeFromGenericMetatypeU__FT1xGVS_15GenericMetatypeQ___MQ_ : $@thin <T> (GenericMetatype<T>) -> @thick T.metatype {
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*GenericMetatype<T>, #value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick T.metatype
// CHECK:         return [[META]] : $@thick T.metatype
// CHECK:       }
func genericMetatypeFromGenericMetatype<T>(x: GenericMetatype<T>)-> T.metatype {
  return x.value
}
// CHECK-LABEL: sil @_TF20metatype_abstraction34dynamicMetatypeFromGenericMetatypeFT1xGVS_15GenericMetatypeCS_1C__MS1_ : $@thin (GenericMetatype<C>) -> @thick C.metatype {
// CHECK:         [[ADDR:%.*]] = struct_element_addr %1#1 : $*GenericMetatype<C>, #value
// CHECK:         [[META:%.*]] = load [[ADDR]] : $*@thick C.metatype
// CHECK:         return [[META]] : $@thick C.metatype
// CHECK:       }
func dynamicMetatypeFromGenericMetatype(x: GenericMetatype<C>) -> C.metatype {
  return x.value
}
// CHECK-LABEL: sil @_TF20metatype_abstraction33staticMetatypeFromGenericMetatypeFT1xGVS_15GenericMetatypeVS_1S__MS1_ : $@thin (GenericMetatype<S>) -> @thin S.metatype {
// CHECK:         [[META:%.*]] = metatype $@thin S.metatype
// CHECK:         return [[META]] : $@thin S.metatype
// CHECK:       }
func staticMetatypeFromGenericMetatype(x: GenericMetatype<S>) -> S.metatype {
  return x.value
}

func takeGeneric<T>(x: T) {}
func takeGenericMetatype<T>(x: T.metatype) {}

// CHECK-LABEL: sil @_TF20metatype_abstraction23staticMetatypeToGenericFT1xMVS_1S_T_ : $@thin (@thin S.metatype) -> ()
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick S.metatype
// CHECK:         [[META:%.*]] = metatype $@thick S.metatype
// CHECK:         store [[META]] to [[MAT]]#1 : $*@thick S.metatype
// CHECK:         apply {{%.*}}<T = S.metatype>([[MAT]]#1)
func staticMetatypeToGeneric(x: S.metatype) {
  takeGeneric(x)
}
// CHECK-LABEL: sil @_TF20metatype_abstraction31staticMetatypeToGenericMetatypeFT1xMVS_1S_T_ : $@thin (@thin S.metatype) -> ()
// CHECK:         [[META:%.*]] = metatype $@thick S.metatype
// CHECK:         apply {{%.*}}<T = S>([[META]])
func staticMetatypeToGenericMetatype(x: S.metatype) {
  takeGenericMetatype(x)
}
// CHECK-LABEL: sil @_TF20metatype_abstraction24dynamicMetatypeToGenericFT1xMCS_1C_T_ : $@thin (@thick C.metatype) -> () {
// CHECK:         [[META:%.*]] = load {{%.*}} : $*@thick C.metatype
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick C.metatype
// CHECK:         store [[META]] to [[MAT]]#1 : $*@thick C.metatype
// CHECK:         apply {{%.*}}<T = C.metatype>([[MAT]]#1) : $@thin <T> (@in T) -> ()
func dynamicMetatypeToGeneric(x: C.metatype) {
  takeGeneric(x)
}
// CHECK-LABEL: sil @_TF20metatype_abstraction32dynamicMetatypeToGenericMetatypeFT1xMCS_1C_T_ : $@thin (@thick C.metatype) -> () {
// CHECK:         [[META:%.*]] = load %1#1 : $*@thick C.metatype
// CHECK:         apply {{%.*}}<T = C>([[META]]) : $@thin <T> (@thick T.metatype) -> ()
func dynamicMetatypeToGenericMetatype(x: C.metatype) {
  takeGenericMetatype(x)
}
// CHECK-LABEL: sil @_TF20metatype_abstraction24genericMetatypeToGenericU__FT1xMQ__T_ : $@thin <U> (@thick U.metatype) -> () {
// CHECK:         [[META:%.*]] = load {{%.*}} : $*@thick U.metatype
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick U.metatype
// CHECK:         store [[META]] to [[MAT]]#1 : $*@thick U.metatype
// CHECK:         apply {{%.*}}<T = U.metatype>([[MAT]]#1) : $@thin <T> (@in T) -> ()
func genericMetatypeToGeneric<U>(x: U.metatype) {
  takeGeneric(x)
}
func genericMetatypeToGenericMetatype<U>(x: U.metatype) {
  takeGenericMetatype(x)
}
