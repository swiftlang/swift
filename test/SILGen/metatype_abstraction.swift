
// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership -module-name Swift -parse-stdlib %s | %FileCheck %s

enum Optional<Wrapped> {
  case none
  case some(Wrapped)
}

struct S {}
class C {}

struct Generic<T> {
  var value: T
}

struct GenericMetatype<T> {
  var value: T.Type
}

// CHECK-LABEL: sil hidden @$Ss26genericMetatypeFromGeneric{{[_0-9a-zA-Z]*}}F
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*Generic<T.Type>, #Generic.value
// CHECK:         [[META:%.*]] = load [trivial] [[ADDR]] : $*@thick T.Type
// CHECK:         return [[META]] : $@thick T.Type
// CHECK:       }
func genericMetatypeFromGeneric<T>(_ x: Generic<T.Type>) -> T.Type {
  var x = x
  return x.value
}
// CHECK-LABEL: sil hidden @$Ss26dynamicMetatypeFromGeneric{{[_0-9a-zA-Z]*}}F
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*Generic<C.Type>, #Generic.value
// CHECK:         [[META:%.*]] = load [trivial] [[ADDR]] : $*@thick C.Type
// CHECK:         return [[META]] : $@thick C.Type
// CHECK:       }
func dynamicMetatypeFromGeneric(_ x: Generic<C.Type>) -> C.Type {
  var x = x
  return x.value
}
// CHECK-LABEL: sil hidden @$Ss25staticMetatypeFromGeneric{{[_0-9a-zA-Z]*}}F
// CHECK:         [[META:%.*]] = metatype $@thin S.Type
// CHECK:         return [[META]] : $@thin S.Type
// CHECK:       }
func staticMetatypeFromGeneric(_ x: Generic<S.Type>) -> S.Type {
  return x.value
}

// CHECK-LABEL: sil hidden @$Ss026genericMetatypeFromGenericB0{{[_0-9a-zA-Z]*}}F
// CHECK:         [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*GenericMetatype<T>, #GenericMetatype.value
// CHECK:         [[META:%.*]] = load [trivial] [[ADDR]] : $*@thick T.Type
// CHECK:         return [[META]] : $@thick T.Type
// CHECK:       }
func genericMetatypeFromGenericMetatype<T>(_ x: GenericMetatype<T>)-> T.Type {
  var x = x
  return x.value
}
// CHECK-LABEL: sil hidden @$Ss026dynamicMetatypeFromGenericB0ys1CCms0dB0VyACGF
// CHECK:         [[XBOX:%[0-9]+]] = alloc_box ${ var GenericMetatype<C> }
// CHECK:         [[PX:%[0-9]+]] = project_box [[XBOX]]
// CHECK:         [[READ:%.*]] = begin_access [read] [unknown] [[PX]] : $*GenericMetatype<C>
// CHECK:         [[ADDR:%.*]] = struct_element_addr [[READ]] : $*GenericMetatype<C>, #GenericMetatype.value
// CHECK:         [[META:%.*]] = load [trivial] [[ADDR]] : $*@thick C.Type
// CHECK:         return [[META]] : $@thick C.Type
// CHECK:       }
func dynamicMetatypeFromGenericMetatype(_ x: GenericMetatype<C>) -> C.Type {
  var x = x
  return x.value
}

func takeGeneric<T>(_ x: T) {}
func takeGenericMetatype<T>(_ x: T.Type) {}

// CHECK-LABEL: sil hidden @$Ss23staticMetatypeToGeneric{{[_0-9a-zA-Z]*}}F
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick S.Type
// CHECK:         [[META:%.*]] = metatype $@thick S.Type
// CHECK:         store [[META]] to [trivial] [[MAT]] : $*@thick S.Type
// CHECK:         apply {{%.*}}<S.Type>([[MAT]])
func staticMetatypeToGeneric(_ x: S.Type) {
  takeGeneric(x)
}
// CHECK-LABEL: sil hidden @$Ss023staticMetatypeToGenericB0{{[_0-9a-zA-Z]*}}F
// CHECK:         [[META:%.*]] = metatype $@thick S.Type
// CHECK:         apply {{%.*}}<S>([[META]])
func staticMetatypeToGenericMetatype(_ x: S.Type) {
  takeGenericMetatype(x)
}
// CHECK-LABEL: sil hidden @$Ss24dynamicMetatypeToGeneric{{[_0-9a-zA-Z]*}}F
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick C.Type
// CHECK:         apply {{%.*}}<C.Type>([[MAT]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> ()
func dynamicMetatypeToGeneric(_ x: C.Type) {
  var x = x
  takeGeneric(x)
}
// CHECK-LABEL: sil hidden @$Ss024dynamicMetatypeToGenericB0yys1CCmF
// CHECK:         [[XBOX:%[0-9]+]] = alloc_box ${ var @thick C.Type }
// CHECK:         [[PX:%[0-9]+]] = project_box [[XBOX]]
// CHECK:         [[READ:%.*]] = begin_access [read] [unknown] [[PX]] : $*@thick C.Type
// CHECK:         [[META:%.*]] = load [trivial] [[READ]] : $*@thick C.Type
// CHECK:         apply {{%.*}}<C>([[META]]) : $@convention(thin) <τ_0_0> (@thick τ_0_0.Type) -> ()
func dynamicMetatypeToGenericMetatype(_ x: C.Type) {
  var x = x
  takeGenericMetatype(x)
}
// CHECK-LABEL: sil hidden @$Ss24genericMetatypeToGeneric{{[_0-9a-zA-Z]*}}F
// CHECK:         [[MAT:%.*]] = alloc_stack $@thick U.Type
// CHECK:         apply {{%.*}}<U.Type>([[MAT]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> ()
func genericMetatypeToGeneric<U>(_ x: U.Type) {
  var x = x
  takeGeneric(x)
}
func genericMetatypeToGenericMetatype<U>(_ x: U.Type) {
  takeGenericMetatype(x)
}

// CHECK-LABEL: sil hidden @$Ss019static_metatype_of_B0ys1SVmmACF
// CHECK:         metatype $@thin S.Type.Type
func static_metatype_of_metatype(_ x: S) -> S.Type.Type {
  return type(of: type(of: x))
}

// CHECK-LABEL: sil hidden @$Ss018class_metatype_of_B0ys1CCmmACF
// CHECK:         [[METATYPE:%.*]] = value_metatype $@thick C.Type
// CHECK:         [[META_METATYPE:%.*]] = value_metatype $@thick C.Type.Type, [[METATYPE]]
func class_metatype_of_metatype(_ x: C) -> C.Type.Type {
  return type(of: type(of: x))
}

// CHECK-LABEL: sil hidden @$Ss020generic_metatype_of_B0{{[_0-9a-zA-Z]*}}F
// CHECK:         [[METATYPE:%.*]] = value_metatype $@thick T.Type
// CHECK:         [[META_METATYPE:%.*]] = value_metatype $@thick T.Type.Type, [[METATYPE]]
func generic_metatype_of_metatype<T>(_ x: T) -> T.Type.Type {
  return type(of: type(of: x))
}

// FIXME rdar://problem/18419772
/*
func existential_metatype_of_metatype(_ x: Any) -> Any.Type.Type {
 return type(of: type(of: x))
}
 */

func function_metatype_of_metatype(_ x: @escaping () -> ()) -> (() -> ()).Type.Type {
  return type(of: type(of: x))
}
