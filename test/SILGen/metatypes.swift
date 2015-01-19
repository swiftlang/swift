// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | FileCheck %s

import Swift

// FIXME: typeof should be available through the standard library.

protocol SomeProtocol {
  func method()
  func static_method()
}

protocol Any {}
struct SomeStruct : Any {}

class SomeClass : SomeProtocol {
  func method() {}
  func static_method() {}
}

class SomeSubclass : SomeClass {}

// CHECK-LABEL: sil hidden @_TF9metatypes16static_metatypes
func static_metatypes()
  -> (SomeStruct.Type, SomeClass.Type, SomeClass.Type)
{
  // CHECK: [[STRUCT:%[0-9]+]] = metatype $@thin SomeStruct.Type
  // CHECK: [[CLASS:%[0-9]+]] = metatype $@thick SomeClass.Type
  // CHECK: [[SUBCLASS:%[0-9]+]] = metatype $@thick SomeSubclass.Type
  // CHECK: [[SUBCLASS_UPCAST:%[0-9]+]] = upcast [[SUBCLASS]] : ${{.*}} to $@thick SomeClass.Type
  // CHECK: tuple ([[STRUCT]] : {{.*}}, [[CLASS]] : {{.*}}, [[SUBCLASS_UPCAST]] : {{.*}})
  return (SomeStruct.self, SomeClass.self, SomeSubclass.self)
}

// CHECK-LABEL: sil hidden @_TF9metatypes16struct_metatypes
func struct_metatypes(s: SomeStruct)
  -> (SomeStruct.Type, SomeStruct.Type)
{
  // CHECK: [[STRUCT1:%[0-9]+]] = metatype $@thin SomeStruct.Type
  // CHECK: [[STRUCT2:%[0-9]+]] = metatype $@thin SomeStruct.Type
  // CHECK: tuple ([[STRUCT1]] : {{.*}}, [[STRUCT2]] : {{.*}})
  return (s.dynamicType, SomeStruct.self)
}

// CHECK-LABEL: sil hidden @_TF9metatypes15class_metatypes
func class_metatypes(c: SomeClass, s: SomeSubclass)
  -> (SomeClass.Type, SomeClass.Type)
{
  // CHECK: [[CLASS:%[0-9]+]] = value_metatype $@thick SomeClass.Type,
  // CHECK: [[SUBCLASS:%[0-9]+]] = value_metatype $@thick SomeSubclass.Type,
  // CHECK: [[SUBCLASS_UPCAST:%[0-9]+]] = upcast [[SUBCLASS]] : ${{.*}} to $@thick SomeClass.Type
  // CHECK: tuple ([[CLASS]] : {{.*}}, [[SUBCLASS_UPCAST]] : {{.*}})
  return (c.dynamicType, s.dynamicType)
}

// CHECK-LABEL: sil hidden @_TF9metatypes19archetype_metatypes
// CHECK-NEXT: bb0(%0 : $*T):
func archetype_metatypes<T>(t: T) -> (T.Type, T.Type) {
  // CHECK: [[STATIC_T:%[0-9]+]] = metatype $@thick T.Type
  // CHECK: [[DYN_T:%[0-9]+]] = value_metatype $@thick T.Type, %0
  // CHECK: tuple ([[STATIC_T]] : {{.*}}, [[DYN_T]] : {{.*}})
  return (T.self, t.dynamicType)
}

// CHECK-LABEL: sil hidden @_TF9metatypes21existential_metatypes
func existential_metatypes(p: SomeProtocol) -> SomeProtocol.Type {
  // CHECK: existential_metatype $@thick SomeProtocol.Type
  return p.dynamicType
}

struct SomeGenericStruct<T> {}

func generic_metatypes<T>(x: T)
  -> (SomeGenericStruct<T>.Type, SomeGenericStruct<SomeStruct>.Type)
{
  // CHECK: metatype $@thin SomeGenericStruct<T>
  // CHECK: metatype $@thin SomeGenericStruct<SomeStruct>
  return (SomeGenericStruct<T>.self, SomeGenericStruct<SomeStruct>.self)
}

// rdar://16610078

// CHECK-LABEL: sil hidden @_TF9metatypes30existential_metatype_from_thinFT_PMPS_3Any_ : $@thin () -> @thick Any.Type
// CHECK:      [[T0:%.*]] = metatype $@thin SomeStruct.Type
// CHECK-NEXT: [[T1:%.*]] = metatype $@thick SomeStruct.Type
// CHECK-NEXT: [[T2:%.*]] = init_existential_metatype [[T1]] : $@thick SomeStruct.Type : $@thick Any.Type
// CHECK-NEXT: return [[T2]] : $@thick Any.Type
func existential_metatype_from_thin() -> Any.Type {
  return SomeStruct.self
}

// CHECK-LABEL: sil hidden @_TF9metatypes36existential_metatype_from_thin_valueFT_PMPS_3Any_ : $@thin () -> @thick Any.Type
// CHECK:      [[T0:%.*]] = function_ref @_TFV9metatypes10SomeStructCfMS0_FT_S0_
// CHECK-NEXT: [[T1:%.*]] = metatype $@thin SomeStruct.Type
// CHECK-NEXT: [[T2:%.*]] = apply [[T0]]([[T1]])
// CHECK-NEXT: debug_value [[T2]] : $SomeStruct  // let s
// CHECK-NEXT: [[T0:%.*]] = metatype $@thin SomeStruct.Type
// CHECK-NEXT: [[T1:%.*]] = metatype $@thick SomeStruct.Type
// CHECK-NEXT: [[T2:%.*]] = init_existential_metatype [[T1]] : $@thick SomeStruct.Type : $@thick Any.Type
// CHECK-NEXT: return [[T2]] : $@thick Any.Type
func existential_metatype_from_thin_value() -> Any.Type {
  let s = SomeStruct()
  return s.dynamicType
}

// CHECK-LABEL: sil hidden @_TF9metatypes20specialized_metatypeFT_GVSs10DictionarySSSi_
// CHECK:         metatype $@thin Dictionary<String, Int>.Type
func specialized_metatype() -> Dictionary<String, Int> {
  let dict = Swift.Dictionary<Swift.String, Int>()
  return dict
}
