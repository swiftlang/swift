// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -parse-stdlib -emit-silgen %s | %FileCheck %s

import Swift

protocol SomeProtocol {
  func method()
  func static_method()
}


protocol A {}
struct SomeStruct : A {}


class SomeClass : SomeProtocol {
  func method() {}
  func static_method() {}
}

class SomeSubclass : SomeClass {}

// CHECK-LABEL: sil hidden @_T09metatypes07static_A0{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden @_T09metatypes07struct_A0{{[_0-9a-zA-Z]*}}F
func struct_metatypes(s: SomeStruct)
  -> (SomeStruct.Type, SomeStruct.Type)
{
  // CHECK: [[STRUCT1:%[0-9]+]] = metatype $@thin SomeStruct.Type
  // CHECK: [[STRUCT2:%[0-9]+]] = metatype $@thin SomeStruct.Type
  // CHECK: tuple ([[STRUCT1]] : {{.*}}, [[STRUCT2]] : {{.*}})
  return (type(of: s), SomeStruct.self)
}

// CHECK-LABEL: sil hidden @_T09metatypes06class_A0{{[_0-9a-zA-Z]*}}F
func class_metatypes(c: SomeClass, s: SomeSubclass)
  -> (SomeClass.Type, SomeClass.Type)
{
  // CHECK: [[CLASS:%[0-9]+]] = value_metatype $@thick SomeClass.Type,
  // CHECK: [[SUBCLASS:%[0-9]+]] = value_metatype $@thick SomeSubclass.Type,
  // CHECK: [[SUBCLASS_UPCAST:%[0-9]+]] = upcast [[SUBCLASS]] : ${{.*}} to $@thick SomeClass.Type
  // CHECK: tuple ([[CLASS]] : {{.*}}, [[SUBCLASS_UPCAST]] : {{.*}})
  return (type(of: c), type(of: s))
}

// CHECK-LABEL: sil hidden @_T09metatypes010archetype_A0{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*T):
func archetype_metatypes<T>(t: T) -> (T.Type, T.Type) {
  // CHECK: [[STATIC_T:%[0-9]+]] = metatype $@thick T.Type
  // CHECK: [[DYN_T:%[0-9]+]] = value_metatype $@thick T.Type, %0
  // CHECK: tuple ([[STATIC_T]] : {{.*}}, [[DYN_T]] : {{.*}})
  return (T.self, type(of: t))
}

// CHECK-LABEL: sil hidden @_T09metatypes012existential_A0{{[_0-9a-zA-Z]*}}F
func existential_metatypes(p: SomeProtocol) -> SomeProtocol.Type {
  // CHECK: existential_metatype $@thick SomeProtocol.Type
  return type(of: p)
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

// CHECK-LABEL: sil hidden @_T09metatypes30existential_metatype_from_thinypXpyF : $@convention(thin) () -> @thick Any.Type
// CHECK:      [[T0:%.*]] = metatype $@thin SomeStruct.Type
// CHECK-NEXT: [[T1:%.*]] = metatype $@thick SomeStruct.Type
// CHECK-NEXT: [[T2:%.*]] = init_existential_metatype [[T1]] : $@thick SomeStruct.Type, $@thick Any.Type
// CHECK-NEXT: return [[T2]] : $@thick Any.Type
func existential_metatype_from_thin() -> Any.Type {
  return SomeStruct.self
}

// CHECK-LABEL: sil hidden @_T09metatypes36existential_metatype_from_thin_valueypXpyF : $@convention(thin) () -> @thick Any.Type
// CHECK:      [[T0:%.*]] = function_ref @_T09metatypes10SomeStructV{{[_0-9a-zA-Z]*}}fC
// CHECK-NEXT: [[T1:%.*]] = metatype $@thin SomeStruct.Type
// CHECK-NEXT: [[T2:%.*]] = apply [[T0]]([[T1]])
// CHECK-NEXT: debug_value [[T2]] : $SomeStruct, let, name "s"
// CHECK-NEXT: [[T0:%.*]] = metatype $@thin SomeStruct.Type
// CHECK-NEXT: [[T1:%.*]] = metatype $@thick SomeStruct.Type
// CHECK-NEXT: [[T2:%.*]] = init_existential_metatype [[T1]] : $@thick SomeStruct.Type, $@thick Any.Type
// CHECK-NEXT: return [[T2]] : $@thick Any.Type
func existential_metatype_from_thin_value() -> Any.Type {
  let s = SomeStruct()
  return type(of: s)
}

// CHECK-LABEL: sil hidden @_T09metatypes20specialized_metatypes10DictionaryVySSSiGyF
// CHECK:         metatype $@thin Dictionary<String, Int>.Type
func specialized_metatype() -> Dictionary<String, Int> {
  let dict = Swift.Dictionary<Swift.String, Int>()
  return dict
}
