// RUN: %swift -parse-stdlib -emit-sil %s | FileCheck %s

// FIXME: typeof should be available through the standard library.

protocol SomeProtocol {
  typealias SomeAssociated

  func method()
  func static_method()
}

struct SomeStruct {}

class SomeClass : SomeProtocol {
  typealias SomeAssociated = SomeStruct

  func method() {}
  func static_method() {}
}

class SomeSubclass : SomeClass {}

// CHECK: sil @_T9metatypes16static_metatypesFT_TMVS_10SomeStructMCS_9SomeClassMS1__ :
func static_metatypes()
  -> (SomeStruct.metatype, SomeClass.metatype, SomeClass.metatype)
{
  // CHECK: [[STRUCT:%[0-9]+]] = metatype $SomeStruct.metatype
  // CHECK: [[CLASS:%[0-9]+]] = metatype $SomeClass.metatype
  // CHECK: [[SUBCLASS:%[0-9]+]] = metatype $SomeSubclass.metatype
  // CHECK: [[SUBCLASS_UPCAST:%[0-9]+]] = upcast [[SUBCLASS]] : ${{.*}} to $SomeClass.metatype
  // CHECK: tuple ([[STRUCT]] : {{.*}}, [[CLASS]] : {{.*}}, [[SUBCLASS_UPCAST]] : {{.*}})
  return (SomeStruct, SomeClass, SomeSubclass)
}

// CHECK: sil @_T9metatypes16struct_metatypesFT1sVS_10SomeStruct_TMS0_MS0__ :
func struct_metatypes(s:SomeStruct)
  -> (SomeStruct.metatype, SomeStruct.metatype)
{
  // CHECK: [[STRUCT1:%[0-9]+]] = metatype $SomeStruct.metatype
  // CHECK: [[STRUCT2:%[0-9]+]] = metatype $SomeStruct.metatype
  // CHECK: tuple ([[STRUCT1]] : {{.*}}, [[STRUCT2]] : {{.*}})
  return (Builtin.typeof(s), SomeStruct)
}

// CHECK: sil @_T9metatypes15class_metatypesFT1cCS_9SomeClass1sCS_12SomeSubclass_TMS0_MS0__ :
func class_metatypes(c:SomeClass, s:SomeSubclass)
  -> (SomeClass.metatype, SomeClass.metatype)
{
  // CHECK: [[CADDR:%[0-9]+]] = alloc_var stack $SomeClass
  // CHECK: [[SADDR:%[0-9]+]] = alloc_var stack $SomeSubclass
  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[CLASS:%[0-9]+]] = class_metatype $SomeClass.metatype, [[C]]
  // CHECK: [[S:%[0-9]+]] = load [[SADDR]]
  // CHECK: [[SUBCLASS:%[0-9]+]] = class_metatype $SomeSubclass.metatype, [[S]]
  // CHECK: [[SUBCLASS_UPCAST:%[0-9]+]] = upcast [[SUBCLASS]] : ${{.*}} to $SomeClass.metatype
  // CHECK: tuple ([[CLASS]] : {{.*}}, [[SUBCLASS_UPCAST]] : {{.*}})
  return (Builtin.typeof(c), Builtin.typeof(s))
}

// CHECK: sil @_T9metatypes19archetype_metatypesU__FT1tQ__TMQ_MQ__ :
func archetype_metatypes<T>(t:T) -> (T.metatype, T.metatype) {
  // CHECK: [[STATIC_T:%[0-9]+]] = metatype $T.metatype
  // CHECK: [[T_VALUE:%[0-9]+]] = alloc_var stack $T
  // CHECK: [[DYN_T:%[0-9]+]] = archetype_metatype $T.metatype, [[T_VALUE]]
  // CHECK: tuple ([[STATIC_T]] : {{.*}}, [[DYN_T]] : {{.*}})
  return (T, Builtin.typeof(t))
}

// CHECK: sil @_T9metatypes18protocol_metatypesFT1pPS_12SomeProtocol__MPS0__ :
func protocol_metatypes(p:SomeProtocol) -> SomeProtocol.metatype {
  // CHECK: protocol_metatype $SomeProtocol.metatype
  return Builtin.typeof(p)
}

struct SomeGenericStruct<T> {}

func generic_metatypes<T>(x:T)
  -> (SomeGenericStruct<T>.metatype, SomeGenericStruct<SomeStruct>.metatype)
{
  // CHECK: metatype $SomeGenericStruct<T>
  // CHECK: metatype $SomeGenericStruct<SomeStruct>
  return (SomeGenericStruct<T>, SomeGenericStruct<SomeStruct>)
}
