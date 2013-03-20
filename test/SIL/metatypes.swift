// RUN: %swift -parse-as-library -dump-sil %s | FileCheck %s

protocol SomeProtocol {
  typealias SomeAssociated

  func method()
  func static_method()
}

struct SomeStruct {}

class SomeClass : SomeProtocol {
  typealias SomeAssociated = Int

  func method() {}
  func static_method() {}
}

class SomeSubclass : SomeClass {}

// CHECK: func_decl static_metatypes :
func static_metatypes()
  -> (SomeStruct.metatype, SomeClass.metatype, SomeClass.metatype)
{
  // CHECK: [[STRUCT:%[0-9]+]] = metatype $SomeStruct.metatype
  // CHECK: [[CLASS:%[0-9]+]] = metatype $SomeClass.metatype
  // CHECK: [[SUBCLASS:%[0-9]+]] = metatype $SomeSubclass.metatype
  // CHECK: [[SUBCLASS_UPCAST:%[0-9]+]] = upcast [[SUBCLASS]], $SomeClass.metatype
  // CHECK: tuple ([[STRUCT]], [[CLASS]], [[SUBCLASS_UPCAST]])
  return (SomeStruct, SomeClass, SomeSubclass)
}

// CHECK: func_decl struct_metatypes :
func struct_metatypes(s:SomeStruct)
  -> (SomeStruct.metatype, SomeStruct.metatype)
{
  // CHECK: [[STRUCT1:%[0-9]+]] = metatype $SomeStruct.metatype
  // CHECK: [[STRUCT2:%[0-9]+]] = metatype $SomeStruct.metatype
  // CHECK: tuple ([[STRUCT1]], [[STRUCT2]])
  return (s.metatype, SomeStruct)
}

// CHECK: func_decl class_metatypes :
func class_metatypes(c:SomeClass, s:SomeSubclass)
  -> (SomeClass.metatype, SomeClass.metatype)
{
  // CHECK: [[CBOX:%[0-9]+]] = alloc_box $SomeClass
  // CHECK: [[SBOX:%[0-9]+]] = alloc_box $SomeSubclass
  // CHECK: [[C:%[0-9]+]] = load [[CBOX]]#1
  // CHECK: [[CLASS:%[0-9]+]] = class_metatype $SomeClass.metatype, [[C]]
  // CHECK: [[S:%[0-9]+]] = load [[SBOX]]#1
  // CHECK: [[SUBCLASS:%[0-9]+]] = class_metatype $SomeSubclass.metatype, [[S]]
  // CHECK: [[SUBCLASS_UPCAST:%[0-9]+]] = upcast [[SUBCLASS]], $SomeClass.metatype
  // CHECK: tuple ([[CLASS]], [[SUBCLASS_UPCAST]])
  return (c.metatype, s.metatype)
}
