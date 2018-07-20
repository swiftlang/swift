// RUN: %swiftc_driver %s -g -debug-info-format=codeview -emit-ir -o - | %FileCheck %s

func foo() {
  var myDictionary: [Int64 : Float] = [1729: 2.718]
}

// CHECK-DAG: ![[INT64:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int64"
// CHECK-DAG: ![[INT64PTR:[0-9]+]] = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: ![[INT64]], size: 64)
// CHECK-DAG: ![[FLOAT:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Float"
// CHECK-DAG: ![[FLOATPTR:[0-9]+]] = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: ![[FLOAT]], size: 64)

// CHECK-DAG: ![[INT64BASE:[0-9]+]] = !DIBasicType(name: "$SBi64_D", size: 64, encoding: DW_ATE_unsigned)
// CHECK-DAG: ![[INT64BASEPTR:[0-9]+]] = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: ![[INT64BASE]], size: 64)

// CHECK-DAG: !DILocalVariable(name: "myDictionary",{{.*}} type: ![[DICTIONARY:[0-9]+]])
// CHECK-DAG: ![[DICTIONARY]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Dictionary<Int64,Float>",{{.*}} size: 64
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "_storage",{{.*}}, baseType: ![[STORAGEPTR:[0-9]+]], size: 64)
// CHECK-DAG: ![[STORAGEPTR]] = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: ![[STORAGETYPE:[0-9]+]], size: 64)

// CHECK-DAG: ![[STORAGETYPE]] = !DICompositeType(tag: DW_TAG_structure_type, name: "_RawNativeDictionaryStorage<Int64,Float>",{{.*}} size: 576
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "bucketCount",{{.*}} baseType: ![[INT64BASE]], size: 64, offset: 128)
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "count",{{.*}} baseType: ![[INT64BASE]], size: 64, offset: 192)
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "initializedEntries",{{.*}} baseType: ![[INT64BASEPTR]], size: 64, offset: 256)
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "keys",{{.*}} baseType: ![[INT64PTR]], size: 64, offset: 384)
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "values",{{.*}} baseType: ![[FLOATPTR]], size: 64, offset: 448)
