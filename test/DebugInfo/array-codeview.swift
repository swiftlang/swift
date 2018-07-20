// RUN: %swiftc_driver %s -g -debug-info-format=codeview -emit-ir -o - | %FileCheck %s

func foo() {
  var myArray: [Int64] = [101, 102, 103]
}

// CHECK-DAG: ![[INT64:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int64"
// CHECK-DAG: ![[INT64BASE:[0-9]+]] = !DIBasicType(name: "$SBi64_D", size: 64, encoding: DW_ATE_unsigned)

// CHECK-DAG: !DILocalVariable(name: "myArray",{{.*}} type: ![[ARRAY:[0-9]+]])
// CHECK-DAG: ![[ARRAY]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Array<Int64>",{{.*}} size: 64,
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "_storage",{{.*}} baseType: ![[ARRAYSTORAGEPTR:[0-9]+]], size: 64)
// CHECK-DAG: ![[ARRAYSTORAGEPTR]] = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: ![[ARRAYSTORAGE:[0-9]+]], size: 64)

// CHECK-DAG: ![[ARRAYSTORAGE]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Array<Int64>_storage",{{.*}} size: 256,
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "_count",{{.*}} baseType: ![[INT64BASE]], size: 64, offset: 128)
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "_firstElement",{{.*}} baseType: ![[INT64]], offset: 256)
