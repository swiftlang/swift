// RUN: %swiftc_driver %s -g -debug-info-format=codeview -emit-ir -o - | %FileCheck %s

func foo() {
  enum MyRawEnum { case Alpha, Beta } // Swift enum with no associated values.
  var myRawEnumVar: MyRawEnum = .Beta

  enum MyEnum { // Swift enum with associated values.
  case Power(Float)
  case Wisdom(Int64)
  case Courage(Float)
  }
  var myEnumVar: MyEnum = .Courage(1.1)
}

// CHECK-DAG: ![[INT8BASE:[0-9]+]] = !DIBasicType(name: "$SBi8_D", size: 8, encoding: DW_ATE_unsigned)
// CHECK-DAG: ![[INT64:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int64"
// CHECK-DAG: ![[FLOAT:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Float"

// CHECK-DAG: !DILocalVariable(name: "myRawEnumVar",{{.*}} type: ![[MYRAWENUM:[0-9]+]])
// CHECK-DAG: ![[MYRAWENUM]] = !DICompositeType(tag: DW_TAG_enumeration_type, name: "MyRawEnum",{{.*}} baseType: ![[INT8BASE]], size: 8, elements: ![[RAWENUMCASES:[0-9]+]],
// CHECK-DAG: ![[RAWENUMCASES]] = !{![[ALPHA:[0-9]+]], ![[BETA:[0-9]+]]}
// CHECK-DAG: ![[ALPHA]] = !DIEnumerator(name: "Alpha", value: 0)
// CHECK-DAG: ![[BETA]] = !DIEnumerator(name: "Beta", value: 1)

// CHECK-DAG: !DILocalVariable(name: "myEnumVar",{{.*}} type: ![[MYENUM:[0-9]+]])
// CHECK-DAG: ![[MYENUM]] = !DICompositeType(tag: DW_TAG_structure_type, name: "MyEnum",{{.*}} size: 72, elements: ![[ENUMELEMS:[0-9]+]],
// CHECK-DAG: ![[ENUMELEMS]] = !{![[VALUETYPE:[0-9]+]], ![[ASSOCIATEDVALUE:[0-9]+]]}

// CHECK-DAG: ![[VALUETYPE]] = !DIDerivedType(tag: DW_TAG_member, name: "Case",{{.*}} baseType: ![[ENUM:[0-9]+]], size: 8, offset: 64)
// CHECK-DAG: ![[ENUM]] = !DICompositeType(tag: DW_TAG_enumeration_type,{{.*}} baseType: ![[INT8BASE]], size: 8, elements: ![[ENUMCASES:[0-9]+]])
// CHECK-DAG: ![[ENUMCASES]] = !{![[PCASE:[0-9]+]], ![[WCASE:[0-9]+]], ![[CCASE:[0-9]+]]}
// CHECK-DAG: ![[PCASE]] = !DIEnumerator(name: "Power", value: 0)
// CHECK-DAG: ![[WCASE]] = !DIEnumerator(name: "Wisdom", value: 1)
// CHECK-DAG: ![[CCASE]] = !DIEnumerator(name: "Courage", value: 2)

// CHECK-DAG: ![[ASSOCIATEDVALUE]] = !DIDerivedType(tag: DW_TAG_member, name: "AssociatedValue",{{.*}} baseType: ![[UNION:[0-9]+]], size: 64)
// CHECK-DAG: ![[UNION]] = !DICompositeType(tag: DW_TAG_union_type,{{.*}} size: 64, elements: ![[ENUMVALUES:[0-9]+]],
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "Power",{{.*}} baseType: ![[FLOAT]], size: 32)
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "Wisdom",{{.*}} baseType: ![[INT64]], size: 64)
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "Courage",{{.*}} baseType: ![[FLOAT]], size: 32)
