// RUN: %swiftc_driver %s -g -debug-info-format=codeview -emit-ir -o - | %FileCheck %s

struct MyStruct { // NOTE: Classes do not work in Windows yet.
  init(_ arg: Int64) {
    // CHECK-DAG: !DILocalVariable(name: "self",{{.*}} type: ![[MYSTRUCT:[0-9]+]])
    // CHECK-DAG: ![[MYSTRUCT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "MyStruct",{{.*}} size: 128, elements: ![[MYSTRUCTELEMS:[0-9]+]],
    // CHECK-DAG: ![[MYSTRUCTELEMS]] = !{![[MYVAL:[0-9]+]], ![[MYBOOL:[0-9]+]], ![[MYFLOAT32:[0-9]+]]}
    self.myVal = arg
    // CHECK-DAG: !DILocalVariable(name: "arg",{{.*}} type: ![[INT64:[0-9]+]])
    // CHECK-DAG: ![[INT64]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int64",{{.*}} size: 64, elements:
  }
  var myVal: Int64
  // CHECK-DAG: ![[MYVAL]] = !DIDerivedType(tag: DW_TAG_member, name: "myVal",{{.*}} size: 64)
  // CHECK-DAG: ![[BASICINT64:[0-9]+]] = !DIBasicType(name: "$SBi64_D", size: 64, encoding: DW_ATE_unsigned)
  var myBool: Bool = false
  // CHECK-DAG: ![[MYBOOL]] = !DIDerivedType(tag: DW_TAG_member, name: "myBool",{{.*}} size: 8, offset: 64)
  // NOTE: CodeView only recognizes boolean variables with 8 bits.
  // CHECK-DAG: !DIBasicType(name: "$SBi1_D", size: 8, encoding: DW_ATE_boolean)
  var myFloat: Float32 = 1.234
  // CHECK-DAG: ![[MYFLOAT32]] = !DIDerivedType(tag: DW_TAG_member, name: "myFloat",{{.*}} baseType: ![[FLOAT32:[0-9]+]], size: 32, offset: 72)
  // CHECK-DAG: !DIBasicType(name: "$SBf32_D", size: 32, encoding: DW_ATE_float)
}
func useReference(_ inoutArg: inout Int64) {
  // CHECK-DAG: distinct !DISubprogram(name: "useReference",{{.*}} type: ![[USEREFTYPE:[0-9]+]],
  // CHECK-DAG: ![[USEREFTYPE]] = !DISubroutineType(types: ![[USEREFLIST:[0-9]+]])
  // CHECK-DAG: ![[USEREFLIST]] = !{!{{[0-9]+}}, ![[INT64REF:[0-9]+]]}
  // CHECK-DAG: ![[INT64REF]] = !DIDerivedType(tag: DW_TAG_reference_type, baseType: ![[INT64]], size: 64)
  inoutArg += 1
  // CHECK-DAG: ![[INOUTARG:[0-9]+]] = !DILocalVariable(name: "inoutArg", arg: 1,{{.*}} type: ![[INT64REF]])
  // CHECK-DAG: call void @llvm.dbg.declare(metadata {{.*}}, metadata ![[INOUTARG:[0-9]+]], metadata !DIExpression())
}
func foo() {
  var myTuple: (real: Double, imaginary: Double, String) = (1.1, 2.2, "number")
  // CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "(real: Swift.Double, imaginary: Swift.Double, Swift.String)",{{.*}} elements: ![[TUPLEELEMS:[0-9]+]],{{.*}} identifier: "$SSd4real_Sd9imaginarySStD"
  // CHECK-DAG: ![[TUPLEELEMS]] = !{![[REALTYPE:[0-9]+]], ![[IMAGINARYTYPE:[0-9]+]], ![[STRINGTYPE:[0-9]+]]}
  // CHECK-DAG: ![[REALTYPE]] = !DIDerivedType(tag: DW_TAG_member, name: "real",
  // CHECK-DAG: ![[IMAGINARYTYPE]] = !DIDerivedType(tag: DW_TAG_member, name: "imaginary",
  // NOTE: If the name of the element was not specified, use the element index.
  // CHECK-DAG: ![[STRINGTYPE]] = !DIDerivedType(tag: DW_TAG_member, name: "2",

  var myOptional: Int64? = nil
  // CHECK-DAG: !DILocalVariable(name: "myOptional",{{.*}} type: ![[OPTIONAL:[0-9]+]])
  // CHECK-DAG: ![[OPTIONAL]] = !DICompositeType(tag: DW_TAG_union_type, name: "Optional<Int64>",
  // CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "some",{{.*}} baseType: ![[INT64]], size: 64, align: 64)
  // FIXME: Optional enums with raw values are apparently a special case and
  //        do not render correctly in Visual Studio.
}

// TODO: Character: In LLDB, a Swift Character is shown as an integral type and
// the same behavior appears in WinDbg. I'm not sure if WinDbg can handle UTF-16
// characters but if it can it might be nice to figure out how to get WinDbg to
// display a Swift Character.

// TODO: Globals are not shown in WinDbg, but they are not shown in LLDB either.
// This is probably because they are declared as hidden in the LLVM-IR.
