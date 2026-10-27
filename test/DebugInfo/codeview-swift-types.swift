// RUN: %target-swift-frontend %s -parse-stdlib -module-name M -Onone -emit-ir -g -debug-info-format=codeview -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -parse-stdlib -module-name M -Onone -emit-ir -g -o - | %FileCheck %s --check-prefix=DWARF

public typealias MyInt = Builtin.Int64

@inline(never) public func use<T>(_ x: T) {}

public func f<T>(_ x: T, _ i: MyInt, _ p: Builtin.RawPointer) {
  use(x)
  use(i)
  use(p)
}

// CHECK-DAG: !DILocalVariable(name: "$\CF\84_0_0",{{.*}} type: ![[T:[0-9]+]]
// CHECK-DAG: ![[T]] = !DICompositeType(tag: DW_TAG_structure_type, name: "T",{{.*}} size: {{64|32}},{{.*}} identifier: "$sBpD")
// CHECK-DAG: !DILocalVariable(name: "i",{{.*}} type: ![[I:[0-9]+]]
// CHECK-DAG: ![[I]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[MYINT:[0-9]+]])
// CHECK-DAG: ![[MYINT]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s1M5MyIntaD",{{.*}} baseType: ![[INT64:[0-9]+]])
// CHECK-DAG: ![[INT64]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int64",{{.*}} size: 64,{{.*}} identifier: "$sBi64_D")
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "$sBi64_D",{{.*}} size: 64,{{.*}} identifier: "$sBi64_D")
// CHECK-DAG: !DILocalVariable(name: "p",{{.*}} type: ![[P:[0-9]+]]
// CHECK-DAG: ![[P]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[RAWPTR:[0-9]+]])
// CHECK-DAG: ![[RAWPTR]] = !DICompositeType(tag: DW_TAG_structure_type, name: "RawPointer",{{.*}} size: {{64|32}},{{.*}} identifier: "$sBpD")
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "$sBpD",{{.*}} size: {{64|32}},{{.*}} identifier: "$sBpD")

// DWARF-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "T",{{.*}} baseType: ![[PTR:[0-9]+]]
// DWARF-DAG: ![[PTR]] = !DIDerivedType(tag: DW_TAG_pointer_type, name: "$sBpD", baseType: null
// DWARF-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$s1M5MyIntaD",{{.*}} baseType: ![[INT64:[0-9]+]]
// DWARF-DAG: ![[INT64]] = !DIDerivedType(tag: DW_TAG_typedef, name: "Int64",{{.*}} baseType: ![[INT:[0-9]+]]
// DWARF-DAG: ![[INT]] = !DIBasicType(name: "$sBi64_D", size: 64
