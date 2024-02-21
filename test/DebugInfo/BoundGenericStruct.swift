// RUN: %target-swift-frontend %s -O -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -gdwarf-types -o - | %FileCheck %s --check-prefix=DWARF
public struct S<T> {
  let t : T
}

public let s = S<Int>(t: 0)

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$s18BoundGenericStruct1SVySiGD",
// CHECK-SAME:             templateParams: ![[PARAMS:[0-9]+]]
// CHECK: ![[PARAMS]] = !{![[INTPARAM:[0-9]+]]}
// CHECK: ![[INTPARAM]] = !DITemplateTypeParameter(type: ![[INT:[0-9]+]])
// CHECK: ![[INT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSiD",


// DWARF: !DICompositeType(tag: DW_TAG_structure_type, name: "$s18BoundGenericStruct1SVySiGD",
// DWARF-SAME:             templateParams: ![[PARAMS:[0-9]+]]
// DWARF: ![[PARAMS]] = !{![[INTPARAM:[0-9]+]]}
// DWARF: ![[INTPARAM]] = !DITemplateTypeParameter(type: ![[INT:[0-9]+]])
// DWARF: ![[INT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", {{.*}}identifier: "$sSiD"

// DWARF: !DICompositeType(tag: DW_TAG_structure_type, name: "S", 
// DWARF-SAME:             identifier: "$s18BoundGenericStruct1SVyxGD")
// DWARF: !DIDerivedType(tag: DW_TAG_member, name: "t"
// DWARF: !DICompositeType(tag: DW_TAG_structure_type, name: "$sxD"


