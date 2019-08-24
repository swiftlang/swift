// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -emit-ir -gdwarf-types -o - \
// RUN:   | %FileCheck %s --check-prefix=DWARF

// Don't emit a line number for tuple types. They are unnamed
// and have no declaration, so the line number is nonsensical.
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$sSi_SftD"
// CHECK-NOT: line:
// CHECK-NEXT: =
let tuple1 : (Int, Float) = (1, 2.89)

// Tuple types.
var tuple2: (Double, Bool) = (1, true)
// DWARF: !DIGlobalVariable(name: "tuple2",{{.*}} type: ![[TUPTY:[^,)]+]]
// DWARF: ![[TUPTY]] = !DICompositeType(tag: DW_TAG_structure_type,
// DWARF-SAME:                          elements: ![[ELEMS:[0-9]+]]
// DWARF: ![[ELEMS]] = !{![[MD:[0-9]+]], ![[MB:[0-9]+]]}
// DWARF: ![[MD]] = !DIDerivedType(tag: DW_TAG_member
// DWARF-SAME:                     baseType: ![[DBL:[0-9]+]]
// DWARF: ![[DBL]] = !DICompositeType({{.*}}, name: "Double"
// DWARF: ![[MB]] = !DIDerivedType(tag: DW_TAG_member,
// DWARF-SAME:                     baseType: ![[B:[0-9]+]]
// DWARF: ![[B]] = !DICompositeType({{.*}}, name: "Bool"
func myprint(_ p: (i: Int, b: Bool)) {
     print("\(p.i) -> \(p.b)")
}
