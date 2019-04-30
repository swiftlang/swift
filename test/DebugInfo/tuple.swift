// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

// Don't emit a line number for tuple types. They are unnamed
// and have no declaration, so the line number is nonsensical.
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$sSi_SftD"
// CHECK-NOT: line:
// CHECK-NEXT: =
let tuple1 : (Int, Float) = (1, 2.89)

// Tuple types.
var tuple2: (Double, Bool) = (1, true)
func myprint(_ p: (i: Int, b: Bool)) {
     print("\(p.i) -> \(p.b)")
}
