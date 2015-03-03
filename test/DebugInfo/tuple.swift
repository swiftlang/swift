// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s
// Don't emit a line number for tuple types. They are unnamed
// and have no declaration, so the line number is nonsensical.
// CHECK: !MDCompositeType(tag: DW_TAG_structure_type, name: "_TtTSiSf_"
let tuple : (Int, Float) = (1, 2.89)
