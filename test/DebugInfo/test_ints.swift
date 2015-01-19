// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// These two should not have the same type.
// CHECK: _TtVSs5Int64"} ; [ DW_TAG_structure_type ] [Int64] [line 0, size 64, align 64, offset 0] [def] [from ]
// CHECK: !"_TtVSs5Int64"{{.*}}} ; [ DW_TAG_variable ] [a] [line [[@LINE+1]]] [def]
var a : Int64 = 2

// CHECK: !"_TtSi"{{.*}}} ; [ DW_TAG_variable ] [b] [line [[@LINE+1]]] [def]
var b = 2

