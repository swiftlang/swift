// RUN: %swift -target x86_64-apple-macosx10.9 %s -emit-ir -g -o - | FileCheck %s
// XFAIL: linux

// These two should not have the same type.
// CHECK: _TtVSs5Int64"} ; [ DW_TAG_structure_type ] [Int64] [line 0, size 64, align 64, offset 0] [def] [from ]
// CHECK: metadata !"_TtVSs5Int64"{{.*}}} ; [ DW_TAG_variable ] [a] [line [[@LINE+1]]] [def]
var a : Int64 = 2

// CHECK: metadata !"_TtSi"{{.*}}} ; [ DW_TAG_variable ] [b] [line [[@LINE+1]]] [def]
var b = 2

