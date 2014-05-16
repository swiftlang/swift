// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -g -o - | FileCheck %s

// Type:
// Swift.Dictionary<Swift.Int64, Swift.String>
// CHECK: null, null, metadata ![[DT:.*]]} ; [ DW_TAG_structure_type ] [Dictionary]
// CHECK: null, null, metadata ![[TT1:.*]]} ; [ DW_TAG_structure_type ] [_TtT4NameSS2IdSi_]
// CHECK: null, null, metadata ![[TT2:.*]]} ; [ DW_TAG_structure_type ] [_TtTSS2IdSi_]

// Variable:
// mangling.myDict : Swift.Dictionary<Swift.Int64, Swift.String>
// CHECK: metadata !"myDict", metadata !"_Tv8mangling6myDictGVSs10DictionarySiSS_", metadata !{{.*}}, i32 [[@LINE+1]], metadata ![[DT]], i32 0, {{.*}} [ DW_TAG_variable ] [myDict]
var myDict = Dictionary<Int, String>()
myDict[12] = "Hello!"

// mangling.myTuple1 : (Name : Swift.String, Id : Swift.Int64)
// CHECK: metadata !"myTuple1", metadata !"_Tv8mangling8myTuple1T4NameSS2IdSi_", metadata !{{.*}}, i32 [[@LINE+1]], metadata ![[TT1]], i32 0, {{.*}} [ DW_TAG_variable ] [myTuple1]
var myTuple1 : (Name: String, Id: Int) = ("A", 1)
// mangling.myTuple2 : (Swift.String, Id : Swift.Int64)
// CHECK: metadata !"myTuple2", metadata !"_Tv8mangling8myTuple2TSS2IdSi_", metadata !{{.*}}, i32 [[@LINE+1]], metadata ![[TT2]], i32 0, {{.*}} [ DW_TAG_variable ] [myTuple2]
var myTuple2 : (      String, Id: Int) = ("B", 2)
// mangling.myTuple3 : (Swift.String, Swift.Int64)

// FIXME: Pending <rdar://problem/16860038>
// FIXME: metadata !"myTuple3", metadata !"_Tv8mangling8myTuple3TSSSi_", metadata !{{.*}}, i32 [[@LINE+1]], metadata ![[TT3]], i32 0, {{.*}} [ DW_TAG_variable ] [myTuple3]
// var myTuple3 : (      String,     Int) = ("C", 3)
// println({ $$0.1 }(myTuple3))

println(myTuple1.Id)
println(myTuple2.Id)

