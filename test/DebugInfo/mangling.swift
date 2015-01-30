// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// XFAIL: linux

// Type:
// Swift.Dictionary<Swift.Int64, Swift.String>
// CHECK: ![[DT:[^,]+]]} ; [ DW_TAG_structure_type ] [Dictionary]
// CHECK: ![[TT1:[^,]+]]} ; [ DW_TAG_structure_type ] [_TtT4NameSS2IdSi_]
// CHECK: ![[TT2:[^,]+]]} ; [ DW_TAG_structure_type ] [_TtTSS2IdSi_]

// Variable:
// mangling.myDict : Swift.Dictionary<Swift.Int64, Swift.String>
// CHECK: \00myDict\00_Tv8mangling6myDictGVSs10DictionarySiSS_\00[[@LINE+1]]\000\001"{{, [^,]+, [^,]+}}, ![[DT]], {{.*}} [ DW_TAG_variable ] [myDict]
var myDict = Dictionary<Int, String>()
myDict[12] = "Hello!"

// mangling.myTuple1 : (Name : Swift.String, Id : Swift.Int64)
// CHECK: \00myTuple1\00_Tv8mangling8myTuple1T4NameSS2IdSi_\00[[@LINE+1]]\000\001"{{, [^,]+, [^,]+}}, ![[TT1]], {{.*}} [ DW_TAG_variable ] [myTuple1]
var myTuple1 : (Name: String, Id: Int) = ("A", 1)
// mangling.myTuple2 : (Swift.String, Id : Swift.Int64)
// CHECK: \00myTuple2\00_Tv8mangling8myTuple2TSS2IdSi_\00[[@LINE+1]]\000\001"{{, [^,]+, [^,]+}}, ![[TT2]], {{.*}} [ DW_TAG_variable ] [myTuple2]
var myTuple2 : (      String, Id: Int) = ("B", 2)
// mangling.myTuple3 : (Swift.String, Swift.Int64)

// FIXME: Pending <rdar://problem/16860038>
// FIXME: \00myTuple3\00_Tv8mangling8myTuple3TSSSi_\00[[@LINE+1]]\000\001"{{, [^,]+, [^,]+}}, metadata ![[TT3]], {{.*}} [ DW_TAG_variable ] [myTuple3]
// var myTuple3 : (      String,     Int) = ("C", 3)
// println({ $$0.1 }(myTuple3))

println(myTuple1.Id)
println(myTuple2.Id)

