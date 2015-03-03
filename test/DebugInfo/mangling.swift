// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// Type:
// Swift.Dictionary<Swift.Int64, Swift.String>
// CHECK: !MDCompositeType(tag: DW_TAG_structure_type, name: "Dictionary",{{.*}} identifier: [[DT:[^,)]+]])
// CHECK: !MDCompositeType(tag: DW_TAG_structure_type, name: "_TtT4NameSS2IdSi_",{{.*}} identifier: [[TT1:[^,)]+]])
// CHECK: !MDCompositeType(tag: DW_TAG_structure_type, name: "_TtTSS2IdSi_",{{.*}} identifier: [[TT2:[^,)]+]])

// Variable:
// mangling.myDict : Swift.Dictionary<Swift.Int64, Swift.String>
// CHECK: !MDGlobalVariable(name: "myDict", linkageName: "_Tv8mangling6myDictGVSs10DictionarySiSS_",
// CHECK-SAME:              line: [[@LINE+2]]
// CHECK-SAME:              type: ![[DT]]
var myDict = Dictionary<Int, String>()
myDict[12] = "Hello!"

// mangling.myTuple1 : (Name : Swift.String, Id : Swift.Int64)
// CHECK: !MDGlobalVariable(name: "myTuple1", linkageName: "_Tv8mangling8myTuple1T4NameSS2IdSi_",
// CHECK-SAME:              line: [[@LINE+2]]
// CHECK-SAME:              type: ![[TT1]]
var myTuple1 : (Name: String, Id: Int) = ("A", 1)
// mangling.myTuple2 : (Swift.String, Id : Swift.Int64)
// CHECK: !MDGlobalVariable(name: "myTuple2", linkageName: "_Tv8mangling8myTuple2TSS2IdSi_",
// CHECK-SAME:              line: [[@LINE+2]]
// CHECK-SAME:              type: ![[TT2]]
var myTuple2 : (      String, Id: Int) = ("B", 2)
// mangling.myTuple3 : (Swift.String, Swift.Int64)

// FIXME: Pending <rdar://problem/16860038>
// FIXME: \00myTuple3\00_Tv8mangling8myTuple3TSSSi_\00[[@LINE+1]]\000\001"{{, [^,]+, [^,]+}}, metadata ![[TT3]], {{.*}} [ DW_TAG_variable ] [myTuple3]
// var myTuple3 : (      String,     Int) = ("C", 3)
// println({ $$0.1 }(myTuple3))

println(myTuple1.Id)
println(myTuple2.Id)

