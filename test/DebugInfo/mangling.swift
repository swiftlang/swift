// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

// Type:
// Swift.Dictionary<Swift.Int64, Swift.String>

func markUsed<T>(_ t: T) {}

// Variable:
// mangling.myDict : Swift.Dictionary<Swift.Int64, Swift.String>
// CHECK: !DIGlobalVariable(name: "myDict",
// CHECK-SAME:       linkageName: "$S8mangling6myDictSDys5Int64VSSGvp",
// CHECK-SAME:              line: [[@LINE+3]]
// CHECK-SAME:              type: ![[DT:[0-9]+]]
// CHECK: ![[DT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Dictionary"
var myDict = Dictionary<Int64, String>()
myDict[12] = "Hello!"

// mangling.myTuple1 : (Name : Swift.String, Id : Swift.Int64)
// CHECK: !DIGlobalVariable(name: "myTuple1",
// CHECK-SAME:       linkageName: "$S8mangling8myTuple1SS4Name_s5Int64V2Idtvp",
// CHECK-SAME:              line: [[@LINE+3]]
// CHECK-SAME:              type: ![[TT1:[0-9]+]]
// CHECK: ![[TT1]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$SSS4Name_s5Int64V2IdtD"
var myTuple1 : (Name: String, Id: Int64) = ("A", 1)
// mangling.myTuple2 : (Swift.String, Id : Swift.Int64)
// CHECK: !DIGlobalVariable(name: "myTuple2",
// CHECK-SAME:       linkageName: "$S8mangling8myTuple2SS_s5Int64V2Idtvp",
// CHECK-SAME:              line: [[@LINE+3]]
// CHECK-SAME:              type: ![[TT2:[0-9]+]]
// CHECK: ![[TT2]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$SSS_s5Int64V2IdtD"
var myTuple2 : (      String, Id: Int64) = ("B", 2)
// mangling.myTuple3 : (Swift.String, Swift.Int64)
// CHECK: !DIGlobalVariable(name: "myTuple3",
// CHECK-SAME:       linkageName: "$S8mangling8myTuple3SS_s5Int64Vtvp",
// CHECK-SAME:              line: [[@LINE+3]]
// CHECK-SAME:              type: ![[TT3:[0-9]+]]
// CHECK: ![[TT3]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$SSS_s5Int64VtD"
var myTuple3 : (      String,     Int64) = ("C", 3)

markUsed(myTuple1.Id)
markUsed(myTuple2.Id)
markUsed({ $0.1 }(myTuple3))
