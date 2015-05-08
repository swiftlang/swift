// RUN: %target-swift-frontend %s -g -emit-ir -o - | FileCheck %s

// Ensure that the debug info we're emitting passes the back end verifier.
// RUN: %target-swift-frontend %s -g -S -o - | FileCheck %s --check-prefix ASM-%target-object-format
// ASM-macho: .section __DWARF,__debug_info
// ASM-elf: .section .debug_info,"",@progbits

// Test variables-interpreter.swift runs this code with `swift -g -i`.
// Test variables-repl.swift runs this code with `swift -g < variables.swift`.

// CHECK-DAG: ![[TLC:.*]] = !MDModule(name: "variables"

// Global variables.
var glob_i8:   Int8 = 8;
// CHECK-DAG: !DIGlobalVariable(name: "glob_i8",{{.*}} scope: ![[TLC]],{{.*}} line: [[@LINE-1]],{{.*}} type: ![[I8:[^,]+]]
var glob_i16:  Int16 = 16;
// CHECK-DAG: !DIGlobalVariable(name: "glob_i16",{{.*}} scope: ![[TLC]],{{.*}} line: [[@LINE-1]],{{.*}} type: ![[I16:[^,]+]]
var glob_i32:  Int32 = 32;
// CHECK-DAG: !DIGlobalVariable(name: "glob_i32",{{.*}} scope: ![[TLC]],{{.*}} line: [[@LINE-1]],{{.*}} type: ![[I32:[^,]+]]
var glob_i64:  Int64 = 64;
// CHECK-DAG: !DIGlobalVariable(name: "glob_i64",{{.*}} scope: ![[TLC]],{{.*}} line: [[@LINE-1]],{{.*}} type: ![[I64:[^,]+]]
var glob_f:    Float = 2.89;
// CHECK-DAG: !DIGlobalVariable(name: "glob_f",{{.*}} scope: ![[TLC]],{{.*}} line: [[@LINE-1]],{{.*}} type: ![[F:[^,]+]]
var glob_d:    Double = 3.14;
// CHECK-DAG: !DIGlobalVariable(name: "glob_d",{{.*}} scope: ![[TLC]],{{.*}} line: [[@LINE-1]],{{.*}} type: ![[D:[^,]+]]
var glob_b:    Bool = true
// CHECK-DAG: !DIGlobalVariable(name: "glob_b",{{.*}} scope: ![[TLC]],{{.*}} line: [[@LINE-1]],{{.*}} type: ![[B:[^,]+]]
var glob_s:    String = "😄"
// CHECK-DAG: !DIGlobalVariable(name: "glob_s",{{.*}} scope: ![[TLC]],{{.*}} line: [[@LINE-1]],{{.*}} type: ![[S:[^,]+]]
// FIXME: Dreadful type-checker performance prevents this from being this single
// print expression:
//   print("\(glob_v), \(glob_i8), \(glob_i16), \(glob_i32), \(glob_i64), \(glob_f), \(glob_d), \(glob_b), \(glob_s)", appendNewline: false)
print(", \(glob_i8)", appendNewline: false)
print(", \(glob_i16)", appendNewline: false)
print(", \(glob_i32)", appendNewline: false)
print(", \(glob_i64)", appendNewline: false)
print(", \(glob_f)", appendNewline: false)
print(", \(glob_d)", appendNewline: false)
print(", \(glob_b)", appendNewline: false)
print(", \(glob_s)", appendNewline: false)
var unused: Int32 = -1

// CHECK-DAG: ![[RT:[0-9]+]] ={{.*}}"Swift.swiftmodule"


// Stack variables.
func foo(dt: Float) -> Float {
  // CHECK-DAG: call void @llvm.dbg.declare
  // CHECK-DAG: !DILocalVariable(tag: DW_TAG_auto_variable, name: "f"
  var f: Float = 9.78;
  // CHECK-DAG: !DILocalVariable(tag: DW_TAG_auto_variable, name: "r"
  var r: Float = f*dt;
  return r;
}

var g = foo(1.0);

// Tuple types.
var tuple: (Int, Bool) = (1, true)
// CHECK-DAG: !DIGlobalVariable(name: "tuple", linkageName: "_Tv{{9variables|4main}}5tupleTSiSb_",{{.*}} type: ![[TUPTY:[^,)]+]]
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type,{{.*}} elements: ![[ELEMS:[0-9]+]],{{.*}} identifier: [[TUPTY]]
// CHECK-DAG: ![[ELEMS]] = !{![[MI64:[0-9]+]], ![[MB:[0-9]+]]}
// CHECK-DAG: ![[MI64]] = !DIDerivedType(tag: DW_TAG_member,{{.*}} baseType: !"_TtSi"
// CHECK-DAG: ![[MB]] = !DIDerivedType(tag: DW_TAG_member,{{.*}} baseType: ![[B]]
func myprint(p: (i: Int, b: Bool)) {
     print("\(p.i) -> \(p.b)")
}



myprint(tuple)

// Arrays are represented as an instantiation of Array.
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "Array",{{.*}} identifier: [[Array:"[^"]+"]]
// CHECK-DAG: !DIGlobalVariable(name: "array_of_tuples",{{.*}} type: ![[Array]]
var array_of_tuples : [(a : Int, b : Int)] = [(1,2)]
var twod : [[Int]] = [[1]]

func bar( x: [(a : Int, b : Int)], y: [[Int]] ) {
}


// CHECK-DAG: !DIGlobalVariable(name: "P",{{.*}} type: ![[PTY:[0-9]+]]
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "_TtT1xSd1ySd1zSd_",{{.*}} identifier: [[PTUP:[^,)]+]]
// CHECK-DAG: ![[PTY]] = !DIDerivedType(tag: DW_TAG_typedef, name: "_Tta{{9variables|4main}}5Point",{{.*}} baseType: ![[PTUP]]
typealias Point = (x: Double, y: Double, z: Double)
var P:Point = (1, 2, 3)
func myprint(p: (x: Double, y: Double, z: Double)) {
     print("(\(p.x), \(p.y), \(p.z))")
}
myprint(P)

// CHECK-DAG: !DIGlobalVariable(name: "P2",{{.*}} type: ![[APTY:[0-9]+]]
// CHECK-DAG: ![[APTY]] = !DIDerivedType(tag: DW_TAG_typedef, name: "_Tta{{9variables|4main}}13AliasForPoint",{{.*}} baseType: ![[PTY:[0-9]+]]
typealias AliasForPoint = Point
var P2:AliasForPoint = (4, 5, 6)
myprint(P2)

// Unions.
enum TriValue {
  case false_
  case true_
  case top
}
// CHECK-DAG: !DIGlobalVariable(name: "unknown",{{.*}} type: !"_TtO{{9variables|4main}}8TriValue"
// CHECK-DAG: !DICompositeType(tag: DW_TAG_union_type, name: "TriValue", {{.*}}identifier: "_TtO{{9variables|4main}}8TriValue"
var unknown = TriValue.top
func myprint(value: TriValue) {
     switch value {
     case TriValue.false_: print("false")
     case TriValue.true_:  print("true")
     case TriValue.top:   print("⊤")
     }
}
myprint(unknown)

// CHECK-DAG: !DIFile(filename: "variables.swift"
