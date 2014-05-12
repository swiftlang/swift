// RUN: %target-build-swift %s -g -emit-ir -o - | FileCheck %s

// Ensure that the debug info we're emitting passes the back end verifier.
// RUN: %target-build-swift %s -g -S -o - | FileCheck %s --check-prefix ASM
// ASM: DWARF
//

// Test variables-interpreter.swift runs this code with `swift -g -i`.
// Test variables-repl.swift runs this code with `swift -g < variables.swift`.

// CHECK-DAG: ![[TLC:[0-9]+]] ={{.*}} metadata !"top_level_code",{{.*}} ; [ DW_TAG_subprogram ] [line 1] [def] [scope 0]

// Global variables.
var glob_i8:   Int8 = 8;
// CHECK-DAG: metadata ![[TLC]], metadata !"glob_i8", {{.*}}, i32 [[@LINE-1]], metadata ![[I8:.*]], i32 0{{.*}}DW_TAG_variable{{.*}}glob_i8
var glob_i16:  Int16 = 16;
// CHECK-DAG: metadata ![[TLC]], {{.*}}, i32 [[@LINE-1]], metadata ![[I16:.*]], i32 0{{.*}}DW_TAG_variable{{.*}}glob_i16
var glob_i32:  Int32 = 32;
// CHECK-DAG: metadata ![[TLC]], {{.*}}, i32 [[@LINE-1]], metadata ![[I32:.*]], i32 0{{.*}}DW_TAG_variable{{.*}}glob_i32
var glob_i64:  Int64 = 64;
// CHECK-DAG: metadata ![[TLC]], {{.*}}, i32 [[@LINE-1]], metadata ![[I64:.*]], i32 0{{.*}}DW_TAG_variable{{.*}}glob_i64
var glob_f:    Float = 2.89;
// CHECK-DAG: metadata ![[TLC]], {{.*}}, i32 [[@LINE-1]], metadata ![[F:.*]], i32 0{{.*}}DW_TAG_variable{{.*}}glob_f
var glob_d:    Double = 3.14;
// CHECK-DAG: metadata ![[TLC]], {{.*}}, i32 [[@LINE-1]], metadata ![[D:.*]], i32 0{{.*}}DW_TAG_variable{{.*}}glob_d
var glob_b:    Bool = true
// CHECK-DAG: metadata ![[TLC]], {{.*}}, i32 [[@LINE-1]], metadata ![[B:.*]], i32 0{{.*}}DW_TAG_variable{{.*}}glob_b
var glob_s:    String = "😄"
// CHECK-DAG: metadata ![[TLC]], {{.*}}, i32 [[@LINE-1]], metadata ![[S:.*]], i32 0{{.*}}DW_TAG_variable{{.*}}glob_s
// FIXME: Dreadful type-checker performance prevents this from being this single
// print expression:
//   print("\(glob_v), \(glob_i8), \(glob_i16), \(glob_i32), \(glob_i64), \(glob_f), \(glob_d), \(glob_b), \(glob_s)")
print(", \(glob_i8)")
print(", \(glob_i16)")
print(", \(glob_i32)")
print(", \(glob_i64)")
print(", \(glob_f)")
print(", \(glob_d)")
print(", \(glob_b)")
print(", \(glob_s)")
var unused: Int32 = -1

// CHECK-DAG: ![[RT:[0-9]+]] ={{.*}}"Swift.swiftmodule"


// Stack variables.
func foo(dt: Float) -> Float {
  // CHECK-DAG: call void @llvm.dbg.declare
  // CHECK-DAG: [ DW_TAG_auto_variable ] [f] [line
  var f: Float = 9.78;
  // CHECK-DAG: [ DW_TAG_auto_variable ] [r] [line
  var r: Float = f*dt;
  return r;
}

var g = foo(1.0);

// Tuple types.
var tuple: (Int, Bool) = (1, true)
// CHECK-DAG: "_Tv{{9variables|4main}}5tupleTSiSb_", metadata !{{[0-9]+}}, i32 [[@LINE-1]], metadata ![[TUPTY:.*]], i32 0{{.*}}[ DW_TAG_variable ] [tuple] [line [[@LINE-1]]] [def]
// CHECK-DAG: null, metadata ![[ELEMS:[0-9]+]], i32 {{[0-9]+}}, null, null, metadata ![[TUPTY]]}
// CHECK-DAG: ![[ELEMS]] = metadata !{metadata ![[MI64:[0-9]+]], metadata ![[MB:[0-9]+]]}
// CHECK-DAG: ![[MI64]] = {{.*}}[ DW_TAG_member ]{{.*}}[from _TtSi]
// CHECK-DAG: ![[MB]] = {{.*}}![[B]]} {{.*}}DW_TAG_member
func println(p: (i: Int, b: Bool)) {
     println("\(p.i) -> \(p.b)")
}



println(tuple)

// Arrays are represented as an instantiation of Array.
// CHECK-DAG: null, null, metadata ![[Array:.*]]} ; [ DW_TAG_structure_type ] [Array]
// CHECK-DAG: metadata ![[Array]], i32 0,{{.*}}[ DW_TAG_variable ] [array_of_tuples] [line [[@LINE+1]]] [def]
var array_of_tuples : (a : Int, b : Int)[] = [(1,2)]
var twod : Int[][] = [[1]]

func bar( x: (a : Int, b : Int)[], y: Int[][] ) {
}


// CHECK-DAG: i32 [[@LINE+4]], metadata ![[PTY:[0-9]+]], i32 0, i32 1,{{.*}} [ DW_TAG_variable ] [P] [line [[@LINE+4]]] [def]
// CHECK-DAG: null, null, metadata ![[PTUP:.*]]} ; [ DW_TAG_structure_type ] [_TtT1xSd1ySd1zSd_]
// CHECK-DAG: ![[PTY]] = metadata{{.*}}metadata ![[PTUP]]} ; [ DW_TAG_typedef ] [_Tta{{9variables|4main}}5Point] [line [[@LINE+1]], size 0, align 0, offset 0] [from _TtT1xSd1ySd1zSd_]
typealias Point = (x: Double, y: Double, z: Double)
var P:Point = (1, 2, 3)
func println(p: (x: Double, y: Double, z: Double)) {
     println("(\(p.x), \(p.y), \(p.z))")
}
println(P)

// CHECK-DAG: i32 [[@LINE+3]], metadata ![[APTY:[0-9]+]], i32 0, i32 1,{{.*}} [ DW_TAG_variable ] [P2] [line [[@LINE+3]]] [def]
// CHECK-DAG: ![[APTY]] = metadata{{.*}}metadata ![[PTY:[0-9]+]]} ; [ DW_TAG_typedef ] [_Tta{{9variables|4main}}13AliasForPoint] [line [[@LINE+1]], size 0, align 0, offset 0] [from _Tta{{9variables|4main}}5Point]
typealias AliasForPoint = Point
var P2:AliasForPoint = (4, 5, 6)
println(P2)

// Unions.
enum TriValue {
  case false
  case true
  case top
}
// CHECK-DAG: i32 [[@LINE+2]], metadata ![[UNIONTYPE:.*]], i32 0, {{.*}}[ DW_TAG_variable ] [unknown] [line [[@LINE+2]]] [def]
// CHECK-DAG: ![[UNIONTYPE]] ={{.*}}[ DW_TAG_union_type ] [_TtO{{9variables|4main}}8TriValue]
var unknown = TriValue.top
func println(value: TriValue) {
     switch value {
     case TriValue.false: println("false")
     case TriValue.true:  println("true")
     case TriValue.top:   println("⊤")
     }
}
println(unknown)

// CHECK-DAG: metadata !{i32 {{[0-9]+}}, metadata !{{[0-9]+}}, metadata ![[FILE:[0-9]+]], metadata !"", metadata !"", metadata !"top_level_code", {{.*}} [ DW_TAG_subprogram ] [line 1] [def]
// CHECK-DAG: [ DW_TAG_file_type ] [{{.*}}variables.swift]
