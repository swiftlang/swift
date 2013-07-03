// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s

// Global variables.
var glob_v:    Void;
var glob_i8:   Int8 = 8;
// CHECK-DAG: i32 [[@LINE-1]], metadata ![[I8:[0-9]+]]{{.*}}DW_TAG_variable{{.*}}glob_i8
var glob_i16:  Int16 = 16;
// CHECK-DAG: i32 [[@LINE-1]], metadata ![[I16:[0-9]+]]{{.*}}DW_TAG_variable{{.*}}glob_i16
var glob_i32:  Int32 = 32;
// CHECK-DAG: i32 [[@LINE-1]], metadata ![[I32:[0-9]+]]{{.*}}DW_TAG_variable{{.*}}glob_i32
var glob_i64:  Int64 = 64;
// CHECK-DAG: i32 [[@LINE-1]], metadata ![[I64:[0-9]+]]{{.*}}DW_TAG_variable{{.*}}glob_i64
var glob_i128: Int128 = 128;
// CHECK-DAG: i32 [[@LINE-1]], metadata ![[I128:[0-9]+]]{{.*}}DW_TAG_variable{{.*}}glob_i128
var glob_f:    Float = 2.89;
// CHECK-DAG: i32 [[@LINE-1]], metadata ![[F:[0-9]+]]{{.*}}DW_TAG_variable{{.*}}glob_f
var glob_d:    Double = 3.14;
// CHECK-DAG: i32 [[@LINE-1]], metadata ![[D:[0-9]+]]{{.*}}DW_TAG_variable{{.*}}glob_d
var glob_b:    Bool = true
// CHECK-DAG: i32 [[@LINE-1]], metadata ![[B:[0-9]+]]{{.*}}DW_TAG_variable{{.*}}glob_b
var glob_s:    String = "😄"
// CHECK-DAG: i32 [[@LINE-1]], metadata ![[S:[0-9]+]]{{.*}}DW_TAG_variable{{.*}}glob_s
print("\(glob_v), \(glob_i8), \(glob_i16), \(glob_i32), \(glob_i64), \(glob_i128), \(glob_f), \(glob_d), \(glob_b), \(glob_s)")
var unused: Int32 = -1
// CHECK-DAG: ![[RT:[0-9]+]] ={{.*}}"swift.swift"
// CHECK-DAG: ![[I8]] ={{.*}}metadata ![[RT]]{{.*}}[ DW_TAG_structure_type ] [Int8]
// CHECK-DAG: ![[I16]] ={{.*}}metadata ![[RT]]{{.*}}[ DW_TAG_structure_type ] [Int16]
// CHECK-DAG: ![[I32]] ={{.*}}metadata ![[RT]]{{.*}}[ DW_TAG_structure_type ] [Int32]
// CHECK-DAG: ![[I64]] ={{.*}}metadata ![[RT]]{{.*}}[ DW_TAG_structure_type ] [Int64]
// CHECK-DAG: ![[I128]] ={{.*}}metadata ![[RT]]{{.*}}[ DW_TAG_structure_type ] [Int128]
// CHECK-DAG: ![[F]] ={{.*}}metadata ![[RT]]{{.*}}[ DW_TAG_structure_type ] [Float32]
// CHECK-DAG: ![[D]] ={{.*}}metadata ![[RT]]{{.*}}[ DW_TAG_structure_type ] [Float64]
// CHECK-DAG: ![[S]] ={{.*}}metadata ![[RT]]{{.*}}[ DW_TAG_structure_type ] [String]

// Stack variables.
func foo(dt: Float) -> Float {
  // CHECK-DAG: call void @llvm.dbg.declare
  // CHECK-DAG: [ DW_TAG_auto_variable ] [f] [line [[@LINE+1]]]
  var f: Float = 9.78;
  // CHECK-DAG: [ DW_TAG_auto_variable ] [r] [line [[@LINE+1]]]
  var r: Float = f*dt;
  return r;
}
