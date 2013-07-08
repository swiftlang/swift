// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s

// Verify that we generate appropriate names for accessors.
// CHECK: DW_TAG_subprogram{{.*}}x.get
// CHECK: DW_TAG_subprogram{{.*}}x.set

// Variable getter/setter
var _x : Int = 0
var x_modify_count : Int = 0
var x: Int {
    get:
      return _x
    set:
      x_modify_count = x_modify_count + 1
      _x = value
}
