// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

import swift

// CHECK: define void @_T4meta2t0FT_T_()
// CHECK-NEXT: :
// CHECK-NEXT: ret void
func t0() {
  var x = swift
}

// CHECK: define void @_T4meta2t1FT_T_()
// CHECK-NEXT: :
// CHECK-NEXT: ret void
func t1() {
  var x = int
}