// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

import swift

// CHECK: define void @_T7foreach12foreach_testFT1rNSs5Range_T_(
func foreach_test(r : Range) {
  var sum : Int = 0
  // CHECK: call { i64, i64 } @_TNSs5Range11getElementsfRS_FT_S_
  // CHECK: br label
  // CHECK: call i1 @_TNSs5Range5emptyfRS_FT_NSs4Bool
  // CHECK: call i1 @_TNSs4Bool13getLogicValuefRS_FT_i1
  // CHECK-NEXT: [[RESULT:%[A-Za-z0-9.]+]] = xor
  // CHECK: br i1 [[RESULT]], label
  foreach i in r {
    // CHECK: call i64 @_TNSs5Range8getFirstfRS_FT_NSs5Int64
    // CHECK: call i64 @_TSsop1pFT3lhsNSs5Int643rhsS__S_
    sum = sum + i
    // CHECK: call void @_TNSs5Range9dropFirstfRS_FT_T_
    // CHECK-NEXT: br label
  }

  // CHECK: ret void
}