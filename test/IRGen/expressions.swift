// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

import swift


// CHECK: private unnamed_addr constant [20 x i8] c"this is just a test\00"

// CHECK: define i8* @_T11expressions17TestStringLiteralFT_NSs6String() {
// CHECK: call i8* @_TNSs6String24convertFromStringLiteralFT3valp_S_(i8* getelementptr inbounds ([20 x i8]* @0, i32 0, i32 0))

func TestStringLiteral() -> String { 
  return "this is just a test"
}
