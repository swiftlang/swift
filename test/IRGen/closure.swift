// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

import swift

func a(i : int) -> int { 
  var f : (int,int) -> int = { $1 }
  return f(i,i)
}
// CHECK: define i64 @_T7closure1aFT1iNSs5int64_S0_(i64) {
// CHECK: %f = alloca { i8*, i8* }, align 8
// CHECK: [[CLOSUREPTRPTR:%.*]] = getelementptr inbounds { i8*, i8* }* %f, i32 0, i32 0
// CHECK: [[CLOSUREPTR:%.*]] = load i8** [[CLOSUREPTRPTR]]
// CHECK: [[CASTCLOSUREPTR:%.*]] = bitcast i8* [[CLOSUREPTR]] to i64 (i64, i64, i8*)*
// CHECK: call i64 [[CASTCLOSUREPTR]]
// CHECK: define internal i64 @closure(i64, i64, i8*) {
// CHECK: %"$0" = alloca %_TSs5int64
// CHECK: %"$1" = alloca %_TSs5int64
