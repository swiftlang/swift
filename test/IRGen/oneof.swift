// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

import swift

// CHECK: [[TY0:%.*]] = type { i1, [15 x i8] }
// CHECK: [[INT:%.*]] = type { { i64 } }

oneof Ty0 {
  x : int,
  y
}

func f0(t0 : Ty0) -> int {
  return 0
}
// CHECK: define i64 @_T5oneof2f0FT2t0NS_3Ty0_NSs5int64([[TY0]]*) {

func f1() -> Ty0 {
  return :x(0)
}
// CHECK:    define void @_T5oneof2f1FT_NS_3Ty0([[TY0]]*) {
// CHECK:      call void @_T5oneof3Ty01xFNSs5int64S0_([[TY0]]* noalias sret {{%.*}}, i64 0)
// CHECK-NEXT: ret void
