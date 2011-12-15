// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

import swift

// CHECK: [[TY0:%.*]] = type { { [[INT:%.*]], {{%.*}} } }
// CHECK: [[INT]] = type { { i64 } }

struct Ty0 {
  x : int,
  y : int
}

func f0() -> Ty0 {
  return Ty0(4, 10)
}
// CHECK: define { i64, i64 } @_T6struct2f0FT_NS_3Ty0() {
// CHECK: call { i64, i64 } @_T6struct3Ty03Ty0FT1xNSs5int641yS1__S0_(i64 4, i64 10)