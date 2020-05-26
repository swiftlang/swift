// RUN: %target-swift-frontend -primary-file %s  -parse-as-library -emit-sil -O | %FileCheck %s

private func recFunc(_ x: Int32) -> Int32 {
  if x > 0 {
    return recFunc(x - 1)
  }
  return 0
}

// CHECK-LABEL: sil {{.*}}callit
// CHECK: bb0:
// CHECK: [[INTLIT:%.*]] = integer_literal $Builtin.Int32, 3
// CHECK: [[STRUCT:%.*]] = struct $Int32 ([[INTLIT]] : $Builtin.Int32)
// CHECK: [[REF:%.*]] = function_ref @$s16inline_recursive7recFunc33_38E63D320CFF538A1F98BBC31453B1EBLLys5Int32VAEF
// CHECK: [[APPLY:%.*]] = apply [[REF]]([[STRUCT]])
// CHECK: return [[APPLY]]

func callit() -> Int32 {
  return recFunc(3)
}

private func recFuncManyCalls(_ x: Int32) -> Int32 {
  if x > 4 {
    return recFuncManyCalls(x - 1)
    + recFuncManyCalls(x - 2)
    + recFuncManyCalls(x - 3)
    + recFuncManyCalls(x - 4)
    + recFuncManyCalls(x - 5)
  }
  return 0
}

// CHECK-LABEL: sil hidden {{.*}}callother
// CHECK: bb0:
// CHECK: [[FN:%.*]] = function_ref {{.*}}recFuncManyCalls
// CHECK: [[APPLY:%.*]] = apply [[FN]]
// CHECK-NOT: apply
// CHECK: return [[APPLY]]
func callother() -> Int32 {
  return recFuncManyCalls(10)
}
