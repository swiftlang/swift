// RUN: %target-swift-frontend -primary-file %s  -parse-as-library -emit-sil -O | FileCheck %s

private func recFunc(x: Int) -> Int {
  if x > 0 {
    return recFunc(x - 1)
  }
  return 0
}

// Ensure that we do not inline self-recursive functions into other
// functions since doing so can result in large code growth if we run
// the inlining pass multiple times.

// CHECK-LABEL: sil hidden @_TF16inline_recursive6callitFT_Si
// CHECK: bb0:
// CHECK: [[FN:%.*]] = function_ref @_TF16inline_recursiveP33_38E63D320CFF538A1F98BBC31453B1EB7recFuncFSiSi
// CHECK: [[BUILTIN_INT:%.*]] = integer_literal $Builtin.Int64, 3
// CHECK: [[INT:%.*]] = struct $Int ([[BUILTIN_INT]] : $Builtin.Int64)
// CHECK: [[APPLY:%.*]] = apply [[FN]]([[INT]])
// CHECK: return [[APPLY]]
func callit() -> Int {
  return recFunc(3)
}
