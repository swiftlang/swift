// RUN: %target-swift-frontend -emit-sil -Xllvm -tf-dynamic-compilation -Xllvm -debug -Xllvm -debug-only -Xllvm irgensil %s 2>&1
// RUN: %target-swift-frontend -emit-ir -Xllvm -tf-dynamic-compilation -Xllvm -debug -Xllvm -debug-only -Xllvm irgensil %s 2>&1 | %FileCheck %s

import TensorFlow

class Dynamic {
  static let float = 1.0
}

public func unknownAttribute() {
  _ = #tfop("_DynamicOp", value$tensor: Dynamic.float) as TensorHandle<Float>
  // CHECK-LABEL: IRGen for graph_op: _DynamicOp
  // CHECK-NEXT: operand: value$tensor
  // CHECK-NEXT: end operands
}

public func knownAttribute() {
  _ = #tfop("_DynamicOp", value$tensor: 1.0) as TensorHandle<Float>
  // CHECK-LABEL: IRGen for graph_op: _DynamicOp
  // CHECK-NEXT: end operands
}
