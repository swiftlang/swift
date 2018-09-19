// RUN: %target-swift-frontend -emit-ir -Xllvm -tf-dynamic-compilation -Xllvm -debug -Xllvm -debug-only -Xllvm irgensil %s 2>&1 | %FileCheck %s

import TensorFlow

class Dynamic {
  static let float = 1.0
}

public func unknownAttribute() {
  let x: TensorHandle<Float> = #tfop("Dummy1", value$tensor: Dynamic.float)
  _hostOp(x)
  // CHECK-LABEL: IRGen for graph_op: Dummy1
  // CHECK-NEXT: operand: value$tensor
  // CHECK-NEXT: end operands
}

public func knownAttribute() {
  let x: TensorHandle<Float> = #tfop("Dummy2", value$tensor: 1.0)
  _hostOp(x)
  // CHECK-LABEL: IRGen for graph_op: Dummy2
  // CHECK-NEXT: end operands
}
