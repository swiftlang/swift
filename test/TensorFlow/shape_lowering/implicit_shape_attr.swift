// RUN: %target-swift-frontend -emit-sil -Xllvm -tf-dump-graph %s
// RUN: %target-swift-frontend -emit-sil -Xllvm -tf-dump-graph %s | %FileCheck %s

// These tests are in separate files because functions appear in the GraphDef
// in nondeterministic order.

import TensorFlow

public func test() {
  let _ = Tensor<Float>([1, 2, 3])
}

// CHECK-LABEL: tensor_shape {
// CHECK-NEXT: dim {
// CHECK-NEXT:   size: 3
// CHECK-NEXT: }
