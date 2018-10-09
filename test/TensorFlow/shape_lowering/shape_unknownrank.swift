// RUN: %target-swift-frontend -emit-sil -Xllvm -tf-dump-graph %s
// RUN: %target-swift-frontend -emit-sil -Xllvm -tf-dump-graph %s | %FileCheck %s

// FIXME: Our graph lowering doesn't handle ops with explicit shape attrs.
// XFAIL: *

// These tests are in separate files because functions appear in the GraphDef
// in nondeterministic order.

import TensorFlow

public func test() {
  let _: TensorHandle<Float> = #tfop("Placeholder",
                                     dtype$dtype: Float.tensorFlowDataType,
                                     shape: nil as TensorShape?)
}

// CHECK-LABEL: key: "shape"
// CHECK-NEXT: value {
// CHECK-NEXT:   list {
// CHECK-NEXT:     shape {
// CHECK-NEXT:       unknown_rank: true
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT: }
