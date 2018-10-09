// RUN: %target-swift-frontend -emit-sil -Xllvm -tf-dump-graph %s
// RUN: %target-swift-frontend -emit-sil -Xllvm -tf-dump-graph %s | %FileCheck %s

// These tests are in separate files because functions appear in the GraphDef
// in nondeterministic order.

import TensorFlow

public func test() {
  let _: ResourceHandle = #tfop("AnonymousIterator",
                                output_types$dtype: [Float.tensorFlowDataType],
                                output_shapes: [TensorShape([])])
}

// CHECK-LABEL: key: "output_shapes"
// CHECK-NEXT: value {
// CHECK-NEXT:   list {
// CHECK-NEXT:     shape {
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT: }
