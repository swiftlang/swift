// RUN: %target-swift-frontend -Xllvm -tf-dynamic-compilation=false -Xllvm -tf-dump-intermediates -emit-sil %s | %FileCheck %s

import TensorFlow
import CTensorFlow

public func basicDebugValues(_ x: Tensor<Float>) {
  let y = x + 1
  let z = y.squared()
}

// FIXME: `debug_value_addr` for `z` is not currently preserved due to SSA promotion in deabstraction.
public func debugValuesInLoop(_ x: Tensor<Float>, _ n: Int) {
  var z = x.squared()
  for i in 0..<n {
    z += x
  }
}

// Opaque handles should not trigger send/receive even at -Onone, so we won't
// expect their `debug_value` to work.
public func noCopyForOpaqueHandles() {
  let values = Tensor<Float>([1.0])
  let dataset: VariantHandle =
    #tfop("TensorSliceDataset", [values],
          Toutput_types$dtype: [Float.tensorFlowDataType], output_shapes: [TensorShape()])
}

// CHECK-LABEL: // basicDebugValues(_:)
// CHECK-NEXT: sil @{{.*}}basicDebugValues{{.*}} {
// CHECK: debug_value %{{.*}} : $Tensor<Float>, let, name "x", argno 1
// CHECK: debug_value %{{.*}} : $Tensor<Float>, let, name "y"
// CHECK: debug_value %{{.*}} : $Tensor<Float>, let, name "z"



// CHECK-LABEL: // debugValuesInLoop(_:_:)
// CHECK-NEXT: sil @{{.*}}debugValuesInLoop{{.*}} {
// CHECK: bb0(%0 : $Tensor<Float>, %1 : $Int):
// CHECK:   debug_value %{{.*}} : $Tensor<Float>, let, name "x", argno 1
// CHECK:   debug_value %{{.*}} : $Int, let, name "i"
