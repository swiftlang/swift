// RUN: %target-swift-frontend -Xllvm -tf-dynamic-compilation=false -Xllvm -tf-dump-intermediates -emit-sil -O %s | %FileCheck %s
// REQUIRES: deprecated_gpe_mode

import TensorFlow

@_optimize(none)
public func basicDebugValues(_ x: Tensor<Float>) {
  let y = x + 1
  let z = y.squared()
}

// FIXME: `debug_value_addr` for `z` is not currently preserved due to SSA promotion in deabstraction.
@_optimize(none)
public func debugValuesInLoop(_ x: Tensor<Float>, _ n: Int) {
  var z = x.squared()
  for i in 0..<n {
    z += x
  }
}

// Opaque handles should not trigger send/receive even at -Onone, so we won't
// expect their `debug_value` to work.
@_optimize(none)
public func noCopyForOpaqueHandles() {
  let values = Tensor<Float>([1.0])
  let dataset: VariantHandle =
    #tfop("TensorSliceDataset", [values],
          Toutput_types$dtype: [Float.tensorFlowDataType], output_shapes: [TensorShape()])
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}basicDebugValues
// CHECK: debug_value %{{.*}} : $Tensor<Float>, let, name "x", argno 1
// CHECK: debug_value %{{.*}} : $Tensor<Float>, let, name "y"
// CHECK: debug_value %{{.*}} : $Tensor<Float>, let, name "z"


// CHECK-LABEL: --- TFPartition Host Result: {{.*}}debugValuesInLoop{{.*}}
// CHECK: bb0(%0 : $Tensor<Float>):
// CHECK:   debug_value %{{.*}} : $Tensor<Float>, let, name "x", argno 1
// CHECK:   debug_value %{{.*}} : $Int, let, name "i"
