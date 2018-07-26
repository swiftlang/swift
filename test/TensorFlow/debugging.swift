// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -Xllvm -tf-strict-deabstraction %s  | %FileCheck %s

import TensorFlow

public func basicDebugValues(_ x: Tensor<Float>) {
  let y = x + 1
  let z = y.squared()
}

// CHECK-LABEL: @{{.*}}basicDebugValues{{.*}}
// CHECK: debug_value %{{.*}} : $Tensor<Float>, let, name "x", argno 1
//
// FIXME(SR-8375): Deabstraction is currently removing `debug_value` for `y`. 
// This should be fixed.
// HECK: debug_value %{{.*}} : $Tensor<Float>, let, name "y"
//
// CHECK: debug_value %{{.*}} : $Tensor<Float>, let, name "z"

