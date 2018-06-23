// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -verify -verify-ignore-unknown | %FileCheck %s

import TensorFlow

@inline(never)
func hostCode(_ x: Tensor<Float>) -> Tensor<Float> {
  return x.toDevice()
}

@TensorFlowGraph
public func send(_ x: Tensor<Float>) -> Tensor<Float> {
  let y = x + x
  // TODO(rxwei): Expected error but unknown location "host code is not allowed
  // in a @convention(tensorflow) function"
  let z = hostCode(y).toDevice()
  return z + z
}
