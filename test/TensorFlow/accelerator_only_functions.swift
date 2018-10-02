// RUN: %target-swift-frontend -Xllvm -tf-dynamic-compilation -Xllvm -tf-dump-graph -Xllvm -tf-module-level-graph=false -O -emit-sil %s | %FileCheck %s

import TensorFlow

@TensorFlowGraph
public func add(x: Tensor<Float>, y: Tensor<Float>) -> Tensor<Float> {
  return x + y
}

// CHECK-LABEL: --- TFPartition GraphDef Proto:
// CHECK-NEXT: library {
// CHECK-NEXT:   function {
// CHECK-NEXT:     signature {
// CHECK-NEXT:       name: "{{.*}}add{{.*}}.tf_only"

@TensorFlowGraph
public func loop(x: Tensor<Float>) -> Tensor<Float> {
  var y = x + 1
  for i in 1...10 {
    y += y
  }
  return y
}

// CHECK-LABEL: --- TFPartition GraphDef Proto:
// CHECK-NEXT: library {
// CHECK-NEXT:   function {
// CHECK-NEXT:     signature {
// CHECK-NEXT:       name: "{{.*}}loop{{.*}}.tf_only"
