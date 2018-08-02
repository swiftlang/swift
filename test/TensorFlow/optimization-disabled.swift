// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Onone -emit-sil -Xllvm -tf-module-level-graph=false -verify %s | %FileCheck %s
import TensorFlow

public func testArrayValues() -> Tensor<Float> {
  let x: Tensor<Float> = [[1, 2], [3, 4]]
  return (matmul(x, x) + x).toHost()
}

/*
CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testArrayValues
CHECK: %0 = graph_op "Const"() {dtype: $Float, value$tensor: f32 0x3F800000 /* 1 */, __device: "ALL_DEVICES"} : $TensorHandle<Float>
CHECK: %1 = graph_op "Const"() {dtype: $Float, value$tensor: f32 0x40000000 /* 2 */
CHECK-LABEL: ----
*/

