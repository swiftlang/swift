// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Onone -emit-sil -Xllvm -tf-strict-deabstraction -Xllvm -tf-module-level-graph=false -verify %s | %FileCheck %s
import TensorFlow

// expected-warning @+1 8 {{value implicitly copied to the host}}
public func testArrayValues() -> Tensor<Float> {
// expected-warning @+1 6 {{value implicitly copied to the host}}
  let x: Tensor<Float> = [[1, 2], [3, 4]]
  // expected-note @-1 8 {{value used here}}
  return (matmul(x, x) + x).toHost()
// expected-warning {{value implicitly copied to the host}}
}

/*
CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testArrayValues
CHECK: %0 = graph_op "Const"() {dtype: $Float, value$tensor: f32 0x3F800000 /* 1 */, __device: "ALL_DEVICES"} : $TensorHandle<Float>
CHECK: %1 = graph_op "tfc.SendToHost,i"(%0 : $TensorHandle<Float>) {tensorId: i32 0, __device: "/device:CPU:0"}
CHECK-NOT: tfc.RecvFromHost
CHECK-LABEL: ----
*/

