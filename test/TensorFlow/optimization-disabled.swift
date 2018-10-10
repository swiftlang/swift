// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Onone -emit-sil -Xllvm -tf-module-level-graph=false -verify %s | %FileCheck %s
import TensorFlow

public func testArrayValues() -> Tensor<Float> {
  let x: Tensor<Float> = [[1, 2], [3, 4]]
  return (matmul(x, x) + x).toHost()
}

/*
CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testArrayValues
CHECK: %0 = graph_op "Const"() {dtype$dtype: i32 1, value$tensor: f32 0x3F800000 /* 1 */, __device: "ALL_DEVICES"} : $TensorHandle<Float>
CHECK: %1 = graph_op "Const"() {dtype$dtype: i32 1, value$tensor: f32 0x40000000 /* 2 */
CHECK-LABEL: ----
*/

// The failing test case from https://bugs.swift.org/browse/SR-8426
public func testSendsInALoopGPU() {
  TensorFlow.enableGPU()
  let maxCount = 10
  // a cannot be an integer tensor due to a TensorFlow Eigen bug (b/77737504).
  var a = Tensor<Float>(1)
  var count = 1

  while count < maxCount {
    a += a    // expected-warning  {{implicitly copied to the host}}
    // One send.
    _hostOp(a.toHost())
    count += 1
  }
  a += a
  let _ = a.array
}
// CHECK-LABEL: --- TFDevicePartition Cross Device Tensor Transfer Annotation Result: {{.*}}testSendsInALoopGPU{{.*}}
// There are bunch of sends and receives that happen with Onone
// then sends it to GPU.
// CHECK:  bb1:
// CHECK:      [[A:%.*]] = graph_op "tfc.RecvFromHost
// CHECK:      graph_op "tfc.TensorTransfer"([[A]]
//
// Sends/Receives/Transfers correspond to the warnings at 'a += a' within the loop body
// CHECK:   bb3:
// CHECK-NOT:  graph_op "tfc.RecvFromHost
// CHECK:      graph_op "tfc.TensorTransfer
// CHECK:      graph_op "tfc.TensorTransfer
// CHECK:      graph_op "tfc.SendToHost
// Send/Receives/Transfers correspond to warnings after the loop.
// CHECK:  bb4:
// CHECK-NOT:  graph_op "tfc.RecvFromHost
// CHECK-NOT:  graph_op "tfc.TensorTransfer
// CHECK:      } // end sil function
