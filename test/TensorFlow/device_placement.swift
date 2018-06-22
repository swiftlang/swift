// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -O -emit-sil -verify %s | %FileCheck %s

import TensorFlow

public func implicitDevicePlacement() {
  let x : Tensor<Float> = #tfop("Const", dtype: Float.self, value$tensor: 1.0)
  _hostOp(x)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}implicitDevicePlacement{{.*}}
// CHECK: string_literal utf8 "/device:CPU:0"
// CHECK: builtin "__tfop_Const,dtype,value$tensor,device

public func implicitDeviceConfig() {
  let x = Tensor<Float>(1.0)
  _hostOp(x)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}implicitDeviceConfig{{.*}}
// CHECK: string_literal utf8 "ALL_DEVICES"
// CHECK: builtin "__tfop_Const,dtype$dtype,value$tensor,device

public func explicitDeviceConfigGPU() {
  TensorFlow.enableGPU()
  let x = Tensor<Float>(1.0)
  _hostOp(x)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}explicitDeviceConfigGPU{{.*}}
// CHECK: string_literal utf8 "ALL_DEVICES"
// CHECK: builtin "__tfop_Const,dtype$dtype,value$tensor,device

// Check that in the TF graph, both the function node itself, and ops in the
// function, are placed on GPU.
//
// CHECK:      op: "{{.*}}explicitDeviceConfigGPU{{.*}}.tf_GPU.device_partition"
// CHECK-NEXT: device: "/device:GPU:0"

// CHECK:      library {
// CHECK-NEXT:   function {
// CHECK-NEXT:     signature {
// CHECK-NEXT:       name: "{{.*}}explicitDeviceConfigGPU{{.*}}.tf_GPU.device_partition"
// CHECK:          node_def {
// CHECK:            op: "Const"
// CHECK-NEXT:       device: "/device:GPU:0"

public func explicitDeviceConfigTPU() {
  TensorFlow.enableTPU()
  let x = Tensor<Float>(1.0)
  _hostOp(x)
}

// For TPU placement, the nodes in the graph function cannot have an explicit
// device attr.
// CHECK:          node_def {
// CHECK:            op: "Const"
// CHECK-NEXT:       attr {
// CHECK-NEXT:         key: "_tpu_replicate"

// This involves cross-device sends/recvs.
public func explicitDevicePlacementGPU() {
  let x : Tensor<Float> = #tfop("Const", dtype: Float.self, value$tensor: 1.0, device: "/device:GPU:0")
  _hostOp(x)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}explicitDevicePlacementGPU{{.*}}
// CHECK: string_literal utf8 "/device:GPU:0"
// CHECK: builtin "__tfop_Const,dtype,value$tensor,device

// CHECK-LABEL: --- TFDevicePartition Cross Device Tensor Transfer Annotation Result: {{.*}}explicitDevicePlacementGPU{{.*}}
// CHECK: builtin "__tfop_tfc.TensorTransfer

// CHECK-LABEL: --- TFDevicePartition Per-Device Function Extraction Result: {{.*}}explicitDevicePlacementGPU{{.*}}CPU{{.*}}
// CHECK: builtin "__tfop_tfc.D2DTensorRecv

// CHECK-LABEL: --- TFDevicePartition Per-Device Function Extraction Result: {{.*}}explicitDevicePlacementGPU{{.*}}GPU{{.*}}
// CHECK: builtin "__tfop_tfc.D2DTensorSend

// Check that in the TF graph, there is one function node for each of GPU and
// CPU. The GPU graph function has a send node, and the CPU one has a recv node.
//
// CHECK:      node {
// CHECK-NEXT:   name: "tfc_func_{{.*}}explicitDevicePlacementGPU{{.*}}.tf"
// CHECK-NEXT:   op: "{{.*}}explicitDevicePlacementGPU{{.*}}.tf_CPU.device_partition"
// CHECK-NEXT:   device: "/device:CPU:0"
// CHECK:      node {
// CHECK-NEXT:  name: "tfc_output_0_{{.*}}explicitDevicePlacementGPU{{.*}}.tf"
// CHECK-NEXT:  op: "Identity"
// CHECK-NEXT:  input: "tfc_func_{{.*}}explicitDevicePlacementGPU{{.*}}.tf"
// CHECK:      node {
// CHECK-NEXT:   name: "tfc_func_{{.*}}explicitDevicePlacementGPU{{.*}}.tf_helper_0"
// CHECK-NEXT:   op: "{{.*}}explicitDevicePlacementGPU{{.*}}.tf_GPU.device_partition"
// CHECK-NEXT:   device: "/device:GPU:0"
// CHECK:      library {
// CHECK:        function {
// CHECK-NEXT:     signature {
// CHECK:          name: "{{.*}}explicitDevicePlacementGPU{{.*}}.tf_CPU.device_partition"
// CHECK:          node_def {
// CHECK:            op: "_Recv"
// CHECK-NEXT:       device: "/device:CPU:0"

// Ideally we want to also check there is another graph function with name
// "{{.*}}explicitDevicePlacementGPU{{.*}}.tf_GPU.device_partition", and there
// is a an op "_Send" on device "/device:GPU:0". Unfortunately the serialized
// GraphDef does not give deterministic ordering on these two graph functions,
// causing the test to be flakey.
// Instead, we check on the _Send node in the next test.

public func explicitDevicePlacementAll() {
  let x : Tensor<Float> = #tfop("Const", dtype: Float.self, value$tensor: 1.0, device: "/device:GPU:0")
  // For GPU -> TPU transfer, always go through CPU first. Compiler can be
  // extended to generate this Identity op if needed.
  let x_cpu : Tensor<Float> = #tfop("Identity", x, __shapes: [TensorShape()], device: "/device:CPU:0")
  let y_cpu : Tensor<Float> = #tfop("Const", dtype: Float.self, value$tensor: 1.0, __shapes: [TensorShape()], device: "/device:CPU:0")
  // y is sent from CPU to TPU.
  let z_tpu : Tensor<Float> = #tfop("Add", x_cpu, y_cpu , __shapes: [TensorShape()], device: "TPU_SYSTEM")
  _hostOp(z_tpu)
}

// There are 4 tensor tranfers: getting x from GPU to CPU; getting the two
// operands of Add from CPU to TPU, and getting the result back to CPU.
//
// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}explicitDevicePlacementAll{{.*}}
//
// CHECK:      builtin "__tfop_tfc.TensorTransfer
// CHECK:      builtin "__tfop_tfc.TensorTransfer
// CHECK:      builtin "__tfop_tfc.TensorTransfer
// CHECK:      builtin "__tfop_tfc.TensorTransfer
// CHECK-NOT:  builtin "__tfop_tfc.TensorTransfer

// CHECK-LABEL: --- TFDevicePartition Per-Device Function Extraction Result: {{.*}}explicitDevicePlacementAll{{.*}}CPU{{.*}}
// get x from GPU
// CHECK: builtin "__tfop_tfc.D2DTensorRecv
// send x_cpu to TPU
// CHECK: builtin "__tfop_tfc.D2DTensorSend
// send y_cpu to TPU
// CHECK: builtin "__tfop_tfc.D2DTensorSend
// get add result from TPU
// CHECK: builtin "__tfop_tfc.D2DTensorRecv

// CHECK-LABEL: --- TFDevicePartition Per-Device Function Extraction Result: {{.*}}explicitDevicePlacementAll{{.*}}GPU{{.*}}
// send x to CPU
// CHECK: builtin "__tfop_tfc.D2DTensorSend

// CHECK-LABEL: --- TFDevicePartition Per-Device Function Extraction Result: {{.*}}explicitDevicePlacementAll{{.*}}TPU{{.*}}
// Receive two operands from CPU, and send the add result back to CPU.
// CHECK: builtin "__tfop_tfc.D2DTensorRecv
// CHECK: builtin "__tfop_tfc.D2DTensorRecv
// CHECK: builtin "__tfop_tfc.D2DTensorSend

// These send/recv ops are threaded via control dependency.
// CHECK:          name: "{{.*}}explicitDevicePlacementAll{{.*}}.tf_CPU.device_partition"
// CHECK:          node_def {
// CHECK:            op: "_Recv"
// CHECK-NEXT:       device: "/device:CPU:0"
// CHECK:          node_def {
// CHECK:            op: "InfeedEnqueueTuple"
// CHECK-NEXT:       input:
// CHECK-NEXT:       input: "^tf_recv_0"
// CHECK-NEXT:       device: "/device:CPU:0"
// CHECK:          node_def {
// CHECK:            op: "InfeedEnqueueTuple"
// CHECK-NEXT:       input:
// CHECK-NEXT:       input: "^tf_infeed_enqueue_1"
// CHECK-NEXT:       device: "/device:CPU:0"
// CHECK:          node_def {
// CHECK:            op: "OutfeedDequeueTuple"
// CHECK-NEXT:       input: "^tf_infeed_enqueue_2"
// CHECK-NEXT:       device: "/device:CPU:0"

public func GPUToTPUTransfer_Unsupported() {
  let x : Tensor<Float> = #tfop("Const", dtype: Float.self, value$tensor: 1.0, __shapes: [TensorShape()], device: "/device:GPU:0")
  // expected-error @+1 {{TPU infeed enqueue cannot run on this device}}
  let _ : Tensor<Float> = #tfop("Identity", x, device: "TPU_SYSTEM")
}
