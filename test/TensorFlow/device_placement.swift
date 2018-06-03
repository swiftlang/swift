// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -O -emit-sil -verify %s | %FileCheck %s

import TensorFlow

public func implicitDevicePlacement() {
  let x : Tensor<Float> = #tfop("Const", dtype: Float.self, value$tensor: 1.0)
  let _ = x.array
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}implicitDevicePlacement{{.*}}
// CHECK: string_literal utf8 "/device:CPU:0"
// CHECK: builtin "__tfop_Const,dtype,value$tensor,device

public func implicitDeviceConfig() {
  let x = Tensor<Float>(1.0)
  let _ = x.array
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}implicitDeviceConfig{{.*}}
// CHECK: string_literal utf8 "/device:CPU:0"
// CHECK: builtin "__tfop_Const,dtype$dtype,value$tensor,device

public func explicitDeviceConfig() {
  TensorFlow.enableGPU()
  let x = Tensor<Float>(1.0)
  let _ = x.array
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}explicitDeviceConfig{{.*}}
// CHECK: string_literal utf8 "/device:GPU:0"
// CHECK: builtin "__tfop_Const,dtype$dtype,value$tensor,device

// Check that in the TF graph, both the function node itself, and ops in the
// function, are placed on GPU.
//
// CHECK:      op: "{{.*}}explicitDeviceConfig{{.*}}.tf_partition"
// CHECK-NEXT: device: "/device:GPU:0"

// CHECK:      library {
// CHECK-NEXT:   function {
// CHECK-NEXT:     signature {
// CHECK-NEXT:       name: "{{.*}}explicitDeviceConfig{{.*}}.tf_partition"
// CHECK:          node_def {
// CHECK:            op: "Const"
// CHECK-NEXT:       device: "/device:GPU:0"
