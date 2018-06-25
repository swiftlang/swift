// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

import TensorFlow

@TensorFlowGraph
func tensorHandles(_ x: TensorHandle<Int32>) -> TensorHandle<Double> {}

// CHECK: {{.*}}tensorHandles{{.*}} : $@convention(tensorflow)

@TensorFlowGraph
func otherHandles(_ x: ResourceHandle) -> VariantHandle {} // okay

// CHECK: {{.*}}otherHandles{{.*}} : $@convention(tensorflow)
