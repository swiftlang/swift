// RUN: %target-swift-frontend -Xllvm -tf-use-device-stack -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -Xllvm -tf-module-level-graph=false -emit-sil -verify %s | %FileCheck %s

import TensorFlow

public func deviceFreeFunc() {
  let y = withDevice(.cpu, 1) { () -> Tensor<Float> in
    // Define this local function with inline never, so that we can partition
    // this function.
    // TODO: assess how to partition the withDevice closure itself.
    @inline(never)
    func tensorFn() -> Tensor<Float> {
      let x = Tensor<Float>(1.0)
      return x + x
    }
    return tensorFn()
  }
  _hostOp(y)
}

// CHECK-LABEL: --- TFDevicePartition Cross Device Tensor Transfer Annotation Result
// CHECK-NEXT: // tensorFn
// No device placement on "Add" at compile time.
// CHECK: graph_op "Add"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>)

// The generated graph should have no device placement. Device placement is done
// at graph function invocation time, via the RUNTIME name mangling.
// CHECK-LABEL: --- TFPartition GraphDef Proto
// CHECK-NOT: device: "
// CHECK:      library {
// CHECK-NEXT:   function {
// CHECK-NEXT:     signature {
// CHECK-NEXT:       name: "{{.*}}.tf_RUNTIME.device_partition"
