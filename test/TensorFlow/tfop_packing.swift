// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s | %FileCheck %s

import TensorFlow

public func packResultsToAggregate_basic() {
  struct Foo {
    let x: Tensor<Float>
    let y: (Tensor<Float>, (a: Tensor<Int32>, b: Tensor<Bool>))
  }
  // expected-error @+1 {{op named 'SomeOp' is not registered in TensorFlow}}
  let (r0, r1): (Foo, Tensor<Double>) = #tfop("SomeOp")
  let _ = Tensor(0) + r0.y.1.a
  _hostOp(r1)
}
// CHECK-LABEL --- TFPartition Accelerator Result: {{.*}}packResultsToAggregate_basic{{.*}}
// CHECK:  ([[A0:%.*]], [[A1:%.*]], [[A2:%.*]], [[A3:%.*]], [[A4:%.*]]) = graph_op "SomeOp"() {__device: "/device:CPU:0"} : $TensorHandle<Float>, $TensorHandle<Float>, $TensorHandle<Int32>, $TensorHandle<Bool>, $TensorHandle<Double>
// CHECK: graph_op "Add,i,i"({{.*}} : $TensorHandle<Int32>, [[A2]] : $TensorHandle<Int32>) {T: $Int32, __device: "/device:CPU:0"} : $TensorHandle<Int32>
// CHECK:  [[PACKED:%.*]] = tuple ([[A0]] : $TensorHandle<Float>, [[A1]] : $TensorHandle<Float>, [[A2]] : $TensorHandle<Int32>, [[A3]] : $TensorHandle<Bool>, [[A4]] : $TensorHandle<Double>)
// CHECK:  return [[PACKED]] : $(TensorHandle<Float>, TensorHandle<Float>, TensorHandle<Int32>, TensorHandle<Bool>, TensorHandle<Double>)

@inlinable @inline(__always)
public func genericPackedResults<T>() -> T {
  // expected-error @+1 {{op named 'SomeOp2' is not registered in TensorFlow}}
  return #tfop("SomeOp2")
}

public func packResultsToAggregate_indirectResult() {
  struct Foo {
    let x: Tensor<Float>
  }
  let foo: Foo = genericPackedResults()
  _hostOp(foo)
}

// CHECK-LABEL --- TFDeabstraction Result: {{.*}}packResultsToAggregate_indirectResult{{.*}}
// CHECK: [[B0:%.*]] = graph_op "SomeOp2"()
// CHECK: [[B1:%.*]] = struct $Tensor<Float> ([[B0]] : $TensorHandle<Float>)
// CHECK: struct $Foo ([[B1]] : $Tensor<Float>)
