// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/ExternalStructs.swift -enable-resilience -emit-module -emit-module-path %t/ExternalStructs.swiftmodule
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -enable-resilience -I %t -verify %s | %FileCheck %s

import TensorFlow
import ExternalStructs

@inlinable @inline(__always)
public func genericPackedResults1<T>() -> T {
  // expected-error @+1 {{op named 'SomeOp2' is not registered in TensorFlow}}
  return #tfop("SomeOp2")
}

public func packResultsToAggregate_indirectResult1() {
  struct Foo {
    let x: Tensor<Float>
  }
  let foo: Foo = genericPackedResults1()
  _hostOp(foo)
}

// CHECK-LABEL: --- TFDeabstraction Result: {{.*}}packResultsToAggregate_indirectResult1{{.*}}
// CHECK: [[B0:%.*]] = graph_op "SomeOp2"()
// CHECK: [[B1:%.*]] = struct $Tensor<Float> ([[B0]] : $TensorHandle<Float>)
// CHECK: [[B2:%.*]] = struct $Foo ([[B1]] : $Tensor<Float>)

@inlinable @inline(__always)
public func genericPackedResults2<T>() -> T {
  // expected-error @+1 {{op named 'SomeOp3' is not registered in TensorFlow}}
  return #tfop("SomeOp3")
}

public func packResultsToAggregate_indirectResult2() {
  struct Foo {
    let x: Tensor<Float>
  }
  let foo: (Foo, Tensor<Float>) = genericPackedResults2()
  _hostOp(foo)
}

// CHECK-LABEL: --- TFDeabstraction Result: {{.*}}packResultsToAggregate_indirectResult2{{.*}}
// CHECK: ([[C0:%.*]], [[C1:%.*]]) = graph_op "SomeOp3"()
// CHECK: [[C2:%.*]] = struct $Tensor<Float> ([[C0]] : $TensorHandle<Float>)
// CHECK: [[C3:%.*]] = struct $Foo ([[C2]] : $Tensor<Float>)
// CHECK: [[C4:%.*]] = struct $Tensor<Float> ([[C1]] : $TensorHandle<Float>)
// CHECK: [[C5:%.*]] = tuple ([[C3]] : $Foo, [[C4]] : $Tensor<Float>)

public func packResultsToAggregate_externalStructFixedLayout() {
  // expected-error @+1 {{op named 'SomeOp4' is not registered in TensorFlow}}
  let foo: ExternalStructFixedLayout = #tfop("SomeOp4")
  _hostOp(foo)
}

// CHECK-LABEL: --- TFDeabstraction Result: {{.*}}packResultsToAggregate_externalStructFixedLayout{{.*}}
// CHECK: [[C0:%.*]] = graph_op "SomeOp4"()
// CHECK: [[C1:%.*]] = struct $Tensor<Float> ([[C0]] : $TensorHandle<Float>)
// CHECK: [[C2:%.*]] = struct $ExternalStructFixedLayout ([[C1]] : $Tensor<Float>)

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
// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}packResultsToAggregate_basic{{.*}}
// CHECK:  ([[A0:%.*]], [[A1:%.*]], [[A2:%.*]], [[A3:%.*]], [[A4:%.*]]) = graph_op "SomeOp"() {__device: "/device:CPU:0"} : $TensorHandle<Float>, $TensorHandle<Float>, $TensorHandle<Int32>, $TensorHandle<Bool>, $TensorHandle<Double>
// CHECK: graph_op "Add,i,i"({{.*}} : $TensorHandle<Int32>, [[A2]] : $TensorHandle<Int32>) {T: $Int32, __device: "/device:CPU:0"} : $TensorHandle<Int32>
// CHECK:  [[PACKED:%.*]] = tuple ([[A0]] : $TensorHandle<Float>, [[A1]] : $TensorHandle<Float>, [[A2]] : $TensorHandle<Int32>, [[A3]] : $TensorHandle<Bool>, [[A4]] : $TensorHandle<Double>)
// CHECK:  return [[PACKED]] : $(TensorHandle<Float>, TensorHandle<Float>, TensorHandle<Int32>, TensorHandle<Bool>, TensorHandle<Double>)


