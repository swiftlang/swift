// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/ExternalStructs.swift -enable-resilience -emit-module -emit-module-path %t/ExternalStructs.swiftmodule
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -enable-resilience -I %t -verify %s | %FileCheck %s

import TensorFlow
import ExternalStructs

@inlinable @inline(__always)
public func genericPackedResults1<T: TensorGroup>() -> T {
  // expected-error @+1 {{op named 'SomeOp2' is not registered in TensorFlow}}
  return #tfop("SomeOp2")
}

struct ContainsOneTensor {
  let x: Tensor<Float>
}
extension ContainsOneTensor : TensorGroup {
  public func _unpackTensorHandles(into address: UnsafeMutablePointer<CTensorHandle>?) {
    fatalError("dummy conformance")
  }
  public static var _typeList: [TensorDataType] {
    fatalError("dummy conformance")
  }
  public static var _unknownShapeList: [TensorShape?] {
    fatalError("dummy conformance")
  }
  public init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    fatalError("dummy conformance")
  }
}

public func packResultsToAggregate_indirectResult1() {
  let foo: ContainsOneTensor = genericPackedResults1()
  _hostOp(foo)
}

// CHECK-LABEL: --- TFDeabstraction Result: {{.*}}packResultsToAggregate_indirectResult1{{.*}}
// CHECK: [[B0:%.*]] = graph_op "SomeOp2"()
// CHECK: [[B1:%.*]] = struct $Tensor<Float> ([[B0]] : $TensorHandle<Float>)
// CHECK: [[B2:%.*]] = struct $ContainsOneTensor ([[B1]] : $Tensor<Float>)

@inlinable @inline(__always)
public func genericPackedResults2<T: TensorGroup>() -> (T, Tensor<Float>) {
  // expected-error @+1 {{op named 'SomeOp3' is not registered in TensorFlow}}
  return #tfop("SomeOp3")
}

public func packResultsToAggregate_indirectResult2() {
  let foo: (ContainsOneTensor, Tensor<Float>) = genericPackedResults2()
  _hostOp(foo)
}

// CHECK-LABEL: --- TFDeabstraction Result: {{.*}}packResultsToAggregate_indirectResult2{{.*}}
// CHECK: ([[C0:%.*]], [[C1:%.*]]) = graph_op "SomeOp3"()
// CHECK: [[C2:%.*]] = struct $Tensor<Float> ([[C0]] : $TensorHandle<Float>)
// CHECK: [[C3:%.*]] = struct $ContainsOneTensor ([[C2]] : $Tensor<Float>)
// CHECK: [[C4:%.*]] = struct $Tensor<Float> ([[C1]] : $TensorHandle<Float>)
// CHECK: [[C5:%.*]] = tuple ([[C3]] : $ContainsOneTensor, [[C4]] : $Tensor<Float>)

public func packResultsToAggregate_externalStructFixedLayout() {
  // expected-error @+1 {{op named 'SomeOp4' is not registered in TensorFlow}}
  let foo: ExternalStructFixedLayout = #tfop("SomeOp4")
  _hostOp(foo)
}

// CHECK-LABEL: --- TFDeabstraction Result: {{.*}}packResultsToAggregate_externalStructFixedLayout{{.*}}
// CHECK: [[C0:%.*]] = graph_op "SomeOp4"()
// CHECK: [[C1:%.*]] = struct $Tensor<Float> ([[C0]] : $TensorHandle<Float>)
// CHECK: [[C2:%.*]] = struct $ExternalStructFixedLayout ([[C1]] : $Tensor<Float>)

struct ManyNestedTensors {
  let x: Tensor<Float>
  let y: (Tensor<Float>, (a: Tensor<Int32>, b: Tensor<Bool>))
}
extension ManyNestedTensors : TensorGroup {
  public func _unpackTensorHandles(into address: UnsafeMutablePointer<CTensorHandle>?) {
    fatalError("dummy conformance")
  }
  public static var _typeList: [TensorDataType] {
    fatalError("dummy conformance")
  }
  public static var _unknownShapeList: [TensorShape?] {
    fatalError("dummy conformance")
  }
  public init(_owning tensorHandles: UnsafePointer<CTensorHandle>?) {
    fatalError("dummy conformance")
  }
}

public func unpackAggregate_basic() {
  let foo = ManyNestedTensors(x: Tensor(1), y: (Tensor(2), (a: Tensor(3), b: Tensor(false))))
  // expected-error @+1 {{op named 'SomeOp5' is not registered in TensorFlow}}
  let _: Tensor<Float> = #tfop("SomeOp5", [foo])
}

// CHECK-LABEL: --- TFDeabstraction Result: {{.*}}unpackAggregate_basic{{.*}}
// CHECK: [[D_ManyNestedTensors_x:%.*]] = graph_op "Const"(){{.*}}f32 0x3F800000 /* 1 */{{.*}}
// CHECK: [[D_ManyNestedTensors_y_0:%.*]] = graph_op "Const"(){{.*}}f32 0x40000000 /* 2 */{{.*}}
// CHECK: [[D_ManyNestedTensors_y_1_a:%.*]] = graph_op "Const"(){{.*}}i32 3{{.*}}
// CHECK: [[D_ManyNestedTensors_y_1_b:%.*]] = graph_op "Const"(){{.*}}i1 0{{.*}}
// CHECK: graph_op "SomeOp5"({{\[}}[[D_ManyNestedTensors_x]] : $TensorHandle<Float>, [[D_ManyNestedTensors_y_0]] : $TensorHandle<Float>, [[D_ManyNestedTensors_y_1_a]] : $TensorHandle<Int32>, [[D_ManyNestedTensors_y_1_b]] : $TensorHandle<Bool>{{\]}})

@inlinable @inline(__always)
public func genericUnpackedAggregate<T : TensorGroup>(t: T) -> Tensor<Float> {
  // expected-error @+1 {{op named 'SomeOp6' is not registered in TensorFlow}}
  return #tfop("SomeOp6", [t])
}

public func unpackAggregate_generic() {
  let foo = ManyNestedTensors(x: Tensor(1), y: (Tensor(2), (a: Tensor(3), b: Tensor(false))))
  let _: Tensor<Float> = genericUnpackedAggregate(t: foo)
}

// CHECK-LABEL: --- TFDeabstraction Result: {{.*}}unpackAggregate_generic{{.*}}
// CHECK: [[E_ManyNestedTensors_x:%.*]] = graph_op "Const"(){{.*}}f32 0x3F800000 /* 1 */{{.*}}
// CHECK: [[E_ManyNestedTensors_y_0:%.*]] = graph_op "Const"(){{.*}}f32 0x40000000 /* 2 */{{.*}}
// CHECK: [[E_ManyNestedTensors_y_1_a:%.*]] = graph_op "Const"(){{.*}}i32 3{{.*}}
// CHECK: [[E_ManyNestedTensors_y_1_b:%.*]] = graph_op "Const"(){{.*}}i1 0{{.*}}
// CHECK: graph_op "SomeOp6"({{\[}}[[E_ManyNestedTensors_x]] : $TensorHandle<Float>, [[E_ManyNestedTensors_y_0]] : $TensorHandle<Float>, [[E_ManyNestedTensors_y_1_a]] : $TensorHandle<Int32>, [[E_ManyNestedTensors_y_1_b]] : $TensorHandle<Bool>{{\]}})

// Checks that unpacking reuses extraction instructions instead of making new
// ones for each element of the input list.
// TODO(SR-8680): After fixing tuples, make x a tuple, so that we also test tuple extract reuse.
// expected-warning @+1 {{copied to the accelerator}}
public func unpackAggregate_reuse(x: Tensor<Float>) {
  // expected-error @+2 {{op named 'SomeOp7' is not registered in TensorFlow}}
  // expected-note @+1 {{value used here}}
  let _: Tensor<Float> = #tfop("SomeOp7", [x, x])
}

// CHECK-LABEL: --- TFDeabstraction Result: {{.*}}unpackAggregate_reuse{{.*}}
// CHECK: struct_extract
// CHECK: struct_extract
// CHECK: [[F_TensorHandle:%.*]] = struct_extract %0 : $Tensor<Float>, #Tensor.handle
// CHECK: graph_op "SomeOp7"({{\[}}[[F_TensorHandle]] : $TensorHandle<Float>, [[F_TensorHandle]] : $TensorHandle<Float>{{\]}})

// TODO(SR-8680): Make this testcase work.
// public func unpackAggregate_tuple() {
//   // xpected-error @+1 {{op named 'SomeOp7' is not registered in TensorFlow}}
//   let _: Tensor<Float> = #tfop("SomeOp7", [(Tensor<Int32>(1), Tensor<Int32>(2))])
// }

public func packResultsToAggregate_basic() {
  // expected-error @+1 {{op named 'SomeOp' is not registered in TensorFlow}}
  let (r0, r1): (ManyNestedTensors, Tensor<Double>) = #tfop("SomeOp")
  let _ = Tensor(0) + r0.y.1.a
  _hostOp(r1)
}
// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}packResultsToAggregate_basic{{.*}}
// CHECK:  ([[A0:%.*]], [[A1:%.*]], [[A2:%.*]], [[A3:%.*]], [[A4:%.*]]) = graph_op "SomeOp"() {__device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<Float>, $TensorHandle<Float>, $TensorHandle<Int32>, $TensorHandle<Bool>, $TensorHandle<Double>
// CHECK: graph_op "Add"({{.*}} : $TensorHandle<Int32>, [[A2]] : $TensorHandle<Int32>) {T$dtype: i32 3, __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<Int32>
// CHECK:  [[PACKED:%.*]] = tuple ([[A0]] : $TensorHandle<Float>, [[A1]] : $TensorHandle<Float>, [[A2]] : $TensorHandle<Int32>, [[A3]] : $TensorHandle<Bool>, [[A4]] : $TensorHandle<Double>)
// CHECK:  return [[PACKED]] : $(TensorHandle<Float>, TensorHandle<Float>, TensorHandle<Int32>, TensorHandle<Bool>, TensorHandle<Double>)


