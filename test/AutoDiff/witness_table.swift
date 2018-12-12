// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

protocol Proto : Differentiable {
  @differentiable(reverse)
  func function1(_ x: Float, _ y: Float) -> Float

  @differentiable(reverse, wrt: (self, .0, .1))
  func function2(_ x: Float, _ y: Float) -> Float

  @differentiable(reverse, wrt: (.1))
  func function3(_ x: Float, _ y: Float) -> Float
}

struct S : Proto {
  typealias TangentVector = S
  typealias CotangentVector = S
  func moved(toward vector: TangentVector) -> S {
    fatalError("unimplemented")
  }

  let p: Float

  // The adjoint is not relevant for this test, but specifying a custom
  // adjoint prevents the AD transform from trying and failing to compute an
  // adjoint.
  @differentiable(reverse, adjoint: afunction1, jvp: dfunction1, vjp: pfunction1)
  func function1(_ x: Float, _ y: Float) -> Float {
    fatalError("unimplemented")
  }

  func afunction1(_ x: Float, _ y: Float, _ res: Float, _ seed: Float) -> (Float, Float) {
    fatalError("unimplemented")
  }

  func dfunction1(_ x: Float, _ y: Float) -> (Float, (Float, Float) -> Float) {
    fatalError("unimplemented")
  }

  func pfunction1(_ x: Float, _ y: Float) -> (Float, (Float) -> (Float, Float)) {
    fatalError("unimplemented")
  }

  // CHECK-LABEL: // jvpSSU protocol witness for Proto.function1(_:_:) in conformance S
  // CHECK-NEXT: sil {{.*}}jvp008SSU{{.*}}P9function1{{.*}} $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float) {
  // CHECK: [[F1_REF_1:%.*]] = function_ref {{.*}}function1{{.*}}
  // CHECK-NEXT: [[F1_AD_1:%.*]] = autodiff_function [wrt 0 1] [order 1] [[F1_REF_1]] : $@convention(method) (Float, Float, S) -> Float
  // CHECK-NEXT: [[F1_JVP:%.*]] = autodiff_function_extract [jvp] [order 1] [[F1_AD_1]] : $@autodiff @convention(method) (Float, Float, @nondiff S) -> Float
  // CHECK-NEXT: apply [[F1_JVP]]
  // CHECK: } // end sil function {{.*}}jvp008SSU{{.*}}P9function1{{.*}}

  // CHECK-LABEL: // vjpSSU protocol witness for Proto.function1(_:_:) in conformance S
  // CHECK-NEXT: sil {{.*}}vjp008SSU{{.*}}P9function1{{.*}} $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float)) {
  // CHECK: [[F1_REF_2:%.*]] = function_ref {{.*}}function1{{.*}}
  // CHECK-NEXT: [[F1_AD_2:%.*]] = autodiff_function [wrt 0 1] [order 1] [[F1_REF_2]] : $@convention(method) (Float, Float, S) -> Float
  // CHECK-NEXT: [[F1_VJP:%.*]] = autodiff_function_extract [vjp] [order 1] [[F1_AD_2]] : $@autodiff @convention(method) (Float, Float, @nondiff S) -> Float
  // CHECK-NEXT: apply [[F1_VJP]]
  // CHECK: } // end sil function {{.*}}vjp008SSU{{.*}}P9function1{{.*}}

  // The adjoint is not relevant for this test, but specifying a custom
  // adjoint prevents the AD transform from trying and failing to compute an
  // adjoint.
  @differentiable(reverse, wrt: (self, .0, .1), adjoint: afunction2, jvp: dfunction2, vjp: pfunction2)
  func function2(_ x: Float, _ y: Float) -> Float {
    fatalError("unimplemented")
  }

  func afunction2(_ x: Float, _ y: Float, _ res: Float, _ seed: Float) -> (S, Float, Float) {
    fatalError("unimplemented")
  }

  func dfunction2(_ x: Float, _ y: Float) -> (Float, (S, Float, Float) -> Float) {
    fatalError("unimplemented")
  }

  func pfunction2(_ x: Float, _ y: Float) -> (Float, (Float) -> (S, Float, Float)) {
    fatalError("unimplemented")
  }

  // CHECK-LABEL: // jvpSSS protocol witness for Proto.function2(_:_:) in conformance S
  // CHECK-NEXT: sil {{.*}}jvp008SSS{{.*}}P9function2{{.*}} $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (@in_guaranteed S, Float, Float) -> Float) {
  // CHECK: [[F2_REF_1:%.*]] = function_ref {{.*}}function2{{.*}}
  // CHECK-NEXT: [[F2_AD_1:%.*]] = autodiff_function [wrt 0 1 2] [order 1] [[F2_REF_1]] : $@convention(method) (Float, Float, S) -> Float
  // CHECK-NEXT: [[F2_JVP:%.*]] = autodiff_function_extract [jvp] [order 1] [[F2_AD_1]] : $@autodiff @convention(method) (Float, Float, S) -> Float
  // CHECK-NEXT: apply [[F2_JVP]]
  // CHECK: } // end sil function {{.*}}jvp008SSS{{.*}}P9function2{{.*}}

  // CHECK-LABEL: // vjpSSS protocol witness for Proto.function2(_:_:) in conformance S
  // CHECK-NEXT: sil {{.*}}vjp008SSS{{.*}}P9function2{{.*}} $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float) -> (@out S, Float, Float)) {
  // CHECK: [[F2_REF_2:%.*]] = function_ref {{.*}}function2{{.*}}
  // CHECK-NEXT: [[F2_AD_2:%.*]] = autodiff_function [wrt 0 1 2] [order 1] [[F2_REF_2]] : $@convention(method) (Float, Float, S) -> Float
  // CHECK-NEXT: [[F2_VJP:%.*]] = autodiff_function_extract [vjp] [order 1] [[F2_AD_2]] : $@autodiff @convention(method) (Float, Float, S) -> Float
  // CHECK-NEXT: apply [[F2_VJP]]
  // CHECK: } // end sil function {{.*}}vjp008SSS{{.*}}P9function2{{.*}}


  // The adjoint is not relevant for this test, but specifying a custom
  // adjoint prevents the AD transform from trying and failing to compute an
  // adjoint.
  @differentiable(reverse, wrt: (.1), adjoint: afunction3, jvp: dfunction3, vjp: pfunction3)
  func function3(_ x: Float, _ y: Float) -> Float {
    fatalError("unimplemented")
  }

  func afunction3(_ x: Float, _ y: Float, _ res: Float, _ seed: Float) -> (Float) {
    fatalError("unimplemented")
  }

  func dfunction3(_ x: Float, _ y: Float) -> (Float, (Float) -> Float) {
    fatalError("unimplemented")
  }

  func pfunction3(_ x: Float, _ y: Float) -> (Float, (Float) -> Float) {
    fatalError("unimplemented")
  }

  // CHECK-LABEL: // jvpUSU protocol witness for Proto.function3(_:_:) in conformance S
  // CHECK-NEXT: sil {{.*}}jvp008USU{{.*}}P9function3{{.*}} $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK: [[F3_REF_1:%.*]] = function_ref {{.*}}function3{{.*}}
  // CHECK-NEXT: [[F3_AD_1:%.*]] = autodiff_function [wrt 1] [order 1] [[F3_REF_1]] : $@convention(method) (Float, Float, S) -> Float
  // CHECK-NEXT: [[F3_JVP:%.*]] = autodiff_function_extract [jvp] [order 1] [[F3_AD_1]] : $@autodiff @convention(method) (@nondiff Float, Float, @nondiff S) -> Float
  // CHECK-NEXT: apply [[F3_JVP]]
  // CHECK: } // end sil function {{.*}}jvp008USU{{.*}}P9function3{{.*}}

  // CHECK-LABEL: // vjpUSU protocol witness for Proto.function3(_:_:) in conformance S
  // CHECK-NEXT: sil {{.*}}vjp008USU{{.*}}P9function3{{.*}} $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK: [[F3_REF_2:%.*]] = function_ref {{.*}}function3{{.*}}
  // CHECK-NEXT: [[F3_AD_2:%.*]] = autodiff_function [wrt 1] [order 1] [[F3_REF_2]] : $@convention(method) (Float, Float, S) -> Float
  // CHECK-NEXT: [[F3_VJP:%.*]] = autodiff_function_extract [vjp] [order 1] [[F3_AD_2]] : $@autodiff @convention(method) (@nondiff Float, Float, @nondiff S) -> Float
  // CHECK-NEXT: apply [[F3_VJP]]
  // CHECK: } // end sil function {{.*}}vjp008USU{{.*}}P9function3{{.*}}
}

// CHECK-LABEL: sil_witness_table {{.*}} S: Proto {{.*}} {
// CHECK: method #Proto.function1!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float {{.*}} // protocol witness for Proto.function1(_:_:) in conformance S
// CHECK: autodiff_associated_function jvp 1 SSU #Proto.function1!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // jvpSSU protocol witness for Proto.function1(_:_:) in conformance S
// CHECK: autodiff_associated_function vjp 1 SSU #Proto.function1!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // vjpSSU protocol witness for Proto.function1(_:_:) in conformance S
// CHECK: method #Proto.function2!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // protocol witness for Proto.function2(_:_:) in conformance S
// CHECK: autodiff_associated_function jvp 1 SSS #Proto.function2!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // jvpSSS protocol witness for Proto.function2(_:_:) in conformance S
// CHECK: autodiff_associated_function vjp 1 SSS #Proto.function2!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // vjpSSS protocol witness for Proto.function2(_:_:) in conformance S
// CHECK: method #Proto.function3!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // protocol witness for Proto.function3(_:_:) in conformance S
// CHECK: autodiff_associated_function jvp 1 USU #Proto.function3!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // jvpUSU protocol witness for Proto.function3(_:_:) in conformance S
// CHECK: autodiff_associated_function vjp 1 USU #Proto.function3!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // vjpUSU protocol witness for Proto.function3(_:_:) in conformance S
// CHECK: }
