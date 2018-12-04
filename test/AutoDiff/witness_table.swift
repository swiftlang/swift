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

  // CHECK-LABEL: // jvpMSSU protocol witness for Proto.function1(_:_:) in conformance S
  // CHECK-NEXT: sil {{.*}}jvp009MSSU{{.*}}P9function1{{.*}} $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float) {
  // CHECK: // function_ref S.dfunction1(_:_:)
  // CHECK: } // end sil function {{.*}}jvp009MSSU{{.*}}P9function1{{.*}}

  // CHECK-LABEL: // vjpMSSU protocol witness for Proto.function1(_:_:) in conformance S
  // CHECK-NEXT: sil {{.*}}vjp009MSSU{{.*}}P9function1{{.*}} $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float)) {
  // CHECK: // function_ref S.pfunction1(_:_:)
  // CHECK: } // end sil function {{.*}}vjp009MSSU{{.*}}P9function1{{.*}}


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

  // CHECK-LABEL: // jvpMSSS protocol witness for Proto.function2(_:_:) in conformance S
  // CHECK-NEXT: sil {{.*}}jvp009MSSS{{.*}}P9function2{{.*}} $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (@in_guaranteed S, Float, Float) -> Float) {
  // CHECK: // function_ref S.dfunction2(_:_:)
  // CHECK: } // end sil function {{.*}}jvp009MSSS{{.*}}P9function2{{.*}}

  // CHECK-LABEL: // vjpMSSS protocol witness for Proto.function2(_:_:) in conformance S
  // CHECK-NEXT: sil {{.*}}vjp009MSSS{{.*}}P9function2{{.*}} $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float) -> (@out S, Float, Float)) {
  // CHECK: // function_ref S.pfunction2(_:_:)
  // CHECK: } // end sil function {{.*}}vjp009MSSS{{.*}}P9function2{{.*}}


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

  // CHECK-LABEL: // jvpMUSU protocol witness for Proto.function3(_:_:) in conformance S
  // CHECK-NEXT: sil {{.*}}jvp009MUSU{{.*}}P9function3{{.*}} $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK: // function_ref S.dfunction3(_:_:)
  // CHECK: } // end sil function {{.*}}jvp009MUSU{{.*}}P9function3{{.*}}

  // CHECK-LABEL: // vjpMUSU protocol witness for Proto.function3(_:_:) in conformance S
  // CHECK-NEXT: sil {{.*}}vjp009MUSU{{.*}}P9function3{{.*}} $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK: // function_ref S.pfunction3(_:_:)
  // CHECK: } // end sil function {{.*}}vjp009MUSU{{.*}}P9function3{{.*}}
}

// CHECK-LABEL: sil_witness_table {{.*}} S: Proto {{.*}} {
// CHECK: method #Proto.function1!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float {{.*}} // protocol witness for Proto.function1(_:_:) in conformance S
// CHECK: autodiff_associated_function jvp 1 MSSU #Proto.function1!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // jvpMSSU protocol witness for Proto.function1(_:_:) in conformance S
// CHECK: autodiff_associated_function vjp 1 MSSU #Proto.function1!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // vjpMSSU protocol witness for Proto.function1(_:_:) in conformance S
// CHECK: method #Proto.function2!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // protocol witness for Proto.function2(_:_:) in conformance S
// CHECK: autodiff_associated_function jvp 1 MSSS #Proto.function2!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // jvpMSSS protocol witness for Proto.function2(_:_:) in conformance S
// CHECK: autodiff_associated_function vjp 1 MSSS #Proto.function2!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // vjpMSSS protocol witness for Proto.function2(_:_:) in conformance S
// CHECK: method #Proto.function3!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // protocol witness for Proto.function3(_:_:) in conformance S
// CHECK: autodiff_associated_function jvp 1 MUSU #Proto.function3!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // jvpMUSU protocol witness for Proto.function3(_:_:) in conformance S
// CHECK: autodiff_associated_function vjp 1 MUSU #Proto.function3!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : {{.*}} // vjpMUSU protocol witness for Proto.function3(_:_:) in conformance S
// CHECK: }
