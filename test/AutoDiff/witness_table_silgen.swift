// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

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

  @differentiable(reverse, jvp: dfunction1, vjp: pfunction1)
  func function1(_ x: Float, _ y: Float) -> Float {
    return x + y + p
  }

  func dfunction1(_ x: Float, _ y: Float) -> (Float, (Float, Float) -> Float) {
    fatalError("unimplemented")
  }

  func pfunction1(_ x: Float, _ y: Float) -> (Float, (Float) -> (Float, Float)) {
    fatalError("unimplemented")
  }

  // CHECK-LABEL: sil {{.*}} @AD__{{.*}}function1{{.*}}_jvp_SSU : $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float) {
  // CHECK: [[JVP1_FNREF:%.*]] = function_ref {{.*}}function1{{.*}} : $@convention(method) (Float, Float, S) -> Float
  // CHECK: [[JVP1_ADFUNC:%.*]] = autodiff_function [wrt 0 1] [order 1] [[JVP1_FNREF]] : $@convention(method) (Float, Float, S) -> Float
  // CHECK: [[JVP1:%.*]] = autodiff_function_extract [jvp] [order 1] [[JVP1_ADFUNC]] : $@autodiff @convention(method) (Float, Float, @nondiff S) -> Float
  // CHECK: apply [[JVP1]]
  // CHECK: } // end sil function 'AD__{{.*}}function1{{.*}}_jvp_SSU'

  // CHECK-LABEL: sil {{.*}} @AD__{{.*}}function1{{.*}}_vjp_SSU : $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float)) {
  // CHECK: [[VJP1_FNREF:%.*]] = function_ref {{.*}}function1{{.*}} : $@convention(method) (Float, Float, S) -> Float
  // CHECK: [[VJP1_ADFUNC:%.*]] = autodiff_function [wrt 0 1] [order 1] [[VJP1_FNREF]] : $@convention(method) (Float, Float, S) -> Float
  // CHECK: [[VJP1:%.*]] = autodiff_function_extract [vjp] [order 1] [[VJP1_ADFUNC]] : $@autodiff @convention(method) (Float, Float, @nondiff S) -> Float
  // CHECK: apply [[VJP1]]
  // CHECK: } // end sil function 'AD__{{.*}}function1{{.*}}_vjp_SSU'

  @differentiable(reverse, wrt: (self, .0, .1), jvp: dfunction2, vjp: pfunction2)
  func function2(_ x: Float, _ y: Float) -> Float {
    return x + y + p
  }

  func dfunction2(_ x: Float, _ y: Float) -> (Float, (S, Float, Float) -> Float) {
    fatalError("unimplemented")
  }

  func pfunction2(_ x: Float, _ y: Float) -> (Float, (Float) -> (S, Float, Float)) {
    fatalError("unimplemented")
  }

  // CHECK-LABEL: sil {{.*}} @AD__{{.*}}function2{{.*}}_jvp_SSS : $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (@in_guaranteed S, Float, Float) -> Float) {
  // CHECK: [[JVP2_FNREF:%.*]] = function_ref {{.*}}function2{{.*}} : $@convention(method) (Float, Float, S) -> Float
  // CHECK: [[JVP2_ADFUNC:%.*]] = autodiff_function [wrt 0 1 2] [order 1] [[JVP2_FNREF]] : $@convention(method) (Float, Float, S) -> Float
  // CHECK: [[JVP2:%.*]] = autodiff_function_extract [jvp] [order 1] [[JVP2_ADFUNC]] : $@autodiff @convention(method) (Float, Float, S) -> Float
  // CHECK: apply [[JVP2]]
  // CHECK: } // end sil function 'AD__{{.*}}function2{{.*}}_jvp_SSS'

  // CHECK-LABEL: sil {{.*}} @AD__{{.*}}function2{{.*}}_vjp_SSS : $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float) -> (@out S, Float, Float)) {
  // CHECK: [[VJP2_FNREF:%.*]] = function_ref {{.*}}function2{{.*}} : $@convention(method) (Float, Float, S) -> Float
  // CHECK: [[VJP2_ADFUNC:%.*]] = autodiff_function [wrt 0 1 2] [order 1] [[VJP2_FNREF]] : $@convention(method) (Float, Float, S) -> Float
  // CHECK: [[VJP2:%.*]] = autodiff_function_extract [vjp] [order 1] [[VJP2_ADFUNC]] : $@autodiff @convention(method) (Float, Float, S) -> Float
  // CHECK: apply [[VJP2]]
  // CHECK: } // end sil function 'AD__{{.*}}function2{{.*}}_vjp_SSS'

  @differentiable(reverse, wrt: (.1), jvp: dfunction3, vjp: pfunction3)
  func function3(_ x: Float, _ y: Float) -> Float {
    return x + y + p
  }

  func dfunction3(_ x: Float, _ y: Float) -> (Float, (Float) -> Float) {
    fatalError("unimplemented")
  }

  func pfunction3(_ x: Float, _ y: Float) -> (Float, (Float) -> Float) {
    fatalError("unimplemented")
  }

  // CHECK-LABEL: sil {{.*}} @AD__{{.*}}function3{{.*}}_jvp_USU : $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK: [[JVP3_FNREF:%.*]] = function_ref {{.*}}function3{{.*}} : $@convention(method) (Float, Float, S) -> Float
  // CHECK: [[JVP3_ADFUNC:%.*]] = autodiff_function [wrt 1] [order 1] [[JVP3_FNREF]] : $@convention(method) (Float, Float, S) -> Float
  // CHECK: [[JVP3:%.*]] = autodiff_function_extract [jvp] [order 1] [[JVP3_ADFUNC]] : $@autodiff @convention(method) (@nondiff Float, Float, @nondiff S) -> Float
  // CHECK: apply [[JVP3]]
  // CHECK: } // end sil function 'AD__{{.*}}function3{{.*}}_jvp_USU'

  // CHECK-LABEL: sil {{.*}} @AD__{{.*}}function3{{.*}}_vjp_USU : $@convention(witness_method: Proto) (Float, Float, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK: [[VJP3_FNREF:%.*]] = function_ref {{.*}}function3{{.*}} : $@convention(method) (Float, Float, S) -> Float
  // CHECK: [[VJP3_ADFUNC:%.*]] = autodiff_function [wrt 1] [order 1] [[VJP3_FNREF]] : $@convention(method) (Float, Float, S) -> Float
  // CHECK: [[VJP3:%.*]] = autodiff_function_extract [vjp] [order 1] [[VJP3_ADFUNC]] : $@autodiff @convention(method) (@nondiff Float, Float, @nondiff S) -> Float
  // CHECK: apply [[VJP3]]
  // CHECK: } // end sil function 'AD__{{.*}}function3{{.*}}_vjp_USU'
}

// CHECK-LABEL: sil_witness_table hidden S: Proto module witness_table_silgen {
// CHECK-NEXT:  base_protocol Differentiable: S: Differentiable module witness_table_silgen
// CHECK-NEXT:  method #Proto.function1!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : @{{.*}}function1
// CHECK-NEXT:  method #Proto.function1!1.jvp.1.SSU: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : @AD__{{.*}}function1{{.*}}_jvp_SSU
// CHECK-NEXT:  method #Proto.function1!1.vjp.1.SSU: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : @AD__{{.*}}function1{{.*}}_vjp_SSU
// CHECK-NEXT:  method #Proto.function2!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : @{{.*}}function2
// CHECK-NEXT:  method #Proto.function2!1.jvp.1.SSS: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : @AD__{{.*}}function2{{.*}}_jvp_SSS
// CHECK-NEXT:  method #Proto.function2!1.vjp.1.SSS: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : @AD__{{.*}}function2{{.*}}_vjp_SSS
// CHECK-NEXT:  method #Proto.function3!1: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : @{{.*}}function3
// CHECK-NEXT:  method #Proto.function3!1.jvp.1.USU: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : @AD__{{.*}}function3{{.*}}_jvp_USU
// CHECK-NEXT:  method #Proto.function3!1.vjp.1.USU: <Self where Self : Proto> (Self) -> (Float, Float) -> Float : @AD__{{.*}}function3{{.*}}_vjp_USU
// CHECK-NEXT:}
