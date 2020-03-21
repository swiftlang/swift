// RUN: %target-swift-frontend -emit-sil -verify -Xllvm -differentiation-skip-folding-differentiable-function-extraction %s
// RUN: %target-swift-frontend -emit-sil -verify -Xllvm -differentiation-skip-folding-differentiable-function-extraction %s | %FileCheck %s

protocol Proto : Differentiable {
  @differentiable(wrt: (x, y))
  func function1(_ x: Float, _ y: Double) -> Float

  @differentiable(wrt: (self, x, y))
  func function2(_ x: Float, _ y: Double) -> Float

  @differentiable(wrt: y)
  func function3(_ x: Float, _ y: Double) -> Double
}

struct S : Proto, AdditiveArithmetic {
  typealias Scalar = Float

  @differentiable
  var p: Float

  @differentiable(wrt: (x, y))
  func function1(_ x: Float, _ y: Double) -> Float {
    return x + p
  }

  // CHECK-LABEL: sil {{.*}} @AD__{{.*}}function1{{.*}}_jvp_SSU : $@convention(witness_method: Proto) (Float, Double, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float, Double) -> Float) {
  // CHECK: [[JVP1_ORIG_FNREF:%.*]] = function_ref {{.*}}function1{{.*}} : $@convention(method) (Float, Double, S) -> Float
  // CHECK: [[JVP1_VJP_FNREF:%.*]] = differentiability_witness_function [vjp] [parameters 0 1] [results 0] @{{.*}}function1{{.*}}
  // CHECK: [[JVP1_ADFUNC:%.*]] = differentiable_function [parameters 0 1] [[JVP1_ORIG_FNREF]] : {{.*}} with_derivative {{{%.*}} : {{.*}}, [[JVP1_VJP_FNREF]] : {{.*}}}
  // CHECK: [[JVP1:%.*]] = differentiable_function_extract [jvp] [[JVP1_ADFUNC]] : $@differentiable @convention(method) (Float, Double, @noDerivative S) -> Float
  // CHECK: apply [[JVP1]]
  // CHECK: } // end sil function 'AD__{{.*}}function1{{.*}}_jvp_SSU'

  // CHECK-LABEL: sil {{.*}} @AD__{{.*}}function1{{.*}}_vjp_SSU : $@convention(witness_method: Proto) (Float, Double, @in_guaranteed S) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Double)) {
  // CHECK: [[VJP1_ORIG_FNREF:%.*]] = function_ref {{.*}}function1{{.*}} : $@convention(method) (Float, Double, S) -> Float
  // CHECK: [[VJP1_VJP_FNREF:%.*]] = differentiability_witness_function [vjp] [parameters 0 1] [results 0] @{{.*}}function1{{.*}}
  // CHECK: [[VJP1_ADFUNC:%.*]] = differentiable_function [parameters 0 1] [[VJP1_ORIG_FNREF]] : {{.*}} with_derivative {{{%.*}} : {{.*}}, [[VJP1_VJP_FNREF]] : {{.*}}}
  // CHECK: [[VJP1:%.*]] = differentiable_function_extract [vjp] [[VJP1_ADFUNC]] : $@differentiable @convention(method) (Float, Double, @noDerivative S) -> Float
  // CHECK: apply [[VJP1]]
  // CHECK: } // end sil function 'AD__{{.*}}function1{{.*}}_vjp_SSU'

  @differentiable(wrt: (self, x, y))
  func function2(_ x: Float, _ y: Double) -> Float {
    return x + p
  }

  // CHECK-LABEL: sil {{.*}} @AD__{{.*}}function2{{.*}}_jvp_SSS : $@convention(witness_method: Proto) (Float, Double, @in_guaranteed S) -> (Float, @owned @callee_guaranteed @substituted <τ_0_0> (Float, Double, @in_guaranteed τ_0_0) -> Float for <S>) {
  // CHECK: [[JVP2_ORIG_FNREF:%.*]] = function_ref {{.*}}function2{{.*}} : $@convention(method) (Float, Double, S) -> Float
  // CHECK: [[JVP2_VJP_FNREF:%.*]] = differentiability_witness_function [vjp] [parameters 0 1 2] [results 0] @{{.*}}function2{{.*}}
  // CHECK: [[JVP2_ADFUNC:%.*]] = differentiable_function [parameters 0 1 2] [[JVP2_ORIG_FNREF]] : {{.*}} with_derivative {{{%.*}} : {{.*}}, [[JVP2_VJP_FNREF]] : {{.*}}}
  // CHECK: [[JVP2:%.*]] = differentiable_function_extract [jvp] [[JVP2_ADFUNC]] : $@differentiable @convention(method) (Float, Double, S) -> Float
  // CHECK: apply [[JVP2]]
  // CHECK: } // end sil function 'AD__{{.*}}function2{{.*}}_jvp_SSS'

  // CHECK-LABEL: sil {{.*}} @AD__{{.*}}function2{{.*}}_vjp_SSS : $@convention(witness_method: Proto) (Float, Double, @in_guaranteed S) -> (Float, @owned @callee_guaranteed @substituted <τ_0_0> (Float) -> (Float, Double, @out τ_0_0) for <S>) {
  // CHECK: [[VJP2_ORIG_FNREF:%.*]] = function_ref {{.*}}function2{{.*}} : $@convention(method) (Float, Double, S) -> Float
  // CHECK: [[VJP2_VJP_FNREF:%.*]] = differentiability_witness_function [vjp] [parameters 0 1 2] [results 0] @{{.*}}function2{{.*}}
  // CHECK: [[VJP2_ADFUNC:%.*]] = differentiable_function [parameters 0 1 2] [[VJP2_ORIG_FNREF]] : {{.*}} with_derivative {{{%.*}} : {{.*}}, [[VJP2_VJP_FNREF]] : {{.*}}}
  // CHECK: [[VJP2:%.*]] = differentiable_function_extract [vjp] [[VJP2_ADFUNC]] : $@differentiable @convention(method) (Float, Double, S) -> Float
  // CHECK: apply [[VJP2]]
  // CHECK: } // end sil function 'AD__{{.*}}function2{{.*}}_vjp_SSS'

  @differentiable(wrt: (y))
  func function3(_ x: Float, _ y: Double) -> Double {
    return y
  }

  // CHECK-LABEL: sil {{.*}} @AD__{{.*}}function3{{.*}}_jvp_USU : $@convention(witness_method: Proto) (Float, Double, @in_guaranteed S) -> (Double, @owned @callee_guaranteed (Double) -> Double) {
  // CHECK: [[JVP3_ORIG_FNREF:%.*]] = function_ref {{.*}}function3{{.*}} : $@convention(method) (Float, Double, S) -> Double
  // CHECK: [[JVP3_VJP_FNREF:%.*]] = differentiability_witness_function [vjp] [parameters 1] [results 0] @{{.*}}function3{{.*}}
  // CHECK: [[JVP3_ADFUNC:%.*]] = differentiable_function [parameters 1] [[JVP3_ORIG_FNREF]] : {{.*}} with_derivative {{{%.*}} : {{.*}}, [[JVP3_VJP_FNREF]] : {{.*}}}
  // CHECK: [[JVP3:%.*]] = differentiable_function_extract [jvp] [[JVP3_ADFUNC]] : $@differentiable @convention(method) (@noDerivative Float, Double, @noDerivative S) -> Double
  // CHECK: apply [[JVP3]]
  // CHECK: } // end sil function 'AD__{{.*}}function3{{.*}}_jvp_USU'

  // CHECK-LABEL: sil {{.*}} @AD__{{.*}}function3{{.*}}_vjp_USU : $@convention(witness_method: Proto) (Float, Double, @in_guaranteed S) -> (Double, @owned @callee_guaranteed (Double) -> Double) {
  // CHECK: [[VJP3_ORIG_FNREF:%.*]] = function_ref {{.*}}function3{{.*}} : $@convention(method) (Float, Double, S) -> Double
  // CHECK: [[VJP3_VJP_FNREF:%.*]] = differentiability_witness_function [vjp] [parameters 1] [results 0] @{{.*}}function3{{.*}}
  // CHECK: [[VJP3_ADFUNC:%.*]] = differentiable_function [parameters 1] [[VJP3_ORIG_FNREF]] : {{.*}} with_derivative {{{%.*}} : {{.*}}, [[VJP3_VJP_FNREF]] : {{.*}}}
  // CHECK: [[VJP3:%.*]] = differentiable_function_extract [vjp] [[VJP3_ADFUNC]] : $@differentiable @convention(method) (@noDerivative Float, Double, @noDerivative S) -> Double
  // CHECK: apply [[VJP3]]
  // CHECK: } // end sil function 'AD__{{.*}}function3{{.*}}_vjp_USU'
}

// CHECK-LABEL: sil_witness_table hidden S: Proto module witness_table_sil {
// CHECK-NEXT:  base_protocol Differentiable: S: Differentiable module witness_table_sil
// CHECK-NEXT:  method #Proto.function1!1: <Self where Self : Proto> (Self) -> (Float, Double) -> Float : @{{.*}}function1
// CHECK-NEXT:  method #Proto.function1!1.jvp.SSU.<Self where Self : Proto>: <Self where Self : Proto> (Self) -> (Float, Double) -> Float : @AD__{{.*}}function1{{.*}}_jvp_SSU
// CHECK-NEXT:  method #Proto.function1!1.vjp.SSU.<Self where Self : Proto>: <Self where Self : Proto> (Self) -> (Float, Double) -> Float : @AD__{{.*}}function1{{.*}}_vjp_SSU
// CHECK-NEXT:  method #Proto.function2!1: <Self where Self : Proto> (Self) -> (Float, Double) -> Float : @{{.*}}function2
// CHECK-NEXT:  method #Proto.function2!1.jvp.SSS.<Self where Self : Proto>: <Self where Self : Proto> (Self) -> (Float, Double) -> Float : @AD__{{.*}}function2{{.*}}_jvp_SSS
// CHECK-NEXT:  method #Proto.function2!1.vjp.SSS.<Self where Self : Proto>: <Self where Self : Proto> (Self) -> (Float, Double) -> Float : @AD__{{.*}}function2{{.*}}_vjp_SSS
// CHECK-NEXT:  method #Proto.function3!1: <Self where Self : Proto> (Self) -> (Float, Double) -> Double : @{{.*}}function3
// CHECK-NEXT:  method #Proto.function3!1.jvp.USU.<Self where Self : Proto>: <Self where Self : Proto> (Self) -> (Float, Double) -> Double : @AD__{{.*}}function3{{.*}}_jvp_USU
// CHECK-NEXT:  method #Proto.function3!1.vjp.USU.<Self where Self : Proto>: <Self where Self : Proto> (Self) -> (Float, Double) -> Double : @AD__{{.*}}function3{{.*}}_vjp_USU
// CHECK-NEXT:}
