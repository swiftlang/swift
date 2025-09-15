// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen -disable-availability-checking -module-name main %s | %FileCheck %s

func generic<T, U>(_ f: @escaping (T) -> U) -> (T) -> U { return f }

// CHECK-LABEL: sil {{.*}}4main{{.*}}11sameGeneric
func sameGeneric<X, Y, Z>(_: X, _: Y, _ f: @escaping (Z) -> Z) -> (Z) -> Z {
// CHECK: bb0({{.*}}, [[F:%[0-9]+]] : @guaranteed $@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <Z, Z>
  // Similarly generic types should be directly substitutable
  // CHECK: [[GENERIC:%.*]] = function_ref @{{.*}}4main{{.*}}7generic
  // CHECK: [[RET:%.*]] = apply [[GENERIC]]<Z, Z>([[F]])
  // CHECK: return [[RET]]
  return generic(f)
}

// CHECK-LABEL: sil {{.*}}4main{{.*}}16concreteIndirect
func concreteIndirect(_ f: @escaping (Any) -> Any) -> (Any) -> Any {
// CHECK: bb0([[F:%[0-9]+]] : @guaranteed $@callee_guaranteed (@in_guaranteed Any) -> @out Any)
  // Any is passed indirectly, but is a concrete type, so we need to convert
  // to the generic abstraction level
  // CHECK: [[F2:%.*]] = copy_value [[F]]
  // CHECK: [[GENERIC_F:%.*]] = convert_function [[F2]] : {{.*}} to $@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <Any, Any>
  // CHECK: [[GENERIC:%.*]] = function_ref @{{.*}}4main{{.*}}7generic
  // CHECK: [[GENERIC_RET:%.*]] = apply [[GENERIC]]<Any, Any>([[GENERIC_F]])
  // CHECK: [[RET:%.*]] = convert_function [[GENERIC_RET]] : {{.*}}
  // CHECK: return [[RET]]
  return generic(f)
}

// CHECK-LABEL: sil {{.*}}4main{{.*}}14concreteDirect
func concreteDirect(_ f: @escaping (Int) -> String) -> (Int) -> String {
// CHECK: bb0([[F:%[0-9]+]] : @guaranteed $@callee_guaranteed (Int) -> @owned String):
  // Int and String are passed and returned directly, so we need both
  // thunking and conversion to the substituted form
  // CHECK: [[F2:%.*]] = copy_value [[F]]
  // CHECK: [[REABSTRACT_F:%.*]] = partial_apply {{.*}}([[F2]])
  // CHECK: [[GENERIC_F:%.*]] = convert_function [[REABSTRACT_F]] : {{.*}} to $@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <Int, String>
  // CHECK: [[GENERIC:%.*]] = function_ref @{{.*}}4main{{.*}}7generic
  // CHECK: [[GENERIC_RET:%.*]] = apply [[GENERIC]]<Int, String>([[GENERIC_F]])
  // CHECK: [[REABSTRACT_RET:%.*]] = convert_function [[GENERIC_RET]] : {{.*}}
  // CHECK: [[RET:%.*]] = partial_apply {{.*}}([[REABSTRACT_RET]])
  // CHECK: return [[RET]]

  return generic(f)
}

func genericTakesFunction<T, U>(
  _ f: @escaping ((T) -> U) -> (T) -> U
) -> ((T) -> U) -> (T) -> U { return f }

func sameGenericTakesFunction<T>(
  _ f: @escaping ((T) -> T) -> (T) -> T
) -> ((T) -> T) -> (T) -> T {
  return genericTakesFunction(f)
}

// CHECK-LABEL: sil {{.*}}4main29concreteIndirectTakesFunction
func concreteIndirectTakesFunction(
  _ f: @escaping ((Any) -> Any) -> (Any) -> Any
) -> ((Any) -> Any) -> (Any) -> Any {
// CHECK: bb0([[F:%[0-9]+]] : @guaranteed $@callee_guaranteed

  // Calling convention matches callee, but the representation of the argument
  // to `f` needs to change, so we still have to thunk
  // CHECK: [[F2:%.*]] = copy_value [[F]]
  // CHECK: [[REABSTRACT_F:%.*]] = partial_apply {{.*}}([[F2]])
  // CHECK: [[GENERIC_F:%.*]] = convert_function [[REABSTRACT_F]]
  // CHECK: [[GENERIC:%.*]] = function_ref @{{.*}}4main{{.*}}20genericTakesFunction
  // CHECK: [[GENERIC_RET:%.*]] = apply [[GENERIC]]<Any, Any>([[GENERIC_F]])
  // CHECK: [[REABSTRACT_RET:%.*]] = convert_function [[GENERIC_RET]] : {{.*}}
  // CHECK: [[RET:%.*]] = partial_apply {{.*}}([[REABSTRACT_RET]])
  // CHECK: return [[RET]]
  return genericTakesFunction(f)
}

func concreteDirectTakesFunction(
  _ f: @escaping ((Int) -> String) -> (Int) -> String
) -> ((Int) -> String) -> (Int) -> String {
  // Int and String are passed and returned directly, so we need both
  // thunking and conversion to the substituted form
  return genericTakesFunction(f)
}
