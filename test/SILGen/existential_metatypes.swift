// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen -parse-stdlib %s | %FileCheck %s

protocol P {
  init()
  static func staticMethod() -> Self
}

struct S: P {
  init() {}
  static func staticMethod() -> S { return S() }
}

// CHECK-LABEL: sil hidden @_T021existential_metatypes0A8MetatypeyAA1P_pF
// CHECK: bb0([[X:%.*]] : $*P):
func existentialMetatype(_ x: P) {
  // CHECK: [[TYPE1:%.*]] = existential_metatype $@thick P.Type, [[X]]
  let type1 = type(of: x)
  // CHECK: [[INSTANCE1:%.*]] = alloc_stack $P
  // CHECK: [[OPEN_TYPE1:%.*]] = open_existential_metatype [[TYPE1]]
  // CHECK: [[INSTANCE1_VALUE:%.*]] = init_existential_addr [[INSTANCE1]] : $*P
  // CHECK: [[INIT:%.*]] = witness_method {{.*}} #P.init!allocator
  // CHECK: apply [[INIT]]<{{.*}}>([[INSTANCE1_VALUE]], [[OPEN_TYPE1]])
  let instance1 = type1.init()

  // CHECK: [[S:%.*]] = metatype $@thick S.Type
  // CHECK: [[TYPE2:%.*]] = init_existential_metatype [[S]] : $@thick S.Type, $@thick P.Type
  let type2: P.Type = S.self
  // CHECK: [[INSTANCE2:%.*]] = alloc_stack $P
  // CHECK: [[OPEN_TYPE2:%.*]] = open_existential_metatype [[TYPE2]]
  // CHECK: [[INSTANCE2_VALUE:%.*]] = init_existential_addr [[INSTANCE2]] : $*P
  // CHECK: [[STATIC_METHOD:%.*]] = witness_method {{.*}} #P.staticMethod
  // CHECK: apply [[STATIC_METHOD]]<{{.*}}>([[INSTANCE2_VALUE]], [[OPEN_TYPE2]])
  let instance2 = type2.staticMethod()
}

protocol PP: P {}
protocol Q {}

// CHECK-LABEL: sil hidden @_T021existential_metatypes0A15MetatypeUpcast1AA1P_pXpAA2PP_pXpF
// CHECK:         [[OPENED:%.*]] = open_existential_metatype %0
// CHECK:         [[NEW:%.*]] = init_existential_metatype [[OPENED]]
// CHECK:         return [[NEW]]
func existentialMetatypeUpcast1(_ x: PP.Type) -> P.Type {
  return x
}

// CHECK-LABEL: sil hidden @_T021existential_metatypes0A15MetatypeUpcast2AA1P_pXpAaC_AA1QpXpF
// CHECK:         [[OPENED:%.*]] = open_existential_metatype %0
// CHECK:         [[NEW:%.*]] = init_existential_metatype [[OPENED]]
// CHECK:         return [[NEW]]
func existentialMetatypeUpcast2(_ x: (P & Q).Type) -> P.Type {
  return x
}
