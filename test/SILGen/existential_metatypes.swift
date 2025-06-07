// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-stdlib %s | %FileCheck %s

@_semantics("typechecker.type(of:)")
public func type<T, Metatype>(of value: T) -> Metatype {}

struct Value {
  func method() {}
}

protocol P {
  init()
  static func staticMethod() -> Self
  static var value: Value { get }
}

struct S: P {
  init() {}
  static func staticMethod() -> S { return S() }
  static var value: Value { return Value() }
}

// CHECK-LABEL: sil hidden [ossa] @$s21existential_metatypes0A8MetatypeyyAA1P_pF
// CHECK: bb0([[X:%.*]] : $*any P):
func existentialMetatype(_ x: P) {
  // CHECK: [[TYPE1:%.*]] = existential_metatype $@thick any P.Type, [[X]]
  // CHECK: [[MV1:%.*]] = move_value [var_decl] [[TYPE1]] : $@thick any P.Type

  let type1 = type(of: x)
  // CHECK: [[INSTANCE1:%.*]] = alloc_stack [lexical] [var_decl] $any P
  // CHECK: [[OPEN_TYPE1:%.*]] = open_existential_metatype [[MV1]]
  // CHECK: [[INIT:%.*]] = witness_method {{.*}} #P.init!allocator
  // CHECK: [[INSTANCE1_VALUE:%.*]] = init_existential_addr [[INSTANCE1]] : $*any P
  // CHECK: apply [[INIT]]<{{.*}}>([[INSTANCE1_VALUE]], [[OPEN_TYPE1]])
  let instance1 = type1.init()

  // CHECK: [[S:%.*]] = metatype $@thick S.Type
  // CHECK: [[TYPE2:%.*]] = init_existential_metatype [[S]] : $@thick S.Type, $@thick any P.Type
  // CHECK: [[MV2:%.*]] = move_value [var_decl] [[TYPE2]] : $@thick any P.Type
  let type2: P.Type = S.self
  // CHECK: [[INSTANCE2:%.*]] = alloc_stack [lexical] [var_decl] $any P
  // CHECK: [[OPEN_TYPE2:%.*]] = open_existential_metatype [[MV2]]
  // CHECK: [[STATIC_METHOD:%.*]] = witness_method {{.*}} #P.staticMethod
  // CHECK: [[INSTANCE2_VALUE:%.*]] = init_existential_addr [[INSTANCE2]] : $*any P
  // CHECK: apply [[STATIC_METHOD]]<{{.*}}>([[INSTANCE2_VALUE]], [[OPEN_TYPE2]])
  let instance2 = type2.staticMethod()
}

protocol PP: P {}
protocol Q {}

// CHECK-LABEL: sil hidden [ossa] @$s21existential_metatypes0A15MetatypeUpcast1yAA1P_pXpAA2PP_pXpF
// CHECK:         [[OPENED:%.*]] = open_existential_metatype %0
// CHECK:         [[NEW:%.*]] = init_existential_metatype [[OPENED]]
// CHECK:         return [[NEW]]
func existentialMetatypeUpcast1(_ x: PP.Type) -> P.Type {
  return x
}

// CHECK-LABEL: sil hidden [ossa] @$s21existential_metatypes0A15MetatypeUpcast2yAA1P_pXpAaC_AA1QpXpF
// CHECK:         [[OPENED:%.*]] = open_existential_metatype %0
// CHECK:         [[NEW:%.*]] = init_existential_metatype [[OPENED]]
// CHECK:         return [[NEW]]
func existentialMetatypeUpcast2(_ x: (P & Q).Type) -> P.Type {
  return x
}

// rdar://32288618
// CHECK-LABEL: sil hidden [ossa] @$s21existential_metatypes0A19MetatypeVarPropertyAA5ValueVyF : $@convention(thin) () -> Value
func existentialMetatypeVarProperty() -> Value {
  // CHECK:      [[BOX:%.*]] = alloc_box ${ var @thick any P.Type }
  // CHECK:      [[LIFETIME:%.*]] = begin_borrow [var_decl] [[BOX]]
  // CHECK:      [[ADDR:%.*]] = project_box [[LIFETIME]] : ${ var @thick any P.Type }, 0
  // CHECK:      [[T0:%.*]] = metatype $@thick S.Type
  // CHECK:      [[T1:%.*]] = init_existential_metatype [[T0]]
  // CHECK:      store [[T1]] to [trivial] [[ADDR]] :
  // CHECK:      [[T0:%.*]] = begin_access [read] [unknown] [[ADDR]] :
  // CHECK:      [[T1:%.*]] = load [trivial] [[T0]]
  // CHECK:      open_existential_metatype [[T1]] :
  var type: P.Type = S.self
  return type.value
}

// rdar://45956703
// CHECK-LABEL: sil hidden [ossa] @$s21existential_metatypes31getterResultStaticStorageAccessyyF
var _type: P.Type { get {return S.self } set {} }
func getterResultStaticStorageAccess() {
  // CHECK:      [[GET_TYPE:%.*]] = function_ref @$s21existential_metatypes5_typeAA1P_pXpvg
  // CHECK-NEXT: [[TYPE:%.*]] = apply [[GET_TYPE]]() : $@convention(thin) () -> @thick any P.Type
  // CHECK-NEXT: [[OPEN:%.*]] = open_existential_metatype [[TYPE]] : $@thick any P.Type to $@thick ([[ARCHETYPE:@opened\(.*, any P\) Self]]).Type
  // CHECK-NEXT: [[GET_VALUE:%.*]] = witness_method $[[ARCHETYPE]], #P.value!getter
  // CHECK-NEXT: [[VALUE:%.*]] = apply [[GET_VALUE]]<[[ARCHETYPE]]>([[OPEN]])
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[METHOD:%.*]] = function_ref @$s21existential_metatypes5ValueV6methodyyF
  // CHECK-NEXT: apply [[METHOD]]([[VALUE]])
  _type.value.method()
}
