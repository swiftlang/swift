
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-stdlib -parse-as-library -module-name Swift %s | %FileCheck %s

enum Optional<T> {
  case some(T)
  case none
}

precedencegroup AssignmentPrecedence {}

typealias AnyObject = Builtin.AnyObject

// -- Class-bound archetypes and existentials are *not* address-only and can
//    be manipulated using normal reference type value semantics.

protocol NotClassBound {
  func notClassBoundMethod()
}
protocol ClassBound : class {
  func classBoundMethod()
}

protocol ClassBound2 : class {
  func classBound2Method()
}

class ConcreteClass : NotClassBound, ClassBound, ClassBound2 {
  func notClassBoundMethod() {}
  func classBoundMethod() {}
  func classBound2Method() {}
}

class ConcreteSubclass : ConcreteClass { }

// CHECK-LABEL: sil hidden [ossa] @$ss19class_bound_generic{{[_0-9a-zA-Z]*}}F
func class_bound_generic<T : ClassBound>(x: T) -> T {
  var x = x
  // CHECK: bb0([[X:%.*]] : @guaranteed $T):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $<τ_0_0 where τ_0_0 : ClassBound> { var τ_0_0 } <T>
  // CHECK:   [[X_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[X_ADDR]]
  // CHECK:   [[PB:%.*]] = project_box [[X_LIFETIME]]
  // CHECK:   [[X_COPY:%.*]] = copy_value [[X]]
  // CHECK:   store [[X_COPY]] to [init] [[PB]]
  return x
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*T
  // CHECK:   [[X1:%.*]] = load [copy] [[READ]]
  // CHECK:   end_borrow [[X_LIFETIME]]
  // CHECK:   destroy_value [[X_ADDR]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden [ossa] @$ss21class_bound_generic_2{{[_0-9a-zA-Z]*}}F
func class_bound_generic_2<T : ClassBound & NotClassBound>(x: T) -> T {
  var x = x
  // CHECK: bb0([[X:%.*]] : @guaranteed $T):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $<τ_0_0 where τ_0_0 : ClassBound, τ_0_0 : NotClassBound> { var τ_0_0 } <T>
  // CHECK:   [[X_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[X_ADDR]]
  // CHECK:   [[PB:%.*]] = project_box [[X_LIFETIME]]
  // CHECK:   [[X_COPY:%.*]] = copy_value [[X]]
  // CHECK:   store [[X_COPY]] to [init] [[PB]]
  return x
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*T
  // CHECK:   [[X1:%.*]] = load [copy] [[READ]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden [ossa] @$ss20class_bound_protocol{{[_0-9a-zA-Z]*}}F
func class_bound_protocol(x: ClassBound) -> ClassBound {
  var x = x
  // CHECK: bb0([[X:%.*]] : @guaranteed $any ClassBound):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box ${ var any ClassBound }
  // CHECK:   [[X_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[X_ADDR]]
  // CHECK:   [[PB:%.*]] = project_box [[X_LIFETIME]]
  // CHECK:   [[X_COPY:%.*]] = copy_value [[X]]
  // CHECK:   store [[X_COPY]] to [init] [[PB]]
  return x
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*any ClassBound
  // CHECK:   [[X1:%.*]] = load [copy] [[READ]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden [ossa] @$ss32class_bound_protocol_composition{{[_0-9a-zA-Z]*}}F
func class_bound_protocol_composition(x: ClassBound & NotClassBound)
-> ClassBound & NotClassBound {
  var x = x
  // CHECK: bb0([[X:%.*]] : @guaranteed $any ClassBound & NotClassBound):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box ${ var any ClassBound & NotClassBound }
  // CHECK:   [[X_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[X_ADDR]]
  // CHECK:   [[PB:%.*]] = project_box [[X_LIFETIME]]
  // CHECK:   [[X_COPY:%.*]] = copy_value [[X]]
  // CHECK:   store [[X_COPY]] to [init] [[PB]]
  return x
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*any ClassBound & NotClassBound
  // CHECK:   [[X1:%.*]] = load [copy] [[READ]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden [ossa] @$ss19class_bound_erasure{{[_0-9a-zA-Z]*}}F
func class_bound_erasure(x: ConcreteClass) -> ClassBound {
  return x
  // CHECK: [[PROTO:%.*]] = init_existential_ref {{%.*}} : $ConcreteClass, $any ClassBound
  // CHECK: return [[PROTO]]
}

// CHECK-LABEL: sil hidden [ossa] @$ss30class_bound_existential_upcast1xs10ClassBound_psAC_s0E6Bound2p_tF :
func class_bound_existential_upcast(x: ClassBound & ClassBound2)
-> ClassBound {
  return x
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $any ClassBound & ClassBound2):
  // CHECK:   [[OPENED:%.*]] = open_existential_ref [[ARG]] : $any ClassBound & ClassBound2 to [[OPENED_TYPE:\$@opened\(.*, any ClassBound & ClassBound2\) Self]]
  // CHECK:   [[OPENED_COPY:%.*]] = copy_value [[OPENED]]
  // CHECK:   [[PROTO:%.*]] = init_existential_ref [[OPENED_COPY]] : [[OPENED_TYPE]] : [[OPENED_TYPE]], $any ClassBound
  // CHECK:   return [[PROTO]]
}
// CHECK: } // end sil function '$ss30class_bound_existential_upcast1xs10ClassBound_psAC_s0E6Bound2p_tF'

// CHECK-LABEL: sil hidden [ossa] @$ss41class_bound_to_unbound_existential_upcast1xs13NotClassBound_ps0hI0_sACp_tF :
// CHECK: bb0([[ARG0:%.*]] : $*any NotClassBound, [[ARG1:%.*]] : @guaranteed $any ClassBound & NotClassBound):
// CHECK:   [[X_OPENED:%.*]] = open_existential_ref [[ARG1]] : $any ClassBound & NotClassBound to [[OPENED_TYPE:\$@opened\(.*, any ClassBound & NotClassBound\) Self]]
// CHECK:   [[PAYLOAD_ADDR:%.*]] = init_existential_addr [[ARG0]] : $*any NotClassBound, [[OPENED_TYPE]]
// CHECK:   [[X_OPENED_COPY:%.*]] = copy_value [[X_OPENED]]
// CHECK:   store [[X_OPENED_COPY]] to [init] [[PAYLOAD_ADDR]]
func class_bound_to_unbound_existential_upcast
(x: ClassBound & NotClassBound) -> NotClassBound {
  return x
}

// CHECK-LABEL: sil hidden [ossa] @$ss18class_bound_method1xys10ClassBound_p_tF :
// CHECK: bb0([[ARG:%.*]] : @guaranteed $any ClassBound):
func class_bound_method(x: ClassBound) {
  var x = x
  x.classBoundMethod()
  // CHECK: [[XBOX:%.*]] = alloc_box ${ var any ClassBound }, var, name "x"
  // CHECK: [[XLIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[XBOX]]
  // CHECK: [[XBOX_PB:%.*]] = project_box [[XLIFETIME]]
  // CHECK: [[ARG_COPY:%.*]] = copy_value [[ARG]]
  // CHECK: store [[ARG_COPY]] to [init] [[XBOX_PB]]
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[XBOX_PB]] : $*any ClassBound
  // CHECK: [[X:%.*]] = load [copy] [[READ]] : $*any ClassBound
  // CHECK: [[PROJ:%.*]] = open_existential_ref [[X]] : $any ClassBound to $[[OPENED:@opened\(.*, any ClassBound\) Self]]
  // CHECK: [[METHOD:%.*]] = witness_method $[[OPENED]], #ClassBound.classBoundMethod :
  // CHECK: apply [[METHOD]]<[[OPENED]]>([[PROJ]])
  // CHECK: destroy_value [[PROJ]]
  // CHECK: end_borrow [[XLIFETIME]]
  // CHECK: destroy_value [[XBOX]]
}
// CHECK: } // end sil function '$ss18class_bound_method1xys10ClassBound_p_tF'

// rdar://problem/31858378
struct Value {}

protocol HasMutatingMethod {
  mutating func mutateMe()
  var mutatingCounter: Value { get set }
  var nonMutatingCounter: Value { get nonmutating set }
}

protocol InheritsMutatingMethod : class, HasMutatingMethod {}

func takesInOut<T>(_: inout T) {}

// CHECK-LABEL: sil hidden [ossa] @$ss27takesInheritsMutatingMethod1x1yys0bcD0_pz_s5ValueVtF : $@convention(thin) (@inout any InheritsMutatingMethod, Value) -> () {
func takesInheritsMutatingMethod(x: inout InheritsMutatingMethod,
                                 y: Value) {
  // CHECK:      [[X_ADDR:%.*]] = begin_access [modify] [unknown] %0 : $*any InheritsMutatingMethod
  // CHECK-NEXT: [[X_VALUE:%.*]] = load [copy] [[X_ADDR]] : $*any InheritsMutatingMethod
  // CHECK-NEXT: [[X_PAYLOAD:%.*]] = open_existential_ref [[X_VALUE]] : $any InheritsMutatingMethod to $@opened("{{.*}}", any InheritsMutatingMethod) Self
  // CHECK-NEXT: [[TEMPORARY:%.*]] = alloc_stack $@opened("{{.*}}", any InheritsMutatingMethod) Self
  // CHECK-NEXT: store [[X_PAYLOAD]] to [init] [[TEMPORARY]] : $*@opened("{{.*}}", any InheritsMutatingMethod) Self
  // CHECK-NEXT: [[METHOD:%.*]] = witness_method $@opened("{{.*}}", any InheritsMutatingMethod) Self, #HasMutatingMethod.mutateMe : <Self where Self : HasMutatingMethod> (inout Self) -> () -> (), [[X_PAYLOAD]] : $@opened("{{.*}}", any InheritsMutatingMethod) Self : $@convention(witness_method: HasMutatingMethod) <τ_0_0 where τ_0_0 : HasMutatingMethod> (@inout τ_0_0) -> ()
  // CHECK-NEXT: apply [[METHOD]]<@opened("{{.*}}", any InheritsMutatingMethod) Self>([[TEMPORARY]]) : $@convention(witness_method: HasMutatingMethod) <τ_0_0 where τ_0_0 : HasMutatingMethod> (@inout τ_0_0) -> ()
  // CHECK-NEXT: [[X_PAYLOAD:%.*]] = load [take] [[TEMPORARY]] : $*@opened("{{.*}}", any InheritsMutatingMethod) Self
  // CHECK-NEXT: [[X_VALUE:%.*]] = init_existential_ref [[X_PAYLOAD]] : $@opened("{{.*}}", any InheritsMutatingMethod) Self : $@opened("{{.*}}", any InheritsMutatingMethod) Self, $any InheritsMutatingMethod
  // CHECK-NEXT: assign [[X_VALUE]] to [[X_ADDR]] : $*any InheritsMutatingMethod
  // CHECK-NEXT: end_access [[X_ADDR]] : $*any InheritsMutatingMethod
  // CHECK-NEXT: dealloc_stack [[TEMPORARY]] : $*@opened("{{.*}}", any InheritsMutatingMethod) Self
  x.mutateMe()

  // CHECK-NEXT: [[X_ADDR:%.*]] = begin_access [read] [unknown] %0 : $*any InheritsMutatingMethod
  // CHECK-NEXT: [[X_VALUE:%.*]] = load [copy] [[X_ADDR]] : $*any InheritsMutatingMethod
  // CHECK-NEXT: [[X_PAYLOAD:%.*]] = open_existential_ref [[X_VALUE]] : $any InheritsMutatingMethod to $@opened("{{.*}}", any InheritsMutatingMethod) Self
  // CHECK-NEXT: [[TEMPORARY:%.*]] = alloc_stack $@opened("{{.*}}", any InheritsMutatingMethod) Self
  // CHECK-NEXT: store [[X_PAYLOAD]] to [init] [[TEMPORARY]] : $*@opened("{{.*}}", any InheritsMutatingMethod) Self
  // CHECK-NEXT: [[METHOD:%.*]] = witness_method $@opened("{{.*}}", any InheritsMutatingMethod) Self, #HasMutatingMethod.mutatingCounter!getter : <Self where Self : HasMutatingMethod> (Self) -> () -> Value, [[X_PAYLOAD]] : $@opened("{{.*}}", any InheritsMutatingMethod) Self : $@convention(witness_method: HasMutatingMethod) <τ_0_0 where τ_0_0 : HasMutatingMethod> (@in_guaranteed τ_0_0) -> Value
  // CHECK-NEXT: [[RESULT_VALUE:%.*]] = apply [[METHOD]]<@opened("{{.*}}", any InheritsMutatingMethod) Self>([[TEMPORARY]]) : $@convention(witness_method: HasMutatingMethod) <τ_0_0 where τ_0_0 : HasMutatingMethod> (@in_guaranteed τ_0_0) -> Value
  // CHECK-NEXT: destroy_addr
  // CHECK-NEXT: end_access [[X_ADDR]] : $*any InheritsMutatingMethod
  // CHECK-NEXT: ignored_use [[RESULT_VALUE]]
  // CHECK-NEXT: dealloc_stack [[TEMPORARY]] : $*@opened("{{.*}}", any InheritsMutatingMethod) Self
  _ = x.mutatingCounter

  // CHECK-NEXT: [[X_ADDR:%.*]] = begin_access [modify] [unknown] %0 : $*any InheritsMutatingMethod
  // CHECK-NEXT: [[X_VALUE:%.*]] = load [copy] [[X_ADDR]] : $*any InheritsMutatingMethod
  // CHECK-NEXT: [[X_PAYLOAD:%.*]] = open_existential_ref [[X_VALUE]] : $any InheritsMutatingMethod to $@opened("{{.*}}", any InheritsMutatingMethod) Self
  // CHECK-NEXT: [[TEMPORARY:%.*]] = alloc_stack $@opened("{{.*}}", any InheritsMutatingMethod) Self
  // CHECK-NEXT: store [[X_PAYLOAD]] to [init] [[TEMPORARY]] : $*@opened("{{.*}}", any InheritsMutatingMethod) Self
  // CHECK-NEXT: [[METHOD:%.*]] = witness_method $@opened("{{.*}}", any InheritsMutatingMethod) Self, #HasMutatingMethod.mutatingCounter!setter : <Self where Self : HasMutatingMethod> (inout Self) -> (Value) -> (), [[X_PAYLOAD]] : $@opened("{{.*}}", any InheritsMutatingMethod) Self : $@convention(witness_method: HasMutatingMethod) <τ_0_0 where τ_0_0 : HasMutatingMethod> (Value, @inout τ_0_0) -> ()
  // CHECK-NEXT: apply [[METHOD]]<@opened("{{.*}}", any InheritsMutatingMethod) Self>(%1, [[TEMPORARY]]) : $@convention(witness_method: HasMutatingMethod) <τ_0_0 where τ_0_0 : HasMutatingMethod> (Value, @inout τ_0_0) -> ()
  // CHECK-NEXT: [[X_PAYLOAD:%.*]] = load [take] [[TEMPORARY]] : $*@opened("{{.*}}", any InheritsMutatingMethod) Self
  // CHECK-NEXT: [[X_VALUE:%.*]] = init_existential_ref [[X_PAYLOAD]] : $@opened("{{.*}}", any InheritsMutatingMethod) Self : $@opened("{{.*}}", any InheritsMutatingMethod) Self, $any InheritsMutatingMethod
  // CHECK-NEXT: assign [[X_VALUE]] to [[X_ADDR]] : $*any InheritsMutatingMethod
  // CHECK-NEXT: end_access [[X_ADDR]] : $*any InheritsMutatingMethod
  // CHECK-NEXT: dealloc_stack [[TEMPORARY]] : $*@opened("{{.*}}", any InheritsMutatingMethod) Self
  x.mutatingCounter = y

  takesInOut(&x.mutatingCounter)

  _ = x.nonMutatingCounter
  x.nonMutatingCounter = y
  takesInOut(&x.nonMutatingCounter)
}
