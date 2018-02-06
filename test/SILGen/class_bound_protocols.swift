// RUN: %target-swift-frontend -parse-stdlib -parse-as-library -module-name Swift -emit-silgen %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden @_T0s19class_bound_generic{{[_0-9a-zA-Z]*}}F
func class_bound_generic<T : ClassBound>(x: T) -> T {
  var x = x
  // CHECK: bb0([[X:%.*]] : $T):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $<τ_0_0 where τ_0_0 : ClassBound> { var τ_0_0 } <T>
  // CHECK:   [[PB:%.*]] = project_box [[X_ADDR]]
  // CHECK:   [[BORROWED_X:%.*]] = begin_borrow [[X]]
  // CHECK:   [[X_COPY:%.*]] = copy_value [[BORROWED_X]]
  // CHECK:   store [[X_COPY]] to [init] [[PB]]
  return x
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*T
  // CHECK:   [[X1:%.*]] = load [copy] [[READ]]
  // CHECK:   destroy_value [[X_ADDR]]
  // CHECK:   destroy_value [[X]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_T0s21class_bound_generic_2{{[_0-9a-zA-Z]*}}F
func class_bound_generic_2<T : ClassBound & NotClassBound>(x: T) -> T {
  var x = x
  // CHECK: bb0([[X:%.*]] : $T):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $<τ_0_0 where τ_0_0 : ClassBound, τ_0_0 : NotClassBound> { var τ_0_0 } <T>
  // CHECK:   [[PB:%.*]] = project_box [[X_ADDR]]
  // CHECK:   [[BORROWED_X:%.*]] = begin_borrow [[X]]
  // CHECK:   [[X_COPY:%.*]] = copy_value [[BORROWED_X]]
  // CHECK:   store [[X_COPY]] to [init] [[PB]]
  // CHECK:   end_borrow [[BORROWED_X]] from [[X]]
  return x
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*T
  // CHECK:   [[X1:%.*]] = load [copy] [[READ]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_T0s20class_bound_protocol{{[_0-9a-zA-Z]*}}F
func class_bound_protocol(x: ClassBound) -> ClassBound {
  var x = x
  // CHECK: bb0([[X:%.*]] : $ClassBound):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box ${ var ClassBound }
  // CHECK:   [[PB:%.*]] = project_box [[X_ADDR]]
  // CHECK:   [[BORROWED_X:%.*]] = begin_borrow [[X]]
  // CHECK:   [[X_COPY:%.*]] = copy_value [[BORROWED_X]]
  // CHECK:   store [[X_COPY]] to [init] [[PB]]
  // CHECK:   end_borrow [[BORROWED_X]] from [[X]]
  return x
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*ClassBound
  // CHECK:   [[X1:%.*]] = load [copy] [[READ]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_T0s32class_bound_protocol_composition{{[_0-9a-zA-Z]*}}F
func class_bound_protocol_composition(x: ClassBound & NotClassBound)
-> ClassBound & NotClassBound {
  var x = x
  // CHECK: bb0([[X:%.*]] : $ClassBound & NotClassBound):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box ${ var ClassBound & NotClassBound }
  // CHECK:   [[PB:%.*]] = project_box [[X_ADDR]]
  // CHECK:   [[BORROWED_X:%.*]] = begin_borrow [[X]]
  // CHECK:   [[X_COPY:%.*]] = copy_value [[BORROWED_X]]
  // CHECK:   store [[X_COPY]] to [init] [[PB]]
  // CHECK:   end_borrow [[BORROWED_X]] from [[X]]
  return x
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*ClassBound & NotClassBound
  // CHECK:   [[X1:%.*]] = load [copy] [[READ]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_T0s19class_bound_erasure{{[_0-9a-zA-Z]*}}F
func class_bound_erasure(x: ConcreteClass) -> ClassBound {
  return x
  // CHECK: [[PROTO:%.*]] = init_existential_ref {{%.*}} : $ConcreteClass, $ClassBound
  // CHECK: return [[PROTO]]
}

// CHECK-LABEL: sil hidden @_T0s30class_bound_existential_upcasts10ClassBound_psAB_s0E6Bound2p1x_tF :
func class_bound_existential_upcast(x: ClassBound & ClassBound2)
-> ClassBound {
  return x
  // CHECK: bb0([[ARG:%.*]] : $ClassBound & ClassBound2):
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   [[OPENED:%.*]] = open_existential_ref [[BORROWED_ARG]] : $ClassBound & ClassBound2 to [[OPENED_TYPE:\$@opened(.*) ClassBound & ClassBound2]]
  // CHECK:   [[OPENED_COPY:%.*]] = copy_value [[OPENED]]
  // CHECK:   [[PROTO:%.*]] = init_existential_ref [[OPENED_COPY]] : [[OPENED_TYPE]] : [[OPENED_TYPE]], $ClassBound
  // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
  // CHECK:   destroy_value [[ARG]]
  // CHECK:   return [[PROTO]]
}
// CHECK: } // end sil function '_T0s30class_bound_existential_upcasts10ClassBound_psAB_s0E6Bound2p1x_tF'

// CHECK-LABEL: sil hidden @_T0s41class_bound_to_unbound_existential_upcasts13NotClassBound_ps0hI0_sABp1x_tF :
// CHECK: bb0([[ARG0:%.*]] : $*NotClassBound, [[ARG1:%.*]] : $ClassBound & NotClassBound):
// CHECK:   [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG1]]
// CHECK:   [[X_OPENED:%.*]] = open_existential_ref [[BORROWED_ARG1]] : $ClassBound & NotClassBound to [[OPENED_TYPE:\$@opened(.*) ClassBound & NotClassBound]]
// CHECK:   [[PAYLOAD_ADDR:%.*]] = init_existential_addr [[ARG0]] : $*NotClassBound, [[OPENED_TYPE]]
// CHECK:   [[X_OPENED_COPY:%.*]] = copy_value [[X_OPENED]]
// CHECK:   store [[X_OPENED_COPY]] to [init] [[PAYLOAD_ADDR]]
// CHECK:   end_borrow [[BORROWED_ARG1]] from [[ARG1]]
// CHECK:   destroy_value [[ARG1]]
func class_bound_to_unbound_existential_upcast
(x: ClassBound & NotClassBound) -> NotClassBound {
  return x
}

// CHECK-LABEL: sil hidden @_T0s18class_bound_methodys10ClassBound_p1x_tF :
// CHECK: bb0([[ARG:%.*]] : $ClassBound):
func class_bound_method(x: ClassBound) {
  var x = x
  x.classBoundMethod()
  // CHECK: [[XBOX:%.*]] = alloc_box ${ var ClassBound }, var, name "x"
  // CHECK: [[XBOX_PB:%.*]] = project_box [[XBOX]]
  // CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK: [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
  // CHECK: store [[ARG_COPY]] to [init] [[XBOX_PB]]
  // CHECK: end_borrow [[BORROWED_ARG]] from [[ARG]]
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[XBOX_PB]] : $*ClassBound
  // CHECK: [[X:%.*]] = load [copy] [[READ]] : $*ClassBound
  // CHECK: [[PROJ:%.*]] = open_existential_ref [[X]] : $ClassBound to $[[OPENED:@opened(.*) ClassBound]]
  // CHECK: [[METHOD:%.*]] = witness_method $[[OPENED]], #ClassBound.classBoundMethod!1
  // CHECK: apply [[METHOD]]<[[OPENED]]>([[PROJ]])
  // CHECK: destroy_value [[PROJ]]
  // CHECK: destroy_value [[XBOX]]
  // CHECK: destroy_value [[ARG]]
}
// CHECK: } // end sil function '_T0s18class_bound_methodys10ClassBound_p1x_tF'

// rdar://problem/31858378
struct Value {}

protocol HasMutatingMethod {
  mutating func mutateMe()
  var mutatingCounter: Value { get set }
  var nonMutatingCounter: Value { get nonmutating set }
}

protocol InheritsMutatingMethod : class, HasMutatingMethod {}

func takesInOut<T>(_: inout T) {}

// CHECK-LABEL: sil hidden @_T0s27takesInheritsMutatingMethodys0bcD0_pz1x_s5ValueV1ytF : $@convention(thin) (@inout InheritsMutatingMethod, Value) -> () {
func takesInheritsMutatingMethod(x: inout InheritsMutatingMethod,
                                 y: Value) {
  // CHECK:      [[X_ADDR:%.*]] = begin_access [modify] [unknown] %0 : $*InheritsMutatingMethod
  // CHECK-NEXT: [[X_VALUE:%.*]] = load [copy] [[X_ADDR]] : $*InheritsMutatingMethod
  // CHECK-NEXT: [[X_PAYLOAD:%.*]] = open_existential_ref [[X_VALUE]] : $InheritsMutatingMethod to $@opened("{{.*}}") InheritsMutatingMethod
  // CHECK-NEXT: [[TEMPORARY:%.*]] = alloc_stack $@opened("{{.*}}") InheritsMutatingMethod
  // CHECK-NEXT: store [[X_PAYLOAD]] to [init] [[TEMPORARY]] : $*@opened("{{.*}}") InheritsMutatingMethod
  // CHECK-NEXT: [[METHOD:%.*]] = witness_method $@opened("{{.*}}") InheritsMutatingMethod, #HasMutatingMethod.mutateMe!1 : <Self where Self : HasMutatingMethod> (inout Self) -> () -> (), [[X_PAYLOAD]] : $@opened("{{.*}}") InheritsMutatingMethod : $@convention(witness_method: HasMutatingMethod) <τ_0_0 where τ_0_0 : HasMutatingMethod> (@inout τ_0_0) -> ()
  // CHECK-NEXT: apply [[METHOD]]<@opened("{{.*}}") InheritsMutatingMethod>([[TEMPORARY]]) : $@convention(witness_method: HasMutatingMethod) <τ_0_0 where τ_0_0 : HasMutatingMethod> (@inout τ_0_0) -> ()
  // CHECK-NEXT: [[X_PAYLOAD:%.*]] = load [take] [[TEMPORARY]] : $*@opened("{{.*}}") InheritsMutatingMethod
  // CHECK-NEXT: [[X_VALUE:%.*]] = init_existential_ref [[X_PAYLOAD]] : $@opened("{{.*}}") InheritsMutatingMethod : $@opened("{{.*}}") InheritsMutatingMethod, $InheritsMutatingMethod
  // CHECK-NEXT: assign [[X_VALUE]] to [[X_ADDR]] : $*InheritsMutatingMethod
  // CHECK-NEXT: end_access [[X_ADDR]] : $*InheritsMutatingMethod
  // CHECK-NEXT: dealloc_stack [[TEMPORARY]] : $*@opened("{{.*}}") InheritsMutatingMethod
  x.mutateMe()

  // CHECK-NEXT: [[RESULT_BOX:%.*]] = alloc_stack $Value
  // CHECK-NEXT: [[RESULT:%.*]] = mark_uninitialized [var] [[RESULT_BOX]] : $*Value
  // CHECK-NEXT: [[X_ADDR:%.*]] = begin_access [read] [unknown] %0 : $*InheritsMutatingMethod
  // CHECK-NEXT: [[X_VALUE:%.*]] = load [copy] [[X_ADDR]] : $*InheritsMutatingMethod
  // CHECK-NEXT: [[X_PAYLOAD:%.*]] = open_existential_ref [[X_VALUE]] : $InheritsMutatingMethod to $@opened("{{.*}}") InheritsMutatingMethod
  // CHECK-NEXT: [[TEMPORARY:%.*]] = alloc_stack $@opened("{{.*}}") InheritsMutatingMethod
  // CHECK-NEXT: store [[X_PAYLOAD]] to [init] [[TEMPORARY]] : $*@opened("{{.*}}") InheritsMutatingMethod
  // CHECK-NEXT: [[X_PAYLOAD_RELOADED:%.*]] = load_borrow [[TEMPORARY]]
  //
  // ** *NOTE* This extra copy is here since RValue invariants enforce that all
  // ** loadable objects are actually loaded. So we form the RValue and
  // ** load... only to then need to store the value back in a stack location to
  // ** pass to an in_guaranteed method. PredictableMemOpts is able to handle this
  // ** type of temporary codegen successfully.
  // CHECK-NEXT: [[TEMPORARY_2:%.*]] = alloc_stack $@opened("{{.*}}") InheritsMutatingMethod
  // CHECK-NEXT: store_borrow [[X_PAYLOAD_RELOADED:%.*]] to [[TEMPORARY_2]]
  // 
  // CHECK-NEXT: [[METHOD:%.*]] = witness_method $@opened("{{.*}}") InheritsMutatingMethod, #HasMutatingMethod.mutatingCounter!getter.1 : <Self where Self : HasMutatingMethod> (Self) -> () -> Value, [[X_PAYLOAD]] : $@opened("{{.*}}") InheritsMutatingMethod : $@convention(witness_method: HasMutatingMethod) <τ_0_0 where τ_0_0 : HasMutatingMethod> (@in_guaranteed τ_0_0) -> Value
  // CHECK-NEXT: [[RESULT_VALUE:%.*]] = apply [[METHOD]]<@opened("{{.*}}") InheritsMutatingMethod>([[TEMPORARY_2]]) : $@convention(witness_method: HasMutatingMethod) <τ_0_0 where τ_0_0 : HasMutatingMethod> (@in_guaranteed τ_0_0) -> Value
  // CHECK-NEXT: dealloc_stack  [[TEMPORARY_2]]
  // CHECK-NEXT: end_borrow [[X_PAYLOAD_RELOADED]]
  // CHECK-NEXT: assign [[RESULT_VALUE]] to [[RESULT]] : $*Value
  // CHECK-NEXT: destroy_addr [[TEMPORARY]]
  // CHECK-NEXT: end_access [[X_ADDR]]
  // CHECK-NEXT: dealloc_stack [[TEMPORARY]] : $*@opened("{{.*}}") InheritsMutatingMethod
  // CHECK-NEXT: dealloc_stack [[RESULT_BOX]] : $*Value
  _ = x.mutatingCounter

  // CHECK-NEXT: [[X_ADDR:%.*]] = begin_access [modify] [unknown] %0 : $*InheritsMutatingMethod
  // CHECK-NEXT: [[X_VALUE:%.*]] = load [copy] [[X_ADDR]] : $*InheritsMutatingMethod
  // CHECK-NEXT: [[X_PAYLOAD:%.*]] = open_existential_ref [[X_VALUE]] : $InheritsMutatingMethod to $@opened("{{.*}}") InheritsMutatingMethod
  // CHECK-NEXT: [[TEMPORARY:%.*]] = alloc_stack $@opened("{{.*}}") InheritsMutatingMethod
  // CHECK-NEXT: store [[X_PAYLOAD]] to [init] [[TEMPORARY]] : $*@opened("{{.*}}") InheritsMutatingMethod
  // CHECK-NEXT: [[METHOD:%.*]] = witness_method $@opened("{{.*}}") InheritsMutatingMethod, #HasMutatingMethod.mutatingCounter!setter.1 : <Self where Self : HasMutatingMethod> (inout Self) -> (Value) -> (), [[X_PAYLOAD]] : $@opened("{{.*}}") InheritsMutatingMethod : $@convention(witness_method: HasMutatingMethod) <τ_0_0 where τ_0_0 : HasMutatingMethod> (Value, @inout τ_0_0) -> ()
  // CHECK-NEXT: apply [[METHOD]]<@opened("{{.*}}") InheritsMutatingMethod>(%1, [[TEMPORARY]]) : $@convention(witness_method: HasMutatingMethod) <τ_0_0 where τ_0_0 : HasMutatingMethod> (Value, @inout τ_0_0) -> ()
  // CHECK-NEXT: [[X_PAYLOAD:%.*]] = load [take] [[TEMPORARY]] : $*@opened("{{.*}}") InheritsMutatingMethod
  // CHECK-NEXT: [[X_VALUE:%.*]] = init_existential_ref [[X_PAYLOAD]] : $@opened("{{.*}}") InheritsMutatingMethod : $@opened("{{.*}}") InheritsMutatingMethod, $InheritsMutatingMethod
  // CHECK-NEXT: assign [[X_VALUE]] to [[X_ADDR]] : $*InheritsMutatingMethod
  // CHECK-NEXT: end_access [[X_ADDR]] : $*InheritsMutatingMethod
  // CHECK-NEXT: dealloc_stack [[TEMPORARY]] : $*@opened("{{.*}}") InheritsMutatingMethod
  x.mutatingCounter = y

  takesInOut(&x.mutatingCounter)

  _ = x.nonMutatingCounter
  x.nonMutatingCounter = y
  takesInOut(&x.nonMutatingCounter)
}
