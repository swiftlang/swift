// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -parse-stdlib -parse-as-library -emit-silgen %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden @_T021class_bound_protocols0a1_B8_generic{{[_0-9a-zA-Z]*}}F
func class_bound_generic<T : ClassBound>(x: T) -> T {
  var x = x
  // CHECK: bb0([[X:%.*]] : $T):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $<τ_0_0 where τ_0_0 : ClassBound> { var τ_0_0 } <T>
  // CHECK:   [[PB:%.*]] = project_box [[X_ADDR]]
  // CHECK:   [[BORROWED_X:%.*]] = begin_borrow [[X]]
  // CHECK:   [[X_COPY:%.*]] = copy_value [[BORROWED_X]]
  // CHECK:   store [[X_COPY]] to [init] [[PB]]
  return x
  // CHECK:   [[X1:%.*]] = load [copy] [[PB]]
  // CHECK:   destroy_value [[X_ADDR]]
  // CHECK:   destroy_value [[X]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_T021class_bound_protocols0a1_B10_generic_2{{[_0-9a-zA-Z]*}}F
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
  // CHECK:   [[X1:%.*]] = load [copy] [[PB]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_T021class_bound_protocols0a1_B9_protocol{{[_0-9a-zA-Z]*}}F
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
  // CHECK:   [[X1:%.*]] = load [copy] [[PB]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_T021class_bound_protocols0a1_B21_protocol_composition{{[_0-9a-zA-Z]*}}F
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
  // CHECK:   [[X1:%.*]] = load [copy] [[PB]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_T021class_bound_protocols0a1_B8_erasure{{[_0-9a-zA-Z]*}}F
func class_bound_erasure(x: ConcreteClass) -> ClassBound {
  return x
  // CHECK: [[PROTO:%.*]] = init_existential_ref {{%.*}} : $ConcreteClass, $ClassBound
  // CHECK: return [[PROTO]]
}

// CHECK-LABEL: sil hidden @_T021class_bound_protocols0a1_B19_existential_upcastAA10ClassBound_pAaC_AA0F6Bound2p1x_tF :
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
// CHECK: } // end sil function '_T021class_bound_protocols0a1_B19_existential_upcastAA10ClassBound_pAaC_AA0F6Bound2p1x_tF'

// CHECK-LABEL: sil hidden @_T021class_bound_protocols0a1_B30_to_unbound_existential_upcast{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden @_T021class_bound_protocols0a1_B7_method{{[_0-9a-zA-Z]*}}F
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
  // CHECK: [[X:%.*]] = load [copy] [[XBOX_PB]] : $*ClassBound
  // CHECK: [[PROJ:%.*]] = open_existential_ref [[X]] : $ClassBound to $[[OPENED:@opened(.*) ClassBound]]
  // CHECK: [[METHOD:%.*]] = witness_method $[[OPENED]], #ClassBound.classBoundMethod!1
  // CHECK: apply [[METHOD]]<[[OPENED]]>([[PROJ]])
  // CHECK: destroy_value [[PROJ]]
  // CHECK: destroy_value [[XBOX]]
  // CHECK: destroy_value [[ARG]]
}
// CHECK: } // end sil function '_T021class_bound_protocols0a1_B7_methodyAA10ClassBound_p1x_tF'


