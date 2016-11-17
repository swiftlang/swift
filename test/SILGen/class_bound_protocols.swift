// RUN: %target-swift-frontend -parse-stdlib -parse-as-library -emit-silgen %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols19class_bound_generic
func class_bound_generic<T : ClassBound>(x: T) -> T {
  var x = x
  // CHECK: bb0([[X:%.*]] : $T):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $@box T
  // CHECK:   [[PB:%.*]] = project_box [[X_ADDR]]
  // CHECK:   [[X_COPY:%.*]] = copy_value [[X]]
  // CHECK:   store [[X_COPY]] to [init] [[PB]]
  return x
  // CHECK:   [[X1:%.*]] = load [copy] [[PB]]
  // CHECK:   destroy_value [[X_ADDR]]
  // CHECK:   destroy_value [[X]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols21class_bound_generic_2
func class_bound_generic_2<T : ClassBound & NotClassBound>(x: T) -> T {
  var x = x
  // CHECK: bb0([[X:%.*]] : $T):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $@box T
  // CHECK:   [[PB:%.*]] = project_box [[X_ADDR]]
  // CHECK:   [[X_COPY:%.*]] = copy_value [[X]]
  // CHECK:   store [[X_COPY]] to [init] [[PB]]
  return x
  // CHECK:   [[X1:%.*]] = load [copy] [[PB]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols20class_bound_protocol
func class_bound_protocol(x: ClassBound) -> ClassBound {
  var x = x
  // CHECK: bb0([[X:%.*]] : $ClassBound):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $@box ClassBound
  // CHECK:   [[PB:%.*]] = project_box [[X_ADDR]]
  // CHECK:   [[X_COPY:%.*]] = copy_value [[X]]
  // CHECK:   store [[X_COPY]] to [init] [[PB]]
  return x
  // CHECK:   [[X1:%.*]] = load [copy] [[PB]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols32class_bound_protocol_composition
func class_bound_protocol_composition(x: ClassBound & NotClassBound)
-> ClassBound & NotClassBound {
  var x = x
  // CHECK: bb0([[X:%.*]] : $ClassBound & NotClassBound):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $@box (ClassBound & NotClassBound)
  // CHECK:   [[PB:%.*]] = project_box [[X_ADDR]]
  // CHECK:   [[X_COPY:%.*]] = copy_value [[X]]
  // CHECK:   store [[X_COPY]] to [init] [[PB]]
  return x
  // CHECK:   [[X1:%.*]] = load [copy] [[PB]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols19class_bound_erasure
func class_bound_erasure(x: ConcreteClass) -> ClassBound {
  return x
  // CHECK: [[PROTO:%.*]] = init_existential_ref {{%.*}} : $ConcreteClass, $ClassBound
  // CHECK: return [[PROTO]]
}

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols30class_bound_existential_upcastFT1xPS_10ClassBoundS_11ClassBound2__PS0__ :
func class_bound_existential_upcast(x: ClassBound & ClassBound2)
-> ClassBound {
  return x
  // CHECK: bb0([[ARG:%.*]] : $ClassBound & ClassBound2):
  // CHECK:   [[OPENED:%.*]] = open_existential_ref [[ARG]] : $ClassBound & ClassBound2 to [[OPENED_TYPE:\$@opened(.*) ClassBound & ClassBound2]]
  // CHECK:   [[OPENED_COPY:%.*]] = copy_value [[OPENED]]
  // CHECK:   [[PROTO:%.*]] = init_existential_ref [[OPENED_COPY]] : [[OPENED_TYPE]] : [[OPENED_TYPE]], $ClassBound
  // CHECK:   destroy_value [[ARG]]
  // CHECK:   return [[PROTO]]
}
// CHECK: } // end sil function '_TF21class_bound_protocols30class_bound_existential_upcastFT1xPS_10ClassBoundS_11ClassBound2__PS0__'

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols41class_bound_to_unbound_existential_upcast
func class_bound_to_unbound_existential_upcast
(x: ClassBound & NotClassBound) -> NotClassBound {
  return x
  // CHECK: [[X_OPENED:%.*]] = open_existential_ref %1 : $ClassBound & NotClassBound to [[OPENED_TYPE:\$@opened(.*) ClassBound & NotClassBound]]
  // CHECK: [[PAYLOAD_ADDR:%.*]] = init_existential_addr %0 : $*NotClassBound, [[OPENED_TYPE]]
  // CHECK: [[X_OPENED_COPY:%.*]] = copy_value [[X_OPENED]]
  // CHECK: store [[X_OPENED_COPY]] to [init] [[PAYLOAD_ADDR]]
}

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols18class_bound_method
// CHECK: bb0([[ARG:%.*]] : $ClassBound):
func class_bound_method(x: ClassBound) {
  var x = x
  x.classBoundMethod()
  // CHECK: [[XBOX:%.*]] = alloc_box $@box ClassBound, var, name "x"
  // CHECK: [[XBOX_PB:%.*]] = project_box [[XBOX]]
  // CHECK: [[ARG_COPY:%.*]] = copy_value [[ARG]]
  // CHECK: store [[ARG_COPY]] to [init] [[XBOX_PB]]
  // CHECK: [[X:%.*]] = load [copy] [[XBOX_PB]] : $*ClassBound
  // CHECK: [[PROJ:%.*]] = open_existential_ref [[X]] : $ClassBound to $[[OPENED:@opened(.*) ClassBound]]
  // CHECK: [[METHOD:%.*]] = witness_method $[[OPENED]], #ClassBound.classBoundMethod!1
  // CHECK: apply [[METHOD]]<[[OPENED]]>([[PROJ]])
  // CHECK: destroy_value [[PROJ]]
  // CHECK: destroy_value [[XBOX]]
  // CHECK: destroy_value [[ARG]]
}
// CHECK: } // end sil function '_TF21class_bound_protocols18class_bound_methodFT1xPS_10ClassBound__T_'

