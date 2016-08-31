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
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $T
  // CHECK:   [[PB:%.*]] = project_box [[X_ADDR]]
  // CHECK:   store [[X]] to [[PB]]
  return x
  // CHECK:   [[X1:%.*]] = load [[PB]]
  // CHECK:   retain [[X1]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols21class_bound_generic_2
func class_bound_generic_2<T : ClassBound & NotClassBound>(x: T) -> T {
  var x = x
  // CHECK: bb0([[X:%.*]] : $T):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $T
  // CHECK:   [[PB:%.*]] = project_box [[X_ADDR]]
  // CHECK:   store [[X]] to [[PB]]
  return x
  // CHECK:   [[X1:%.*]] = load [[PB]]
  // CHECK:   retain [[X1]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols20class_bound_protocol
func class_bound_protocol(x: ClassBound) -> ClassBound {
  var x = x
  // CHECK: bb0([[X:%.*]] : $ClassBound):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $ClassBound
  // CHECK:   [[PB:%.*]] = project_box [[X_ADDR]]
  // CHECK:   store [[X]] to [[PB]]
  return x
  // CHECK:   [[X1:%.*]] = load [[PB]]
  // CHECK:   retain [[X1]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols32class_bound_protocol_composition
func class_bound_protocol_composition(x: ClassBound & NotClassBound)
-> ClassBound & NotClassBound {
  var x = x
  // CHECK: bb0([[X:%.*]] : $ClassBound & NotClassBound):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $ClassBound & NotClassBound
  // CHECK:   [[PB:%.*]] = project_box [[X_ADDR]]
  // CHECK:   store [[X]] to [[PB]]
  return x
  // CHECK:   [[X1:%.*]] = load [[PB]]
  // CHECK:   retain [[X1]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols19class_bound_erasure
func class_bound_erasure(x: ConcreteClass) -> ClassBound {
  return x
  // CHECK: [[PROTO:%.*]] = init_existential_ref {{%.*}} : $ConcreteClass, $ClassBound
  // CHECK: return [[PROTO]]
}

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols30class_bound_existential_upcast
func class_bound_existential_upcast(x: ClassBound & ClassBound2)
-> ClassBound {
  return x
  // CHECK: [[OPENED:%.*]] = open_existential_ref {{%.*}} : $ClassBound & ClassBound2 to [[OPENED_TYPE:\$@opened(.*) ClassBound & ClassBound2]]
  // CHECK: [[PROTO:%.*]] = init_existential_ref [[OPENED]] : [[OPENED_TYPE]] : [[OPENED_TYPE]], $ClassBound
  // CHECK: return [[PROTO]]
}

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols41class_bound_to_unbound_existential_upcast
func class_bound_to_unbound_existential_upcast
(x: ClassBound & NotClassBound) -> NotClassBound {
  return x
  // CHECK: [[X_OPENED:%.*]] = open_existential_ref %1 : $ClassBound & NotClassBound to [[OPENED_TYPE:\$@opened(.*) ClassBound & NotClassBound]]
  // CHECK: [[PAYLOAD_ADDR:%.*]] = init_existential_addr %0 : $*NotClassBound, [[OPENED_TYPE]]
  // CHECK: store [[X_OPENED]] to [[PAYLOAD_ADDR]]
}

// CHECK-LABEL: sil hidden @_TF21class_bound_protocols18class_bound_method
func class_bound_method(x: ClassBound) {
  var x = x
  x.classBoundMethod()
  // CHECK: [[X:%.*]] = load {{%.*}} : $*ClassBound
  // CHECK: [[PROJ:%.*]] = open_existential_ref [[X]] : $ClassBound to $[[OPENED:@opened(.*) ClassBound]]
  // CHECK: [[METHOD:%.*]] = witness_method $[[OPENED]], #ClassBound.classBoundMethod!1
  // CHECK: apply [[METHOD]]<[[OPENED]]>([[PROJ]])
}

