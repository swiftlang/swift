// RUN: %swift -parse-stdlib -parse-as-library -emit-silgen %s | FileCheck %s

// -- Class-bound archetypes and existentials are *not* address-only and can
//    be manipulated using normal reference type value semantics.

protocol NotClassBound {
  func notClassBoundMethod()
}
@class_protocol protocol ClassBound {
  func classBoundMethod()
}

@class_protocol protocol ClassBound2 {
  func classBound2Method()
}

class ConcreteClass : NotClassBound, ClassBound, ClassBound2 {
  func notClassBoundMethod() {}
  func classBoundMethod() {}
  func classBound2Method() {}
}

class ConcreteSubclass : ConcreteClass { }

// CHECK-LABEL: sil  @_TF21class_bound_protocols19class_bound_generic
func class_bound_generic<T : ClassBound>(var x: T) -> T {
  // CHECK: bb0([[X:%.*]] : $T):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $T
  // CHECK:   store [[X]] to [[X_ADDR]]
  return x
  // CHECK:   [[X1:%.*]] = load [[X_ADDR]]
  // CHECK:   retain [[X1]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil  @_TF21class_bound_protocols21class_bound_generic_2
func class_bound_generic_2<T : protocol<ClassBound, NotClassBound>>(var x: T) -> T {
  // CHECK: bb0([[X:%.*]] : $T):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $T
  // CHECK:   store [[X]] to [[X_ADDR]]
  return x
  // CHECK:   [[X1:%.*]] = load [[X_ADDR]]
  // CHECK:   retain [[X1]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil  @_TF21class_bound_protocols20class_bound_protocol
func class_bound_protocol(var x: ClassBound) -> ClassBound {
  // CHECK: bb0([[X:%.*]] : $ClassBound):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $ClassBound
  // CHECK:   store [[X]] to [[X_ADDR]]
  return x
  // CHECK:   [[X1:%.*]] = load [[X_ADDR]]
  // CHECK:   retain [[X1]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil  @_TF21class_bound_protocols32class_bound_protocol_composition
func class_bound_protocol_composition(var x: protocol<ClassBound, NotClassBound>)
-> protocol<ClassBound, NotClassBound> {
  // CHECK: bb0([[X:%.*]] : $protocol<ClassBound, NotClassBound>):
  // CHECK:   [[X_ADDR:%.*]] = alloc_box $protocol<ClassBound, NotClassBound>
  // CHECK:   store [[X]] to [[X_ADDR]]
  return x
  // CHECK:   [[X1:%.*]] = load [[X_ADDR]]
  // CHECK:   retain [[X1]]
  // CHECK:   return [[X1]]
}

// CHECK-LABEL: sil  @_TF21class_bound_protocols19class_bound_erasure
func class_bound_erasure(x: ConcreteClass) -> ClassBound {
  return x
  // CHECK: [[PROTO:%.*]] = init_existential_ref {{%.*}} : $ConcreteClass, $ClassBound
  // CHECK: return [[PROTO]]
}

// CHECK-LABEL: sil  @_TF21class_bound_protocols30class_bound_existential_upcast
func class_bound_existential_upcast(x: protocol<ClassBound,ClassBound2>)
-> ClassBound {
  return x
  // CHECK: [[PROTO:%.*]] = upcast_existential_ref {{%.*}} to $ClassBound
  // CHECK: return [[PROTO]]
}

// CHECK-LABEL: sil  @_TF21class_bound_protocols41class_bound_to_unbound_existential_upcast
func class_bound_to_unbound_existential_upcast
(var x:protocol<ClassBound,NotClassBound>) -> NotClassBound {
  return x
  // CHECK: [[X:%.*]] = load {{%.*}} : $*protocol<ClassBound, NotClassBound>
  // CHECK: upcast_existential [take] [[X]] : $protocol<ClassBound, NotClassBound> to {{%.*}}
}

// CHECK-LABEL: sil  @_TF21class_bound_protocols18class_bound_method
func class_bound_method(var x: ClassBound) {
  x.classBoundMethod()
  // CHECK: [[X:%.*]] = load {{%.*}} : $*ClassBound
  // CHECK: [[PROJ:%.*]] = project_existential_ref [[X]] : $ClassBound
  // CHECK: [[METHOD:%.*]] = protocol_method [[X]] : {{.*}}, #ClassBound.classBoundMethod!1
  // CHECK: apply [[METHOD]]([[PROJ]]) : $@cc(witness_method) @callee_owned (@owned Self) -> ()
}

