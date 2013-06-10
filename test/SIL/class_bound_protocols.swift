// RUN: %swift -parse-as-library -emit-sil %s | FileCheck %s

// -- Class-bound archetypes and existentials are *not* address-only and can
//    be manipulated using normal reference type value semantics.

protocol NotClassBound {
  func notClassBoundMethod()
}
protocol [class_protocol] ClassBound {
  func classBoundMethod()
}

protocol [class_protocol] ClassBound2 {
  func classBound2Method()
}

class ConcreteClass : NotClassBound, ClassBound, ClassBound2 {
  func notClassBoundMethod() {}
  func classBoundMethod() {}
  func classBound2Method() {}
}

class ConcreteSubclass : ConcreteClass { }

// CHECK: sil @_T21class_bound_protocols19class_bound_genericUS_10ClassBound__FT1xQ__Q_
func class_bound_generic<T:ClassBound>(x:T) -> T {
  // CHECK: bb0([[X:%.*]] : $T):
  // CHECK:   [[X_ADDR:%.*]] = alloc_var stack $T
  // CHECK:   store [[X]] to [[X_ADDR]]
  return x
  // CHECK:   [[X1:%.*]] = load [[X_ADDR]]
  // CHECK:   retain [[X1]]
  // CHECK:   return [[X1]]
}

// CHECK: sil @_T21class_bound_protocols21class_bound_generic_2US_10ClassBoundS_13NotClassBound__FT1xQ__Q_
func class_bound_generic_2<T:protocol<ClassBound, NotClassBound>>(x:T) -> T {
  // CHECK: bb0([[X:%.*]] : $T):
  // CHECK:   [[X_ADDR:%.*]] = alloc_var stack $T
  // CHECK:   store [[X]] to [[X_ADDR]]
  return x
  // CHECK:   [[X1:%.*]] = load [[X_ADDR]]
  // CHECK:   retain [[X1]]
  // CHECK:   return [[X1]]
}

// CHECK: sil @_T21class_bound_protocols20class_bound_protocolFT1xPS_10ClassBound__PS0__
func class_bound_protocol(x:ClassBound) -> ClassBound {
  // CHECK: bb0([[X:%.*]] : $ClassBound):
  // CHECK:   [[X_ADDR:%.*]] = alloc_var stack $ClassBound
  // CHECK:   store [[X]] to [[X_ADDR]]
  return x
  // CHECK:   [[X1:%.*]] = load [[X_ADDR]]
  // CHECK:   retain [[X1]]
  // CHECK:   return [[X1]]
}

// CHECK: sil @_T21class_bound_protocols32class_bound_protocol_compositionFT1xPS_10ClassBoundS_13NotClassBound__PS0_S1__
func class_bound_protocol_composition(x:protocol<ClassBound, NotClassBound>)
-> protocol<ClassBound, NotClassBound> {
  // CHECK: bb0([[X:%.*]] : $protocol<ClassBound, NotClassBound>):
  // CHECK:   [[X_ADDR:%.*]] = alloc_var stack $protocol<ClassBound, NotClassBound>
  // CHECK:   store [[X]] to [[X_ADDR]]
  return x
  // CHECK:   [[X1:%.*]] = load [[X_ADDR]]
  // CHECK:   retain [[X1]]
  // CHECK:   return [[X1]]
}

// CHECK: sil @_T21class_bound_protocols19class_bound_erasureFT1xCS_13ConcreteClass_PS_10ClassBound_
func class_bound_erasure(x:ConcreteClass) -> ClassBound {
  return x
  // CHECK: [[PROTO:%.*]] = init_existential_ref {{%.*}} : $ConcreteClass, $ClassBound
  // CHECK: return [[PROTO]]
}

// CHECK: sil @_T21class_bound_protocols30class_bound_existential_upcastFT1xPS_10ClassBoundS_11ClassBound2__PS0__
func class_bound_existential_upcast(x:protocol<ClassBound,ClassBound2>)
-> ClassBound {
  return x
  // CHECK: [[PROTO:%.*]] = upcast_existential_ref {{%.*}}, $ClassBound
  // CHECK: return [[PROTO]]
}

func class_bound_to_unbound_existential_upcast
(x:protocol<ClassBound,NotClassBound>) -> NotClassBound {
  return x
  // CHECK: [[X:%.*]] = load {{%.*}} : $*protocol<ClassBound, NotClassBound>
  // CHECK: upcast_existential [take] [[X]] to {{%.*}}
}

// TODO: method access on class-bound existential
//func class_bound_method(x:ClassBound) {
//  x.classBoundMethod()
//}

