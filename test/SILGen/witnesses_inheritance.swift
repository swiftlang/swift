// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Fooable {
  func foo()
  static func class_foo()
}

protocol Barrable : Fooable {
  func bar()
  static func class_bar()
}

class X : Fooable {
  func foo() {}
  class func class_foo() {}
}

// -- Derived class conforms to a refined protocol
class Y : X, Barrable {
  func bar() {}
  // CHECK-NOT: sil hidden [thunk] @_TTWC21witnesses_inheritance1YS_7FooableS_FS1_3fooUS1___fQPS1_FT_T_
  class func class_bar() {}
  // CHECK-LABEL: sil hidden [thunk] @_TTWC21witnesses_inheritance1YS_8BarrableS_ZFS1_9class_barUS1___fMQPS1_FT_T_
}

class A : Fooable {
  func foo() {}
  func bar() {}
  class func class_foo() {}
  class func class_bar() {}
}

// -- Derived class conforms to a refined protocol using its base's methods
class B : A, Barrable {}
// CHECK-NOT: sil hidden [thunk] @_TTWC21witnesses_inheritance1BS_7FooableS_FS1_3fooUS1___fQPS1_FT_T_
// CHECK-NOT: sil hidden [thunk] @_TTWC21witnesses_inheritance1BS_7FooableS_ZFS1_9class_fooUS1___fMQPS1_FT_T_
// CHECK-LABEL: sil hidden [thunk] @_TTWC21witnesses_inheritance1BS_8BarrableS_FS1_3barUS1___fQPS1_FT_T_
// CHECK:         upcast {{%.*}} : $B to $A
// CHECK-LABEL: sil hidden [thunk] @_TTWC21witnesses_inheritance1BS_8BarrableS_ZFS1_9class_barUS1___fMQPS1_FT_T_
// CHECK:         upcast {{%.*}} : $@thick B.Type to $@thick A.Type
