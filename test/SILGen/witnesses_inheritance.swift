// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

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
  // CHECK-NOT: sil hidden [transparent] [thunk] @_TTWC21witnesses_inheritance1YS_7FooableS_FS1_3foo
  class func class_bar() {}
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC21witnesses_inheritance1YS_8BarrableS_ZFS1_9class_bar
}

class A : Fooable {
  func foo() {}
  func bar() {}
  class func class_foo() {}
  class func class_bar() {}
}

// -- Derived class conforms to a refined protocol using its base's methods
class B : A, Barrable {}
// CHECK-NOT: sil hidden [transparent] [thunk] @_TTWC21witnesses_inheritance1BS_7FooableS_FS1_3foo
// CHECK-NOT: sil hidden [transparent] [thunk] @_TTWC21witnesses_inheritance1BS_7FooableS_ZFS1_9class_foo
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC21witnesses_inheritance1BS_8BarrableS_FS1_3bar
// CHECK:         [[B:%.*]] = load {{%.*}} : $*B
// CHECK-NEXT:    [[A:%.*]] = upcast [[B]] : $B to $A
// CHECK-NEXT:    [[METH:%.*]] = class_method [[A]] : $A, #A.bar!1
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC21witnesses_inheritance1BS_8BarrableS_ZFS1_9class_bar
// CHECK:         upcast {{%.*}} : $@thick B.Type to $@thick A.Type

// Add tests to make sure that we handle address only case correctly.
