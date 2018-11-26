// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

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
  // CHECK-NOT: sil private [transparent] [thunk] @$s21witnesses_inheritance1YCAA7FooableA2aDP3foo{{[_0-9a-zA-Z]*}}FTW
  class func class_bar() {}
  // CHECK-LABEL: sil private [transparent] [thunk] @$s21witnesses_inheritance1YCAA8BarrableA2aDP9class_bar{{[_0-9a-zA-Z]*}}FZTW
}

class A : Fooable {
  func foo() {}
  func bar() {}
  class func class_foo() {}
  class func class_bar() {}
}

// -- Derived class conforms to a refined protocol using its base's methods
class B : A, Barrable {}
// CHECK-NOT: sil private [transparent] [thunk] @$s21witnesses_inheritance1BCAA7FooableA2aDP3foo{{[_0-9a-zA-Z]*}}FTW
// CHECK-NOT: sil private [transparent] [thunk] @$s21witnesses_inheritance1BCAA7FooableA2aDP9class_foo{{[_0-9a-zA-Z]*}}FZTW
// CHECK-LABEL: sil private [transparent] [thunk] @$s21witnesses_inheritance1BCAA8BarrableA2aDP3bar{{[_0-9a-zA-Z]*}}FTW
// CHECK:         [[B:%.*]] = load_borrow {{%.*}} : $*B
// CHECK-NEXT:    [[A:%.*]] = upcast [[B]] : $B to $A
// CHECK-NEXT:    [[METH:%.*]] = class_method [[A]] : $A, #A.bar!1
// CHECK-NEXT:    apply [[METH]]([[A]]) : $@convention(method) (@guaranteed A) -> ()
// CHECK:         end_borrow [[B]]

// CHECK-LABEL: sil private [transparent] [thunk] @$s21witnesses_inheritance1BCAA8BarrableA2aDP9class_bar{{[_0-9a-zA-Z]*}}FZTW
// CHECK:         upcast {{%.*}} : $@thick B.Type to $@thick A.Type

// Add tests to make sure that we handle address only case correctly.
