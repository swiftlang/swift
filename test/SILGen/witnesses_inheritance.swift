// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

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
  // CHECK-NOT: sil hidden [transparent] [thunk] @_T021witnesses_inheritance1YCAA7FooableAaaDP3foo{{[_0-9a-zA-Z]*}}FTW
  class func class_bar() {}
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_T021witnesses_inheritance1YCAA8BarrableAaaDP9class_bar{{[_0-9a-zA-Z]*}}FZTW
}

class A : Fooable {
  func foo() {}
  func bar() {}
  class func class_foo() {}
  class func class_bar() {}
}

// -- Derived class conforms to a refined protocol using its base's methods
class B : A, Barrable {}
// CHECK-NOT: sil hidden [transparent] [thunk] @_T021witnesses_inheritance1BCAA7FooableAaaDP3foo{{[_0-9a-zA-Z]*}}FTW
// CHECK-NOT: sil hidden [transparent] [thunk] @_T021witnesses_inheritance1BCAA7FooableAaaDP9class_foo{{[_0-9a-zA-Z]*}}FZTW
// CHECK-LABEL: sil hidden [transparent] [thunk] @_T021witnesses_inheritance1BCAA8BarrableAaaDP3bar{{[_0-9a-zA-Z]*}}FTW
// CHECK:         [[B:%.*]] = load [take] {{%.*}} : $*B
// CHECK-NEXT:    [[A:%.*]] = upcast [[B]] : $B to $A
// CHECK-NEXT:    [[BORROWED_A:%.*]] = begin_borrow [[A]]
// CHECK-NEXT:    [[METH:%.*]] = class_method [[BORROWED_A]] : $A, #A.bar!1
// CHECK-NEXT:    apply [[METH]]([[BORROWED_A]]) : $@convention(method) (@guaranteed A) -> ()
// CHECK:         end_borrow [[BORROWED_A]] from [[A]]
// CHECK-LABEL: sil hidden [transparent] [thunk] @_T021witnesses_inheritance1BCAA8BarrableAaaDP9class_bar{{[_0-9a-zA-Z]*}}FZTW
// CHECK:         upcast {{%.*}} : $@thick B.Type to $@thick A.Type

// Add tests to make sure that we handle address only case correctly.
