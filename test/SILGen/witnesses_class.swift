// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

protocol Fooable: class {
  func foo()
  static func bar()
  init()
}

class Foo: Fooable {
  
  func foo() { }
  // CHECK-LABEL: sil private [transparent] [thunk] @_T015witnesses_class3FooCAA7FooableA2aDP3foo{{[_0-9a-zA-Z]*}}FTW
  // CHECK-NOT:     function_ref
  // CHECK:         class_method

  class func bar() {}
  // CHECK-LABEL: sil private [transparent] [thunk] @_T015witnesses_class3FooCAA7FooableA2aDP3bar{{[_0-9a-zA-Z]*}}FZTW
  // CHECK-NOT:     function_ref
  // CHECK:         class_method

  required init() {}
  // CHECK-LABEL: sil private [transparent] [thunk] @_T015witnesses_class3FooCAA7FooableA2aDP{{[_0-9a-zA-Z]*}}fCTW
  // CHECK-NOT:     function_ref
  // CHECK:         class_method
}

// CHECK-LABEL: sil hidden @_T015witnesses_class3gen{{[_0-9a-zA-Z]*}}F
// CHECK:         bb0([[SELF:%.*]] : $T)
// CHECK:         [[METHOD:%.*]] = witness_method $T
// CHECK-NOT:     copy_value [[SELF]]
// CHECK:         [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK-NOT:     copy_value [[SELF]]
// CHECK:         apply [[METHOD]]<T>([[BORROWED_SELF]])
// CHECK:         end_borrow [[BORROWED_SELF]] from [[SELF]]
// CHECK:         destroy_value [[SELF]]
// CHECK-NOT:         destroy_value [[SELF]]
// CHECK:         return
func gen<T: Fooable>(_ foo: T) {
  foo.foo()
}

// CHECK-LABEL: sil hidden @_T015witnesses_class2exyAA7Fooable_pF
// CHECK: bb0([[SELF:%[0-0]+]] : $Fooable):
// CHECK:         [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK:         [[SELF_PROJ:%.*]] = open_existential_ref [[BORROWED_SELF]]
// CHECK:         [[METHOD:%.*]] = witness_method $[[OPENED:@opened(.*) Fooable]],
// CHECK-NOT:     copy_value [[SELF_PROJ]] : $
// CHECK:         apply [[METHOD]]<[[OPENED]]>([[SELF_PROJ]])
// CHECK:         end_borrow [[BORROWED_SELF]] from [[SELF]]
// CHECK:         destroy_value [[SELF]]
// CHECK-NOT:     destroy_value [[SELF]]
// CHECK:         return
func ex(_ foo: Fooable) {
  foo.foo()
}
