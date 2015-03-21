// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Fooable: class {
  func foo()
  static func bar()
  init()
}

class Foo: Fooable {
  
  func foo() { }
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC15witnesses_class3FooS_7FooableS_FS1_3fooUS1___fQPS1_FT_T_
  // CHECK-NOT:     function_ref
  // CHECK:         class_method

  class func bar() {}
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC15witnesses_class3FooS_7FooableS_ZFS1_3barUS1___fMQPS1_FT_T_
  // CHECK-NOT:     function_ref
  // CHECK:         class_method

  required init() {}
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC15witnesses_class3FooS_7FooableS_FS1_CUS1___fMQPS1_FT_S2_
  // CHECK-NOT:     function_ref
  // CHECK:         class_method
}

// CHECK-LABEL: sil hidden @_TF15witnesses_class3genUS_7Fooable__FQ_T_
// CHECK:         [[METHOD:%.*]] = witness_method $T
// CHECK:         strong_retain [[SELF:%.*]] : $
// CHECK:         apply [[METHOD]]<T>([[SELF]])
// CHECK:         return
func gen<T: Fooable>(foo: T) {
  foo.foo()
}

// CHECK-LABEL: sil hidden @_TF15witnesses_class2exFPS_7Fooable_T_
// CHECK:         strong_retain [[SELF:%.*]] : $
// CHECK:         [[SELF_PROJ:%.*]] = open_existential_ref [[SELF]]
// CHECK:         [[METHOD:%.*]] = witness_method $[[OPENED:@opened(.*) Fooable]],
// CHECK:         apply [[METHOD]]<[[OPENED]]>([[SELF_PROJ]])
// CHECK:         return
func ex(foo: Fooable) {
  foo.foo()
}
