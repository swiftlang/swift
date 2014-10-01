// RUN: %swift -emit-silgen %s | FileCheck %s

protocol Fooable: class {
  func foo()
}

class Foo: Fooable {
  func foo() { }
}

// CHECK-LABEL: sil hidden @_TF15witnesses_class3genUS_7Fooable__FQ_T_
// CHECK:         strong_retain [[SELF:%.*]] : $
// CHECK:         [[METHOD:%.*]] = witness_method $T
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
