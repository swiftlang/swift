// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Foo {
  static func foo()
}

func getFoo<T: Foo>(t: T.Type) -> () -> () {
  return t.foo
}

// CHECK-LABEL: sil hidden @_TF21partial_apply_generic6getFooUS_3Foo__FMQ_FT_T_
// CHECK:         function_ref @_TZFP21partial_apply_generic3Foo3fooUS0___FMQPS0_FT_T_ 

// CHECK-LABEL: sil shared @_TZFP21partial_apply_generic3Foo3fooUS0___FMQPS0_FT_T_
// CHECK:         witness_method $Self, #Foo.foo!1
