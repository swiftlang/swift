// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Foo {
  static func foo()
}

func getFoo<T: Foo>(t: T.Type) -> () -> () {
  return t.foo
}

// CHECK-LABEL: sil hidden @_TF21partial_apply_generic6getFoouRq_S_3Foo_FMq_FT_T_
// CHECK:         function_ref @_TZFP21partial_apply_generic3Foo3foouRq_S0__FMq_FT_T_ 

// CHECK-LABEL: sil shared @_TZFP21partial_apply_generic3Foo3foouRq_S0__FMq_FT_T_
// CHECK:         witness_method $Self, #Foo.foo!1
