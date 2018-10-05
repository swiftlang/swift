// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s
func foo<T>(_: Any) -> T {
  fatalError()
}

func foo<T>(_: Any?) -> T {
  fatalError()
}

// CHECK: function_ref @$s12rdar386258243fooyxyplF : $@convention(thin) <τ_0_0> (@in_guaranteed Any) -> @out τ_0_0
var _: String = foo("hello")
