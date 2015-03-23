// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

class Foo {
  var bar: Bar!
}

class Bar {
  var bas: C!
}

class C {}
class D: C {}

// CHECK-LABEL: sil hidden @_TF27force_cast_chained_optional4testFCS_3FooCS_1D
// CHECK:         class_method %0 : $Foo, #Foo.bar!getter.1 : Foo -> () -> Bar! , $@cc(method) @thin (@owned Foo) ->
// CHECK:         select_enum_addr
// CHECK:         cond_br {{%.*}}, [[SOME_BAR:bb[0-9]+]], [[NO_BAR:bb[0-9]+]]
// CHECK:       [[NO_BAR]]:
// CHECK:         br [[TRAP:bb[0-9]+]]
// CHECK:       [[SOME_BAR]]:
// CHECK:         [[PAYLOAD_ADDR:%.*]] = unchecked_take_enum_data_addr {{%.*}} : $*ImplicitlyUnwrappedOptional<Bar>
// CHECK:         [[BAR:%.*]] = load [[PAYLOAD_ADDR]]
// CHECK:         class_method {{%.*}} : $Bar, #Bar.bas!getter.1 : Bar -> () -> C! , $@cc(method) @thin (@owned Bar) ->
// CHECK:         function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
// CHECK:         unconditional_checked_cast {{%.*}} : $C to $D
// CHECK:       [[TRAP]]:
// CHECK:         unreachable
func test(x: Foo) -> D {
  return x.bar?.bas as! D
}
