// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

class Foo {
  var bar: Bar!
}

class Bar {
  var bas: C!
}

class C {}
class D: C {}

// CHECK-LABEL: sil hidden @_T027force_cast_chained_optional4testAA1DCAA3FooCF
// CHECK: bb0([[ARG:%.*]] : $Foo):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   class_method [[BORROWED_ARG]] : $Foo, #Foo.bar!getter.1 : (Foo) -> () -> Bar!, $@convention(method) (@guaranteed Foo) ->
// CHECK:   select_enum_addr
// CHECK:   cond_br {{%.*}}, [[SOME_BAR:bb[0-9]+]], [[NO_BAR:bb[0-9]+]]
//
// CHECK: [[NO_BAR]]:
// CHECK:   br [[TRAP:bb[0-9]+]]
//
// CHECK: [[SOME_BAR]]:
// CHECK:   [[PAYLOAD_ADDR:%.*]] = unchecked_take_enum_data_addr {{%.*}} : $*Optional<Bar>
// CHECK:   [[BAR:%.*]] = load [copy] [[PAYLOAD_ADDR]]
// CHECK:   [[METHOD:%.*]] = class_method [[BAR]] : $Bar, #Bar.bas!getter.1 : (Bar) -> () -> C!, $@convention(method) (@guaranteed Bar) ->
// CHECK:   apply [[METHOD]]([[BAR]])
// CHECK:   destroy_value [[BAR]]
// CHECK:   unconditional_checked_cast {{%.*}} : $C to $D
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
//
// CHECK: [[TRAP]]:
// CHECK:   unreachable
func test(_ x: Foo) -> D {
  return x.bar?.bas as! D
}
