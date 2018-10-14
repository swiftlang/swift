
// RUN: %target-swift-emit-silgen -module-name force_cast_chained_optional -enable-sil-ownership %s | %FileCheck %s

class Foo {
  var bar: Bar!
}

class Bar {
  var bas: C!
}

class C {}
class D: C {}

// CHECK-LABEL: sil hidden @$s27force_cast_chained_optional4testyAA1DCAA3FooCF
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Foo):
// CHECK:   class_method [[ARG]] : $Foo, #Foo.bar!getter.1 : (Foo) -> () -> Bar?, $@convention(method) (@guaranteed Foo) ->
// CHECK:   select_enum_addr {{%.*}}
// CHECK:   cond_br {{%.*}}, [[SOME_BAR:bb[0-9]+]], [[NO_BAR:bb[0-9]+]]
//
// CHECK: [[SOME_BAR]]:
// CHECK:   [[PAYLOAD_ADDR:%.*]] = unchecked_take_enum_data_addr {{%.*}} : $*Optional<Bar>
// CHECK:   [[BAR:%.*]] = load [copy] [[PAYLOAD_ADDR]]
// CHECK:   [[BORROWED_BAR:%.*]] = begin_borrow [[BAR]]
// CHECK:   [[METHOD:%.*]] = class_method [[BORROWED_BAR]] : $Bar, #Bar.bas!getter.1 : (Bar) -> () -> C?, $@convention(method) (@guaranteed Bar) ->
// CHECK:   apply [[METHOD]]([[BORROWED_BAR]])
// CHECK:   end_borrow [[BORROWED_BAR]]
// CHECK:   unconditional_checked_cast {{%.*}} : $C to $D
//
// CHECK: [[NO_BAR]]:
// CHECK:   unreachable
func test(_ x: Foo) -> D {
  return x.bar?.bas as! D
}
