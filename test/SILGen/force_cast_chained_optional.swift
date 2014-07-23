// RUN: %swift -emit-silgen %s | FileCheck %s

class Foo {
  var bar: Bar!
}

class Bar {
  var bas: C!
}

class C {}
class D: C {}

// CHECK-LABEL: sil @_TF27force_cast_chained_optional4testFCS_3FooCS_1D
// CHECK:         class_method {{%.*}} : $Foo, #Foo.bar!getter.1
// CHECK:         function_ref @_TFSs41_doesImplicitlyUnwrappedOptionalHaveValueU__FRGSQQ__Bi1_
// CHECK:         cond_br {{%.*}}, [[SOME_BAR:bb[0-9]+]], [[NO_BAR:bb[0-9]+]]
// CHECK:       [[TRAP:bb[0-9]+]]:
// CHECK:         unreachable
// CHECK:       [[NO_BAR]]:
// CHECK:         br [[NO_BAR_2:bb[0-9]+]]
// CHECK:       [[SOME_BAR]]:
// CHECK:         function_ref @_TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
// CHECK:         br [[BAR_CONT:bb[0-9]+]]
// CHECK:       [[NO_BAR_2]]:
// CHECK:         function_ref @_TFSs26_injectNothingIntoOptionalU__FT_GSqQ__
// CHECK:         br [[BAR_CONT]]
// CHECK:       [[BAR_CONT]]:
// CHECK:         function_ref @_TFSs22_doesOptionalHaveValueU__FRGSqQ__Bi1_
// CHECK:         cond_br {{%.*}}, [[SOME_BAR_3:bb[0-9]+]], [[NO_BAR_3:bb[0-9]+]]
// CHECK:       [[NO_BAR_3]]:
// CHECK:         br [[TRAP]]
// CHECK:       [[SOME_BAR_3]]:
// CHECK:         class_method {{%.*}} : $Bar, #Bar.bas!getter.1
// CHECK:         function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
// CHECK:         unconditional_checked_cast {{%.*}} : $C to $D
func test(x: Foo) -> D {
  return x.bar?.bas as D
}
