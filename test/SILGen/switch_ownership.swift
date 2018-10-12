// RUN: %target-swift-emit-silgen -module-name switch -enable-sil-ownership %s | %FileCheck %s
//
// A temporary file for testing switch code around ownership. Once SILGenPattern
// refactoring is complete, this will be merged into the normal pattern file.

//////////////////
// Declarations //
//////////////////

class Klass {
  init() {}
}

enum Foo { case A, B }

enum NonTrivialFoo {
case A(Klass)
case B(Klass)
}

func a() {}
func b() {}
func c() {}
func d() {}
func e() {}
func f() {}
func g() {}

///////////
// Tests //
///////////

// CHECK-LABEL: sil hidden @$s6switch05test_A19_two_trivial_unions1x1yyAA3FooO_AFtF : $@convention(thin) (Foo, Foo) -> () {
func test_switch_two_trivial_unions(x: Foo, y: Foo) {
  // CHECK:   [[T0:%.*]] = tuple (%0 : $Foo, %1 : $Foo)
  // CHECK:   ([[X:%.*]], [[Y:%.*]]) = destructure_tuple [[T0]]
  // CHECK:   switch_enum [[Y]] : $Foo, case #Foo.A!enumelt: [[IS_CASE1:bb[0-9]+]], default [[IS_NOT_CASE1:bb[0-9]+]]

  switch (x, y) {
  // CHECK: [[IS_CASE1]]:
  case (_, Foo.A):
  // CHECK:   function_ref @$s6switch1ayyF
    a()

  // CHECK: [[IS_NOT_CASE1]](
  // CHECK:   switch_enum [[X]] : $Foo, case #Foo.B!enumelt: [[IS_CASE2:bb[0-9]+]], default [[IS_NOT_CASE2:bb[0-9]+]]
  // CHECK: [[IS_CASE2]]:
  case (Foo.B, _):
  // CHECK:   function_ref @$s6switch1byyF
    b()

  // CHECK: [[IS_NOT_CASE2]](
  // CHECK:   switch_enum [[Y]] : $Foo, case #Foo.B!enumelt: [[IS_CASE3:bb[0-9]+]], default [[UNREACHABLE:bb[0-9]+]]
  // CHECK: [[IS_CASE3]]:
  case (_, Foo.B):
  // CHECK:   function_ref @$s6switch1cyyF
    c()

  // CHECK: [[UNREACHABLE]](
  // CHECK:   unreachable
  }
}
// CHECK: } // end sil function '$s6switch05test_A19_two_trivial_unions1x1yyAA3FooO_AFtF'

// CHECK-LABEL: sil hidden @$s6switch05test_A22_two_nontrivial_unions1x1yyAA13NonTrivialFooO_AFtF : $@convention(thin) (@guaranteed NonTrivialFoo, @guaranteed NonTrivialFoo) -> () {
func test_switch_two_nontrivial_unions(x: NonTrivialFoo, y: NonTrivialFoo) {
  // CHECK:   [[ARG0_COPY:%.*]] = copy_value %0
  // CHECK:   [[ARG1_COPY:%.*]] = copy_value %1
  // CHECK:   [[T0:%.*]] = tuple ([[ARG0_COPY]] : $NonTrivialFoo, [[ARG1_COPY]] : $NonTrivialFoo)
  // CHECK:   ([[X:%.*]], [[Y:%.*]]) = destructure_tuple [[T0]]
  // CHECK:   [[BORROWED_Y:%.*]] = begin_borrow [[Y]]
  // CHECK:   [[BORROWED_Y_COPY:%.*]] = copy_value [[BORROWED_Y]]
  // CHECK:   switch_enum [[BORROWED_Y_COPY]] : $NonTrivialFoo, case #NonTrivialFoo.A!enumelt.1: [[IS_CASE1:bb[0-9]+]], default [[IS_NOT_CASE1:bb[0-9]+]]

  switch (x, y) {
  // CHECK: [[IS_CASE1]]({{%.*}} : @owned $Klass)
  case (_, NonTrivialFoo.A):
  // CHECK:   function_ref @$s6switch1ayyF
    a()

  // CHECK: [[IS_NOT_CASE1]]({{%.*}} : @owned $NonTrivialFoo):
  // CHECK:   [[BORROWED_X:%.*]] = begin_borrow [[X]]
  // CHECK:   [[BORROWED_X_COPY:%.*]] = copy_value [[BORROWED_X]]
  // CHECK:   switch_enum [[BORROWED_X_COPY]] : $NonTrivialFoo, case #NonTrivialFoo.B!enumelt.1: [[IS_CASE2:bb[0-9]+]], default [[IS_NOT_CASE2:bb[0-9]+]]
  // CHECK: [[IS_CASE2]]({{%.*}} : @owned $Klass)
  case (NonTrivialFoo.B, _):
  // CHECK:   function_ref @$s6switch1byyF
    b()

  // CHECK: [[IS_NOT_CASE2]]({{%.*}} : @owned $NonTrivialFoo)
  // CHECK:   [[Y_COPY:%.*]] = copy_value [[Y]]
  // CHECK:   switch_enum [[Y_COPY]] : $NonTrivialFoo, case #NonTrivialFoo.B!enumelt.1: [[IS_CASE3:bb[0-9]+]], default [[UNREACHABLE:bb[0-9]+]]
  // CHECK: [[IS_CASE3]]({{%.*}} : @owned $Klass):
  case (_, NonTrivialFoo.B):
  // CHECK:   function_ref @$s6switch1cyyF
    c()

  // CHECK: [[UNREACHABLE]]({{%.*}} : @owned $NonTrivialFoo):
  // CHECK:   unreachable
  }
}
// CHECK: } // end sil function '$s6switch05test_A22_two_nontrivial_unions1x1yyAA13NonTrivialFooO_AFtF'
