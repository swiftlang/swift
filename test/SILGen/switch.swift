// RUN: %swift -emit-silgen %s | FileCheck %s

// TODO: Implement tuple equality in the library.
// BLOCKED: <rdar://problem/13822406>
func [infix] ~= (x:(Int, Int), y:(Int, Int)) -> Bool {
  return x.0 == y.0 && x.1 == y.1
}

// Some fake predicates for pattern guards.
func runced() -> Bool { return true }
func funged() -> Bool { return true }
func ansed() -> Bool { return true }

func foo() -> Int { return 0 }
func bar() -> Int { return 0 }
func foobar() -> (Int, Int) { return (0, 0) }

func a() {}
func b() {}
func c() {}
func d() {}
func e() {}
func f() {}
func g() {}

// CHECK: sil @_T6switch5test1FT_T_
func test1() {
  switch foo() {
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case _:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  b()
}

// CHECK: sil @_T6switch5test2FT_T_
func test2() {
  switch foo() {
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case _:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  case _: // The second case is unreachable.
    b()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  c()
}

// CHECK: sil @_T6switch5test3FT_T_
func test3() {
  switch foo() {
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   function_ref @_T6switch6runcedFT_Sb
  // CHECK:   condbranch {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NO_CASE2:bb[0-9]+]]

  // -- TODO: Clean up these empty waypoint bbs.
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case _ where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NO_CASE2]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case _:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  c()
}

// CHECK: sil @_T6switch5test4FT_T_
func test4() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case _:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  b()
}

// CHECK: sil @_T6switch5test5FT_T_
func test5() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   function_ref @_T6switch6runcedFT_Sb
  // CHECK:   condbranch {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case _ where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @_T6switch6fungedFT_Sb
  // CHECK:   condbranch {{%.*}}, [[YES_CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  // CHECK: [[YES_CASE2]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case (_, _) where funged():
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[NOT_CASE2]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case _:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1dFT_T_
  d()
}

// CHECK: sil @_T6switch5test6FT_T_
func test6() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case (_, _):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  case (_, _): // The second case is unreachable.
    b()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  c()
}

// CHECK: sil @_T6switch5test7FT_T_
func test7() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   function_ref @_T6switch6runcedFT_Sb
  // CHECK:   condbranch {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case (_, _) where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NOT_CASE1]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case (_, _):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()
  }
  c()
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
}

// CHECK: sil @_T6switch5test8FT_T_
func test8() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   function_ref @_T6switch6foobarFT_TSiSi_
  // CHECK:   condbranch {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case foobar():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   condbranch {{%.*}}, [[YES_CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  // CHECK: [[YES_CASE2]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case (foo(), _):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()
  // CHECK: [[NOT_CASE2]]:
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE3_GUARD:bb[0-9]+]], [[NOT_CASE3:bb[0-9]+]]
  // CHECK: [[CASE3_GUARD]]:
  // CHECK:   function_ref @_T6switch6runcedFT_Sb
  // CHECK:   condbranch {{%.*}}, [[YES_CASE3:bb[0-9]+]], [[NOT_CASE3]]
  // CHECK: [[YES_CASE3]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case (_, bar()) where runced():
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()
  // CHECK: [[NOT_CASE3]]:
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE4_GUARD_1:bb[0-9]+]], [[NOT_CASE4:bb[0-9]+]]
  // CHECK: [[CASE4_GUARD_1]]:
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE4_GUARD_2:bb[0-9]+]], [[NOT_CASE4]]
  // CHECK: [[CASE4_GUARD_2]]:
  // CHECK:   function_ref @_T6switch6fungedFT_Sb
  // CHECK:   condbranch {{%.*}}, [[YES_CASE4:bb[0-9]+]], [[NOT_CASE4]]
  // CHECK: [[YES_CASE4]]:
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case (foo(), bar()) where funged():
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_T6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()
  // CHECK: [[NOT_CASE4]]:
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE5_GUARD_1:bb[0-9]+]], [[NOT_CASE5:bb[0-9]+]]
  // CHECK: [[CASE5_GUARD_1]]:
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   condbranch {{%.*}}, [[YES_CASE5:bb[0-9]+]], [[NOT_CASE5:bb[0-9]+]]
  // CHECK: [[YES_CASE5]]:
  // CHECK:   br [[CASE5:bb[0-9]+]]
  case (foo(), bar()):
  // CHECK: [[CASE5]]:
  // CHECK:   function_ref @_T6switch1eFT_T_
  // CHECK:   br [[CONT]]
    e()
  // CHECK: [[NOT_CASE5]]:
  // CHECK:   br [[CASE6:bb[0-9]+]]
  case _:
  // CHECK: [[CASE6]]:
  // CHECK:   function_ref @_T6switch1fFT_T_
  // CHECK:   br [[CONT]]
    f()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1gFT_T_
  g()
}

// CHECK: sil @_T6switch5test9FT_T_
func test9() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   condbranch {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case (foo(), _):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @_T6switch6foobarFT_TSiSi_
  // CHECK:   condbranch {{%.*}}, [[YES_CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  // CHECK: [[YES_CASE2]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case foobar():
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[NOT_CASE2]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case _:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1dFT_T_
  d()
}

// CHECK: sil @_T6switch6test10FT_T_
func test10() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @_TSsoi2zzFT3minSi3maxSi_VSs17IntEnumeratorType
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   condbranch {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case (foo()..bar(), _):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NOT_CASE1]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case _:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  c()
}

protocol P { func p() }

struct X : P { func p() {} }
struct Y : P { func p() {} }
struct Z : P { func p() {} }

// CHECK: sil @_T6switch10test_isa_1FT1pPS_1P__T_
func test_isa_1(p:P) {
  switch p {
  // CHECK:   [[CAST_X:%.*]] = project_downcast_existential_addr conditional [[P:%.*]] : $*P to $*X
  // CHECK:   [[TEST_X:%.*]] = is_nonnull [[CAST_X]] : $*X
  // CHECK:   condbranch [[TEST_X]], [[IS_X:bb[0-9]+]], [[IS_NOT_X:bb[0-9]+]]

  // CHECK: [[IS_NOT_X]]:
  // CHECK:   [[CAST_Y:%.*]] = project_downcast_existential_addr conditional [[P]] : $*P to $*Y
  // CHECK:   [[TEST_Y:%.*]] = is_nonnull [[CAST_Y]] : $*Y
  // CHECK:   condbranch [[TEST_Y]], [[IS_Y:bb[0-9]+]], [[IS_NOT_Y:bb[0-9]+]]

  // CHECK: [[IS_NOT_Y]]:
  // CHECK:   [[CAST_Z:%.*]] = project_downcast_existential_addr conditional [[P]] : $*P to $*Z
  // CHECK:   [[TEST_Z:%.*]] = is_nonnull [[CAST_Z]] : $*Z
  // CHECK:   condbranch [[TEST_Z]], [[IS_Z:bb[0-9]+]], [[IS_NOT_Z:bb[0-9]+]]

  // CHECK: [[IS_X]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case is X:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_Y]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case is Y:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[Y_CONT:bb[0-9]+]]
    b()

  // CHECK: [[IS_Z]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case is Z:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  // CHECK:   br [[Z_CONT:bb[0-9]+]]
    c()

  // CHECK: [[IS_NOT_Z]]:
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case _:
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_T6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1eFT_T_
  e()
}

// CHECK: sil @_T6switch10test_isa_2FT1pPS_1P__T_
func test_isa_2(p:P) {
  switch (p, foo()) {
  // CHECK:   [[CAST_X:%.*]] = project_downcast_existential_addr conditional [[P:%.*]] : $*P to $*X
  // CHECK:   [[TEST_X:%.*]] = is_nonnull [[CAST_X]] : $*X
  // CHECK:   condbranch [[TEST_X]], [[IS_X:bb[0-9]+]], [[IS_NOT_X:bb[0-9]+]]
  // CHECK: [[IS_NOT_X]]:
  // CHECK:   [[CAST_Y:%.*]] = project_downcast_existential_addr conditional [[P]] : $*P to $*Y
  // CHECK:   [[TEST_Y:%.*]] = is_nonnull [[CAST_Y]] : $*Y
  // CHECK:   condbranch [[TEST_Y]], [[IS_Y:bb[0-9]+]], [[IS_NOT_Y:bb[0-9]+]]

  // CHECK: [[IS_X]]:
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   condbranch {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case (is X, foo()):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NOT_CASE1]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case (is Y, foo()):
  // -- Emitted inside the 'is Y' dispatch tree below
    b()
  case (is X, _):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[IS_Y]]:
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   condbranch {{%.*}}, [[YES_CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  // CHECK: [[YES_CASE2]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  // -- case (is Y, foo()):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
  // CHECK: [[NOT_CASE2]]:
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   condbranch {{%.*}}, [[YES_CASE4:bb[0-9]+]], [[NOT_CASE4:bb[0-9]+]]
  // CHECK: [[YES_CASE4]]:
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case (is Y, bar()):
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_T6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()

  // CHECK: [[NOT_CASE4]]:
  // CHECK:   br [[CASE5:bb[0-9]+]]

  // CHECK: [[IS_NOT_Y]]:
  // CHECK:   br [[CASE5]]
  case _:
  // CHECK: [[CASE5]]:
  // CHECK:   function_ref @_T6switch1eFT_T_
  // CHECK:   br [[CONT]]
    e()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1fFT_T_
  f()
}

class B {}
class D1 : B {}
class D2 : D1 {}

func test_isa_class_1(x:B) {
  switch x {
  // CHECK:   [[CAST_D1:%.*]] = downcast conditional [[X:%.*]] : $B to $D1
  // CHECK:   [[TEST_D1:%.*]] = is_nonnull %5 : $D1
  // CHECK:   condbranch [[TEST_D1]], [[IS_D1:bb[0-9]+]], [[IS_NOT_D1:bb[0-9]+]]
  // CHECK: [[IS_NOT_D1]]:
  // CHECK:   [[CAST_D2:%.*]] = downcast conditional [[X:%.*]] : $B to $D2
  // CHECK:   [[TEST_D2:%.*]] = is_nonnull %8 : $D2
  // CHECK:   condbranch [[TEST_D2]], [[IS_D2:bb[0-9]+]], [[IS_NOT_D2:bb[0-9]+]]

  // CHECK: [[IS_D1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case is D1:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_D2]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case is D2:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    b()

  // CHECK: [[IS_NOT_D2]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  default:
  // CHECK: [[CASE3]]:
  // CHECK:  function_ref @_T6switch1cFT_T_
  // CHECK:  br [[CONT]]
    c()
  }
  // CHECK: [[CONT]]:
  d()
}

union MaybePair {
  case Neither
  case Left(Int)
  case Right(String)
  case Both(Int, String)
}

// CHECK: sil @_T6switch12test_union_1FT1uOS_9MaybePair_T_
func test_union_1(u:MaybePair) {
  switch u {
  // CHECK: switch_union {{%.*}} : $MaybePair,
  // CHECK:   case #MaybePair.Neither!unionelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybePair.Left!unionelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Right!unionelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Both!unionelt.1: [[IS_BOTH:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case .Neither:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]({{%.*}}):
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case .Left:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]({{%.*}}):
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case .Right:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[IS_BOTH]]({{%.*}}):
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case .Both:
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_T6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()
  }

  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1eFT_T_
  e()
}

// CHECK: sil @_T6switch12test_union_2FT1uOS_9MaybePair_T_
func test_union_2(u:MaybePair) {
  switch u {
  // CHECK: switch_union {{%.*}} : $MaybePair,
  // CHECK:   case #MaybePair.Neither!unionelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybePair.Left!unionelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Right!unionelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   default [[DEFAULT:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case .Neither:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]({{%.*}}):
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case .Left:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]({{%.*}}):
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case .Right:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // -- missing .Both case
  // CHECK: [[DEFAULT]]:
  // CHECK:   unreachable
  }

  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1dFT_T_
  d()
}

// CHECK: sil @_T6switch12test_union_3FT1uOS_9MaybePair_T_
func test_union_3(u:MaybePair) {
  switch u {
  // CHECK: switch_union {{%.*}} : $MaybePair,
  // CHECK:   case #MaybePair.Neither!unionelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybePair.Left!unionelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Right!unionelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   default [[DEFAULT:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case .Neither:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]({{%.*}}):
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case .Left:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]({{%.*}}):
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case .Right:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[DEFAULT]]:
  // CHECK:   br [[CASE4:bb[0-9]+]]
  default:
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_T6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()
  }

  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1eFT_T_
  e()
}

// CHECK: sil @_T6switch12test_union_4FT1uOS_9MaybePair_T_
func test_union_4(u:MaybePair) {
  switch u {
  // CHECK: switch_union {{%.*}} : $MaybePair,
  // CHECK:   case #MaybePair.Neither!unionelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybePair.Left!unionelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Right!unionelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Both!unionelt.1: [[IS_BOTH:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case .Neither(_):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]({{%.*}}):
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case .Left(_):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]({{%.*}}):
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case .Right(_):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[IS_BOTH]]({{%.*}}):
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case .Both(_):
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_T6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()
  }

  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1eFT_T_
  e()
}

// CHECK: sil @_T6switch12test_union_5FT1uOS_9MaybePair_T_
func test_union_5(u:MaybePair) {
  switch u {
  // CHECK: switch_union {{%.*}} : $MaybePair,
  // CHECK:   case #MaybePair.Neither!unionelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybePair.Left!unionelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Right!unionelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Both!unionelt.1: [[IS_BOTH:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case .Neither():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]({{%.*}}):
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case .Left(_):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]({{%.*}}):
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case .Right(_):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[IS_BOTH]]({{%.*}}):
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case .Both(_, _):
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_T6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()
  }

  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T6switch1eFT_T_
  e()
}


