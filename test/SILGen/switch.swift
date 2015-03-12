// RUN: %target-swift-frontend -enable-experimental-patterns -emit-silgen %s | FileCheck %s

// TODO: Implement tuple equality in the library.
// BLOCKED: <rdar://problem/13822406>
func ~= (x: (Int, Int), y: (Int, Int)) -> Bool {
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

// CHECK-LABEL: sil hidden  @_TF6switch5test1FT_T_
func test1() {
  switch foo() {
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case _:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  b()
}

// CHECK-LABEL: sil hidden  @_TF6switch5test2FT_T_
func test2() {
  switch foo() {
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case _:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  case _: // The second case is unreachable.
    b()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  c()
}

// CHECK-LABEL: sil hidden  @_TF6switch5test3FT_T_
func test3() {
  switch foo() {
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   function_ref @_TF6switch6runcedFT_Sb
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NO_CASE2:bb[0-9]+]]

  // -- TODO: Clean up these empty waypoint bbs.
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case _ where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NO_CASE2]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case _:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  c()
}

// CHECK-LABEL: sil hidden  @_TF6switch5test4FT_T_
func test4() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   function_ref @_TF6switch3barFT_Si
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case _:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  b()
}

// CHECK-LABEL: sil hidden  @_TF6switch5test5FT_T_
func test5() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   function_ref @_TF6switch3barFT_Si
  // CHECK:   function_ref @_TF6switch6runcedFT_Sb
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case _ where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @_TF6switch6fungedFT_Sb
  // CHECK:   cond_br {{%.*}}, [[YES_CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  // CHECK: [[YES_CASE2]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case (_, _) where funged():
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[NOT_CASE2]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case _:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  d()
}

// CHECK-LABEL: sil hidden  @_TF6switch5test6FT_T_
func test6() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   function_ref @_TF6switch3barFT_Si
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case (_, _):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  case (_, _): // The second case is unreachable.
    b()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  c()
}

// CHECK-LABEL: sil hidden  @_TF6switch5test7FT_T_
func test7() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   function_ref @_TF6switch3barFT_Si
  // CHECK:   function_ref @_TF6switch6runcedFT_Sb
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case (_, _) where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NOT_CASE1]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case (_, _):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()
  }
  c()
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
}

// CHECK-LABEL: sil hidden  @_TF6switch5test8FT_T_
func test8() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   function_ref @_TF6switch3barFT_Si
  // CHECK:   function_ref @_TF6switch6foobarFT_TSiSi_
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case foobar():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   cond_br {{%.*}}, [[CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  case (foo(), _):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()
  // CHECK: [[NOT_CASE2]]:
  // CHECK:   function_ref @_TF6switch3barFT_Si
  // CHECK:   cond_br {{%.*}}, [[CASE3_GUARD:bb[0-9]+]], [[NOT_CASE3:bb[0-9]+]]
  // CHECK: [[CASE3_GUARD]]:
  // CHECK:   function_ref @_TF6switch6runcedFT_Sb
  // CHECK:   cond_br {{%.*}}, [[CASE3:bb[0-9]+]], [[NOT_CASE3_GUARD:bb[0-9]+]]
  case (_, bar()) where runced():
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()
  // CHECK: [[NOT_CASE3_GUARD]]:
  // CHECK: [[NOT_CASE3]]:
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   cond_br {{%.*}}, [[CASE4_GUARD_1:bb[0-9]+]], [[NOT_CASE4_1:bb[0-9]+]]
  // CHECK: [[CASE4_GUARD_1]]:
  // CHECK:   function_ref @_TF6switch3barFT_Si
  // CHECK:   cond_br {{%.*}}, [[CASE4_GUARD_2:bb[0-9]+]], [[NOT_CASE4_2:bb[0-9]+]]
  // CHECK: [[CASE4_GUARD_2]]:
  // CHECK:   function_ref @_TF6switch6fungedFT_Sb
  // CHECK:   cond_br {{%.*}}, [[CASE4:bb[0-9]+]], [[NOT_CASE4_3:bb[0-9]+]]
  case (foo(), bar()) where funged():
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()
  // CHECK: [[NOT_CASE4_3]]:
  // CHECK: [[NOT_CASE4_2]]:
  // CHECK: [[NOT_CASE4_1]]:
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   cond_br {{%.*}}, [[CASE5_GUARD_1:bb[0-9]+]], [[NOT_CASE5:bb[0-9]+]]
  // CHECK: [[CASE5_GUARD_1]]:
  // CHECK:   function_ref @_TF6switch3barFT_Si
  // CHECK:   cond_br {{%.*}}, [[YES_CASE5:bb[0-9]+]], [[NOT_CASE5:bb[0-9]+]]
  // CHECK: [[YES_CASE5]]:
  // CHECK:   br [[CASE5:bb[0-9]+]]
  case (foo(), bar()):
  // CHECK: [[CASE5]]:
  // CHECK:   function_ref @_TF6switch1eFT_T_
  // CHECK:   br [[CONT]]
    e()
  // CHECK: [[NOT_CASE5]]:
  // CHECK:   br [[CASE6:bb[0-9]+]]
  case _:
  // CHECK: [[CASE6]]:
  // CHECK:   function_ref @_TF6switch1fFT_T_
  // CHECK:   br [[CONT]]
    f()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1gFT_T_
  g()
}

// CHECK-LABEL: sil hidden  @_TF6switch5test9FT_T_
func test9() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   function_ref @_TF6switch3barFT_Si
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case (foo(), _):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @_TF6switch6foobarFT_TSiSi_
  // CHECK:   cond_br {{%.*}}, [[YES_CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  // CHECK: [[YES_CASE2]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case foobar():
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[NOT_CASE2]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case _:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  d()
}

// CHECK-LABEL: sil hidden  @_TF6switch6test10FT_T_
func test10() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   function_ref @_TF6switch3barFT_Si
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case (foo()...bar(), _):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NOT_CASE1]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case _:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  c()
}

protocol P { func p() }

struct X : P { func p() {} }
struct Y : P { func p() {} }
struct Z : P { func p() {} }

// CHECK-LABEL: sil hidden  @_TF6switch10test_isa_1FT1pPS_1P__T_
func test_isa_1(#p: P) {
  switch p {
  // CHECK:   checked_cast_addr_br take_on_success P in [[P:%.*]] : $*P to X in {{%.*}} : $*X, [[IS_X:bb[0-9]+]], [[IS_NOT_X:bb[0-9]+]]

  // CHECK: [[IS_X]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case is X:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_NOT_X]]:
  // CHECK:   checked_cast_addr_br take_on_success P in [[P]] : $*P to Y in {{%.*}} : $*Y, [[IS_Y:bb[0-9]+]], [[IS_NOT_Y:bb[0-9]+]]

// CHECK: [[IS_Y]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case is Y:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[Y_CONT:bb[0-9]+]]
    b()

  // CHECK: [[IS_NOT_Y]]:
  // CHECK:   checked_cast_addr_br take_on_success P in [[P]] : $*P to Z in {{%.*}} : $*Z, [[IS_Z:bb[0-9]+]], [[IS_NOT_Z:bb[0-9]+]]

  // CHECK: [[IS_Z]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case is Z:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[Z_CONT:bb[0-9]+]]
    c()

  // CHECK: [[IS_NOT_Z]]:
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case _:
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1eFT_T_
  e()
}

// CHECK-LABEL: sil hidden  @_TF6switch10test_isa_2FT1pPS_1P__T_
func test_isa_2(#p: P) {
  switch (p, foo()) {
  // CHECK:   checked_cast_addr_br copy_on_success P in [[P:%.*]] : $*P to X in {{%.*}} : $*X, [[IS_X:bb[0-9]+]], [[IS_NOT_X:bb[0-9]+]]

  // CHECK: [[IS_X]]:
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   cond_br {{%.*}}, [[CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  case (is X, foo()):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_NOT_X]]:
  // CHECK:   checked_cast_addr_br copy_on_success P in [[P]] : $*P to Y in {{%.*}} : $*Y, [[IS_Y:bb[0-9]+]], [[IS_NOT_Y:bb[0-9]+]]

  // CHECK: [[IS_Y]]:
  // CHECK:   function_ref @_TF6switch3fooFT_Si
  // CHECK:   cond_br {{%.*}}, [[CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  case (is Y, foo()):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[NOT_CASE2]]:
  // CHECK: [[IS_NOT_Y]]:

  // CHECK:   checked_cast_addr_br take_on_success P in [[P:%.*]] : $*P to X in {{%.*}} : $*X, [[CASE3:bb[0-9]+]], [[IS_NOT_X:bb[0-9]+]]

  case (is X, _):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // -- case (is Y, foo()):
  // CHECK: [[IS_NOT_X]]:
  // CHECK:   checked_cast_addr_br copy_on_success P in [[P]] : $*P to Y in {{%.*}} : $*Y, [[IS_Y:bb[0-9]+]], [[IS_NOT_Y:bb[0-9]+]]
  // CHECK: [[IS_Y]]:
  // CHECK:   function_ref @_TF6switch3barFT_Si
  // CHECK:   cond_br {{%.*}}, [[CASE4:bb[0-9]+]], [[NOT_CASE4:bb[0-9]+]]
  case (is Y, bar()):
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()

  // CHECK: [[NOT_CASE4]]:
  // CHECK:   br [[CASE5:bb[0-9]+]]
  // CHECK: [[IS_NOT_Y]]:
  // CHECK:   br [[CASE5]]
  case _:
  // CHECK: [[CASE5]]:
  // CHECK:   function_ref @_TF6switch1eFT_T_
  // CHECK:   br [[CONT]]
    e()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1fFT_T_
  f()
}

class B {}
class C : B {}
class D1 : C {}
class D2 : D1 {}
class E : C {}

// CHECK-LABEL: sil hidden @_TF6switch16test_isa_class_1FT1xCS_1B_T_
func test_isa_class_1(let #x: B) {
  // CHECK: strong_retain %0
  switch x {
  // CHECK:   checked_cast_br [[X:%.*]] : $B to $D1, [[IS_D1:bb[0-9]+]], [[IS_NOT_D1:bb[0-9]+]]

  // CHECK: [[IS_D1]]([[CAST_D1:%.*]]):
  // CHECK:   function_ref @_TF6switch6runcedFT_Sb
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NO_CASE1:bb[0-9]+]]

  // CHECK: [[YES_CASE1]]:
  // CHECK:   strong_release [[CAST_D1]]
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case is D1 where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NO_CASE1]]:
  // CHECK: [[IS_NOT_D1]]:
  // CHECK:   checked_cast_br [[X]] : $B to $D2, [[IS_D2:bb[0-9]+]], [[IS_NOT_D2:bb[0-9]+]]

  // CHECK: [[IS_D2]]([[CAST_D2:%.*]]):
  // CHECK:   strong_release [[CAST_D2]]
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case is D2:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_NOT_D2]]:
  // CHECK:   checked_cast_br [[X]] : $B to $E, [[IS_E:bb[0-9]+]], [[IS_NOT_E:bb[0-9]+]]

  // CHECK: [[IS_E]]([[CAST_E:%.*]]):
  // CHECK:   function_ref @_TF6switch6fungedFT_Sb
  // CHECK:   cond_br {{%.*}}, [[CASE3:bb[0-9]+]], [[NO_CASE3:bb[0-9]+]]

  case is E where funged():
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[NO_CASE3]]:
  // CHECK: [[IS_NOT_E]]:
  // CHECK:   checked_cast_br [[X]] : $B to $C, [[IS_C:bb[0-9]+]], [[IS_NOT_C:bb[0-9]+]]

  // CHECK: [[IS_C]]([[CAST_C:%.*]]):
  // CHECK:   br [[CASE4]]
  case is C:
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()

  // CHECK: [[IS_NOT_C]]:
  // CHECK:   strong_release [[X]]
  // CHECK:   br [[CASE5:bb[0-9]+]]
  default:
  // CHECK: [[CASE5]]:
  // CHECK:  function_ref @_TF6switch1eFT_T_
  // CHECK:  br [[CONT]]
    e()
  }
  // CHECK: [[CONT]]:
  // CHECK: strong_release %0
  f()
}

// CHECK-LABEL: sil hidden @_TF6switch16test_isa_class_2FT1xCS_1B_PSs9AnyObject_
func test_isa_class_2(#x: B) -> AnyObject {
  // CHECK:   strong_retain [[X:%0]]
  switch x {
  // CHECK:   checked_cast_br [[X]] : $B to $D1, [[IS_D1:bb[0-9]+]], [[IS_NOT_D1:bb[0-9]+]]

  // CHECK: [[IS_D1]]([[CAST_D1:%.*]]):
  // CHECK:   strong_retain [[CAST_D1]]
  // CHECK:   function_ref @_TF6switch6runcedFT_Sb
  // CHECK:   cond_br {{%.*}}, [[CASE1:bb[0-9]+]], [[NO_CASE1:bb[0-9]+]]

  case let y as D1 where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   [[RET:%.*]] = init_existential_ref [[CAST_D1]]
  // CHECK:   strong_release [[X]] : $B
  // CHECK:   br [[CONT:bb[0-9]+]]([[RET]] : $AnyObject)
    a()
    return y

  // CHECK: [[NO_CASE1]]:
  // CHECK:   strong_release [[CAST_D1]]
  // CHECK: [[IS_NOT_D1]]:
  // CHECK:   checked_cast_br [[X]] : $B to $D2, [[CASE2:bb[0-9]+]], [[IS_NOT_D2:bb[0-9]+]]

  case let y as D2:
  // CHECK: [[CASE2]]([[CAST_D2:%.*]]):
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   [[RET:%.*]] = init_existential_ref [[CAST_D2]]
  // CHECK:   br [[CONT]]([[RET]] : $AnyObject)
    b()
    return y

  // CHECK: [[IS_NOT_D2]]:
  // CHECK:   checked_cast_br [[X]] : $B to $E, [[IS_E:bb[0-9]+]], [[IS_NOT_E:bb[0-9]+]]

  // CHECK: [[IS_E]]([[CAST_E:%.*]]):
  // CHECK:   strong_retain [[CAST_E]]
  // CHECK:   function_ref @_TF6switch6fungedFT_Sb
  // CHECK:   cond_br {{%.*}}, [[CASE3:bb[0-9]+]], [[NO_CASE3:bb[0-9]+]]

  case let y as E where funged():
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   [[RET:%.*]] = init_existential_ref [[CAST_E]]
  // CHECK:   strong_release [[X]] : $B
  // CHECK:   br [[CONT]]([[RET]] : $AnyObject)
    c()
    return y

  // CHECK: [[NO_CASE3]]:
  // CHECK    strong_release [[CAST_E]]
  // CHECK: [[IS_NOT_E]]:
  // CHECK:   checked_cast_br [[X]] : $B to $C, [[CASE4:bb[0-9]+]], [[IS_NOT_C:bb[0-9]+]]
  case let y as C:
  // CHECK: [[CASE4]]([[CAST_C:%.*]]):
  // CHECK:   function_ref @_TF6switch1dFT_T_
  // CHECK:   [[RET:%.*]] = init_existential_ref [[CAST_C]]
  // CHECK:   br [[CONT]]([[RET]] : $AnyObject)
    d()
    return y

  // CHECK: [[IS_NOT_C]]:
  // CHECK:   strong_release [[X]]
  // CHECK:   br [[CASE5:bb[0-9]+]]
  default:
  // CHECK: [[CASE5]]:
  // CHECK:   function_ref @_TF6switch1eFT_T_
  // CHECK:   strong_retain [[X]]
  // CHECK:   [[RET:%.*]] = init_existential_ref [[X]]
  // CHECK:   br [[CONT]]([[RET]] : $AnyObject)
    e()
    return x
  }

  // CHECK: [[CONT]]([[T0:%.*]] : $AnyObject):
  // CHECK:   strong_release [[X]]
  // CHECK:   return [[T0]]
}

enum MaybePair {
  case Neither
  case Left(Int)
  case Right(String)
  case Both(Int, String)
}

// CHECK-LABEL: sil hidden  @_TF6switch12test_union_1FT1uOS_9MaybePair_T_
func test_union_1(#u: MaybePair) {
  switch u {
  // CHECK: switch_enum [[SUBJECT:%.*]] : $MaybePair,
  // CHECK:   case #MaybePair.Neither!enumelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybePair.Left!enumelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Right!enumelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Both!enumelt.1: [[IS_BOTH:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  // CHECK-NOT: release
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case .Neither:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]({{%.*}}):
  // CHECK-NOT: release
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case (.Left):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]([[STR:%.*]] : $String):
  // CHECK:   release_value [[STR]] : $String
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case var .Right:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[IS_BOTH]]([[TUP:%.*]] : $(Int, String)):
  // CHECK:   release_value [[TUP]] : $(Int, String)
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case .Both:
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()
  }

  // CHECK: [[CONT]]:
  // CHECK-NOT: switch_enum [[SUBJECT]]
  // CHECK:   function_ref @_TF6switch1eFT_T_
  e()
}

// CHECK-LABEL: sil hidden  @_TF6switch12test_union_2FT1uOS_9MaybePair_T_
func test_union_2(#u: MaybePair) {
  switch u {
  // CHECK: switch_enum {{%.*}} : $MaybePair,
  // CHECK:   case #MaybePair.Neither!enumelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybePair.Left!enumelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Right!enumelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   default [[DEFAULT:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case .Neither:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]({{%.*}}):
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case .Left:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]({{%.*}}):
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case .Right:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // -- missing .Both case
  // CHECK: [[DEFAULT]]:
  // CHECK:   unreachable
  }

  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  d()
}

// CHECK-LABEL: sil hidden  @_TF6switch12test_union_3FT1uOS_9MaybePair_T_
func test_union_3(#u: MaybePair) {
  // CHECK:   retain_value [[SUBJECT:%0]]
  switch u {
  // CHECK: switch_enum [[SUBJECT]] : $MaybePair,
  // CHECK:   case #MaybePair.Neither!enumelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybePair.Left!enumelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Right!enumelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   default [[DEFAULT:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case .Neither:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]({{%.*}}):
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case .Left:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]([[STR:%.*]] : $String):
  // CHECK:   release_value [[STR]] : $String
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case .Right:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[DEFAULT]]:
  // -- Emitted up above for some reason --
  // CHECK:   br [[DEFAULT_IMPL:bb[0-9]+]]
  // CHECK: [[DEFAULT_IMPL]]:
  // -- Ensure the fully-opaque value is destroyed in the default case.
  // CHECK:   release_value [[SUBJECT]] :
  // CHECK:   br [[CASE4:bb[0-9]+]]
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  // CHECK:   br [[CONT]]

  default:
    d()
  }

  // CHECK: [[CONT]]:
  // CHECK-NOT: switch_enum [[SUBJECT]]
  // CHECK:   function_ref @_TF6switch1eFT_T_
  // CHECK:   release_value [[SUBJECT]]
  e()
}

// CHECK-LABEL: sil hidden  @_TF6switch12test_union_4FT1uOS_9MaybePair_T_
func test_union_4(#u: MaybePair) {
  switch u {
  // CHECK: switch_enum {{%.*}} : $MaybePair,
  // CHECK:   case #MaybePair.Neither!enumelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybePair.Left!enumelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Right!enumelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Both!enumelt.1: [[IS_BOTH:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case .Neither(_):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]({{%.*}}):
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case .Left(_):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]({{%.*}}):
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case .Right(_):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[IS_BOTH]]({{%.*}}):
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case .Both(_):
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()
  }

  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1eFT_T_
  e()
}

// CHECK-LABEL: sil hidden  @_TF6switch12test_union_5FT1uOS_9MaybePair_T_
func test_union_5(#u: MaybePair) {
  switch u {
  // CHECK: switch_enum {{%.*}} : $MaybePair,
  // CHECK:   case #MaybePair.Neither!enumelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybePair.Left!enumelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Right!enumelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Both!enumelt.1: [[IS_BOTH:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case .Neither():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]({{%.*}}):
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case .Left(_):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]({{%.*}}):
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case .Right(_):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[IS_BOTH]]({{%.*}}):
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case .Both(_, _):
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()
  }

  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1eFT_T_
  e()
}

enum MaybeAddressOnlyPair {
  case Neither
  case Left(P)
  case Right(String)
  case Both(P, String)
}

func test_union_addr_only_1(#u: MaybeAddressOnlyPair) {
  switch u {
  // CHECK: switch_enum_addr [[ENUM_ADDR:%.*]] : $*MaybeAddressOnlyPair,
  // CHECK:   case #MaybeAddressOnlyPair.Neither!enumelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybeAddressOnlyPair.Left!enumelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybeAddressOnlyPair.Right!enumelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   case #MaybeAddressOnlyPair.Both!enumelt.1: [[IS_BOTH:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case .Neither:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]:
  // CHECK:   [[P:%.*]] = unchecked_take_enum_data_addr [[ENUM_ADDR]] : $*MaybeAddressOnlyPair, #MaybeAddressOnlyPair.Left!enumelt.1
  // CHECK:   destroy_addr [[P]]
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case .Left(_):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]:
  // CHECK:   [[STR_ADDR:%.*]] = unchecked_take_enum_data_addr [[ENUM_ADDR]] : $*MaybeAddressOnlyPair, #MaybeAddressOnlyPair.Right!enumelt.1
  // CHECK:   [[STR:%.*]] = load [[STR_ADDR]]
  // CHECK:   release_value [[STR]] : $String
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case .Right(_):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[IS_BOTH]]:
  // CHECK:   [[P_STR_TUPLE:%.*]] = unchecked_take_enum_data_addr [[ENUM_ADDR]] : $*MaybeAddressOnlyPair, #MaybeAddressOnlyPair.Both!enumelt.1
  // CHECK:   destroy_addr [[P_STR_TUPLE]]
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case .Both(_):
  // CHECK:   function_ref @_TF6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1eFT_T_
  e()
}

enum Generic<T, U> {
  case Foo(T)
  case Bar(U)
}

// Check that switching over a generic instance generates verified SIL.
func test_union_generic_instance(#u: Generic<Int, String>) {
  switch u {
  case .Foo(var x):
    a()
  case .Bar(var y):
    b()
  }
  c()
}

enum Foo { case A, B }

// CHECK: sil hidden @_TF6switch22test_switch_two_unionsFT1xOS_3Foo1yS0__T_
func test_switch_two_unions(#x: Foo, #y: Foo) {
  // CHECK:   [[T0:%.*]] = tuple (%0 : $Foo, %1 : $Foo)
  // CHECK:   [[X:%.*]] = tuple_extract [[T0]] : $(Foo, Foo), 0
  // CHECK:   [[Y:%.*]] = tuple_extract [[T0]] : $(Foo, Foo), 1

  // CHECK:   switch_enum [[Y]] : $Foo, case #Foo.A!enumelt: [[IS_CASE1:bb[0-9]+]], default [[IS_NOT_CASE1:bb[0-9]+]]

  switch (x, y) {
  // CHECK: [[IS_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case (_,     Foo.A):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
    a()

  // CHECK: [[IS_NOT_CASE1]]:
  // CHECK:   switch_enum [[X]] : $Foo, case #Foo.B!enumelt: [[IS_CASE2:bb[0-9]+]], default [[IS_NOT_CASE2:bb[0-9]+]]
  // CHECK: [[IS_CASE2]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case (Foo.B, _    ):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
    b()

  // CHECK: [[IS_NOT_CASE2]]:
  // CHECK:   switch_enum [[Y]] : $Foo, case #Foo.B!enumelt: [[IS_CASE3:bb[0-9]+]], default [[UNREACHABLE:bb[0-9]+]]
  // CHECK: [[IS_CASE3]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case (_,     Foo.B):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
    c()

  // CHECK: [[UNREACHABLE]]:
  // CHECK:   unreachable
  }
}

struct StructPatternTest {
  var x: Int
  var y: String
}

// CHECK: sil hidden @_TF6switch19test_struct_patternFT1sVS_17StructPatternTest_T_
func test_struct_pattern(#s: StructPatternTest) {
  switch s {
  // CHECK:   [[X:%.*]] = struct_extract [[S:%.*]] : $StructPatternTest, #StructPatternTest.x
  // CHECK:   [[Y:%.*]] = struct_extract [[S]] : $StructPatternTest, #StructPatternTest.y
  // CHECK:   retain_value [[Y]]
  // CHECK:   cond_br {{%.*}}, [[IS_CASE1:bb[0-9]+]], [[IS_NOT_CASE1:bb[0-9]+]]

  // CHECK: [[IS_CASE1]]:
  // CHECK:   release_value [[Y]]
  // CHECK:   release_value [[S]]
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case StructPatternTest(x: 0):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_NOT_CASE1]]:
  // CHECK:   cond_br {{%.*}}, [[IS_CASE2:bb[0-9]+]], [[IS_NOT_CASE2:bb[0-9]+]]

  // CHECK: [[IS_CASE2]]:
  // CHECK:   release_value [[Y]]
  // CHECK:   release_value [[S]]
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case StructPatternTest(y: ""):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_NOT_CASE2]]:
  // CHECK:   release_value [[Y]]
  // CHECK:   release_value [[S]]
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case (StructPatternTest(x: _)):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  d()
}

struct StructPatternTestAO {
  var x: Int
  var y: P
}

func ~=(a: P, b: P) -> Bool { return true }

// CHECK: sil hidden @_TF6switch22test_struct_pattern_aoFT1sVS_19StructPatternTestAO1pPS_1P__T_
func test_struct_pattern_ao(#s: StructPatternTestAO, #p: P) {
  // CHECK:   [[S:%.*]] = alloc_stack $StructPatternTestAO
  // CHECK:   copy_addr %0 to [initialization] [[S]]#1
  // CHECK:   [[T0:%.*]] = struct_element_addr [[S]]#1 : $*StructPatternTestAO, #StructPatternTestAO.x
  // CHECK:   [[X:%.*]] = load [[T0]]
  // CHECK:   [[T0:%.*]] = struct_element_addr [[S]]#1 : $*StructPatternTestAO, #StructPatternTestAO.y
  // CHECK:   [[Y:%.*]] = alloc_stack $P
  // CHECK:   copy_addr [[T0]] to [initialization] [[Y]]#1
  
  switch s {
  // CHECK:   cond_br {{%.*}}, [[IS_CASE1:bb[0-9]+]], [[IS_NOT_CASE1:bb[0-9]+]]
  // CHECK: [[IS_CASE1]]:
  // CHECK:   destroy_addr [[Y]]#1
  // CHECK:   dealloc_stack [[Y]]#0
  // CHECK:   destroy_addr [[S]]#1
  // CHECK:   dealloc_stack [[S]]#0
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case StructPatternTestAO(x: 0):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_NOT_CASE1]]:
  // CHECK:   [[TEMP:%.*]] = alloc_stack $P
  // CHECK:   copy_addr [[Y]]#1 to [initialization] [[TEMP]]#1
  // CHECK:   cond_br {{%.*}}, [[IS_CASE2:bb[0-9]+]], [[IS_NOT_CASE2:bb[0-9]+]]

  // CHECK: [[IS_CASE2]]:
  // CHECK:   destroy_addr [[TEMP]]#1
  // CHECK:   dealloc_stack [[TEMP]]#0
  // CHECK:   destroy_addr [[Y]]#1
  // CHECK:   dealloc_stack [[Y]]#0
  // CHECK:   destroy_addr [[S]]#1
  // CHECK:   dealloc_stack [[S]]#0
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case StructPatternTestAO(y: p):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_NOT_CASE2]]:
  // CHECK:   destroy_addr [[TEMP]]#1
  // CHECK:   dealloc_stack [[TEMP]]#0
  // CHECK:   br [[IS_CASE3:bb[0-9]+]]
  // CHECK: [[IS_CASE3]]:
  // CHECK:   destroy_addr [[Y]]#1
  // CHECK:   dealloc_stack [[Y]]#0
  // CHECK:   destroy_addr [[S]]#1
  // CHECK:   dealloc_stack [[S]]#0
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case StructPatternTestAO(x: _):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  // CHECK:   destroy_addr %0 : $*StructPatternTestAO
  d()
}

class ClassPatternTest {
  var x: Int = 0
  var y: String = ""
}

// CHECK-LABEL: sil hidden @_TF6switch18test_class_patternFT1kCS_16ClassPatternTest_T_
// CHECK-NEXT: bb0([[C:%.*]] : $ClassPatternTest):
func test_class_pattern(#k: ClassPatternTest) {
  switch k {
  // CHECK: [[XM:%.*]] = class_method [[C]] : $ClassPatternTest, #ClassPatternTest.x!getter.1
  // CHECK: [[X:%.*]]= apply [[XM:%.*]]([[C]])
  // CHECK: [[YM:%.*]] = class_method [[C]] : $ClassPatternTest, #ClassPatternTest.y!getter.1
  // CHECK: [[Y:%.*]]= apply [[YM:%.*]]([[C]])
  // CHECK:   cond_br {{%.*}}, [[IS_CASE1:bb[0-9]+]], [[IS_NOT_CASE1:bb[0-9]+]]

  // CHECK: [[IS_CASE1]]:
  // CHECK:   release_value [[Y]]
  // CHECK:   strong_release [[C]]
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case ClassPatternTest(x: 0):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_NOT_CASE1]]:
  // CHECK:   cond_br {{%.*}}, [[IS_CASE2:bb[0-9]+]], [[IS_NOT_CASE2:bb[0-9]+]]

  // CHECK: [[IS_CASE2]]:
  // CHECK:   release_value [[Y]]
  // CHECK:   strong_release [[C]]
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case ClassPatternTest(y: ""):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_NOT_CASE2]]:
  // CHECK:   release_value [[Y]]
  // CHECK:   strong_release [[C]]
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case ClassPatternTest(x: _):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  d()
}

class SubclassTestA : ClassPatternTest {}
class SubclassTestB : ClassPatternTest {}

// CHECK-LABEL: sil hidden @{{.*}}test_class_pattern_with_isa_1
// CHECK-NEXT: bb0([[C:%.*]] : $ClassPatternTest):

func test_class_pattern_with_isa_1(#k: ClassPatternTest) {
  switch k {
  // CHECK: [[XM:%.*]] = class_method %0 : $ClassPatternTest, #ClassPatternTest.x!getter.1
  // CHECK: [[X:%.*]] = apply [[XM:%.*]](%0)
  // CHECK:   cond_br {{%.*}}, [[IS_CASE1:bb[0-9]+]], [[IS_NOT_CASE1:bb[0-9]+]]

  // CHECK: [[IS_CASE1]]:
  // CHECK:   strong_release [[C]]
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case ClassPatternTest(x: 0):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_NOT_CASE1]]:
  // CHECK:   checked_cast_br [[C]] : $ClassPatternTest to $SubclassTestA, [[IS_A:bb[0-9]+]], [[IS_NOT_A:bb[0-9]+]]

  // CHECK: [[IS_A]]([[A:%.*]] : $SubclassTestA):
  // CHECK:   strong_release [[A]]
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case is SubclassTestA:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()


  // CHECK: [[IS_NOT_A]]:
  // CHECK:   [[YM:%.*]] = class_method %0 : $ClassPatternTest, #ClassPatternTest.y!getter.1
  // CHECK:   [[Y:%.*]] = apply [[YM:%.*]](%0)
  // CHECK:   cond_br {{%.*}}, [[IS_CASE3:bb[0-9]+]], [[IS_NOT_CASE3:bb[0-9]+]]

  // CHECK: [[IS_CASE3]]:
  // CHECK:   release_value [[Y]]
  // CHECK:   strong_release %0
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case ClassPatternTest(y: ""):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[IS_NOT_CASE3]]:
  // CHECK:   release_value [[Y]]
  // CHECK:   checked_cast_br %0 : $ClassPatternTest to $SubclassTestB, [[IS_B:bb[0-9]+]], [[IS_NOT_B:bb[0-9]+]]

  // CHECK: [[IS_B]]([[B:%.*]] : $SubclassTestB):
  // CHECK:   strong_release [[B]]
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case is SubclassTestB:
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_TF6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()

  // CHECK: [[IS_NOT_B]]:
  // CHECK:   strong_release %0
  // CHECK:   unreachable
  }

  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF6switch1eFT_T_
  e()
}




func test_class_pattern_with_isa_2(#k: ClassPatternTest) {
  switch k {
  case is SubclassTestA:
    a()
  case ClassPatternTest(x: 0):
    b()
  case is SubclassTestB:
    c()
  case ClassPatternTest(y: ""):
    d()
  }
  e()
}

// <rdar://problem/14826416>
func rdar14826416<T, U>(#t: T, #u: U) {
  switch t {
  case is Int: println("Int")
  case is U: println("U")
  case _: println("other")
  }
}
// CHECK-LABEL: sil hidden @_TF6switch12rdar14826416U___FT1tQ_1uQ0__T_
// CHECK:   checked_cast_addr_br take_on_success T in {{%.*}} : $*T to Int in {{%.*}} : $*Int, [[IS_INT:bb[0-9]+]], [[ISNT_INT:bb[0-9]+]]
// CHECK: [[ISNT_INT]]:
// CHECK:   checked_cast_addr_br take_on_success T in {{%.*}} : $*T to U in {{%.*}} : $*U, [[ISNT_INT_IS_U:bb[0-9]+]], [[ISNT_INT_ISNT_U:bb[0-9]+]]

// <rdar://problem/14835992>
class Rdar14835992 {}
class SubRdar14835992 : Rdar14835992 {}

// CHECK-LABEL: sil hidden @_TF6switch12rdar14835992U___FT1tCS_12Rdar148359922ttQ_2uuQ0__T_
func rdar14835992<T, U>(#t: Rdar14835992, #tt: T, #uu: U) {
  switch t {
  case is SubRdar14835992: println("Sub")
  case is T: println("T")
  case is U: println("U")
  case _: println("other")
  }
}


struct StructWithComputedProperty {
  var foo: Int { return 0 }
}

// rdar://15859432
// CHECK-LABEL: sil hidden @{{.*}}StructWithComputedProperty
// CHECK: function_ref{{.*}}StructWithComputedProperty.foo.getter
func testStructWithComputedProperty(#s : StructWithComputedProperty) {
  switch s {
  case StructWithComputedProperty(foo: let a):
    println(a)
  }
}

// <rdar://problem/17272985>
enum ABC { case A, B, C }

// CHECK-LABEL: sil hidden @_TF6switch18testTupleWildcardsFTOS_3ABCS0__T_ 
// CHECK:         [[X:%.*]] = tuple_extract {{%.*}} : $(ABC, ABC), 0
// CHECK:         [[Y:%.*]] = tuple_extract {{%.*}} : $(ABC, ABC), 1
// CHECK:         switch_enum [[X]] : $ABC, case #ABC.A!enumelt: [[X_A:bb[0-9]+]], default [[X_NOT_A:bb[0-9]+]]
// CHECK:       [[X_A]]:
// CHECK:         function_ref @_TF6switch1aFT_T_
// CHECK:       [[X_NOT_A]]:
// CHECK:         switch_enum [[Y]] : $ABC, case #ABC.A!enumelt: [[Y_A:bb[0-9]+]], case #ABC.B!enumelt: [[Y_B:bb[0-9]+]], case #ABC.C!enumelt: [[Y_C:bb[0-9]+]]
// CHECK-NOT: default
// CHECK:       [[Y_A]]:
// CHECK:         function_ref @_TF6switch1bFT_T_
// CHECK:       [[Y_B]]:
// CHECK:         function_ref @_TF6switch1cFT_T_
// CHECK:       [[Y_C]]:
// CHECK:         switch_enum [[X]] : $ABC, case #ABC.C!enumelt: [[X_C:bb[0-9]+]], default [[X_NOT_C:bb[0-9]+]]
// CHECK:       [[X_C]]:
// CHECK:         function_ref @_TF6switch1dFT_T_
// CHECK:       [[X_NOT_C]]:
// CHECK:         function_ref @_TF6switch1eFT_T_
func testTupleWildcards(x: ABC, y: ABC) {
  switch (x, y) {
  case (.A, _):
    a()
  case (_, .A):
    b()
  case (_, .B):
    c()
  case (.C, .C):
    d()
  default:
    e()
  }
}

enum LabeledScalarPayload {
  case Payload(name: Int)
}

// CHECK-LABEL: sil hidden @_TF6switch24testLabeledScalarPayloadFOS_20LabeledScalarPayloadP_
func testLabeledScalarPayload(lsp: LabeledScalarPayload) -> Any {
  // CHECK: switch_enum {{%.*}}, case #LabeledScalarPayload.Payload!enumelt.1: bb1
  switch lsp {
  // CHECK: bb1([[TUPLE:%.*]] : $(name: Int)):
  // CHECK:   [[X:%.*]] = tuple_extract [[TUPLE]]
  // CHECK:   [[ANY_X_ADDR:%.*]] = init_existential_addr {{%.*}}, $Int
  // CHECK:   store [[X]] to [[ANY_X_ADDR]]
  case let .Payload(x):
    return x
  }
}

// CHECK-LABEL: sil hidden @_TF6switch19testOptionalPatternFGSqSi_T_
func testOptionalPattern(value : Int?) {
  // CHECK: switch_enum %0 : $Optional<Int>, case #Optional.Some!enumelt.1: bb1, default [[NILBB:bb[0-9]+]]
  switch value {
  case 1?: a()
  case 2?: b()
  case nil: d()
  default: e()
  }
}


// x? and .None should both be considered "similar" and thus handled in the same
// switch on the enum kind.  There should be no unreachable generated.
// CHECK-LABEL: sil hidden @_TF6switch19testOptionalEnumMixFGSqSi_Si
func testOptionalEnumMix(a : Int?) -> Int {
  // CHECK: debug_value %0 : $Optional<Int>  // let a
  // CHECK-NEXT: switch_enum %0 : $Optional<Int>, case #Optional.Some!enumelt.1: [[SOMEBB:bb[0-9]+]], case #Optional.None!enumelt: [[NILBB:bb[0-9]+]]
  switch a {
  case let x?:
    return 0

  // CHECK: [[SOMEBB]](%3 : $Int):
  // CHECK-NEXT: debug_value %3 : $Int  // let x
  // CHECK: integer_literal $Builtin.Int2048, 0

  case .None:
    return 42

  // CHECK: [[NILBB]]:
  // CHECK: integer_literal $Builtin.Int2048, 42
  }
}





