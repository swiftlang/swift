// RUN: %target-swift-emit-silgen -enable-sil-ownership -module-name switch %s | %FileCheck %s

func markUsed<T>(_ t: T) {}

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

// CHECK-LABEL: sil hidden @$s6switch5test1yyF
func test1() {
  switch foo() {
  // CHECK:   function_ref @$s6switch3fooSiyF
  case _:
  // CHECK:   function_ref @$s6switch1ayyF
    a()
  }
  // CHECK:   function_ref @$s6switch1byyF
  b()
}

// CHECK-LABEL: sil hidden @$s6switch5test2yyF
func test2() {
  switch foo() {
  // CHECK:   function_ref @$s6switch3fooSiyF
  case _:
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  case _: // The second case is unreachable.
    b()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @$s6switch1cyyF
  c()
}

// CHECK-LABEL: sil hidden @$s6switch5test3yyF
func test3() {
  switch foo() {
  // CHECK:   function_ref @$s6switch3fooSiyF
  // CHECK:   function_ref @$s6switch6runcedSbyF
  // CHECK:   cond_br {{%.*}}, [[CASE1:bb[0-9]+]], [[CASE2:bb[0-9]+]]

  case _ where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  case _:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @$s6switch1byyF
  // CHECK:   br [[CONT]]
    b()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @$s6switch1cyyF
  c()
}

// CHECK-LABEL: sil hidden @$s6switch5test4yyF
func test4() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @$s6switch3fooSiyF
  // CHECK:   function_ref @$s6switch3barSiyF
  case _:
  // CHECK:   function_ref @$s6switch1ayyF
    a()
  }
  // CHECK:   function_ref @$s6switch1byyF
  b()
}

// CHECK-LABEL: sil hidden @$s6switch5test5yyF
func test5() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @$s6switch3fooSiyF
  // CHECK:   function_ref @$s6switch3barSiyF
  // CHECK:   function_ref @$s6switch6runcedSbyF
  // CHECK:   cond_br {{%.*}}, [[CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  case _ where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @$s6switch6fungedSbyF
  // CHECK:   cond_br {{%.*}}, [[YES_CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  // CHECK: [[YES_CASE2]]:
  case (_, _) where funged():
  // CHECK:   function_ref @$s6switch1byyF
  // CHECK:   br [[CONT]]
    b()

  case _:
  // CHECK: [[NOT_CASE2]]:
  // CHECK:   function_ref @$s6switch1cyyF
    c()
  }
  // CHECK:   function_ref @$s6switch1dyyF
  d()
}

// CHECK-LABEL: sil hidden @$s6switch5test6yyF
func test6() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @$s6switch3fooSiyF
  // CHECK:   function_ref @$s6switch3barSiyF
  case (_, _):
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  case (_, _): // The second case is unreachable.
    b()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @$s6switch1cyyF
  c()
}

// CHECK-LABEL: sil hidden @$s6switch5test7yyF
func test7() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @$s6switch3fooSiyF
  // CHECK:   function_ref @$s6switch3barSiyF
  // CHECK:   function_ref @$s6switch6runcedSbyF
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  case (_, _) where runced():
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  case (_, _):
  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @$s6switch1byyF
    b()
  }
  c()
  // CHECK:   function_ref @$s6switch1cyyF
}

// CHECK-LABEL: sil hidden @$s6switch5test8yyF
func test8() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @$s6switch3fooSiyF
  // CHECK:   function_ref @$s6switch3barSiyF
  // CHECK:   function_ref @$s6switch6foobarSi_SityF
  // CHECK:   cond_br {{%.*}}, [[CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  case foobar():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @$s6switch3fooSiyF
  // CHECK:   cond_br {{%.*}}, [[CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  case (foo(), _):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @$s6switch1byyF
  // CHECK:   br [[CONT]]
    b()
  // CHECK: [[NOT_CASE2]]:
  // CHECK:   function_ref @$s6switch3barSiyF
  // CHECK:   cond_br {{%.*}}, [[CASE3_GUARD:bb[0-9]+]], [[NOT_CASE3:bb[0-9]+]]
  // CHECK: [[CASE3_GUARD]]:
  // CHECK:   function_ref @$s6switch6runcedSbyF
  // CHECK:   cond_br {{%.*}}, [[CASE3:bb[0-9]+]], [[NOT_CASE3_GUARD:bb[0-9]+]]
  case (_, bar()) where runced():
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @$s6switch1cyyF
  // CHECK:   br [[CONT]]
    c()
  // CHECK: [[NOT_CASE3_GUARD]]:
  // CHECK: [[NOT_CASE3]]:
  // CHECK:   function_ref @$s6switch3fooSiyF
  // CHECK:   cond_br {{%.*}}, [[CASE4_GUARD_1:bb[0-9]+]], [[NOT_CASE4_1:bb[0-9]+]]
  // CHECK: [[CASE4_GUARD_1]]:
  // CHECK:   function_ref @$s6switch3barSiyF
  // CHECK:   cond_br {{%.*}}, [[CASE4_GUARD_2:bb[0-9]+]], [[NOT_CASE4_2:bb[0-9]+]]
  // CHECK: [[CASE4_GUARD_2]]:
  // CHECK:   function_ref @$s6switch6fungedSbyF
  // CHECK:   cond_br {{%.*}}, [[CASE4:bb[0-9]+]], [[NOT_CASE4_3:bb[0-9]+]]
  case (foo(), bar()) where funged():
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @$s6switch1dyyF
  // CHECK:   br [[CONT]]
    d()
  // CHECK: [[NOT_CASE4_3]]:
  // CHECK: [[NOT_CASE4_2]]:
  // CHECK: [[NOT_CASE4_1]]:
  // CHECK:   function_ref @$s6switch3fooSiyF
  // CHECK:   cond_br {{%.*}}, [[CASE5_GUARD_1:bb[0-9]+]], [[NOT_CASE5:bb[0-9]+]]
  // CHECK: [[CASE5_GUARD_1]]:
  // CHECK:   function_ref @$s6switch3barSiyF
  // CHECK:   cond_br {{%.*}}, [[YES_CASE5:bb[0-9]+]], [[NOT_CASE5:bb[0-9]+]]
  // CHECK: [[YES_CASE5]]:
  case (foo(), bar()):
  // CHECK:   function_ref @$s6switch1eyyF
  // CHECK:   br [[CONT]]
    e()
  // CHECK: [[NOT_CASE5]]:
  // CHECK:   br [[CASE6:bb[0-9]+]]
  case _:
  // CHECK: [[CASE6]]:
  // CHECK:   function_ref @$s6switch1fyyF
  // CHECK:   br [[CONT]]
    f()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @$s6switch1gyyF
  g()
}

// CHECK-LABEL: sil hidden @$s6switch5test9yyF
func test9() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @$s6switch3fooSiyF
  // CHECK:   function_ref @$s6switch3barSiyF
  // CHECK:   function_ref @$s6switch3fooSiyF
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  case (foo(), _):
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @$s6switch6foobarSi_SityF
  // CHECK:   cond_br {{%.*}}, [[CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  case foobar():
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @$s6switch1byyF
  // CHECK:   br [[CONT]]
    b()

  case _:
  // CHECK: [[NOT_CASE2]]:
  // CHECK:   function_ref @$s6switch1cyyF
    c()
  }
  // CHECK:   function_ref @$s6switch1dyyF
  d()
}

// CHECK-LABEL: sil hidden @$s6switch6test10yyF
func test10() {
  switch (foo(), bar()) {
  // CHECK:   function_ref @$s6switch3fooSiyF
  // CHECK:   function_ref @$s6switch3barSiyF
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  case (foo()...bar(), _):
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  case _:
  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @$s6switch1byyF
  // CHECK:   br [[CONT]]
    b()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @$s6switch1cyyF
  c()
}

protocol P { func p() }

struct X : P { func p() {} }
struct Y : P { func p() {} }
struct Z : P { func p() {} }

// CHECK-LABEL: sil hidden @$s6switch10test_isa_11pyAA1P_p_tF
func test_isa_1(p: P) {
  // CHECK: [[PTMPBUF:%[0-9]+]] = alloc_stack $P
  // CHECK-NEXT: copy_addr %0 to [initialization] [[PTMPBUF]] : $*P
  switch p {
    // CHECK: [[TMPBUF:%[0-9]+]] = alloc_stack $X
  // CHECK:   checked_cast_addr_br copy_on_success P in [[P:%.*]] : $*P to X in [[TMPBUF]] : $*X, [[IS_X:bb[0-9]+]], [[IS_NOT_X:bb[0-9]+]]

  case is X:
  // CHECK: [[IS_X]]:
  // CHECK-NEXT: load [trivial] [[TMPBUF]]
  // CHECK-NEXT: dealloc_stack [[TMPBUF]]
  // CHECK-NEXT: destroy_addr [[PTMPBUF]]
  // CHECK-NEXT: dealloc_stack [[PTMPBUF]]
    a()
    // CHECK:   function_ref @$s6switch1ayyF
    // CHECK:   br [[CONT:bb[0-9]+]]
    
  // CHECK: [[IS_NOT_X]]:
  // CHECK:   checked_cast_addr_br copy_on_success P in [[P]] : $*P to Y in {{%.*}} : $*Y, [[IS_Y:bb[0-9]+]], [[IS_NOT_Y:bb[0-9]+]]

// CHECK: [[IS_Y]]:
  case is Y:
  // CHECK:   function_ref @$s6switch1byyF
  // CHECK:   br [[Y_CONT:bb[0-9]+]]
    b()

  // CHECK: [[IS_NOT_Y]]:
  // CHECK:   checked_cast_addr_br copy_on_success P in [[P]] : $*P to Z in {{%.*}} : $*Z, [[IS_Z:bb[0-9]+]], [[IS_NOT_Z:bb[0-9]+]]

  // CHECK: [[IS_Z]]:
  case is Z:
  // CHECK:   function_ref @$s6switch1cyyF
  // CHECK:   br [[Z_CONT:bb[0-9]+]]
    c()

  // CHECK: [[IS_NOT_Z]]:
  case _:
  // CHECK:   function_ref @$s6switch1dyyF
  // CHECK:   br [[CONT]]
    d()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @$s6switch1eyyF
  e()
}

// CHECK-LABEL: sil hidden @$s6switch10test_isa_21pyAA1P_p_tF
func test_isa_2(p: P) {
  switch (p, foo()) {
  // CHECK:   checked_cast_addr_br copy_on_success P in [[P:%.*]] : $*P to X in {{%.*}} : $*X, [[IS_X:bb[0-9]+]], [[IS_NOT_X:bb[0-9]+]]

  // CHECK: [[IS_X]]:
  // CHECK:   function_ref @$s6switch3fooSiyF
  // CHECK:   cond_br {{%.*}}, [[CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  case (is X, foo()):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_NOT_X]]:
  // CHECK:   checked_cast_addr_br copy_on_success P in [[P]] : $*P to Y in {{%.*}} : $*Y, [[IS_Y:bb[0-9]+]], [[IS_NOT_Y:bb[0-9]+]]

  // CHECK: [[IS_Y]]:
  // CHECK:   function_ref @$s6switch3fooSiyF
  // CHECK:   cond_br {{%.*}}, [[CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  case (is Y, foo()):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @$s6switch1byyF
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[NOT_CASE2]]:
  // CHECK: [[IS_NOT_Y]]:

  // CHECK:   checked_cast_addr_br copy_on_success P in [[P:%.*]] : $*P to X in {{%.*}} : $*X, [[CASE3:bb[0-9]+]], [[IS_NOT_X:bb[0-9]+]]

  case (is X, _):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @$s6switch1cyyF
  // CHECK:   br [[CONT]]
    c()

  // -- case (is Y, foo()):
  // CHECK: [[IS_NOT_X]]:
  // CHECK:   checked_cast_addr_br copy_on_success P in [[P]] : $*P to Y in {{%.*}} : $*Y, [[IS_Y:bb[0-9]+]], [[IS_NOT_Y:bb[0-9]+]]
  // CHECK: [[IS_Y]]:
  // CHECK:   function_ref @$s6switch3barSiyF
  // CHECK:   cond_br {{%.*}}, [[CASE4:bb[0-9]+]], [[NOT_CASE4:bb[0-9]+]]
  case (is Y, bar()):
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @$s6switch1dyyF
  // CHECK:   br [[CONT]]
    d()

  // CHECK: [[NOT_CASE4]]:
  // CHECK:   br [[CASE5:bb[0-9]+]]
  // CHECK: [[IS_NOT_Y]]:
  // CHECK:   br [[CASE5]]
  case _:
  // CHECK: [[CASE5]]:
  // CHECK:   function_ref @$s6switch1eyyF
  // CHECK:   br [[CONT]]
    e()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @$s6switch1fyyF
  f()
}

class B {}
class C : B {}
class D1 : C {}
class D2 : D1 {}
class E : C {}

// CHECK-LABEL: sil hidden @$s6switch16test_isa_class_11xyAA1BC_tF : $@convention(thin) (@guaranteed B) -> () {
func test_isa_class_1(x: B) {
  // CHECK: bb0([[X:%.*]] : @guaranteed $B):
  // CHECK:   checked_cast_br [[X]] : $B to $D1, [[IS_D1:bb[0-9]+]], [[IS_NOT_D1:bb[0-9]+]]
  switch x {

  // CHECK: [[IS_D1]]([[CAST_D1:%.*]] : @guaranteed $D1):
  // CHECK:   [[CAST_D1_COPY:%.*]] = copy_value [[CAST_D1]]
  // CHECK:   function_ref @$s6switch6runcedSbyF : $@convention(thin) () -> Bool
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NO_CASE1:bb[0-9]+]]

  // CHECK: [[YES_CASE1]]:
  case is D1 where runced():
  // CHECK:   destroy_value [[CAST_D1_COPY]]
  // CHECK:   end_borrow [[CAST_D1]]
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[NO_CASE1]]:
  // CHECK-NEXT:   destroy_value [[CAST_D1_COPY]]
  // CHECK-NEXT:   end_borrow [[CAST_D1]]
  // CHECK:   br [[NEXT_CASE:bb[0-9]+]]

  // CHECK: [[IS_NOT_D1]]([[CASTFAIL_D1:%.*]] : @guaranteed $B):
  // CHECK-NEXT:   end_borrow [[CASTFAIL_D1]]
  // CHECK-NEXT:   br [[NEXT_CASE]]

  // CHECK: [[NEXT_CASE]]:
  // CHECK:   checked_cast_br [[X]] : $B to $D2, [[IS_D2:bb[0-9]+]], [[IS_NOT_D2:bb[0-9]+]]
  case is D2:
  // CHECK: [[IS_D2]]([[CAST_D2:%.*]] : @guaranteed $D2):
  // CHECK:   [[CAST_D2_COPY:%.*]] = copy_value [[CAST_D2]]
  // CHECK:   destroy_value [[CAST_D2_COPY]]
  // CHECK:   function_ref @$s6switch1byyF
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_NOT_D2]]([[CASTFAIL_D2:%.*]] : @guaranteed $B):
  // CHECK:   end_borrow [[CASTFAIL_D2]]
  // CHECK:   checked_cast_br [[X]] : $B to $E, [[IS_E:bb[0-9]+]], [[IS_NOT_E:bb[0-9]+]]
  case is E where funged():
  // CHECK: [[IS_E]]([[CAST_E:%.*]] : @guaranteed $E):
  // CHECK:   [[CAST_E_COPY:%.*]] = copy_value [[CAST_E]]
  // CHECK:   function_ref @$s6switch6fungedSbyF
  // CHECK:   cond_br {{%.*}}, [[CASE3:bb[0-9]+]], [[NO_CASE3:bb[0-9]+]]

  // CHECK: [[CASE3]]:
  // CHECK:   destroy_value [[CAST_E_COPY]]
  // CHECK:   function_ref @$s6switch1cyyF
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[NO_CASE3]]:
  // CHECK-NEXT:   destroy_value [[CAST_E_COPY]]
  // CHECK-NEXT:   end_borrow
  // CHECK:   br [[NEXT_CASE:bb[0-9]+]]

  // CHECK: [[IS_NOT_E]]([[NOTCAST_E:%.*]] : @guaranteed $B):
  // CHECK:   end_borrow [[NOTCAST_E]]
  // CHECK:   br [[NEXT_CASE]]

  // CHECK: [[NEXT_CASE]]:
  // CHECK:   checked_cast_br [[X]] : $B to $C, [[IS_C:bb[0-9]+]], [[IS_NOT_C:bb[0-9]+]]

  case is C:
  // CHECK: [[IS_C]]([[CAST_C:%.*]] : @guaranteed $C):
  // CHECK:   [[CAST_C_COPY:%.*]] = copy_value [[CAST_C]]
  // CHECK:   destroy_value [[CAST_C_COPY]]
  // CHECK:   end_borrow [[CAST_C]]
  // CHECK:   function_ref @$s6switch1dyyF
  // CHECK:   br [[CONT]]
    d()

  // CHECK: [[IS_NOT_C]]([[NOCAST_C:%.*]] : @guaranteed $B):
  // CHECK:   end_borrow [[NOCAST_C]]
  default:
  // CHECK:    function_ref @$s6switch1eyyF
  // CHECK:    br [[CONT]]
    e()
  }
  // CHECK: [[CONT]]:
  // CHECK:   [[F_FUNC:%.*]] = function_ref @$s6switch1fyyF : $@convention(thin) () -> ()
  // CHECK:   apply [[F_FUNC]]()
  f()
}
// CHECK: } // end sil function '$s6switch16test_isa_class_11xyAA1BC_tF'

// CHECK-LABEL: sil hidden @$s6switch16test_isa_class_21xyXlAA1BC_tF : $@convention(thin)
func test_isa_class_2(x: B) -> AnyObject {
  // CHECK: bb0([[X:%.*]] : @guaranteed $B):
  switch x {

  // CHECK:   checked_cast_br [[X]] : $B to $D1, [[IS_D1:bb[0-9]+]], [[IS_NOT_D1:bb[0-9]+]]
  case let y as D1 where runced():
  // CHECK: [[IS_D1]]([[CAST_D1:%.*]] : @guaranteed $D1):
  // CHECK:   [[CAST_D1_COPY:%.*]] = copy_value [[CAST_D1]]
  // CHECK:   function_ref @$s6switch6runcedSbyF
  // CHECK:   cond_br {{%.*}}, [[CASE1:bb[0-9]+]], [[NO_CASE1:bb[0-9]+]]

  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   [[BORROWED_CAST_D1_COPY:%.*]] = begin_borrow [[CAST_D1_COPY]]
  // CHECK:   [[CAST_D1_COPY_COPY:%.*]] = copy_value [[BORROWED_CAST_D1_COPY]]
  // CHECK:   [[RET:%.*]] = init_existential_ref [[CAST_D1_COPY_COPY]]
  // CHECK:   end_borrow [[BORROWED_CAST_D1_COPY]]
  // CHECK:   destroy_value [[CAST_D1_COPY]]
  // CHECK:   br [[CONT:bb[0-9]+]]([[RET]] : $AnyObject)
    a()
    return y

  // CHECK: [[NO_CASE1]]:
  // CHECK:   destroy_value [[CAST_D1_COPY]]
  // CHECK:   br [[NEXT_CASE:bb5]]
  
  // CHECK: [[IS_NOT_D1]]([[NOCAST_D1:%.*]] : @guaranteed $B):
  // CHECK:   end_borrow [[NOCAST_D1]]
  // CHECK:   br [[NEXT_CASE]]

  // CHECK: [[NEXT_CASE]]:
  // CHECK:   checked_cast_br [[X]] : $B to $D2, [[CASE2:bb[0-9]+]], [[IS_NOT_D2:bb[0-9]+]]
  case let y as D2:
  // CHECK: [[CASE2]]([[CAST_D2:%.*]] : @guaranteed $D2):
  // CHECK:   [[CAST_D2_COPY:%.*]] = copy_value [[CAST_D2]]
  // CHECK:   function_ref @$s6switch1byyF
  // CHECK:   [[BORROWED_CAST_D2_COPY:%.*]] = begin_borrow [[CAST_D2_COPY]]
  // CHECK:   [[CAST_D2_COPY_COPY:%.*]] = copy_value [[BORROWED_CAST_D2_COPY]]
  // CHECK:   [[RET:%.*]] = init_existential_ref [[CAST_D2_COPY_COPY]]
  // CHECK:   end_borrow [[BORROWED_CAST_D2_COPY]]
  // CHECK:   destroy_value [[CAST_D2_COPY]]
  // CHECK:   br [[CONT]]([[RET]] : $AnyObject)
    b()
    return y

  // CHECK: [[IS_NOT_D2]]([[NOCAST_D2:%.*]] : @guaranteed $B):
  // CHECK:   end_borrow [[NOCAST_D2]]
  // CHECK:   checked_cast_br [[X]] : $B to $E, [[IS_E:bb[0-9]+]], [[IS_NOT_E:bb[0-9]+]]
  case let y as E where funged():
  // CHECK: [[IS_E]]([[CAST_E:%.*]] : @guaranteed $E):
  // CHECK:   [[CAST_E_COPY:%.*]] = copy_value [[CAST_E]]
  // CHECK:   function_ref @$s6switch6fungedSbyF
  // CHECK:   cond_br {{%.*}}, [[CASE3:bb[0-9]+]], [[NO_CASE3:bb[0-9]+]]

  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @$s6switch1cyyF
  // CHECK:   [[BORROWED_CAST_E_COPY:%.*]] = begin_borrow [[CAST_E_COPY]]
  // CHECK:   [[CAST_E_COPY_COPY:%.*]] = copy_value [[BORROWED_CAST_E_COPY]]
  // CHECK:   [[RET:%.*]] = init_existential_ref [[CAST_E_COPY_COPY]]
  // CHECK:   end_borrow [[BORROWED_CAST_E_COPY]]
  // CHECK:   destroy_value [[CAST_E_COPY]]
  // CHECK:   end_borrow [[CAST_E]]
  // CHECK:   br [[CONT]]([[RET]] : $AnyObject)
    c()
    return y

  // CHECK: [[NO_CASE3]]:
  // CHECK    destroy_value [[CAST_E_COPY]]
  // CHECK:   br [[NEXT_CASE:bb[0-9]+]]

  // CHECK: [[IS_NOT_E]]([[NOCAST_E:%.*]] : @guaranteed $B):
  // CHECK:   end_borrow [[NOCAST_E]]
  // CHECK:   br [[NEXT_CASE]]

  // CHECK: [[NEXT_CASE]]
  // CHECK:   checked_cast_br [[X]] : $B to $C, [[CASE4:bb[0-9]+]], [[IS_NOT_C:bb[0-9]+]]
  case let y as C:
  // CHECK: [[CASE4]]([[CAST_C:%.*]] : @guaranteed $C):
  // CHECK:   [[CAST_C_COPY:%.*]] = copy_value [[CAST_C]]
  // CHECK:   function_ref @$s6switch1dyyF
  // CHECK:   [[BORROWED_CAST_C_COPY:%.*]] = begin_borrow [[CAST_C_COPY]]
  // CHECK:   [[CAST_C_COPY_COPY:%.*]] = copy_value [[BORROWED_CAST_C_COPY]]
  // CHECK:   [[RET:%.*]] = init_existential_ref [[CAST_C_COPY_COPY]]
  // CHECK:   end_borrow [[BORROWED_CAST_C_COPY]]
  // CHECK:   destroy_value [[CAST_C_COPY]]
  // CHECK:   br [[CONT]]([[RET]] : $AnyObject)
    d()
    return y

  // CHECK: [[IS_NOT_C]]([[NOCAST_C:%.*]] : @guaranteed $B):
  // CHECK:   end_borrow [[NOCAST_C]]
  default:
  // CHECK:   function_ref @$s6switch1eyyF
  // CHECK:   [[X_COPY_2:%.*]] = copy_value [[X]]
  // CHECK:   [[RET:%.*]] = init_existential_ref [[X_COPY_2]]
  // CHECK:   br [[CONT]]([[RET]] : $AnyObject)
    e()
    return x
  }

  // CHECK: [[CONT]]([[T0:%.*]] : @owned $AnyObject):
  // CHECK:   return [[T0]]
}
// CHECK: } // end sil function '$s6switch16test_isa_class_21xyXlAA1BC_tF'

enum MaybePair {
  case Neither
  case Left(Int)
  case Right(String)
  case Both(Int, String)
}

// CHECK-LABEL: sil hidden @$s6switch12test_union_11uyAA9MaybePairO_tF
func test_union_1(u: MaybePair) {
  switch u {
  // CHECK: switch_enum [[SUBJECT:%.*]] : $MaybePair,
  // CHECK:   case #MaybePair.Neither!enumelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybePair.Left!enumelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Right!enumelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Both!enumelt.1: [[IS_BOTH:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  // CHECK-NOT: destroy_value
  case .Neither:
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]({{%.*}}):
  // CHECK-NOT: destroy_value
  case (.Left):
  // CHECK:   function_ref @$s6switch1byyF
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]([[STR:%.*]] : @guaranteed $String):
  case var .Right:
  // CHECK:   function_ref @$s6switch1cyyF
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[IS_BOTH]]([[TUP:%.*]] : @guaranteed $(Int, String)):
  case .Both:
  // CHECK:   ({{%.*}}, [[TUP_STR:%.*]]) = destructure_tuple [[TUP]]
  // CHECK:   function_ref @$s6switch1dyyF
  // CHECK:   br [[CONT]]
    d()
  }

  // CHECK: [[CONT]]:
  // CHECK-NOT: switch_enum [[SUBJECT]]
  // CHECK:   function_ref @$s6switch1eyyF
  e()
}

// CHECK-LABEL: sil hidden @$s6switch12test_union_31uyAA9MaybePairO_tF : $@convention(thin) (@guaranteed MaybePair) -> () {
func test_union_3(u: MaybePair) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $MaybePair):
  // CHECK:   switch_enum [[ARG]] : $MaybePair,
  // CHECK:     case #MaybePair.Neither!enumelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:     case #MaybePair.Left!enumelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:     case #MaybePair.Right!enumelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:     default [[DEFAULT:bb[0-9]+]]
  switch u {
  // CHECK: [[IS_NEITHER]]:
  case .Neither:
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]({{%.*}}):
  case .Left:
  // CHECK:   function_ref @$s6switch1byyF
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]([[STR:%.*]] : @guaranteed $String):
  case .Right:
  // CHECK:   function_ref @$s6switch1cyyF
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[DEFAULT]](
  // -- Ensure the fully-opaque value is destroyed in the default case.
  // CHECK:   function_ref @$s6switch1dyyF
  // CHECK:   br [[CONT]]

  default:
    d()
  }

  // CHECK: [[CONT]]:
  // CHECK-NOT: switch_enum [[ARG]]
  // CHECK:   function_ref @$s6switch1eyyF
  e()
}

// CHECK-LABEL: sil hidden @$s6switch12test_union_41uyAA9MaybePairO_tF
func test_union_4(u: MaybePair) {
  switch u {
  // CHECK: switch_enum {{%.*}} : $MaybePair,
  // CHECK:   case #MaybePair.Neither!enumelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybePair.Left!enumelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Right!enumelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Both!enumelt.1: [[IS_BOTH:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  case .Neither:
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]({{%.*}}):
  case .Left(_):
  // CHECK:   function_ref @$s6switch1byyF
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]({{%.*}}):
  case .Right(_):
  // CHECK:   function_ref @$s6switch1cyyF
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[IS_BOTH]]({{%.*}}):
  case .Both(_):
  // CHECK:   function_ref @$s6switch1dyyF
  // CHECK:   br [[CONT]]
    d()
  }

  // CHECK: [[CONT]]:
  // CHECK:   function_ref @$s6switch1eyyF
  e()
}

// CHECK-LABEL: sil hidden @$s6switch12test_union_51uyAA9MaybePairO_tF
func test_union_5(u: MaybePair) {
  switch u {
  // CHECK: switch_enum {{%.*}} : $MaybePair,
  // CHECK:   case #MaybePair.Neither!enumelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybePair.Left!enumelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Right!enumelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   case #MaybePair.Both!enumelt.1: [[IS_BOTH:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  case .Neither:
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]({{%.*}}):
  case .Left(_):
  // CHECK:   function_ref @$s6switch1byyF
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]({{%.*}}):
  case .Right(_):
  // CHECK:   function_ref @$s6switch1cyyF
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[IS_BOTH]]({{%.*}}):
  case .Both(_, _):
  // CHECK:   function_ref @$s6switch1dyyF
  // CHECK:   br [[CONT]]
    d()
  }

  // CHECK: [[CONT]]:
  // CHECK:   function_ref @$s6switch1eyyF
  e()
}

enum MaybeAddressOnlyPair {
  case Neither
  case Left(P)
  case Right(String)
  case Both(P, String)
}

// CHECK-LABEL: sil hidden @$s6switch22test_union_addr_only_11uyAA20MaybeAddressOnlyPairO_tF
func test_union_addr_only_1(u: MaybeAddressOnlyPair) {
  switch u {
  // CHECK: switch_enum_addr [[ENUM_ADDR:%.*]] : $*MaybeAddressOnlyPair,
  // CHECK:   case #MaybeAddressOnlyPair.Neither!enumelt: [[IS_NEITHER:bb[0-9]+]],
  // CHECK:   case #MaybeAddressOnlyPair.Left!enumelt.1: [[IS_LEFT:bb[0-9]+]],
  // CHECK:   case #MaybeAddressOnlyPair.Right!enumelt.1: [[IS_RIGHT:bb[0-9]+]],
  // CHECK:   case #MaybeAddressOnlyPair.Both!enumelt.1: [[IS_BOTH:bb[0-9]+]]

  // CHECK: [[IS_NEITHER]]:
  case .Neither:
  // CHECK:   function_ref @$s6switch1ayyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()

  // CHECK: [[IS_LEFT]]:
  // CHECK:   [[P:%.*]] = unchecked_take_enum_data_addr [[ENUM_ADDR]] : $*MaybeAddressOnlyPair, #MaybeAddressOnlyPair.Left!enumelt.1
  case .Left(_):
  // CHECK:   destroy_addr [[P]]
  // CHECK:   function_ref @$s6switch1byyF
  // CHECK:   br [[CONT]]
    b()

  // CHECK: [[IS_RIGHT]]:
  // CHECK:   [[STR_ADDR:%.*]] = unchecked_take_enum_data_addr [[ENUM_ADDR]] : $*MaybeAddressOnlyPair, #MaybeAddressOnlyPair.Right!enumelt.1
  // CHECK:   [[STR:%.*]] = load [take] [[STR_ADDR]]
  case .Right(_):
  // CHECK:   destroy_value [[STR]] : $String
  // CHECK:   function_ref @$s6switch1cyyF
  // CHECK:   br [[CONT]]
    c()

  // CHECK: [[IS_BOTH]]:
  // CHECK:   [[P_STR_TUPLE:%.*]] = unchecked_take_enum_data_addr [[ENUM_ADDR]] : $*MaybeAddressOnlyPair, #MaybeAddressOnlyPair.Both!enumelt.1
  case .Both(_):
  // CHECK:   destroy_addr [[P_STR_TUPLE]]
  // CHECK:   function_ref @$s6switch1dyyF
  // CHECK:   br [[CONT]]
    d()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @$s6switch1eyyF
  e()
}

enum Generic<T, U> {
  case Foo(T)
  case Bar(U)
}

// Check that switching over a generic instance generates verified SIL.
func test_union_generic_instance(u: Generic<Int, String>) {
  switch u {
  case .Foo(var x):
    a()
  case .Bar(var y):
    b()
  }
  c()
}

enum Foo { case A, B }

// CHECK-LABEL: sil hidden @$s6switch05test_A11_two_unions1x1yyAA3FooO_AFtF
func test_switch_two_unions(x: Foo, y: Foo) {
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


// <rdar://problem/14826416>
func rdar14826416<T, U>(t: T, u: U) {
  switch t {
  case is Int: markUsed("Int")
  case is U: markUsed("U")
  case _: markUsed("other")
  }
}
// CHECK-LABEL: sil hidden @$s6switch12rdar14826416{{[_0-9a-zA-Z]*}}F
// CHECK:   checked_cast_addr_br copy_on_success T in {{%.*}} : $*T to Int in {{%.*}} : $*Int, [[IS_INT:bb[0-9]+]], [[ISNT_INT:bb[0-9]+]]
// CHECK: [[ISNT_INT]]:
// CHECK:   checked_cast_addr_br copy_on_success T in {{%.*}} : $*T to U in {{%.*}} : $*U, [[ISNT_INT_IS_U:bb[0-9]+]], [[ISNT_INT_ISNT_U:bb[0-9]+]]

// <rdar://problem/14835992>
class Rdar14835992 {}
class SubRdar14835992 : Rdar14835992 {}

// CHECK-LABEL: sil hidden @$s6switch12rdar14835992{{[_0-9a-zA-Z]*}}F
func rdar14835992<T, U>(t: Rdar14835992, tt: T, uu: U) {
  switch t {
  case is SubRdar14835992: markUsed("Sub")
  case is T: markUsed("T")
  case is U: markUsed("U")
  case _: markUsed("other")
  }
}




// <rdar://problem/17272985>
enum ABC { case A, B, C }

// CHECK-LABEL: sil hidden @$s6switch18testTupleWildcardsyyAA3ABCO_ADtF
// CHECK:         ([[X:%.*]], [[Y:%.*]]) = destructure_tuple {{%.*}} : $(ABC, ABC)
// CHECK:         switch_enum [[X]] : $ABC, case #ABC.A!enumelt: [[X_A:bb[0-9]+]], default [[X_NOT_A:bb[0-9]+]]
// CHECK:       [[X_A]]:
// CHECK:         function_ref @$s6switch1ayyF
// CHECK:       [[X_NOT_A]](
// CHECK:         switch_enum [[Y]] : $ABC, case #ABC.A!enumelt: [[Y_A:bb[0-9]+]], case #ABC.B!enumelt: [[Y_B:bb[0-9]+]], case #ABC.C!enumelt: [[Y_C:bb[0-9]+]]
// CHECK-NOT: default
// CHECK:       [[Y_A]]:
// CHECK:         function_ref @$s6switch1byyF
// CHECK:       [[Y_B]]:
// CHECK:         function_ref @$s6switch1cyyF
// CHECK:       [[Y_C]]:
// CHECK:         switch_enum [[X]] : $ABC, case #ABC.C!enumelt: [[X_C:bb[0-9]+]], default [[X_NOT_C:bb[0-9]+]]
// CHECK:       [[X_C]]:
// CHECK:         function_ref @$s6switch1dyyF
// CHECK:       [[X_NOT_C]](
// CHECK:         function_ref @$s6switch1eyyF
func testTupleWildcards(_ x: ABC, _ y: ABC) {
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

// CHECK-LABEL: sil hidden @$s6switch24testLabeledScalarPayloadyypAA0cdE0OF
func testLabeledScalarPayload(_ lsp: LabeledScalarPayload) -> Any {
  // CHECK: switch_enum {{%.*}}, case #LabeledScalarPayload.Payload!enumelt.1: bb1
  switch lsp {
  // CHECK: bb1([[TUPLE:%.*]] : @trivial $(name: Int)):
  // CHECK:   [[X:%.*]] = destructure_tuple [[TUPLE]]
  // CHECK:   [[ANY_X_ADDR:%.*]] = init_existential_addr {{%.*}}, $Int
  // CHECK:   store [[X]] to [trivial] [[ANY_X_ADDR]]
  case let .Payload(x):
    return x
  }
}

// There should be no unreachable generated.
// CHECK-LABEL: sil hidden @$s6switch19testOptionalPatternyySiSgF
func testOptionalPattern(_ value : Int?) {
  // CHECK: switch_enum %0 : $Optional<Int>, case #Optional.some!enumelt.1: bb1, case #Optional.none!enumelt: [[NILBB:bb[0-9]+]]
  switch value {
  case 1?: a()
  case 2?: b()
  case nil: d()
  default: e()
  }
}


// x? and .none should both be considered "similar" and thus handled in the same
// switch on the enum kind.  There should be no unreachable generated.
// CHECK-LABEL: sil hidden @$s6switch19testOptionalEnumMixyS2iSgF
func testOptionalEnumMix(_ a : Int?) -> Int {
  // CHECK: debug_value %0 : $Optional<Int>, let, name "a"
  // CHECK-NEXT: switch_enum %0 : $Optional<Int>, case #Optional.some!enumelt.1: [[SOMEBB:bb[0-9]+]], case #Optional.none!enumelt: [[NILBB:bb[0-9]+]]
  switch a {
  case let x?:
    return 0

  // CHECK: [[SOMEBB]](%3 : @trivial $Int):
  // CHECK-NEXT: debug_value %3 : $Int, let, name "x"
  // CHECK: integer_literal $Builtin.Int2048, 0

  case .none:
    return 42

  // CHECK: [[NILBB]]:
  // CHECK: integer_literal $Builtin.Int2048, 42
  }
}

// x? and nil should both be considered "similar" and thus handled in the same
// switch on the enum kind.  There should be no unreachable generated.
// CHECK-LABEL: sil hidden @$s6switch26testOptionalEnumMixWithNilyS2iSgF
func testOptionalEnumMixWithNil(_ a : Int?) -> Int {
  // CHECK: debug_value %0 : $Optional<Int>, let, name "a"
  // CHECK-NEXT: switch_enum %0 : $Optional<Int>, case #Optional.some!enumelt.1: [[SOMEBB:bb[0-9]+]], case #Optional.none!enumelt: [[NILBB:bb[0-9]+]]
  switch a {
  case let x?:
    return 0

  // CHECK: [[SOMEBB]](%3 : @trivial $Int):
  // CHECK-NEXT: debug_value %3 : $Int, let, name "x"
  // CHECK: integer_literal $Builtin.Int2048, 0

  case nil:
    return 42

  // CHECK: [[NILBB]]:
  // CHECK: integer_literal $Builtin.Int2048, 42
  }
}

// SR-3518
// CHECK-LABEL: sil hidden @$s6switch43testMultiPatternsWithOuterScopeSameNamedVar4base6filterySiSg_AEtF
func testMultiPatternsWithOuterScopeSameNamedVar(base: Int?, filter: Int?) {
  switch(base, filter) {
    
  case (.some(let base), .some(let filter)):
    // CHECK: bb2(%10 : @trivial $Int):
    // CHECK-NEXT: debug_value %8 : $Int, let, name "base"
    // CHECK-NEXT: debug_value %10 : $Int, let, name "filter"
    print("both: \(base), \(filter)")
  case (.some(let base), .none), (.none, .some(let base)):
    // CHECK: bb3:
    // CHECK-NEXT: debug_value %8 : $Int, let, name "base"
    // CHECK-NEXT: br bb6(%8 : $Int)

    // CHECK: bb5([[OTHER_BASE:%.*]] : @trivial $Int)
    // CHECK-NEXT: debug_value [[OTHER_BASE]] : $Int, let, name "base"
    // CHECK-NEXT: br bb6([[OTHER_BASE]] : $Int)
    
    // CHECK: bb6([[ARG:%.*]] : @trivial $Int):
    print("single: \(base)")
  default:
    print("default")
  }
}

// All cases are unreachable, either structurally (tuples involving Never) or
// nominally (empty enums).  We fold all of these to 'unreachable'.
enum MyNever {}
func ~= (_ : MyNever, _ : MyNever) -> Bool { return true }
func myFatalError() -> MyNever { fatalError("asdf") }

func testUninhabitedSwitchScrutinee() {
  func test1(x : MyNever) {
    // CHECK: bb0(%0 : @trivial $MyNever):
    // CHECK-NEXT: debug_value %0 : $MyNever, let, name "x"
    // CHECK-NEXT: unreachable
    switch x {
    case myFatalError(): break
    case myFatalError(): break
    case myFatalError(): break
    }
  }
  func test2(x : Never) {
    // CHECK: bb0(%0 : @trivial $Never):
    // CHECK-NEXT: debug_value %0 : $Never, let, name "x"
    // CHECK-NEXT: unreachable
    switch (x, x) {}
  }
  func test3(x : Never) {
    // CHECK: unreachable
    // CHECK-NEXT: } // end sil function '$s6switch30testUninhabitedSwitchScrutineeyyF5test3L_1xys5NeverO_tF'
    switch (x, 5, x) {}
  }
  func test4(x : Never) {
    // CHECK: unreachable
    // CHECK-NEXT: } // end sil function '$s6switch30testUninhabitedSwitchScrutineeyyF5test4L_1xys5NeverO_tF'
    switch ((8, 6, 7), (5, 3, (0, x))) {}
  }
  func test5() {
    // CHECK: %0 = function_ref @$s6switch12myFatalErrorAA7MyNeverOyF : $@convention(thin) () -> MyNever
    // CHECK-NEXT: %1 = apply %0() : $@convention(thin) () -> MyNever
    // CHECK-NEXT: unreachable
    switch myFatalError() {}
  }
}

// Make sure that we properly can handle address only tuples with loadable
// subtypes.
class Klass {}

enum TrivialSingleCaseEnum {
case a
}

enum NonTrivialSingleCaseEnum {
case a(Klass)
}

// CHECK-LABEL: sil hidden @$s6switch33address_only_with_trivial_subtypeyyAA21TrivialSingleCaseEnumO_yptF : $@convention(thin) (TrivialSingleCaseEnum, @in_guaranteed Any) -> () {
// CHECK: [[MEM:%.*]] = alloc_stack $(TrivialSingleCaseEnum, Any)
// CHECK: [[INIT_TUP_0:%.*]] = tuple_element_addr [[MEM]] : $*(TrivialSingleCaseEnum, Any), 0
// CHECK: [[INIT_TUP_1:%.*]] = tuple_element_addr [[MEM]] : $*(TrivialSingleCaseEnum, Any), 1
// CHECK: store {{%.*}} to [trivial] [[INIT_TUP_0]]
// CHECK: copy_addr [take] {{%.*}} to [initialization] [[INIT_TUP_1]]
// CHECK: [[TUP_0:%.*]] = tuple_element_addr [[MEM]] : $*(TrivialSingleCaseEnum, Any), 0
// CHECK: [[TUP_0_VAL:%.*]] = load [trivial] [[TUP_0]]
// CHECK: [[TUP_1:%.*]] = tuple_element_addr [[MEM]] : $*(TrivialSingleCaseEnum, Any), 1
// CHECK: switch_enum [[TUP_0_VAL]]
//
// CHECK: } // end sil function '$s6switch33address_only_with_trivial_subtypeyyAA21TrivialSingleCaseEnumO_yptF'
func address_only_with_trivial_subtype(_ a: TrivialSingleCaseEnum, _ value: Any) {
  switch (a, value) {
  case (.a, _):
    break
  default:
    break
  }
}

// CHECK-LABEL: sil hidden @$s6switch36address_only_with_nontrivial_subtypeyyAA24NonTrivialSingleCaseEnumO_yptF : $@convention(thin) (@guaranteed NonTrivialSingleCaseEnum, @in_guaranteed Any) -> () {
// CHECK: [[MEM:%.*]] = alloc_stack $(NonTrivialSingleCaseEnum, Any)
// CHECK: [[INIT_TUP_0:%.*]] = tuple_element_addr [[MEM]] : $*(NonTrivialSingleCaseEnum, Any), 0
// CHECK: [[INIT_TUP_1:%.*]] = tuple_element_addr [[MEM]] : $*(NonTrivialSingleCaseEnum, Any), 1
// CHECK: store {{%.*}} to [init] [[INIT_TUP_0]]
// CHECK: copy_addr [take] {{%.*}} to [initialization] [[INIT_TUP_1]]
// CHECK: [[TUP_0:%.*]] = tuple_element_addr [[MEM]] : $*(NonTrivialSingleCaseEnum, Any), 0
// CHECK: [[TUP_0_VAL:%.*]] = load_borrow [[TUP_0]]
// CHECK: [[TUP_1:%.*]] = tuple_element_addr [[MEM]] : $*(NonTrivialSingleCaseEnum, Any), 1
// CHECK: switch_enum [[TUP_0_VAL]]
// CHECK: } // end sil function '$s6switch36address_only_with_nontrivial_subtypeyyAA24NonTrivialSingleCaseEnumO_yptF'
func address_only_with_nontrivial_subtype(_ a: NonTrivialSingleCaseEnum, _ value: Any) {
  switch (a, value) {
  case (.a, _):
    break
  default:
    break
  }
}
