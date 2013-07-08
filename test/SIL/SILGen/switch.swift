// RUN: %swift -emit-sil %s | FileCheck %s

// TODO: ~= should be built into the compiler.
operator infix ~= {}

// Matching by equality comparison
func [infix] ~= (x:Int, y:Int) -> Bool { return x == y }
func [infix] ~= (x:Double, y:Double) -> Bool { return x == y }
func [infix] ~= (x:String, y:String) -> Bool { return x == y }
func [infix] ~= (x:(Int, Int), y:(Int, Int)) -> Bool {
  return x.0 == y.0 && x.1 == y.1
}

// Matching by range inclusion (e.g. 1..9 ~= 5)
func [infix] ~= (x:IntEnumeratorType, y:Int) -> Bool {
  return x.contains(y)
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
  // CHECK:   condbranch {{%.*}}, [[CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[NOT_CASE1]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]

  case _ where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
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
  // CHECK:   condbranch {{%.*}}, [[CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @_T6switch6fungedFT_Sb
  // CHECK:   condbranch {{%.*}}, [[CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  // CHECK: [[NOT_CASE2]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case _ where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  case (_, _) where funged():
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()
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
  // CHECK:   condbranch {{%.*}}, [[CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[NOT_CASE1]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case (_, _) where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
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
  // CHECK:   function_ref @_T6switchoi2teFT1xTSiSi_1yTSiSi__Sb
  // CHECK:   function_ref @_T6switch6foobarFT_TSiSi_
  // CHECK:   condbranch {{%.*}}, [[CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @_T6switchoi2teFT1xSi1ySi_Sb
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  // CHECK: [[NOT_CASE2]]:
  // CHECK:   function_ref @_T6switchoi2teFT1xSi1ySi_Sb
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE3_GUARD:bb[0-9]+]], [[NOT_CASE3:bb[0-9]+]]
  // CHECK: [[CASE3_GUARD]]:
  // CHECK:   function_ref @_T6switch6runcedFT_Sb
  // CHECK:   condbranch {{%.*}}, [[CASE3:bb[0-9]+]], [[NOT_CASE3]]
  // CHECK: [[NOT_CASE3]]:
  // CHECK:   function_ref @_T6switchoi2teFT1xSi1ySi_Sb
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE4_GUARD_1:bb[0-9]+]], [[NOT_CASE4:bb[0-9]+]]
  // CHECK: [[CASE4_GUARD_1]]:
  // CHECK:   function_ref @_T6switchoi2teFT1xSi1ySi_Sb
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE4_GUARD_2:bb[0-9]+]], [[NOT_CASE4]]
  // CHECK: [[CASE4_GUARD_2]]:
  // CHECK:   function_ref @_T6switch6fungedFT_Sb
  // CHECK:   condbranch {{%.*}}, [[CASE4:bb[0-9]+]], [[NOT_CASE4]]
  // CHECK: [[NOT_CASE4]]:
  // CHECK:   function_ref @_T6switchoi2teFT1xSi1ySi_Sb
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE5_GUARD_1:bb[0-9]+]], [[NOT_CASE5:bb[0-9]+]]
  // CHECK: [[CASE5_GUARD_1]]:
  // CHECK:   function_ref @_T6switchoi2teFT1xSi1ySi_Sb
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE5:bb[0-9]+]], [[NOT_CASE5]]
  // CHECK: [[NOT_CASE5]]:
  // CHECK:   br [[CASE6:bb[0-9]+]]
  case foobar():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  case (foo(), _):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()
  case (_, bar()) where runced():
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()
  case (foo(), bar()) where funged():
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_T6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()
  case (foo(), bar()):
  // CHECK: [[CASE5]]:
  // CHECK:   function_ref @_T6switch1eFT_T_
  // CHECK:   br [[CONT]]
    e()
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
  // CHECK:   function_ref @_T6switchoi2teFT1xSi1ySi_Sb
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @_T6switchoi2teFT1xTSiSi_1yTSiSi__Sb
  // CHECK:   function_ref @_T6switch6foobarFT_TSiSi_
  // CHECK:   condbranch {{%.*}}, [[CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  // CHECK: [[NOT_CASE2]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case (foo(), _):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  case foobar():
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()
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
  // CHECK:   function_ref @_T6switchoi2teFT1xVSs17IntEnumeratorType1ySi_Sb
  // CHECK:   function_ref @_TSsoi2zzFT3minSi3maxSi_VSs17IntEnumeratorType
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[NOT_CASE1]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case (foo()..bar(), _):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
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
  // -- TODO eliminate these empty bbs
  // CHECK: [[IS_X]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  // CHECK: [[IS_NOT_X]]:
  // CHECK:   [[CAST_Y:%.*]] = project_downcast_existential_addr conditional [[P]] : $*P to $*Y
  // CHECK:   [[TEST_Y:%.*]] = is_nonnull [[CAST_Y]] : $*Y
  // CHECK:   condbranch [[TEST_Y]], [[IS_Y:bb[0-9]+]], [[IS_NOT_Y:bb[0-9]+]]
  // CHECK: [[IS_Y]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  // CHECK: [[IS_NOT_Y]]:
  // CHECK:   [[CAST_Z:%.*]] = project_downcast_existential_addr conditional [[P]] : $*P to $*Z
  // CHECK:   [[TEST_Z:%.*]] = is_nonnull [[CAST_Z]] : $*Z
  // CHECK:   condbranch [[TEST_Z]], [[IS_Z:bb[0-9]+]], [[IS_NOT_Z:bb[0-9]+]]
  // CHECK: [[IS_Z]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  // CHECK: [[IS_NOT_Z]]:
  // CHECK:   br [[CASE4:bb[0-9]+]]

  case is X:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  case is Y:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()
  case is Z:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()
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

func test_isa_2(p:P) {
  switch (p, foo()) {
  // CHECK:   [[CAST_X:%.*]] = project_downcast_existential_addr conditional [[P:%.*]] : $*P to $*X
  // CHECK:   [[TEST_X:%.*]] = is_nonnull [[CAST_X]] : $*X
  // CHECK:   condbranch [[TEST_X]], [[IS_X:bb[0-9]+]], [[IS_NOT_X:bb[0-9]+]]
  // CHECK: [[IS_X]]:
  // CHECK:   function_ref @_T6switchoi2teFT1xSi1ySi_Sb
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[NOT_CASE1]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  // CHECK: [[IS_NOT_X]]:
  // CHECK:   [[CAST_Y:%.*]] = project_downcast_existential_addr conditional [[P:%.*]] : $*P to $*Y
  // CHECK:   [[TEST_Y:%.*]] = is_nonnull [[CAST_Y]] : $*Y
  // CHECK:   condbranch [[TEST_Y]], [[IS_Y:bb[0-9]+]], [[IS_NOT_Y:bb[0-9]+]]
  // CHECK: [[IS_Y]]:
  // CHECK:   function_ref @_T6switchoi2teFT1xSi1ySi_Sb
  // CHECK:   function_ref @_T6switch3fooFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  // CHECK: [[NOT_CASE2]]:
  // CHECK:   function_ref @_T6switchoi2teFT1xSi1ySi_Sb
  // CHECK:   function_ref @_T6switch3barFT_Si
  // CHECK:   condbranch {{%.*}}, [[CASE4:bb[0-9]+]], [[NOT_CASE4:bb[0-9]+]]
  // CHECK: [[NOT_CASE4]]:
  // CHECK:   br [[CASE5:bb[0-9]+]]
  // CHECK: [[IS_NOT_Y]]:
  // CHECK:   br [[CASE5:bb[0-9]+]]

  case (is X, foo()):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T6switch1aFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  case (is Y, foo()):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T6switch1bFT_T_
  // CHECK:   br [[CONT]]
    b()
  case (is X, _):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T6switch1cFT_T_
  // CHECK:   br [[CONT]]
    c()
  case (is Y, bar()):
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_T6switch1dFT_T_
  // CHECK:   br [[CONT]]
    d()
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
