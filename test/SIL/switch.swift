// RUN: %swift -emit-sil %s | FileCheck %s

// Some fake predicates for pattern guards.
func runced() -> Bool { return true }
func funged() -> Bool { return true }
func ansed() -> Bool { return true }

func foo() -> Int { return 0 }
func bar() -> Int { return 0 }

func a() {}
func b() {}
func c() {}
func d() {}

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
