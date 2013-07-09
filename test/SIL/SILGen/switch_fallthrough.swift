// RUN: %swift -emit-silgen %s | FileCheck %s

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
func e() {}
func f() {}
func g() {}

// CHECK: sil @_T18switch_fallthrough5test1FT_T_
func test1() {
  switch foo() {
  // CHECK:   condbranch {{%.*}}, [[CASE1:bb[0-9]+]], {{bb[0-9]+}}
  case foo():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T18switch_fallthrough1aFT_T_
  // CHECK:   br [[CASE2:bb[0-9]+]]
    a()
    fallthrough
  case bar():
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T18switch_fallthrough1bFT_T_
  // CHECK:   br [[CASE3:bb[0-9]+]]
    b()
    fallthrough
  case _:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T18switch_fallthrough1cFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    c()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T18switch_fallthrough1dFT_T_
  d()
}

// Fallthrough should work even if the next case is normally unreachable
// CHECK: sil @_T18switch_fallthrough5test2FT_T_
func test2() {
  switch foo() {
  // CHECK:   condbranch {{%.*}}, [[CASE1:bb[0-9]+]], {{bb[0-9]+}}
  case foo():
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T18switch_fallthrough1aFT_T_
  // CHECK:   br [[CASE2:bb[0-9]+]]
    a()
    fallthrough
  case _:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T18switch_fallthrough1bFT_T_
  // CHECK:   br [[CASE3:bb[0-9]+]]
    b()
    fallthrough
  case _:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T18switch_fallthrough1cFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    c()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T18switch_fallthrough1dFT_T_
  d()
}

// CHECK: sil @_T18switch_fallthrough5test3FT_T_
func test3() {
  switch (foo(), bar()) {
  // CHECK:   condbranch {{%.*}}, [[CASE1:bb[0-9]+]], {{bb[0-9]+}}
  case (foo(), bar()):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T18switch_fallthrough1aFT_T_
  // CHECK:   br [[CASE2:bb[0-9]+]]
    a()
    fallthrough
  case (foo(), _):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T18switch_fallthrough1bFT_T_
  // CHECK:   br [[CASE3:bb[0-9]+]]
    b()
    fallthrough
  case (_, _):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T18switch_fallthrough1cFT_T_
  // CHECK:   br [[CASE4:bb[0-9]+]]
    c()
    fallthrough
  case (_, _):
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_T18switch_fallthrough1dFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    d()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T18switch_fallthrough1eFT_T_
  e()
}
