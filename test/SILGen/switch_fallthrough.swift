// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden  @_T018switch_fallthrough5test1yyF
func test1() {
  switch foo() {
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], {{bb[0-9]+}}
  // CHECK: [[YES_CASE1]]:
  case foo():
  // CHECK:   function_ref @_T018switch_fallthrough1ayyF
  // CHECK:   br [[CASE2:bb[0-9]+]]
    a()
    fallthrough
  case bar():
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T018switch_fallthrough1byyF
  // CHECK:   br [[CASE3:bb[0-9]+]]
    b()
    fallthrough
  case _:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T018switch_fallthrough1cyyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    c()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T018switch_fallthrough1dyyF
  d()
}

// Fallthrough should work even if the next case is normally unreachable
// CHECK-LABEL: sil hidden  @_T018switch_fallthrough5test2yyF
func test2() {
  switch foo() {
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], {{bb[0-9]+}}
  // CHECK: [[YES_CASE1]]:
  case foo():
  // CHECK:   function_ref @_T018switch_fallthrough1ayyF
  // CHECK:   br [[CASE2:bb[0-9]+]]
    a()
    fallthrough
  case _:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T018switch_fallthrough1byyF
  // CHECK:   br [[CASE3:bb[0-9]+]]
    b()
    fallthrough
  case _:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T018switch_fallthrough1cyyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    c()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T018switch_fallthrough1dyyF
  d()
}

// CHECK-LABEL: sil hidden  @_T018switch_fallthrough5test3yyF
func test3() {
  switch (foo(), bar()) {
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], {{bb[0-9]+}}
  // CHECK: [[YES_CASE1]]:
  case (foo(), bar()):
  // CHECK:   function_ref @_T018switch_fallthrough1ayyF
  // CHECK:   br [[CASE2:bb[0-9]+]]
    a()
    fallthrough
  case (foo(), _):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T018switch_fallthrough1byyF
  // CHECK:   br [[CASE3:bb[0-9]+]]
    b()
    fallthrough
  case (_, _):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T018switch_fallthrough1cyyF
  // CHECK:   br [[CASE4:bb[0-9]+]]
    c()
    fallthrough
  case (_, _):
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_T018switch_fallthrough1dyyF
  // CHECK:   br [[CONT:bb[0-9]+]]
    d()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_T018switch_fallthrough1eyyF
  e()
}

// Fallthrough should clean up nested pattern variables from the exited scope.
func test4() {
  switch (foo(), bar()) {
  // CHECK:   [[A:%.*]] = alloc_box ${ var (Int, Int) }
  // CHECK:   cond_br {{%.*}}, [[CASE1:bb[0-9]+]], {{bb[0-9]+}}
  case var a where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
    fallthrough
  case _ where funged():
  // CHECK: [[CASE2]]:
  // CHECK:   br [[CONT:bb[0-9]+]]
    ()

  // CHECK:   [[B:%.*]] = alloc_box ${ var Int }
  // CHECK:   [[C:%.*]] = alloc_box ${ var Int }
  // CHECK:   cond_br {{%.*}}, [[CASE4:bb[0-9]+]],
  case (var b, var c) where ansed():
  // CHECK: [[CASE4]]:
  // CHECK:   br [[CASE5:bb[0-9]+]]
    fallthrough
  case _:
  // CHECK: [[CASE5]]:
  // CHECK:   br [[CONT]]
    ()
  }
  // CHECK: [[CONT]]:
  // CHECK-NEXT: tuple ()
  // CHECK-NEXT: return
}
