// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

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

// CHECK-LABEL: sil hidden  @_TF18switch_fallthrough5test1FT_T_
func test1() {
  switch foo() {
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], {{bb[0-9]+}}
  // CHECK: [[YES_CASE1]]:
  case foo():
  // CHECK:   function_ref @_TF18switch_fallthrough1aFT_T_
  // CHECK:   br [[CASE2:bb[0-9]+]]
    a()
    fallthrough
  case bar():
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF18switch_fallthrough1bFT_T_
  // CHECK:   br [[CASE3:bb[0-9]+]]
    b()
    fallthrough
  case _:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF18switch_fallthrough1cFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    c()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF18switch_fallthrough1dFT_T_
  d()
}

// Fallthrough should work even if the next case is normally unreachable
// CHECK-LABEL: sil hidden  @_TF18switch_fallthrough5test2FT_T_
func test2() {
  switch foo() {
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], {{bb[0-9]+}}
  // CHECK: [[YES_CASE1]]:
  case foo():
  // CHECK:   function_ref @_TF18switch_fallthrough1aFT_T_
  // CHECK:   br [[CASE2:bb[0-9]+]]
    a()
    fallthrough
  case _:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF18switch_fallthrough1bFT_T_
  // CHECK:   br [[CASE3:bb[0-9]+]]
    b()
    fallthrough
  case _:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF18switch_fallthrough1cFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    c()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF18switch_fallthrough1dFT_T_
  d()
}

// CHECK-LABEL: sil hidden  @_TF18switch_fallthrough5test3FT_T_
func test3() {
  switch (foo(), bar()) {
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], {{bb[0-9]+}}
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case (foo(), bar()):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF18switch_fallthrough1aFT_T_
  // CHECK:   br [[CASE2:bb[0-9]+]]
    a()
    fallthrough
  case (foo(), _):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF18switch_fallthrough1bFT_T_
  // CHECK:   br [[CASE3:bb[0-9]+]]
    b()
    fallthrough
  case (_, _):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF18switch_fallthrough1cFT_T_
  // CHECK:   br [[CASE4:bb[0-9]+]]
    c()
    fallthrough
  case (_, _):
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_TF18switch_fallthrough1dFT_T_
  // CHECK:   br [[CONT:bb[0-9]+]]
    d()
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF18switch_fallthrough1eFT_T_
  e()
}

// Fallthrough should clean up nested pattern variables from the exited scope.
func test4() {
  switch (foo(), bar()) {
  // CHECK:   [[A:%.*]] = alloc_box $(Int, Int)
  // CHECK:   cond_br {{%.*}}, [[CASE1:bb[0-9]+]], {{bb[0-9]+}}
  case var a where runced():
  // CHECK: [[CASE1]]:
  // CHECK:   release [[A]]
  // CHECK:   br [[CASE2:bb[0-9]+]]
    fallthrough
  case _ where funged():
  // CHECK: [[CASE2]]:
  // CHECK:   br [[CONT:bb[0-9]+]]
    ()

  // CHECK:   [[B:%.*]] = alloc_box $Int
  // CHECK:   [[C:%.*]] = alloc_box $Int
  // CHECK:   cond_br {{%.*}}, [[CASE4:bb[0-9]+]],
  case (var b, var c) where ansed():
  // CHECK: [[CASE4]]:
  // CHECK:   release [[C]]#0
  // CHECK:   release [[B]]#0
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
