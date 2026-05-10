// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

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

func z(_ i: Int) {}

// CHECK-LABEL: sil hidden [ossa] @$s18switch_fallthrough5test1yyF
func test1() {
  switch foo() {
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], {{bb[0-9]+}}
  // CHECK: [[YES_CASE1]]:
  case foo():
  // CHECK:   function_ref @$s18switch_fallthrough1ayyF
  // CHECK:   br [[CASE2:bb[0-9]+]]
    a()
    fallthrough
  case bar():
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @$s18switch_fallthrough1byyF
  // CHECK:   br [[CASE3:bb[0-9]+]]
    b()
    fallthrough
  case _:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @$s18switch_fallthrough1cyyF
    c()
  }
  // CHECK:   function_ref @$s18switch_fallthrough1dyyF
  d()
}

// Fallthrough should work even if the next case is normally unreachable
// CHECK-LABEL: sil hidden [ossa] @$s18switch_fallthrough5test2yyF
func test2() {
  switch foo() {
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], {{bb[0-9]+}}
  // CHECK: [[YES_CASE1]]:
  case foo():
  // CHECK:   function_ref @$s18switch_fallthrough1ayyF
  // CHECK:   br [[CASE2:bb[0-9]+]]
    a()
    fallthrough
  case _:
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @$s18switch_fallthrough1byyF
  // CHECK:   br [[CASE3:bb[0-9]+]]
    b()
    fallthrough
  case _:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @$s18switch_fallthrough1cyyF
    c()
  }
  // CHECK:   function_ref @$s18switch_fallthrough1dyyF
  d()
}

// CHECK-LABEL: sil hidden [ossa] @$s18switch_fallthrough5test3yyF
func test3() {
  switch (foo(), bar()) {
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], {{bb[0-9]+}}
  // CHECK: [[YES_CASE1]]:
  case (foo(), bar()):
  // CHECK:   function_ref @$s18switch_fallthrough1ayyF
  // CHECK:   br [[CASE2:bb[0-9]+]]
    a()
    fallthrough
  case (foo(), _):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @$s18switch_fallthrough1byyF
  // CHECK:   br [[CASE3:bb[0-9]+]]
    b()
    fallthrough
  case (_, _):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @$s18switch_fallthrough1cyyF
  // CHECK:   br [[CASE4:bb[0-9]+]]
    c()
    fallthrough
  case (_, _):
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @$s18switch_fallthrough1dyyF
    d()
  }
  // CHECK:   function_ref @$s18switch_fallthrough1eyyF
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

// Fallthrough into case block with binding // CHECK-LABEL: sil hidden [ossa] @$s18switch_fallthrough5test5yyF
func test5() {
  switch (foo(), bar()) {
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], {{bb[0-9]+}}
  // CHECK: [[YES_CASE1]]:
  case (var n, foo()):
    // Check that the var is boxed and unboxed and the final value is the one that falls through into the next case
    // CHECK:   [[BOX:%.*]] = alloc_box ${ var Int }, var, name "n"
    // CHECK:   [[N_LIFETIME:%.*]] = begin_borrow [var_decl] [[BOX]]
    // CHECK:   [[N_BOX:%.*]] = project_box [[N_LIFETIME]] : ${ var Int }, 0
    // CHECK:   function_ref @$s18switch_fallthrough1ayyF
    // CHECK:   [[N:%.*]] = load [trivial] [[N_BOX]] : $*Int
    // CHECK:   destroy_value [[BOX]] : ${ var Int }
    // CHECK:   br [[CASE2:bb[0-9]+]]([[N]] : $Int)
    a()
    fallthrough
  case (foo(), let n):
    // CHECK:   cond_br {{%.*}}, [[YES_SECOND_CONDITION:bb[0-9]+]], {{bb[0-9]+}}
    // CHECK: [[YES_SECOND_CONDITION]]:
    // CHECK:   br [[CASE2]]([[SECOND_N:%.*]] : $Int)
    
    // CHECK: [[CASE2]]([[INCOMING_N:%.*]] : $Int):
    // CHECK:   debug_value [[INCOMING_N]] : $Int, let, name "n"
    // CHECK:   [[Z:%.*]] = function_ref @$s18switch_fallthrough1zyySiF
    // CHECK:    apply [[Z]]([[INCOMING_N]]) : $@convention(thin) (Int) -> ()
    // CHECK:   br [[CONT:bb[0-9]+]]
    z(n)
  case (_, _):
    break
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @$s18switch_fallthrough1eyyF
  e()
}

// rdar://problem/67704651 - crash due to nested fallthrough
func testNestedFallthrough(x: (Int, String), y: (Int, Int)) {
  switch x {
  case (17, let s):
    switch y {
    case (42, let i):
      print("the answer")
    default:
      print("nope")
    }
    fallthrough
  case (42, let s):
    print("42 and \(s)")
  default:
    print("done")
  }
}
