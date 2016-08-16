// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

// TODO: Implement tuple equality in the library.
// BLOCKED: <rdar://problem/13822406>
func ~= (x: (Int, Int), y: (Int, Int)) -> Bool {
  return x.0 == y.0 && x.1 == y.1
}

// Some fake predicates for pattern guards.
func runced() -> Bool { return true }
func funged() -> Bool { return true }
func ansed() -> Bool { return true }

func runced(x x: Int) -> Bool { return true }
func funged(x x: Int) -> Bool { return true }
func ansed(x x: Int) -> Bool { return true }

func foo() -> Int { return 0 }
func bar() -> Int { return 0 }
func foobar() -> (Int, Int) { return (0, 0) }

func foos() -> String { return "" }
func bars() -> String { return "" }

func a() {}
func b() {}
func c() {}
func d() {}
func e() {}
func f() {}
func g() {}

func a(x x: Int) {}
func b(x x: Int) {}
func c(x x: Int) {}
func d(x x: Int) {}

func a(x x: String) {}
func b(x x: String) {}

func aa(x x: (Int, Int)) {}
func bb(x x: (Int, Int)) {}
func cc(x x: (Int, Int)) {}

// CHECK-LABEL: sil hidden @_TF10switch_var10test_var_1FT_T_
func test_var_1() {
  // CHECK:   function_ref @_TF10switch_var3fooFT_Si
  switch foo() {
  // CHECK:   [[XADDR:%.*]] = alloc_box $Int
  // CHECK:   [[X:%.*]] = project_box [[XADDR]]
  // CHECK-NOT: br bb
  case var x:
  // CHECK:   function_ref @_TF10switch_var1aFT1xSi_T_
  // CHECK:   load [[X]]
  // CHECK:   release [[XADDR]]
  // CHECK:   br [[CONT:bb[0-9]+]]
    a(x: x)
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF10switch_var1bFT_T_
  b()
}

// CHECK-LABEL: sil hidden @_TF10switch_var10test_var_2FT_T_
func test_var_2() {
  // CHECK:   function_ref @_TF10switch_var3fooFT_Si
  switch foo() {
  // CHECK:   [[XADDR:%.*]] = alloc_box $Int
  // CHECK:   [[X:%.*]] = project_box [[XADDR]]
  // CHECK:   function_ref @_TF10switch_var6runcedFT1xSi_Sb
  // CHECK:   load [[X]]
  // CHECK:   cond_br {{%.*}}, [[CASE1:bb[0-9]+]], [[NO_CASE1:bb[0-9]+]]
  // -- TODO: Clean up these empty waypoint bbs.
  case var x where runced(x: x):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF10switch_var1aFT1xSi_T_
  // CHECK:   load [[X]]
  // CHECK:   release [[XADDR]]
  // CHECK:   br [[CONT:bb[0-9]+]]
    a(x: x)
  // CHECK: [[NO_CASE1]]:
  // CHECK:   [[YADDR:%.*]] = alloc_box $Int
  // CHECK:   [[Y:%.*]] = project_box [[YADDR]]
  // CHECK:   function_ref @_TF10switch_var6fungedFT1xSi_Sb
  // CHECK:   load [[Y]]
  // CHECK:   cond_br {{%.*}}, [[CASE2:bb[0-9]+]], [[NO_CASE2:bb[0-9]+]]
  case var y where funged(x: y):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF10switch_var1bFT1xSi_T_
  // CHECK:   load [[Y]]
  // CHECK:   release [[YADDR]]
  // CHECK:   br [[CONT]]
    b(x: y)
  // CHECK: [[NO_CASE2]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case var z:
  // CHECK: [[CASE3]]:
  // CHECK:   [[ZADDR:%.*]] = alloc_box $Int
  // CHECK:   [[Z:%.*]] = project_box [[ZADDR]]
  // CHECK:   function_ref @_TF10switch_var1cFT1xSi_T_
  // CHECK:   load [[Z]]
  // CHECK:   release [[ZADDR]]
  // CHECK:   br [[CONT]]
    c(x: z)
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF10switch_var1dFT_T_
  d()
}

// CHECK-LABEL: sil hidden  @_TF10switch_var10test_var_3FT_T_
func test_var_3() {
  // CHECK:   function_ref @_TF10switch_var3fooFT_Si
  // CHECK:   function_ref @_TF10switch_var3barFT_Si
  switch (foo(), bar()) {
  // CHECK:   [[XADDR:%.*]] = alloc_box $(Int, Int)
  // CHECK:   [[X:%.*]] = project_box [[XADDR]]
  // CHECK:   function_ref @_TF10switch_var6runcedFT1xSi_Sb
  // CHECK:   tuple_element_addr [[X]] : {{.*}}, 0
  // CHECK:   cond_br {{%.*}}, [[CASE1:bb[0-9]+]], [[NO_CASE1:bb[0-9]+]]
  case var x where runced(x: x.0):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF10switch_var2aaFT1xTSiSi__T_
  // CHECK:   load [[X]]
  // CHECK:   release [[XADDR]]
  // CHECK:   br [[CONT:bb[0-9]+]]
    aa(x: x)
  // CHECK: [[NO_CASE1]]:
  // CHECK:   [[YADDR:%.*]] = alloc_box $Int
  // CHECK:   [[Y:%.*]] = project_box [[YADDR]]
  // CHECK:   [[Z:%.*]] = alloc_box $Int
  // CHECK:   [[Z:%.*]] = project_box [[ZADDR]]
  // CHECK:   function_ref @_TF10switch_var6fungedFT1xSi_Sb
  // CHECK:   load [[Y]]
  // CHECK:   cond_br {{%.*}}, [[CASE2:bb[0-9]+]], [[NO_CASE2:bb[0-9]+]]
  case (var y, var z) where funged(x: y):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF10switch_var1aFT1xSi_T_
  // CHECK:   load [[Y]]
  // CHECK:   function_ref @_TF10switch_var1bFT1xSi_T_
  // CHECK:   load [[Z]]
  // CHECK:   release [[ZADDR]]
  // CHECK:   release [[YADDR]]
  // CHECK:   br [[CONT]]
    a(x: y)
    b(x: z)
  // CHECK: [[NO_CASE2]]:
  // CHECK:   [[WADDR:%.*]] = alloc_box $(Int, Int)
  // CHECK:   [[W:%.*]] = project_box [[WADDR]]
  // CHECK:   function_ref @_TF10switch_var5ansedFT1xSi_Sb
  // CHECK:   tuple_element_addr [[W]] : {{.*}}, 0
  // CHECK:   cond_br {{%.*}}, [[CASE3:bb[0-9]+]], [[NO_CASE3:bb[0-9]+]]
  case var w where ansed(x: w.0):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF10switch_var2bbFT1xTSiSi__T_
  // CHECK:   load [[W]]
  // CHECK:   br [[CONT]]
    bb(x: w)
  // CHECK: [[NO_CASE3]]:
  // CHECK:   release [[WADDR]]
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case var v:
  // CHECK: [[CASE4]]:
  // CHECK:   [[VADDR:%.*]] = alloc_box $(Int, Int) 
  // CHECK:   [[V:%.*]] = project_box [[VADDR]]
  // CHECK:   function_ref @_TF10switch_var2ccFT1xTSiSi__T_
  // CHECK:   load [[V]]
  // CHECK:   release [[VADDR]]
  // CHECK:   br [[CONT]]
    cc(x: v)
  }
  // CHECK: [[CONT]]:
  // CHECK:   function_ref @_TF10switch_var1dFT_T_
  d()
}

protocol P { func p() }

struct X : P { func p() {} }
struct Y : P { func p() {} }
struct Z : P { func p() {} }

// CHECK-LABEL: sil hidden  @_TF10switch_var10test_var_4FT1pPS_1P__T_
func test_var_4(p p: P) {
  // CHECK:   function_ref @_TF10switch_var3fooFT_Si
  switch (p, foo()) {
  // CHECK:   [[PAIR:%.*]] = alloc_stack $(P, Int)
  // CHECK:   store
  // CHECK:   [[PAIR_0:%.*]] = tuple_element_addr [[PAIR]] : $*(P, Int), 0
  // CHECK:   [[T0:%.*]] = tuple_element_addr [[PAIR]] : $*(P, Int), 1
  // CHECK:   [[PAIR_1:%.*]] = load [[T0]] : $*Int
  // CHECK:   [[TMP:%.*]] = alloc_stack $X
  // CHECK:   checked_cast_addr_br copy_on_success P in [[PAIR_0]] : $*P to X in [[TMP]] : $*X, [[IS_X:bb[0-9]+]], [[IS_NOT_X:bb[0-9]+]]

  // CHECK: [[IS_X]]:
  // CHECK:   [[T0:%.*]] = load [[TMP]] : $*X
  // CHECK:   [[XADDR:%.*]] = alloc_box $Int
  // CHECK:   [[X:%.*]] = project_box [[XADDR]]
  // CHECK:   store [[PAIR_1]] to [[X]]
  // CHECK:   function_ref @_TF10switch_var6runcedFT1xSi_Sb
  // CHECK:   load [[X]]
  // CHECK:   cond_br {{%.*}}, [[CASE1:bb[0-9]+]], [[NO_CASE1:bb[0-9]+]]
  case (is X, var x) where runced(x: x):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_TF10switch_var1aFT1xSi_T_
  // CHECK:   release [[XADDR]]
  // CHECK:   dealloc_stack [[TMP]]
  // CHECK:   destroy_addr [[PAIR_0]] : $*P
  // CHECK:   dealloc_stack [[PAIR]]
  // CHECK:   br [[CONT:bb[0-9]+]]
    a(x: x)

  // CHECK: [[NO_CASE1]]:
  // CHECK:   release [[XADDR]]
  // CHECK:   dealloc_stack [[TMP]]
  // CHECK:   br [[NEXT:bb[0-9]+]]
  // CHECK: [[IS_NOT_X]]:
  // CHECK:   dealloc_stack [[TMP]]
  // CHECK:   br [[NEXT]]

  // CHECK: [[NEXT]]:
  // CHECK:   [[TMP:%.*]] = alloc_stack $Y
  // CHECK:   checked_cast_addr_br copy_on_success P in [[PAIR_0]] : $*P to Y in [[TMP]] : $*Y, [[IS_Y:bb[0-9]+]], [[IS_NOT_Y:bb[0-9]+]]

  // CHECK: [[IS_Y]]:
  // CHECK:   [[T0:%.*]] = load [[TMP]] : $*Y
  // CHECK:   [[YADDR:%.*]] = alloc_box $Int
  // CHECK:   [[Y:%.*]] = project_box [[YADDR]]
  // CHECK:   store [[PAIR_1]] to [[Y]]
  // CHECK:   function_ref @_TF10switch_var6fungedFT1xSi_Sb
  // CHECK:   load [[Y]]
  // CHECK:   cond_br {{%.*}}, [[CASE2:bb[0-9]+]], [[NO_CASE2:bb[0-9]+]]

  case (is Y, var y) where funged(x: y):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_TF10switch_var1bFT1xSi_T_
  // CHECK:   load [[Y]]
  // CHECK:   release [[YADDR]]
  // CHECK:   dealloc_stack [[TMP]]
  // CHECK:   destroy_addr [[PAIR_0]] : $*P
  // CHECK:   dealloc_stack [[PAIR]]
  // CHECK:   br [[CONT]]
    b(x: y)

  // CHECK: [[NO_CASE2]]:
  // CHECK:   release [[YADDR]]
  // CHECK:   dealloc_stack [[TMP]]
  // CHECK:   br [[NEXT:bb[0-9]+]]
  // CHECK: [[IS_NOT_Y]]:
  // CHECK:   dealloc_stack [[TMP]]
  // CHECK:   br [[NEXT]]

  // CHECK: [[NEXT]]:
  // CHECK:   [[ZADDR:%.*]] = alloc_box $(P, Int)
  // CHECK:   [[Z:%.*]] = project_box [[ZADDR]]
  // CHECK:   function_ref @_TF10switch_var5ansedFT1xSi_Sb
  // CHECK:   tuple_element_addr [[Z]] : {{.*}}, 1
  // CHECK:   cond_br {{%.*}}, [[CASE3:bb[0-9]+]], [[DFLT_NO_CASE3:bb[0-9]+]]
  case var z where ansed(x: z.1):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_TF10switch_var1cFT1xSi_T_
  // CHECK:   tuple_element_addr [[Z]] : {{.*}}, 1
  // CHECK:   release [[ZADDR]]
  // CHECK-NEXT: dealloc_stack [[PAIR]]
  // CHECK:   br [[CONT]]
    c(x: z.1)

  // CHECK: [[DFLT_NO_CASE3]]:
  // CHECK:   release [[ZADDR]]
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case (_, var w):
  // CHECK: [[CASE4]]:
  // CHECK:   [[PAIR_0:%.*]] = tuple_element_addr [[PAIR]] : $*(P, Int), 0
  // CHECK:   [[WADDR:%.*]] = alloc_box $Int
  // CHECK:   [[W:%.*]] = project_box [[WADDR]]
  // CHECK:   function_ref @_TF10switch_var1dFT1xSi_T_
  // CHECK:   load [[W]]
  // CHECK:   release [[WADDR]]
  // CHECK:   destroy_addr [[PAIR_0]] : $*P
  // CHECK:   dealloc_stack [[PAIR]]
  // CHECK:   br [[CONT]]
    d(x: w)
  }
  e()
}

// CHECK-LABEL: sil hidden @_TF10switch_var10test_var_5FT_T_ : $@convention(thin) () -> () {
func test_var_5() {
  // CHECK:   function_ref @_TF10switch_var3fooFT_Si
  // CHECK:   function_ref @_TF10switch_var3barFT_Si
  switch (foo(), bar()) {
  // CHECK:   [[XADDR:%.*]] = alloc_box $(Int, Int)
  // CHECK:   [[X:%.*]] = project_box [[XADDR]]
  // CHECK:   cond_br {{%.*}}, [[CASE1:bb[0-9]+]], [[NO_CASE1:bb[0-9]+]]
  case var x where runced(x: x.0):
  // CHECK: [[CASE1]]:
  // CHECK:   br [[CONT:bb[0-9]+]]
    a()
  // CHECK: [[NO_CASE1]]:
  // CHECK:   [[YADDR:%[0-9]+]] = alloc_box $Int
  // CHECK:   [[Y:%[0-9]+]] = project_box [[YADDR]]
  // CHECK:   [[ZADDR:%[0-9]+]] = alloc_box $Int
  // CHECK:   [[Z:%[0-9]+]] = project_box [[ZADDR]]
  // CHECK:   cond_br {{%.*}}, [[CASE2:bb[0-9]+]], [[NO_CASE2:bb[0-9]+]]
  case (var y, var z) where funged(x: y):
  // CHECK: [[CASE2]]:
  // CHECK:   release [[ZADDR]]
  // CHECK:   release [[YADDR]]
  // CHECK:   br [[CONT]]
    b()
  // CHECK: [[NO_CASE2]]:
  // CHECK:   release [[ZADDR]]
  // CHECK:   release [[YADDR]]
  // CHECK:   cond_br {{%.*}}, [[CASE3:bb[0-9]+]], [[NO_CASE3:bb[0-9]+]]
  case (_, _) where runced():
  // CHECK: [[CASE3]]:
  // CHECK:   br [[CONT]]
    c()
  // CHECK: [[NO_CASE3]]:
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case _:
  // CHECK: [[CASE4]]:
  // CHECK:   br [[CONT]]
    d()
  }
  // CHECK: [[CONT]]:
  e()
}

// CHECK-LABEL: sil hidden @_TF10switch_var15test_var_returnFT_T_ : $@convention(thin) () -> () {
func test_var_return() {
  switch (foo(), bar()) {
  case var x where runced():
    // CHECK: [[XADDR:%[0-9]+]] = alloc_box $(Int, Int)
    // CHECK: [[X:%[0-9]+]] = project_box [[XADDR]]
    // CHECK: function_ref @_TF10switch_var1aFT_T_
    // CHECK: release [[XADDR]]
    // CHECK: br [[EPILOG:bb[0-9]+]]
    a()
    return
  // CHECK: [[YADDR:%[0-9]+]] = alloc_box $Int
  // CHECK: [[Y:%[0-9]+]] = project_box [[YADDR]]
  // CHECK: [[ZADDR:%[0-9]+]] = alloc_box $Int
  // CHECK: [[Z:%[0-9]+]] = project_box [[ZADDR]]
  case (var y, var z) where funged():
    // CHECK: function_ref @_TF10switch_var1bFT_T_
    // CHECK: release [[ZADDR]]
    // CHECK: release [[YADDR]]
    // CHECK: br [[EPILOG]]
    b()
    return
  case var w where ansed():
    // CHECK: [[WADDR:%[0-9]+]] = alloc_box $(Int, Int)
    // CHECK: [[W:%[0-9]+]] = project_box [[WADDR]]
    // CHECK: function_ref @_TF10switch_var1cFT_T_
    // CHECK-NOT: release [[ZADDR]]
    // CHECK-NOT: release [[YADDR]]
    // CHECK: release [[WADDR]]
    // CHECK: br [[EPILOG]]
    c()
    return
  case var v:
    // CHECK: [[VADDR:%[0-9]+]] = alloc_box $(Int, Int)
    // CHECK: [[V:%[0-9]+]] = project_box [[VADDR]]
    // CHECK: function_ref @_TF10switch_var1dFT_T_
    // CHECK-NOT: release [[ZADDR]]
    // CHECK-NOT: release [[YADDR]]
    // CHECK: release [[VADDR]]
    // CHECK: br [[EPILOG]]
    d()
    return
  }
}

// When all of the bindings in a column are immutable, don't emit a mutable
// box. <rdar://problem/15873365>
// CHECK-LABEL: sil hidden @_TF10switch_var8test_letFT_T_ : $@convention(thin) () -> () {
func test_let() {
  // CHECK: [[FOOS:%.*]] = function_ref @_TF10switch_var4foosFT_SS
  // CHECK: [[VAL:%.*]] = apply [[FOOS]]()

  // CHECK: retain_value [[VAL]]
  // CHECK: function_ref @_TF10switch_var6runcedFT_Sb
  // CHECK: cond_br {{%.*}}, [[CASE1:bb[0-9]+]], [[NO_CASE1:bb[0-9]+]]
  switch foos() {
  case let x where runced():
    // CHECK: [[CASE1]]:
    // CHECK: [[A:%.*]] = function_ref @_TF10switch_var1aFT1xSS_T_
    // CHECK: retain_value [[VAL]]
    // CHECK: apply [[A]]([[VAL]])
    // CHECK: release_value [[VAL]]
    // CHECK: release_value [[VAL]]
    // CHECK: br [[CONT:bb[0-9]+]]
    a(x: x)
  // CHECK: [[NO_CASE1]]:
  // CHECK:   release_value [[VAL]]
  // CHECK:   br [[TRY_CASE2:bb[0-9]+]]
  // CHECK: [[TRY_CASE2]]:
  // CHECK:   retain_value [[VAL]]
  // CHECK:   function_ref @_TF10switch_var6fungedFT_Sb
  // CHECK:   cond_br {{%.*}}, [[CASE2:bb[0-9]+]], [[NO_CASE2:bb[0-9]+]]
  case let y where funged():
    // CHECK: [[CASE2]]:
    // CHECK: [[B:%.*]] = function_ref @_TF10switch_var1bFT1xSS_T_
    // CHECK: retain_value [[VAL]]
    // CHECK: apply [[B]]([[VAL]])
    // CHECK: release_value [[VAL]]
    // CHECK: release_value [[VAL]]
    // CHECK: br [[CONT]]
    b(x: y)
  // CHECK: [[NO_CASE2]]:
  // CHECK:   retain_value [[VAL]]
  // CHECK:   function_ref @_TF10switch_var4barsFT_SS
  // CHECK:   retain_value [[VAL]]
  // CHECK:   cond_br {{%.*}}, [[YES_CASE3:bb[0-9]+]], [[NO_CASE3:bb[0-9]+]]
  // CHECK: [[YES_CASE3]]:
  // CHECK:   release_value [[VAL]]
  // CHECK:   release_value [[VAL]]
  // ExprPatterns implicitly contain a 'let' binding.
  case bars():
    // CHECK:   function_ref @_TF10switch_var1cFT_T_
    // CHECK:   br [[CONT]]
    c()
  // CHECK: [[NO_CASE3]]:
  // CHECK:   release_value [[VAL]]
  // CHECK:   br [[LEAVE_CASE3:bb[0-9]+]]
  // CHECK: [[LEAVE_CASE3]]:
  case _:
    // CHECK:   release_value [[VAL]]
    // CHECK:   function_ref @_TF10switch_var1dFT_T_
    // CHECK:   br [[CONT]]
    d()
  }
  // CHECK: [[CONT]]:
  // CHECK:   return
}

// If one of the bindings is a "var", allocate a box for the column.
// CHECK-LABEL: sil hidden @_TF10switch_var18test_mixed_let_varFT_T_ : $@convention(thin) () -> () {
func test_mixed_let_var() {
  // CHECK: [[FOOS:%.*]] = function_ref @_TF10switch_var4foosFT_SS
  // CHECK: [[VAL:%.*]] = apply [[FOOS]]()
  switch foos() {
  case var x where runced():
    // CHECK: [[BOX:%.*]] = alloc_box $String, var, name "x"
    // CHECK: [[PBOX:%.*]] = project_box [[BOX]]
    // CHECK: store [[VAL]] to [[PBOX]]
    // CHECK: [[A:%.*]] = function_ref @_TF10switch_var1aFT1xSS_T_
    // CHECK: [[X:%.*]] = load [[PBOX]]
    // CHECK: retain_value [[X]]
    // CHECK: apply [[A]]([[X]])
    a(x: x)
  case let y where funged():
    // CHECK: [[B:%.*]] = function_ref @_TF10switch_var1bFT1xSS_T_
    // CHECK: retain_value [[VAL]]
    // CHECK: apply [[B]]([[VAL]])
    b(x: y)
  case bars():
    c()
  case _:
    d()
  }
}

// CHECK-LABEL: sil hidden @_TF10switch_var23test_multiple_patterns1FT_T_ : $@convention(thin) () -> () {
func test_multiple_patterns1() {
  // CHECK:   function_ref @_TF10switch_var6foobarFT_TSiSi_
  switch foobar() {
  // CHECK-NOT: br bb
  case (0, let x), (let x, 0):
    // CHECK:   cond_br {{%.*}}, [[FIRST_MATCH_CASE:bb[0-9]+]], [[FIRST_FAIL:bb[0-9]+]]
    // CHECK:   [[FIRST_MATCH_CASE]]:
    // CHECK:     debug_value [[FIRST_X:%.*]] :
    // CHECK:     br [[CASE_BODY:bb[0-9]+]]([[FIRST_X]] : $Int)
    // CHECK:   [[FIRST_FAIL]]:
    // CHECK:     cond_br {{%.*}}, [[SECOND_MATCH_CASE:bb[0-9]+]], [[SECOND_FAIL:bb[0-9]+]]
    // CHECK:   [[SECOND_MATCH_CASE]]:
    // CHECK:     debug_value [[SECOND_X:%.*]] :
    // CHECK:     br [[CASE_BODY]]([[SECOND_X]] : $Int)
    // CHECK:   [[CASE_BODY]]([[BODY_VAR:%.*]] : $Int):
    // CHECK:     [[A:%.*]] = function_ref @_TF10switch_var1aFT1xSi_T_
    // CHECK:     apply [[A]]([[BODY_VAR]])
    a(x: x)
  default:
    // CHECK:   [[SECOND_FAIL]]:
    // CHECK:     function_ref @_TF10switch_var1bFT_T_
    b()
  }
}

// CHECK-LABEL: sil hidden @_TF10switch_var23test_multiple_patterns2FT_T_ : $@convention(thin) () -> () {
func test_multiple_patterns2() {
  let t1 = 2
  let t2 = 4
  // CHECK:   debug_value [[T1:%.*]] :
  // CHECK:   debug_value [[T2:%.*]] :
  switch (0,0) {
    // CHECK-NOT: br bb
  case (_, let x) where x > t1, (let x, _) where x > t2:
    // CHECK:   [[FIRST_X:%.*]] = tuple_extract {{%.*}} : $(Int, Int), 1
    // CHECK:   debug_value [[FIRST_X]] :
    // CHECK:   apply {{%.*}}([[FIRST_X]], [[T1]])
    // CHECK:   cond_br {{%.*}}, [[FIRST_MATCH_CASE:bb[0-9]+]], [[FIRST_FAIL:bb[0-9]+]]
    // CHECK:   [[FIRST_MATCH_CASE]]:
    // CHECK:     br [[CASE_BODY:bb[0-9]+]]([[FIRST_X]] : $Int)
    // CHECK:   [[FIRST_FAIL]]:
    // CHECK:     debug_value [[SECOND_X:%.*]] :
    // CHECK:     apply {{%.*}}([[SECOND_X]], [[T2]])
    // CHECK:     cond_br {{%.*}}, [[SECOND_MATCH_CASE:bb[0-9]+]], [[SECOND_FAIL:bb[0-9]+]]
    // CHECK:   [[SECOND_MATCH_CASE]]:
    // CHECK:     br [[CASE_BODY]]([[SECOND_X]] : $Int)
    // CHECK:   [[CASE_BODY]]([[BODY_VAR:%.*]] : $Int):
    // CHECK:     [[A:%.*]] = function_ref @_TF10switch_var1aFT1xSi_T_
    // CHECK:     apply [[A]]([[BODY_VAR]])
    a(x: x)
  default:
    // CHECK:   [[SECOND_FAIL]]:
    // CHECK:     function_ref @_TF10switch_var1bFT_T_
    b()
  }
}

enum Foo {
  case A(Int, Double)
  case B(Double, Int)
  case C(Int, Int, Double)
}

// CHECK-LABEL: sil hidden @_TF10switch_var23test_multiple_patterns3FT_T_ : $@convention(thin) () -> () {
func test_multiple_patterns3() {
  let f = Foo.C(0, 1, 2.0)
  switch f {
    // CHECK:   switch_enum {{%.*}} : $Foo, case #Foo.A!enumelt.1: [[A:bb[0-9]+]], case #Foo.B!enumelt.1: [[B:bb[0-9]+]], case #Foo.C!enumelt.1: [[C:bb[0-9]+]]
  case .A(let x, let n), .B(let n, let x), .C(_, let x, let n):
    // CHECK:   [[A]]({{%.*}} : $(Int, Double)):
    // CHECK:     [[A_X:%.*]] = tuple_extract
    // CHECK:     [[A_N:%.*]] = tuple_extract
    // CHECK:     br [[CASE_BODY:bb[0-9]+]]([[A_X]] : $Int, [[A_N]] : $Double)
    
    // CHECK:   [[B]]({{%.*}} : $(Double, Int)):
    // CHECK:     [[B_N:%.*]] = tuple_extract
    // CHECK:     [[B_X:%.*]] = tuple_extract
    // CHECK:     br [[CASE_BODY]]([[B_X]] : $Int, [[B_N]] : $Double)

    // CHECK:   [[C]]({{%.*}} : $(Int, Int, Double)):
    // CHECK:     [[C__:%.*]] = tuple_extract
    // CHECK:     [[C_X:%.*]] = tuple_extract
    // CHECK:     [[C_N:%.*]] = tuple_extract
    // CHECK:     br [[CASE_BODY]]([[C_X]] : $Int, [[C_N]] : $Double)

    // CHECK:   [[CASE_BODY]]([[BODY_X:%.*]] : $Int, [[BODY_N:%.*]] : $Double):
    // CHECK:     [[FUNC_A:%.*]] = function_ref @_TF10switch_var1aFT1xSi_T_
    // CHECK:     apply [[FUNC_A]]([[BODY_X]])
    a(x: x)
  }
}

enum Bar {
  case Y(Foo, Int)
  case Z(Int, Foo)
}

// CHECK-LABEL: sil hidden @_TF10switch_var23test_multiple_patterns4FT_T_ : $@convention(thin) () -> () {
func test_multiple_patterns4() {
  let b = Bar.Y(.C(0, 1, 2.0), 3)
  switch b {
    // CHECK:   switch_enum {{%.*}} : $Bar, case #Bar.Y!enumelt.1: [[Y:bb[0-9]+]], case #Bar.Z!enumelt.1: [[Z:bb[0-9]+]]
  case .Y(.A(let x, _), _), .Y(.B(_, let x), _), .Y(.C, let x), .Z(let x, _):
    // CHECK:   [[Y]]({{%.*}} : $(Foo, Int)):
    // CHECK:     [[Y_F:%.*]] = tuple_extract
    // CHECK:     [[Y_X:%.*]] = tuple_extract
    // CHECK:     switch_enum [[Y_F]] : $Foo, case #Foo.A!enumelt.1: [[A:bb[0-9]+]], case #Foo.B!enumelt.1: [[B:bb[0-9]+]], case #Foo.C!enumelt.1: [[C:bb[0-9]+]]
    
    // CHECK:   [[A]]({{%.*}} : $(Int, Double)):
    // CHECK:     [[A_X:%.*]] = tuple_extract
    // CHECK:     [[A_N:%.*]] = tuple_extract
    // CHECK:     br [[CASE_BODY:bb[0-9]+]]([[A_X]] : $Int)
    
    // CHECK:   [[B]]({{%.*}} : $(Double, Int)):
    // CHECK:     [[B_N:%.*]] = tuple_extract
    // CHECK:     [[B_X:%.*]] = tuple_extract
    // CHECK:     br [[CASE_BODY]]([[B_X]] : $Int)
    
    // CHECK:   [[C]]({{%.*}} : $(Int, Int, Double)):
    // CHECK:     br [[CASE_BODY]]([[Y_X]] : $Int)

    // CHECK:   [[Z]]({{%.*}} : $(Int, Foo)):
    // CHECK:     [[Z_X:%.*]] = tuple_extract
    // CHECK:     [[Z_F:%.*]] = tuple_extract
    // CHECK:     br [[CASE_BODY]]([[Z_X]] : $Int)

    // CHECK:   [[CASE_BODY]]([[BODY_X:%.*]] : $Int):
    // CHECK:     [[FUNC_A:%.*]] = function_ref @_TF10switch_var1aFT1xSi_T_
    // CHECK:     apply [[FUNC_A]]([[BODY_X]])
    a(x: x)
  }
}

func aaa(x x: inout Int) {}

// CHECK-LABEL: sil hidden @_TF10switch_var23test_multiple_patterns5FT_T_ : $@convention(thin) () -> () {
func test_multiple_patterns5() {
  let b = Bar.Y(.C(0, 1, 2.0), 3)
  switch b {
    // CHECK:   switch_enum {{%.*}} : $Bar, case #Bar.Y!enumelt.1: [[Y:bb[0-9]+]], case #Bar.Z!enumelt.1: [[Z:bb[0-9]+]]
  case .Y(.A(var x, _), _), .Y(.B(_, var x), _), .Y(.C, var x), .Z(var x, _):
    // CHECK:   [[Y]]({{%.*}} : $(Foo, Int)):
    // CHECK:     [[Y_F:%.*]] = tuple_extract
    // CHECK:     [[Y_X:%.*]] = tuple_extract
    // CHECK:     switch_enum [[Y_F]] : $Foo, case #Foo.A!enumelt.1: [[A:bb[0-9]+]], case #Foo.B!enumelt.1: [[B:bb[0-9]+]], case #Foo.C!enumelt.1: [[C:bb[0-9]+]]
    
    // CHECK:   [[A]]({{%.*}} : $(Int, Double)):
    // CHECK:     [[A_X:%.*]] = tuple_extract
    // CHECK:     [[A_N:%.*]] = tuple_extract
    // CHECK:     br [[CASE_BODY:bb[0-9]+]]([[A_X]] : $Int)
    
    // CHECK:   [[B]]({{%.*}} : $(Double, Int)):
    // CHECK:     [[B_N:%.*]] = tuple_extract
    // CHECK:     [[B_X:%.*]] = tuple_extract
    // CHECK:     br [[CASE_BODY]]([[B_X]] : $Int)
    
    // CHECK:   [[C]]({{%.*}} : $(Int, Int, Double)):
    // CHECK:     br [[CASE_BODY]]([[Y_X]] : $Int)
    
    // CHECK:   [[Z]]({{%.*}} : $(Int, Foo)):
    // CHECK:     [[Z_X:%.*]] = tuple_extract
    // CHECK:     [[Z_F:%.*]] = tuple_extract
    // CHECK:     br [[CASE_BODY]]([[Z_X]] : $Int)
    
    // CHECK:   [[CASE_BODY]]([[BODY_X:%.*]] : $Int):
    // CHECK:     store [[BODY_X]] to [[BOX_X:%.*]] : $*Int
    // CHECK:     [[FUNC_AAA:%.*]] = function_ref @_TF10switch_var3aaaFT1xRSi_T_
    // CHECK:     apply [[FUNC_AAA]]([[BOX_X]])
    aaa(x: &x)
  }
}

