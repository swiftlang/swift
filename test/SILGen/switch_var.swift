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

func runced(x:Int) -> Bool { return true }
func funged(x:Int) -> Bool { return true }
func ansed(x:Int) -> Bool { return true }

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

func a(x:Int) {}
func b(x:Int) {}
func c(x:Int) {}
func d(x:Int) {}

func aa(x:(Int, Int)) {}
func bb(x:(Int, Int)) {}
func cc(x:(Int, Int)) {}

// CHECK: sil @_T10switch_var10test_var_1FT_T_
func test_var_1() {
  // CHECK:   function_ref @_T10switch_var3fooFT_Si
  switch foo() {
  // CHECK:   [[X:%.*]] = alloc_box $Int64
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case var x:
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T10switch_var1aFT1xSi_T_
  // CHECK:   load [[X]]#1
  // CHECK:   br [[CONT:bb[0-9]+]]
    a(x)
  }
  // CHECK: [[CONT]]:
  // CHECK:   release [[X]]#0
  // CHECK:   function_ref @_T10switch_var1bFT_T_
  b()
}

// CHECK: sil @_T10switch_var10test_var_2FT_T_
func test_var_2() {
  // CHECK:   function_ref @_T10switch_var3fooFT_Si
  switch foo() {
  // CHECK:   [[XYZ:%.*]] = alloc_box $Int64
  // CHECK:   function_ref @_T10switch_var6runcedFT1xSi_Sb
  // CHECK:   load [[XYZ]]#1
  // CHECK:   condbranch {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NO_CASE1:bb[0-9]+]]
  // -- TODO: Clean up these empty waypoint bbs.
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case var x where runced(x):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T10switch_var1aFT1xSi_T_
  // CHECK:   load [[XYZ]]#1
  // CHECK:   br [[CONT:bb[0-9]+]]
    a(x)
  // CHECK: [[NO_CASE1]]:
  // CHECK:   function_ref @_T10switch_var6fungedFT1xSi_Sb
  // CHECK:   load [[XYZ]]#1
  // CHECK:   condbranch {{%.*}}, [[YES_CASE2:bb[0-9]+]], [[NO_CASE2:bb[0-9]+]]
  // CHECK: [[YES_CASE2]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case var y where funged(y):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T10switch_var1bFT1xSi_T_
  // CHECK:   load [[XYZ]]#1
  // CHECK:   br [[CONT]]
    b(y)
  // CHECK: [[NO_CASE2]]:
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case var z:
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T10switch_var1cFT1xSi_T_
  // CHECK:   load [[XYZ]]#1
  // CHECK:   br [[CONT]]
    c(z)
  }
  // CHECK: [[CONT]]:
  // CHECK:   release [[XYZ]]#0
  // CHECK:   function_ref @_T10switch_var1dFT_T_
  d()
}

// CHECK: sil @_T10switch_var10test_var_3FT_T_
func test_var_3() {
  // CHECK:   function_ref @_T10switch_var3fooFT_Si
  // CHECK:   function_ref @_T10switch_var3barFT_Si
  switch (foo(), bar()) {
  // CHECK:   [[XWV:%.*]] = alloc_box $(Int64, Int64)
  // CHECK:   function_ref @_T10switch_var6runcedFT1xSi_Sb
  // CHECK:   tuple_element_addr [[XWV]]#1 : {{.*}}, 0
  // CHECK:   condbranch {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NO_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case var x where runced(x.0):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T10switch_var2aaFT1xTSiSi__T_
  // CHECK:   load [[XWV]]#1
  // CHECK:   br [[CONT1:bb[0-9]+]]
    aa(x)
  // CHECK: [[NO_CASE1]]:
  // CHECK:   [[Y:%.*]] = alloc_box $Int64
  // CHECK:   [[Z:%.*]] = alloc_box $Int64
  // CHECK:   function_ref @_T10switch_var6fungedFT1xSi_Sb
  // CHECK:   load [[Y]]#1
  // CHECK:   condbranch {{%.*}}, [[YES_CASE2:bb[0-9]+]], [[NO_CASE2:bb[0-9]+]]
  // CHECK: [[YES_CASE2]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case (var y, var z) where funged(y):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T10switch_var1aFT1xSi_T_
  // CHECK:   load [[Y]]#1
  // CHECK:   function_ref @_T10switch_var1bFT1xSi_T_
  // CHECK:   load [[Z]]#1
  // CHECK:   br [[CONT2:bb[0-9]+]]
    a(y)
    b(z)
  // CHECK: [[NO_CASE2]]:
  // CHECK:   function_ref @_T10switch_var5ansedFT1xSi_Sb
  // CHECK:   tuple_element_addr [[XWV]]#1 : {{.*}}, 0
  // CHECK:   condbranch {{%.*}}, [[YES_CASE3:bb[0-9]+]], [[NO_CASE3:bb[0-9]+]]
  // CHECK: [[YES_CASE3]]:
  // CHECK:   release [[Z]]#0
  // CHECK:   release [[Y]]#0
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case var w where ansed(w.0):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T10switch_var2bbFT1xTSiSi__T_
  // CHECK:   load [[XWV]]#1
  // CHECK:   br [[CONT1]]
    bb(w)
  // CHECK: [[NO_CASE3]]:
  // CHECK:   release [[Z]]#0
  // CHECK:   release [[Y]]#0
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case var v:
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_T10switch_var2ccFT1xTSiSi__T_
  // CHECK:   load [[XWV]]#1
  // CHECK:   br [[CONT1]]
    cc(v)
  }
  // CHECK: [[CONT2]]:
  // CHECK:   release [[Z]]#0
  // CHECK:   release [[Y]]#0
  // CHECK:   br [[CONT1]]
  // CHECK: [[CONT1]]:
  // CHECK:   release [[XWV]]#0
  // CHECK:   function_ref @_T10switch_var1dFT_T_
  d()
}

protocol P { func p() }

struct X : P { func p() {} }
struct Y : P { func p() {} }
struct Z : P { func p() {} }

// CHECK: sil @_T10switch_var10test_var_4FT1pPS_1P__T_
func test_var_4(p:P) {
  // CHECK:   function_ref @_T10switch_var3fooFT_Si
  switch (p, foo()) {
  // CHECK:   [[Z:%.*]] = alloc_box $(P, Int64)
  // CHECK:   [[XYW:%.*]] = alloc_box $Int64
  // CHECK:   is_nonnull {{%.*}} : $*X
  // CHECK:   condbranch {{%.*}}, [[IS_X:bb[0-9]+]], [[IS_NOT_X:bb[0-9]+]]
  // CHECK: [[IS_NOT_X]]:
  // CHECK:   is_nonnull {{%.*}} : $*Y
  // CHECK:   condbranch {{%.*}}, [[IS_Y:bb[0-9]+]], [[IS_NOT_Y:bb[0-9]+]]

  // CHECK: [[IS_X]]:
  // CHECK:   function_ref @_T10switch_var6runcedFT1xSi_Sb
  // CHECK:   load [[XYW]]#1
  // CHECK:   condbranch {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NO_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case (is X, var x) where runced(x):
  // CHECK: [[CASE1]]:
  // CHECK:   function_ref @_T10switch_var1aFT1xSi_T_
  // CHECK:   br [[CONT2:bb[0-9]+]]
    a(x)

  // CHECK: [[NO_CASE1]]:
  // CHECK:   function_ref @_T10switch_var5ansedFT1xSi_Sb
  // CHECK:   tuple_element_addr [[Z]]#1 : {{.*}}, 1
  // CHECK:   condbranch {{%.*}}, [[X_YES_CASE3:bb[0-9]+]], [[X_NO_CASE3:bb[0-9]+]]
  // CHECK: [[X_YES_CASE3]]:
  // CHECK:   release [[XYW]]#0
  // CHECK:   br [[CASE3:bb[0-9]+]]
  // CHECK: [[X_NO_CASE3]]:
  // CHECK:   br [[CASE4:bb[0-9]+]]

  // CHECK: [[IS_Y]]:
  // CHECK:   function_ref @_T10switch_var6fungedFT1xSi_Sb
  // CHECK:   load [[XYW]]#1
  // CHECK:   condbranch {{%.*}}, [[YES_CASE2:bb[0-9]+]], [[NO_CASE2:bb[0-9]+]]
  // CHECK: [[YES_CASE2]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case (is Y, var y) where funged(y):
  // CHECK: [[CASE2]]:
  // CHECK:   function_ref @_T10switch_var1bFT1xSi_T_
  // CHECK:   load [[XYW]]#1
  // CHECK:   br [[CONT2]]
    b(y)

  // CHECK: [[NO_CASE2]]:
  // CHECK:   function_ref @_T10switch_var5ansedFT1xSi_Sb
  // CHECK:   tuple_element_addr [[Z]]#1 : {{.*}}, 1
  // CHECK:   condbranch {{%.*}}, [[Y_YES_CASE3:bb[0-9]+]], [[Y_NO_CASE3:bb[0-9]+]]
  // CHECK: [[Y_YES_CASE3]]:
  // CHECK:   release [[XYW]]#0
  // CHECK:   br [[CASE3]]
  // CHECK: [[Y_NO_CASE3]]:
  // CHECK:   br [[CASE4]]

  // CHECK: [[IS_NOT_Y]]:
  // CHECK:   function_ref @_T10switch_var5ansedFT1xSi_Sb
  // CHECK:   tuple_element_addr [[Z]]#1 : {{.*}}, 1
  // CHECK:   condbranch {{%.*}}, [[DFLT_YES_CASE3:bb[0-9]+]], [[DFLT_NO_CASE3:bb[0-9]+]]
  // CHECK: [[DFLT_YES_CASE3]]:
  // CHECK:   release [[XYW]]#0
  // CHECK:   br [[CASE3]]
  case var z where ansed(z.1):
  // CHECK: [[CASE3]]:
  // CHECK:   function_ref @_T10switch_var1cFT1xSi_T_
  // CHECK:   tuple_element_addr [[Z]]#1 : {{.*}}, 1
  // CHECK:   br [[CONT1:bb[0-9]+]]
    c(z.1)

  // CHECK: [[DFLT_NO_CASE3]]:
  // CHECK:   br [[CASE4]]
  case (_, var w):
  // CHECK: [[CASE4]]:
  // CHECK:   function_ref @_T10switch_var1dFT1xSi_T_
  // CHECK:   load [[XYW]]#1
    d(w)
  }
  // CHECK: [[CONT2]]:
  // CHECK:   release [[XYW]]#0
  // CHECK:   br [[CONT1]]
  // CHECK: [[CONT1]]:
  // CHECK:   release [[Z]]#0
  // CHECK:   function_ref @_T10switch_var1eFT_T_
  e()
}

func test_var_5() {
  // CHECK:   function_ref @_T10switch_var3fooFT_Si
  // CHECK:   function_ref @_T10switch_var3barFT_Si
  switch (foo(), bar()) {
  // CHECK:   [[X:%.*]] = alloc_box $(Int64, Int64)
  // CHECK:   condbranch {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NO_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[CASE1:bb[0-9]+]]
  case var x where runced(x.0):
  // CHECK: [[CASE1]]:
  // CHECK:   br [[CONT1:bb[0-9]+]]
    a()
  // CHECK: [[NO_CASE1]]:
  // CHECK:   [[Y:%.*]] = alloc_box $Int64
  // CHECK:   [[Z:%.*]] = alloc_box $Int64
  // CHECK:   condbranch {{%.*}}, [[YES_CASE2:bb[0-9]+]], [[NO_CASE2:bb[0-9]+]]
  // CHECK: [[YES_CASE2]]:
  // CHECK:   br [[CASE2:bb[0-9]+]]
  case (var y, var z) where funged(y):
  // CHECK: [[CASE2]]:
  // CHECK:   br [[CONT2:bb[0-9]+]]
    b()
  // CHECK: [[NO_CASE2]]:
  // CHECK:   condbranch {{%.*}}, [[YES_CASE3:bb[0-9]+]], [[NO_CASE3:bb[0-9]+]]
  // CHECK: [[YES_CASE3]]:
  // CHECK:   release [[Z]]#0
  // CHECK:   release [[Y]]#0
  // CHECK:   br [[CASE3:bb[0-9]+]]
  case (_, _) where runced():
  // CHECK: [[CASE3]]:
  // CHECK:   br [[CONT1]]
    c()
  // CHECK: [[NO_CASE3]]:
  // CHECK:   release [[Z]]#0
  // CHECK:   release [[Y]]#0
  // CHECK:   br [[CASE4:bb[0-9]+]]
  case _:
  // CHECK: [[CASE4]]:
  // CHECK:   br [[CONT1]]
    d()
  }
  // CHECK: [[CONT2]]:
  // CHECK:   release [[Z]]#0
  // CHECK:   release [[Y]]#0
  // CHECK:   br [[CONT1]]
  // CHECK: [[CONT1]]:
  // CHECK:   release [[X]]#0
  e()
}

func test_var_return() {
  switch (foo(), bar()) {
  // CHECK: [[XWV:%.*]] = alloc_box $(Int64, Int64)
  case var x where runced():
    // CHECK: function_ref @_T10switch_var1aFT_T_
    // CHECK: release [[XWV]]#0
    // CHECK: br [[EPILOG:bb[0-9]+]]
    a()
    return
  // CHECK: [[Y:%.*]] = alloc_box $Int64
  // CHECK: [[Z:%.*]] = alloc_box $Int64
  case (var y, var z) where funged():
    // CHECK: function_ref @_T10switch_var1bFT_T_
    // CHECK: release [[Z]]#0
    // CHECK: release [[Y]]#0
    // CHECK: release [[XWV]]#0
    // CHECK: br [[EPILOG]]
    b()
    return
  case var w where ansed():
    // CHECK: function_ref @_T10switch_var1cFT_T_
    // CHECK-NOT: release [[Z]]#0
    // CHECK-NOT: release [[Y]]#0
    // CHECK: release [[XWV]]#0
    // CHECK: br [[EPILOG]]
    c()
    return
  case var v:
    // CHECK: function_ref @_T10switch_var1dFT_T_
    // CHECK-NOT: release [[Z]]#0
    // CHECK-NOT: release [[Y]]#0
    // CHECK: release [[XWV]]#0
    // CHECK: br [[EPILOG]]
    d()
    return
  }
}
