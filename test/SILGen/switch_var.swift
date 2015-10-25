// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

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

protocol P { func p() }

struct X : P { func p() {} }
struct Y : P { func p() {} }
struct Z : P { func p() {} }

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
  case let x where runced():
    // CHECK: [[A:%.*]] = function_ref @_TF10switch_var1aFT1xSS_T_
    // CHECK: retain_value [[VAL]]
    // CHECK: apply [[A]]([[VAL]])
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
