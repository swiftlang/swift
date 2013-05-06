// RUN: %swift -dump-ast %s 2>&1 | FileCheck %s

// CHECK: func_decl "foo"
func foo(x:Int) -> (Int) -> (Int) -> Int {
  // CHECK: func_expr type='(bar : Int) -> (Int) -> Int' captures=(x)
  return func(bar:Int) -> (Int) -> Int {
    // CHECK: func_expr type='(bas : Int) -> Int' captures=(x, bar)
    return func(bas:Int) -> Int {
      return x + bar + bas
    }
  }
}
