// RUN: %target-swift-frontend -dump-ast %s 2>&1 | FileCheck %s

// CHECK: func_decl "foo2(_:)"
func foo2(x: Int) -> (Int) -> (Int) -> Int {
  // CHECK: closure_expr type='(Int) -> (Int) -> Int' {{.*}} discriminator=0 captures=(x)
  return {(bar: Int) -> (Int) -> Int in
    // CHECK: closure_expr type='(Int) -> Int' {{.*}} discriminator=0 captures=(x, bar)
    return {(bas: Int) -> Int in
      return x + bar + bas
    }
  }
}
