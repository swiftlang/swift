// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s

// CHECK: func_decl{{.*}}"foo2(_:)"
func foo2(_ x: Int) -> (Int) -> (Int) -> Int {
  // CHECK: closure_expr type="(Int) -> (Int) -> Int" {{.*}} discriminator=0 nonisolated captures=(x)
  return {(bar: Int) -> (Int) -> Int in
    // CHECK: closure_expr type="(Int) -> Int" {{.*}} discriminator=0 nonisolated captures=(x<direct>, bar<direct>)
    return {(bas: Int) -> Int in
      return x + bar + bas
    }
  }
}
