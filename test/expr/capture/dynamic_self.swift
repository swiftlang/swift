// RUN: %target-swift-frontend -dump-ast %s 2>&1 | %FileCheck %s

// CHECK: func_decl "clone()" interface type='(Android) -> () -> Self'

class Android {
  func clone() -> Self {
    // CHECK: closure_expr type='() -> Self' {{.*}} discriminator=0 captures=(<dynamic_self> self)
    let fn = { return self }
    return fn()
  }
}
