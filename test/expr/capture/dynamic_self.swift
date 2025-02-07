// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s

// CHECK: func_decl{{.*}}"clone()" interface_type="(Android) -> () -> Self"

class Android {
  func clone() -> Self {
    // CHECK: closure_expr type="() -> Self" {{.*}} discriminator=0 nonisolated captures=(<dynamic_self> self<direct>)
    let fn = { return self }
    return fn()
  }
}
