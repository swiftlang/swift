// RUN: %target-typecheck-verify-swift -dump-ast > %t.ast 2>&1
// RUN: %FileCheck %s < %t.ast

// Test overriding of protocol members.

protocol P0 {
  func foo()
}

protocol P1: P0 {
  // CHECK: func_decl{{.*}}foo(){{.*}}Self : P1{{.*}}override={{.*}}P0.foo
  func foo()
}

protocol P2 {
  func foo()
}

protocol P3: P1, P2 {
  // CHECK: func_decl{{.*}}foo(){{.*}}Self : P3{{.*}}override={{.*}}P2.foo{{.*,.*}}P1.foo
  func foo()
}

protocol P4: P0 {
  // CHECK: func_decl{{.*}}foo(){{.*}}Self : P4
  // CHECK-NOT: override=
  // CHECK-SAME: )
  func foo() -> Int
}
