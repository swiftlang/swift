// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: %target-swift-frontend -parse-as-library -typecheck -verify-ast-cache \
// RUN:   -module-name testmod %s 2>&1 | %FileCheck %s

// CHECK: AST round-trip verification: PASSED

// This test verifies that function bodies are serialized in the AST cache
// and deserialized correctly. The body must match structurally including
// return_stmt, declref_expr, member_ref_expr, etc.

func identity<T>(_ x: T) -> T { x }

struct Foo {
  var x: Int
  func bar() -> Int { x + 1 }
}
