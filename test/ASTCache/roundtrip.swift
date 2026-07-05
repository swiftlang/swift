// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: %target-swift-frontend -parse-as-library -typecheck -verify-ast-cache \
// RUN:   -module-name testmod %s 2>&1 | %FileCheck %s

// CHECK: AST round-trip verification: PASSED

// This test verifies the -verify-ast-cache frontend action, which:
//   1. Type-checks the primary SourceFile.
//   2. Serializes it to a temp .swiftast via the purpose-built AST cache format.
//   3. Deserializes it into a fresh SourceFile.
//   4. Dumps both ASTs to JSON, normalizes, and compares.
//   5. Prints "AST round-trip verification: PASSED" if structurally identical.
//
// The test fails (no CHECK match) if -verify-ast-cache is not wired up or if
// the deserialized AST differs structurally from the original.

struct Foo {
  var x: Int
  func bar() -> Int { x + 1 }
}

extension Foo {
  var baz: Int { x * 2 }
}
