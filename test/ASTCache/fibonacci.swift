// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -typecheck %s 2>&1 | FileCheck %s --check-prefix=FIRST
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -typecheck %s 2>&1 | FileCheck %s --check-prefix=SECOND

// FIRST: AST cache: MISS (no cache file)
// FIRST: AST cache: SAVED

// SECOND: AST cache: HIT

// Test that AST caching works with generics and functions.
// Uses -parse-as-library to avoid script-mode (C4).
func fibonacci(_ n: Int) -> Int {
  if n <= 1 { return n }
  return fibonacci(n - 1) + fibonacci(n - 2)
}

func identity<T>(_ x: T) -> T { x }
