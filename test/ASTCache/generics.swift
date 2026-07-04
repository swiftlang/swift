// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.cold.o %s 2>&1 | FileCheck %s --check-prefix=COLD
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.warm.o %s 2>&1 | FileCheck %s --check-prefix=WARM

// COLD: AST cache: MISS (no cache file)
// COLD: AST cache: SAVED
// WARM: AST cache: HIT

// Test that SIL lowering works from a cached AST with generic constructors (Crash 4).
// Generic constructors trigger SILFunctionType construction which can encounter
// ErrorType in parameter types during deserialization.
struct Wrapper<T> {
  var value: T

  init(_ x: T) {
    self.value = x
  }
}

struct Pair<A, B> {
  var first: A
  var second: B

  init(a: A, b: B) {
    self.first = a
    self.second = b
  }
}
