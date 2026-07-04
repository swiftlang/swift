// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.cold.o %s 2>&1 | FileCheck %s --check-prefix=COLD
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.warm.o %s 2>&1 | FileCheck %s --check-prefix=WARM

// COLD: AST cache: MISS (no cache file)
// COLD: AST cache: SAVED
// WARM: AST cache: HIT

// Test that SIL lowering works from a cached AST with accessors (Crash 1+3).
// Computed properties trigger accessor synthesis. Stored properties trigger
// Copyable conformance lookup during SIL lowering.
struct Container {
  var value: Int

  var doubled: Int {
    get { return value * 2 }
  }

  var description: String {
    return "Container(\(value))"
  }
}
