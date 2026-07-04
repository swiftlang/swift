// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.cold.o %s 2>&1 | FileCheck %s --check-prefix=COLD
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.warm.o %s 2>&1 | FileCheck %s --check-prefix=WARM

// COLD: AST cache: MISS (no cache file)
// COLD: AST cache: SAVED
// WARM: AST cache: HIT

// Test that SIL lowering works from a cached AST with default arguments (Crash 2).
// Default arguments trigger DefaultArgumentInitializer path during SIL lowering.
struct Calculator {
  var initial: Double

  init(value: Double = 42.0, scale: Int = 1) {
    self.initial = value * Double(scale)
  }

  func compute(x: Int = 10, y: Int = 20) -> Int {
    return x + y
  }
}
