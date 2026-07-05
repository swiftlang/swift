// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.cold.o %s 2>&1 | %FileCheck %s --check-prefix=COLD
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.warm.o %s 2>&1 | %FileCheck %s --check-prefix=WARM

// COLD: AST cache: MISS (no cache file)
// COLD: AST cache: SAVED

// WARM: AST cache: HIT

// Test that cross-file references resolve correctly when loading from cache.
// Uses -primary-file to specify the main file while making other files
// available as part of the module.

// --- Declare a type that will be referenced from the primary file ---
struct CrossFileStruct {
  var value: Int

  func doubled() -> Int {
    return value * 2
  }
}

extension CrossFileStruct {
  func tripled() -> Int {
    return value * 3
  }
}

// --- Reference it from the same file (simple case) ---
func useCrossFileStruct(_ s: CrossFileStruct) -> Int {
  return s.doubled() + s.tripled()
}
