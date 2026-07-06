// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -typecheck %s 2>&1 | %FileCheck %s --check-prefix=FIRST
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -typecheck %s 2>&1 | %FileCheck %s --check-prefix=SECOND

// FIRST: AST cache: MISS (no cache file)
// FIRST: AST cache: SAVED

// SECOND: AST cache: HIT

// This test verifies that protocols with associated types are correctly
// cached and deserialized, including the generic_signature field which
// requires force-computation during deserialization.

protocol Container {
  associatedtype Item
  func getItem() -> Item
}

struct IntContainer: Container {
  typealias Item = Int
  func getItem() -> Int { 0 }
}
