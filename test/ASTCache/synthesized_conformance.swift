// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -typecheck %s 2>&1 | %FileCheck %s --check-prefix=FIRST
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -typecheck %s 2>&1 | %FileCheck %s --check-prefix=SECOND

// FIRST: AST cache: MISS (no cache file)
// FIRST: AST cache: SAVED

// SECOND: AST cache: HIT

// This test verifies that synthesized conformances (Sendable, Hashable, etc.)
// are correctly preserved through the AST cache round-trip. The SnapshotDeserializer
// marks conformances as Synthesized when their protocol is not in the explicit
// inherits list.

// Explicit conformance to Container (should remain explicit after deserialization)
protocol Container {
  associatedtype Item
  func getItem() -> Item
}

// Has explicit Container conformance + synthesized Sendable/Hashable/etc.
struct MyContainer: Container {
  let value: Int
  func getItem() -> Int { value }
}

// Enum with synthesized Hashable/Equatable (no explicit conformance)
enum Color: Int {
  case red, green, blue
}
