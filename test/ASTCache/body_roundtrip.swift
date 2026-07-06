// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: %target-swift-frontend -parse-as-library -typecheck -verify-ast-cache \
// RUN:   -module-name testmod %s 2>&1 | %FileCheck %s

// CHECK: AST round-trip verification: PASSED

// This test verifies that function bodies are serialized in the AST cache
// and deserialized correctly. The body must match structurally including
// return_stmt, declref_expr, member_ref_expr, etc.

// Test 1: Simple function body
func identity<T>(_ x: T) -> T { x }

// Test 2: Struct with stored property (synthesized getter/setter)
struct Foo {
  var x: Int
  func bar() -> Int { x + 1 }
}

// Test 3: Equatable struct (synthesized == and __derived_enum_equals)
extension Foo: Equatable {
  static func == (lhs: Foo, rhs: Foo) -> Bool { lhs.x == rhs.x }
}

// Test 4: Class with init and destructor
final class Bar {
  let value: Int
  init(_ value: Int) { self.value = value }
}

// Test 5: Enum with Equatable conformance (synthesized __derived_enum_equals, hash)
enum Color: Equatable {
  case red, green, blue
  var isWarm: Bool { self == .red }
}
