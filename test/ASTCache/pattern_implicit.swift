// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: %target-swift-frontend -parse-as-library -typecheck -verify-ast-cache \
// RUN:   -module-name testmod %s 2>&1 | %FileCheck %s

// CHECK: AST round-trip verification: PASSED

// This test exercises pattern implicit-flag preservation across the AST cache
// round-trip. Two cases must be preserved:
//   1. Non-implicit PatternBindingDecl: the inner patterns (NamedPattern,
//      TypedPattern, BindingPattern, ParenPattern) must NOT be implicit.
//   2. Implicit (synthesized) PatternBindingDecl: the inner patterns MUST
//      remain implicit, matching the original AST.
// The synthesized case is covered by Hashable synthesis on Color, which
// produces an implicit `hashValue` PBD whose inner patterns are implicit.

struct Point {
  // Non-implicit PBD: parsed `let x: Int` produces a TypedPattern wrapping a
  // NamedPattern, neither of which should be implicit after deserialization.
  let x: Int
  // Non-implicit PBD with BindingPattern wrapping a TuplePattern.
  let (a, b): (Int, Int)
}

enum Color: Int {
  case red, green, blue
}
// Synthesized Hashable conformance (RawRepresentable) creates an implicit
// `hashValue` PBD whose inner patterns must stay implicit.

func use(_ p: Point, _ c: Color) -> Bool {
  return p.x == 0 && c == .red
}
