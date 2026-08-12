// Literal expressions may reference a type's `static let` properties. The
// reference folds through to the property's initializer, subject to the same
// look-through and ABI-surface restrictions as a direct variable reference.

// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -typecheck -dump-ast %s -disable-availability-checking -enable-experimental-feature LiteralExpressions -verify | %FileCheck %s

// A `static let` whose initializer is itself a chain of static references folds
// to its literal value.
enum InvaderGrid {
  static let rows = 3
  static let cols = 11
  static let count = rows * cols
}

struct GameState {
  var invaderAlive: [(InvaderGrid.count) of Bool]
  // CHECK-LABEL: (pattern_named type="[33 of Bool]" "invaderAlive")
}

// The explicit `InlineArray<...>` spelling folds the same way as the `of` sugar.
let explicit: InlineArray<(InvaderGrid.count), Bool> = .init(repeating: false)
// CHECK-LABEL: (pattern_named type="InlineArray<33, Bool>" "explicit")

// A static reference may be combined with operators.
let arith: [(InvaderGrid.count + 2) of Int] = .init(repeating: 0)
// CHECK-LABEL: (pattern_named type="[35 of Int]" "arith")

// A static property may reference a static property of another type.
enum Dim { static let n = 4 }
enum Layout { static let total = Dim.n * 2 }
let cross: [(Layout.total) of Int] = .init(repeating: 0)
// CHECK-LABEL: (pattern_named type="[8 of Int]" "cross")

// A `static let` that is part of the module's ABI surface is rejected, just
// like a publicly visible top-level `let`.
public enum Public {
  public static let size = 4096
}
let publicRef: [(Public.size) of UInt8] = .init(repeating: 0)
// expected-error@-1 {{reference to a public 'let' binding is not permitted in a literal expression}}
// expected-error@-2 {{generic value must be an integer literal expression}}

// A reference is only foldable if it is guaranteed to resolve to this exact
// declaration, and hence this exact value, at runtime. A `class` property is
// dispatched through the metatype and may be overridden by a subclass, so it is
// not foldable even when the base declaration has a literal value.
class Overridable {
  class var shared: Int { 4 }
}
let overridable: [(Overridable.shared) of Int] = .init(repeating: 0)
// expected-error@-1 {{unable to resolve variable reference in a literal expression}}
// expected-error@-2 {{generic value must be an integer literal expression}}

final class NotOverridable {
  class var shared: Int { 4 }
}
let notOverridable: [(NotOverridable.shared) of Int] = .init(repeating: 0)
// expected-error@-1 {{unable to resolve variable reference in a literal expression}}
// expected-error@-2 {{generic value must be an integer literal expression}}

// A `dynamic` member can be replaced at runtime via `@_dynamicReplacement`, so
// even an immutable `static let` with a literal initializer is not foldable.
struct Replaceable {
  dynamic static let value = 8
}
let replaceable: [(Replaceable.value) of Int] = .init(repeating: 0)
// expected-error@-1 {{unable to resolve variable reference in a literal expression}}
// expected-error@-2 {{generic value must be an integer literal expression}}
