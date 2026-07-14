// Regression test for the LiteralExpressions constant-folder: only integer
// literal expressions may be folded. Non-integer enum raw values and
// non-integer @section constants must be used as written rather than routed
// through the integer constant-folder (which would reject them with a spurious
// diagnostic), and non-literal / magic-identifier raw values must be rejected
// rather than tripping an assertion in RawValueKey.

// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -typecheck %s -verify -verify-ignore-unrelated -enable-experimental-feature LiteralExpressions

// Non-integer raw values are accepted as written (not folded).
enum StringEnum: String { case a = "foo"; case b = "bar" }
enum DoubleEnum: Double { case a = 1.5; case b = 2.5 }

// Integer raw values are still folded from literal expressions.
enum IntEnum: Int { case a = 2 + 2; case b; case c = 1 << 3 }

// Non-integer @section constants are accepted as written.
@section("mysection") let tupleGlobal: (UInt8, UInt8, UInt8) = (1, 2, 3)
@section("mysection") let boolTuple: (Bool, Bool) = (true, false)
// Integer @section constants are still folded.
@section("mysection") let intGlobal: Int = 2 * 4096

// Magic identifiers, calls, and other non-raw-value expressions are rejected
// with a clear diagnostic (and must not crash).
enum MagicEnum: String {
  case a = #file // expected-error {{raw value for enum case must be a literal}}
}
func nonConst() -> String { "x" }
enum CallEnum: String {
  case a = nonConst() // expected-error {{raw value for enum case must be a literal}}
}
