// A raw value that can never be a literal, such as a regex literal or a magic
// identifier like #file, is diagnosed and replaced with an automatic value, so
// the enum still conforms to RawRepresentable.
//
// With the feature off the parser clears such a raw value. The feature turns
// that parser guard off, so the clearing happens during raw value checking
// instead. Without it the non-literal reached 'cloneRawLiteralExpr' in
// RawRepresentable derivation, which asserted.

// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-typecheck-verify-swift -enable-experimental-feature LiteralExpressions

// Referencing the derived initializer is what reaches the derivation body. The
// enum still conforms, so no 'does not conform' diagnostic appears here.
_ = RegexRawValue.init
enum RegexRawValue: Int {
  case a = #/ /# // expected-error {{raw value for enum case must be a literal}}
}

_ = MagicRawValue.init
enum MagicRawValue: String {
  case a = #file // expected-error {{raw value for enum case must be a literal}}
}

// The derived 'rawValue' getter clones the same expression.
_ = RawValueGetter.a.rawValue
enum RawValueGetter: Int {
  case a = #/ /# // expected-error {{raw value for enum case must be a literal}}
}

// Recovery assigns the automatic value the case would have had, so a later case
// that spells that value explicitly is a duplicate.
enum RecoveredFromZero: Int {
  case a = #/ /#
  // expected-error@-1 {{raw value for enum case must be a literal}}
  // expected-note@-2 {{raw value previously used here}}
  // expected-note@-3 {{raw value implicitly auto-incremented from zero}}
  case b = 0 // expected-error {{raw value for enum case is not unique}}
}

enum RecoveredFromPrevious: Int {
  case z = 5 // expected-note {{raw value auto-incremented from here}}
  case a = #/ /#
  // expected-error@-1 {{raw value for enum case must be a literal}}
  // expected-note@-2 {{raw value previously used here}}
  case b = 6 // expected-error {{raw value for enum case is not unique}}
}

// A case after the recovered one keeps incrementing.
enum RecoveredThenImplicit: Int {
  case a = #/ /#
  // expected-error@-1 {{raw value for enum case must be a literal}}
  // expected-note@-2 {{raw value implicitly auto-incremented from zero}}
  case b // expected-note {{raw value previously used here}}
  case c = 1 // expected-error {{raw value for enum case is not unique}}
}

// A valid literal expression is unaffected.
enum Folded: Int {
  case a = 1 + 1
  case b = 3
}

// An expression that type-checks but does not fold to a literal takes a second
// recovery path, reached after the fold fails rather than before type checking.
// The two paths must agree, both on the value they recover and on registering
// that value for uniqueness.
func nonConstant() -> Int { 4 }

enum FoldPathNumbering: Int {
  case z = 5 // expected-note {{raw value auto-incremented from here}}
  case a = nonConstant()
  // expected-error@-1 {{not supported in a literal expression}}
  // expected-error@-2 {{raw value for enum case must be an integer literal expression}}
  // expected-note@-3 {{raw value previously used here}}
  case b = 6 // expected-error {{raw value for enum case is not unique}}
}

enum EarlyPathNumbering: Int {
  case z = 5 // expected-note {{raw value auto-incremented from here}}
  case a = #file
  // expected-error@-1 {{raw value for enum case must be a literal}}
  // expected-note@-2 {{raw value previously used here}}
  case b = 6 // expected-error {{raw value for enum case is not unique}}
}

// A previous value that cannot seed the sequence must not be handed to the
// automatic-value synthesiser: under integer numbering it rejects any seed that
// is not an integer literal, which would turn recovery into a second error and
// cost the enum its RawRepresentable conformance. Recovery restarts from zero.
func nonConstantDouble() -> Double { 0 }

enum UnusableSeed: Double {
  case a = 1.5
  case b = nonConstantDouble()
  // expected-error@-1 {{raw value for enum case must be a literal}}
}
