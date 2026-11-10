// Enum raw values that used to crash ASTGen: it built an EnumElementDecl holding
// a non-literal raw value, and the bridge then narrowed that to a LiteralExpr.
// Each of these is rejected with the same diagnostic as the C++ parser.

// RUN: %target-typecheck-verify-swift -parse-as-library \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature ParserASTGen
// RUN: %target-typecheck-verify-swift -parse-as-library \
// RUN:   -disable-availability-checking

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

// A literal raw value is fine, including a negated one.
enum Negative: Int {
  case minusOne = -1
  case zero = 0
}

enum NegativeFloat: Double {
  case minusHalf = -0.5
}

enum Str: String {
  case a = "a"
}

// Anything else is rejected by the parser. (With 'LiteralExpressions' enabled the
// rules are different -- an integer literal *expression* is allowed and the other
// literal kinds are not -- which test/LiteralExpressions/EnumRawValue*.swift
// covers, and which also stopped crashing with this change.)
enum Closure: String {
  case bob = {"test"}
  // expected-error@-1 {{raw value for enum case must be a literal}}
}

enum Shifted: Int {
  case universe = 1 << 1
  // expected-error@-1 {{raw value for enum case must be a literal}}
}

enum Awaited: Int {
  case foo = await 5
  // expected-error@-1 {{raw value for enum case must be a literal}}
}
