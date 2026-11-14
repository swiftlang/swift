// Diagnostics that ASTGen emits by referring to the compiler's existing
// diagnostic definitions in DiagnosticsParse.def, rather than by rebuilding the
// message text on the Swift side. See
// lib/ASTGen/Sources/ASTGen/CompilerDiagnostics.swift.
//
// Because the definition is shared with the C++ parser rather than duplicated,
// both parsers must produce these diagnostics identically -- so the same
// expectations are checked against both.

// RUN: %target-typecheck-verify-swift -parse-as-library \
// RUN:   -verify-additional-prefix astgen- \
// RUN:   -enable-experimental-feature ParserASTGen
// RUN: %target-typecheck-verify-swift -parse-as-library

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

// diag::illegal_top_level_stmt
if true { } // expected-error {{statements are not allowed at the top level}}

for _ in 0..<1 { } // expected-error {{statements are not allowed at the top level}}

// diag::illegal_top_level_expr
1 + 1 // expected-error {{expressions are not allowed at the top level}}
// expected-warning@-1 {{result of operator '+' is unused}}

// diag::getset_nontrivial_pattern
//
// The ASTGen-only expectations below are a pre-existing difference in how ASTGen
// recovers from a non-trivial pattern with accessors -- unrelated to which
// diagnostic definition is used, and tracked as part of the error-recovery work.
// The shared diagnostic itself matches the C++ parser exactly.
var (a, b): (Int, Int) { (0, 0) }
// expected-error@-1 {{getter/setter can only be defined for a single variable}}
// expected-astgen-error@-2 {{global 'var' declaration requires an initializer expression or an explicitly stated getter}}
// expected-astgen-note@-3 {{add an initializer to silence this error}}

struct S {
  static var _: Int { 0 }
  // expected-error@-1 {{getter/setter can only be defined for a single variable}}
  // expected-astgen-error@-2 {{property declaration does not bind any variables}}
}

// diag::attr_expected_lparen -- exercises string and boolean diagnostic
// arguments, and the `%select{attribute|modifier}1` formatting.
@_alignment struct MissingAttrArgs {}
// expected-error@-1 {{expected '(' in '_alignment' attribute}}

@_objcRuntimeName struct AlsoMissingAttrArgs {}
// expected-error@-1 {{expected '(' in '_objcRuntimeName' attribute}}
