// Attribute and pound-directive forms that used to crash ASTGen. Each now
// produces output identical to the C++ parser, so the same expectations are
// checked against both.

// RUN: %target-typecheck-verify-swift \
// RUN:   -verify-additional-prefix astgen- \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   -enable-experimental-feature Lifetimes
// RUN: %target-typecheck-verify-swift \
// RUN:   -verify-additional-prefix cxx- \
// RUN:   -enable-experimental-feature Lifetimes

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_Lifetimes

// '@_lifetime()' with no specifier.
struct NE: ~Escapable {}

@_lifetime()
// expected-error@-1 {{expected 'copy', 'borrow', or '&' followed by an identifier or 'self' in lifetime dependence specifier}}
func missingLifetimeDependence(_ ne: NE) -> NE { ne }

// '@_preInverseGenerics(except:)' takes a type. ASTGen used to bail out with
// "does not yet support the except: argument"; the argument is now generated, so
// the feature requirement fires in the type checker, where it belongs.
@_preInverseGenerics(except: ~Copyable)
// expected-error@-1 {{'@_preInverseGenerics' is an experimental feature; use '-enable-experimental-feature PreInverseGenericsExcept'}}
func exceptCopyable<T: ~Copyable & ~Escapable>(_ t: borrowing T) {}

// The bare form needs no feature.
@_preInverseGenerics
func barePreInverseGenerics<T: ~Copyable>(_ t: borrowing T) {}

// A condition directive used where an expression is expected. The C++ parser
// parses it anyway for recovery, rejects it, and yields an ErrorExpr; ASTGen now
// does the same. The two report it differently -- the C++ parser stops the
// condition at the directive and then complains about the missing brace, while
// swift-syntax parses the whole comparison as one expression -- but both reject.
func plainTarget() {}

func hasSymbolInExpressionPosition() {
  if #_hasSymbol(plainTarget) == false {}
  // expected-astgen-error@-1 {{#_hasSymbol may only be used as condition of an 'if', 'guard' or 'while' statement}}
  // expected-cxx-error@-2 {{expected '{' after 'if' condition}}
  // expected-cxx-warning@-3 {{global function 'plainTarget()' is not a weakly linked declaration}}
}
