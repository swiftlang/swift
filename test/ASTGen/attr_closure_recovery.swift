// Attribute and closure forms that used to crash ASTGen. Each is either accepted,
// or rejected with the same diagnostic as the C++ parser -- so the same
// expectations are checked against both parsers.

// RUN: %target-typecheck-verify-swift -parse-as-library \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature RawLayout \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature ParserASTGen
// RUN: %target-typecheck-verify-swift -parse-as-library \
// RUN:   -disable-availability-checking \
// RUN:   -verify-additional-prefix cxx- \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature RawLayout

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_RawLayout
// REQUIRES: swift_feature_Lifetimes

// MARK: - @_rawLayout

// 'count:' may name a generic parameter rather than being a literal. ASTGen only
// handled value expressions here and crashed on a type.
@_rawLayout(likeArrayOf: Element, count: Count)
struct Vector<Element: ~Copyable, let Count: Int>: ~Copyable {}

@_rawLayout(size: 4, alignment: 4)
struct SizeAndAlignment: ~Copyable {}

@_rawLayout(bogus: 4)
// expected-error@-1 {{expected 'size', 'like', or 'likeArrayOf' argument to '@_rawLayout'}}
// expected-cxx-error@-2 {{expected declaration}}
struct UnknownLabel: ~Copyable {}

@_rawLayout(size: notALiteral, alignment: 4)
// expected-error@-1 {{expected integer literal size in '@_rawLayout'}}
// expected-cxx-error@-2 {{expected declaration}}
struct NonLiteralSize: ~Copyable {}

@_rawLayout(size: 4, alignment: notALiteral)
// expected-error@-1 {{expected integer literal alignment in '@_rawLayout'}}
// expected-cxx-error@-2 {{expected declaration}}
struct NonLiteralAlignment: ~Copyable {}

@_rawLayout(size: 4)
// expected-error@-1 {{expected alignment argument after size argument in '@_rawLayout'}}
struct SizeWithoutAlignment: ~Copyable {}

@_rawLayout(likeArrayOf: Int)
// expected-error@-1 {{expected count argument after likeArrayOf argument in '@_rawLayout'}}
struct ArrayWithoutCount: ~Copyable {}

// MARK: - SIL-only type attributes

// ASTGen never parses a SIL file, so a SIL-only type attribute here was written
// in ordinary Swift. Both parsers report it as unknown.
class Klass {}
let silTypeAttr: @moveOnly Klass = Klass()
// expected-error@-1 {{unknown attribute 'moveOnly'}}

// MARK: - Anonymous closure arguments

func anonymousClosureArguments(_ take: ((String, String) -> Void) -> Void) {
  // An explicit -- and empty -- parameter list leaves no parameter for '$0' to
  // name. ASTGen indexed into the list and crashed.
  let noParameters = { () in $0 }
  // expected-error@-1 {{anonymous closure arguments cannot be used inside a closure that has explicit arguments}}
  _ = noParameters

  // When the parameter does have a name, both parsers suggest it instead.
  let named = { (z: Int) in $0 }
  // expected-error@-1 {{anonymous closure arguments cannot be used inside a closure that has explicit arguments; did you mean 'z'?}} {{29-31=z}}
  _ = named

  take { (z, w) in
    _ = $1
    // expected-error@-1 {{anonymous closure arguments cannot be used inside a closure that has explicit arguments; did you mean 'w'?}} {{9-11=w}}
  }
}

// MARK: - @_lifetime

// 'inout' has nothing to apply to here, so neither parser can build a lifetime
// descriptor. ASTGen crashed instead of reporting it.
struct NE: ~Escapable {}
@_lifetime(a: inout a)
// expected-error@-1 {{expected 'copy', 'borrow', or '&' followed by an identifier or 'self' in lifetime dependence specifier}}
func lifetimeBadSpecifier(a: inout NE) {}
