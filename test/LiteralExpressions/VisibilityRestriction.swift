// Literal expressions may not reference publicly visible `let` bindings.
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -typecheck %s -verify \
// RUN:   -package-name myPkg \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature LiteralExpressions

public let publicPageSize = 4096
package let packagePageSize = 4096
internal let internalPageSize = 4096
fileprivate let filePrivatePageSize = 4096
private let privatePageSize = 4096

// =============================================================================
// @section initializer position: public and package bindings are rejected.
// `open` is not exercised here because it cannot apply to a top-level `let`;
// the diagnostic's `%select` branch for `open` is preserved for completeness.
// =============================================================================

@section("mysection") let a = 2 * publicPageSize
// expected-error@-1 {{reference to a public 'let' binding is not permitted in a literal expression}}

@section("mysection") let b = 2 * packagePageSize
// expected-error@-1 {{reference to a package 'let' binding is not permitted in a literal expression}}

// Internal, fileprivate, and private bindings are allowed.
@section("mysection") let c = 2 * internalPageSize
@section("mysection") let d = 2 * filePrivatePageSize
@section("mysection") let e = 2 * privatePageSize

// =============================================================================
// Integer generic argument position: same restriction applies.
// =============================================================================

let f: InlineArray<(2 * publicPageSize), UInt8> = .init(repeating: 0)
// expected-error@-1 {{reference to a public 'let' binding is not permitted in a literal expression}}
// expected-error@-2 {{generic value must be an integer literal expression}}

let g: InlineArray<(2 * packagePageSize), UInt8> = .init(repeating: 0)
// expected-error@-1 {{reference to a package 'let' binding is not permitted in a literal expression}}
// expected-error@-2 {{generic value must be an integer literal expression}}
