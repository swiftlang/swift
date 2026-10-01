// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen -enable-experimental-feature ScopeRestrictions

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_ScopeRestrictions

// FIXME: ASTGen doesn't generate '@_scoped' yet. Until it does, the attribute
// must be rejected rather than dropped, which would bypass the feature gate.

struct S {}

func named(a: Int, b: @_scoped(a) S) {}
// expected-error@-1:23{{unknown attribute '_scoped'}}

func labelledAccessScope(a: inout Int, b: @_scoped(left: &a) S) {}
// expected-error@-1:43{{unknown attribute '_scoped'}}
