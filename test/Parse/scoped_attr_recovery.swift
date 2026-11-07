// RUN: %target-typecheck-verify-swift -enable-experimental-feature ScopeRestrictions

// REQUIRES: swift_feature_ScopeRestrictions

struct S {}
struct Pair {}
struct Box<T> { var value: T }

func speculativeMalformed(a: Int) {
  _ = Box<@_scoped(a,) S>.self
  // expected-cparser-error@-1:22{{unexpected ',' separator}}{{21-22=}} expected-astgen-error@-1 {{invalid argument in '_scoped' attribute}}
  _ = Box<@_scoped(0) S>.self
  // expected-cparser-error@-1:20{{expected identifier or 'self' in '_scoped' attribute}} expected-astgen-error@-1 {{invalid argument in '_scoped' attribute}}
}

func bareKeywordLabel(a: Int, p: @_scoped(default: a) Pair) {}
// expected-cparser-error@-1:43{{keyword 'default' cannot be used as an identifier here}} expected-astgen-error@-1 {{invalid argument in '_scoped' attribute}}
// expected-cparser-note@-2:43{{if this name is unavoidable, use backticks to escape it}}{{43-50=`default`}}

func selfLabel(a: Int, p: @_scoped(self: a) Pair) {}
// expected-cparser-error@-1:36{{keyword 'self' cannot be used as an identifier here}} expected-astgen-error@-1 {{invalid argument in '_scoped' attribute}}
// expected-cparser-note@-2:36{{if this name is unavoidable, use backticks to escape it}}{{36-40=`self`}}

func missingLParen(b: @_scoped S) {}
// expected-error@-1:32{{expected '(' in '_scoped' attribute}}

func emptyRestrictionList(b: @_scoped() S) {}
// expected-error@-1:39{{expected identifier or 'self' in '_scoped' attribute}}

func ampersandWithoutName(b: @_scoped(&) S) {}
// expected-cparser-error@-1:39{{expected identifier or 'self' in '_scoped' attribute}} expected-astgen-error@-1 {{invalid argument in '_scoped' attribute}}

func immortalAccess(b: @_scoped(&immortal) S) {}
// expected-cparser-error@-1:33{{cannot depend on the access scope of 'immortal'}}{{33-34=}} expected-astgen-error@-1 {{invalid argument in '_scoped' attribute}}

// As in an inout argument, '&' must be attached to its operand.
func detachedAmpersand(a: inout Int, b: @_scoped(& a) S) {} // expected-astgen-error {{unexpected code '& a' in attribute}}
// expected-cparser-error@-1:50{{expected identifier or 'self' in '_scoped' attribute}} expected-astgen-error@-1 {{expected argument for '@_scoped' attribute}}

func integerScope(b: @_scoped(0) S) {}
// expected-cparser-error@-1:31{{expected identifier or 'self' in '_scoped' attribute}} expected-astgen-error@-1 {{invalid argument in '_scoped' attribute}}

func dollarScope(b: @_scoped($0) S) {}
// expected-cparser-error@-1:30{{expected identifier or 'self' in '_scoped' attribute}} expected-astgen-error@-1 {{invalid argument in '_scoped' attribute}}

func capitalSelf(b: @_scoped(Self) S) {}
// expected-cparser-error@-1:30{{expected identifier or 'self' in '_scoped' attribute}} expected-astgen-error@-1 {{invalid argument in '_scoped' attribute}}

// FIXME: probably deserves better recovery...
func memberScope(a: Int, b: @_scoped(a.b) S) {}
// expected-cparser-error@-1:39{{expected ',' separator}}{{39-39=,}}
// expected-cparser-error@-2:39{{expected identifier or 'self' in '_scoped' attribute}} expected-astgen-error@-2 {{invalid argument in '_scoped' attribute}}

// Indistinguishable from a missing ',', which is how it is recovered.
func missingRParen(b: @_scoped(immortal S) {} // expected-astgen-note {{to match this opening '('}} expected-astgen-error {{expected ')' to end parameter clause}}
// expected-cparser-error@-1:41{{expected ',' separator}}{{40-40=,}} expected-astgen-error@-1 {{expected type in type}}
// expected-cparser-error@-2:44{{expected parameter type following ':'}} expected-astgen-error@-2 {{unexpected code 'S' in attribute}}

func missingComma(a: Int, b: Int, p: @_scoped(left: a right: b) Pair) {}
// expected-cparser-error@-1:55{{expected ',' separator}}{{54-54=,}} expected-astgen-error@-1 {{unexpected code 'right: b' in attribute}}

func labelWithoutScope(b: @_scoped(left:) Pair) {}
// expected-cparser-error@-1:41{{expected identifier or 'self' in '_scoped' attribute}} expected-astgen-error@-1 {{expected value in attribute}}

func leadingComma(a: Int, b: @_scoped(,a) S) {}
// expected-cparser-error@-1:39{{unexpected ',' separator}}{{39-40=}} expected-astgen-error@-1 {{expected value in attribute}}

func trailingComma(a: Int, b: @_scoped(a,) S) {}
// expected-cparser-error@-1:42{{unexpected ',' separator}}{{41-42=}} expected-astgen-error@-1 {{invalid argument in '_scoped' attribute}}

// A malformed '@_scoped' doesn't derail the attributes after it.
func followedByAttribute(body: @_scoped(0) @Sendable () -> Void) {}
// expected-cparser-error@-1:41{{expected identifier or 'self' in '_scoped' attribute}} expected-astgen-error@-1 {{invalid argument in '_scoped' attribute}}

func repeatedAttribute(a: Int, b: @_scoped(a) @_scoped(a) S) {}
// expected-error@-1:47{{duplicate attribute}}{{47-59=}}

// MARK: - Explicit scope parameters

@_scopeParams func scopeParamsMissingLParen() {}
// expected-error@-1:15{{expected '(' in '_scopeParams' attribute}}

@_scopeParams() func emptyScopeParams() {}
// expected-cparser-error@-1:15{{expected scope parameter name in '_scopeParams' attribute}} expected-astgen-error@-1 {{expected '(' in '_scopeParams' attribute}}

@_scopeParams(a func scopeParamsMissingRParen() {} // expected-astgen-note {{to match this opening '('}}
// expected-cparser-error@-1:17{{expected ',' separator}}{{16-16=,}}
// expected-cparser-error@-2:17{{expected scope parameter name in '_scopeParams' attribute}} expected-astgen-error@-2 {{expected ')' to end attribute}}

@_scopeParams(self) func selfScopeParam() {}
// expected-cparser-error@-1:15{{expected scope parameter name in '_scopeParams' attribute}} expected-astgen-error@-1 {{invalid argument in '_scopeParams' attribute}}

@_scopeParams(Self) func capitalSelfScopeParam() {}
// expected-cparser-error@-1:15{{expected scope parameter name in '_scopeParams' attribute}} expected-astgen-error@-1 {{invalid argument in '_scopeParams' attribute}}

@_scopeParams(&a) func accessScopeParam() {}
// expected-cparser-error@-1:15{{expected scope parameter name in '_scopeParams' attribute}} expected-astgen-error@-1 {{invalid argument in '_scopeParams' attribute}}

@_scopeParams(0) func integerScopeParam() {}
// expected-cparser-error@-1:15{{expected scope parameter name in '_scopeParams' attribute}} expected-astgen-error@-1 {{invalid argument in '_scopeParams' attribute}}

@_scopeParams($0) func dollarScopeParam() {}
// expected-cparser-error@-1:15{{expected scope parameter name in '_scopeParams' attribute}} expected-astgen-error@-1 {{invalid argument in '_scopeParams' attribute}}

@_scopeParams(a, immortal) func immortalScopeParam() {}
// expected-cparser-error@-1:18{{'immortal' cannot be used as a scope parameter name}} expected-astgen-error@-1 {{invalid argument in '_scopeParams' attribute}}
// expected-cparser-note@-2:18{{if this name is unavoidable, use backticks to escape it}}{{18-26=`immortal`}}

@_scopeParams(a: b) func labeledScopeParam() {}
// expected-cparser-error@-1:16{{expected ',' separator}}{{16-16=,}}
// expected-cparser-error@-2:16{{expected scope parameter name in '_scopeParams' attribute}} expected-astgen-error@-2 {{invalid argument in '_scopeParams' attribute}}

@_scopeParams(a.b) func memberScopeParam() {}
// expected-cparser-error@-1:16{{expected ',' separator}}{{16-16=,}}
// expected-cparser-error@-2:16{{expected scope parameter name in '_scopeParams' attribute}} expected-astgen-error@-2 {{invalid argument in '_scopeParams' attribute}}

// Recovered by dropping the separator, so the attribute is still created.
@_scopeParams(,a) func leadingCommaScopeParam() {}
// expected-cparser-error@-1:15{{unexpected ',' separator}}{{15-16=}}
// expected-cparser-error@-2:2{{explicit scope parameters are not supported}} expected-astgen-error@-2 {{expected value in attribute}}

@_scopeParams(a,) func trailingCommaScopeParam() {}
// expected-cparser-error@-1:17{{unexpected ',' separator}}{{16-17=}}
// expected-cparser-error@-2:2{{explicit scope parameters are not supported}} expected-astgen-error@-2 {{expected ')' in '_scopeParams' attribute}}

@_scopeParams(a) @_scopeParams(b) func repeatedScopeParams() {}
// expected-error@-1:19{{duplicate attribute}}
// expected-note@-2:2{{attribute already specified here}}
// expected-error@-3:2{{explicit scope parameters are not supported}}

@_scopeParams(a) class ScopeParamClass {}
// expected-error@-1:2{{'@_scopeParams' attribute cannot be applied to this declaration}}

@_scopeParams(a) var scopeParamVar: Int = 0
// expected-error@-1:2{{'@_scopeParams' attribute cannot be applied to this declaration}}

func scopeParamsOnNonFunctionType(x: @_scopeParams(a) S) {}
// expected-error@-1:38{{'@_scopeParams' only applies to function types}}

func malformedScopeParamsOnFunctionType(body: @_scopeParams(0) @Sendable () -> Void) {}
// expected-cparser-error@-1:61{{expected scope parameter name in '_scopeParams' attribute}} expected-astgen-error@-1 {{invalid argument in '_scopeParams' attribute}}

func repeatedScopeParamsOnFunctionType(body: @_scopeParams(a) @_scopeParams(b) () -> Void) {}
// expected-error@-1:63{{duplicate attribute}}
// expected-error@-2:47{{explicit scope parameters are not supported}}

// Scopes aren't mangled, so an ABI-only declaration has no use for them.
@abi(@_scopeParams(a) func abiScopeParams())
func abiScopeParams() {}
// expected-error@-2:7{{unused '_scopeParams' attribute in '@abi'}}

// MARK: - Access scopes

func accessScopeMissingLParen(a: borrowing @_accessScope Pair) {}
// expected-error@-1:58{{expected '(' in '_accessScope' attribute}}

func emptyAccessScope(a: borrowing @_accessScope() Pair) {}
// expected-error@-1:50{{expected identifier or 'self' in '_accessScope' attribute}}

func accessScopeList(a: borrowing Int, b: borrowing Int, c: borrowing @_accessScope(&a, &b) Pair) {}
// expected-cparser-error@-1:87{{expected ')' in '_accessScope' attribute}} expected-astgen-error@-1 {{invalid argument in '_accessScope' attribute}}

func labeledAccessScope(a: borrowing Int, b: borrowing @_accessScope(left: &a) Pair) {}
// expected-cparser-error@-1:74{{expected ')' in '_accessScope' attribute}} expected-astgen-error@-1 {{invalid argument in '_accessScope' attribute}}

func integerAccessScope(a: borrowing @_accessScope(&0) Pair) {}
// expected-cparser-error@-1:53{{expected identifier or 'self' in '_accessScope' attribute}} expected-astgen-error@-1 {{invalid argument in '_accessScope' attribute}}
