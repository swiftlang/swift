// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t/types.swift %t/valid.swift -enable-experimental-feature ScopeRestrictions -enable-experimental-feature ParserValidation
// RUN: %target-swift-frontend -typecheck -verify %t/types.swift %t/valid.swift -verify-additional-prefix off-
// RUN: %target-swift-frontend -typecheck -verify %t/types.swift %t/recovery.swift -enable-experimental-feature ScopeRestrictions
// RUN: %target-swift-frontend -typecheck -verify %t/types.swift %t/feature_off.swift

// REQUIRES: swift_feature_ScopeRestrictions
// REQUIRES: swift_feature_ParserValidation

//--- types.swift

struct S {}
struct Pair {}
struct Box<T> { var value: T }

//--- valid.swift

// MARK: - Scope specifier forms

func named(a: Int, b: @_scoped(a) S) {}
// expected-off-error@-1:24{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func accessScope(a: inout Int, b: @_scoped(&a) S) {}
// expected-off-error@-1:36{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

extension S {
  func selfAccessScope(b: @_scoped(&self) S) {}
  // expected-off-error@-1:28{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}
  func selfValueScope(b: @_scoped(self) S) {}
  // expected-off-error@-1:27{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}
}

func immortal(b: @_scoped(immortal) S) {}
// expected-off-error@-1:19{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

// A label is never a dependency, even when spelled 'immortal'.
func immortalLabel(a: Int, p: @_scoped(immortal: a) Pair) {}
// expected-off-error@-1:32{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func labelled(a: Int, b: Int, p: @_scoped(left: a, right: b) Pair) {}
// expected-off-error@-1:35{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}
func labelledAccessScopes(a: inout Int, b: inout Int,
                          p: @_scoped(left: &a, right: &b) Pair) {}
                          // expected-off-error@-1:31{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}
func labelledMixed(a: Int, b: inout Int, p: @_scoped(left: a, right: &b) Pair) {}
// expected-off-error@-1:46{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func unlabelledList(a: Int, b: Int, p: @_scoped(a, b) Pair) {}
// expected-off-error@-1:41{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

// MARK: - Positions

func resultPosition(a: Int) -> @_scoped(a) S { S() }
// expected-off-error@-1:33{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func genericArgumentPosition(a: Int, b: Box<@_scoped(a) S>) {}
// expected-off-error@-1:46{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func functionTypeParameterPosition(a: Int, body: (@_scoped(a) S) -> Void) {}
// expected-off-error@-1:52{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func functionTypeResultPosition(a: Int, body: () -> @_scoped(a) S) {}
// expected-off-error@-1:54{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func tuplePosition(a: Int, b: (@_scoped(a) S, Int)) {}
// expected-off-error@-1:33{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func variadicPosition(a: Int, b: @_scoped(a) S...) {}
// expected-off-error@-1:35{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func optionalSugarPosition(a: Int, b: (@_scoped(a) S)?) {}
// expected-off-error@-1:41{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

typealias ScopedAlias = @_scoped(immortal) S
// expected-off-error@-1:26{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

struct StoredProperties {
  var member: @_scoped(immortal) S
  // expected-off-error@-1:16{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}
  subscript(i: Int) -> @_scoped(immortal) S { S() }
  // expected-off-error@-1:25{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}
}

func localBinding() {
  let x: @_scoped(immortal) S = S()
  // expected-off-error@-1:11{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}
  _ = x
}

// MARK: - Composition with other attributes and specifiers

func withOwnershipInout(a: Int, b: inout @_scoped(a) S) {}
// expected-off-error@-1:43{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func withOwnershipBorrowing(a: Int, b: borrowing @_scoped(a) S) {}
// expected-off-error@-1:51{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func withOwnershipConsuming(a: Int, b: consuming @_scoped(a) S) {}
// expected-off-error@-1:51{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func withEscaping(a: Int, body: @escaping @_scoped(a) () -> Void) {}
// expected-off-error@-1:44{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func withSendable(a: Int, body: @_scoped(a) @Sendable () -> Void) {}
// expected-off-error@-1:34{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

// MARK: - Multiple uses

func twoIndependentUses(a: Int, b: @_scoped(a) S, c: @_scoped(a) Pair) {}
// expected-off-error@-1:37{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}
// expected-off-error@-2:55{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

// MARK: - Speculative parsing

func speculative(a: Int) {
  _ = Box<@_scoped(a) S>.self
  // expected-off-error@-1:12{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}
  _ = { (x: S) -> @_scoped(immortal) S in x }
  // expected-off-error@-1:20{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}
}

// MARK: - Escaped identifiers

extension S {
  func escapedSelf(_ `self`: Int, b: @_scoped(`self`) S) {}
  // expected-off-error@-1:39{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}
}

func escapedImmortal(_ `immortal`: Int, b: @_scoped(`immortal`) S) {}
// expected-off-error@-1:45{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func escapedImmortalAccess(_ `immortal`: inout Int, b: @_scoped(&`immortal`) S) {}
// expected-off-error@-1:57{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

func escapedKeyword(`default` d: Int, b: @_scoped(`default`) S) {}
// expected-off-error@-1:43{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

// Keywords would be escaped when used as name of member / scope for type, so require escaping here for simplicity (unlike function labels).
func escapedLabel(a: Int, p: @_scoped(`default`: a) Pair) {}
// expected-off-error@-1:31{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}

//--- feature_off.swift

func stillResolves(a: Int, b: @_scoped(a) S) {}
// expected-error@-1:32{{'@_scoped' attribute is only valid when experimental feature ScopeRestrictions is enabled}}
func useStillResolves() {
  let _: Int = stillResolves
  // expected-error@-1:16{{cannot convert value of type '(Int, S) -> ()' to specified type 'Int'}}
}

//--- recovery.swift

func speculativeMalformed(a: Int) {
  _ = Box<@_scoped(a,) S>.self
  // expected-error@-1:22{{unexpected ',' separator}}{{21-22=}}
  _ = Box<@_scoped(0) S>.self
  // expected-error@-1:20{{expected identifier or 'self' in '_scoped' attribute}}
}

func bareKeywordLabel(a: Int, p: @_scoped(default: a) Pair) {}
// expected-error@-1:43{{keyword 'default' cannot be used as an identifier here}}
// expected-note@-2:43{{if this name is unavoidable, use backticks to escape it}}{{43-50=`default`}}

func selfLabel(a: Int, p: @_scoped(self: a) Pair) {}
// expected-error@-1:36{{keyword 'self' cannot be used as an identifier here}}
// expected-note@-2:36{{if this name is unavoidable, use backticks to escape it}}{{36-40=`self`}}

func missingLParen(b: @_scoped S) {}
// expected-error@-1:32{{expected '(' in '_scoped' attribute}}

func emptySpecifierList(b: @_scoped() S) {}
// expected-error@-1:37{{expected identifier or 'self' in '_scoped' attribute}}

func ampersandWithoutName(b: @_scoped(&) S) {}
// expected-error@-1:39{{expected identifier or 'self' in '_scoped' attribute}}

func immortalAccess(b: @_scoped(&immortal) S) {}
// expected-error@-1:33{{cannot depend on the access scope of 'immortal'}}{{33-34=}}

// As in an inout argument, '&' must be attached to its operand.
func detachedAmpersand(a: inout Int, b: @_scoped(& a) S) {}
// expected-error@-1:50{{expected identifier or 'self' in '_scoped' attribute}}

func integerSpecifier(b: @_scoped(0) S) {}
// expected-error@-1:35{{expected identifier or 'self' in '_scoped' attribute}}

func dollarSpecifier(b: @_scoped($0) S) {}
// expected-error@-1:34{{expected identifier or 'self' in '_scoped' attribute}}

func capitalSelf(b: @_scoped(Self) S) {}
// expected-error@-1:30{{expected identifier or 'self' in '_scoped' attribute}}

// FIXME: probably deserves better recovery...
func memberSpecifier(a: Int, b: @_scoped(a.b) S) {}
// expected-error@-1:43{{expected ',' separator}}{{43-43=,}}
// expected-error@-2:43{{expected identifier or 'self' in '_scoped' attribute}}

// Indistinguishable from a missing ',', which is how it is recovered.
func missingRParen(b: @_scoped(immortal S) {}
// expected-error@-1:41{{expected ',' separator}}{{40-40=,}}
// expected-error@-2:44{{expected parameter type following ':'}}

func missingComma(a: Int, b: Int, p: @_scoped(left: a right: b) Pair) {}
// expected-error@-1:55{{expected ',' separator}}{{54-54=,}}

func labelWithoutSpecifier(b: @_scoped(left:) Pair) {}
// expected-error@-1:45{{expected identifier or 'self' in '_scoped' attribute}}

func leadingComma(a: Int, b: @_scoped(,a) S) {}
// expected-error@-1:39{{unexpected ',' separator}}{{39-40=}}

func trailingComma(a: Int, b: @_scoped(a,) S) {}
// expected-error@-1:42{{unexpected ',' separator}}{{41-42=}}

// A malformed '@_scoped' doesn't derail the attributes after it.
func followedByAttribute(body: @_scoped(0) @Sendable () -> Void) {}
// expected-error@-1:41{{expected identifier or 'self' in '_scoped' attribute}}

func repeatedAttribute(a: Int, b: @_scoped(a) @_scoped(a) S) {}
// expected-error@-1:47{{duplicate attribute}}{{47-59=}}
