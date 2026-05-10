// REQUIRES: swift_feature_SourceWarningControl
// RUN: %target-typecheck-verify-swift -enable-experimental-feature SourceWarningControl

@diagnose(DeprecatedDeclaration, as: error)
func foo() {}

@diagnose(Deprecate, as: warning) // expected-warning {{the diagnostic group identifier 'Deprecate' is unknown}}
func bar() {}

@diagnose(DeprecatedDeclaration, as: what) // expected-error {{expected diagnostic behavior argument 'what' to be either 'error', 'warning' or 'ignored'}}
func baz() {}

// expected-warning @+1 {{the diagnostic group identifier 'Hmm' is unknown}}
@diagnose(Hmm, as: what) // expected-error {{expected diagnostic behavior argument 'what' to be either 'error', 'warning' or 'ignored'}}
func qux() {}

@diagnose(ExistentialAny, as: ignored, reason: "for a lit test")
func quux() {}

@diagnose(ExistentialAny, as: ignored, reason: BadReason) // expected-error {{expected string literal in 'diagnose' attribute}}
func corge() {}

// Backward compatibility: @warn is an alias for @diagnose
@warn(DeprecatedDeclaration, as: error) // expected-warning {{'@warn' has been renamed to '@diagnose'}}
func warnAlias() {}

@warn(ExistentialAny, as: ignored, reason: "backward compat test") // expected-warning {{'@warn' has been renamed to '@diagnose'}}
func warnAliasWithReason() {}

@warn(ExistentialAny, as: ignored, reason: BadReason) // expected-warning {{'@warn' has been renamed to '@diagnose'}} expected-error {{expected string literal in 'warn' attribute}}
func warnAliasCorge() {}
