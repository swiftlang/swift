// REQUIRES: swift_feature_SourceWarningControl
// RUN: %target-typecheck-verify-swift -enable-experimental-feature SourceWarningControl

@warn(DeprecatedDeclaration, as: error)
func foo() {}

@warn(Deprecate, as: warning) // expected-error {{expected 'Deprecate' option to be a diagnostic group identifier}}
func bar() {}

@warn(DeprecatedDeclaration, as: what) // expected-error {{expected diagnostic behavior argument 'what' to be either 'error', 'warning' or 'ignored'}}
func baz() {}

// expected-error @+1 {{expected 'Hmm' option to be a diagnostic group identifier}}
@warn(Hmm, as: what) // expected-error {{expected diagnostic behavior argument 'what' to be either 'error', 'warning' or 'ignored'}}
func qux() {}

@warn(ExistentialAny, as: ignored, reason: "for a lit test")
func quux() {}

@warn(ExistentialAny, as: ignored, reason: BadReason) // expected-error {{expected string literal in 'warn' attribute}}
func corge() {}
