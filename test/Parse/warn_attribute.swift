// REQUIRES: swift_feature_SourceWarningControl
// RUN: %target-typecheck-verify-swift -enable-experimental-feature SourceWarningControl

@warn(DeprecatedDeclaration, as: error)
func foo() {}

@warn(Deprecate, as: warning) // expected-warning {{the diagnostic group identifier 'Deprecate' is unknown}}
func bar() {}

@warn(DeprecatedDeclaration, as: what) // expected-error {{expected diagnostic behavior argument 'what' to be either 'error', 'warning' or 'ignored'}}
func baz() {}

// expected-warning @+1 {{the diagnostic group identifier 'Hmm' is unknown}}
@warn(Hmm, as: what) // expected-error {{expected diagnostic behavior argument 'what' to be either 'error', 'warning' or 'ignored'}}
func qux() {}

@warn(ExistentialAny, as: ignored, reason: "for a lit test")
func quux() {}

@warn(ExistentialAny, as: ignored, reason: BadReason) // expected-error {{expected string literal in 'warn' attribute}}
func corge() {}
