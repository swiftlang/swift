// REQUIRES: swift_feature_SourceWarningControl
// RUN: %target-typecheck-verify-swift -enable-experimental-feature SourceWarningControl

@available(*, deprecated)
func bar() -> [Int] { return [1,2,3] }

@warn(DeprecatedDeclaration, as: error)
func foo() -> [Int] { 
    return bar() // expected-error {{'bar()' is deprecated}}
}

@warn(DeprecatedDeclaration, as: ignored)
func baz() -> [Int] { 
    return bar()
}
