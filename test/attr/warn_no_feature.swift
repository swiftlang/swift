// REQUIRES: swift_feature_SourceWarningControl
// RUN: %target-typecheck-verify-swift

@available(*, deprecated)
func bar() -> [Int] { return [1,2,3] }

// Ensure feature has no effect until enabled

@warn(DeprecatedDeclaration, as: error)
func foo() -> [Int] { 
    return bar() // expected-warning {{'bar()' is deprecated}}
}

@warn(DeprecatedDeclaration, as: ignored)
func baz() -> [Int] { 
    return bar() // expected-warning {{'bar()' is deprecated}}
}
