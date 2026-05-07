// RUN: %target-typecheck-verify-swift 

@available(*, deprecated)
func bar() -> [Int] { return [1,2,3] }

// Ensure feature SourceWarningControl has no effect until enabled

@diagnose(DeprecatedDeclaration, as: error)
func foo() -> [Int] { 
    return bar() // expected-warning {{'bar()' is deprecated}}
}

@diagnose(DeprecatedDeclaration, as: ignored)
func baz() -> [Int] { 
    return bar() // expected-warning {{'bar()' is deprecated}}
}
