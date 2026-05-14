// RUN: %target-typecheck-verify-swift

@available(*, deprecated)
func bar() -> [Int] { return [1,2,3] }

@diagnose(DeprecatedDeclaration, as: error)
func foo() -> [Int] { 
    return bar() // expected-error {{'bar()' is deprecated}}
}

@diagnose(DeprecatedDeclaration, as: ignored)
func baz() -> [Int] { 
    return bar()
}
