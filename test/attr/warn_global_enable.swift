// REQUIRES: swift_feature_SourceWarningControl
// RUN: %target-typecheck-verify-swift -enable-experimental-feature SourceWarningControl

@warn(PerformanceHints, as: error)
func foo() -> [Int] { // expected-error {{Performance: 'foo()' returns an array, leading to implicit copies. Consider using an 'inout' parameter instead.}}
    return [1,2,3]
}

@warn(ReturnTypeImplicitCopy, as: warning)
func bar() -> [Int] { // expected-warning {{Performance: 'bar()' returns an array, leading to implicit copies. Consider using an 'inout' parameter instead.}}
    return [1,2,3]
}
