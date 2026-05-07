// REQUIRES: swift_feature_SourceWarningControl
// RUN: %target-typecheck-verify-swift -enable-experimental-feature SourceWarningControl -warnings-as-errors

@diagnose(PerformanceHints, as: warning)
func foo() -> [Int] { // expected-warning {{Performance: 'foo()' returns an array, leading to implicit copies. Consider using an 'inout' parameter instead.}}
    return [1,2,3]
}

@diagnose(ReturnTypeImplicitCopy, as: ignored)
func bar() -> [Int] {
    return [1,2,3]
}
