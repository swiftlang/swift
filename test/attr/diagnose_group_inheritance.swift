// RUN: %target-typecheck-verify-swift -Wwarning PerformanceHints

@diagnose(ReturnTypeImplicitCopy, as: error)
func foo() -> [Int] { // expected-error {{Performance: 'foo()' returns an array, leading to implicit copies. Consider using an 'inout' parameter instead.}}
    return [1,2,3]
}

func bar() -> [Int] { // expected-warning {{Performance: 'bar()' returns an array, leading to implicit copies. Consider using an 'inout' parameter instead.}}
    return [1,2,3]
}
