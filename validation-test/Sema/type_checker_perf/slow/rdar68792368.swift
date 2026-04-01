// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000

// Invalid expression -- missing function return type

func sum(from a:Int, to b:Int) {
    return b*(b+1)/2 - (a-1)*a/2
    // expected-error@-1 {{reasonable time}}
}
