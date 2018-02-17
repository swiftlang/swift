// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

_ = [0,1,2,3].lazy.map { String($0)+"hi" }.sorted(by: { $0 > $1 })
// expected-error@-1 {{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}
