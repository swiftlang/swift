// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

_ = [0,1,2,3].lazy.map { String($0)+"hi" }.sorted(by: { $0 > $1 && $1 < $0 && ($1 + $0) < 1000 })
// expected-error@-1 {{reasonable time}}
