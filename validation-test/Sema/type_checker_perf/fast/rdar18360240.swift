// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: OS=macosx
// REQUIRES: asserts

let empty: [Int] = []
let _ = empty + empty + empty + empty + empty + empty + empty + empty + empty + empty + empty + empty
