// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

let _: (Character) -> Bool = { c in
  ("a" <= c && c <= "z") || ("A" <= c && c <= "Z") || c == "_"
}
