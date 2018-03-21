// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

let _: (Character) -> Bool = { c in
  // expected-error@-1 {{reasonable time}}
  ("a" <= c && c <= "z") || ("A" <= c && c <= "Z") || c == "_"
}
