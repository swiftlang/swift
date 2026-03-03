// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10

// Note: the scope threshold is intentionally set low so that the expression will fail.
//
// The purpose of the test is to ensure the diagnostic points at the second statement in
// the closure, and not the closure itself.
// 
// If the expression becomes very fast and we manage to type check it with fewer than
// 10 scopes, please *do not* remove the expected error! Instead, make the expression
// more complex again.

let s = ""
let n = 0

let closure = {
  let _ = 0
  let _ = "" + s + "" + s + "" + s + "" + n + "" // expected-error {{reasonable time}}
  let _ = 0
}
