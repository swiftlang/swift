// RUN: %target-typecheck-verify-swift -verify-additional-prefix noerror-
// RUN: %target-typecheck-verify-swift -verify-additional-prefix werror- -Werror UselessConditionalStatement

func testUselessIfCondition(_ x: Int) {
  // expected-noerror-warning@+2 {{'if' condition is always true}}{{none}}
  // expected-werror-error@+1 {{'if' condition is always true}}{{none}}
  if case _ = x {
    _ = 0
  } else {
    _ = 1
  }
}

func testUselessWhile(_ x: Int) {
  // expected-noerror-warning@+2 {{'while' condition is always true}}{{none}}
  // expected-werror-error@+1 {{'while' condition is always true}}{{none}}
  while case _ = x {
    _ = 0
    break
  }
  _ = 1
}

func testUselessGuard(_ x: Int) {
  // expected-noerror-warning@+2 {{'guard' condition is always true, body is unreachable}}{{none}}
  // expected-werror-error@+1 {{'guard' condition is always true, body is unreachable}}{{none}}
  guard case _ = x else {
    _ = 0
    return
  }
  _ = 1
}
