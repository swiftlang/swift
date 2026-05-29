// RUN: %target-typecheck-verify-swift

func testUselessIfCondition(_ x: Int) {
  // expected-warning@+1 {{'if' condition is always true}}{{group-name=UselessConditionalStatement}}{{none}}
  if case _ = x {
    _ = 0
  } else {
    _ = 1
  }
}

func testUselessWhile(_ x: Int) {
  // expected-warning@+1 {{'while' condition is always true}}{{group-name=UselessConditionalStatement}}{{none}}
  while case _ = x {
    _ = 0
    break
  }
  _ = 1
}

func testUselessGuard(_ x: Int) {
  // expected-warning@+1 {{'guard' condition is always true, body is unreachable}}{{none}}
  guard case _ = x else {
    _ = 0
    return
  }
  _ = 1
}
