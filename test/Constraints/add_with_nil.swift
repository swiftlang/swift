// RUN: %target-typecheck-verify-swift

func test_int_plus_nil(_ x: Int) -> Int {
  return x + nil
  // expected-error@-1 {{'nil' is not compatible with expected argument type 'Int'}}
}

func test_nil_plus_nil() -> Any? {
  return nil + nil
  // expected-error@-1 {{binary operator '+' cannot be applied to two operands with no exact type matches}}
}
