// RUN: %target-typecheck-verify-swift

class NonAdditiveClass {}

func test_non_additive_class_addition() -> NonAdditiveClass {
  let a = NonAdditiveClass()
  let b = NonAdditiveClass()
  return a + b
  // expected-error@-1 {{binary operator '+' cannot be applied to two 'NonAdditiveClass' operands}}
}
