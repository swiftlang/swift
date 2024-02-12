// RUN: %target-typecheck-verify-swift

struct Result {}

@resultBuilder
struct Builder { // expected-note 4 {{add 'buildOptional(_:)' to the result builder 'Builder' to add support for 'if' statements without an 'else'}}
  static func buildBlock() -> Result {
    Result()
  }
}

func test_builder<T>(@Builder _: () -> T) {}
func test_builder(@Builder _: () -> Int) {}

test_builder { // expected-error {{no exact matches in call to global function 'test_builder'}}
  let _ = 0

  // expected-error@+2 {{cannot find 'does_not_exist' in scope}}
  // expected-note@+1 2 {{closure containing control flow statement cannot be used with result builder 'Builder'}}
  if let x = does_not_exist {
  }
}

func test(_: Int) -> Bool {
  return false
}

test_builder {
  let totalSeconds = 42000 // expected-note {{'totalSeconds' declared here}}
  test(totalseconds / 3600) // expected-error {{cannot find 'totalseconds' in scope; did you mean 'totalSeconds'?}}
}

test_builder { // expected-error {{no exact matches in call to global function 'test_builder'}}
  test(doesntExist()) // expected-error {{cannot find 'doesntExist' in scope}}

  // expected-note@+1 2 {{closure containing control flow statement cannot be used with result builder 'Builder'}}
  if let result = doesntExist() { // expected-error {{cannot find 'doesntExist' in scope}}
  }

  if bar = test(42) {} // expected-error {{cannot find 'bar' in scope}}

  let foo = bar() // expected-error {{cannot find 'bar' in scope}}

  switch (doesntExist()) { // expected-error {{cannot find 'doesntExist' in scope}}
  }
}
