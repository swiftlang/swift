// RUN: %target-typecheck-verify-swift

struct Result {}

@resultBuilder
struct Builder {
  static func buildBlock() -> Result {
    Result()
  }
}

func test_builder<T>(@Builder _: () -> T) {}
func test_builder(@Builder _: () -> Int) {}

test_builder {
  let _ = 0

  if let x = does_not_exist { // expected-error {{cannot find 'does_not_exist' in scope}}
  }
}

func test(_: Int) -> Bool {
  return false
}

test_builder {
  let totalSeconds = 42000 // expected-note {{'totalSeconds' declared here}}
  test(totalseconds / 3600) // expected-error {{cannot find 'totalseconds' in scope; did you mean 'totalSeconds'?}}
}

test_builder {
  test(doesntExist()) // expected-error {{cannot find 'doesntExist' in scope}}

  if let result = doesntExist() {
  }

  if bar = test(42) {}

  let foo = bar()

  switch (doesntExist()) {
  }
}
