// RUN: %target-typecheck-verify-swift

struct Result {}

@_functionBuilder
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
  let totalSeconds = 42000
  test(totalseconds / 3600) // expected-error {{cannot find 'totalseconds' in scope}}
}

test_builder {
  test(doesntExist()) // expected-error {{cannot find 'doesntExist' in scope}}

  if let result = doesntExist() { // expected-error {{cannot find 'doesntExist' in scope}}
  }

  if bar = test(42) {} // expected-error {{cannot find 'bar' in scope}}

  let foo = bar() // expected-error {{cannot find 'bar' in scope}}

  switch (doesntExist()) { // expected-error {{cannot find 'doesntExist' in scope}}
  }
}
