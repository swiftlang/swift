// rdar://165791585
// RUN: %target-typecheck-verify-swift

struct Comment: ExpressibleByStringLiteral {
  init(stringLiteral: String) {}
}

func require<T>(
  _ optionalValue: T?,
  _ comment: @autoclosure () -> Comment? = nil
) throws -> T { fatalError() }

func require<T>(
  _ optionalValue: T,
  _ comment: @autoclosure () -> Comment? = nil
) throws -> T { fatalError() }

struct Baz {}
struct Bar {
  func baz() -> Baz { Baz() }
}

func test(bar: Bar?) throws {
  _ = try require(bar?.baz())

  _ = try require("", bar?.baz())
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
