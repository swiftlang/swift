// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/55293

@resultBuilder
struct Builder {
  static func buildBlock<T>(_ value: T) -> T { value }

  static func buildEither<T, F>(first: T) -> Either<T, F> { .first(first) }

  static func buildEither<T, F>(second: F) -> Either<T, F> { .second(second) }
}

enum Either<T, F> {
  case first(T)
  case second(F)
}

struct A {}
struct B {}

func build<Content>(@Builder _ content: () -> Content) -> Content {
  content()
}

func testReturnStatementsInResultBuilderClosure() {
  let _ = build {
    if Bool.random() {
      return A() // expected-error {{cannot use explicit 'return' statement in the body of result builder 'Builder'}}
      // expected-note@-1 {{remove 'return' statements to apply the result builder}}
    } else {
      return B()
    }
  }
}
