// rdar://147051650 (https://github.com/swiftlang/swift/issues/80019)
// RUN: %target-typecheck-verify-swift

@resultBuilder
struct Builder {
  static func buildBlock<T>(_ x: T) -> T { x }
  static func buildEither<T>(first x: T) -> T { x }
  static func buildEither<T>(second x: T) -> T { x }
}

struct Foo {
  var bar: Int = 0
}

func build<T>(@Builder _ body: () -> T) {}

func test(foo: Foo) {
  build { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    switch foo {
    case .bar: 0
    }
  }
}
