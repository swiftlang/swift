// rdar://79092863 (https://github.com/swiftlang/swift/issues/57077)
// RUN: %target-typecheck-verify-swift

protocol P {}

struct S1: P {}
struct S2: P {}

@resultBuilder
struct Builder {
  static func buildEither<T: P>(first: T) -> some P { first }
  static func buildEither<T: P>(second: T) -> some P { second }
  static func buildBlock<T: P>(_ x: T) -> some P { x }
}

enum E {
  case a
  case b

  @Builder
  var value: some P { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    switch self {
    case .a:
      S1()
    case .b:
      S2()
    }
  }

  @Builder
  var sameType: some P { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    if true {
      S1()
    } else {
      S1()
    }
  }
}
