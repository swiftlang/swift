// rdar://192708018 (https://github.com/swiftlang/swift/issues/78813)
// RUN: %target-typecheck-verify-swift

protocol Parser<Input, Output> {
  associatedtype Input
  associatedtype Output
}

struct Leaf<Output>: Parser {
  typealias Input = Substring
}

struct Pair<P0: Parser, P1: Parser>: Parser {
  typealias Input = P0.Input
  typealias Output = (P0.Output, P1.Output)
}

@resultBuilder
enum ParserBuilder {
  static func buildBlock<P0: Parser, P1: Parser>(_ p0: P0, _ p1: P1) -> Pair<P0, P1> { fatalError() }
}

struct A {}
struct B {}
struct C {}

struct S {
  @ParserBuilder
  var body: some Parser<Substring, (A, B)> { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    Leaf<A>()
    Leaf<C>()
  }
}
