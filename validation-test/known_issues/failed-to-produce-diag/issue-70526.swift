// rdar://119842048 (https://github.com/swiftlang/swift/issues/70526)
// RUN: %target-typecheck-verify-swift

protocol Parser<Output> {
  associatedtype Output
}

@resultBuilder
enum ParserBuilder {
  static func buildPartialBlock<P: Parser>(first: P) -> P { first }
  static func buildPartialBlock<P0: Parser, P1: Parser>(
    accumulated: P0, next: P1
  ) -> Take<P0.Output, P1.Output> {
    fatalError()
  }
}

struct Take<O0, O1>: Parser {
  typealias Output = (O0, O1)
}

struct Leaf<Output>: Parser {}

struct Many<Element: Parser, Result>: Parser {
  typealias Output = Result

  init<I: IteratorProtocol>(
    into initialResult: Result,
    _ updateAccumulatingResult: (inout Result, Element.Output) -> Void,
    decumulator: (Result) -> I,
    @ParserBuilder element: () -> Element
  ) where I.Element == Element.Output {}
}

func test() -> some Parser<[String: Int]> {
  Many(into: [String: Int]()) { object, pair in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    let (name, value) = pair
    object[name] = value
  } decumulator: { object in
    (object.sorted(by: { $0.key < $1.key }) as [(String, Int)])
      .reversed()
      .makeIterator()
  } element: {
    Leaf<String>()
    Leaf<Int>()
  }
}
