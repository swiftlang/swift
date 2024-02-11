// RUN: %target-typecheck-verify-swift -enable-experimental-associated-type-inference
// RUN: %target-typecheck-verify-swift -disable-experimental-associated-type-inference

struct G<T> {
  class Nested {}
}

extension G.Nested: Sequence {
  func makeIterator() -> AnyIterator<String> { fatalError() }
}

extension G: LazySequenceProtocol, IteratorProtocol {
  mutating func next() -> Int? { fatalError() }
}

let c: G<Float>.Type = G<Float>.Iterator.self
let n: Int.Type = G<Float>.Element.self
let d: AnyIterator<String>.Type = G<Float>.Nested.Iterator.self
let s: String.Type = G<Float>.Nested.Element.self
