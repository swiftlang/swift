// RUN: %target-run-simple-swift(-target %target-swift-5.1-abi-triple -parse-as-library)
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

struct Failure: Error, Equatable { var value = 1 }

extension Sequence {
  var async: AsyncLazySequence<Self> { AsyncLazySequence(self) }
}
public struct AsyncLazySequence<S: Sequence>: AsyncSequence {
  public typealias Element = S.Element
  public struct Iterator: AsyncIteratorProtocol {
    var it: S.Iterator
    init(_ i: S.Iterator) { it = i }
    public mutating func next() async -> S.Element? { it.next() }
  }
  let s: S
  init(_ x: S) { s = x }
  public func makeAsyncIterator() -> Iterator { Iterator(s.makeIterator()) }
}
extension AsyncSequence where Element: Equatable {
  func failOn(_ error: Error, at x: Element) -> AsyncThrowingMapSequence<Self, Element> {
    map { (v: Element) throws -> Element in if v == x { throw error }; return v }
  }
  func containsByDraining(_ search: Element) async rethrows -> Bool {
    for try await e in self { if e == search { return true } }
    return false
  }
}

@main struct Main {
  static func main() async {
    var threw = false
    do {
      _ = try await [1, 2, 3].async.failOn(Failure(value: 42), at: 2)
                                   .containsByDraining(99)
    } catch {
      threw = (error as? Failure) == Failure(value: 42)
    }
    // CHECK: propagated: true
    print("propagated: \(threw)")
  }
}
