// RUN: %target-swift-frontend -typecheck %s -verify

// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
struct MyAsyncSequence<Element>: AsyncSequence {
  struct AsyncIterator: AsyncIteratorProtocol {
    mutating func next() -> Element? { nil }
  }

  func makeAsyncIterator() -> AsyncIterator { .init() }
}

@available(SwiftStdlib 5.1, *)
func testMe(ms: MyAsyncSequence<String>) {
  let flatMS = ms.flatMap { string in
    return MyAsyncSequence<[Character]>()
  }

  let _: AsyncFlatMapSequence<MyAsyncSequence<String>, MyAsyncSequence<[Character]>> = flatMS
}
