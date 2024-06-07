// RUN: %target-swift-frontend -strict-concurrency=complete -emit-sil -o /dev/null %s -verify -disable-availability-checking
// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
struct S: AsyncSequence {
  typealias Element = Int
  struct AsyncIterator: AsyncIteratorProtocol {
    mutating func next() async -> Int? { nil }
  }

  func makeAsyncIterator() -> AsyncIterator { AsyncIterator() }
}

@available(SwiftStdlib 5.1, *)
struct TS: AsyncSequence {
  typealias Element = Int
  struct AsyncIterator: AsyncIteratorProtocol {
    mutating func next() async throws -> Int? { nil }
  }

  func makeAsyncIterator() -> AsyncIterator { AsyncIterator() }
}

@available(SwiftStdlib 5.1, *)
struct SpecificTS<F: Error>: AsyncSequence {
  typealias Element = Int
  typealias Failure = F
  struct AsyncIterator: AsyncIteratorProtocol {
    typealias Failure = F
    mutating func next() async throws -> Int? { nil }
  }

  func makeAsyncIterator() -> AsyncIterator { AsyncIterator() }
}

@available(SwiftStdlib 5.1, *)
struct GenericTS<Failure: Error>: AsyncSequence {
  typealias Element = Int
  struct AsyncIterator: AsyncIteratorProtocol {
    mutating func next() async throws -> Int? { nil }
  }

  func makeAsyncIterator() -> AsyncIterator { AsyncIterator() }
}

@available(SwiftStdlib 5.1, *)
struct SequenceAdapter<Base: AsyncSequence>: AsyncSequence {
  typealias Element = Base.Element

  struct AsyncIterator: AsyncIteratorProtocol {
    mutating func next() async rethrows -> Base.Element? { nil }
  }

  func makeAsyncIterator() -> AsyncIterator { AsyncIterator() }
}

public struct NormalThrowingAsyncSequence<Element, Failure>: AsyncSequence {
  private let iteratorMaker: () -> AsyncIterator

  public struct AsyncIterator: AsyncIteratorProtocol {
    let nextMaker: () async throws -> Element?
    public mutating func next() async throws -> Element? {
      try await nextMaker()
    }
  }

  public func makeAsyncIterator() -> AsyncIterator {
    iteratorMaker()
  }
}


enum MyError: Error {
case fail
}

@available(SwiftStdlib 5.1, *)
func testAssocTypeInference(sf: S.Failure, tsf: TS.Failure, gtsf1: GenericTS<MyError>.Failure, adapter: SequenceAdapter<SpecificTS<MyError>>.Failure, ntas: NormalThrowingAsyncSequence<String, MyError>.Failure) {
  let _: Int = sf // expected-error{{cannot convert value of type 'S.__AsyncSequence_Failure' (aka 'Never') to specified type 'Int'}}
  let _: Int = tsf // expected-error{{cannot convert value of type 'TS.__AsyncSequence_Failure' (aka 'any Error') to specified type 'Int'}}
  let _: Int = gtsf1 // expected-error{{cannot convert value of type 'GenericTS<MyError>.__AsyncSequence_Failure' (aka 'any Error') to specified type 'Int'}}
  let _: Int = adapter // expected-error{{cannot convert value of type 'SequenceAdapter<SpecificTS<MyError>>.__AsyncSequence_Failure' (aka 'MyError') to specified type 'Int'}}
  let _: Int = ntas // expected-error{{cannot convert value of type 'NormalThrowingAsyncSequence<String, MyError>.__AsyncSequence_Failure' (aka 'any Error') to specified type 'Int'}}
}


@available(SwiftStdlib 5.1, *)
func test(s: S) async {
  for await x in s { _ = x }
}

enum OtherError: Error {
case boom
}


@available(SwiftStdlib 5.1, *)
func testMyError(s: SpecificTS<MyError>, so: SpecificTS<OtherError>) async throws(MyError) {
  for try await x in s { _ = x }

  for try await x in so { _ = x }
  // expected-error@-1{{thrown expression type 'SpecificTS<OtherError>.AsyncIterator.Failure' (aka 'OtherError') cannot be converted to error type 'MyError'}}
}
