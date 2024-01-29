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

enum MyError: Error {
case fail
}

@available(SwiftStdlib 5.1, *)
func testAssocTypeInference(sf: S.Failure, tsf: TS.Failure, gtsf1: GenericTS<MyError>.Failure, adapter: SequenceAdapter<GenericTS<MyError>>.Failure) {
  let _: Int = sf // expected-error{{cannot convert value of type 'S.Failure' (aka 'Never') to specified type 'Int'}}
  let _: Int = tsf // expected-error{{cannot convert value of type 'TS.Failure' (aka 'any Error') to specified type 'Int'}}
  let _: Int = gtsf1 // expected-error{{cannot convert value of type 'GenericTS<MyError>.Failure' (aka 'MyError') to specified type 'Int'}}
  let _: Int = adapter // expected-error{{cannot convert value of type 'SequenceAdapter<GenericTS<MyError>>.Failure' (aka 'MyError') to specified type 'Int'}}
}


@available(SwiftStdlib 5.1, *)
func test(s: S) async {
  for await x in s { _ = x }
}

enum OtherError: Error {
case boom
}


@available(SwiftStdlib 5.1, *)
func testMyError(s: GenericTS<MyError>, so: GenericTS<OtherError>) async throws(MyError) {
  for try await x in s { _ = x }

  for try await x in so { _ = x }
  // expected-error@-1{{thrown expression type 'OtherError' cannot be converted to error type 'MyError'}}
}
