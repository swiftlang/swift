// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify

// REQUIRES: concurrency

// Tests for the use of 'rethrows' on generic functions that have AsyncSequence
// and AsyncIteratorProtocol requirements.

func f1(_ seq: some AsyncSequence) async rethrows {
  for try await _ in seq { }
}

func f2(_ seq: some AsyncSequence, body: () throws -> Void) async rethrows {
  for try await _ in seq {
    try body()
  }
}

func f3(_ seq: some AsyncSequence, _ seq2: some AsyncSequence) async rethrows {
  for try await _ in seq {
  }

  for try await _ in seq2 {
  }
}

enum HomeworkError: Error {
case dogAteIt
}

func testCalls(x: some AsyncSequence<Int, Never>, y: any AsyncSequence<Int, any Error>) async throws {
  await f1(x)
  try await f1(y)

  await f2(x) { print("Hello") }
  try await f2(y) { print("Hello") }

  try await f2(x) { throw HomeworkError.dogAteIt }
  try await f2(y) { throw HomeworkError.dogAteIt }

  await f3(x, x)
  try await f3(x, y)
}
