//===--- AsyncStreamYield.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// The stream storage runs in the _Concurrency library, unspecialized for the
// client's element type.

import TestsUtils

public let benchmarks = [
  BenchmarkInfo(name: "AsyncStream.Buffered.Int",
                runFunction: run_AsyncStreamBufferedInt,
                tags: [.concurrency]),
  BenchmarkInfo(name: "AsyncStream.Buffered.String",
                runFunction: run_AsyncStreamBufferedString,
                tags: [.concurrency],
                setUpFunction: { blackHole(strings) }),
  BenchmarkInfo(name: "AsyncThrowingStream.Buffered.String",
                runFunction: run_AsyncThrowingStreamBufferedString,
                tags: [.concurrency],
                setUpFunction: { blackHole(strings) }),
  BenchmarkInfo(name: "AsyncStream.MapFilter.String",
                runFunction: run_AsyncStreamMapFilterString,
                tags: [.concurrency],
                setUpFunction: { blackHole(strings) }),
]

private let count = 1_000

// Keep these longer than 15 UTF-8 bytes so that element copies retain.
private let strings: [String] = (0..<count).map {
  "async stream element number \($0)"
}

private struct StreamEnd: Error {}

@inline(never)
private func run_AsyncStreamBufferedInt(_ n: Int) async {
  for _ in 0..<n {
    let (stream, continuation) = AsyncStream.makeStream(of: Int.self)
    for i in 0..<count {
      continuation.yield(i)
    }
    continuation.finish()
    var sum = 0
    for await value in stream {
      sum &+= value
    }
    check(sum == count * (count - 1) / 2)
  }
}

@inline(never)
private func run_AsyncStreamBufferedString(_ n: Int) async {
  let elements = strings
  for _ in 0..<n {
    let (stream, continuation) = AsyncStream.makeStream(of: String.self)
    for element in elements {
      continuation.yield(element)
    }
    continuation.finish()
    var received = 0
    for await element in stream {
      blackHole(element)
      received += 1
    }
    check(received == count)
  }
}

@inline(never)
private func run_AsyncThrowingStreamBufferedString(_ n: Int) async {
  let elements = strings
  for _ in 0..<n {
    let (stream, continuation) =
      AsyncThrowingStream.makeStream(of: String.self)
    for element in elements {
      continuation.yield(element)
    }
    continuation.finish(throwing: StreamEnd())
    var received = 0
    do {
      for try await element in stream {
        blackHole(element)
        received += 1
      }
      check(false)
    } catch {
      check(error is StreamEnd)
    }
    check(received == count)
  }
}

@inline(never)
private func run_AsyncStreamMapFilterString(_ n: Int) async {
  let elements = strings
  for _ in 0..<n {
    let (stream, continuation) = AsyncStream.makeStream(of: String.self)
    for element in elements {
      continuation.yield(element)
    }
    continuation.finish()
    var received = 0
    for await element in stream.map({ $0 }).filter({ !$0.isEmpty }) {
      blackHole(element)
      received += 1
    }
    check(received == count)
  }
}
