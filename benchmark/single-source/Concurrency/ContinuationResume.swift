//===--- ContinuationResume.swift -----------------------------------------===//
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

// CheckedContinuation's init and resume run in the _Concurrency library;
// UnsafeContinuation is emitted into the client.

import TestsUtils

public let benchmarks = [
  BenchmarkInfo(name: "CheckedContinuation.Resume.Int",
                runFunction: run_CheckedContinuationResumeInt,
                tags: [.concurrency]),
  BenchmarkInfo(name: "CheckedContinuation.Resume.String",
                runFunction: run_CheckedContinuationResumeString,
                tags: [.concurrency],
                setUpFunction: { blackHole(strings) }),
  BenchmarkInfo(name: "CheckedContinuation.Throw",
                runFunction: run_CheckedContinuationThrow,
                tags: [.concurrency]),
  BenchmarkInfo(name: "UnsafeContinuation.Resume.String",
                runFunction: run_UnsafeContinuationResumeString,
                tags: [.concurrency],
                setUpFunction: { blackHole(strings) }),
]

private let count = 5_000

// Keep these longer than 15 UTF-8 bytes so that element copies retain.
private let strings: [String] = (0..<count).map {
  "continuation result number \($0)"
}

private struct ResumeError: Error {}

@inline(never)
private func run_CheckedContinuationResumeInt(_ n: Int) async {
  for _ in 0..<n {
    var sum = 0
    for i in 0..<count {
      sum &+= await withCheckedContinuation {
        (c: CheckedContinuation<Int, Never>) in
        c.resume(returning: i)
      }
    }
    check(sum == count * (count - 1) / 2)
  }
}

@inline(never)
private func run_CheckedContinuationResumeString(_ n: Int) async {
  let elements = strings
  var received = 0
  for _ in 0..<n {
    for element in elements {
      let result = await withCheckedContinuation {
        (c: CheckedContinuation<String, Never>) in
        c.resume(returning: element)
      }
      blackHole(result)
      received += 1
    }
  }
  check(received == n * count)
}

@inline(never)
private func run_CheckedContinuationThrow(_ n: Int) async {
  var caught = 0
  for _ in 0..<n {
    for _ in 0..<count {
      do {
        let result = try await withCheckedThrowingContinuation {
          (c: CheckedContinuation<Int, Error>) in
          c.resume(throwing: ResumeError())
        }
        blackHole(result)
      } catch {
        caught += 1
      }
    }
  }
  check(caught == n * count)
}

@inline(never)
private func run_UnsafeContinuationResumeString(_ n: Int) async {
  let elements = strings
  var received = 0
  for _ in 0..<n {
    for element in elements {
      let result = await withUnsafeContinuation {
        (c: UnsafeContinuation<String, Never>) in
        c.resume(returning: element)
      }
      blackHole(result)
      received += 1
    }
  }
  check(received == n * count)
}
