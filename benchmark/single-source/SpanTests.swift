//===--- SpanTests.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

let t: [BenchmarkCategory] = [.validation, .api, .algorithm]

public let benchmarks = [
  BenchmarkInfo(
    name: "SpanSumIterateToCount",
    runFunction: run_SpanSumIterateToCount,
    tags: t),
  BenchmarkInfo(
    name: "SpanSumIterateToUnknown",
    runFunction: run_SpanSumIterateToUnknown,
    tags: t),
  BenchmarkInfo(
    name: "SpanSumIterateWithPrecondition",
    runFunction: run_SpanSumIterateWithPrecondition,
    tags: t),
  BenchmarkInfo(
    name: "SpanIterateOverIndices",
    runFunction: run_SpanIterateOverIndices,
    tags: t),
  BenchmarkInfo(
    name: "SpanSearch",
    runFunction: run_SpanSearch,
    tags: t),
  BenchmarkInfo(
    name: "SpanBinarySearch",
    runFunction: run_SpanBinarySearch,
    tags: t),
  BenchmarkInfo(
    name: "MutableSpanSumOverIndices",
    runFunction: run_MutableSpanSumOverIndices,
    tags: t),
  BenchmarkInfo(
    name: "MutableSpanSumUnknownBound",
    runFunction: run_MutableSpanSumUnknownBound,
    tags: t),
  BenchmarkInfo(
    name: "MutableSpanZeroInit",
    runFunction: run_MutableSpanZeroInit,
    tags: t),
  BenchmarkInfo(
    name: "MutableSpanCopyElements",
    runFunction: run_MutableSpanCopyElements,
    tags: t),
  BenchmarkInfo(
    name: "MutableSpanVectorSum",
    runFunction: run_MutableSpanVectorSum,
    tags: t),
  BenchmarkInfo(
    name: "MutableSpanBubbleSort",
    runFunction: run_MutableSpanBubbleSort,
    tags: t),
]

let spanSize = 10000

var randomArray: [Int] = {
  var g = SplitMix64(seed: 42)
  return (0..<spanSize).map { _ in Int.random(in: 0..<1000, using: &g) }
}()

let sortedArray: [Int] = {
  return randomArray.sorted()
}()

var outputArray: [Int] = {
  return Array(repeating: 0, count: spanSize)
}()

@inline(never)
func getUnknownBound(_ span: Span<Int>) -> Int {
  return span.count
}

@inline(never)
func getUnknownMutableSpanBound(_ span: borrowing MutableSpan<Int>) -> Int {
  return span.count
}

@inline(never)
func getSearchTarget(_ span: Span<Int>) -> Int {
  return span.count / 2
}

func isSorted(_ span: borrowing MutableSpan<Int>) -> Bool {
  if span.count <= 1 {
    return true
  }
  for i in 0..<span.count - 1 {
    if span[i] > span[i + 1] {
      return false
    }
  }
  return true
}

@inline(never)
public func run_SpanSumIterateToCount(_ n: Int) {
  if #available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *) {
    let span = randomArray.span
    for _ in 0..<10_000 {
      var sum = 0
      for i in 0..<span.count {
        sum &+= span[i]
      }
      blackHole(sum)
    }
  }
}

@inline(never)
public func run_SpanSumIterateToUnknown(_ n: Int) {
  if #available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *) {
    let span = randomArray.span
    let bound = getUnknownBound(span)
    for _ in 0..<10_000 {
      var sum = 0
      for i in 0..<bound {
        sum &+= span[i]
      }
      blackHole(sum)
    }
  }
}

@inline(never)
public func run_SpanSumIterateWithPrecondition(_ n: Int) {
  if #available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *) {
    let span = randomArray.span
    let bound = getUnknownBound(span)
    for _ in 0..<10_000 {
      var sum = 0
      precondition(bound <= span.count)
      for i in 0..<bound {
        sum &+= span[i]
      }
      blackHole(sum)
    }
  }
}

@inline(never)
public func run_SpanIterateOverIndices(_ n: Int) {
  if #available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *) {
    let span = randomArray.span
    for _ in 0..<10_000 {
      var sum = 0
      for i in span.indices {
        sum &+= span[i]
      }
      blackHole(sum)
    }
  }
}

@inline(never)
public func run_SpanSearch(_ n: Int) {
  if #available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *) {
    let span = randomArray.span
    let target = getSearchTarget(span)
    for _ in 0..<10_000 {
      var found: Int? = nil
      for i in span.indices {
        if span[i] == target {
          found = i
          break
        }
      }
      blackHole(found)
    }
  }
}

@inline(never)
public func run_SpanBinarySearch(_ n: Int) {
  if #available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *) {
    let span = sortedArray.span
    let target = getSearchTarget(span)
    for _ in 0..<10_000 {
      var result: Int? = nil
      
      // Handle edge cases for empty or single-element spans
      if span.count == 0 {
        result = nil
      } else if span.count == 1 {
        result = span[0] == target ? 0 : nil
      } else {
        var low = 0
        var high = span.count - 1
        
        while low <= high {
          let mid = low + (high - low) / 2

          if span[mid] == target {
            result = mid
            break
          } else if span[mid] < target {
            low = mid + 1
          } else {
            high = mid - 1
          }
        }
      }
      blackHole(result)
    }
  }
}

@inline(never)
public func run_MutableSpanSumOverIndices(_ n: Int) {
  if #available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *) {
    let span = randomArray.mutableSpan
    for _ in 0..<10_000 {
      var sum = 0
      for i in span.indices {
        sum &+= span[i]
      }
      blackHole(sum)
    }
  }
}

@inline(never)
public func run_MutableSpanSumUnknownBound(_ n: Int) {
  if #available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *) {
    let span = randomArray.mutableSpan
    let bound = getUnknownMutableSpanBound(span)
    for _ in 0..<10_000 {
      var sum = 0
      for i in 0..<bound {
        sum &+= span[i]
      }
      blackHole(sum)
    }
  }
}

@inline(never)
public func run_MutableSpanZeroInit(_ n: Int) {
  if #available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *) {
    var span = outputArray.mutableSpan
    for _ in 0..<10_000 {
      for i in span.indices {
        span[i] = 0
      }
    }
  }
}

@inline(never)
public func run_MutableSpanCopyElements(_ n: Int) {
  if #available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *) {
    let inputSpan = randomArray.span
    var outputSpan = outputArray.mutableSpan
    for _ in 0..<10_000 {
      for i in inputSpan.indices {
        outputSpan[i] = inputSpan[i]
      }
    }
  }
}

@inline(never)
public func run_MutableSpanVectorSum(_ n: Int) {
  if #available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *) {
    let input1 = randomArray.span
    let input2 = randomArray.span
    var output = outputArray.mutableSpan
    for _ in 0..<10_000 {
      for i in input1.indices {
        output[i] = input1[i] &+ input2[i]
      }
    }
  }
}

@inline(never)
public func run_MutableSpanBubbleSort(_ n: Int) {
  if #available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *) {
    var span = randomArray.mutableSpan
    if span.count > 1 {
      for i in 0..<span.count - 1 {
        for j in 0..<span.count - i - 1 {
          if span[j] > span[j + 1] {
            let tmp = span[j]
            span[j] = span[j + 1]
            span[j + 1] = tmp
          }
        }
      }
    }
    check(isSorted(span))
  }
}
