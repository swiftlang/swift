//===--- BucketSort.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// This benchmark demonstrates the benefits of ExistentialSpecializer
// optimization pass. It's a generic version of the classic BucketSort algorithm
// adapted from an original implementation from the Swift Algorithm Club.
// See https://en.wikipedia.org/wiki/Bucket_sort and
// https://github.com/raywenderlich/swift-algorithm-club/tree/master/Bucket%20Sort
//
// It sorts an array of generic `SortableItem`s. If the type of `sortingAlgo`
// is not known to the call site at line 89, the `sort` method can not be
// specialized to integer array sorting, which will lead to a huge performance
// loss. Since `SortingAlgorithm` and `InsertionSort` are declared to be
// `public` and the lines 88-90 can not be inlined in `bucketSort` (due to
// inlining heuristic limitations), compiler without ExistentialSpecializer
// optimization can not achieve this feat. With ExistentialSpecializer which
// enables generic specialization recursively in a call chain, we're able to
// specialize line 89 for `InsertionSort` on integers.

import TestsUtils

public let BucketSort = BenchmarkInfo(
  name: "BucketSort",
  runFunction: run_BucketSort,
  tags: [.validation, .algorithm],
  setUpFunction: { blackHole(buckets) }
)

public protocol IntegerConvertible {
    func convertToInt() -> Int
}

extension Int: IntegerConvertible, SortableItem {
    public func convertToInt() -> Int {
        return self
    }
}

public protocol SortableItem: IntegerConvertible, Comparable { }

public protocol SortingAlgorithm {
    func sort<T: SortableItem>(_ items: [T]) -> [T]
}

public struct InsertionSort: SortingAlgorithm {
    public func sort<T: SortableItem>(_ items: [T]) -> [T] {
        var sortedItems = items
        for i in 0 ..< sortedItems.count {
            var j = i
            while j > 0 && sortedItems[j-1] > sortedItems[j] {
                let temp = sortedItems[j-1]
                sortedItems[j-1] = sortedItems[j]
                sortedItems[j] = temp
                j -= 1
            }
        }
        return sortedItems
    }
}

func distribute<T>(_ item: T, bucketArray: inout [Bucket<T>]) {
  let val = item.convertToInt()
  let capacity = bucketArray.first!.capacity
  let index = val / capacity
  bucketArray[index].add(item)
}

struct Bucket<T: SortableItem> {
  var items: [T]
  let capacity: Int
  init(capacity: Int) {
    self.capacity = capacity
    items = [T]()
  }
  mutating func add(_ item: T) {
    if items.count < capacity {
      items.append(item)
    }
  }
  func sort(_ sortingAlgo: SortingAlgorithm) -> [T] {
    return sortingAlgo.sort(items)
  }
}

func bucketSort<T>(
  _ items: [T], sortingAlgorithm: SortingAlgorithm, bucketArray: [Bucket<T>]
) -> [T] {
  var copyBucketArray = bucketArray
  for item in items {
    distribute(item, bucketArray: &copyBucketArray)
  }
  var sortedArray = [T]()
  for bucket in copyBucketArray {
    sortedArray += bucket.sort(sortingAlgorithm)
  }
  return sortedArray
}

func isAscending(_ a: [Int]) -> Bool {
  return zip(a, a.dropFirst()).allSatisfy(<=)
}

let items: [Int] = {
  var g = SplitMix64(seed: 42)
  return (0..<10_000).map {_ in Int.random(in: 0..<1000, using: &g) }
}()

let buckets: [Bucket<Int>] = {
  let bucketCount = 10
  let maxValue = items.max()!.convertToInt()
  let maxCapacity = Int(
    (Double(maxValue + 1) / Double(bucketCount)).rounded(.up))
  return (0..<bucketCount).map { _ in Bucket<Int>(capacity: maxCapacity) }
}()

@inline(never)
func run_BucketSort(_ N : Int) {
  for _ in 0..<N {
    let sortedArray = bucketSort(
      items, sortingAlgorithm: InsertionSort(), bucketArray: buckets)
    CheckResults(isAscending(sortedArray))
  }
}
