//===--- BucketSort.swift ----------------------------------------------------===//
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
// Adapted from the original implementation: 
// https://github.com/raywenderlich/swift-algorithm-club/tree/master/Bucket%20Sort
// Issue: https://github.com/raywenderlich/swift-algorithm-club/issues/863

// This benchmark implements the classical BucketSort algorithm described at
// https://en.wikipedia.org/wiki/Bucket_sort. The implementation allows for an
// array of generic type of "SortableItem" to be sorted. Unfortunately,  if
// ``sortingAlgo'' type is not known for the callsite in line 84, then the 
// ``sort'' method can not be specialized to integer array sorting, which will 
// lead to a huge performance loss. Since SortingAlgorithm and InsertionSort are 
// declared to be ``public'' and that lines 83-85 can not be inlined in
// BucketSortImpl (due to inlining heuristic limitations), today swift 
// compiler (without ExistentialSpecializer) can not achieve this feat.  With 
// ExistentialSpecializer which enables generic specialization recursively in a
// call chain, we are able to specialize line 84 for InsertionSort on integers.

import TestsUtils
import Foundation

public let BucketSort = BenchmarkInfo(
  name: "BucketSort",
  runFunction: run_BucketSort,
  tags: [.validation, .algorithm],
  setUpFunction: { buildWorkload() },
  legacyFactor: 10)

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
func BucketSortImpl<T>(_ items: [T], sortingAlgorithm: SortingAlgorithm, bucketArray: [Bucket<T>]) -> [T] {
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
func isArraySorted(_ arr: [Int]) -> Bool {
  var idx = 0
  while idx < (arr.count - 1) {
    if arr[idx] > arr[idx+1] {
       return false
    }
    idx += 1
  }
  return true
}
let NUMITEMS = 10000
let MAXBUCKETSIZE = 1000
let NUMBUCKETS: Int = 10
let items: [Int] = {
  var array: [Int]? = [Int]()
  for _ in 0..<NUMITEMS {
    array!.append(Int.random(in: 0..<MAXBUCKETSIZE))
  }
  return array!
}()

let buckets: [Bucket<Int>] = {
  let val = (items.max()?.convertToInt())! + 1
  let maxCapacity = Int( ceil( Double(val) / Double(NUMBUCKETS)))
  var bucketArray = [Bucket<Int>]()
  for _ in 0..<NUMBUCKETS {
    bucketArray.append(Bucket<Int>(capacity: maxCapacity))
  }
  return bucketArray
}()
@inline(never)
func run_BucketSort(_ N : Int) {
  for _ in 0..<N {
    let sortedArray = BucketSortImpl(items, sortingAlgorithm: InsertionSort(), bucketArray: buckets)
    CheckResults(isArraySorted(sortedArray))
  }
}
@inline(never)
func buildWorkload() {
  blackHole(items)
  blackHole(buckets)
}
