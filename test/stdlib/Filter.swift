//===--- Filter.swift - tests for lazy filtering --------------------------===//
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
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


let FilterTests = TestSuite("Filter")

enum FilterTestError: Error {
  case stopped
}

func expectFilterTestError(_ body: () throws -> Void) {
  var didThrow = false
  do {
    try body()
  } catch FilterTestError.stopped {
    didThrow = true
  } catch {
    expectUnreachable("unexpected error: \(error)")
  }
  expectTrue(didThrow)
}

// Check that the generic parameter is called 'Base'.
protocol TestProtocol1 {}

extension LazyFilterIterator where Base : TestProtocol1 {
  var _baseIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension LazyFilterSequence where Base : TestProtocol1 {
  var _baseIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension LazyFilterCollection where Base : TestProtocol1 {
  var _baseIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

final class FilterCollectionCounters {
  var subscriptCount = 0
  var indexBeforeCount = 0
}

struct CountingFilterCollection: Collection {
  var elements: [Int]
  var counters: FilterCollectionCounters

  var startIndex: Int { elements.startIndex }
  var endIndex: Int { elements.endIndex }

  subscript(position: Int) -> Int {
    counters.subscriptCount += 1
    return elements[position]
  }

  func index(after i: Int) -> Int {
    return i + 1
  }
}

struct CountingBidirectionalFilterCollection: BidirectionalCollection {
  var elements: [Int]
  var counters: FilterCollectionCounters

  var startIndex: Int { elements.startIndex }
  var endIndex: Int { elements.endIndex }

  subscript(position: Int) -> Int {
    counters.subscriptCount += 1
    return elements[position]
  }

  func index(after i: Int) -> Int {
    return i + 1
  }

  func index(before i: Int) -> Int {
    counters.indexBeforeCount += 1
    return i - 1
  }
}

FilterTests.test("filtering collections") {
  let f0 = LazyFilterCollection(_base: 0..<30) { $0 % 7 == 0 }
  expectEqualSequence([0, 7, 14, 21, 28], f0)

  let f1 = LazyFilterCollection(_base: 1..<30) { $0 % 7 == 0 }
  expectEqualSequence([7, 14, 21, 28], f1)
}

FilterTests.test("filtering sequences") {
  let f0 = (0..<30).makeIterator().lazy.filter { $0 % 7 == 0 }
  expectEqualSequence([0, 7, 14, 21, 28], f0)

  let f1 = (1..<30).makeIterator().lazy.filter { $0 % 7 == 0 }
  expectEqualSequence([7, 14, 21, 28], f1)
}

FilterTests.test("single-count") {
  // Check that we're only calling a lazy filter's predicate one time for
  // each element in a sequence or collection.
  var count = 0
  let mod7AndCount: (Int) -> Bool = {
    count += 1
    return $0 % 7 == 0
  }
    
  let f0 = (0..<30).makeIterator().lazy.filter(mod7AndCount)
  _ = Array(f0)
  expectEqual(30, count)

  count = 0
  let f1 = LazyFilterCollection(_base: 0..<30, mod7AndCount)
  _ = Array(f1)
  expectEqual(30, count)
}

FilterTests.test("chained filter order") {
  let array = [1]
  
  let lazyFilter = array.lazy
    .filter { _ in false }
    .filter { _ in
      expectUnreachable("Executed second filter before first")
      return true
    }
  let lazyResult = Array(lazyFilter)
  
  let result = array
    .filter { _ in false }
    .filter { _ in
      expectUnreachable("Executed second filter before first")
      return true
    }
  
  expectEqual(lazyResult.count, 0)
  expectEqual(result.count, 0)
}

FilterTests.test("lazy filter first") {
  let counters = FilterCollectionCounters()
  var filterCount = 0

  let result = CountingFilterCollection(
    elements: Array(0..<10),
    counters: counters
  ).lazy
    .filter {
      filterCount += 1
      return $0 == 4
    }
    .first

  expectEqual(Optional(4), result)
  expectEqual(5, filterCount)
  expectEqual(5, counters.subscriptCount)
}

FilterTests.test("lazy filter first(where:)") {
  var filterCount = 0
  var firstCount = 0

  let result = (0..<10).makeIterator().lazy
    .filter {
      filterCount += 1
      return $0 % 2 == 0
    }
    .first {
      firstCount += 1
      return $0 > 4
    }

  expectEqual(Optional(6), result)
  expectEqual(7, filterCount)
  expectEqual(4, firstCount)
}

FilterTests.test("lazy filter last") {
  let counters = FilterCollectionCounters()
  var filterCount = 0

  let result = CountingBidirectionalFilterCollection(
    elements: Array(0..<10),
    counters: counters
  )
    .lazy
    .filter {
      filterCount += 1
      return $0 == 9
    }
    .last

  expectEqual(Optional(9), result)
  expectEqual(1, filterCount)
  expectEqual(1, counters.subscriptCount)
  expectEqual(1, counters.indexBeforeCount)
}

FilterTests.test("lazy filter last(where:)") {
  var filterCount = 0
  var lastCount = 0

  let result = (0..<10).lazy
    .filter {
      filterCount += 1
      return $0 % 2 == 1
    }
    .last {
      lastCount += 1
      return $0 < 8
    }

  expectEqual(Optional(7), result)
  expectEqual(3, filterCount)
  expectEqual(2, lastCount)
}

FilterTests.test("lazy filter lastIndex(where:)") {
  var filterCount = 0
  var lastIndexCount = 0

  let result = (0..<10).lazy
    .filter {
      filterCount += 1
      return $0 % 2 == 1
    }
    .lastIndex {
      lastIndexCount += 1
      return $0 < 8
    }

  expectEqual(Optional(7), result)
  expectEqual(3, filterCount)
  expectEqual(2, lastIndexCount)
}

FilterTests.test("lazy filter terminal operations with no match") {
  var filterCount = 0

  let first = (0..<5).lazy
    .filter { _ in
      filterCount += 1
      return false
    }
    .first

  expectNil(first)
  expectEqual(5, filterCount)

  filterCount = 0
  let last = (0..<5).lazy
    .filter { _ in
      filterCount += 1
      return false
    }
    .last

  expectNil(last)
  expectEqual(5, filterCount)

  filterCount = 0
  var predicateCount = 0
  let lastWhere = (0..<5).lazy
    .filter {
      filterCount += 1
      return $0 % 2 == 1
    }
    .last {
      predicateCount += 1
      return $0 < 0
    }

  expectNil(lastWhere)
  expectEqual(5, filterCount)
  expectEqual(2, predicateCount)

  filterCount = 0
  predicateCount = 0
  let lastIndexWhere = (0..<5).lazy
    .filter {
      filterCount += 1
      return $0 % 2 == 1
    }
    .lastIndex {
      predicateCount += 1
      return $0 < 0
    }

  expectNil(lastIndexWhere)
  expectEqual(5, filterCount)
  expectEqual(2, predicateCount)
}

FilterTests.test("lazy filter terminal operations on empty bases") {
  var filterCount = 0
  var predicateCount = 0
  let filtered = [Int]().lazy.filter { _ in
    filterCount += 1
    return true
  }

  expectNil(filtered.first)
  expectNil(filtered.last)
  expectNil(filtered.last { _ in
    predicateCount += 1
    return true
  })
  expectNil(filtered.lastIndex { _ in
    predicateCount += 1
    return true
  })
  expectEqual(0, filterCount)
  expectEqual(0, predicateCount)
}

FilterTests.test("lazy filter first(where:) with no match") {
  var filterCount = 0
  var firstCount = 0

  let result = (0..<5).makeIterator().lazy
    .filter {
      filterCount += 1
      return $0 % 2 == 0
    }
    .first {
      firstCount += 1
      return $0 < 0
    }

  expectNil(result)
  expectEqual(5, filterCount)
  expectEqual(3, firstCount)
}

FilterTests.test("lazy filter first(where:) propagates throws") {
  var filterCount = 0
  var firstCount = 0

  expectFilterTestError {
    _ = try (0..<10).makeIterator().lazy
      .filter {
        filterCount += 1
        return $0 % 2 == 0
      }
      .first {
        firstCount += 1
        if $0 == 4 {
          throw FilterTestError.stopped
        }
        return false
      }
  }

  expectEqual(5, filterCount)
  expectEqual(3, firstCount)
}

FilterTests.test("lazy filter last(where:) propagates throws") {
  var filterCount = 0
  var lastCount = 0

  expectFilterTestError {
    _ = try (0..<10).lazy
      .filter {
        filterCount += 1
        return $0 % 2 == 1
      }
      .last {
        lastCount += 1
        if $0 == 7 {
          throw FilterTestError.stopped
        }
        return false
      }
  }

  expectEqual(3, filterCount)
  expectEqual(2, lastCount)
}

FilterTests.test("lazy filter lastIndex(where:) propagates throws") {
  var filterCount = 0
  var lastIndexCount = 0

  expectFilterTestError {
    _ = try (0..<10).lazy
      .filter {
        filterCount += 1
        return $0 % 2 == 1
      }
      .lastIndex {
        lastIndexCount += 1
        if $0 == 7 {
          throw FilterTestError.stopped
        }
        return false
      }
  }

  expectEqual(3, filterCount)
  expectEqual(2, lastIndexCount)
}

runAllTests()
