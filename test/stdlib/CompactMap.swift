//===--- CompactMap.swift - tests for lazy compactmapping --------------------------===//
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
// RUN: rm -rf %t ; mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out -swift-version 5 && %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest

let CompactMapTests = TestSuite("CompactMap")

// Check that the generic parameters are called 'Base' and 'Element'.
protocol TestProtocol1 {}

extension LazyCompactMapIterator where Base : TestProtocol1, Element : TestProtocol1 {
  var _baseIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension LazyCompactMapSequence where Base : TestProtocol1, Element : TestProtocol1 {
  var _baseIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension LazyCompactMapCollection where Base : TestProtocol1, Element : TestProtocol1 {
  var _baseIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

CompactMapTests.test("CompactMapping Collections") {
  let f0 = LazyCompactMapCollection(_base: 0..<30, transform: { num -> Int? in
    num % 7 == 0 ? num * num : nil
  })
  expectEqualSequence([0, 49, 196, 441, 784], f0)

  let f1 = LazyCompactMapCollection(_base: 1..<30, transform: { num -> Int? in
    num % 7 == 0 ? num * num : nil
  })
  expectEqualSequence([49, 196, 441, 784], f1)
}

CompactMapTests.test("CompactMapping Sequences") {
  let f0 = LazyCompactMapSequence(_base: 0..<30, transform: { num -> Int? in
    num % 7 == 0 ? num * num : nil
  })
  expectEqualSequence([0, 49, 196, 441, 784], f0)

  let f1 = LazyCompactMapSequence(_base: 1..<30, transform: { num -> Int? in
    num % 7 == 0 ? num * num : nil
  })
  expectEqualSequence([49, 196, 441, 784], f1)
}

CompactMapTests.test("Single Count") {
  // Check that we're only calling a lazy compactMap's transform one time for
  // each element in a sequence or collection.

  var count = 0
  let mod7SquareAndCount = { (num: Int) -> Int? in
    count += 1
    return num % 7 == 0 ? num * num : nil
  }

  let f0 = (0..<30).makeIterator().lazy.compactMap(mod7SquareAndCount)
  _ = Array(f0)
  expectEqual(30, count)

  count = 0
  let f1 = LazyCompactMapCollection(
    _base: 0..<30,
    transform: mod7SquareAndCount
  )
  _ = Array(f1)
  expectEqual(30, count)
}

CompactMapTests.test("CompactMap Combiner Correctness/Sequence") {
  func combinerCorrectnessTest<S: Sequence>(_ base: S) where S.Element == Int {
    let lazyBase = base.lazy
    let filterFunc = { (num: Int) -> Bool in num % 7 == 0 }
    typealias ExpectedType = LazyCompactMapSequence<S, Int>
    let mapFunc = { (num: Int) -> Int in num * num }
    let compactMapFunc = { (num: Int) -> Int? in
      num % 2 == 0 ? num * num : nil
    }

    var f0 = base.filter(filterFunc).map(mapFunc)
    var f0Lazy = lazyBase.filter(filterFunc).map(mapFunc)
    // Just to make sure we're actually comparing two different types
    expectType([Int].self, &f0)
    expectType(ExpectedType.self, &f0Lazy)
    expectEqualSequence(f0, f0Lazy)

    var f1 = base.map(mapFunc).filter(filterFunc)
    var f1Lazy = lazyBase.filter(filterFunc).map(mapFunc)
    expectType([Int].self, &f1)
    expectType(ExpectedType.self, &f1Lazy)
    expectEqualSequence(f1, f1Lazy)

    var f2 = base.compactMap(compactMapFunc).map(mapFunc).filter(filterFunc)
    var f2Lazy = lazyBase.compactMap(compactMapFunc).map(mapFunc).filter(filterFunc)
    expectType([Int].self, &f2)
    expectType(ExpectedType.self, &f2Lazy)
    expectEqualSequence(f2, f2Lazy)
    // Make sure we're actually comparing something, otherwise this isn't a very good test
    // Put here because the combined compactMap + filter + map is the one that
    // filters out the most elements
    expectNotEqual(f2.count, 0)

    var f3 = base.filter(filterFunc).map(mapFunc).compactMap(compactMapFunc)
    var f3Lazy = lazyBase.filter(filterFunc).map(mapFunc).compactMap(compactMapFunc)
    expectType([Int].self, &f3)
    expectType(ExpectedType.self, &f3Lazy)
    expectEqualSequence(f3, f3Lazy)
  }
  combinerCorrectnessTest(0..<60)
  combinerCorrectnessTest(1..<60)
  combinerCorrectnessTest(Array(0..<60))
}

CompactMapTests.test("CompactMap Combiner Correctness/Collection") {
  func combinerCorrectnessTest<C: Collection>(_ base: C) where C.Element == Int {
    let lazyBase = base.lazy
    let filterFunc = { (num: Int) -> Bool in num % 7 == 0 }
    typealias ExpectedType = LazyCompactMapCollection<C, Int>
    let mapFunc = { (num: Int) -> Int in num * num }
    let compactMapFunc = { (num: Int) -> Int? in
      num % 2 == 0 ? num * num : nil
    }

    var f0 = base.filter(filterFunc).map(mapFunc)
    var f0Lazy = lazyBase.filter(filterFunc).map(mapFunc)
    // Just to make sure we're actually comparing two different types
    expectType([Int].self, &f0)
    expectType(ExpectedType.self, &f0Lazy)
    expectEqualSequence(f0, f0Lazy)

    var f1 = base.map(mapFunc).filter(filterFunc)
    var f1Lazy = lazyBase.filter(filterFunc).map(mapFunc)
    expectType([Int].self, &f1)
    expectType(ExpectedType.self, &f1Lazy)
    expectEqualSequence(f1, f1Lazy)

    var f2 = base.compactMap(compactMapFunc).map(mapFunc).filter(filterFunc)
    var f2Lazy = lazyBase.compactMap(compactMapFunc).map(mapFunc).filter(filterFunc)
    expectType([Int].self, &f2)
    expectType(ExpectedType.self, &f2Lazy)
    expectEqualSequence(f2, f2Lazy)
    // Make sure we're actually comparing something, otherwise this isn't a very good test
    // Put here because the combined compactMap + filter + map is the one that
    // filters out the most elements
    expectNotEqual(f2.count, 0)

    var f3 = base.filter(filterFunc).map(mapFunc).compactMap(compactMapFunc)
    var f3Lazy = lazyBase.filter(filterFunc).map(mapFunc).compactMap(compactMapFunc)
    expectType([Int].self, &f3)
    expectType(ExpectedType.self, &f3Lazy)
    expectEqualSequence(f3, f3Lazy)
  }
  combinerCorrectnessTest(0..<60)
  combinerCorrectnessTest(1..<60)
  combinerCorrectnessTest(Array(0..<60))
}

runAllTests()
