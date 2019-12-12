//===--- OptionSetTest.swift - Test for library-only option sets ----------===//
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

struct PackagingOptions : OptionSet {
  let rawValue: Int
  init(rawValue: Int) { self.rawValue = rawValue }

  static let box = PackagingOptions(rawValue: 1)
  static let carton = PackagingOptions(rawValue: 2)
  static let bag = PackagingOptions(rawValue: 4)
  static let satchel = PackagingOptions(rawValue: 8)

  static let boxOrBag: PackagingOptions = [box, bag]
  static let boxOrCartonOrBag: PackagingOptions = [box, carton, bag]
  static let satchelOrBag = satchel.union(bag)
}

var tests = TestSuite("OptionSet")
defer { runAllTests() }

tests.test("basics") {
  typealias P = PackagingOptions

  expectNotEqual(P(), .box)
  expectEqual(P.box, .box)
  expectNotEqual(P.box, .carton)
  expectNotEqual(P.box, .boxOrBag)

  expectEqual(.box, P.box.intersection(.boxOrBag))
  expectEqual(.bag, P.bag.intersection(.boxOrBag))
  expectEqual(P(), P.bag.intersection(.box))
  expectEqual(P(), P.box.intersection(.satchel))
  expectEqual(.boxOrBag, P.bag.union(.box))
  expectEqual(.boxOrBag, P.box.union(.bag))
  expectEqual(.boxOrCartonOrBag, P.boxOrBag.union(.carton))
  expectEqual([.satchel, .box], P.satchelOrBag.symmetricDifference(.boxOrBag))

  var p = P.box
  p.formIntersection(.boxOrBag)
  expectEqual(.box, p)

  p = .bag
  p.formIntersection(.boxOrBag)
  expectEqual(.bag, p)

  p = .bag
  p.formIntersection(.box)
  expectEqual(P(), p)

  p = .box
  p.formIntersection(.satchel)
  expectEqual(P(), p)

  p = .bag
  p.formUnion(.box)
  expectEqual(.boxOrBag, p)

  p = .box
  p.formUnion(.bag)
  expectEqual(.boxOrBag, p)

  p = .boxOrBag
  p.formUnion(.carton)
  expectEqual(.boxOrCartonOrBag, p)

  p = .satchelOrBag
  p.formSymmetricDifference(.boxOrBag)
  expectEqual([.satchel, .box], p)
}

tests.test("set algebra") {
  typealias P = PackagingOptions

  // remove
  var p = P.boxOrBag
  expectNil(p.remove(P.carton))

  p = P.boxOrBag
  p.remove(P.boxOrCartonOrBag)
  expectEqual(P(), p)

  p = P.boxOrBag
  let removed = p.remove(P.satchelOrBag)
  if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
    // https://github.com/apple/swift/pull/28378
    expectEqual(P.bag, removed)
  }
  expectEqual(P.box, p)

  // insert
  p = P.box
  var insertionResult = p.insert(.bag)
  expectTrue(insertionResult.inserted)
  expectEqual(P.bag, insertionResult.memberAfterInsert)
  expectEqual(P.boxOrBag, p)

  insertionResult = p.insert(.bag)
  expectFalse(insertionResult.inserted)
  expectEqual(P.bag, insertionResult.memberAfterInsert)

  // update
  p = P.box
  expectNil(p.update(with: .bag))
  expectEqual(P.boxOrBag, p)

  p = P.box
  expectEqual(P.box, p.update(with: .boxOrBag))
  expectEqual(P.boxOrBag, p)
}
