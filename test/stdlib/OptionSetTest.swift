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

struct PackagingOptions : OptionSet {
  let rawValue: Int
  init(rawValue: Int) { self.rawValue = rawValue }

  static let
    Box = PackagingOptions(rawValue: 1),
    Carton = PackagingOptions(rawValue: 2),
    Bag = PackagingOptions(rawValue: 4),
    Satchel = PackagingOptions(rawValue: 8)

  // FIXME: these must be separate decls because of <rdar://20962990>
  static let BoxOrBag: PackagingOptions = [Box, Bag]
  static let BoxOrCartonOrBag: PackagingOptions = [Box, Carton, Bag]
  static let SatchelOrBag = Satchel.union(Bag)
}

import StdlibUnittest


var tests = TestSuite("OptionSet")

tests.test("basics") {
  typealias P = PackagingOptions

  expectNotEqual(P(), .Box)
  expectEqual(P.Box, .Box)
  expectNotEqual(P.Box, .Carton)
  expectNotEqual(P.Box, .BoxOrBag)

  expectEqual(.Box, P.Box.intersection(.BoxOrBag))
  expectEqual(.Bag, P.Bag.intersection(.BoxOrBag))
  expectEqual(P(), P.Bag.intersection(.Box))
  expectEqual(P(), P.Box.intersection(.Satchel))
  expectEqual(.BoxOrBag, P.Bag.union(.Box))
  expectEqual(.BoxOrBag, P.Box.union(.Bag))
  expectEqual(.BoxOrCartonOrBag, P.BoxOrBag.union(.Carton))
  expectEqual([.Satchel, .Box], P.SatchelOrBag.symmetricDifference(.BoxOrBag))

  var p = P.Box
  p.formIntersection(.BoxOrBag)
  expectEqual(.Box, p)
  
  p = .Bag
  p.formIntersection(.BoxOrBag)
  expectEqual(.Bag, p)
  
  p = .Bag
  p.formIntersection(.Box)
  expectEqual(P(), p)
  
  p = .Box
  p.formIntersection(.Satchel)
  expectEqual(P(), p)
  
  p = .Bag
  p.formUnion(.Box)
  expectEqual(.BoxOrBag, p)
  
  p = .Box
  p.formUnion(.Bag)
  expectEqual(.BoxOrBag, p)
  
  p = .BoxOrBag
  p.formUnion(.Carton)
  expectEqual(.BoxOrCartonOrBag, p)

  p = .SatchelOrBag
  p.formSymmetricDifference(.BoxOrBag)
  expectEqual([.Satchel, .Box], p)
}

// FIXME: add tests for all of SetAlgebra, in particular
// insert/remove/replace.

runAllTests()
