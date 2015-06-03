//===--- OptionSetTest.swift - Test for library-only option sets ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

struct PackagingOptions : OptionSetType {
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

  expectEqual(.Box, P.Box.intersect(.BoxOrBag))
  expectEqual(.Bag, P.Bag.intersect(.BoxOrBag))
  expectEqual(P(), P.Bag.intersect(.Box))
  expectEqual(P(), P.Box.intersect(.Satchel))
  expectEqual(.BoxOrBag, P.Bag.union(.Box))
  expectEqual(.BoxOrBag, P.Box.union(.Bag))
  expectEqual(.BoxOrCartonOrBag, P.BoxOrBag.union(.Carton))
  expectEqual([.Satchel, .Box], P.SatchelOrBag.exclusiveOr(.BoxOrBag))

  var p = P.Box
  p.intersectInPlace(.BoxOrBag)
  expectEqual(.Box, p)
  
  p = .Bag
  p.intersectInPlace(.BoxOrBag)
  expectEqual(.Bag, p)
  
  p = .Bag
  p.intersectInPlace(.Box)
  expectEqual(P(), p)
  
  p = .Box
  p.intersectInPlace(.Satchel)
  expectEqual(P(), p)
  
  p = .Bag
  p.unionInPlace(.Box)
  expectEqual(.BoxOrBag, p)
  
  p = .Box
  p.unionInPlace(.Bag)
  expectEqual(.BoxOrBag, p)
  
  p = .BoxOrBag
  p.unionInPlace(.Carton)
  expectEqual(.BoxOrCartonOrBag, p)

  p = .SatchelOrBag
  p.exclusiveOrInPlace(.BoxOrBag)
  expectEqual([.Satchel, .Box], p)
}

runAllTests()
