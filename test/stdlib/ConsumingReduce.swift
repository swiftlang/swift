//===--- ConsumingReduce.swift - tests reduce consuming initial value -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let ConsumingReduceTests = TestSuite("ConsumingReduce")

struct Noncopyable<Wrapped: ~Copyable>: ~Copyable {
  var wrapped: Wrapped
}

extension Noncopyable where Wrapped: BinaryInteger {
  static func +(lhs: consuming Self, rhs: borrowing Wrapped) -> Self {
    lhs.wrapped += rhs
    return lhs
  }
}

extension Sequence where Element: BinaryInteger {
  func noncopyableSum() -> Noncopyable<Element> {
    self.reduce(.init(wrapped: 0)) { $0 + $1 }
  }
}

class Klass {
  var i: Int
  init(_ i: Int) { self.i = i }
}

ConsumingReduceTests.test("consuming reduce") {
  expectEqual((0..<10).noncopyableSum().wrapped, 45)
  
  // TODO: this doesn't really test much, may want to
  // use isTriviallyIdentical(to:) with an array
  // once it's available
  var d = (0..<10).reduce(Klass(0)) {
    $0.i += $1
    return $0
  }
  expectTrue(isKnownUniquelyReferenced(&d))
}

runAllTests()
