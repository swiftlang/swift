//===--- NoncopyableComparable.swift - tests for Comparable: ~Copyable -------===//
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
// RUN: %target-run-simple-swift(-enable-experimental-feature Lifetimes)
// REQUIRES: executable_test
// REQUIRES: swift_feature_Lifetimes

import StdlibUnittest

let NoncopyableComparableTests = TestSuite("NoncopyableComparable")

struct Noncopyable<Wrapped: ~Copyable & ~Escapable>: ~Copyable, ~Escapable {
  var wrapped: Wrapped
}

extension Noncopyable: Equatable where Wrapped: Equatable & ~Copyable & ~Escapable { }

extension Noncopyable: Comparable where Wrapped: Comparable & ~Copyable & ~Escapable {
  static func < (lhs: borrowing Self, rhs: borrowing Self) -> Bool { lhs.wrapped < rhs.wrapped }
}

extension Noncopyable: Escapable where Wrapped: Escapable & ~Copyable { }

struct Nonescapable: ~Escapable {
  let wrapped: Int
}

extension Nonescapable: Equatable { }

extension Nonescapable: Comparable {
  static func <(lhs: Self, rhs: Self) -> Bool { lhs.wrapped < rhs.wrapped }

}

extension Comparable where Self: ~Copyable & ~Escapable {
  func isLessThan(_ other: borrowing Self) -> Bool {
    self < other
  }
}

func isLessOrEqual<T: Comparable & ~Copyable & ~Escapable>(_ lhs: borrowing T, _ rhs: borrowing T) -> Bool {
  lhs <= rhs
}

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: Comparable & ~Copyable {
    func minIndex() -> Int? {
        indices.min { self[$0] < self[$1] }
    }
	
	func inReverseOrder() -> Bool {
		for i in indices.dropFirst() {
			if self[i] > self[i-1] { return false }
		}
		return true
	}
}

NoncopyableComparableTests.test("comparing noncopyables") {
  let a = Noncopyable(wrapped: 0)
  let b = Noncopyable(wrapped: 1)
  let c = Noncopyable(wrapped: 2)
  
  expectTrue(a < b)
  expectTrue(c > a)
  expectFalse(a < a)
  expectFalse(a > c)
  expectFalse(c <= a)
  expectTrue(a <= a)
  expectTrue(a < b && b < c)

  expectTrue(a.isLessThan(b))
  expectTrue(isLessOrEqual(a,b))
  expectFalse(b.isLessThan(a))

  let nc2 = Noncopyable(wrapped: Noncopyable(wrapped: "1"))
  expectFalse(nc2 < nc2)
  expectFalse(nc2 > nc2)
  expectTrue(nc2 >= nc2)
  expectTrue(nc2 < .init(wrapped: .init(wrapped: "2")))

  guard #available(SwiftStdlib 6.2, *) else { return }

  var array: [3 of Noncopyable] = [c,b,a]
  expectTrue(array.minIndex() == 2)
  expectTrue(array.inReverseOrder())
  array.swapAt(1, 2)
  expectFalse(array.inReverseOrder())
}

NoncopyableComparableTests.test("comparing nonescapables") {
  let a = Noncopyable<Nonescapable>(wrapped: .init(wrapped: 0))
  let b = Noncopyable<Nonescapable>(wrapped: .init(wrapped: 1))
  let c = Noncopyable<Nonescapable>(wrapped: .init(wrapped: 2))
  
  expectTrue(a < b)
  expectTrue(c > a)
  expectFalse(a < a)
  expectFalse(a > c)
  expectFalse(c <= a)
  expectTrue(a <= a)
  expectTrue(a < b && b < c)
  
  expectTrue(a.isLessThan(b))
  expectTrue(isLessOrEqual(a,b))
  expectFalse(b.isLessThan(a))
}  

runAllTests()
