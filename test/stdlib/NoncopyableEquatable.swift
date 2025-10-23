//===--- NoncopyableEquatable.swift - tests for Equatable: ~Copyable ---------===//
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

let NoncopyableEquatableTests = TestSuite("NoncopyableEquatable")

struct Noncopyable<Wrapped: ~Copyable>: ~Copyable {
  var wrapped: Wrapped
}

extension Noncopyable: Equatable where Wrapped: Equatable & ~Copyable {
	static func ==(lhs: borrowing Self, rhs: borrowing Self) -> Bool { lhs.wrapped == rhs.wrapped }
}

extension Equatable where Self: ~Copyable {
  func isSame(as other: borrowing Self) -> Bool {
    self == other
  }
}

func isNotSame<T: Equatable & ~Copyable>(_ lhs: borrowing T, _ rhs: borrowing T) -> Bool {
  lhs != rhs
}

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: Equatable & ~Copyable {
  func firstIndex(of element: borrowing Element) -> Int? {
    indices.first { self[$0] == element }
  }
}

NoncopyableEquatableTests.test("equating noncopyables") {  
  // TODO: update StdLibUnitTest to work with ~Copyable Equtable
  
  let a = Noncopyable(wrapped: 1)
  let b = Noncopyable(wrapped: 2)
  let c = Noncopyable(wrapped: 1)

  expectFalse(a.isSame(as: b))
  expectTrue(a.isSame(as: c))

  expectTrue(isNotSame(a,b))
  
  
  let nc2 = Noncopyable(wrapped: Noncopyable(wrapped: "1"))
  expectTrue(nc2 == nc2)
  expectTrue(nc2 != .init(wrapped: .init(wrapped: "2")))

  guard #available(SwiftStdlib 6.2, *) else { return }

  let array: [3 of Noncopyable] = [a,b,c]
  let d = Noncopyable(wrapped: 2)
  let i = array.firstIndex(of: d)
  expectNotNil(i)
  
  // TODO: reduce and file the assertion when next line uncommented:
  // expectTrue(d == array[i!])

  // SIL verification failed: instruction isn't dominated by its operand: properlyDominates(valueI, I)
  // Verifying instruction:
  // %356 = load_borrow %278 : $*InlineArray<3, Noncopyable<Int>> // users: %574, %383, %400, %385
  // ->   end_borrow %356 : $InlineArray<3, Noncopyable<Int>> // id: %574

  guard let i else { fatalError() }
  // this works though:
  expectTrue(d == array[i])

  expectNil(array.firstIndex(of: .init(wrapped: 3)))
}

runAllTests()
