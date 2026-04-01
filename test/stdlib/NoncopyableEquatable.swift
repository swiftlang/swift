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
// RUN: %target-run-simple-swift(-enable-experimental-feature Lifetimes)
// REQUIRES: executable_test
// REQUIRES: swift_feature_Lifetimes

import StdlibUnittest

let NoncopyableEquatableTests = TestSuite("NoncopyableEquatable")

struct Noncopyable<Wrapped: ~Copyable & ~Escapable>: ~Copyable, ~Escapable {
  var wrapped: Wrapped

  @_lifetime(copy wrapped)
  init(wrapping wrapped: consuming Wrapped) {
    self.wrapped = wrapped
  }
}

struct Nonescapable: ~Escapable {
  let wrapped: Int
}

extension Nonescapable: Equatable {
  static func ==(lhs: Self, rhs: Self) -> Bool { lhs.wrapped == rhs.wrapped }
}

extension Noncopyable: Equatable where Wrapped: Equatable & ~Copyable & ~Escapable {
	static func ==(lhs: borrowing Self, rhs: borrowing Self) -> Bool { lhs.wrapped == rhs.wrapped }
}

extension Equatable where Self: ~Copyable & ~Escapable {
  func isSame(as other: borrowing Self) -> Bool {
    self == other
  }
}

extension Noncopyable: Escapable where Wrapped: Escapable & ~Copyable { }

func isNotSame<T: Equatable & ~Copyable & ~Escapable>(_ lhs: borrowing T, _ rhs: borrowing T) -> Bool {
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
  
  let a = Noncopyable(wrapping: 1)
  let b = Noncopyable(wrapping: 2)
  let c = Noncopyable(wrapping: 1)

  expectFalse(a.isSame(as: b))
  expectTrue(a.isSame(as: c))

  expectTrue(isNotSame(a,b))
  
  
  let nc2 = Noncopyable(wrapping: Noncopyable(wrapping: "1"))
  expectTrue(nc2 == nc2)
  expectTrue(nc2 != .init(wrapping: .init(wrapping: "2")))

  guard #available(SwiftStdlib 6.2, *) else { return }

  let array: [3 of Noncopyable] = [a,b,c]
  let d = Noncopyable(wrapping: 2)
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

  expectNil(array.firstIndex(of: .init(wrapping: 3)))
}

NoncopyableEquatableTests.test("equating nonescapables") {  
  let nc1 = Noncopyable<Nonescapable>(wrapping: .init(wrapped: 1))
  let nc2 = Noncopyable<Nonescapable>(wrapping: .init(wrapped: 1))
  let nc3 = Noncopyable<Nonescapable>(wrapping: .init(wrapped: 2))
  
  expectTrue(nc1 == nc2)
  expectFalse(nc1 == nc3)
  expectTrue(nc1.isSame(as: nc2))
  expectFalse(nc1.isSame(as: nc3))
  expectTrue(isNotSame(nc1, nc3))
  expectFalse(isNotSame(nc1, nc2))
}


runAllTests()
