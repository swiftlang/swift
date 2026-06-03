//===--- NoncopyableHashable.swift - tests for Hashable: ~Copyable ----------===//
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

let NoncopyableHashableTests = TestSuite("NoncopyableHashable")

struct Noncopyable<Wrapped: ~Copyable & ~Escapable>: ~Copyable {
  var wrapped: Wrapped

  @_lifetime(copy wrapped)
  init(wrapping wrapped: consuming Wrapped) {
    self.wrapped = wrapped
  }
}

extension Noncopyable: Equatable where Wrapped: Equatable & ~Copyable & ~Escapable { }

extension Noncopyable: Hashable where Wrapped: Hashable & ~Copyable & ~Escapable { }

extension Noncopyable: Escapable where Wrapped: Escapable & ~Copyable { }

struct Nonescapable: ~Escapable {
  let wrapped: Int
}

extension Nonescapable: Equatable { }

extension Nonescapable: Hashable { }

extension Hashable where Self: ~Copyable & ~Escapable {
  func sameHash(as other: borrowing Self) -> Bool {
    self.hashValue == other.hashValue
  }
}

func differentHash<T: Hashable & ~Copyable & ~Escapable>(_ lhs: borrowing T, _ rhs: borrowing T) -> Bool {
  lhs.hashValue != rhs.hashValue
}

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: Hashable & ~Copyable {
  func combinedHashes() -> Int {
    var hasher = Hasher()
    for i in self.indices {
      self[i].hash(into: &hasher)
    }
    return hasher.finalize()
  }
}

NoncopyableHashableTests.test("hashing noncopyables") {
  let a = Noncopyable(wrapping: 1)
  let b = Noncopyable(wrapping: 2)
  let c = Noncopyable(wrapping: 1)

  expectTrue(a.sameHash(as: a))
  expectFalse(a.sameHash(as: b))
  expectTrue(a.sameHash(as: c))

  expectTrue(differentHash(a,b))
  expectFalse(differentHash(a,a))
  expectFalse(differentHash(a,c))

  let nc2 = Noncopyable(wrapping: Noncopyable(wrapping: "1"))
  expectTrue(nc2.sameHash(as: nc2))
  expectTrue(differentHash(nc2, .init(wrapping: .init(wrapping: "2"))))

  guard #available(SwiftStdlib 6.2, *) else { return }

  let a1: [_ of _] = [a,b]
  let d = Noncopyable(wrapping: 2)
  let a2: [_ of _] = [c,d]
  expectEqual(a1.combinedHashes(), a2.combinedHashes())
  
}

NoncopyableHashableTests.test("hashing nonescapables") {  
  let nc1 = Noncopyable<Nonescapable>(wrapping: .init(wrapped: 1))
  let nc2 = Noncopyable<Nonescapable>(wrapping: .init(wrapped: 1))
  let nc3 = Noncopyable<Nonescapable>(wrapping: .init(wrapped: 2))
  
  expectTrue(nc1.hashValue == nc2.hashValue)
  expectFalse(nc1.hashValue == nc3.hashValue)
  expectTrue(nc1.sameHash(as: nc2))
  expectFalse(nc1.sameHash(as: nc3))
  expectTrue(differentHash(nc1, nc3))
  expectFalse(differentHash(nc1, nc2))
}

runAllTests()
