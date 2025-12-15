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
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let NoncopyableHashableTests = TestSuite("NoncopyableHashable")

struct Noncopyable<Wrapped: ~Copyable>: ~Copyable {
  var wrapped: Wrapped
}

extension Noncopyable: Equatable where Wrapped: Equatable & ~Copyable { }

extension Noncopyable: Hashable where Wrapped: Hashable & ~Copyable { }


extension Hashable where Self: ~Copyable {
  func sameHash(as other: borrowing Self) -> Bool {
    self.hashValue == other.hashValue
  }
}

func differentHash<T: Hashable & ~Copyable>(_ lhs: borrowing T, _ rhs: borrowing T) -> Bool {
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
  let a = Noncopyable(wrapped: 1)
  let b = Noncopyable(wrapped: 2)
  let c = Noncopyable(wrapped: 1)

  expectTrue(a.sameHash(as: a))
  expectFalse(a.sameHash(as: b))
  expectTrue(a.sameHash(as: c))

  expectTrue(differentHash(a,b))
  expectFalse(differentHash(a,a))
  expectFalse(differentHash(a,c))

  let nc2 = Noncopyable(wrapped: Noncopyable(wrapped: "1"))
  expectTrue(nc2.sameHash(as: nc2))
  expectTrue(differentHash(nc2, .init(wrapped: .init(wrapped: "2"))))

  guard #available(SwiftStdlib 6.2, *) else { return }

  let a1: [_ of _] = [a,b]
  let d = Noncopyable(wrapped: 2)
  let a2: [_ of _] = [c,d]
  expectEqual(a1.combinedHashes(), a2.combinedHashes())
  
}

runAllTests()
