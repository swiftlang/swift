//===--- NoncopyableEquatable.swift - tests for the two reduce variants -----------------===//
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

let NoncopyableEquatableTests = TestSuite("NoncopyableEquatable")

struct Noncopyable: ~Copyable {
    var i: Int
}

extension Noncopyable: Equatable { }

extension Equatable where Self: ~Copyable {
    func isSame(as other: borrowing Self) -> Bool {
        self == other
    }
}

func isNotSame<T: Equatable & ~Copyable>(_ lhs: borrowing T, _ rhs: borrowing T) -> Bool {
    lhs != rhs
}

extension Noncopyable: Comparable {
    static func <(lhs: borrowing Self, rhs: borrowing Self) -> Bool {
        lhs.i < rhs.i
    }
}

NoncopyableEquatableTests.test("equating noncopyables") {
    let a = Noncopyable(i: 1)
    let b = Noncopyable(i: 2)
    let c = Noncopyable(i: 1)

    expectFalse(a.isSame(as: b))
    expectTrue(a.isSame(as: c))

    expectTrue(isNotSame(a,b))
}

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: Comparable & ~Copyable {
    func minIndex() -> Int? {
        indices.min { self[$0] < self[$1] }
    }
}

NoncopyableEquatableTests.test("comparing noncopyables") {

    guard #available(SwiftStdlib 6.2, *) else { return }

    let a = Noncopyable(i: 1)
    let b = Noncopyable(i: 2)
    let c = Noncopyable(i: 0)

    expectTrue(a > c)
    expectFalse(a <= c)

    let array: [3 of Noncopyable] = [a,b,c]
    expectTrue(array.minIndex() == 2)
 }

extension Noncopyable: CustomStringConvertible {
    var description: String {
        "No copying the number \(i)"
    }
 }

 extension Noncopyable: CustomDebugStringConvertible {
    var debugDescription: String {
        "Noncopyable(i: \(i))"
    }
 }

extension Noncopyable: LosslessStringConvertible {
    init?(_ description: String) {
        guard let i = Int(description) else { return nil }
        self.i = i
    }
}

extension LosslessStringConvertible where Self: ~Copyable {
    init(riskily with: String) {
        self = .init(with)!
    }
}

func debug(value: borrowing some CustomDebugStringConvertible & ~Copyable) -> String {
    value.debugDescription
}

NoncopyableEquatableTests.test("noncopyable interpolation") {

    let a = Noncopyable(riskily: "99")

    expectEqual("NC: \(a)", "NC: No copying the number \(a.i)")
    expectEqual("Noncopyable(i: \(a.i))", debug(value: a))
}

NoncopyableEquatableTests.test("equating optional noncopyables") {
    let o1: Noncopyable? = Noncopyable(i: 1)
    let o2: Noncopyable = .init(i: 1)
    expectTrue(o1 == consume o2)
}


extension Noncopyable {
    struct Nope: Error, Equatable { let s: String }

    init(throws from: String) throws(Nope) {
        guard let i = Int(from) else { throw Nope(s: from) }
        self = .init(i: i)
    }
}

extension Noncopyable: Hashable { }

NoncopyableEquatableTests.test("hashing noncopyables") {
    let o1: Noncopyable = .init(i: 1)
    let o2: Noncopyable = .init(i: 1)
    let o3: Noncopyable = .init(i: 2)

    expectTrue(o1.hashValue == o2.hashValue)
    expectFalse(o2.hashValue == o3.hashValue)
}

NoncopyableEquatableTests.test("hashing noncopyables") {
    let o1: Noncopyable? = .init(i: 1)
    let o2: Noncopyable? = .init(i: 1)
    let o3: Noncopyable? = nil

    expectTrue(o1.hashValue == o2.hashValue)
    expectFalse(o2.hashValue == o3.hashValue)
}

NoncopyableEquatableTests.test("equating optional noncopyables") {
    let o1: Noncopyable? = Noncopyable(i: 1)
    let o2: Noncopyable = .init(i: 1)
    expectTrue(o1 == consume o2)
}

NoncopyableEquatableTests.test("noncopyable result") {
    let r = Result { () throws(Noncopyable.Nope) -> Noncopyable in 
        try Noncopyable(throws: "99") 
    }
    expectTrue(r == .success(.init(i: 99)))

    let n = Result { () throws(Noncopyable.Nope) -> Noncopyable in try Noncopyable(throws: "nope") }
    expectTrue(r != n)
    let m = Result { () throws(Noncopyable.Nope) -> Noncopyable in  try Noncopyable(throws: "nope") }
    expectTrue(n == m)

    expectEqual(try! r.get().i, 99)
}

runAllTests()
