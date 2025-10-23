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

NoncopyableEquatableTests.test("comparing noncopyables") {
    let a = Noncopyable(i: 1)
    let b = Noncopyable(i: 2)
    let c = Noncopyable(i: 1)

    expectFalse(a.isSame(as: b))
    expectTrue(a.isSame(as: c))

    expectTrue(isNotSame(a,b))
}

runAllTests()