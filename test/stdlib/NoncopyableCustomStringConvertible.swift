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

let NoncopyableCustomStringConvertibleTests = TestSuite("NoncopyableCustomStringConvertible")

struct Noncopyable<Wrapped: ~Copyable & CustomStringConvertible>: ~Copyable {
  var wrapped: Wrapped
}

// This approach is not necessarily what we would expect users to do, but for now we do
// not have a generalized solution for reflection-based stringifying of noncopyable types,
// so using this technique for testing purposes.
extension Noncopyable: CustomStringConvertible where Wrapped: CustomStringConvertible & ~Copyable {
    var description: String {
        "No copying the value \(wrapped)"
    }
 }

extension Noncopyable: CustomDebugStringConvertible where Wrapped: CustomStringConvertible & ~Copyable {
    var debugDescription: String {
        "Noncopyable(wrapping: \(wrapped))"
    }
 }

extension Noncopyable<Int>: LosslessStringConvertible {
    init?(_ description: String) {
        guard let i: Int = .init(description) else { return nil }
        self.wrapped = i
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

NoncopyableCustomStringConvertibleTests.test("noncopyable interpolation") {
  let a: Noncopyable<Int> = .init(riskily: "99")
  let b: Noncopyable<Noncopyable<Int>> = Noncopyable(wrapped: Noncopyable(riskily: "42"))

  expectEqual("NC: \(a)", "NC: No copying the value 99")
  expectEqual("Noncopyable(wrapping: 99)", debug(value: a))

  expectEqual("NC: \(b)", "NC: No copying the value No copying the value 42")
  expectEqual("Noncopyable(wrapping: No copying the value 42)", debug(value: b))

}

runAllTests()
