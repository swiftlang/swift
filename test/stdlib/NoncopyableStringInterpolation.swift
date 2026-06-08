//===--- NoncopyableStringInterpolation.swift ----------------------------===//
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
// RUN: %target-run-simple-swift(-swift-version 5 -enable-experimental-feature Lifetimes)
// REQUIRES: executable_test
// REQUIRES: swift_feature_Lifetimes

import StdlibUnittest

let NoncopyableStringInterpolationTests =
  TestSuite("NoncopyableStringInterpolation")

struct NCDescribed: ~Copyable, CustomStringConvertible {
  let value: Int
  var description: String { "C(\(value))" }
}

struct NCStreamed: ~Copyable, TextOutputStreamable {
  let value: Int
  func write<Target: TextOutputStream>(to target: inout Target) {
    "T(\(value))".write(to: &target)
  }
}

struct NCBoth: ~Copyable, CustomStringConvertible, TextOutputStreamable {
  let value: Int
  var description: String { "D(\(value))" }
  func write<Target: TextOutputStream>(to target: inout Target) {
    "W(\(value))".write(to: &target)
  }
}

// A ~Escapable (but Copyable) CustomStringConvertible leaf.
struct NEDescribed: ~Escapable, CustomStringConvertible {
  let value: Int
  var description: String { "E(\(value))" }
}

NoncopyableStringInterpolationTests.test("interpolating noncopyables") {
  let c = NCDescribed(value: 1)
  expectEqual("[C(1)]", "[\(c)]")

  let t = NCStreamed(value: 2)
  expectEqual("[T(2)]", "[\(t)]")

  let b = NCBoth(value: 3)
  expectEqual("[W(3)]", "[\(b)]")

  expectEqual("D(3)", String(describing: b))
}

NoncopyableStringInterpolationTests.test("interpolating nonescapables") {
  let e = NEDescribed(value: 4)
  expectEqual("[E(4)]", "[\(e)]")
}

NoncopyableStringInterpolationTests.test("interpolating optionals with default") {
  let someC: NCDescribed? = NCDescribed(value: 5)
  expectEqual("[C(5)]", "[\(someC, default: "none")]")
  let noneC: NCDescribed? = nil
  expectEqual("[none]", "[\(noneC, default: "none")]")

  let someT: NCStreamed? = NCStreamed(value: 6)
  expectEqual("[T(6)]", "[\(someT, default: "none")]")
  let noneT: NCStreamed? = nil
  expectEqual("[none]", "[\(noneT, default: "none")]")

  let someB: NCBoth? = NCBoth(value: 7)
  expectEqual("[W(7)]", "[\(someB, default: "none")]")
}

NoncopyableStringInterpolationTests.test("interpolating copyables (baseline)") {
  expectEqual("[42]", "[\(42)]")
  expectEqual("[hi]", "[\("hi")]")
  let none: Int? = nil
  expectEqual("[none]", "[\(none, default: "none")]")
  let some: Int? = 9
  expectEqual("[9]", "[\(some, default: "none")]")
}

runAllTests()
