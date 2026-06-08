//===--- NoncopyableTextOutputStreamable.swift ---------------------------===//
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

let NoncopyableTextOutputStreamableTests =
  TestSuite("NoncopyableTextOutputStreamable")

struct Box<Wrapped: ~Copyable & ~Escapable>: ~Copyable, ~Escapable {
  var wrapped: Wrapped

  @_lifetime(copy wrapped)
  init(_ wrapped: consuming Wrapped) {
    self.wrapped = wrapped
  }
}

extension Box: TextOutputStreamable
where Wrapped: TextOutputStreamable & ~Copyable & ~Escapable {
  func write<Target: TextOutputStream>(to target: inout Target) {
    "B<".write(to: &target)
    wrapped.write(to: &target)
    ">".write(to: &target)
  }
}

extension Box: Escapable where Wrapped: Escapable & ~Copyable {}

struct NCStreamable: ~Copyable, TextOutputStreamable {
  let value: Int
  func write<Target: TextOutputStream>(to target: inout Target) {
    "S(\(value))".write(to: &target)
  }
}

struct NEStreamable: ~Escapable, TextOutputStreamable {
  let value: Int
  func write<Target: TextOutputStream>(to target: inout Target) {
    "S(\(value))".write(to: &target)
  }
}

struct NCBoth: ~Copyable, CustomStringConvertible, TextOutputStreamable {
  let value: Int
  var description: String { "D(\(value))" }
  func write<Target: TextOutputStream>(to target: inout Target) {
    "W(\(value))".write(to: &target)
  }
}

extension TextOutputStreamable where Self: ~Copyable & ~Escapable {
  func streamedString() -> String {
    var s = ""
    write(to: &s)
    return s
  }
}

func streaming<T: TextOutputStreamable & ~Copyable & ~Escapable>(
  _ value: borrowing T
) -> String {
  var s = ""
  value.write(to: &s)
  return s
}

NoncopyableTextOutputStreamableTests.test("streaming noncopyables") {
  let s = NCStreamable(value: 1)
  var out = ""
  s.write(to: &out)
  expectEqual("S(1)", out)

  expectEqual("S(1)", s.streamedString())
  expectEqual("S(1)", streaming(s))

  expectEqual("S(1)", String(describing: s))

  let nested = Box(NEStreamable(value: 2))
  expectEqual("B<S(2)>", streaming(nested))
  expectEqual("B<S(2)>", String(describing: nested))
}

NoncopyableTextOutputStreamableTests.test("streaming nonescapables") {
  let s = NEStreamable(value: 3)
  var out = ""
  s.write(to: &out)
  expectEqual("S(3)", out)
  expectEqual("S(3)", s.streamedString())
  expectEqual("S(3)", streaming(s))
  expectEqual("S(3)", String(describing: s))
}

NoncopyableTextOutputStreamableTests.test("String(describing:) combined overload") {
  let b = NCBoth(value: 4)

  expectEqual("D(4)", String(describing: b))

  expectEqual("W(4)", streaming(b))
  expectEqual("W(4)", b.streamedString())
}

runAllTests()
