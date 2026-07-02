//===--- NoncopyableCustomStringConvertible.swift ------------------------===//
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

let NoncopyableCustomStringConvertibleTests =
  TestSuite("NoncopyableCustomStringConvertible")

struct Noncopyable<Wrapped: ~Copyable & ~Escapable>: ~Copyable, ~Escapable {
  var wrapped: Wrapped

  @_lifetime(copy wrapped)
  init(wrapping wrapped: consuming Wrapped) {
    self.wrapped = wrapped
  }
}

extension Noncopyable: CustomStringConvertible
where Wrapped: CustomStringConvertible & ~Copyable & ~Escapable {
  var description: String { "N(\(wrapped.description))" }
}

extension Noncopyable: CustomDebugStringConvertible
where Wrapped: CustomDebugStringConvertible & ~Copyable & ~Escapable {
  var debugDescription: String { "N[\(wrapped.debugDescription)]" }
}

extension Noncopyable: Escapable where Wrapped: Escapable & ~Copyable { }

struct Nonescapable: ~Escapable {
  let value: Int
}

extension Nonescapable: CustomStringConvertible {
  var description: String { "NE(\(value))" }
}

extension Nonescapable: CustomDebugStringConvertible {
  var debugDescription: String { "NE[\(value)]" }
}

struct Plain: CustomStringConvertible, CustomDebugStringConvertible {
  let value: Int
  var description: String { "C(\(value))" }
  var debugDescription: String { "C[\(value)]" }
}

// Access via protocol extensions constrained to ~Copyable & ~Escapable: the
// path that did not compile before the protocol relaxation.
extension CustomStringConvertible where Self: ~Copyable & ~Escapable {
  func describe() -> String { description }
}

extension CustomDebugStringConvertible where Self: ~Copyable & ~Escapable {
  func debugDescribe() -> String { debugDescription }
}

// Access via free generics that explicitly suppress Copyable/Escapable.
func describing<T: CustomStringConvertible & ~Copyable & ~Escapable>(
  _ value: borrowing T
) -> String {
  value.description
}

func debugDescribing<T: CustomDebugStringConvertible & ~Copyable & ~Escapable>(
  _ value: borrowing T
) -> String {
  value.debugDescription
}

NoncopyableCustomStringConvertibleTests.test("describing noncopyables") {
  // Wrap a both-conforming leaf to exercise the description and debug paths.
  let a = Noncopyable(wrapping: Plain(value: 1))

  expectEqual("N(C(1))", a.description)
  expectEqual("N[C[1]]", a.debugDescription)

  expectEqual("N(C(1))", String(describing: a))

  expectPrinted("N(C(1))", a)

  expectEqual("N(C(1))", a.describe())
  expectEqual("N[C[1]]", a.debugDescribe())

  expectEqual("N(C(1))", describing(a))
  expectEqual("N[C[1]]", debugDescribing(a))

  let i = Noncopyable(wrapping: 5)
  expectEqual("N(5)", i.description)
  expectEqual("N(5)", String(describing: i))
  expectPrinted("N(5)", i)

  let nested = Noncopyable(wrapping: Noncopyable(wrapping: Plain(value: 2)))
  expectEqual("N(N(C(2)))", nested.description)
  expectEqual("N[N[C[2]]]", debugDescribing(nested))
  expectPrinted("N(N(C(2)))", nested)
}

NoncopyableCustomStringConvertibleTests.test("describing nonescapables") {
  let n = Noncopyable<Nonescapable>(wrapping: .init(value: 7))
  expectEqual("N(NE(7))", n.description)
  expectEqual("N[NE[7]]", n.debugDescription)
  expectEqual("N(NE(7))", String(describing: n))
  expectEqual("N(NE(7))", describing(n))
  expectEqual("N[NE[7]]", debugDescribing(n))

  let ne = Nonescapable(value: 7)
  expectEqual("NE(7)", ne.description)
  expectEqual("NE[7]", ne.debugDescription)
  expectEqual("NE(7)", String(describing: ne))
  expectEqual("NE(7)", ne.describe())
  expectEqual("NE[7]", ne.debugDescribe())
  expectPrinted("NE(7)", ne)
}

NoncopyableCustomStringConvertibleTests.test("describing copyables (baseline)") {
  let c = Plain(value: 3)
  expectEqual("C(3)", c.description)
  expectEqual("C[3]", c.debugDescription)
  expectEqual("C(3)", String(describing: c))
  expectEqual("C(3)", c.describe())
  expectEqual("C[3]", c.debugDescribe())
  expectEqual("C(3)", describing(c))
  expectEqual("C[3]", debugDescribing(c))
  expectPrinted("C(3)", c)
  expectDebugPrinted("C[3]", c)

  let wrapped = Noncopyable(wrapping: c)
  expectEqual("N(C(3))", wrapped.description)
}

runAllTests()
