//===--- StringUTF8StorageProperty.swift ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-stdlib-swift

// REQUIRES: executable_test

// Blocked by rdar://181604244 (opaque values borrow accessors)
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

import StdlibUnittest

var suite = TestSuite("StringUTF8StorageProperty")
defer { runAllTests() }

@available(SwiftStdlib 6.2, *)
func byteCount(_ span: UTF8Span?) -> Int? {
  span?.count
}

@available(SwiftStdlib 6.2, *)
func byteCount(_ span: Span<UInt8>?) -> Int? {
  span?.count
}

@available(SwiftStdlib 6.2, *)
@inline(never)
func utf8Count(of string: String?) -> Int? {
  byteCount(string?.utf8Span)
}

@available(SwiftStdlib 6.2, *)
@inline(never)
func utf8Count(ofInout string: inout String?) -> Int? {
  byteCount(string?.utf8Span)
}

struct Name {
  var string: String
}

struct Person {
  var name: Name
}

@available(SwiftStdlib 6.2, *)
@inline(never)
func utf8Count(ofName name: Name?) -> Int? {
  byteCount(name?.string.utf8Span)
}

@available(SwiftStdlib 6.2, *)
@inline(never)
func utf8Count(ofInoutName name: inout Name?) -> Int? {
  byteCount(name?.string.utf8Span)
}

@available(SwiftStdlib 6.2, *)
@inline(never)
func utf8Count(ofPerson person: Person?) -> Int? {
  byteCount(person?.name.string.utf8Span)
}

suite.test("Span from Small String")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  var s = "123456789".utf8
  let u = Array(s)
#if os(watchOS) && _pointerBitWidth(_32)
  expectNil(s._span)
#endif
  var string = String(s)
  string.reserveCapacity(12)
  s = (consume string).utf8
  guard let span = expectNotNil(s._span) else { return }

  let count = span.count
  expectEqual(count, s.count)

  for i in span.indices {
    expectEqual(span[i], u[i])
  }
}

suite.test("Span from Large Native String")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let s = "A long string that is altogether not smol.".utf8
  let u = Array(s)
  guard let span = expectNotNil(s._span) else { return }

  let count = span.count
  expectEqual(count, s.count)

  for i in span.indices {
    expectEqual(span[i], u[i])
  }
}

suite.test("Span from Small String's Substring")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let s = "A small string.".dropFirst(8).utf8
  let u = Array("string.".utf8)
  guard let span = expectNotNil(s._span) else { return }

  let count = span.count
  expectEqual(count, s.count)

  for i in span.indices {
    expectEqual(span[i], u[i])
  }
}

suite.test("Span from Large Native String's Substring")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let t = "A long string that is altogether not smol."
  let s = t.dropFirst(22).prefix(10).utf8
  let u = Array("altogether".utf8)
  guard let span = expectNotNil(s._span) else { return }

  let count = span.count
  expectEqual(count, s.count)

  for i in span.indices {
    expectEqual(span[i], u[i])
  }
}

suite.test("Span from String.utf8Span")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let s = String(200)
  guard let utf8span = expectNotNil(s._utf8Span) else { return }
  let span1 = utf8span.span
  let utf8view = s.utf8
  guard let span2 = expectNotNil(utf8view._span) else { return }
  expectEqual(span1.count, span2.count)
  for (i,j) in zip(span1.indices, span2.indices) {
    expectEqual(span1[i], span2[j])
  }
}

suite.test("UTF8Span from Span")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let s = String(200).utf8
  guard let span1 = expectNotNil(s._span) else { return }
  guard let utf8 = expectNotNil(try? UTF8Span(validating: span1)) else { return }

  let span2 = utf8.span
  expectTrue(span1.isIdentical(to: span2))
}

suite.test("Span from Substring.utf8Span")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let s = String(22000).dropFirst().dropLast()
  guard let utf8span = expectNotNil(s._utf8Span) else { return }
  let span1 = utf8span.span
  let utf8view = s.utf8
  guard let span2 = expectNotNil(utf8view._span) else { return }
  expectEqual(span1.count, span2.count)
  for (i,j) in zip(span1.indices, span2.indices) {
    expectEqual(span1[i], span2[j])
  }
}

suite.test("UTF8Span through an optional chain")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let small: String? = "abc"
  let large: String? = String(repeating: "x", count: 100)
  expectEqual(utf8Count(of: small), 3)
  expectEqual(utf8Count(of: large), 100)
  expectNil(utf8Count(of: nil))

  var local = small
  local = large
  expectEqual(byteCount(local?.utf8Span), 100)
  local = small
  expectEqual(utf8Count(ofInout: &local), 3)
  local = nil
  expectNil(utf8Count(ofInout: &local))
}

suite.test("Span through an optional chain of UTF8Span")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  func count(of string: String?) -> Int? {
    byteCount(string?.utf8Span.span)
  }

  expectEqual(count(of: "abc"), 3)
  expectEqual(count(of: String(repeating: "x", count: 100)), 100)
  expectNil(count(of: nil))
}

suite.test("UTF8Span through stored properties after an optional binding")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let small = Name(string: "abc")
  let large = Name(string: String(repeating: "x", count: 100))
  expectEqual(utf8Count(ofName: small), 3)
  expectEqual(utf8Count(ofName: large), 100)
  expectNil(utf8Count(ofName: nil))

  var local: Name? = small
  expectEqual(utf8Count(ofInoutName: &local), 3)
  local = large
  expectEqual(utf8Count(ofInoutName: &local), 100)
  local = nil
  expectNil(utf8Count(ofInoutName: &local))

  expectEqual(utf8Count(ofPerson: Person(name: small)), 3)
  expectEqual(utf8Count(ofPerson: Person(name: large)), 100)
  expectNil(utf8Count(ofPerson: nil))
}
