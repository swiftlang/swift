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

// RUN: %target-run-stdlib-swift(-enable-experimental-feature LifetimeDependence -enable-experimental-feature Span -enable-experimental-feature AddressableTypes)

// REQUIRES: executable_test

import StdlibUnittest

var suite = TestSuite("StringUTF8StorageProperty")
defer { runAllTests() }

suite.test("Span from Small String")
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let s = "A small string.".utf8
  let u = Array(s)
  let span = s.span

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
  let span = s.span

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
  let span = s.span

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
  let span = s.span

  let count = span.count
  expectEqual(count, s.count)

  for i in span.indices {
    expectEqual(span[i], u[i])
  }
}

import Foundation

let strings: [NSString: String] = [
  "Hello, World!" as NSString: "Hello, World!",
  "A long ASCII string exceeding 16 code units." as NSString: "A long ASCII string exceeding 16 code units.",
  "🇯🇵" as NSString: "🇯🇵",
  NSString(utf8String: "🏂☃❅❆❄︎⛄️❄️")!: "🏂☃❅❆❄︎⛄️❄️",
]

strings.forEach { string, expected in
  suite.test("Span from Bridged String: \(expected)")
  .require(.stdlib_6_2).code {
    guard #available(SwiftStdlib 6.2, *) else { return }

    let bridged = String(string)
    let utf8 = bridged.utf8
    let span = utf8.span
    expectEqual(span.count, expected.utf8.count)
    for (i,j) in zip(span.indices, expected.utf8.indices) {
      expectEqual(span[i], expected.utf8[j])
    }
  }
}

strings.forEach { string, expected in
  suite.test("Span from Bridged String Substring: \(expected)")
  .require(.stdlib_6_2).code {
    guard #available(SwiftStdlib 6.2, *) else { return }

    let bridged = String(string).dropFirst()
    let utf8 = bridged.utf8
    let span = utf8.span
    let expected = expected.dropFirst()
    expectEqual(span.count, expected.utf8.count)
    for (i,j) in zip(span.indices, expected.utf8.indices) {
      expectEqual(span[i], expected.utf8[j])
    }
  }
}
