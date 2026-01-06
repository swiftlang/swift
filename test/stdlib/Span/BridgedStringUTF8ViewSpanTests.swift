//===--- BridgedStringSpanTests.swift -------------------------------------===//
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
// REQUIRES: objc_interop

import StdlibUnittest

import Foundation

var suite = TestSuite("EagerLazyBridging String Tests")
defer { runAllTests() }

let strings = [
  "Hello, World!",
  "123456789",
  "A long ASCII string exceeding 16 code units.",
  "🇯🇵",
  "🏂☃❅❆❄︎⛄️❄️",
  "z",
  "",
]

strings.forEach { expected in
  suite.test("Span from Bridged String Stability: \(expected)")
  .require(.stdlib_6_2).code {
    guard #available(SwiftStdlib 6.2, *) else { return }

    guard let nss = expectNotNil(NSString(utf8String: expected)) else { return }

    let bridged = String(nss).utf8
    var p: ObjectIdentifier? = nil
    for (i, j) in zip(0..<3, bridged.indices) {
      guard let span = expectNotNil(bridged._span) else { continue }
      let c = span.withUnsafeBufferPointer {
        let o = unsafeBitCast($0.baseAddress, to: ObjectIdentifier.self)
        if p == nil {
          p = o
        } else {
          expectEqual(p, o)
        }
        return $0[i]
      }
      expectEqual(c, bridged[j])
    }
  }
}

strings.forEach { expected in
  suite.test("Span from Bridged String: \(expected)")
  .require(.stdlib_6_2).code {
    guard #available(SwiftStdlib 6.2, *) else { return }

    guard let nss = expectNotNil(NSString(utf8String: expected)) else { return }

    let bridged = String(nss)
    let utf8 = bridged.utf8
    guard let span = expectNotNil(utf8._span) else { return }
    expectEqual(span.count, expected.utf8.count)
    for (i,j) in zip(span.indices, expected.utf8.indices) {
      expectEqual(span[i], expected.utf8[j])
    }
  }
}

strings.forEach { expected in
  suite.test("UTF8Span from Bridged String: \(expected)")
  .require(.stdlib_6_2).code {
    guard #available(SwiftStdlib 6.2, *) else { return }

    guard let nss = expectNotNil(NSString(utf8String: expected)) else { return }

    let bridged = String(nss)
    guard let utf8 = expectNotNil(bridged._utf8Span) else { return }
    expectEqual(utf8.count, expected.utf8.count)
    for (i,j) in zip(utf8.span.indices, expected.utf8.indices) {
      expectEqual(utf8.span[i], expected.utf8[j])
    }
  }
}

strings.forEach { expected in
  suite.test("Span from Bridged String Substring: \(expected)")
  .require(.stdlib_6_2).code {
    guard #available(SwiftStdlib 6.2, *) else { return }

    guard let nss = expectNotNil(NSString(utf8String: expected)) else { return }

    let bridged = String(nss).dropFirst()
    let utf8 = bridged.utf8
    guard let span = expectNotNil(utf8._span) else { return }
    let expected = expected.dropFirst()
    expectEqual(span.count, expected.utf8.count)
    for (i,j) in zip(span.indices, expected.utf8.indices) {
      expectEqual(span[i], expected.utf8[j])
    }
  }

  strings.forEach { expected in
    suite.test("UTF8Span from Bridged String Substring: \(expected)")
    .require(.stdlib_6_2).code {
      guard #available(SwiftStdlib 6.2, *) else { return }

      guard let nss = expectNotNil(NSString(utf8String: expected)) else { return }

      let bridged = String(nss).dropFirst()
      guard let utf8 = expectNotNil(bridged._utf8Span) else { return }
      let expected = expected.dropFirst()
      expectEqual(utf8.count, expected.utf8.count)
      for (i,j) in zip(utf8.span.indices, expected.utf8.indices) {
        expectEqual(utf8.span[i], expected.utf8[j])
      }
    }
  }
}
