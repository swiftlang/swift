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

// RUN: %target-run-stdlib-swift -enable-experimental-feature LifetimeDependence

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_feature_LifetimeDependence

import StdlibUnittest

import Foundation

var suite = TestSuite("EagerLazyBridging String Tests")
defer { runAllTests() }

let strings = [
  "Hello, World!",
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

    let string = NSString(utf8String: expected)
    guard let string else { expectNotNil(string); return }

    let bridged = String(string).utf8
    var p: ObjectIdentifier? = nil
    for (i, j) in zip(0..<3, bridged.indices) {
      let span = bridged.span
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

    let string = NSString(utf8String: expected)
    guard let string else { expectNotNil(string); return }

    let bridged = String(string)
    let utf8 = bridged.utf8
    let span = utf8.span
    expectEqual(span.count, expected.utf8.count)
    for (i,j) in zip(span.indices, expected.utf8.indices) {
      expectEqual(span[i], expected.utf8[j])
    }
  }
}

strings.forEach { expected in
  suite.test("Span from Bridged String Substring: \(expected)")
  .require(.stdlib_6_2).code {
    guard #available(SwiftStdlib 6.2, *) else { return }

    let string = NSString(utf8String: expected)
    guard let string else { expectNotNil(string); return }

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
