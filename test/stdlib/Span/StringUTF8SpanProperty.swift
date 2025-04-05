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

// RUN: %target-run-stdlib-swift(-enable-experimental-feature LifetimeDependence -enable-experimental-feature AddressableTypes)

// REQUIRES: executable_test
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_AddressableTypes

import StdlibUnittest

var suite = TestSuite("StringUTF8StorageProperty")
defer { runAllTests() }

suite.test("Span from Small String")
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let s = "A small string.".utf8
  let u = Array(s)
  expectCrashLater()
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
.skip(.wasiAny(reason: "Trap tests aren't supported on WASI."))
.require(.stdlib_6_2).code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  let s = "A small string.".dropFirst(8).utf8
  let u = Array("string.".utf8)
  expectCrashLater()
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
