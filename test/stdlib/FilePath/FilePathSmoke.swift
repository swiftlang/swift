//===--- FilePathSmoke.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s %S/Inputs/FilePathTestSupport.swift -o %t/a.out -module-name main
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest

@main
struct FilePathSmoke {
  static func main() {
    let suite = TestSuite("FilePath.Smoke")

    suite.test("public API")
    .skip(.custom(
      { if #available(SwiftStdlib 9999, *) { false } else { true } },
      reason: "Requires SwiftStdlib 9999"))
    .code {
      guard #available(SwiftStdlib 9999, *) else { return }
      let p = FilePath("/usr/local/bin")!
      expectEqual(p.components.map(\.description), ["usr", "local", "bin"])
      expectEqual(p.description, universal("/usr/local/bin"))
      expectNotNil(p.anchor)
    }

    runAllTests()
  }
}
