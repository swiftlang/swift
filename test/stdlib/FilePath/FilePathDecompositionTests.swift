//===--- FilePathDecompositionTests.swift ---------------------------------===//
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
// RUN: %target-build-swift %s %S/Inputs/FilePathTestSupport.swift %S/Inputs/TestHelpers.swift %S/Inputs/PathTestCases.swift -o %t/a.out -module-name main
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest

// Drive the per-platform `Expected` decomposition for `tc` through `expect*`
// assertions, then reconstruct via the decomposition initializer and check
// the result equals the original. Goes through `withPlatform(p)` so a build
// targeting a different platform passes vacuously rather than asserting
// against another platform's expected values.
@available(SwiftStdlib 9999, *)
func runCase(_ tc: PathTestCase, platform: _Platform) {
  withPlatform(platform) {
    let expected: Expected
    switch platform {
    case .linux: expected = tc.linux
    case .darwin: expected = tc.darwin
    case .windows: expected = tc.windows
    }

    let path = FilePath(tc.input)!

    // anchor
    let anchorDesc = path.anchor?.description
    expectEqual(anchorDesc, expected.anchor,
      "[\(platform)] input=\(tc.input.debugDescription) anchor: got \(anchorDesc.debugDescription), expected \(expected.anchor.debugDescription)")

    // components
    let compDescs = path.components.map(\.description)
    expectEqual(compDescs, expected.components,
      "[\(platform)] input=\(tc.input.debugDescription) components: got \(compDescs), expected \(expected.components)")

    // hasTrailingSeparator
    expectEqual(path.hasTrailingSeparator, expected.hasTrailingSeparator,
      "[\(platform)] input=\(tc.input.debugDescription) hasTrailingSep: got \(path.hasTrailingSeparator), expected \(expected.hasTrailingSeparator)")

    // isResourceFork (Darwin)
    if platform == .darwin {
      expectEqual(path.isResourceFork, expected.isResourceFork,
        "[\(platform)] input=\(tc.input.debugDescription) isResourceFork: got \(path.isResourceFork), expected \(expected.isResourceFork)")
    }

    // printed
    expectEqual(path.description, expected.printed,
      "[\(platform)] input=\(tc.input.debugDescription) printed: got \(path.description.debugDescription), expected \(expected.printed.debugDescription)")

    // isAbsolute
    expectEqual(path.isAbsolute, expected.isAbsolute,
      "[\(platform)] input=\(tc.input.debugDescription) isAbsolute: got \(path.isAbsolute), expected \(expected.isAbsolute)")

    // isRooted
    let expectedRooted = expected.isRooted ?? expected.isAbsolute
    let actualRooted = path.anchor?.isRooted ?? false
    expectEqual(actualRooted, expectedRooted,
      "[\(platform)] input=\(tc.input.debugDescription) isRooted: got \(actualRooted), expected \(expectedRooted)")

    // driveLetter
    if let expectedDrive = expected.driveLetter {
      expectTrue(path.anchor?._driveLetter == expectedDrive,
        "[\(platform)] input=\(tc.input.debugDescription) driveLetter: got \(path.anchor?._driveLetter.debugDescription ?? "nil"), expected \(expectedDrive)")
    }

    // kinds
    let actualKinds = path.components.map(\.kind)
    if let expectedKinds = expected.kinds {
      expectEqual(actualKinds, expectedKinds,
        "[\(platform)] input=\(tc.input.debugDescription) kinds: got \(actualKinds), expected \(expectedKinds)")
    } else {
      let allRegular = actualKinds.allSatisfy { $0 == .regular }
      expectTrue(allRegular,
        "[\(platform)] input=\(tc.input.debugDescription) kinds: expected all .regular, got \(actualKinds)")
    }

    // Round-trip: reconstruct from decomposition
    let roundTrip: FilePath
    if expected.isResourceFork {
      roundTrip = FilePath(
        anchor: path.anchor,
        path.components,
        resourceFork: true)
    } else {
      roundTrip = FilePath(
        anchor: path.anchor,
        path.components,
        hasTrailingSeparator: path.hasTrailingSeparator)
    }
    expectEqual(roundTrip, path,
      "[\(platform)] input=\(tc.input.debugDescription) round-trip failed: got \(roundTrip.description.debugDescription), expected \(path.description.debugDescription)")
  }
}

@main
struct FilePathDecompositionTests {
  static func main() {
    let suite = TestSuite("FilePath.Decomposition")

    suite.test("allCasesLinux")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      for tc in pathTestCases { runCase(tc, platform: .linux) }
    }

    suite.test("allCasesDarwin")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      for tc in pathTestCases { runCase(tc, platform: .darwin) }
    }

    suite.test("allCasesWindows")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      for tc in pathTestCases { runCase(tc, platform: .windows) }
    }

    runAllTests()
  }
}
