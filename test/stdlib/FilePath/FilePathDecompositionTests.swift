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

// Ported from the SE-0529 reference suite's `DecompositionTests.swift`. Free
// function (no wrapping struct), prefixed `@available(SwiftStdlib 9999, *)`,
// otherwise verbatim — the package's `expect*` seam helpers map onto
// StdlibUnittest's free `expect*` functions of the same name with the same
// (value, value, message) shape.
@available(SwiftStdlib 9999, *)
func runCase(_ tc: PathTestCase, platform: REVIEW_ONLY_Platform) {
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

// Ported from the SE-0529 reference suite's `ProbeTests.swift`. Free function,
// prefixed `@available(SwiftStdlib 9999, *)`. Source-location forwarding uses
// StdlibUnittest's `file: String = #file, line: UInt = #line` convention. The
// "force a failure" expectNotNil(Optional<Int>.none, ...) idiom from the
// package becomes `expectTrue(false, ...)` here.
@available(SwiftStdlib 9999, *)
func probe(
  _ input: String,
  platform: REVIEW_ONLY_Platform,
  anchor: String?,
  components: [String],
  trailingSeparator: Bool = false,
  printed: String,
  file: String = #file, line: UInt = #line
) {
  withPlatform(platform) {
    guard let path = FilePath(input) else {
      expectTrue(false,
        "[\(platform)] \(input.debugDescription) FilePath init returned nil",
        file: file, line: line)
      return
    }
    expectEqual(path.anchor?.description, anchor,
      "[\(platform)] \(input.debugDescription) anchor: got \(path.anchor?.description.debugDescription ?? "nil")",
      file: file, line: line)
    expectEqual(path.components.map(\.description), components,
      "[\(platform)] \(input.debugDescription) components: got \(path.components.map(\.description))",
      file: file, line: line)
    expectEqual(path.hasTrailingSeparator, trailingSeparator,
      "[\(platform)] \(input.debugDescription) trailingSeparator: got \(path.hasTrailingSeparator)",
      file: file, line: line)
    expectEqual(path.description, printed,
      "[\(platform)] \(input.debugDescription) printed: got \(path.description.debugDescription)",
      file: file, line: line)
  }
}

@main
struct FilePathDecompositionTests {
  static func main() {
    let suite = TestSuite("FilePath.Decomposition")

    // MARK: - Full-table runs (one per platform)
    //
    // On any single build only the matching platform's body does work; the
    // other two run inert via withPlatform and pass vacuously.

    suite.test("allCasesLinux")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      for tc in pathTestCases { runCase(tc, platform: .linux) } }

    suite.test("allCasesDarwin")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      for tc in pathTestCases { runCase(tc, platform: .darwin) } }

    suite.test("allCasesWindows")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      for tc in pathTestCases { runCase(tc, platform: .windows) } }

    // MARK: - Probes (verbatim sequences from package ProbeTests.swift)

    suite.test("probeDarwinRelativeReparse")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // .vol token as a relative component under a volfs anchor.
      probe("/.vol/1234/5678/.vol/x", platform: .darwin,
        anchor: "/.vol/1234/5678", components: [".vol", "x"],
        printed: "/.vol/1234/5678/.vol/x")
      // .nofollow token as a relative component under a volfs anchor.
      probe("/.vol/1234/5678/.nofollow/x", platform: .darwin,
        anchor: "/.vol/1234/5678", components: [".nofollow", "x"],
        printed: "/.vol/1234/5678/.nofollow/x")
      // .resolve/3 tokens as relative components under a volfs anchor.
      probe("/.vol/1234/5678/.resolve/3/x", platform: .darwin,
        anchor: "/.vol/1234/5678", components: [".resolve", "3", "x"],
        printed: "/.vol/1234/5678/.resolve/3/x")
      // .vol AFTER .nofollow forms a combined anchor.
      probe("/.nofollow/.vol/1234/5678", platform: .darwin,
        anchor: "/.nofollow/.vol/1234/5678", components: [],
        printed: "/.nofollow/.vol/1234/5678")
      // .nofollow token as a relative component under a .nofollow anchor.
      probe("/.nofollow/.nofollow/x", platform: .darwin,
        anchor: "/.nofollow/", components: [".nofollow", "x"],
        printed: "/.nofollow/.nofollow/x")
      // Inner `.resolve/1` is NOT canonicalized: canonicalization is anchor-only.
      probe("/.resolve/3/.resolve/1/x", platform: .darwin,
        anchor: "/.resolve/3/", components: [".resolve", "1", "x"],
        printed: "/.resolve/3/.resolve/1/x") }

    suite.test("probeBareResolveVolAnchors")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // GAP: bare `/.resolve/1` is NOT a resolve anchor.
      probe("/.resolve/1", platform: .darwin,
        anchor: "/", components: [".resolve", "1"],
        printed: "/.resolve/1")
      // SPECIFIED: `/.resolve/1/` canonicalizes to `/.nofollow/`.
      probe("/.resolve/1/", platform: .darwin,
        anchor: "/.nofollow/", components: [],
        printed: "/.nofollow/")
      // GAP: bare `/.resolve/3` (non-canonicalizing flag).
      probe("/.resolve/3", platform: .darwin,
        anchor: "/", components: [".resolve", "3"],
        printed: "/.resolve/3")
      // GAP-adjacent: `/.vol/1234/2/` — `2`->`@` fires; trailing slash is sep.
      probe("/.vol/1234/2/", platform: .darwin,
        anchor: "/.vol/1234/@", components: [], trailingSeparator: true,
        printed: "/.vol/1234/@/")
      // Linux contrast: no Darwin anchor magic at all.
      probe("/.resolve/1", platform: .linux,
        anchor: "/", components: [".resolve", "1"],
        printed: "/.resolve/1") }

    suite.test("probeWindowsThreePlusBackslashes")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      probe(#"\\\server\share"#, platform: .windows,
        anchor: #"\"#, components: ["server", "share"],
        printed: #"\server\share"#)
      probe(#"\\\\server"#, platform: .windows,
        anchor: #"\"#, components: ["server"],
        printed: #"\server"#)
      probe(#"\\\"#, platform: .windows,
        anchor: #"\"#, components: [],
        printed: #"\"#) }

    suite.test("probeWindowsDegenerateDeviceSigil")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      probe(#"\\."#, platform: .windows,
        anchor: #"\\.\"#, components: [],
        printed: #"\\.\"#)
      probe(#"\\?"#, platform: .windows,
        anchor: #"\\?\"#, components: [],
        printed: #"\\?\"#)

      // Extra observed properties for the same inputs.
      withPlatform(.windows) {
        let dotInput: String = #"\\."#
        let dot = FilePath(dotInput)!
        expectTrue(dot.isAbsolute, #"\\. should be absolute (observed)"#)
        expectFalse(dot.anchor?._isVerbatimComponent ?? true,
          #"\\. is device-namespace, not verbatim (observed)"#)

        let qInput: String = #"\\?"#
        let q = FilePath(qInput)!
        expectTrue(q.isAbsolute, #"\\? should be absolute (observed)"#)
        expectTrue(q.anchor?._isVerbatimComponent ?? false,
          #"\\? becomes verbatim-component (observed)"#)
      } }

    suite.test("probeMatchesPrefixNearMisses")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      probe("/.nofollowing/bar", platform: .darwin,
        anchor: "/", components: [".nofollowing", "bar"],
        printed: "/.nofollowing/bar")
      probe("/.resolved/x", platform: .darwin,
        anchor: "/", components: [".resolved", "x"],
        printed: "/.resolved/x")
      probe("/.volume/x", platform: .darwin,
        anchor: "/", components: [".volume", "x"],
        printed: "/.volume/x")
      probe("/.resolvex/1/y", platform: .darwin,
        anchor: "/", components: [".resolvex", "1", "y"],
        printed: "/.resolvex/1/y")
      // Keyword present but missing the structural slashes.
      probe("/.vol", platform: .darwin,
        anchor: "/", components: [".vol"],
        printed: "/.vol")
      probe("/.vol/1234", platform: .darwin,
        anchor: "/", components: [".vol", "1234"],
        printed: "/.vol/1234") }

    runAllTests()
  }
}
