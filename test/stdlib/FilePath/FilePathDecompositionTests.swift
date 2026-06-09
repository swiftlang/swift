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
// the result equals the original. Picks the host platform's column from
// the `PathTestCase` table at compile time.
@available(SwiftStdlib 9999, *)
func runCase(_ tc: PathTestCase) {
#if canImport(Darwin)
  let expected = tc.darwin
#elseif os(Windows)
  let expected = tc.windows
#else
  let expected = tc.linux
#endif

  let path = FilePath(tc.input)!

  // anchor
  let anchorDesc = path.anchor?.description
  expectEqual(anchorDesc, expected.anchor,
    "input=\(tc.input.debugDescription) anchor: got \(anchorDesc.debugDescription), expected \(expected.anchor.debugDescription)")

  // components
  let compDescs = path.components.map(\.description)
  expectEqual(compDescs, expected.components,
    "input=\(tc.input.debugDescription) components: got \(compDescs), expected \(expected.components)")

  // hasTrailingSeparator
  expectEqual(path.hasTrailingSeparator, expected.hasTrailingSeparator,
    "input=\(tc.input.debugDescription) hasTrailingSep: got \(path.hasTrailingSeparator), expected \(expected.hasTrailingSeparator)")

  // isResourceFork (Darwin)
#if canImport(Darwin)
  expectEqual(path.isResourceFork, expected.isResourceFork,
    "input=\(tc.input.debugDescription) isResourceFork: got \(path.isResourceFork), expected \(expected.isResourceFork)")
#endif

  // printed
  expectEqual(path.description, expected.printed,
    "input=\(tc.input.debugDescription) printed: got \(path.description.debugDescription), expected \(expected.printed.debugDescription)")

  // isAbsolute
  expectEqual(path.isAbsolute, expected.isAbsolute,
    "input=\(tc.input.debugDescription) isAbsolute: got \(path.isAbsolute), expected \(expected.isAbsolute)")

  // isRooted
  let expectedRooted = expected.isRooted ?? expected.isAbsolute
  let actualRooted = path.anchor?.isRooted ?? false
  expectEqual(actualRooted, expectedRooted,
    "input=\(tc.input.debugDescription) isRooted: got \(actualRooted), expected \(expectedRooted)")

  // driveLetter
  if let expectedDrive = expected.driveLetter {
    expectTrue(path.anchor?._driveLetter == expectedDrive,
      "input=\(tc.input.debugDescription) driveLetter: got \(path.anchor?._driveLetter.debugDescription ?? "nil"), expected \(expectedDrive)")
  }

  // kinds
  let actualKinds = path.components.map(\.kind)
  if let expectedKinds = expected.kinds {
    expectEqual(actualKinds, expectedKinds,
      "input=\(tc.input.debugDescription) kinds: got \(actualKinds), expected \(expectedKinds)")
  } else {
    let allRegular = actualKinds.allSatisfy { $0 == .regular }
    expectTrue(allRegular,
      "input=\(tc.input.debugDescription) kinds: expected all .regular, got \(actualKinds)")
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
    "input=\(tc.input.debugDescription) round-trip failed: got \(roundTrip.description.debugDescription), expected \(path.description.debugDescription)")
}

// Pin the four primary decomposition fields (anchor, components,
// hasTrailingSeparator, printed) for `input`. Used by the edge-case tests
// below. The `file` / `line` defaults forward the caller's source location
// so a failure points at the assertion row rather than this helper.
@available(SwiftStdlib 9999, *)
func expectDecomposition(
  _ input: String,
  anchor: String?,
  components: [String],
  trailingSeparator: Bool = false,
  printed: String,
  file: String = #file, line: UInt = #line
) {
  guard let path = FilePath(input) else {
    expectTrue(false,
      "\(input.debugDescription) FilePath init returned nil",
      file: file, line: line)
    return
  }
  expectEqual(path.anchor?.description, anchor,
    "\(input.debugDescription) anchor: got \(path.anchor?.description.debugDescription ?? "nil")",
    file: file, line: line)
  expectEqual(path.components.map(\.description), components,
    "\(input.debugDescription) components: got \(path.components.map(\.description))",
    file: file, line: line)
  expectEqual(path.hasTrailingSeparator, trailingSeparator,
    "\(input.debugDescription) trailingSeparator: got \(path.hasTrailingSeparator)",
    file: file, line: line)
  expectEqual(path.description, printed,
    "\(input.debugDescription) printed: got \(path.description.debugDescription)",
    file: file, line: line)
}

@main
struct FilePathDecompositionTests {
  static func main() {
    let suite = TestSuite("FilePath.Decomposition")

    // MARK: - Full-table run
    //
    // The shared `pathTestCases` table carries expectations for all three
    // platforms; `runCase` consumes the host platform's column.

    suite.test("allCases")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      for tc in pathTestCases { runCase(tc) } }

    // MARK: - Edge cases
    //
    // Each test pins the decomposition of a degenerate / boundary input that
    // the table-driven `pathTestCases` doesn't cover. Inputs are grouped by
    // the structural pattern they exercise; each test body is gated to the
    // platform whose parser the case characterizes.

    suite.test("darwinRelativeReparse")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
#if canImport(Darwin)
      // .vol token as a relative component under a volfs anchor.
      expectDecomposition("/.vol/1234/5678/.vol/x",
        anchor: "/.vol/1234/5678", components: [".vol", "x"],
        printed: "/.vol/1234/5678/.vol/x")
      // .nofollow token as a relative component under a volfs anchor.
      expectDecomposition("/.vol/1234/5678/.nofollow/x",
        anchor: "/.vol/1234/5678", components: [".nofollow", "x"],
        printed: "/.vol/1234/5678/.nofollow/x")
      // .resolve/3 tokens as relative components under a volfs anchor.
      expectDecomposition("/.vol/1234/5678/.resolve/3/x",
        anchor: "/.vol/1234/5678", components: [".resolve", "3", "x"],
        printed: "/.vol/1234/5678/.resolve/3/x")
      // .vol AFTER .nofollow forms a combined anchor.
      expectDecomposition("/.nofollow/.vol/1234/5678",
        anchor: "/.nofollow/.vol/1234/5678", components: [],
        printed: "/.nofollow/.vol/1234/5678")
      // .nofollow token as a relative component under a .nofollow anchor.
      expectDecomposition("/.nofollow/.nofollow/x",
        anchor: "/.nofollow/", components: [".nofollow", "x"],
        printed: "/.nofollow/.nofollow/x")
      // Inner `.resolve/1` is NOT canonicalized: canonicalization is anchor-only.
      expectDecomposition("/.resolve/3/.resolve/1/x",
        anchor: "/.resolve/3/", components: [".resolve", "1", "x"],
        printed: "/.resolve/3/.resolve/1/x")
#endif
    }

    suite.test("bareResolveVolAnchors")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
#if canImport(Darwin)
      // Bare `/.resolve/1` is NOT a resolve anchor — `_parseResolve` requires
      // the trailing slash, so this falls through to a plain `/` root.
      expectDecomposition("/.resolve/1",
        anchor: "/", components: [".resolve", "1"],
        printed: "/.resolve/1")
      // `/.resolve/1/` canonicalizes to `/.nofollow/`.
      expectDecomposition("/.resolve/1/",
        anchor: "/.nofollow/", components: [],
        printed: "/.nofollow/")
      // Bare `/.resolve/3` (non-canonicalizing flag) — same fall-through.
      expectDecomposition("/.resolve/3",
        anchor: "/", components: [".resolve", "3"],
        printed: "/.resolve/3")
      // `/.vol/1234/2/` — `2`->`@` canonicalization fires; the trailing slash
      // is a separator (the volfs anchor itself is not slash-terminated).
      expectDecomposition("/.vol/1234/2/",
        anchor: "/.vol/1234/@", components: [], trailingSeparator: true,
        printed: "/.vol/1234/@/")
#elseif !os(Windows)
      // Linux contrast: no Darwin anchor magic at all.
      expectDecomposition("/.resolve/1",
        anchor: "/", components: [".resolve", "1"],
        printed: "/.resolve/1")
#endif
    }

    suite.test("windowsThreePlusBackslashes")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
#if os(Windows)
      expectDecomposition(#"\\\server\share"#,
        anchor: #"\"#, components: ["server", "share"],
        printed: #"\server\share"#)
      expectDecomposition(#"\\\\server"#,
        anchor: #"\"#, components: ["server"],
        printed: #"\server"#)
      expectDecomposition(#"\\\"#,
        anchor: #"\"#, components: [],
        printed: #"\"#)
#endif
    }

    suite.test("windowsDegenerateDeviceSigil")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
#if os(Windows)
      expectDecomposition(#"\\."#,
        anchor: #"\\.\"#, components: [],
        printed: #"\\.\"#)
      expectDecomposition(#"\\?"#,
        anchor: #"\\?\"#, components: [],
        printed: #"\\?\"#)

      // Same inputs, additional structural assertions.
      let dotInput: String = #"\\."#
      let dot = FilePath(dotInput)!
      expectTrue(dot.isAbsolute, #"\\. is absolute"#)
      expectFalse(dot.anchor?._isVerbatimComponent ?? true,
        #"\\. is device-namespace, not verbatim"#)

      let qInput: String = #"\\?"#
      let q = FilePath(qInput)!
      expectTrue(q.isAbsolute, #"\\? is absolute"#)
      expectTrue(q.anchor?._isVerbatimComponent ?? false,
        #"\\? is verbatim-component"#)
#endif
    }

    suite.test("matchesPrefixNearMisses")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
#if canImport(Darwin)
      expectDecomposition("/.nofollowing/bar",
        anchor: "/", components: [".nofollowing", "bar"],
        printed: "/.nofollowing/bar")
      expectDecomposition("/.resolved/x",
        anchor: "/", components: [".resolved", "x"],
        printed: "/.resolved/x")
      expectDecomposition("/.volume/x",
        anchor: "/", components: [".volume", "x"],
        printed: "/.volume/x")
      expectDecomposition("/.resolvex/1/y",
        anchor: "/", components: [".resolvex", "1", "y"],
        printed: "/.resolvex/1/y")
      // Keyword present but missing the structural slashes.
      expectDecomposition("/.vol",
        anchor: "/", components: [".vol"],
        printed: "/.vol")
      expectDecomposition("/.vol/1234",
        anchor: "/", components: [".vol", "1234"],
        printed: "/.vol/1234")
#endif
    }

    runAllTests()
  }
}
