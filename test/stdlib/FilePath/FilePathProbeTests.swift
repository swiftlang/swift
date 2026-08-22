//===--- FilePathProbeTests.swift -----------------------------------------===//
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

// Edge-case decomposition probes: each test pins the decomposition of a
// degenerate / boundary input that the table-driven `pathTestCases` doesn't
// cover (Darwin anchor reparse, bare resolve/vol forms, Windows multi-
// backslash roots, Windows empty-device sigils, prefix near-misses).

/// Pin the four primary decomposition fields for `input` on `platform`.
@available(SwiftStdlib 9999, *)
func expectDecomposition(
  _ input: String,
  platform: _Platform,
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
struct FilePathProbeTests {
  static func main() {
    let suite = TestSuite("FilePath.Probes")

    // MARK: - Darwin relative-portion re-parse vs combined anchors
    //
    // `_normalizeDarwin` extracts the relative portion into a fresh string
    // and dot-normalizes it; that path re-runs `_parseRoot()` on the slice.
    // This pins which token-shaped sequences AFTER a leading anchor extend
    // the anchor and which fall back to plain components.
    suite.test("darwinRelativeReparse")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // .vol token as a relative component under a volfs anchor.
      expectDecomposition("/.vol/1234/5678/.vol/x", platform: .darwin,
        anchor: "/.vol/1234/5678", components: [".vol", "x"],
        printed: "/.vol/1234/5678/.vol/x")
      // .nofollow token as a relative component under a volfs anchor.
      expectDecomposition("/.vol/1234/5678/.nofollow/x", platform: .darwin,
        anchor: "/.vol/1234/5678", components: [".nofollow", "x"],
        printed: "/.vol/1234/5678/.nofollow/x")
      // .resolve/3 tokens as relative components under a volfs anchor.
      expectDecomposition("/.vol/1234/5678/.resolve/3/x", platform: .darwin,
        anchor: "/.vol/1234/5678", components: [".resolve", "3", "x"],
        printed: "/.vol/1234/5678/.resolve/3/x")
      // .vol AFTER .nofollow forms a combined anchor — proposal line 111:
      // an anchor may include resolve flags AND/OR a volume identifier.
      expectDecomposition("/.nofollow/.vol/1234/5678", platform: .darwin,
        anchor: "/.nofollow/.vol/1234/5678", components: [],
        printed: "/.nofollow/.vol/1234/5678")
      // .nofollow token as a relative component under a .nofollow anchor.
      expectDecomposition("/.nofollow/.nofollow/x", platform: .darwin,
        anchor: "/.nofollow/", components: [".nofollow", "x"],
        printed: "/.nofollow/.nofollow/x")
      // Inner `.resolve/1` is NOT canonicalized: canonicalization is anchor-
      // only, and here `.resolve/1` is in component position under a
      // `.resolve/3/` anchor.
      expectDecomposition("/.resolve/3/.resolve/1/x", platform: .darwin,
        anchor: "/.resolve/3/", components: [".resolve", "1", "x"],
        printed: "/.resolve/3/.resolve/1/x")
    }

    // MARK: - Bare resolve/vol anchors without trailing content
    //
    // The parser treats volfs anchors and resolve/nofollow anchors
    // asymmetrically:
    //   * `_parseVol` recognizes `/.vol/F/I` WITHOUT a trailing slash; the
    //     `2`->`@` canonicalization also fires without one.
    //   * `_parseResolve`/`_parseNofollow` REQUIRE the trailing slash; bare
    //     `/.resolve/N` and `/.nofollow` fall through to a plain `/` root
    //     with the bytes as regular components, and
    //     `_canonicalizeDarwinAnchor` does NOT fire on `/.resolve/1`.
    suite.test("bareResolveVolAnchors")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Bare `/.resolve/1` is NOT a resolve anchor — `_parseResolve`
      // requires the trailing slash, so this falls through to a plain `/`
      // root.
      expectDecomposition("/.resolve/1", platform: .darwin,
        anchor: "/", components: [".resolve", "1"],
        printed: "/.resolve/1")
      // `/.resolve/1/` canonicalizes to `/.nofollow/`. Here with NO
      // following path -> anchor only, no components.
      expectDecomposition("/.resolve/1/", platform: .darwin,
        anchor: "/.nofollow/", components: [],
        printed: "/.nofollow/")
      // Bare `/.resolve/3` (non-canonicalizing flag) — same fall-through.
      expectDecomposition("/.resolve/3", platform: .darwin,
        anchor: "/", components: [".resolve", "3"],
        printed: "/.resolve/3")
      // `/.vol/1234/2/` — the `2`->`@` canonicalization fires, and the
      // trailing slash is a separator (the volfs anchor itself is not
      // slash-terminated).
      expectDecomposition("/.vol/1234/2/", platform: .darwin,
        anchor: "/.vol/1234/@", components: [], trailingSeparator: true,
        printed: "/.vol/1234/@/")
      // Linux contrast for the same bytes: no Darwin anchor magic at all.
      expectDecomposition("/.resolve/1", platform: .linux,
        anchor: "/", components: [".resolve", "1"],
        printed: "/.resolve/1")
    }

    // MARK: - Windows three-or-more leading backslashes
    //
    // `_prenormalizeWindowsRoots` returns after the FIRST backslash for 3+
    // leading backslashes ("NOT a UNC/device path"), and separator
    // coalescing collapses the rest. Result: a single `\` current-drive
    // root with the remainder as components.
    suite.test("windowsThreePlusBackslashes")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      expectDecomposition(#"\\\server\share"#, platform: .windows,
        anchor: #"\"#, components: ["server", "share"],
        printed: #"\server\share"#)
      expectDecomposition(#"\\\\server"#, platform: .windows,
        anchor: #"\"#, components: ["server"],
        printed: #"\server"#)
      expectDecomposition(#"\\\"#, platform: .windows,
        anchor: #"\"#, components: [],
        printed: #"\"#)
    }

    // MARK: - Windows degenerate device/sigil forms
    //
    // A `\\.` or `\\?` with NO trailing backslash and NO device name gets a
    // backslash SYNTHESIZED by `_prenormalizeWindowsRoots`
    // (`expectBackslash` inserts one), yielding the empty-device anchors
    // `\\.\` / `\\?\`. Both are absolute; `\\?` becomes verbatim-component.
    suite.test("windowsDegenerateDeviceSigil")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      expectDecomposition(#"\\."#, platform: .windows,
        anchor: #"\\.\"#, components: [],
        printed: #"\\.\"#)
      expectDecomposition(#"\\?"#, platform: .windows,
        anchor: #"\\?\"#, components: [],
        printed: #"\\?\"#)

      // Same inputs, additional structural assertions. Bind through String
      // locals so the failable `init?(_:)` is selected (a bare string
      // literal binds the non-failable ExpressibleByStringLiteral init).
      withPlatform(.windows) {
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
      }
    }

    // MARK: - _matches* prefix pre-filter near-misses
    //
    // `_matchesNofollow`/`_matchesResolve`/`_matchesVol` are prefix
    // PRE-FILTERS; the real validation lives in `_parseNofollow`/
    // `_parseResolve`/`_parseVol`. A near-miss whose keyword is only a
    // PREFIX of the component (`/.nofollowing`, `/.resolved`, `/.volume`,
    // `/.resolvex`) therefore fails the parser and falls through to a
    // plain `/` root with the bytes as regular components.
    suite.test("matchesPrefixNearMisses")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      expectDecomposition("/.nofollowing/bar", platform: .darwin,
        anchor: "/", components: [".nofollowing", "bar"],
        printed: "/.nofollowing/bar")
      expectDecomposition("/.resolved/x", platform: .darwin,
        anchor: "/", components: [".resolved", "x"],
        printed: "/.resolved/x")
      expectDecomposition("/.volume/x", platform: .darwin,
        anchor: "/", components: [".volume", "x"],
        printed: "/.volume/x")
      expectDecomposition("/.resolvex/1/y", platform: .darwin,
        anchor: "/", components: [".resolvex", "1", "y"],
        printed: "/.resolvex/1/y")
      // Keyword present but missing the structural slashes:
      // `.vol` with no FSID/FILEID, and FSID but no FILEID.
      expectDecomposition("/.vol", platform: .darwin,
        anchor: "/", components: [".vol"],
        printed: "/.vol")
      expectDecomposition("/.vol/1234", platform: .darwin,
        anchor: "/", components: [".vol", "1234"],
        printed: "/.vol/1234")
    }

    runAllTests()
  }
}
