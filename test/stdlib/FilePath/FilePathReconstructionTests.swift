//===--- FilePathReconstructionTests.swift --------------------------------===//
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

// Reconstruction and the suffix/anchor setters as direct API. These were
// previously exercised only as the round-trip tail of decomposition. Here
// they are driven directly with caller-built parts. Expectations from
// SE-0529:
//   * "Path reconstruction" (477-517): `init(anchor:_:hasTrailingSeparator:)`
//     and the Darwin `init(anchor:_:resourceFork:)`. The reconstructed path
//     "parses and normalizes exactly as if the equivalent string literal
//     had been provided."
//   * "Trailing separators" (400-427): `hasTrailingSeparator` get/set,
//     `withTrailingSeparator()`, `withoutTrailingSeparator()`.
//   * "Resource forks" (438-472): `isResourceFork` get/set,
//     `withResourceFork()`, `withoutResourceFork()`, and the documented
//     trailing-separator <-> resource-fork swap.
//   * `anchor` get/set (174-201; examples at 46-55).

// Build component arrays inside the active platform (Component init
// consults the platform for its separator check).
@available(SwiftStdlib 9999, *)
private func comps(_ names: String...) -> [FilePath.Component] {
  names.map { FilePath.Component($0)! }
}

@main
struct FilePathReconstructionTests {
  static func main() {
    let suite = TestSuite("FilePath.Reconstruction")

    // MARK: - init(anchor:_:hasTrailingSeparator:)

    suite.test("reconstructRelativeNilAnchor")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let p = FilePath(anchor: nil, comps("foo", "bar"))
      expectEqual(p.description, universal("foo/bar"), "relative reconstruction")
      expectNil(p.anchor, "nil anchor stays relative")
      expectEqual(p.components.map(\.description), ["foo", "bar"])
    }

    suite.test("reconstructBasicRoot")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let p = FilePath(anchor: FilePath.Anchor("/"), comps("foo", "bar"))
      expectEqual(p.description, universal("/foo/bar"),
        "basic-root reconstruction")
      expectEqual(p.anchor?.description, universal("/"),
        "anchor is the basic root")
      expectEqual(p.components.map(\.description), ["foo", "bar"])
    }

    suite.test("reconstructWindowsDriveAbsolute")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        let p = FilePath(anchor: FilePath.Anchor(#"C:\"#), comps("foo", "bar"))
        // Anchor ends in a separator, so no gap separator is inserted.
        expectEqual(p.description, #"C:\foo\bar"#, "C:\\ reconstruction")
        expectTrue(p.anchor?.description == #"C:\"#, "anchor is C:\\")
      }
    }

    suite.test("reconstructWindowsDriveRelative")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        let p = FilePath(anchor: FilePath.Anchor("C:"), comps("foo", "bar"))
        // Drive-relative `C:`: the colon is the boundary, so NO gap
        // separator — `C:foo\bar`, not `C:\foo\bar`.
        expectEqual(p.description, #"C:foo\bar"#,
          "C: (drive-relative) reconstruction")
        expectTrue(p.anchor?.description == "C:", "anchor is C:")
        expectFalse(p.isAbsolute, "C:foo\\bar is relative")
      }
    }

    suite.test("reconstructDarwinNofollow")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        let p = FilePath(anchor: FilePath.Anchor("/.nofollow/"),
                         comps("foo", "bar"))
        expectEqual(p.description, "/.nofollow/foo/bar",
          "/.nofollow/ reconstruction")
        expectTrue(p.anchor?.description == "/.nofollow/",
          "anchor is /.nofollow/")
        expectEqual(p.components.map(\.description), ["foo", "bar"])
      }
    }

    suite.test("reconstructTrailingSeparatorFlag")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let withSep = FilePath(
        anchor: FilePath.Anchor("/"), comps("foo"), hasTrailingSeparator: true)
      expectEqual(withSep.description, universal("/foo/"),
        "hasTrailingSeparator: true")
      expectTrue(withSep.hasTrailingSeparator, "trailing separator present")

      let noSep = FilePath(
        anchor: FilePath.Anchor("/"), comps("foo"), hasTrailingSeparator: false)
      expectEqual(noSep.description, universal("/foo"),
        "hasTrailingSeparator: false")
      expectFalse(noSep.hasTrailingSeparator, "no trailing separator")
    }

    suite.test("reconstructEmptyComponentsWithAnchor")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let root = FilePath(anchor: FilePath.Anchor("/"), [] as [FilePath.Component])
      expectEqual(root.description, universal("/"), "anchor-only basic root")
      expectTrue(root.components.isEmpty, "no components")

      withPlatform(.windows) {
        let drive = FilePath(anchor: FilePath.Anchor(#"C:\"#),
                             [] as [FilePath.Component])
        expectEqual(drive.description, #"C:\"#, "anchor-only C:\\")
        expectTrue(drive.components.isEmpty, "no components")

        // \\server\share is a complete root, so the appended `\` is a
        // trailing separator (proposal line 561).
        let unc = FilePath(
          anchor: FilePath.Anchor(#"\\server\share"#),
          [] as [FilePath.Component],
          hasTrailingSeparator: true)
        expectEqual(unc.description, #"\\server\share\"#,
          "UNC + trailing separator")
        expectTrue(unc.hasTrailingSeparator, "UNC trailing separator present")
      }
    }

    // MARK: - Darwin init(anchor:_:resourceFork:)

    suite.test("reconstructDarwinResourceFork")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        let p = FilePath(
          anchor: FilePath.Anchor("/"), comps("foo", "bar"), resourceFork: true)
        expectEqual(p.description, "/foo/bar/..namedfork/rsrc",
          "resource-fork reconstruction")
        expectTrue(p.isResourceFork, "isResourceFork is true")
        expectFalse(p.hasTrailingSeparator,
          "resource fork excludes trailing separator")
        expectEqual(p.components.map(\.description), ["foo", "bar"],
          "suffix is not a component")
      }
    }

    // MARK: - Emergent semantics under reconstruction

    suite.test("reconstructDarwinAnchorAbsorption")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        let p = FilePath(anchor: FilePath.Anchor("/"),
                         comps(".nofollow", "foo"))
        expectEqual(p.description, "/.nofollow/foo", "absorbed printed form")
        expectTrue(p.anchor?.description == "/.nofollow/",
          ".nofollow absorbed into anchor")
        expectEqual(p.components.map(\.description), ["foo"],
          "only foo remains a component")
        expectEqual(p, FilePath("/.nofollow/foo"),
          "reconstruction == equivalent string literal")
      }
    }

    // MARK: - hasTrailingSeparator setter + with/without

    suite.test("trailingSeparatorSetter")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var p = FilePath("/foo")
      expectFalse(p.hasTrailingSeparator, "starts without")

      p.hasTrailingSeparator = true
      expectEqual(p.description, universal("/foo/"), "set true adds separator")

      p.hasTrailingSeparator = true  // no-op
      expectEqual(p.description, universal("/foo/"),
        "set true again is a no-op")

      p.hasTrailingSeparator = false
      expectEqual(p.description, universal("/foo"),
        "set false removes separator")

      p.hasTrailingSeparator = false  // no-op
      expectEqual(p.description, universal("/foo"),
        "set false again is a no-op")
    }

    suite.test("withTrailingSeparatorMethods")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      expectEqual(FilePath("/foo").withTrailingSeparator().description,
        universal("/foo/"), "adds separator")
      expectEqual(FilePath("/foo/").withTrailingSeparator().description,
        universal("/foo/"), "no-op when already present")
      expectEqual(FilePath("/foo/").withoutTrailingSeparator().description,
        universal("/foo"), "removes separator")
      expectEqual(FilePath("/foo").withoutTrailingSeparator().description,
        universal("/foo"), "no-op when absent")
    }

    suite.test("trailingSeparatorAnchorOnly")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // The basic root's separator is structural (part of the anchor), so
      // it is NOT a trailing separator and cannot be "added".
      let root = FilePath("/")
      expectFalse(root.hasTrailingSeparator,
        "basic root has no trailing separator")
      expectEqual(root.withTrailingSeparator().description, universal("/"),
        "withTrailingSeparator on basic root is a no-op")

      withPlatform(.windows) {
        // \\server\share is a complete root; adding a separator yields a
        // real trailing separator (proposal lines 561, 573).
        let unc = FilePath(#"\\server\share"#)
        expectFalse(unc.hasTrailingSeparator,
          "bare UNC has no trailing separator")
        let withSep = unc.withTrailingSeparator()
        expectEqual(withSep.description, #"\\server\share\"#,
          "UNC + separator")
        expectTrue(withSep.hasTrailingSeparator,
          "now has trailing separator")
      }
    }

    // MARK: - Darwin isResourceFork setter + with/without + suffix swap

    suite.test("resourceForkSetter")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        let base = FilePath("/foo")
        expectFalse(base.isResourceFork, "plain path is not a resource fork")

        let forked = base.withResourceFork()
        expectEqual(forked.description, "/foo/..namedfork/rsrc", "adds suffix")
        expectTrue(forked.isResourceFork, "isResourceFork true")

        let unforked = forked.withoutResourceFork()
        expectEqual(unforked.description, "/foo", "removes suffix")
        expectFalse(unforked.isResourceFork, "isResourceFork false")
        expectEqual(base.withoutResourceFork().description, "/foo", "no-op")
      }
    }

    suite.test("suffixSwapTrailingToResourceFork")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        // Setting isResourceFork on a path with a trailing separator
        // REPLACES the separator with the resource-fork suffix
        // (proposal lines 449-452).
        var p = FilePath("/foo/")
        expectTrue(p.hasTrailingSeparator, "starts with trailing separator")
        p.isResourceFork = true
        expectEqual(p.description, "/foo/..namedfork/rsrc",
          "separator -> resource fork")
        expectTrue(p.isResourceFork, "now a resource fork")
        expectFalse(p.hasTrailingSeparator,
          "no longer a trailing separator")
      }
    }

    suite.test("suffixSwapResourceForkToTrailing")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        // Setting hasTrailingSeparator on a resource-fork path REPLACES
        // the suffix with a trailing separator (proposal lines 413-415).
        var p = FilePath("/foo/..namedfork/rsrc")
        expectTrue(p.isResourceFork, "starts as resource fork")
        p.hasTrailingSeparator = true
        expectEqual(p.description, "/foo/", "resource fork -> separator")
        expectTrue(p.hasTrailingSeparator, "now a trailing separator")
        expectFalse(p.isResourceFork, "no longer a resource fork")
      }
    }

    // MARK: - anchor get/set

    suite.test("anchorTransplantToVerbatim")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        // Proposal example (lines 46-50).
        var p = FilePath(#"C:\Users\dev\project"#)
        expectTrue(p.anchor?.description == #"C:\"#, "starts as C:\\")
        expectTrue(p.anchor?._isVerbatimComponent == false,
          "not verbatim initially")

        p.anchor = FilePath.Anchor(#"\\?\C:\"#)
        expectEqual(p.description, #"\\?\C:\Users\dev\project"#,
          "transplanted to verbatim")
        expectTrue(p.anchor?._isVerbatimComponent == true, "now verbatim")
        expectTrue(p.anchor?._driveLetter == "C", "drive letter preserved")
      }
    }

    suite.test("anchorStripDarwinToRoot")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        // Proposal example (lines 52-55).
        var p = FilePath("/.nofollow/etc/passwd")
        expectTrue(p.anchor?.description == "/.nofollow/",
          "starts as /.nofollow/")
        p.anchor = FilePath.Anchor("/")
        expectEqual(p.description, "/etc/passwd", "stripped to /")
        expectTrue(p.anchor?.description == "/", "anchor now /")
      }
    }

    suite.test("anchorSetToNil")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var p = FilePath("/foo/bar")
      p.anchor = nil
      expectEqual(p.description, universal("foo/bar"),
        "anchor removed -> relative")
      expectNil(p.anchor, "anchor is nil")
      expectFalse(p.isAbsolute, "now relative")
    }

    suite.test("anchorSetOntoRelative")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var p = FilePath("foo/bar")
      expectNil(p.anchor, "starts relative")
      p.anchor = FilePath.Anchor("/")
      expectEqual(p.description, universal("/foo/bar"), "anchor added")
      // The basic root is fully qualified on unix but is the current-drive
      // root (not absolute) on Windows.
      withPlatforms(.linux, .darwin) {
        expectTrue(p.isAbsolute, "now absolute")
      }
      withPlatform(.windows) {
        var q = FilePath(#"foo\bar"#)
        expectNil(q.anchor, "starts relative")
        q.anchor = FilePath.Anchor("C:")
        expectEqual(q.description, #"C:foo\bar"#,
          "C: prepended without a gap separator")
        expectFalse(q.isAbsolute, "drive-relative is not absolute")
      }
    }

    runAllTests()
  }
}
