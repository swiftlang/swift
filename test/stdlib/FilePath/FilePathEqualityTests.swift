//===--- FilePathEqualityTests.swift --------------------------------------===//
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

// Equality, hashing, ordering. FilePath is pitched as a Dictionary key and as
// sortable, so this is load-bearing. Every expected value is derived from
// SE-0529, not from running the implementation.

@main
struct FilePathEqualityTests {
  static func main() {
    let suite = TestSuite("FilePath.Equality")

    // MARK: - Encoding-difference equality (proposal lines 701-711)

    suite.test("encodingDifferencesAreEqual")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      expectEqual(FilePath("a///b"), FilePath("a/b"), "a///b == a/b")
      expectEqual(FilePath("a/./b"), FilePath("a/b"), "a/./b == a/b")
      expectEqual(FilePath("/./foo"), FilePath("/foo"), "/./foo == /foo")
    }

    // MARK: - Suffix significance (proposal lines 713-714)

    suite.test("trailingSeparatorIsSignificant")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      expectNotEqual(FilePath("/tmp/foo"), FilePath("/tmp/foo/"),
        "trailing separator differs")
    }

    suite.test("currentDirectoryIsNotEmpty")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      expectNotEqual(FilePath("."), FilePath(""), "\".\" != \"\"")
    }

    // MARK: - Anchor significance (proposal lines 716-717)

    suite.test("darwinAnchorIsSignificant")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        expectNotEqual(FilePath("/.nofollow/foo/bar"), FilePath("/foo/bar"),
          "differing Darwin anchors")
      }
    }

    suite.test("windowsAnchorIsSignificant")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        expectNotEqual(FilePath(#"\\.\C:\foo\bar"#), FilePath(#"C:\foo\bar"#),
          "differing Windows anchors")
      }
    }

    // MARK: - Darwin canonicalization equality (proposal 220-223, 546, 549)

    suite.test("darwinCanonicalizationEquality")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        expectEqual(FilePath("/.resolve/1/foo"), FilePath("/.nofollow/foo"),
          "/.resolve/1/foo == /.nofollow/foo")
        expectEqual(FilePath("/.vol/1234/2/x"), FilePath("/.vol/1234/@/x"),
          "/.vol/1234/2/x == /.vol/1234/@/x")
        expectEqual(
          FilePath("/.resolve/1/.vol/1234/2/x"),
          FilePath("/.nofollow/.vol/1234/@/x"),
          "/.resolve/1/.vol/1234/2/x == /.nofollow/.vol/1234/@/x")
        expectEqual(
          FilePath("/.resolve/3/.vol/1234/2/x"),
          FilePath("/.resolve/3/.vol/1234/@/x"),
          "/.resolve/3/.vol/1234/2/x == /.resolve/3/.vol/1234/@/x")
      }
    }

    // MARK: - Hash agrees with equality (equal direction; proposal line 720)

    suite.test("hashAgreesWithEquality")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      expectEqual(FilePath("a///b").hashValue, FilePath("a/b").hashValue,
        "hash(a///b) == hash(a/b)")
      expectEqual(FilePath("a/./b").hashValue, FilePath("a/b").hashValue,
        "hash(a/./b) == hash(a/b)")
      expectEqual(FilePath("/./foo").hashValue, FilePath("/foo").hashValue,
        "hash(/./foo) == hash(/foo)")
      withPlatform(.darwin) {
        expectEqual(FilePath("/.resolve/1/foo").hashValue,
                    FilePath("/.nofollow/foo").hashValue,
          "hash canonicalization (resolve)")
        expectEqual(FilePath("/.vol/1234/2/x").hashValue,
                    FilePath("/.vol/1234/@/x").hashValue,
          "hash canonicalization (vol)")
      }
    }

    // MARK: - Comparable ordering (proposal lines 722-724)

    suite.test("orderingDistinguishesOnAnchor")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        expectTrue(FilePath("/.nofollow/foo") < FilePath("/foo"),
          "/.nofollow/foo < /foo")
        expectFalse(FilePath("/foo") < FilePath("/.nofollow/foo"),
          "not /foo < /.nofollow/foo")
      }
    }

    suite.test("orderingDistinguishesOnComponents")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      expectTrue(FilePath("foo/aaa") < FilePath("foo/bbb"),
        "foo/aaa < foo/bbb")
      expectFalse(FilePath("foo/bbb") < FilePath("foo/aaa"),
        "not foo/bbb < foo/aaa")
    }

    suite.test("orderingDistinguishesOnSuffix")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      expectTrue(FilePath("/tmp/foo") < FilePath("/tmp/foo/"),
        "/tmp/foo < /tmp/foo/")
      expectFalse(FilePath("/tmp/foo/") < FilePath("/tmp/foo"),
        "not /tmp/foo/ < /tmp/foo")
    }

    suite.test("orderingIsStrictTotalOnThreeElements")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Sorted order is "a" < "a/b" < "b":
      //   "a"   is a proper prefix of "a/b"          => "a"   < "a/b"
      //   "a/b" vs "b" first differ at 'a' < 'b'     => "a/b" < "b"
      let p1 = FilePath("a")
      let p2 = FilePath("a/b")
      let p3 = FilePath("b")

      // irreflexivity
      expectFalse(p1 < p1, "irreflexive p1")
      expectFalse(p2 < p2, "irreflexive p2")
      expectFalse(p3 < p3, "irreflexive p3")

      // the order itself
      expectTrue(p1 < p2, "p1 < p2")
      expectTrue(p2 < p3, "p2 < p3")

      // antisymmetry
      expectFalse(p2 < p1, "antisymmetry p1/p2")
      expectFalse(p3 < p2, "antisymmetry p2/p3")

      // transitivity
      expectTrue(p1 < p3, "transitivity p1 < p3")
    }

    // MARK: - Component equality / ordering (brief)

    suite.test("componentEqualityAndOrdering")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      expectEqual(FilePath.Component("foo"), FilePath.Component("foo"),
        "foo == foo")
      expectNotEqual(FilePath.Component("foo"), FilePath.Component("bar"),
        "foo != bar")
      expectEqual(FilePath.Component("foo").hashValue,
                  FilePath.Component("foo").hashValue,
        "equal components hash equal")
      expectTrue(FilePath.Component("a") < FilePath.Component("b"),
        "component a < b")
      expectTrue(FilePath.Component("a") < FilePath.Component("ab"),
        "component a < ab")
    }

    // MARK: - Anchor equality / ordering (brief)

    suite.test("anchorEqualityAndOrdering")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        expectEqual(FilePath.Anchor("/.resolve/1/"),
                    FilePath.Anchor("/.nofollow/"),
          "/.resolve/1/ anchor == /.nofollow/ anchor")
        expectEqual(FilePath.Anchor("/.resolve/1/").hashValue,
                    FilePath.Anchor("/.nofollow/").hashValue,
          "canonical anchors hash equal")
        expectNotEqual(FilePath.Anchor("/"), FilePath.Anchor("/.nofollow/"),
          "/ anchor != /.nofollow/ anchor")
        expectTrue(
          FilePath.Anchor("/.nofollow/") < FilePath.Anchor("/.vol/1234/5678"),
          "/.nofollow/ < /.vol/1234/5678")
      }
    }

    runAllTests()
  }
}
