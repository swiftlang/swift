//===--- FilePathComponentViewTests.swift ---------------------------------===//
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
// RUN: %target-build-swift %s %S/Inputs/FilePathTestSupport.swift %S/Inputs/TestHelpers.swift -o %t/a.out -module-name main
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest

// Migrated onto the StdlibUnittest seam: assertions go through StdlibUnittest's
// `expect*` natives and platform-specific bodies are wrapped in
// `withPlatform(.windows)` / `withPlatform(.darwin)`. Platform-INDEPENDENT
// tests assert through `universal(...)` (FilePathTestSupport.swift), which
// spells an expected path in the built platform's separator, so each runs
// unchanged on whatever platform is built.

@main
struct FilePathComponentViewTests {
  static func main() {
    let suite = TestSuite("FilePath.ComponentView")


    // MARK: - Basic collection properties

    suite.test("emptyPath")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let path = FilePath("")
      expectTrue(path.components.isEmpty)
      expectEqual(path.components.count, 0)
      expectEqual(path.components.startIndex, path.components.endIndex)
    }


    suite.test("rootOnlyHasNoComponents")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let root = FilePath("/")
      expectTrue(root.components.isEmpty)
      expectNotNil(root.anchor)
    }


    suite.test("rootOnlyHasNoComponentsWindows")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        let winRoot = FilePath(#"C:\"#)
        expectTrue(winRoot.components.isEmpty)
        expectNotNil(winRoot.anchor)
      }
    }


    suite.test("indexTraversal")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let path = FilePath("/usr/local/bin")
      let cv = path.components
      expectEqual(cv.count, 3)

      var idx = cv.startIndex
      expectEqual(cv[idx].description, "usr")
      idx = cv.index(after: idx)
      expectEqual(cv[idx].description, "local")
      idx = cv.index(after: idx)
      expectEqual(cv[idx].description, "bin")
      idx = cv.index(after: idx)
      expectEqual(idx, cv.endIndex)

      // Reverse traversal
      idx = cv.index(before: cv.endIndex)
      expectEqual(cv[idx].description, "bin")
      idx = cv.index(before: idx)
      expectEqual(cv[idx].description, "local")
    }


    // MARK: - append

    suite.test("appendToRelative")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("a/b")
      path.components.append("c")

      expectEqual(path.description, universal("a/b/c"))
      expectEqual(path.components.map(\.description), ["a", "b", "c"])
    }


    suite.test("appendToAbsolute")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr")
      path.components.append("local")

      expectEqual(path.description, universal("/usr/local"))
      expectEqual(path.anchor?.description, universal("/"))
    }


    suite.test("appendToEmpty")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("")
      path.components.append("hello")

      expectEqual(path.description, "hello")
    }


    suite.test("appendToRootOnly")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/")
      path.components.append("usr")

      expectEqual(path.description, universal("/usr"))
      expectEqual(path.anchor?.description, universal("/"))
    }


    suite.test("appendContentsOf")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr")
      path.components.append(contentsOf: ["local", "bin"] as [FilePath.Component])

      expectEqual(path.description, universal("/usr/local/bin"))
    }


    // MARK: - insert

    suite.test("insertAtBeginning")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/local/bin")
      path.components.insert("usr", at: path.components.idx(0))

      expectEqual(path.description, universal("/usr/local/bin"))
    }


    suite.test("insertInMiddle")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr/bin")
      path.components.insert("local", at: path.components.idx(1))

      expectEqual(path.description, universal("/usr/local/bin"))
    }


    suite.test("insertAtEnd")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr/local")
      path.components.insert("bin", at: path.components.endIndex)

      expectEqual(path.description, universal("/usr/local/bin"))
    }


    // MARK: - remove

    suite.test("removeFirst")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr/local/bin")
      path.components.removeFirst()

      expectEqual(path.description, universal("/local/bin"))
      expectEqual(path.anchor?.description, universal("/"))
    }


    suite.test("removeLast")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr/local/bin")
      path.components.removeLast()

      expectEqual(path.description, universal("/usr/local"))
    }


    suite.test("removeAtIndex")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr/local/bin")
      path.components.remove(at: path.components.idx(1))

      expectEqual(path.description, universal("/usr/bin"))
    }


    suite.test("removeAllComponents")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr/local")
      var cv = path.components
      cv.removeAll()
      path.components = cv

      // Anchor is preserved, components are gone
      expectEqual(path.description, universal("/"))
      expectEqual(path.anchor?.description, universal("/"))
      expectTrue(path.components.isEmpty)
    }


    suite.test("removeAllFromRelative")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("a/b/c")
      path.components.removeAll()

      expectEqual(path.description, "")
      expectTrue(path.isEmpty)
    }


    // MARK: - replaceSubrange

    suite.test("replaceMiddle")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr/local/bin")
      path.components.replaceSubrange(
        path.components.range(1..<2),
        with: ["share", "man"] as [FilePath.Component])

      expectEqual(path.description, universal("/usr/share/man/bin"))
    }


    suite.test("replaceAll")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/old/path")
      path.components.replaceSubrange(
        path.components.startIndex..<path.components.endIndex,
        with: ["new", "path"] as [FilePath.Component])

      expectEqual(path.description, universal("/new/path"))
      expectEqual(path.anchor?.description, universal("/"))
    }


    suite.test("replaceWithEmpty")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr/local/bin")
      path.components.replaceSubrange(
        path.components.range(1..<3), with: [] as [FilePath.Component])

      expectEqual(path.description, universal("/usr"))
    }


    suite.test("replaceEmptyRange")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr/bin")
      path.components.replaceSubrange(
        path.components.range(1..<1),
        with: ["local"] as [FilePath.Component])

      expectEqual(path.description, universal("/usr/local/bin"))
    }


    // MARK: - Normalization interactions

    suite.test("dotComponentInsertion")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Component.init normalizes through FilePath, so "." as a
      // single component is `.currentDirectory` kind
      let dot: FilePath.Component = "."
      expectEqual(dot.kind, .currentDirectory)

      let dotdot: FilePath.Component = ".."
      expectEqual(dotdot.kind, .parentDirectory)
    }


    suite.test("appendDotDot")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr/local")
      var cv = path.components
      cv.append("..")
      path.components = cv

      // ".." is preserved as a component (no lexical collapsing)
      expectEqual(path.components.map(\.description), ["usr", "local", ".."])
      expectEqual(path.description, universal("/usr/local/.."))
    }


    suite.test("appendDotToRelative")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // With the view-on-storage architecture, appending a "." component
      // directly mutates storage without renormalization. The dot persists.
      var path = FilePath("a/b")
      path.components.append(".")

      expectEqual(path.components.map(\.description), ["a", "b", "."])
      expectEqual(path.description, universal("a/b/."))
    }


    suite.test("componentInitNormalizesInput")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Component.init?(_:) goes through FilePath, which normalizes.
      // So Component("a//b") is nil (normalizes to multi-component path)
      let str1: String = "a//b"
      let multiComp: FilePath.Component? = .init(str1)
      expectNil(multiComp)
      let str2: String = "a/b"
      let withSlash: FilePath.Component? = .init(str2)
      expectNil(withSlash)
      let str3: String = "/"
      let rootOnly: FilePath.Component? = .init(str3)
      expectNil(rootOnly)
      let str4: String = ""
      let empty: FilePath.Component? = .init(str4)
      expectNil(empty)
      let str5: String = "hello"
      let valid: FilePath.Component? = .init(str5)
      expectNotNil(valid)
      expectEqual(valid?.description, "hello")
    }


    // MARK: - Windows platform

    suite.test("windowsAppend")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath(#"C:\Users"#)
        path.components.append("Admin")

        expectEqual(path.description, #"C:\Users\Admin"#)
        expectEqual(path.anchor?.description, #"C:\"#)
      }
    }


    suite.test("windowsDriveRelativeAppend")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath("C:src")
        var cv = path.components
        cv.append("main.swift")
        path.components = cv

        // C: anchor (no backslash) — components follow directly
        expectEqual(path.description, #"C:src\main.swift"#)
        expectEqual(path.anchor?.description, "C:")
      }
    }


    suite.test("windowsRemoveComponent")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath(#"C:\Users\Admin\file.txt"#)
        path.components.removeLast()

        expectEqual(path.description, #"C:\Users\Admin"#)
      }
    }


    suite.test("windowsUNCAppend")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath(#"\\server\share"#)
        path.components.append("folder")

        expectEqual(path.description, #"\\server\share\folder"#)
      }
    }


    suite.test("windowsReplaceComponents")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath(#"C:\old\stuff"#)
        path.components.replaceSubrange(
          path.components.startIndex..<path.components.endIndex,
          with: ["new", "things"] as [FilePath.Component])

        expectEqual(path.description, #"C:\new\things"#)
      }
    }


    // MARK: - Anchor preservation

    suite.test("anchorSurvivesMutation")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr/local/bin")
      let originalAnchor = path.anchor

      var cv = path.components
      cv.removeAll()
      cv.append("etc")
      path.components = cv

      expectEqual(path.anchor, originalAnchor)
      expectEqual(path.description, universal("/etc"))
    }


    suite.test("noAnchorSurvivesMutation")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("a/b/c")

      var cv = path.components
      cv.replaceSubrange(cv.startIndex..<cv.endIndex, with: ["x", "y"] as [FilePath.Component])
      path.components = cv

      expectNil(path.anchor)
      expectEqual(path.description, universal("x/y"))
    }


    suite.test("windowsAnchorSurvivesMutation")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath(#"\\server\share\old\path"#)
        let originalAnchor = path.anchor

        var cv = path.components
        cv.removeAll()
        cv.append("new")
        path.components = cv

        expectEqual(path.anchor, originalAnchor)
        expectEqual(path.description, #"\\server\share\new"#)
      }
    }


    // MARK: - Hashable / Equatable

    suite.test("componentViewEquality")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let a = FilePath("/usr/local/bin")
      let b = FilePath("/usr/local/bin")
      expectEqual(a.components, b.components)

      let c = FilePath("/usr/local")
      expectNotEqual(a.components, c.components)
    }


    suite.test("componentViewOrdering")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let a = FilePath("a/b").components
      let b = FilePath("a/c").components
      let c = FilePath("a/b/c").components
      expectTrue(a < b)
      expectTrue(a < c) // prefix is less
    }


    // MARK: - Derived Collection operations

    suite.test("filter")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let path = FilePath("a/b/c/d")
      let even = path.components.enumerated()
        .filter { $0.offset % 2 == 0 }
        .map(\.element)
      expectEqual(even.map(\.description), ["a", "c"])
    }


    suite.test("map")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let path = FilePath("/usr/local/bin")
      let names = path.components.map(\.description)
      expectEqual(names, ["usr", "local", "bin"])
    }


    suite.test("reversed")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let path = FilePath("a/b/c")
      let rev = path.components.reversed().map(\.description)
      expectEqual(rev, ["c", "b", "a"])
    }


    suite.test("prefix")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr/local/bin/tool")
      let first2 = Array(path.components.prefix(2))
      path.components.replaceSubrange(
        path.components.startIndex..<path.components.endIndex, with: first2)

      expectEqual(path.description, universal("/usr/local"))
    }


    suite.test("dropFirst")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/usr/local/bin")
      let tail = Array(path.components.dropFirst())
      path.components.replaceSubrange(
        path.components.startIndex..<path.components.endIndex, with: tail)

      expectEqual(path.description, universal("/local/bin"))
    }


    // MARK: - Round-trip through ComponentView init()

    suite.test("buildFromScratch")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var cv = FilePath.ComponentView()
      cv.append("usr")
      cv.append("local")
      cv.append("bin")

      var path = FilePath("/")
      path.components = cv
      expectEqual(path.description, universal("/usr/local/bin"))
    }


    suite.test("buildRelativeFromScratch")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var cv = FilePath.ComponentView()
      cv.append("src")
      cv.append("main.swift")

      var path = FilePath()
      path.components = cv
      expectEqual(path.description, universal("src/main.swift"))
    }


    suite.test("windowsBuildFromScratch")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var cv = FilePath.ComponentView()
        cv.append("Users")
        cv.append("Admin")
        cv.append("Documents")

        var path = FilePath(#"C:\"#)
        path.components = cv
        expectEqual(path.description, #"C:\Users\Admin\Documents"#)
      }
    }


    // MARK: - Edge cases

    suite.test("singleComponentPath")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("hello")
      expectEqual(path.components.count, 1)
      expectEqual(path.components.first?.description, "hello")

      path.components.removeLast()
      expectTrue(path.isEmpty)
    }


    suite.test("multipleAppends")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/")

      for name: String in ["a", "b", "c", "d", "e"] {
        path.components.append(FilePath.Component(name)!)
      }

      expectEqual(path.components.count, 5)
      expectEqual(path.description, universal("/a/b/c/d/e"))
    }


    suite.test("replaceEntireRelativeKeepsAnchor")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/old/path/here")
      let anchor = path.anchor

      path.components.replaceSubrange(
        path.components.startIndex..<path.components.endIndex, with: [
          "completely" as FilePath.Component,
          "new" as FilePath.Component,
        ])

      expectEqual(path.anchor, anchor)
      expectEqual(path.components.map(\.description), ["completely", "new"])
    }


    // MARK: - Suffix semantics on mutation

    // -- Trailing separator: strip on remove/replace --

    suite.test("trailingSepStrippedOnRemoveLast")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("a/b/c/")
      expectTrue(path.hasTrailingSeparator)

      path.components.removeLast()

      expectFalse(path.hasTrailingSeparator)
      expectEqual(path.description, universal("a/b"))
    }


    suite.test("trailingSepStrippedOnReplaceLast")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("a/b/c/")
      expectTrue(path.hasTrailingSeparator)

      path.components.replaceSubrange(
        path.components.index(before: path.components.endIndex) ..< path.components.endIndex,
        with: ["d" as FilePath.Component])

      expectFalse(path.hasTrailingSeparator)
      expectEqual(path.description, universal("a/b/d"))
    }


    suite.test("trailingSepStrippedOnRemoveAll")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("a/b/c/")
      expectTrue(path.hasTrailingSeparator)

      path.components.removeAll()

      expectFalse(path.hasTrailingSeparator)
      expectEqual(path.description, "")
    }


    suite.test("trailingSepStrippedOnRemoveAllAbsolute")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/a/b/c/")
      expectTrue(path.hasTrailingSeparator)

      path.components.removeAll()

      expectFalse(path.hasTrailingSeparator)
      expectEqual(path.description, universal("/"))
    }


    suite.test("windowsTrailingSepStrippedOnRemoveLast")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath(#"C:\Users\Admin\"#)
        expectTrue(path.hasTrailingSeparator)

        path.components.removeLast()

        expectFalse(path.hasTrailingSeparator)
        expectEqual(path.description, #"C:\Users"#)
      }
    }


    // -- Trailing separator: preserve when last unchanged --

    suite.test("trailingSepPreservedOnInsertFirst")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("a/b/c/")
      expectTrue(path.hasTrailingSeparator)

      path.components.insert("z", at: path.components.idx(0))

      expectTrue(path.hasTrailingSeparator)
      expectEqual(path.description, universal("z/a/b/c/"))
    }


    suite.test("trailingSepPreservedOnReplaceNonLast")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("a/b/c/")

      path.components.replaceSubrange(
        path.components.range(0..<1),
        with: ["x" as FilePath.Component])

      expectTrue(path.hasTrailingSeparator)
      expectEqual(path.description, universal("x/b/c/"))
    }


    suite.test("trailingSepPreservedOnRemoveFirst")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("a/b/c/")

      path.components.removeFirst()

      expectTrue(path.hasTrailingSeparator)
      expectEqual(path.description, universal("b/c/"))
    }


    suite.test("trailingSepPreservedOnInsertMiddle")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/a/c/")

      path.components.insert("b", at: path.components.idx(1))

      expectTrue(path.hasTrailingSeparator)
      expectEqual(path.description, universal("/a/b/c/"))
    }


    suite.test("trailingSepPreservedOnNoChange")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("a/b/c/")
      let cv = path.components
      path.components = cv

      expectTrue(path.hasTrailingSeparator)
      expectEqual(path.description, universal("a/b/c/"))
    }


    suite.test("trailingSepPreservedEmptyToEmpty")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // \\server\share\ decomposes with empty components and
      // trailing sep. Setting empty components back preserves it.
      withPlatform(.windows) {
        var path = FilePath(#"\\server\share\"#)
        expectTrue(path.hasTrailingSeparator)
        expectTrue(path.components.isEmpty)

        let cv = path.components
        path.components = cv

        expectTrue(path.hasTrailingSeparator)
      }
    }


    // -- Trailing separator: strip on append --

    suite.test("trailingSepOnAppend")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("a/b/")
      expectTrue(path.hasTrailingSeparator)

      path.components.append("c")

      expectFalse(path.hasTrailingSeparator)
      expectEqual(path.description, universal("a/b/c"))
    }


    suite.test("trailingSepOnAppendContentsOf")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/dir/")
      expectTrue(path.hasTrailingSeparator)

      path.components.append(contentsOf: ["sub", "file"] as [FilePath.Component])

      expectFalse(path.hasTrailingSeparator)
      expectEqual(path.description, universal("/dir/sub/file"))
    }


    // -- Resource fork: strip on remove/replace (Darwin) --

    suite.test("resourceForkStrippedOnRemoveLast")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        var path = FilePath("/dir/file/..namedfork/rsrc")
        expectTrue(path.isResourceFork)
        expectEqual(path.components.map(\.description), ["dir", "file"])

        path.components.removeLast()

        expectFalse(path.isResourceFork)
        expectEqual(path.description, "/dir")
      }
    }


    suite.test("resourceForkStrippedOnReplaceLast")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        var path = FilePath("/file/..namedfork/rsrc")
        expectTrue(path.isResourceFork)
        expectEqual(path.components.map(\.description), ["file"])

        path.components.replaceSubrange(
          path.components.range(0..<1),
          with: ["other" as FilePath.Component])

        expectFalse(path.isResourceFork)
        expectEqual(path.description, "/other")
      }
    }


    suite.test("resourceForkStrippedOnRemoveAll")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        var path = FilePath("/file/..namedfork/rsrc")
        expectTrue(path.isResourceFork)

        path.components.removeAll()

        expectFalse(path.isResourceFork)
        expectEqual(path.description, "/")
      }
    }


    // -- Resource fork: preserve when last unchanged --

    suite.test("resourceForkPreservedOnInsert")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        var path = FilePath("/file/..namedfork/rsrc")
        expectTrue(path.isResourceFork)
        expectEqual(path.components.map(\.description), ["file"])

        path.components.insert("dir", at: path.components.idx(0))

        expectTrue(path.isResourceFork)
        expectEqual(path.components.map(\.description), ["dir", "file"])
      }
    }


    suite.test("resourceForkPreservedOnNoChange")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        var path = FilePath("/file/..namedfork/rsrc")
        expectTrue(path.isResourceFork)

        let cv = path.components
        path.components = cv

        expectTrue(path.isResourceFork)
      }
    }


    // -- Resource fork: strip on append --

    suite.test("resourceForkOnAppend")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        var path = FilePath("/file/..namedfork/rsrc")
        expectTrue(path.isResourceFork)

        path.components.append("extra")

        expectFalse(path.isResourceFork)
        expectEqual(path.description, "/file/extra")
      }
    }


    // MARK: - Re-decomposition after component mutation
    //
    // When a component mutation produces a string that re-parses to a
    // different decomposition (e.g. inserting `.nofollow` at the front
    // of an absolute Darwin path causes anchor absorption), we honor
    // the new decomposition rather than masking it. The string IS what
    // the kernel sees; pretending otherwise would be a lie.

    // MARK: - Structural suffix rule corner cases

    suite.test("trailingSepStrippedOnRemoveLastEvenWithEqualNeighbor")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/a/b/b/")
      path.components.removeLast()
      expectFalse(path.hasTrailingSeparator)
      expectEqual(path.description, universal("/a/b"))
    }


    suite.test("replaceAllDropsSuffixEvenWhenLastByteEqual")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/a/b/c/")
      path.components.replaceSubrange(
        path.components.startIndex..<path.components.endIndex,
        with: ["x", "y", "c"] as [FilePath.Component])
      expectFalse(path.hasTrailingSeparator)
      expectEqual(path.description, universal("/x/y/c"))
    }


    suite.test("removeAllOnUNCDropsGapSeparator")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath(#"\\server\share\"#)
        expectTrue(path.hasTrailingSeparator)
        path.components.removeAll()
        expectFalse(path.hasTrailingSeparator)
        expectEqual(path.description, #"\\server\share"#)
      }
    }


    // MARK: - removeAll across all anchor shapes
    //
    // The view region for removeAll extends from anchor-end (before any gap
    // separator) to end-of-storage. These cases exercise the four anchor
    // shapes: anchor-includes-trailing-sep, anchor-ends-with-`:`, anchor-
    // with-gap-sep, and verbatim variants of the same.

    suite.test("removeAllUNCWithComponentsDropsGapSep")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath(#"\\server\share\foo\bar"#)
        expectEqual(path.components.map(\.description), ["foo", "bar"])
        path.components.removeAll()
        expectEqual(path.description, #"\\server\share"#)
        expectFalse(path.hasTrailingSeparator)
      }
    }


    suite.test("removeAllDriveAbsoluteKeepsAnchorSep")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath(#"C:\foo\bar"#)
        path.components.removeAll()
        expectEqual(path.description, #"C:\"#)
      }
    }


    suite.test("removeAllDriveRelativeKeepsColon")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath(#"C:foo\bar"#)
        path.components.removeAll()
        expectEqual(path.description, "C:")
      }
    }


    suite.test("removeAllVerbatimDriveKeepsAnchor")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath(#"\\?\C:\foo\bar"#)
        path.components.removeAll()
        expectEqual(path.description, #"\\?\C:\"#)
      }
    }


    suite.test("removeAllVerbatimUNCDropsGapSep")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath(#"\\?\UNC\server\share\foo"#)
        path.components.removeAll()
        expectEqual(path.description, #"\\?\UNC\server\share"#)
      }
    }


    suite.test("removeAllVerbatimDeviceDropsGapSep")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath(#"\\?\name\foo"#)
        path.components.removeAll()
        expectEqual(path.description, #"\\?\name"#)
      }
    }


    // MARK: - Colon-ending anchors: Windows drive-relative vs Darwin volfs
    //
    // The `:` IS the anchor/component boundary only on Windows
    // (drive-relative `C:foo`). On Darwin, `:` is a regular byte that can
    // appear in a volfs FILEID, so an anchor ending in `:` still needs a
    // gap separator before any component bytes.

    suite.test("windowsDriveRelativeAppendStaysDriveRelative")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Appending to `C:` must yield `C:foo`, NOT `C:\foo` (which would
      // be drive-absolute, a different anchor shape).
      withPlatform(.windows) {
        var path = FilePath("C:")
        expectEqual(path.anchor?.description, "C:")
        path.components.append("foo")
        expectEqual(path.description, "C:foo")
        expectEqual(path.anchor?.description, "C:")
        expectEqual(path.components.map(\.description), ["foo"])
      }
    }


    suite.test("windowsDriveRelativeMultiAppendStaysDriveRelative")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath("C:")
        path.components.append("foo")
        path.components.append("bar")
        expectEqual(path.description, #"C:foo\bar"#)
        expectEqual(path.anchor?.description, "C:")
      }
    }


    suite.test("windowsDriveRelativeAssignKeepsAnchor")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath("C:")
        var cv = FilePath.ComponentView()
        cv.append("foo")
        path.components = cv
        expectEqual(path.description, "C:foo")
        expectEqual(path.anchor?.description, "C:")
      }
    }


    suite.test("darwinVolfsColonInFileIdGetsGapSeparator")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Darwin volfs FILEID is "bytes up to next /". A FILEID ending in
      // `:` is degenerate but legal. Adding a component must add a gap
      // separator — the `:`-skips-gap-sep rule is Windows-specific.
      withPlatform(.darwin) {
        var path = FilePath("/.vol/12345/67890:")
        expectEqual(path.anchor?.description, "/.vol/12345/67890:")
        path.components.append("foo")
        expectEqual(path.description, "/.vol/12345/67890:/foo")
        expectEqual(path.anchor?.description, "/.vol/12345/67890:")
        expectEqual(path.components.map(\.description), ["foo"])
      }
    }


    suite.test("darwinVolfsColonAssignKeepsAnchor")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.darwin) {
        var path = FilePath("/.vol/12345/67890:")
        var cv = FilePath.ComponentView()
        cv.append("foo")
        path.components = cv
        expectEqual(path.description, "/.vol/12345/67890:/foo")
        expectEqual(path.anchor?.description, "/.vol/12345/67890:")
      }
    }


    suite.test("windowsUNCWithColonShareGetsGapSeparator")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // The UNC parser allows `:` in share names: `\\server\C:` parses
      // as anchor `\\server\C:` (length 11), NOT drive-relative. The
      // gap separator must be added on append.
      withPlatform(.windows) {
        var path = FilePath(#"\\server\C:"#)
        expectEqual(path.anchor?.description, #"\\server\C:"#)
        path.components.append("foo")
        expectEqual(path.description, #"\\server\C:\foo"#)
        expectEqual(path.anchor?.description, #"\\server\C:"#)
      }
    }


    suite.test("windowsUNCWithColonShareAssignKeepsGap")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatform(.windows) {
        var path = FilePath(#"\\server\C:"#)
        var cv = FilePath.ComponentView()
        cv.append("foo")
        path.components = cv
        expectEqual(path.description, #"\\server\C:\foo"#)
      }
    }


    // MARK: - Suffix interactions with splice

    suite.test("appendAfterTrailingSepAbsorbs")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/foo/")
      expectTrue(path.hasTrailingSeparator)
      path.components.append("bar")
      expectEqual(path.description, universal("/foo/bar"))
      expectFalse(path.hasTrailingSeparator)
    }


    suite.test("replaceSubrangeLastWithEmptyMatchesRemoveLast")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      var path = FilePath("/a/b/c")
      let last = path.components.index(before: path.components.endIndex)
      path.components.replaceSubrange(last..<path.components.endIndex, with: [])
      expectEqual(path.description, universal("/a/b"))
    }


    suite.test("insertInteriorPreservesResourceFork")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Insert in the MIDDLE of a multi-component path that has a
      // resource fork suffix. Middle insert is not touchesEnd, so the
      // suffix region is untouched.
      withPlatform(.darwin) {
        var path = FilePath("/foo/bar/..namedfork/rsrc")
        expectTrue(path.isResourceFork)
        expectEqual(path.components.map(\.description), ["foo", "bar"])
        let afterFoo = path.components.index(after: path.components.startIndex)
        path.components.insert("x", at: afterFoo)
        expectEqual(path.description, "/foo/x/bar/..namedfork/rsrc")
        expectTrue(path.isResourceFork)
        expectEqual(path.components.map(\.description), ["foo", "x", "bar"])
      }
    }


    // MARK: - Cross-anchor assignment
    //
    // Property assignment splices newValue's contributed bytes
    // (`[_originalStart, _suffixEnd)`) into self's post-anchor region.
    // Self's anchor stays put; the new contribution becomes the
    // components+suffix.

    suite.test("assignDifferentAnchorCvKeepsSelfAnchor")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // cv from a path with a different anchor. Only cv's components
      // (the bytes after cv's original anchor) get spliced; self's
      // anchor is preserved.
      withPlatform(.windows) {
        var path = FilePath(#"\foo"#)
        let cv = FilePath(#"C:\bar"#).components
        path.components = cv
        expectEqual(path.description, #"\bar"#)
      }
    }


    suite.test("assignAnchoredCvOntoAnchorlessKeepsAnchorless")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // cv has anchor, self doesn't. Splice copies only cv's component
      // bytes; self stays anchorless.
      var path = FilePath("a/b")
      let cv = FilePath("/foo").components
      path.components = cv
      expectEqual(path.description, "foo")
    }


    suite.test("absorptionThenAssignMatchesInPlace")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // cv mutated to trigger anchor absorption, then assigned back.
      // The splice uses cv's _originalStart (immutable since view
      // creation), so the absorbed bytes are part of the spliced region.
      // Result must match in-place mutation.
      withPlatform(.darwin) {

        var inPlace = FilePath("/foo/bar")
        inPlace.components.insert(".nofollow", at: inPlace.components.startIndex)
        expectEqual(inPlace.description, "/.nofollow/foo/bar")

        var assigned = FilePath("/foo/bar")
        var cv = assigned.components
        cv.insert(".nofollow", at: cv.startIndex)
        assigned.components = cv

        expectEqual(assigned.description, inPlace.description)
      }
    }


    // -- Darwin anchor hazards --

    suite.test("darwinInsertNofollowAtFront")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // /foo/bar -> insert ".nofollow" at 0 -> /.nofollow/foo/bar.
      // Darwin anchor parsing absorbs "/.nofollow/" into the anchor,
      // so the post-mutation decomposition reflects the kernel's view
      // rather than the caller's per-component intent.
      withPlatform(.darwin) {
        var path = FilePath("/foo/bar")
        expectEqual(path.anchor?.description, "/")

        var cv = path.components
        cv.insert(".nofollow", at: cv.idx(0))
        path.components = cv

        expectEqual(path.description, "/.nofollow/foo/bar")
        expectEqual(path.anchor?.description, "/.nofollow/")
        expectEqual(path.components.map(\.description), ["foo", "bar"])
      }
    }


    suite.test("darwinInsertResolveAtFront")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // /usr/bin -> insert ".resolve" at 0
      // Then "usr" looks like the resolve flag value: /.resolve/usr/bin
      withPlatform(.darwin) {
        var path = FilePath("/usr/bin")

        var cv = path.components
        cv.insert(".resolve", at: cv.idx(0))
        path.components = cv

        expectEqual(path.description, "/.resolve/usr/bin")

        // Reparse: /.resolve/usr/ is the anchor (flag value = "usr")
        let newAnchor = path.anchor?.description
        let newComps = path.components.map(\.description)
        expectEqual(newAnchor, "/.resolve/usr/")
        expectEqual(newComps, ["bin"])
      }
    }


    suite.test("darwinInsertVolAtFront")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // /1234/5678/file -> insert ".vol" at 0
      // Becomes /.vol/1234/5678/file — anchor absorbs /.vol/1234/5678
      withPlatform(.darwin) {
        var path = FilePath("/1234/5678/file")

        var cv = path.components
        cv.insert(".vol", at: cv.idx(0))
        path.components = cv

        expectEqual(path.description, "/.vol/1234/5678/file")

        let newAnchor = path.anchor?.description
        let newComps = path.components.map(\.description)
        expectEqual(newAnchor, "/.vol/1234/5678")
        expectEqual(newComps, ["file"])
      }
    }


    suite.test("darwinAppendsFormCombinedAnchor")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Starting from a `/.nofollow/` anchor, appending `.vol`, FSID, and
      // FILEID one at a time. The first two appends leave them as plain
      // components (vol parser fails — incomplete). The third append
      // completes a parsable `.vol/FSID/FILEID` and triggers absorption:
      // the combined anchor `/.nofollow/.vol/N/M` forms and components
      // collapse to empty. (Per proposal line 111: a Darwin anchor may
      // include resolve flags AND/OR a volume identifier.)
      withPlatform(.darwin) {
        var path = FilePath("/.nofollow/")
        expectEqual(path.anchor?.description, "/.nofollow/")
        expectEqual(path.components.map(\.description), [])

        path.components.append(".vol")
        expectEqual(path.anchor?.description, "/.nofollow/")
        expectEqual(path.components.map(\.description), [".vol"])

        path.components.append("1234")
        expectEqual(path.anchor?.description, "/.nofollow/")
        expectEqual(path.components.map(\.description), [".vol", "1234"])

        path.components.append("5678")
        // Absorption: components fold into the combined anchor.
        expectEqual(path.anchor?.description, "/.nofollow/.vol/1234/5678")
        expectEqual(path.components.map(\.description), [])
      }
    }


    suite.test("darwinRemoveLastFromCombinedAnchorStaysWithLeading")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Removing the only component of a combined-anchor path leaves the
      // anchor + gap separator. Same shape as UNC `\\server\share\only` ->
      // `\\server\share\`: the gap separator becomes the trailing separator,
      // anchor stays intact, components empty.
      withPlatform(.darwin) {
        var path = FilePath("/.nofollow/.vol/1234/5678/foo")
        expectEqual(path.anchor?.description, "/.nofollow/.vol/1234/5678")
        expectEqual(path.components.map(\.description), ["foo"])

        path.components.removeLast()

        expectEqual(path.description, "/.nofollow/.vol/1234/5678/")
        expectEqual(path.anchor?.description, "/.nofollow/.vol/1234/5678")
        expectEqual(path.components.map(\.description), [])
        expectTrue(path.hasTrailingSeparator)
      }
    }


    suite.test("darwinRemoveComponentExposesAnchor")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Reverse direction: remove first component to reveal anchor structure.
      // /prefix/.nofollow/foo -> remove "prefix" -> /.nofollow/foo
      withPlatform(.darwin) {
        var path = FilePath("/prefix/.nofollow/foo")
        expectEqual(path.anchor?.description, "/")
        expectEqual(path.components.map(\.description), ["prefix", ".nofollow", "foo"])

        var cv = path.components
        cv.removeFirst()
        path.components = cv

        expectEqual(path.description, "/.nofollow/foo")

        let newAnchor = path.anchor?.description
        let newComps = path.components.map(\.description)
        expectEqual(newAnchor, "/.nofollow/")
        expectEqual(newComps, ["foo"])
      }
    }


    suite.test("darwinReplaceFirstExposesVol")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Replace first component to create .vol anchor
      // /old/1234/5678 -> replace "old" with ".vol" -> /.vol/1234/5678
      withPlatform(.darwin) {
        var path = FilePath("/old/1234/5678")
        expectEqual(path.components.count, 3)

        var cv = path.components
        cv.replaceSubrange(cv.range(0..<1), with: [".vol" as FilePath.Component])
        path.components = cv

        expectEqual(path.description, "/.vol/1234/5678")

        let newAnchor = path.anchor?.description
        let newComps = path.components.map(\.description)
        expectEqual(newAnchor, "/.vol/1234/5678")
        expectEqual(newComps, [])
      }
    }


    suite.test("darwinNofollowOnRelativePathIsSafe")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // .nofollow only triggers anchor parsing on absolute paths
      withPlatform(.darwin) {
        var path = FilePath("a/b")
        var cv = path.components
        cv.insert(".nofollow", at: cv.idx(0))
        path.components = cv

        // No root, so .nofollow is just a regular component
        expectNil(path.anchor)
        expectEqual(path.components.map(\.description), [".nofollow", "a", "b"])
        expectEqual(path.description, ".nofollow/a/b")
      }
    }


    suite.test("darwinNofollowNotFirstIsSafe")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // .nofollow only triggers when it's the path-initial dot component
      withPlatform(.darwin) {
        var path = FilePath("/usr/bin")
        var cv = path.components
        cv.append(".nofollow")
        path.components = cv

        // .nofollow at end doesn't affect the anchor
        expectEqual(path.anchor?.description, "/")
        expectEqual(path.components.map(\.description), ["usr", "bin", ".nofollow"])
      }
    }


    // -- Darwin resource fork hazards --

    suite.test("darwinAppendCreatesResourceFork")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Appending "rsrc" after a component named "..namedfork" produces
      // a path whose tail matches the /..namedfork/rsrc suffix pattern.
      withPlatform(.darwin) {
        var path = FilePath("/file/..namedfork")
        expectFalse(path.isResourceFork)

        var cv = path.components
        cv.append("rsrc")
        path.components = cv

        expectEqual(path.description, "/file/..namedfork/rsrc")

        // Reparse sees the resource fork suffix
        expectTrue(path.isResourceFork)
        // The components no longer include ..namedfork and rsrc
        let newComps = path.components.map(\.description)
        expectEqual(newComps, ["file"])
      }
    }


    suite.test("darwinInsertBeforeRsrcBreaksSuffix")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Inserting between "..namedfork" and "rsrc" breaks the suffix pattern
      withPlatform(.darwin) {
        var path = FilePath("/file/..namedfork/rsrc")
        expectTrue(path.isResourceFork)
        expectEqual(path.components.map(\.description), ["file"])

        var cv = path.components
        cv.append("oops")
        path.components = cv

        // The setter preserves isResourceFork=false (trailing sep context)
        // but reconstruction from decomposed form doesn't auto-add the suffix.
        // This case is tricky: the original decomposition stripped the suffix,
        // so we only have ["file"] + the new component, no resource fork.
        expectEqual(path.components.map(\.description), ["file", "oops"])
        expectFalse(path.isResourceFork)
      }
    }


    suite.test("darwinRemoveLastCreatesResourceFork")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // /dir/file/..namedfork/rsrc/extra — the suffix doesn't match because
      // of trailing content. Removing "extra" exposes the suffix.
      withPlatform(.darwin) {
        var path = FilePath("/dir/file/..namedfork/rsrc/extra")
        expectFalse(path.isResourceFork)
        expectEqual(path.components.map(\.description), [
          "dir", "file", "..namedfork", "rsrc", "extra",
        ])

        var cv = path.components
        cv.removeLast()
        path.components = cv

        expectEqual(path.description, "/dir/file/..namedfork/rsrc")

        // Reparse now sees the resource fork suffix
        expectTrue(path.isResourceFork)
        let newComps = path.components.map(\.description)
        expectEqual(newComps, ["dir", "file"])
      }
    }


    suite.test("darwinReplaceCreatesResourceFork")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Replace last component with "rsrc" when penultimate is "..namedfork"
      withPlatform(.darwin) {
        var path = FilePath("/data/..namedfork/icon")
        expectFalse(path.isResourceFork)

        var cv = path.components
        cv.replaceSubrange(cv.index(before: cv.endIndex) ..< cv.endIndex,
                           with: ["rsrc" as FilePath.Component])
        path.components = cv

        expectEqual(path.description, "/data/..namedfork/rsrc")
        expectTrue(path.isResourceFork)
        expectEqual(path.components.map(\.description), ["data"])
      }
    }


    suite.test("darwinResourceForkOnRelativeIsSafe")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Resource fork suffix works on relative paths too
      withPlatform(.darwin) {
        var path = FilePath("file/..namedfork")
        var cv = path.components
        cv.append("rsrc")
        path.components = cv

        expectEqual(path.description, "file/..namedfork/rsrc")
        expectTrue(path.isResourceFork)
        expectEqual(path.components.map(\.description), ["file"])
      }
    }


    // -- Windows reparse hazards --

    suite.test("windowsRemoveExposesRootBackslash")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // \\server\share\only -> remove "only" -> \\server\share\
      // The trailing separator now belongs to the UNC anchor.
      withPlatform(.windows) {
        var path = FilePath(#"\\server\share\only"#)
        expectEqual(path.anchor?.description, #"\\server\share"#)
        expectEqual(path.components.map(\.description), ["only"])

        var cv = path.components
        cv.removeLast()
        path.components = cv

        // With no components, the anchor stands alone
        expectEqual(path.anchor?.description, #"\\server\share"#)
        expectTrue(path.components.isEmpty)
        expectTrue(path.hasTrailingSeparator)
      }
    }


    suite.test("windowsVerbatimDotPreserved")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // In verbatim paths (\\?\), dot and dotdot are regular components.
      // Appending "." to a verbatim path should NOT be treated as currentDirectory.
      withPlatform(.windows) {
        var path = FilePath(#"\\?\C:\dir"#)
        var cv = path.components
        cv.append(".")
        path.components = cv

        expectEqual(path.description, #"\\?\C:\dir\."#)
        // In verbatim context the "." is a regular component name
        let lastComp = path.components.last!
        expectEqual(lastComp.kind, .regular)
      }
    }


    suite.test("windowsVerbatimDotDotPreserved")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Similarly, ".." in verbatim paths is just a literal name
      withPlatform(.windows) {
        var path = FilePath(#"\\?\C:\dir"#)
        var cv = path.components
        cv.append("..")
        path.components = cv

        expectEqual(path.description, #"\\?\C:\dir\.."#)
        let lastComp = path.components.last!
        expectEqual(lastComp.kind, .regular)
      }
    }


    suite.test("windowsDevicePathAppend")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // \\.\device paths: appending to a device-only path
      withPlatform(.windows) {
        var path = FilePath(#"\\.\COM1"#)
        var cv = path.components
        cv.append("extra")
        path.components = cv

        expectEqual(path.description, #"\\.\COM1\extra"#)
        expectEqual(path.components.map(\.description), ["extra"])
      }
    }

    runAllTests()
  }
}
