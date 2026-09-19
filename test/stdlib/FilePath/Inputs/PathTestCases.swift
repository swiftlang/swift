//===--- PathTestCases.swift ----------------------------------------------===//
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

// Comprehensive test data for FilePath parsing across Linux, Darwin, and Windows.
//
// Each test case specifies the expected decomposition on all three platforms.
// Round-trip assertion: construct from input, decompose, reconstruct, check equality.
//
// Formatting: decomposition fields (anchor, components, hasTrailingSeparator,
// isResourceFork) on one line. Derived fields (printed, isAbsolute, isRooted,
// driveLetter, kinds) on the next.

@available(SwiftStdlib 9999, *)
extension Expected {
    /// Unix parses a backslash-containing path as a single component
    /// (backslash is not a separator on Unix).
    static func singleComponent(
        _ s: String, hasTrailingSeparator: Bool = false
    ) -> Expected {
        Expected(
            anchor: nil, components: [s],
            hasTrailingSeparator: hasTrailingSeparator,
            printed: hasTrailingSeparator ? s + "/" : s,
            isAbsolute: false)
    }
}

@available(SwiftStdlib 9999, *)
let pathTestCases: [PathTestCase] = [

    // MARK: - Empty and trivial

    PathTestCase(
        input: "",
        unix: Expected(
            anchor: nil, components: [],
            printed: "", isAbsolute: false),
        windows: Expected(
            anchor: nil, components: [],
            printed: "", isAbsolute: false)
    ),

    PathTestCase(
        input: "/",
        unix: Expected(
            anchor: "/", components: [],
            printed: "/", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [],
            printed: #"\"#, isAbsolute: false, isRooted: true)
    ),

    // Multiple slashes coalesce on Unix; Windows lowers them through `/`→`\`
    // conversion and then incomplete-UNC handling.

    PathTestCase(
        input: "//",
        unix: Expected(
            anchor: "/", components: [],
            printed: "/", isAbsolute: true),
        windows: Expected(
            // `//` -> `\\` -> incomplete UNC, padded to `\\\` (degraded root).
            anchor: #"\\\"#, components: [],
            printed: #"\\\"#, isAbsolute: true)
    ),

    PathTestCase(
        input: "///",
        unix: Expected(
            anchor: "/", components: [],
            printed: "/", isAbsolute: true),
        windows: Expected(
            // `///` -> `\\\`: 3+ leading backslashes are NOT a UNC/device
            // path; reduce to a single current-drive root.
            anchor: #"\"#, components: [],
            printed: #"\"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - Basic absolute Unix paths

    PathTestCase(
        input: "/foo/bar",
        unix: Expected(
            anchor: "/", components: ["foo", "bar"],
            printed: "/foo/bar", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "bar"],
            printed: #"\foo\bar"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: "/foo",
        unix: Expected(
            anchor: "/", components: ["foo"],
            printed: "/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo"],
            printed: #"\foo"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - Basic relative paths (no anchor)

    PathTestCase(
        input: "foo/bar",
        unix: Expected(
            anchor: nil, components: ["foo", "bar"],
            printed: "foo/bar", isAbsolute: false),
        windows: Expected(
            anchor: nil, components: ["foo", "bar"],
            printed: #"foo\bar"#, isAbsolute: false)
    ),

    PathTestCase(
        input: "foo",
        unix: Expected(
            anchor: nil, components: ["foo"],
            printed: "foo", isAbsolute: false),
        windows: Expected(
            anchor: nil, components: ["foo"],
            printed: "foo", isAbsolute: false)
    ),

    // MARK: - Leading dot

    PathTestCase(
        input: ".",
        unix: Expected(
            anchor: nil, components: ["."],
            printed: ".", isAbsolute: false, kinds: [.currentDirectory]),
        windows: Expected(
            anchor: nil, components: ["."],
            printed: ".", isAbsolute: false, kinds: [.currentDirectory])
    ),

    PathTestCase(
        input: "..",
        unix: Expected(
            anchor: nil, components: [".."],
            printed: "..", isAbsolute: false, kinds: [.parentDirectory]),
        windows: Expected(
            anchor: nil, components: [".."],
            printed: "..", isAbsolute: false, kinds: [.parentDirectory])
    ),

    PathTestCase(
        input: "./foo",
        unix: Expected(
            anchor: nil, components: [".", "foo"],
            printed: "./foo", isAbsolute: false, kinds: [.currentDirectory, .regular]),
        windows: Expected(
            anchor: nil, components: [".", "foo"],
            printed: #".\foo"#, isAbsolute: false, kinds: [.currentDirectory, .regular])
    ),

    PathTestCase(
        input: "../foo",
        unix: Expected(
            anchor: nil, components: ["..", "foo"],
            printed: "../foo", isAbsolute: false, kinds: [.parentDirectory, .regular]),
        windows: Expected(
            anchor: nil, components: ["..", "foo"],
            printed: #"..\foo"#, isAbsolute: false, kinds: [.parentDirectory, .regular])
    ),

    // MARK: - Interior dot normalization

    // Dot is first relative component after root: dropped (rooted)
    PathTestCase(
        input: "/./foo",
        unix: Expected(
            anchor: "/", components: ["foo"],
            printed: "/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo"],
            printed: #"\foo"#, isAbsolute: false, isRooted: true)
    ),

    // Absolute interior dot (not first): dropped
    PathTestCase(
        input: "/foo/./bar",
        unix: Expected(
            anchor: "/", components: ["foo", "bar"],
            printed: "/foo/bar", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "bar"],
            printed: #"\foo\bar"#, isAbsolute: false, isRooted: true)
    ),

    // Relative interior dot (not first): dropped
    PathTestCase(
        input: "foo/./bar",
        unix: Expected(
            anchor: nil, components: ["foo", "bar"],
            printed: "foo/bar", isAbsolute: false),
        windows: Expected(
            anchor: nil, components: ["foo", "bar"],
            printed: #"foo\bar"#, isAbsolute: false)
    ),

    // Trailing dot implies trailing separator
    PathTestCase(
        input: "foo/.",
        unix: Expected(
            anchor: nil, components: ["foo"], hasTrailingSeparator: true,
            printed: "foo/", isAbsolute: false),
        windows: Expected(
            anchor: nil, components: ["foo"], hasTrailingSeparator: true,
            printed: #"foo\"#, isAbsolute: false)
    ),

    // Dot is first relative component after root: dropped (path has root)
    PathTestCase(
        input: "/.",
        unix: Expected(
            anchor: "/", components: [],
            printed: "/", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [],
            printed: #"\"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - Interior dotdot (not lexically normalized, always presented)

    PathTestCase(
        input: "/foo/../bar",
        unix: Expected(
            anchor: "/", components: ["foo", "..", "bar"],
            printed: "/foo/../bar", isAbsolute: true, kinds: [.regular, .parentDirectory, .regular]),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "..", "bar"],
            printed: #"\foo\..\bar"#, isAbsolute: false, isRooted: true, kinds: [.regular, .parentDirectory, .regular])
    ),

    PathTestCase(
        input: "a/../b",
        unix: Expected(
            anchor: nil, components: ["a", "..", "b"],
            printed: "a/../b", isAbsolute: false, kinds: [.regular, .parentDirectory, .regular]),
        windows: Expected(
            anchor: nil, components: ["a", "..", "b"],
            printed: #"a\..\b"#, isAbsolute: false, kinds: [.regular, .parentDirectory, .regular])
    ),

    PathTestCase(
        input: "/a/b/../c",
        unix: Expected(
            anchor: "/", components: ["a", "b", "..", "c"],
            printed: "/a/b/../c", isAbsolute: true, kinds: [.regular, .regular, .parentDirectory, .regular]),
        windows: Expected(
            anchor: #"\"#, components: ["a", "b", "..", "c"],
            printed: #"\a\b\..\c"#, isAbsolute: false, isRooted: true, kinds: [.regular, .regular, .parentDirectory, .regular])
    ),

    // Trailing dotdot (not dropped, unlike trailing dot)
    PathTestCase(
        input: "foo/..",
        unix: Expected(
            anchor: nil, components: ["foo", ".."],
            printed: "foo/..", isAbsolute: false, kinds: [.regular, .parentDirectory]),
        windows: Expected(
            anchor: nil, components: ["foo", ".."],
            printed: #"foo\.."#, isAbsolute: false, kinds: [.regular, .parentDirectory])
    ),

    PathTestCase(
        input: "foo/bar/baz/..",
        unix: Expected(
            anchor: nil, components: ["foo", "bar", "baz", ".."],
            printed: "foo/bar/baz/..", isAbsolute: false,
            kinds: [.regular, .regular, .regular, .parentDirectory]),
        windows: Expected(
            anchor: nil, components: ["foo", "bar", "baz", ".."],
            printed: #"foo\bar\baz\.."#, isAbsolute: false,
            kinds: [.regular, .regular, .regular, .parentDirectory])
    ),

    PathTestCase(
        input: "../..",
        unix: Expected(
            anchor: nil, components: ["..", ".."],
            printed: "../..", isAbsolute: false, kinds: [.parentDirectory, .parentDirectory]),
        windows: Expected(
            anchor: nil, components: ["..", ".."],
            printed: #"..\.."#, isAbsolute: false, kinds: [.parentDirectory, .parentDirectory])
    ),

    // MARK: - Separator coalescing

    PathTestCase(
        input: "foo//bar",
        unix: Expected(
            anchor: nil, components: ["foo", "bar"],
            printed: "foo/bar", isAbsolute: false),
        windows: Expected(
            anchor: nil, components: ["foo", "bar"],
            printed: #"foo\bar"#, isAbsolute: false)
    ),

    PathTestCase(
        input: "a///b",
        unix: Expected(
            anchor: nil, components: ["a", "b"],
            printed: "a/b", isAbsolute: false),
        windows: Expected(
            anchor: nil, components: ["a", "b"],
            printed: #"a\b"#, isAbsolute: false)
    ),

    // Leading double slash: Linux coalesces, Windows sees UNC
    PathTestCase(
        input: "//foo/bar",
        unix: Expected(
            anchor: "/", components: ["foo", "bar"],
            printed: "/foo/bar", isAbsolute: true),
        // UNC: server=foo, share=bar
        windows: Expected(
            anchor: #"\\foo\bar"#, components: [],
            printed: #"\\foo\bar"#, isAbsolute: true)
    ),
    // TODO: verify that Windows does the backslash conversion _before_ checking for short-style UNC prefix

    PathTestCase(
        input: "///foo///bar",
        unix: Expected(
            anchor: "/", components: ["foo", "bar"],
            printed: "/foo/bar", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "bar"],
            printed: #"\foo\bar"#, isAbsolute: false, isRooted: true)
    ),
    // TODO: verify that Windows collapses _after_ checking for short-style UNC prefix

    // MARK: - Trailing separator significance

    PathTestCase(
        input: "/tmp/foo",
        unix: Expected(
            anchor: "/", components: ["tmp", "foo"],
            printed: "/tmp/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["tmp", "foo"],
            printed: #"\tmp\foo"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: "/tmp/foo/",
        unix: Expected(
            anchor: "/", components: ["tmp", "foo"], hasTrailingSeparator: true,
            printed: "/tmp/foo/", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["tmp", "foo"], hasTrailingSeparator: true,
            printed: #"\tmp\foo\"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: "foo/",
        unix: Expected(
            anchor: nil, components: ["foo"], hasTrailingSeparator: true,
            printed: "foo/", isAbsolute: false),
        windows: Expected(
            anchor: nil, components: ["foo"], hasTrailingSeparator: true,
            printed: #"foo\"#, isAbsolute: false)
    ),

    PathTestCase(
        input: "./",
        unix: Expected(
            anchor: nil, components: ["."], hasTrailingSeparator: true,
            printed: "./", isAbsolute: false, kinds: [.currentDirectory]),
        windows: Expected(
            anchor: nil, components: ["."], hasTrailingSeparator: true,
            printed: #".\"#, isAbsolute: false, kinds: [.currentDirectory])
    ),

    PathTestCase(
        input: "../",
        unix: Expected(
            anchor: nil, components: [".."], hasTrailingSeparator: true,
            printed: "../", isAbsolute: false, kinds: [.parentDirectory]),
        windows: Expected(
            anchor: nil, components: [".."], hasTrailingSeparator: true,
            printed: #"..\"#, isAbsolute: false, kinds: [.parentDirectory])
    ),

    // Trailing double sep coalesces to single trailing sep
    PathTestCase(
        input: "/foo//",
        unix: Expected(
            anchor: "/", components: ["foo"], hasTrailingSeparator: true,
            printed: "/foo/", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo"], hasTrailingSeparator: true,
            printed: #"\foo\"#, isAbsolute: false, isRooted: true)
    ),

    // Single component absolute with trailing sep
    PathTestCase(
        input: "/foo/",
        unix: Expected(
            anchor: "/", components: ["foo"], hasTrailingSeparator: true,
            printed: "/foo/", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo"], hasTrailingSeparator: true,
            printed: #"\foo\"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - More dot interactions

    // Mixed dots and dotdots (interior dot dropped, dotdot preserved)
    PathTestCase(
        input: "/foo/bar/./baz/../qux",
        unix: Expected(
            anchor: "/", components: ["foo", "bar", "baz", "..", "qux"],
            printed: "/foo/bar/baz/../qux", isAbsolute: true,
            kinds: [.regular, .regular, .regular, .parentDirectory, .regular]),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "bar", "baz", "..", "qux"],
            printed: #"\foo\bar\baz\..\qux"#, isAbsolute: false, isRooted: true,
            kinds: [.regular, .regular, .regular, .parentDirectory, .regular])
    ),

    // Triple dot is a regular name
    PathTestCase(
        input: "foo/.../bar",
        unix: Expected(
            anchor: nil, components: ["foo", "...", "bar"],
            printed: "foo/.../bar", isAbsolute: false),
        windows: Expected(
            anchor: nil, components: ["foo", "...", "bar"],
            printed: #"foo\...\bar"#, isAbsolute: false)
    ),

    // ..bar is a regular name (not parent directory)
    PathTestCase(
        input: "foo/..bar/baz",
        unix: Expected(
            anchor: nil, components: ["foo", "..bar", "baz"],
            printed: "foo/..bar/baz", isAbsolute: false),
        windows: Expected(
            anchor: nil, components: ["foo", "..bar", "baz"],
            printed: #"foo\..bar\baz"#, isAbsolute: false)
    ),

    // Dotdot at root
    PathTestCase(
        input: "/..",
        unix: Expected(
            anchor: "/", components: [".."],
            printed: "/..", isAbsolute: true, kinds: [.parentDirectory]),
        windows: Expected(
            anchor: #"\"#, components: [".."],
            printed: #"\.."#, isAbsolute: false, isRooted: true, kinds: [.parentDirectory])
    ),

    // Dot then dotdot at root: dot dropped (rooted), dotdot presented
    PathTestCase(
        input: "/./../foo",
        unix: Expected(
            anchor: "/", components: ["..", "foo"],
            printed: "/../foo", isAbsolute: true,
            kinds: [.parentDirectory, .regular]),
        windows: Expected(
            anchor: #"\"#, components: ["..", "foo"],
            printed: #"\..\foo"#, isAbsolute: false, isRooted: true,
            kinds: [.parentDirectory, .regular])
    ),

    // MARK: - Darwin resolve flags (linux/darwin differ)

    // /.resolve/1/ canonicalizes to /.nofollow/ (flag 1 = NOFOLLOW_ANY)
    PathTestCase(
        input: "/.resolve/1/foo/bar",
        linux: Expected(
            anchor: "/", components: [".resolve", "1", "foo", "bar"],
            printed: "/.resolve/1/foo/bar", isAbsolute: true),
        darwin: Expected(
            anchor: "/.nofollow/", components: ["foo", "bar"],
            printed: "/.nofollow/foo/bar", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "1", "foo", "bar"],
            printed: #"\.resolve\1\foo\bar"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: "/.nofollow/foo/bar",
        linux: Expected(
            anchor: "/", components: [".nofollow", "foo", "bar"],
            printed: "/.nofollow/foo/bar", isAbsolute: true),
        darwin: Expected(
            anchor: "/.nofollow/", components: ["foo", "bar"],
            printed: "/.nofollow/foo/bar", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".nofollow", "foo", "bar"],
            printed: #"\.nofollow\foo\bar"#, isAbsolute: false, isRooted: true)
    ),

    // Flag zero: no shorthand
    PathTestCase(
        input: "/.resolve/0/foo",
        linux: Expected(
            anchor: "/", components: [".resolve", "0", "foo"],
            printed: "/.resolve/0/foo", isAbsolute: true),
        darwin: Expected(
            anchor: "/.resolve/0/", components: ["foo"],
            printed: "/.resolve/0/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "0", "foo"],
            printed: #"\.resolve\0\foo"#, isAbsolute: false, isRooted: true)
    ),

    // NODOTDOT: no shorthand
    PathTestCase(
        input: "/.resolve/2/foo",
        linux: Expected(
            anchor: "/", components: [".resolve", "2", "foo"],
            printed: "/.resolve/2/foo", isAbsolute: true),
        darwin: Expected(
            anchor: "/.resolve/2/", components: ["foo"],
            printed: "/.resolve/2/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "2", "foo"],
            printed: #"\.resolve\2\foo"#, isAbsolute: false, isRooted: true)
    ),

    // NOFOLLOW_ANY | NODOTDOT: no shorthand
    PathTestCase(
        input: "/.resolve/3/foo/bar",
        linux: Expected(
            anchor: "/", components: [".resolve", "3", "foo", "bar"],
            printed: "/.resolve/3/foo/bar", isAbsolute: true),
        darwin: Expected(
            anchor: "/.resolve/3/", components: ["foo", "bar"],
            printed: "/.resolve/3/foo/bar", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "3", "foo", "bar"],
            printed: #"\.resolve\3\foo\bar"#, isAbsolute: false, isRooted: true)
    ),

    // Large flag value, multi-digit
    PathTestCase(
        input: "/.resolve/127/a/b",
        linux: Expected(
            anchor: "/", components: [".resolve", "127", "a", "b"],
            printed: "/.resolve/127/a/b", isAbsolute: true),
        darwin: Expected(
            anchor: "/.resolve/127/", components: ["a", "b"],
            printed: "/.resolve/127/a/b", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "127", "a", "b"],
            printed: #"\.resolve\127\a\b"#, isAbsolute: false, isRooted: true)
    ),

    // Very large number, tests multi-digit decimal parsing
    PathTestCase(
        input: "/.resolve/999999/foo",
        linux: Expected(
            anchor: "/", components: [".resolve", "999999", "foo"],
            printed: "/.resolve/999999/foo", isAbsolute: true),
        darwin: Expected(
            anchor: "/.resolve/999999/", components: ["foo"],
            printed: "/.resolve/999999/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "999999", "foo"],
            printed: #"\.resolve\999999\foo"#, isAbsolute: false, isRooted: true)
    ),

    // Without trailing slash: NOT a resolve prefix, regular component
    PathTestCase(
        input: "/.nofollow",
        unix: Expected(
            anchor: "/", components: [".nofollow"],
            printed: "/.nofollow", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".nofollow"],
            printed: #"\.nofollow"#, isAbsolute: false, isRooted: true)
    ),

    // Without trailing slash on resolve: NOT a prefix, regular components
    PathTestCase(
        input: "/.resolve/0",
        unix: Expected(
            anchor: "/", components: [".resolve", "0"],
            printed: "/.resolve/0", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "0"],
            printed: #"\.resolve\0"#, isAbsolute: false, isRooted: true)
    ),

    // Non-numeric flags: still a resolve prefix (field is opaque)
    PathTestCase(
        input: "/.resolve/abc/foo",
        linux: Expected(
            anchor: "/", components: [".resolve", "abc", "foo"],
            printed: "/.resolve/abc/foo", isAbsolute: true),
        darwin: Expected(
            anchor: "/.resolve/abc/", components: ["foo"],
            printed: "/.resolve/abc/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "abc", "foo"],
            printed: #"\.resolve\abc\foo"#, isAbsolute: false, isRooted: true)
    ),

    // Leading zero: field is opaque, "01" != "1", no canonicalization
    PathTestCase(
        input: "/.resolve/01/foo",
        linux: Expected(
            anchor: "/", components: [".resolve", "01", "foo"],
            printed: "/.resolve/01/foo", isAbsolute: true),
        darwin: Expected(
            anchor: "/.resolve/01/", components: ["foo"],
            printed: "/.resolve/01/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "01", "foo"],
            printed: #"\.resolve\01\foo"#, isAbsolute: false, isRooted: true)
    ),

    // Negative-looking flag: field is opaque, still a prefix
    PathTestCase(
        input: "/.resolve/-1/foo",
        linux: Expected(
            anchor: "/", components: [".resolve", "-1", "foo"],
            printed: "/.resolve/-1/foo", isAbsolute: true),
        darwin: Expected(
            anchor: "/.resolve/-1/", components: ["foo"],
            printed: "/.resolve/-1/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "-1", "foo"],
            printed: #"\.resolve\-1\foo"#, isAbsolute: false, isRooted: true)
    ),

    // Wrong case on "resolve" keyword: not a prefix
    PathTestCase(
        input: "/.Resolve/0/foo",
        unix: Expected(
            anchor: "/", components: [".Resolve", "0", "foo"],
            printed: "/.Resolve/0/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".Resolve", "0", "foo"],
            printed: #"\.Resolve\0\foo"#, isAbsolute: false, isRooted: true)
    ),

    // Starts with /. but matches no keyword: regular path
    PathTestCase(
        input: "/.hidden/foo",
        unix: Expected(
            anchor: "/", components: [".hidden", "foo"],
            printed: "/.hidden/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".hidden", "foo"],
            printed: #"\.hidden\foo"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - Darwin .vol paths (linux/darwin differ)

    PathTestCase(
        input: "/.vol/1234/5678",
        linux: Expected(
            anchor: "/", components: [".vol", "1234", "5678"],
            printed: "/.vol/1234/5678", isAbsolute: true),
        darwin: Expected(
            anchor: "/.vol/1234/5678", components: [],
            printed: "/.vol/1234/5678", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "1234", "5678"],
            printed: #"\.vol\1234\5678"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: "/.vol/1234/5678/",
        linux: Expected(
            anchor: "/", components: [".vol", "1234", "5678"], hasTrailingSeparator: true,
            printed: "/.vol/1234/5678/", isAbsolute: true),
        darwin: Expected(
            anchor: "/.vol/1234/5678", components: [], hasTrailingSeparator: true,
            printed: "/.vol/1234/5678/", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "1234", "5678"], hasTrailingSeparator: true,
            printed: #"\.vol\1234\5678\"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: "/.vol/1234/5678/foo/bar",
        linux: Expected(
            anchor: "/", components: [".vol", "1234", "5678", "foo", "bar"],
            printed: "/.vol/1234/5678/foo/bar", isAbsolute: true),
        darwin: Expected(
            anchor: "/.vol/1234/5678", components: ["foo", "bar"],
            printed: "/.vol/1234/5678/foo/bar", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "1234", "5678", "foo", "bar"],
            printed: #"\.vol\1234\5678\foo\bar"#, isAbsolute: false, isRooted: true)
    ),

    // Fileid "2" canonicalizes to "@"
    PathTestCase(
        input: "/.vol/1234/2",
        linux: Expected(
            anchor: "/", components: [".vol", "1234", "2"],
            printed: "/.vol/1234/2", isAbsolute: true),
        darwin: Expected(
            anchor: "/.vol/1234/@", components: [],
            printed: "/.vol/1234/@", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "1234", "2"],
            printed: #"\.vol\1234\2"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: "/.vol/1234/2/foo/bar",
        linux: Expected(
            anchor: "/", components: [".vol", "1234", "2", "foo", "bar"],
            printed: "/.vol/1234/2/foo/bar", isAbsolute: true),
        darwin: Expected(
            anchor: "/.vol/1234/@", components: ["foo", "bar"],
            printed: "/.vol/1234/@/foo/bar", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "1234", "2", "foo", "bar"],
            printed: #"\.vol\1234\2\foo\bar"#, isAbsolute: false, isRooted: true)
    ),

    // "@" already canonical: round-trips
    PathTestCase(
        input: "/.vol/1234/@",
        linux: Expected(
            anchor: "/", components: [".vol", "1234", "@"],
            printed: "/.vol/1234/@", isAbsolute: true),
        darwin: Expected(
            anchor: "/.vol/1234/@", components: [],
            printed: "/.vol/1234/@", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "1234", "@"],
            printed: #"\.vol\1234\@"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: "/.vol/1234/@/",
        linux: Expected(
            anchor: "/", components: [".vol", "1234", "@"], hasTrailingSeparator: true,
            printed: "/.vol/1234/@/", isAbsolute: true),
        darwin: Expected(
            anchor: "/.vol/1234/@", components: [], hasTrailingSeparator: true,
            printed: "/.vol/1234/@/", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "1234", "@"], hasTrailingSeparator: true,
            printed: #"\.vol\1234\@\"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: "/.vol/1234/@/foo",
        linux: Expected(
            anchor: "/", components: [".vol", "1234", "@", "foo"],
            printed: "/.vol/1234/@/foo", isAbsolute: true),
        darwin: Expected(
            anchor: "/.vol/1234/@", components: ["foo"],
            printed: "/.vol/1234/@/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "1234", "@", "foo"],
            printed: #"\.vol\1234\@\foo"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - Darwin combined anchors (resolve flag + volume identifier)
    //
    // Per proposal line 111: an anchor may include resolve flags AND/OR
    // a volume identifier; resolve always precedes vol. The combined form
    // `[/.nofollow/ | /.resolve/N/].vol/FSID/FILEID` is a single anchor.
    // Both canonicalizations (`/.resolve/1/` -> `/.nofollow/`, `2` -> `@`)
    // can fire on the same input.

    // Combined nofollow + vol, no canonicalization.
    PathTestCase(
        input: "/.nofollow/.vol/1234/5678",
        linux: Expected(
            anchor: "/", components: [".nofollow", ".vol", "1234", "5678"],
            printed: "/.nofollow/.vol/1234/5678", isAbsolute: true),
        darwin: Expected(
            anchor: "/.nofollow/.vol/1234/5678", components: [],
            printed: "/.nofollow/.vol/1234/5678", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".nofollow", ".vol", "1234", "5678"],
            printed: #"\.nofollow\.vol\1234\5678"#, isAbsolute: false, isRooted: true)
    ),

    // Combined nofollow + vol with relative content.
    PathTestCase(
        input: "/.nofollow/.vol/1234/5678/foo",
        linux: Expected(
            anchor: "/", components: [".nofollow", ".vol", "1234", "5678", "foo"],
            printed: "/.nofollow/.vol/1234/5678/foo", isAbsolute: true),
        darwin: Expected(
            anchor: "/.nofollow/.vol/1234/5678", components: ["foo"],
            printed: "/.nofollow/.vol/1234/5678/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".nofollow", ".vol", "1234", "5678", "foo"],
            printed: #"\.nofollow\.vol\1234\5678\foo"#, isAbsolute: false, isRooted: true)
    ),

    // Combined nofollow + vol with FILEID canonicalization (`2` -> `@`).
    PathTestCase(
        input: "/.nofollow/.vol/1234/2",
        linux: Expected(
            anchor: "/", components: [".nofollow", ".vol", "1234", "2"],
            printed: "/.nofollow/.vol/1234/2", isAbsolute: true),
        darwin: Expected(
            anchor: "/.nofollow/.vol/1234/@", components: [],
            printed: "/.nofollow/.vol/1234/@", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".nofollow", ".vol", "1234", "2"],
            printed: #"\.nofollow\.vol\1234\2"#, isAbsolute: false, isRooted: true)
    ),

    // Combined nofollow + vol, FILEID canonicalization, with relative + trailing.
    PathTestCase(
        input: "/.nofollow/.vol/1234/2/foo/",
        linux: Expected(
            anchor: "/", components: [".nofollow", ".vol", "1234", "2", "foo"],
            hasTrailingSeparator: true,
            printed: "/.nofollow/.vol/1234/2/foo/", isAbsolute: true),
        darwin: Expected(
            anchor: "/.nofollow/.vol/1234/@", components: ["foo"],
            hasTrailingSeparator: true,
            printed: "/.nofollow/.vol/1234/@/foo/", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".nofollow", ".vol", "1234", "2", "foo"],
            hasTrailingSeparator: true,
            printed: #"\.nofollow\.vol\1234\2\foo\"#, isAbsolute: false, isRooted: true)
    ),

    // Combined resolve/N + vol (non-canonicalizing flag value).
    PathTestCase(
        input: "/.resolve/3/.vol/1234/5678",
        linux: Expected(
            anchor: "/", components: [".resolve", "3", ".vol", "1234", "5678"],
            printed: "/.resolve/3/.vol/1234/5678", isAbsolute: true),
        darwin: Expected(
            anchor: "/.resolve/3/.vol/1234/5678", components: [],
            printed: "/.resolve/3/.vol/1234/5678", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "3", ".vol", "1234", "5678"],
            printed: #"\.resolve\3\.vol\1234\5678"#, isAbsolute: false, isRooted: true)
    ),

    // Combined resolve/3 + vol with FILEID canon — only the FILEID rule
    // fires; `.resolve/3/` is preserved as written.
    PathTestCase(
        input: "/.resolve/3/.vol/1234/2/",
        linux: Expected(
            anchor: "/", components: [".resolve", "3", ".vol", "1234", "2"],
            hasTrailingSeparator: true,
            printed: "/.resolve/3/.vol/1234/2/", isAbsolute: true),
        darwin: Expected(
            anchor: "/.resolve/3/.vol/1234/@", components: [],
            hasTrailingSeparator: true,
            printed: "/.resolve/3/.vol/1234/@/", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "3", ".vol", "1234", "2"],
            hasTrailingSeparator: true,
            printed: #"\.resolve\3\.vol\1234\2\"#, isAbsolute: false, isRooted: true)
    ),

    // Combined resolve/1 + vol — BOTH canonicalizations fire:
    // `/.resolve/1/` -> `/.nofollow/` AND FILEID `2` -> `@`.
    PathTestCase(
        input: "/.resolve/1/.vol/1234/2/",
        linux: Expected(
            anchor: "/", components: [".resolve", "1", ".vol", "1234", "2"],
            hasTrailingSeparator: true,
            printed: "/.resolve/1/.vol/1234/2/", isAbsolute: true),
        darwin: Expected(
            anchor: "/.nofollow/.vol/1234/@", components: [],
            hasTrailingSeparator: true,
            printed: "/.nofollow/.vol/1234/@/", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "1", ".vol", "1234", "2"],
            hasTrailingSeparator: true,
            printed: #"\.resolve\1\.vol\1234\2\"#, isAbsolute: false, isRooted: true)
    ),

    // Combined resolve/1 + vol with relative content — both canons + relative.
    PathTestCase(
        input: "/.resolve/1/.vol/1234/2/foo/bar",
        linux: Expected(
            anchor: "/", components: [".resolve", "1", ".vol", "1234", "2", "foo", "bar"],
            printed: "/.resolve/1/.vol/1234/2/foo/bar", isAbsolute: true),
        darwin: Expected(
            anchor: "/.nofollow/.vol/1234/@", components: ["foo", "bar"],
            printed: "/.nofollow/.vol/1234/@/foo/bar", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "1", ".vol", "1234", "2", "foo", "bar"],
            printed: #"\.resolve\1\.vol\1234\2\foo\bar"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - Darwin resource forks (linux/darwin differ)

    PathTestCase(
        input: "/foo/..namedfork/rsrc",
        linux: Expected(
            anchor: "/", components: ["foo", "..namedfork", "rsrc"],
            printed: "/foo/..namedfork/rsrc", isAbsolute: true),
        darwin: Expected(
            anchor: "/", components: ["foo"], isResourceFork: true,
            printed: "/foo/..namedfork/rsrc", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "..namedfork", "rsrc"],
            printed: #"\foo\..namedfork\rsrc"#, isAbsolute: false, isRooted: true)
    ),

    // Trailing slash breaks resource fork recognition
    PathTestCase(
        input: "/foo/..namedfork/rsrc/",
        unix: Expected(
            anchor: "/", components: ["foo", "..namedfork", "rsrc"], hasTrailingSeparator: true,
            printed: "/foo/..namedfork/rsrc/", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "..namedfork", "rsrc"], hasTrailingSeparator: true,
            printed: #"\foo\..namedfork\rsrc\"#, isAbsolute: false, isRooted: true)
    ),

    // Resource fork on relative path
    PathTestCase(
        input: "foo/..namedfork/rsrc",
        linux: Expected(
            anchor: nil, components: ["foo", "..namedfork", "rsrc"],
            printed: "foo/..namedfork/rsrc", isAbsolute: false),
        darwin: Expected(
            anchor: nil, components: ["foo"], isResourceFork: true,
            printed: "foo/..namedfork/rsrc", isAbsolute: false),
        windows: Expected(
            anchor: nil, components: ["foo", "..namedfork", "rsrc"],
            printed: #"foo\..namedfork\rsrc"#, isAbsolute: false)
    ),

    // Resource fork on root-only path (semantically invalid, syntactically recognized)
    PathTestCase(
        input: "/..namedfork/rsrc",
        linux: Expected(
            anchor: "/", components: ["..namedfork", "rsrc"],
            printed: "/..namedfork/rsrc", isAbsolute: true),
        darwin: Expected(
            anchor: "/", components: [], isResourceFork: true,
            printed: "/..namedfork/rsrc", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["..namedfork", "rsrc"],
            printed: #"\..namedfork\rsrc"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - Windows drive absolute

    PathTestCase(
        input: #"C:\foo\bar"#,
        // Backslash is a legal filename char on Unix: single component
        unix: .singleComponent(#"C:\foo\bar"#),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo", "bar"],
            printed: #"C:\foo\bar"#, isAbsolute: true, driveLetter: "C")
    ),

    PathTestCase(
        input: #"C:\"#,
        unix: .singleComponent(#"C:\"#),
        windows: Expected(
            anchor: #"C:\"#, components: [],
            printed: #"C:\"#, isAbsolute: true, driveLetter: "C")
    ),

    PathTestCase(
        input: #"c:\foo"#,
        unix: .singleComponent(#"c:\foo"#),
        windows: Expected(
            anchor: #"c:\"#, components: ["foo"],
            printed: #"c:\foo"#, isAbsolute: true, driveLetter: "c")
    ),

    // Interior dot dropped
    PathTestCase(
        input: #"C:\foo\.\bar"#,
        unix: .singleComponent(#"C:\foo\.\bar"#),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo", "bar"],
            printed: #"C:\foo\bar"#, isAbsolute: true, driveLetter: "C")
    ),

    // Interior dotdot presented, not collapsed
    PathTestCase(
        input: #"C:\foo\..\bar"#,
        unix: .singleComponent(#"C:\foo\..\bar"#),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo", "..", "bar"],
            printed: #"C:\foo\..\bar"#, isAbsolute: true, driveLetter: "C",
            kinds: [.regular, .parentDirectory, .regular])
    ),

    PathTestCase(
        input: #"C:\foo\"#,
        unix: .singleComponent(#"C:\foo\"#),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo"], hasTrailingSeparator: true,
            printed: #"C:\foo\"#, isAbsolute: true, driveLetter: "C")
    ),

    // MARK: - Windows drive relative

    PathTestCase(
        input: "C:foo",
        unix: Expected(
            anchor: nil, components: ["C:foo"],
            printed: "C:foo", isAbsolute: false),
        windows: Expected(
            anchor: "C:", components: ["foo"],
            printed: "C:foo", isAbsolute: false, driveLetter: "C")
    ),

    PathTestCase(
        input: "C:",
        unix: Expected(
            anchor: nil, components: ["C:"],
            printed: "C:", isAbsolute: false),
        windows: Expected(
            anchor: "C:", components: [],
            printed: "C:", isAbsolute: false, driveLetter: "C")
    ),

    PathTestCase(
        input: #"C:foo\"#,
        unix: .singleComponent(#"C:foo\"#),
        windows: Expected(
            anchor: "C:", components: ["foo"], hasTrailingSeparator: true,
            printed: #"C:foo\"#, isAbsolute: false, driveLetter: "C")
    ),

    // MARK: - Windows rooted-not-absolute

    PathTestCase(
        input: #"\foo"#,
        unix: .singleComponent(#"\foo"#),
        windows: Expected(
            anchor: #"\"#, components: ["foo"],
            printed: #"\foo"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: #"\foo\bar"#,
        unix: .singleComponent(#"\foo\bar"#),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "bar"],
            printed: #"\foo\bar"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: #"\"#,
        unix: .singleComponent(#"\"#),
        windows: Expected(
            anchor: #"\"#, components: [],
            printed: #"\"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - Windows UNC

    PathTestCase(
        input: #"\\server\share\foo"#,
        unix: .singleComponent(#"\\server\share\foo"#),
        windows: Expected(
            anchor: #"\\server\share"#, components: ["foo"],
            printed: #"\\server\share\foo"#, isAbsolute: true)
    ),

    PathTestCase(
        input: #"\\server\share\"#,
        unix: .singleComponent(#"\\server\share\"#),
        windows: Expected(
            anchor: #"\\server\share"#, components: [], hasTrailingSeparator: true,
            printed: #"\\server\share\"#, isAbsolute: true)
    ),

    PathTestCase(
        input: #"\\server\share"#,
        unix: .singleComponent(#"\\server\share"#),
        windows: Expected(
            anchor: #"\\server\share"#, components: [],
            printed: #"\\server\share"#, isAbsolute: true)
    ),

    PathTestCase(
        input: #"\\server\share\foo\bar\"#,
        unix: .singleComponent(#"\\server\share\foo\bar\"#),
        windows: Expected(
            anchor: #"\\server\share"#, components: ["foo", "bar"], hasTrailingSeparator: true,
            printed: #"\\server\share\foo\bar\"#, isAbsolute: true)
    ),

    // MARK: - Windows device paths (\\.\)

    // Bare device prefix: anchor only
    PathTestCase(
        input: #"\\.\"#,
        unix: .singleComponent(#"\\.\"#),
        windows: Expected(
            anchor: #"\\.\"#, components: [],
            printed: #"\\.\"#, isAbsolute: true)
    ),

    PathTestCase(
        input: #"\\.\C:\foo"#,
        unix: .singleComponent(#"\\.\C:\foo"#),
        windows: Expected(
            anchor: #"\\.\C:\"#, components: ["foo"],
            printed: #"\\.\C:\foo"#, isAbsolute: true, driveLetter: "C")
    ),

    PathTestCase(
        input: #"\\.\pipe\name"#,
        unix: .singleComponent(#"\\.\pipe\name"#),
        windows: Expected(
            anchor: #"\\.\pipe"#, components: ["name"],
            printed: #"\\.\pipe\name"#, isAbsolute: true)
    ),

    // Pipe device with trailing sep, no pipe name
    PathTestCase(
        input: #"\\.\pipe\"#,
        unix: .singleComponent(#"\\.\pipe\"#),
        windows: Expected(
            anchor: #"\\.\pipe"#, components: [], hasTrailingSeparator: true,
            printed: #"\\.\pipe\"#, isAbsolute: true)
    ),

    // Device drive without root separator
    PathTestCase(
        input: #"\\.\C:"#,
        unix: .singleComponent(#"\\.\C:"#),
        windows: Expected(
            anchor: #"\\.\C:"#, components: [],
            printed: #"\\.\C:"#, isAbsolute: true, driveLetter: "C")
    ),

    // Dot IS dropped on non-verbatim device paths
    PathTestCase(
        input: #"\\.\C:\foo\.\bar"#,
        unix: .singleComponent(#"\\.\C:\foo\.\bar"#),
        windows: Expected(
            anchor: #"\\.\C:\"#, components: ["foo", "bar"],
            printed: #"\\.\C:\foo\bar"#, isAbsolute: true, driveLetter: "C")
    ),

    // MARK: - Windows verbatim paths (\\?\)

    // Bare verbatim prefix: anchor only
    PathTestCase(
        input: #"\\?\"#,
        unix: .singleComponent(#"\\?\"#),
        windows: Expected(
            anchor: #"\\?\"#, components: [],
            printed: #"\\?\"#, isAbsolute: true)
    ),

    // Verbatim drive without root separator
    PathTestCase(
        input: #"\\?\C:"#,
        unix: .singleComponent(#"\\?\C:"#),
        windows: Expected(
            anchor: #"\\?\C:"#, components: [],
            printed: #"\\?\C:"#, isAbsolute: true, driveLetter: "C")
    ),

    // Dot NOT dropped in verbatim: treated as regular component
    PathTestCase(
        input: #"\\?\C:\foo\.\bar"#,
        unix: .singleComponent(#"\\?\C:\foo\.\bar"#),
        windows: Expected(
            anchor: #"\\?\C:\"#, components: ["foo", ".", "bar"],
            printed: #"\\?\C:\foo\.\bar"#, isAbsolute: true, driveLetter: "C",
            kinds: [.regular, .regular, .regular])
    ),

    // Dotdot NOT special in verbatim: treated as regular component
    PathTestCase(
        input: #"\\?\C:\foo\..\bar"#,
        unix: .singleComponent(#"\\?\C:\foo\..\bar"#),
        windows: Expected(
            anchor: #"\\?\C:\"#, components: ["foo", "..", "bar"],
            printed: #"\\?\C:\foo\..\bar"#, isAbsolute: true, driveLetter: "C",
            kinds: [.regular, .regular, .regular])
    ),

    PathTestCase(
        input: #"\\?\UNC\server\share\foo"#,
        unix: .singleComponent(#"\\?\UNC\server\share\foo"#),
        windows: Expected(
            anchor: #"\\?\UNC\server\share"#, components: ["foo"],
            printed: #"\\?\UNC\server\share\foo"#, isAbsolute: true)
    ),

    PathTestCase(
        input: #"\\?\C:\"#,
        unix: .singleComponent(#"\\?\C:\"#),
        windows: Expected(
            anchor: #"\\?\C:\"#, components: [],
            printed: #"\\?\C:\"#, isAbsolute: true, driveLetter: "C")
    ),

    PathTestCase(
        input: #"\\?\C:\foo\"#,
        unix: .singleComponent(#"\\?\C:\foo\"#),
        windows: Expected(
            anchor: #"\\?\C:\"#, components: ["foo"], hasTrailingSeparator: true,
            printed: #"\\?\C:\foo\"#, isAbsolute: true, driveLetter: "C")
    ),

    // Separator coalescing still happens in verbatim (verbatim only affects dot/dotdot)
    PathTestCase(
        input: #"\\?\C:\foo\\bar"#,
        unix: .singleComponent(#"\\?\C:\foo\\bar"#),
        windows: Expected(
            anchor: #"\\?\C:\"#, components: ["foo", "bar"],
            printed: #"\\?\C:\foo\bar"#, isAbsolute: true, driveLetter: "C")
    ),

    // MARK: - Forward slashes inside verbatim component content

    // / inside verbatim component is a legal filename character, not a separator
    PathTestCase(
        input: #"\\?\C:\foo/bar"#,
        unix: Expected(
            anchor: nil, components: [#"\\?\C:\foo"#, "bar"],
            printed: #"\\?\C:\foo/bar"#, isAbsolute: false),
        windows: Expected(
            anchor: #"\\?\C:\"#, components: ["foo/bar"],
            printed: #"\\?\C:\foo/bar"#, isAbsolute: true, driveLetter: "C")
    ),

    PathTestCase(
        input: #"\\?\C:\a/b/c"#,
        unix: Expected(
            anchor: nil, components: [#"\\?\C:\a"#, "b", "c"],
            printed: #"\\?\C:\a/b/c"#, isAbsolute: false),
        windows: Expected(
            anchor: #"\\?\C:\"#, components: ["a/b/c"],
            printed: #"\\?\C:\a/b/c"#, isAbsolute: true, driveLetter: "C")
    ),

    PathTestCase(
        input: #"\\?\UNC\s\sh\a/b"#,
        unix: Expected(
            anchor: nil, components: [#"\\?\UNC\s\sh\a"#, "b"],
            printed: #"\\?\UNC\s\sh\a/b"#, isAbsolute: false),
        windows: Expected(
            anchor: #"\\?\UNC\s\sh"#, components: ["a/b"],
            printed: #"\\?\UNC\s\sh\a/b"#, isAbsolute: true)
    ),

    // //?/ is NOT verbatim — it's device-namespace (same as //./). All / converted.
    PathTestCase(
        input: "//?/C:/a/b",
        unix: Expected(
            anchor: "/", components: ["?", "C:", "a", "b"],
            printed: "/?/C:/a/b", isAbsolute: true),
        windows: Expected(
            anchor: #"\\.\C:\"#, components: ["a", "b"],
            printed: #"\\.\C:\a\b"#, isAbsolute: true, driveLetter: "C")
    ),

    // Non-verbatim device path: all / converted (regression check)
    PathTestCase(
        input: "//./C:/a/b",
        unix: Expected(
            anchor: "/", components: ["C:", "a", "b"],
            printed: "/C:/a/b", isAbsolute: true),
        windows: Expected(
            anchor: #"\\.\C:\"#, components: ["a", "b"],
            printed: #"\\.\C:\a\b"#, isAbsolute: true, driveLetter: "C")
    ),

    // MARK: - Windows forward slash normalization

    // Triple slash: coalesces on both, rooted on Windows
    PathTestCase(
        input: "///foo/bar",
        unix: Expected(
            anchor: "/", components: ["foo", "bar"],
            printed: "/foo/bar", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "bar"],
            printed: #"\foo\bar"#, isAbsolute: false, isRooted: true)
    ),

    // //./foo on Unix: dot dropped (rooted), "foo" is a regular component
    // //./foo on Windows: device-namespace prefix, "foo" is device name
    PathTestCase(
        input: "//./foo",
        unix: Expected(
            anchor: "/", components: ["foo"],
            printed: "/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\\.\foo"#, components: [],
            printed: #"\\.\foo"#, isAbsolute: true)
    ),

    // //?/foo on Unix: regular components
    // //?/foo on Windows: device-namespace (not verbatim), "foo" is device name
    PathTestCase(
        input: "//?/foo",
        unix: Expected(
            anchor: "/", components: ["?", "foo"],
            printed: "/?/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\\.\foo"#, components: [],
            printed: #"\\.\foo"#, isAbsolute: true)
    ),

    PathTestCase(
        input: "C:/foo/bar",
        // Unix: "C:" is a component (colon is legal), "/" is separator
        unix: Expected(
            anchor: nil, components: ["C:", "foo", "bar"],
            printed: "C:/foo/bar", isAbsolute: false),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo", "bar"],
            printed: #"C:\foo\bar"#, isAbsolute: true, driveLetter: "C")
    ),

    PathTestCase(
        input: "C:/",
        unix: Expected(
            anchor: nil, components: ["C:"], hasTrailingSeparator: true,
            printed: "C:/", isAbsolute: false),
        windows: Expected(
            anchor: #"C:\"#, components: [],
            printed: #"C:\"#, isAbsolute: true, driveLetter: "C")
    ),

    PathTestCase(
        input: "//server/share/foo",
        unix: Expected(
            anchor: "/", components: ["server", "share", "foo"],
            printed: "/server/share/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\\server\share"#, components: ["foo"],
            printed: #"\\server\share\foo"#, isAbsolute: true)
    ),

    // Dot dropped (rooted) on Unix, device-namespace on Windows
    PathTestCase(
        input: "//./C:/foo",
        unix: Expected(
            anchor: "/", components: ["C:", "foo"],
            printed: "/C:/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\\.\C:\"#, components: ["foo"],
            printed: #"\\.\C:\foo"#, isAbsolute: true, driveLetter: "C")
    ),

    PathTestCase(
        input: "//?/C:/foo",
        unix: Expected(
            anchor: "/", components: ["?", "C:", "foo"],
            printed: "/?/C:/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\\.\C:\"#, components: ["foo"],
            printed: #"\\.\C:\foo"#, isAbsolute: true, driveLetter: "C")
    ),

    // MARK: - Cross-platform mismatch

    // Backslash is a legal filename char on Unix
    PathTestCase(
        input: #"foo\bar"#,
        unix: .singleComponent(#"foo\bar"#),
        windows: Expected(
            anchor: nil, components: ["foo", "bar"],
            printed: #"foo\bar"#, isAbsolute: false)
    ),

    // Dot-backslash: Unix sees single component, Windows sees dot + sep + component
    PathTestCase(
        input: #".\foo"#,
        unix: .singleComponent(#".\foo"#),
        windows: Expected(
            anchor: nil, components: [".", "foo"],
            printed: #".\foo"#, isAbsolute: false, kinds: [.currentDirectory, .regular])
    ),

    // Colon in component: legal on Unix, possible ADS marker on Windows
    PathTestCase(
        input: "foo:bar",
        unix: Expected(
            anchor: nil, components: ["foo:bar"],
            printed: "foo:bar", isAbsolute: false),
        windows: Expected(
            anchor: nil, components: ["foo:bar"],
            printed: "foo:bar", isAbsolute: false)
    ),

    // Deeper relative with only backslashes
    PathTestCase(
        input: #"foo\bar\baz"#,
        unix: .singleComponent(#"foo\bar\baz"#),
        windows: Expected(
            anchor: nil, components: ["foo", "bar", "baz"],
            printed: #"foo\bar\baz"#, isAbsolute: false)
    ),

    // Multi-char before colon: not a drive letter
    PathTestCase(
        input: #"CC:\foo"#,
        unix: .singleComponent(#"CC:\foo"#),
        windows: Expected(
            anchor: nil, components: ["CC:", "foo"],
            printed: #"CC:\foo"#, isAbsolute: false)
    ),

    // MARK: - Mixed separators on Windows-style paths

    // Forward slash splits on Unix (two components), both seps normalize on Windows
    PathTestCase(
        input: #"C:\foo/bar"#,
        unix: Expected(
            anchor: nil, components: [#"C:\foo"#, "bar"],
            printed: #"C:\foo/bar"#, isAbsolute: false),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo", "bar"],
            printed: #"C:\foo\bar"#, isAbsolute: true, driveLetter: "C")
    ),

    PathTestCase(
        input: #"C:\foo\bar/baz"#,
        unix: Expected(
            anchor: nil, components: [#"C:\foo\bar"#, "baz"],
            printed: #"C:\foo\bar/baz"#, isAbsolute: false),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo", "bar", "baz"],
            printed: #"C:\foo\bar\baz"#, isAbsolute: true, driveLetter: "C")
    ),

    // UNC server\share then forward slash: Unix splits at /
    PathTestCase(
        input: #"\\server\share/foo"#,
        unix: Expected(
            anchor: nil, components: [#"\\server\share"#, "foo"],
            printed: #"\\server\share/foo"#, isAbsolute: false),
        windows: Expected(
            anchor: #"\\server\share"#, components: ["foo"],
            printed: #"\\server\share\foo"#, isAbsolute: true)
    ),

    // MARK: - Trailing forward slash on backslash paths

    // Unix: forward slash IS a separator, splits the backslash-blob from trailing sep
    PathTestCase(
        input: #"C:\foo/"#,
        unix: .singleComponent(#"C:\foo"#, hasTrailingSeparator: true),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo"], hasTrailingSeparator: true,
            printed: #"C:\foo\"#, isAbsolute: true, driveLetter: "C")
    ),

    PathTestCase(
        input: #"C:\foo\bar/"#,
        unix: .singleComponent(#"C:\foo\bar"#, hasTrailingSeparator: true),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo", "bar"], hasTrailingSeparator: true,
            printed: #"C:\foo\bar\"#, isAbsolute: true, driveLetter: "C")
    ),

    PathTestCase(
        input: #"\\server\share/"#,
        unix: .singleComponent(#"\\server\share"#, hasTrailingSeparator: true),
        windows: Expected(
            anchor: #"\\server\share"#, components: [], hasTrailingSeparator: true,
            printed: #"\\server\share\"#, isAbsolute: true)
    ),

    // MARK: - Unicode in components

    PathTestCase(
        input: "/あ/🧟‍♀️",
        unix: Expected(
            anchor: "/", components: ["あ", "🧟‍♀️"],
            printed: "/あ/🧟‍♀️", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["あ", "🧟‍♀️"],
            printed: #"\あ\🧟‍♀️"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - Additional dot/dotdot patterns

    PathTestCase(
        input: "./foo/bar",
        unix: Expected(
            anchor: nil, components: [".", "foo", "bar"],
            printed: "./foo/bar", isAbsolute: false, kinds: [.currentDirectory, .regular, .regular]),
        windows: Expected(
            anchor: nil, components: [".", "foo", "bar"],
            printed: #".\foo\bar"#, isAbsolute: false, kinds: [.currentDirectory, .regular, .regular])
    ),

    // Dot then double slash: coalesced, dot preserved as first
    PathTestCase(
        input: ".//foo",
        unix: Expected(
            anchor: nil, components: [".", "foo"],
            printed: "./foo", isAbsolute: false, kinds: [.currentDirectory, .regular]),
        windows: Expected(
            anchor: nil, components: [".", "foo"],
            printed: #".\foo"#, isAbsolute: false, kinds: [.currentDirectory, .regular])
    ),

    PathTestCase(
        input: "foo/../../bar",
        unix: Expected(
            anchor: nil, components: ["foo", "..", "..", "bar"],
            printed: "foo/../../bar", isAbsolute: false,
            kinds: [.regular, .parentDirectory, .parentDirectory, .regular]),
        windows: Expected(
            anchor: nil, components: ["foo", "..", "..", "bar"],
            printed: #"foo\..\..\bar"#, isAbsolute: false,
            kinds: [.regular, .parentDirectory, .parentDirectory, .regular])
    ),

    PathTestCase(
        input: "foo/../bar/../baz",
        unix: Expected(
            anchor: nil, components: ["foo", "..", "bar", "..", "baz"],
            printed: "foo/../bar/../baz", isAbsolute: false,
            kinds: [.regular, .parentDirectory, .regular, .parentDirectory, .regular]),
        windows: Expected(
            anchor: nil, components: ["foo", "..", "bar", "..", "baz"],
            printed: #"foo\..\bar\..\baz"#, isAbsolute: false,
            kinds: [.regular, .parentDirectory, .regular, .parentDirectory, .regular])
    ),

    PathTestCase(
        input: "/foo//bar//baz",
        unix: Expected(
            anchor: "/", components: ["foo", "bar", "baz"],
            printed: "/foo/bar/baz", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "bar", "baz"],
            printed: #"\foo\bar\baz"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - More Windows edge cases

    PathTestCase(
        input: #"C:\.."#,
        unix: .singleComponent(#"C:\.."#),
        windows: Expected(
            anchor: #"C:\"#, components: [".."],
            printed: #"C:\.."#, isAbsolute: true, driveLetter: "C",
            kinds: [.parentDirectory])
    ),

    // Double sep after drive root: coalesced
    PathTestCase(
        input: #"C:\\foo"#,
        unix: .singleComponent(#"C:\\foo"#),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo"],
            printed: #"C:\foo"#, isAbsolute: true, driveLetter: "C")
    ),

    // Trailing dot dropped, becomes trailing sep
    PathTestCase(
        input: #"C:\foo\bar\."#,
        unix: .singleComponent(#"C:\foo\bar\."#),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo", "bar"], hasTrailingSeparator: true,
            printed: #"C:\foo\bar\"#, isAbsolute: true, driveLetter: "C")
    ),

    PathTestCase(
        input: #"C:\foo\bar\.."#,
        unix: .singleComponent(#"C:\foo\bar\.."#),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo", "bar", ".."],
            printed: #"C:\foo\bar\.."#, isAbsolute: true, driveLetter: "C",
            kinds: [.regular, .regular, .parentDirectory])
    ),

    PathTestCase(
        input: #"\\server\share\..\foo"#,
        unix: .singleComponent(#"\\server\share\..\foo"#),
        windows: Expected(
            anchor: #"\\server\share"#, components: ["..", "foo"],
            printed: #"\\server\share\..\foo"#, isAbsolute: true,
            kinds: [.parentDirectory, .regular])
    ),

    // Mixed separators: "/" is sep on Unix, both "/" and "\" are on Windows
    PathTestCase(
        input: #"C:/foo\bar"#,
        unix: Expected(
            anchor: nil, components: ["C:", #"foo\bar"#],
            printed: #"C:/foo\bar"#, isAbsolute: false),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo", "bar"],
            printed: #"C:\foo\bar"#, isAbsolute: true, driveLetter: "C")
    ),

    // MARK: - Verbatim dot/dotdot at end

    // In verbatim context, trailing dot is a regular component, NOT dropped
    PathTestCase(
        input: #"\\?\C:\foo\bar\."#,
        unix: .singleComponent(#"\\?\C:\foo\bar\."#),
        windows: Expected(
            anchor: #"\\?\C:\"#, components: ["foo", "bar", "."],
            printed: #"\\?\C:\foo\bar\."#, isAbsolute: true, driveLetter: "C",
            kinds: [.regular, .regular, .regular])
    ),

    PathTestCase(
        input: #"\\?\C:\foo\bar\.."#,
        unix: .singleComponent(#"\\?\C:\foo\bar\.."#),
        windows: Expected(
            anchor: #"\\?\C:\"#, components: ["foo", "bar", ".."],
            printed: #"\\?\C:\foo\bar\.."#, isAbsolute: true, driveLetter: "C",
            kinds: [.regular, .regular, .regular])
    ),

    PathTestCase(
        input: #"\\?\UNC\server\share\..\foo"#,
        unix: .singleComponent(#"\\?\UNC\server\share\..\foo"#),
        windows: Expected(
            anchor: #"\\?\UNC\server\share"#, components: ["..", "foo"],
            printed: #"\\?\UNC\server\share\..\foo"#, isAbsolute: true,
            kinds: [.regular, .regular])
    ),

    // MARK: - Resolve prefix edge cases

    // Prefix recognized with no components
    PathTestCase(
        input: "/.nofollow/",
        linux: Expected(
            anchor: "/", components: [".nofollow"], hasTrailingSeparator: true,
            printed: "/.nofollow/", isAbsolute: true),
        darwin: Expected(
            anchor: "/.nofollow/", components: [],
            printed: "/.nofollow/", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".nofollow"], hasTrailingSeparator: true,
            printed: #"\.nofollow\"#, isAbsolute: false, isRooted: true)
    ),

    // Prefix requires absolute path: not recognized on relative
    PathTestCase(
        input: "foo/.nofollow/bar",
        unix: Expected(
            anchor: nil, components: ["foo", ".nofollow", "bar"],
            printed: "foo/.nofollow/bar", isAbsolute: false),
        windows: Expected(
            anchor: nil, components: ["foo", ".nofollow", "bar"],
            printed: #"foo\.nofollow\bar"#, isAbsolute: false)
    ),

    // Near-miss: wrong case
    PathTestCase(
        input: "/.NOFOLLOW/foo",
        unix: Expected(
            anchor: "/", components: [".NOFOLLOW", "foo"],
            printed: "/.NOFOLLOW/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".NOFOLLOW", "foo"],
            printed: #"\.NOFOLLOW\foo"#, isAbsolute: false, isRooted: true)
    ),

    // Near-miss: extra char
    PathTestCase(
        input: "/.nofollowx/foo",
        unix: Expected(
            anchor: "/", components: [".nofollowx", "foo"],
            printed: "/.nofollowx/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".nofollowx", "foo"],
            printed: #"\.nofollowx\foo"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - .vol degenerate cases

    // Fewer than two components after .vol: not recognized as volfs
    PathTestCase(
        input: "/.vol",
        unix: Expected(
            anchor: "/", components: [".vol"],
            printed: "/.vol", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol"],
            printed: #"\.vol"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: "/.vol/",
        linux: Expected(
            anchor: "/", components: [".vol"], hasTrailingSeparator: true,
            printed: "/.vol/", isAbsolute: true),
        darwin: Expected(
            anchor: "/", components: [".vol"], hasTrailingSeparator: true,
            printed: "/.vol/", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol"], hasTrailingSeparator: true,
            printed: #"\.vol\"#, isAbsolute: false, isRooted: true)
    ),

    // Only one component after .vol: not enough
    PathTestCase(
        input: "/.vol/16777234",
        linux: Expected(
            anchor: "/", components: [".vol", "16777234"],
            printed: "/.vol/16777234", isAbsolute: true),
        darwin: Expected(
            anchor: "/", components: [".vol", "16777234"],
            printed: "/.vol/16777234", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "16777234"],
            printed: #"\.vol\16777234"#, isAbsolute: false, isRooted: true)
    ),

    // Wrong case: not volfs
    PathTestCase(
        input: "/.VOL/1234/5678",
        unix: Expected(
            anchor: "/", components: [".VOL", "1234", "5678"],
            printed: "/.VOL/1234/5678", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".VOL", "1234", "5678"],
            printed: #"\.VOL\1234\5678"#, isAbsolute: false, isRooted: true)
    ),

    // Extra char: not volfs
    PathTestCase(
        input: "/.volx/1234/5678",
        unix: Expected(
            anchor: "/", components: [".volx", "1234", "5678"],
            printed: "/.volx/1234/5678", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".volx", "1234", "5678"],
            printed: #"\.volx\1234\5678"#, isAbsolute: false, isRooted: true)
    ),

    // Non-numeric fsid: parser treats vol IDs opaquely
    PathTestCase(
        input: "/.vol/abc/12345",
        linux: Expected(
            anchor: "/", components: [".vol", "abc", "12345"],
            printed: "/.vol/abc/12345", isAbsolute: true),
        darwin: Expected(
            anchor: "/.vol/abc/12345", components: [],
            printed: "/.vol/abc/12345", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "abc", "12345"],
            printed: #"\.vol\abc\12345"#, isAbsolute: false, isRooted: true)
    ),

    // Dot after volfs anchor: dropped (volfs anchor is rooted)
    PathTestCase(
        input: "/.vol/1234/5678/./foo",
        linux: Expected(
            anchor: "/", components: [".vol", "1234", "5678", "foo"],
            printed: "/.vol/1234/5678/foo", isAbsolute: true),
        darwin: Expected(
            anchor: "/.vol/1234/5678", components: ["foo"],
            printed: "/.vol/1234/5678/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "1234", "5678", "foo"],
            printed: #"\.vol\1234\5678\foo"#, isAbsolute: false, isRooted: true)
    ),

    // Dotdot after volfs anchor: presented, not collapsed
    PathTestCase(
        input: "/.vol/1234/5678/foo/../bar",
        linux: Expected(
            anchor: "/", components: [".vol", "1234", "5678", "foo", "..", "bar"],
            printed: "/.vol/1234/5678/foo/../bar", isAbsolute: true,
            kinds: [.regular, .regular, .regular, .regular, .parentDirectory, .regular]),
        darwin: Expected(
            anchor: "/.vol/1234/5678", components: ["foo", "..", "bar"],
            printed: "/.vol/1234/5678/foo/../bar", isAbsolute: true,
            kinds: [.regular, .parentDirectory, .regular]),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "1234", "5678", "foo", "..", "bar"],
            printed: #"\.vol\1234\5678\foo\..\bar"#, isAbsolute: false, isRooted: true,
            kinds: [.regular, .regular, .regular, .regular, .parentDirectory, .regular])
    ),

    // MARK: - Resource fork near-misses

    PathTestCase(
        input: "/foo/..namedfork/data",
        unix: Expected(
            anchor: "/", components: ["foo", "..namedfork", "data"],
            printed: "/foo/..namedfork/data", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "..namedfork", "data"],
            printed: #"\foo\..namedfork\data"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: "/foo/..namedfork",
        unix: Expected(
            anchor: "/", components: ["foo", "..namedfork"],
            printed: "/foo/..namedfork", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "..namedfork"],
            printed: #"\foo\..namedfork"#, isAbsolute: false, isRooted: true)
    ),

    // Extra path after rsrc: suffix too long, not matched
    PathTestCase(
        input: "/foo/..namedfork/rsrc/extra",
        unix: Expected(
            anchor: "/", components: ["foo", "..namedfork", "rsrc", "extra"],
            printed: "/foo/..namedfork/rsrc/extra", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "..namedfork", "rsrc", "extra"],
            printed: #"\foo\..namedfork\rsrc\extra"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - Darwin double separators within anchor structures
    //
    // FilePath coalesces repeated separators and decomposes the coalesced
    // form; the only input it rejects (returns nil) is one containing NUL.
    // For .vol, the coalesced form yields the normal volfs anchor. The
    // .resolve and resource-fork cases further below remain flagged as known
    // issues pending a parser-vs-proposal decision (see README open questions).

    // Double slash after .vol coalesces to /.vol/1234/5678 (volfs anchor)
    PathTestCase(
        input: "/.vol//1234/5678",
        linux: Expected(
            anchor: "/", components: [".vol", "1234", "5678"],
            printed: "/.vol/1234/5678", isAbsolute: true),
        darwin: Expected(
            anchor: "/.vol/1234/5678", components: [],
            printed: "/.vol/1234/5678", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "1234", "5678"],
            printed: #"\.vol\1234\5678"#, isAbsolute: false, isRooted: true)
    ),

    // Double slash between vol IDs coalesces to /.vol/1234/5678 (volfs anchor)
    PathTestCase(
        input: "/.vol/1234//5678",
        linux: Expected(
            anchor: "/", components: [".vol", "1234", "5678"],
            printed: "/.vol/1234/5678", isAbsolute: true),
        darwin: Expected(
            anchor: "/.vol/1234/5678", components: [],
            printed: "/.vol/1234/5678", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "1234", "5678"],
            printed: #"\.vol\1234\5678"#, isAbsolute: false, isRooted: true)
    ),

    // The resource-fork match is an emergent property of separator
    // coalescing, consistent with the Darwin anchor cases: the whole
    // string is coalesced before suffix detection, so the kernel only
    // ever receives the coalesced form (/foo/..namedfork/rsrc) and there
    // is no separate kernel-behavior question to confirm. See README
    // "Design model: emergent semantics."
    PathTestCase(
        input: "/foo/..namedfork//rsrc",
        linux: Expected(
            anchor: "/", components: ["foo", "..namedfork", "rsrc"],
            printed: "/foo/..namedfork/rsrc", isAbsolute: true),
        darwin: Expected(
            anchor: "/", components: ["foo"], isResourceFork: true,
            printed: "/foo/..namedfork/rsrc", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "..namedfork", "rsrc"],
            printed: #"\foo\..namedfork\rsrc"#, isAbsolute: false, isRooted: true)
    ),

    // Double slash BEFORE resource fork suffix: suffix IS the last 17 bytes,
    // which are "/..namedfork/rsrc" regardless of earlier double slashes.
    PathTestCase(
        input: "/foo//..namedfork/rsrc",
        linux: Expected(
            anchor: "/", components: ["foo", "..namedfork", "rsrc"],
            printed: "/foo/..namedfork/rsrc", isAbsolute: true),
        darwin: Expected(
            anchor: "/", components: ["foo"], isResourceFork: true,
            printed: "/foo/..namedfork/rsrc", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "..namedfork", "rsrc"],
            printed: #"\foo\..namedfork\rsrc"#, isAbsolute: false, isRooted: true)
    ),

    // Double slash where the resolve flag number should be: coalesces to
    // /.resolve/1/foo, which then canonicalizes to /.nofollow/foo on Darwin.
    PathTestCase(
        input: "/.resolve//1/foo",
        linux: Expected(
            anchor: "/", components: [".resolve", "1", "foo"],
            printed: "/.resolve/1/foo", isAbsolute: true),
        darwin: Expected(
            anchor: "/.nofollow/", components: ["foo"],
            printed: "/.nofollow/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".resolve", "1", "foo"],
            printed: #"\.resolve\1\foo"#, isAbsolute: false, isRooted: true)
    ),

    // Double slash after nofollow prefix boundary: prefix matches (first 11
    // bytes are "/.nofollow/"), extra slash is a redundant separator in the
    // relative portion.
    PathTestCase(
        input: "/.nofollow//foo",
        linux: Expected(
            anchor: "/", components: [".nofollow", "foo"],
            printed: "/.nofollow/foo", isAbsolute: true),
        darwin: Expected(
            anchor: "/.nofollow/", components: ["foo"],
            printed: "/.nofollow/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".nofollow", "foo"],
            printed: #"\.nofollow\foo"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - .vol + resource fork

    PathTestCase(
        input: "/.vol/1234/5678/file/..namedfork/rsrc",
        linux: Expected(
            anchor: "/", components: [".vol", "1234", "5678", "file", "..namedfork", "rsrc"],
            printed: "/.vol/1234/5678/file/..namedfork/rsrc", isAbsolute: true),
        darwin: Expected(
            anchor: "/.vol/1234/5678", components: ["file"], isResourceFork: true,
            printed: "/.vol/1234/5678/file/..namedfork/rsrc", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".vol", "1234", "5678", "file", "..namedfork", "rsrc"],
            printed: #"\.vol\1234\5678\file\..namedfork\rsrc"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - Resolve prefix + resource fork

    PathTestCase(
        input: "/.nofollow/path/to/file/..namedfork/rsrc",
        linux: Expected(
            anchor: "/", components: [".nofollow", "path", "to", "file", "..namedfork", "rsrc"],
            printed: "/.nofollow/path/to/file/..namedfork/rsrc", isAbsolute: true),
        darwin: Expected(
            anchor: "/.nofollow/", components: ["path", "to", "file"], isResourceFork: true,
            printed: "/.nofollow/path/to/file/..namedfork/rsrc", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [".nofollow", "path", "to", "file", "..namedfork", "rsrc"],
            printed: #"\.nofollow\path\to\file\..namedfork\rsrc"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - Trailing sep on dot/dotdot after root

    PathTestCase(
        input: "/./",
        unix: Expected(
            anchor: "/", components: [],
            printed: "/", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: [],
            printed: #"\"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: "/../",
        unix: Expected(
            anchor: "/", components: [".."], hasTrailingSeparator: true,
            printed: "/../", isAbsolute: true, kinds: [.parentDirectory]),
        windows: Expected(
            anchor: #"\"#, components: [".."], hasTrailingSeparator: true,
            printed: #"\..\"#, isAbsolute: false, isRooted: true, kinds: [.parentDirectory])
    ),

    PathTestCase(
        input: "/foo/bar/../../baz",
        unix: Expected(
            anchor: "/", components: ["foo", "bar", "..", "..", "baz"],
            printed: "/foo/bar/../../baz", isAbsolute: true,
            kinds: [.regular, .regular, .parentDirectory, .parentDirectory, .regular]),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "bar", "..", "..", "baz"],
            printed: #"\foo\bar\..\..\baz"#, isAbsolute: false, isRooted: true,
            kinds: [.regular, .regular, .parentDirectory, .parentDirectory, .regular])
    ),

    // MARK: - Deeper drive relative

    PathTestCase(
        input: #"C:foo\bar\baz"#,
        unix: .singleComponent(#"C:foo\bar\baz"#),
        windows: Expected(
            anchor: "C:", components: ["foo", "bar", "baz"],
            printed: #"C:foo\bar\baz"#, isAbsolute: false, driveLetter: "C")
    ),

    // MARK: - Windows device UNC

    PathTestCase(
        input: #"\\.\UNC\srv\share\foo"#,
        unix: .singleComponent(#"\\.\UNC\srv\share\foo"#),
        windows: Expected(
            anchor: #"\\.\UNC"#, components: ["srv", "share", "foo"],
            printed: #"\\.\UNC\srv\share\foo"#, isAbsolute: true)
    ),

    // \\.\UNC alone: device name only, no components
    PathTestCase(
        input: #"\\.\UNC"#,
        unix: .singleComponent(#"\\.\UNC"#),
        windows: Expected(
            anchor: #"\\.\UNC"#, components: [],
            printed: #"\\.\UNC"#, isAbsolute: true)
    ),

    // \\.\UNC\ trailing sep after device name
    PathTestCase(
        input: #"\\.\UNC\"#,
        unix: .singleComponent(#"\\.\UNC\"#),
        windows: Expected(
            anchor: #"\\.\UNC"#, components: [], hasTrailingSeparator: true,
            printed: #"\\.\UNC\"#, isAbsolute: true)
    ),

    // \\.\UNC\server: server is a component, NOT absorbed into anchor
    PathTestCase(
        input: #"\\.\UNC\server"#,
        unix: .singleComponent(#"\\.\UNC\server"#),
        windows: Expected(
            anchor: #"\\.\UNC"#, components: ["server"],
            printed: #"\\.\UNC\server"#, isAbsolute: true)
    ),

    // \\.\UNC\server\share: both are components (contrast with \\?\UNC\)
    PathTestCase(
        input: #"\\.\UNC\server\share"#,
        unix: .singleComponent(#"\\.\UNC\server\share"#),
        windows: Expected(
            anchor: #"\\.\UNC"#, components: ["server", "share"],
            printed: #"\\.\UNC\server\share"#, isAbsolute: true)
    ),

    // \\.\UNC\server\share\ trailing sep
    PathTestCase(
        input: #"\\.\UNC\server\share\"#,
        unix: .singleComponent(#"\\.\UNC\server\share\"#),
        windows: Expected(
            anchor: #"\\.\UNC"#, components: ["server", "share"],
            hasTrailingSeparator: true,
            printed: #"\\.\UNC\server\share\"#, isAbsolute: true)
    ),

    // MARK: - Verbatim non-drive non-UNC

    // Unknown device name is part of anchor
    PathTestCase(
        input: #"\\?\foo\bar"#,
        unix: .singleComponent(#"\\?\foo\bar"#),
        windows: Expected(
            anchor: #"\\?\foo"#, components: ["bar"],
            printed: #"\\?\foo\bar"#, isAbsolute: true)
    ),

    // GLOBALROOT (NT-namespace escape): name terminates at first backslash
    PathTestCase(
        input: #"\\?\GLOBALROOT\Device\HarddiskVolume1\foo"#,
        unix: .singleComponent(#"\\?\GLOBALROOT\Device\HarddiskVolume1\foo"#),
        windows: Expected(
            anchor: #"\\?\GLOBALROOT"#,
            components: ["Device", "HarddiskVolume1", "foo"],
            printed: #"\\?\GLOBALROOT\Device\HarddiskVolume1\foo"#,
            isAbsolute: true)
    ),

    // Volume GUID (volumes without a drive letter)
    PathTestCase(
        input: #"\\?\Volume{12345678-1234-1234-1234-123456789012}\foo\bar"#,
        unix: .singleComponent(
            #"\\?\Volume{12345678-1234-1234-1234-123456789012}\foo\bar"#),
        windows: Expected(
            anchor: #"\\?\Volume{12345678-1234-1234-1234-123456789012}"#,
            components: ["foo", "bar"],
            printed: #"\\?\Volume{12345678-1234-1234-1234-123456789012}\foo\bar"#,
            isAbsolute: true)
    ),

    // DosDevices symlink (e.g. PIPE)
    PathTestCase(
        input: #"\\?\PIPE\mypipe"#,
        unix: .singleComponent(#"\\?\PIPE\mypipe"#),
        windows: Expected(
            anchor: #"\\?\PIPE"#, components: ["mypipe"],
            printed: #"\\?\PIPE\mypipe"#, isAbsolute: true)
    ),

    // .. NOT special in verbatim plain: regular component
    PathTestCase(
        input: #"\\?\GLOBALROOT\..\foo"#,
        unix: .singleComponent(#"\\?\GLOBALROOT\..\foo"#),
        windows: Expected(
            anchor: #"\\?\GLOBALROOT"#, components: ["..", "foo"],
            printed: #"\\?\GLOBALROOT\..\foo"#, isAbsolute: true,
            kinds: [.regular, .regular])
    ),

    // . NOT special in verbatim plain: regular component (not dropped)
    PathTestCase(
        input: #"\\?\GLOBALROOT\.\foo"#,
        unix: .singleComponent(#"\\?\GLOBALROOT\.\foo"#),
        windows: Expected(
            anchor: #"\\?\GLOBALROOT"#, components: [".", "foo"],
            printed: #"\\?\GLOBALROOT\.\foo"#, isAbsolute: true,
            kinds: [.regular, .regular])
    ),

    // / inside verbatim plain component is a regular byte, not a separator
    PathTestCase(
        input: #"\\?\GLOBALROOT\foo/bar"#,
        unix: Expected(
            anchor: nil, components: [#"\\?\GLOBALROOT\foo"#, "bar"],
            printed: #"\\?\GLOBALROOT\foo/bar"#, isAbsolute: false),
        windows: Expected(
            anchor: #"\\?\GLOBALROOT"#, components: ["foo/bar"],
            printed: #"\\?\GLOBALROOT\foo/bar"#, isAbsolute: true)
    ),

    // MARK: - Device path forward slashes for pipes

    // Dot dropped (rooted) on Unix, device-namespace pipe on Windows
    PathTestCase(
        input: "//./pipe/foo",
        unix: Expected(
            anchor: "/", components: ["pipe", "foo"],
            printed: "/pipe/foo", isAbsolute: true),
        windows: Expected(
            anchor: #"\\.\pipe"#, components: ["foo"],
            printed: #"\\.\pipe\foo"#, isAbsolute: true)
    ),

    // MARK: - Dotfile components (not special, always regular)

    PathTestCase(
        input: "/foo/.hidden",
        unix: Expected(
            anchor: "/", components: ["foo", ".hidden"],
            printed: "/foo/.hidden", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", ".hidden"],
            printed: #"\foo\.hidden"#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - Double extension and trailing dot

    PathTestCase(
        input: "/foo/bar.tar.gz",
        unix: Expected(
            anchor: "/", components: ["foo", "bar.tar.gz"],
            printed: "/foo/bar.tar.gz", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "bar.tar.gz"],
            printed: #"\foo\bar.tar.gz"#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: "/foo/bar.",
        unix: Expected(
            anchor: "/", components: ["foo", "bar."],
            printed: "/foo/bar.", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "bar."],
            printed: #"\foo\bar."#, isAbsolute: false, isRooted: true)
    ),

    PathTestCase(
        input: "/foo/...",
        unix: Expected(
            anchor: "/", components: ["foo", "..."],
            printed: "/foo/...", isAbsolute: true),
        windows: Expected(
            anchor: #"\"#, components: ["foo", "..."],
            printed: #"\foo\..."#, isAbsolute: false, isRooted: true)
    ),

    // MARK: - Windows IP address as UNC server

    PathTestCase(
        input: #"\\192.168.1.1\share\foo"#,
        unix: .singleComponent(#"\\192.168.1.1\share\foo"#),
        windows: Expected(
            anchor: #"\\192.168.1.1\share"#, components: ["foo"],
            printed: #"\\192.168.1.1\share\foo"#, isAbsolute: true)
    ),

    // MARK: - Windows drive dot and dotdot

    PathTestCase(
        input: "C:.",
        unix: Expected(
            anchor: nil, components: ["C:."],
            printed: "C:.", isAbsolute: false),
        windows: Expected(
            anchor: "C:", components: ["."],
            printed: "C:.", isAbsolute: false, driveLetter: "C",
            kinds: [.currentDirectory])
    ),

    PathTestCase(
        input: "C:..",
        unix: Expected(
            anchor: nil, components: ["C:.."],
            printed: "C:..", isAbsolute: false),
        windows: Expected(
            anchor: "C:", components: [".."],
            printed: "C:..", isAbsolute: false, driveLetter: "C",
            kinds: [.parentDirectory])
    ),

    // MARK: - Windows non-letter drive

    // A drive letter is any single non-separator code unit followed by a
    // colon (matching RtlDetermineDosPathNameType_U, which keys on the
    // second character being a colon and does not validate the first).
    // A digit is a valid drive letter: "1:\foo" is drive-absolute.
    PathTestCase(
        input: #"1:\foo"#,
        unix: .singleComponent(#"1:\foo"#),
        windows: Expected(
            anchor: #"1:\"#, components: ["foo"],
            printed: #"1:\foo"#, isAbsolute: true, driveLetter: "1")
    ),

    // A colon is itself a non-separator, so "::foo" parses as drive ":"
    // (the documented basis for the "=::" environment variable Windows
    // creates). Like "C:foo", it is drive-relative: not rooted, not
    // absolute.
    PathTestCase(
        input: "::foo",
        unix: Expected(
            anchor: nil, components: ["::foo"],
            printed: "::foo", isAbsolute: false),
        windows: Expected(
            anchor: "::", components: ["foo"],
            printed: "::foo", isAbsolute: false, driveLetter: ":")
    ),

    // The non-separator rule extends to device-namespace drives:
    // "\\.\1:" exposes drive "1" just as "\\.\C:" exposes "C".
    PathTestCase(
        input: #"\\.\1:"#,
        unix: .singleComponent(#"\\.\1:"#),
        windows: Expected(
            anchor: #"\\.\1:"#, components: [],
            printed: #"\\.\1:"#, isAbsolute: true, driveLetter: "1")
    ),

    // ...and to verbatim drives: "\\?\1:\" exposes drive "1".
    PathTestCase(
        input: #"\\?\1:\"#,
        unix: .singleComponent(#"\\?\1:\"#),
        windows: Expected(
            anchor: #"\\?\1:\"#, components: [],
            printed: #"\\?\1:\"#, isAbsolute: true, driveLetter: "1")
    ),

    // Verbatim paths take bytes as written: separators are not normalized,
    // so "/" is a legal (non-separator) drive letter and "\\?\/:" has
    // drive "/".
    PathTestCase(
        input: #"\\?\/:"#,
        unix: Expected(
            anchor: nil, components: [#"\\?\"#, ":"],
            printed: #"\\?\/:"#, isAbsolute: false),
        windows: Expected(
            anchor: #"\\?\/:"#, components: [],
            printed: #"\\?\/:"#, isAbsolute: true, driveLetter: "/")
    ),

    // MARK: - Trailing sep on deeper paths

    PathTestCase(
        input: "foo/bar/",
        unix: Expected(
            anchor: nil, components: ["foo", "bar"], hasTrailingSeparator: true,
            printed: "foo/bar/", isAbsolute: false),
        windows: Expected(
            anchor: nil, components: ["foo", "bar"], hasTrailingSeparator: true,
            printed: #"foo\bar\"#, isAbsolute: false)
    ),

    // MARK: - Windows dotdot/dot + trailing sep

    PathTestCase(
        input: #"C:\foo\bar\..\"#,
        unix: .singleComponent(#"C:\foo\bar\..\"#),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo", "bar", ".."], hasTrailingSeparator: true,
            printed: #"C:\foo\bar\..\"#, isAbsolute: true, driveLetter: "C",
            kinds: [.regular, .regular, .parentDirectory])
    ),

    // Trailing dot dropped + trailing sep coalesced
    PathTestCase(
        input: #"C:\foo\bar\.\"#,
        unix: .singleComponent(#"C:\foo\bar\.\"#),
        windows: Expected(
            anchor: #"C:\"#, components: ["foo", "bar"], hasTrailingSeparator: true,
            printed: #"C:\foo\bar\"#, isAbsolute: true, driveLetter: "C")
    ),

    // MARK: - Degenerate Windows UNC / device paths
    //
    // These start with `\\` but are incomplete or have doubled separators.
    // Each is well-defined: the parser pads incomplete UNC forms with a
    // synthesized backslash and coalesces interior doubled separators.

    PathTestCase(
        input: #"\\server"#,
        // Incomplete UNC: server with no share. Padded to `\\server\`.
        unix: .singleComponent(#"\\server"#),
        windows: Expected(
            anchor: #"\\server\"#, components: [],
            printed: #"\\server\"#, isAbsolute: true)
    ),

    PathTestCase(
        input: #"\\server\"#,
        // Server with empty share — same final form as `\\server`.
        unix: .singleComponent(#"\\server\"#),
        windows: Expected(
            anchor: #"\\server\"#, components: [],
            printed: #"\\server\"#, isAbsolute: true)
    ),

    PathTestCase(
        input: #"\\"#,
        // Bare double backslash: padded to a degraded `\\\` root.
        unix: .singleComponent(#"\\"#),
        windows: Expected(
            anchor: #"\\\"#, components: [],
            printed: #"\\\"#, isAbsolute: true)
    ),

    PathTestCase(
        input: #"\\server\share\\foo"#,
        // Doubled separator between share and first component coalesces.
        unix: .singleComponent(#"\\server\share\\foo"#),
        windows: Expected(
            anchor: #"\\server\share"#, components: ["foo"],
            printed: #"\\server\share\foo"#, isAbsolute: true)
    ),

    PathTestCase(
        input: #"\\srv\\share\foo"#,
        // Doubled separator between server and share coalesces.
        unix: .singleComponent(#"\\srv\\share\foo"#),
        windows: Expected(
            anchor: #"\\srv\share"#, components: ["foo"],
            printed: #"\\srv\share\foo"#, isAbsolute: true)
    ),
]
