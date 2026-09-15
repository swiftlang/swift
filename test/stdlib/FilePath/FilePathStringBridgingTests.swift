//===--- FilePathStringBridgingTests.swift --------------------------------===//
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

// String bridging, including ill-formed Unicode. `String(decoding:)` and
// `String(validating:)` on FilePath / Anchor / Component were essentially
// untested, and the U+FFFD path is the most likely Windows-side regression
// at port time. Expectations from SE-0529 ("Paths and strings", 730-777,
// and the `description` docs at 622-623 / 648-649 / 675-676):
//   * `String(decoding:)` — UTF-8 (Linux/Darwin) or UTF-16 (Windows) decode,
//     replacing ill-formed sequences with U+FFFD. Never fails.
//   * `String(validating:)` — `nil` when the content is not well-formed.
//   * `description` equals `String(decoding:)` for the same value.
//
// ENCODING: `FilePath.CodeUnit` and the decode encoding are fixed at compile
// time — `CChar`/UTF-8 off Windows, `UInt16`/UTF-16 on Windows.

@available(SwiftStdlib 9999, *)
private func filePath(fromCodeUnits units: [FilePath.CodeUnit]) -> FilePath? {
  FilePath(codeUnits: units.span)
}

@available(SwiftStdlib 9999, *)
private func component(
  fromCodeUnits units: [FilePath.CodeUnit]
) -> FilePath.Component? {
  FilePath.Component(codeUnits: units.span)
}

@main
struct FilePathStringBridgingTests {
  static func main() {
    let suite = TestSuite("FilePath.StringBridging")

    // MARK: - Well-formed round-trips

    suite.test("wellFormedRoundTripFilePath")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Inputs use `/`-form paths whose stored bytes (and decoded String)
      // differ on Windows; restrict to unix.
      withPlatforms(.linux, .darwin) {
        for s in ["/foo/bar", "foo/bar", "/usr/local/bin", "/café/naïve", "a/b/c"] {
          let p = FilePath(s)!
          expectEqual(String(decoding: p), s,
            "decoding recovers \(s.debugDescription)")
          expectTrue(String(validating: p) == s,
            "validating recovers \(s.debugDescription)")
        }
      }
    }

    suite.test("wellFormedRoundTripAnchor")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      withPlatforms(.linux, .darwin) {
        let a = FilePath.Anchor("/")
        expectEqual(String(decoding: a), "/", "decoding anchor /")
        expectTrue(String(validating: a) == "/", "validating anchor /")
      }
      withPlatform(.windows) {
        let a = FilePath.Anchor(#"C:\"#)
        expectEqual(String(decoding: a), #"C:\"#, "decoding anchor C:\\")
        expectTrue(String(validating: a) == #"C:\"#, "validating anchor C:\\")
      }
      withPlatform(.darwin) {
        let a = FilePath.Anchor("/.nofollow/")
        expectEqual(String(decoding: a), "/.nofollow/", "decoding /.nofollow/")
        expectTrue(String(validating: a) == "/.nofollow/",
          "validating /.nofollow/")
      }
    }

    suite.test("wellFormedRoundTripComponent")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      // Component names have no separator; round-trips are universal.
      for name in ["foo", "file.txt", "café", ".."] {
        let c = FilePath.Component(name)!
        expectEqual(String(decoding: c), name,
          "decoding component \(name.debugDescription)")
        expectTrue(String(validating: c) == name,
          "validating component \(name.debugDescription)")
      }
    }

    // MARK: - description == String(decoding:)

    suite.test("descriptionEqualsDecoding")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let p = FilePath("/foo/bar")
      expectEqual(p.description, String(decoding: p), "FilePath description")

      let a = FilePath.Anchor("/")
      expectEqual(a.description, String(decoding: a), "Anchor description")

      let c = FilePath.Component("foo")
      expectEqual(c.description, String(decoding: c), "Component description")
    }

#if !os(Windows)
    // MARK: - Ill-formed Unicode (UTF-8 / CChar build only)
    //
    // Lone 0x80 is a UTF-8 continuation byte with no leader; 0xFF never
    // appears in valid UTF-8. We append them to "foo" and drive
    // construction through the public codeUnits init.

    @available(SwiftStdlib 9999, *)
    func illFormedUTF8Bytes() -> [FilePath.CodeUnit] {
      let prefix = "foo".utf8.map { FilePath.CodeUnit(bitPattern: $0) }
      let bad: [FilePath.CodeUnit] = [
        FilePath.CodeUnit(bitPattern: 0x80),
        FilePath.CodeUnit(bitPattern: 0xFF),
      ]
      return prefix + bad
    }

    suite.test("illFormedFilePath")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let p = filePath(fromCodeUnits: illFormedUTF8Bytes())!
      expectNil(String(validating: p),
        "String(validating:) is nil for ill-formed FilePath")
      expectTrue(String(decoding: p).unicodeScalars.contains("\u{FFFD}"),
        "String(decoding:) yields U+FFFD for ill-formed FilePath")
      expectEqual(p.description, String(decoding: p),
        "description == decoding (ill-formed)")
    }

    suite.test("illFormedComponent")
    .skip(.custom({ if #available(SwiftStdlib 9999, *) { false } else { true } },
                  reason: "Requires SwiftStdlib 9999"))
    .code { guard #available(SwiftStdlib 9999, *) else { return }
      let c = component(fromCodeUnits: illFormedUTF8Bytes())!

      expectTrue(String(decoding: c).unicodeScalars.contains("\u{FFFD}"),
        "String(decoding:) yields U+FFFD for ill-formed Component")

      // SE-0529 (lines 771-776): String?(validating: component) returns nil
      // when the content is not well-formed Unicode. Fixed in
      // StringBridging.swift to use the decode/re-encode/compare round-trip.
      expectNil(String(validating: c),
        "String(validating:) should be nil for ill-formed Component")
    }
#endif

    runAllTests()
  }
}
