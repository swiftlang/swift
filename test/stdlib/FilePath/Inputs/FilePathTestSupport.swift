//===--- FilePathTestSupport.swift ----------------------------------------===//
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

private var universalRootDescription: String {
#if os(Windows)
  "\\"
#else
  "/"
#endif
}

/// Translate a path string written with `/` as the canonical separator into
/// the built platform's spelling, so that platform-independent tests can be
/// written once and run unchanged everywhere. On a Windows build,
/// `universal("/usr/local/bin")` returns `\usr\local\bin`; elsewhere it
/// returns the input unchanged.
///
/// Only valid for paths that are universal modulo the separator byte:
/// relative paths and plain-root paths. Traps on a backslash literal or a
/// platform-specific anchor (Windows drive `C:`, UNC `\\server\share`,
/// verbatim `\\?\...`, Darwin magic anchors `/.vol/...` etc.); use an exact
/// string in a platform-specific test for those.
@available(SwiftStdlib 9999, *)
func universal(_ canonicalSlashForm: String) -> String {
  precondition(
    !canonicalSlashForm.contains("\\"),
    "universal(): literal contains a backslash; write it with '/' or use an "
    + "exact string in a platform-specific test: \(canonicalSlashForm)")
  let parsed = FilePath(canonicalSlashForm)
  if let anchor = parsed?.anchor {
    precondition(
      anchor.description == universalRootDescription,
      "universal(): literal has a platform-specific anchor "
      + "(\(anchor.description)); use an exact string in a platform-specific "
      + "test: \(canonicalSlashForm)")
  }
#if os(Windows)
  let swapped = canonicalSlashForm.utf8.map {
    $0 == UInt8(ascii: "/") ? UInt8(ascii: "\\") : $0
  }
  return String(decoding: swapped, as: UTF8.self)
#else
  return canonicalSlashForm
#endif
}

// `driveLetter` and `isVerbatimComponent` on `FilePath.Anchor` are
// gated under `#if os(Windows)` in the FilePath sources. Test bodies
// referring to those properties must still type-check on non-Windows
// builds (where the body is `#if`-gated to never run); these shims
// give them a benign default everywhere.
@available(SwiftStdlib 9999, *)
extension FilePath.Anchor {
  var _driveLetter: Unicode.Scalar? {
    #if os(Windows)
    return self.driveLetter
    #else
    return nil
    #endif
  }
  var _isVerbatimComponent: Bool {
    #if os(Windows)
    return self.isVerbatimComponent
    #else
    return false
    #endif
  }
}
