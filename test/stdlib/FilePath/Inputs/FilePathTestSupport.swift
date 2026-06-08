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

enum REVIEW_ONLY_Platform: Sendable { case linux, darwin, windows }

let _builtPlatform: REVIEW_ONLY_Platform = {
  #if os(Windows)
  .windows
  #elseif canImport(Darwin)
  .darwin
  #else
  .linux
  #endif
}()

let allReviewPlatforms: [REVIEW_ONLY_Platform] = [_builtPlatform]

func withPlatform(_ p: REVIEW_ONLY_Platform, _ body: () throws -> Void) rethrows {
  guard p == _builtPlatform else { return }
  try body()
}

func forEachPlatform(_ body: (REVIEW_ONLY_Platform) throws -> Void) rethrows {
  try body(_builtPlatform)
}

private var universalRootDescription: String {
  _builtPlatform == .windows ? "\\" : "/"
}

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
  guard _builtPlatform == .windows else { return canonicalSlashForm }
  let swapped = canonicalSlashForm.utf8.map {
    $0 == UInt8(ascii: "/") ? UInt8(ascii: "\\") : $0
  }
  return String(decoding: swapped, as: UTF8.self)
}

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
