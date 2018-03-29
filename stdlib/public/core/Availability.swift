//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

/// Returns 1 if the running OS version is greater than or equal to
/// major.minor.patchVersion and 0 otherwise.
///
/// This is a magic entry point known to the compiler. It is called in
/// generated code for API availability checking.
@inlinable // FIXME(sil-serialize-all)
@_semantics("availability.osversion")
public func _stdlib_isOSVersionAtLeast(
  _ major: Builtin.Word,
  _ minor: Builtin.Word,
  _ patch: Builtin.Word
) -> Builtin.Int1 {
#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
  let runningVersion = _swift_stdlib_operatingSystemVersion()
  let queryVersion = _SwiftNSOperatingSystemVersion(
    majorVersion: Int(major),
    minorVersion: Int(minor),
    patchVersion: Int(patch)
  )

  let result = runningVersion >= queryVersion
  
  return result._value
#else
  // FIXME: As yet, there is no obvious versioning standard for platforms other
  // than Darwin-based OSes, so we just assume false for now. 
  // rdar://problem/18881232
  return false._value
#endif
}

extension _SwiftNSOperatingSystemVersion : Comparable {

  @inlinable // FIXME(sil-serialize-all)
  public static func == (
    lhs: _SwiftNSOperatingSystemVersion,
    rhs: _SwiftNSOperatingSystemVersion
  ) -> Bool {
    return lhs.majorVersion == rhs.majorVersion &&
           lhs.minorVersion == rhs.minorVersion &&
           lhs.patchVersion == rhs.patchVersion
  }

  /// Lexicographic comparison of version components.
  @inlinable // FIXME(sil-serialize-all)
  public static func < (
    lhs: _SwiftNSOperatingSystemVersion,
    rhs: _SwiftNSOperatingSystemVersion
  ) -> Bool {
    guard lhs.majorVersion == rhs.majorVersion else {
      return lhs.majorVersion < rhs.majorVersion
    }

    guard lhs.minorVersion == rhs.minorVersion else {
      return lhs.minorVersion < rhs.minorVersion
    }

    return lhs.patchVersion < rhs.patchVersion
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func >= (
    lhs: _SwiftNSOperatingSystemVersion,
    rhs: _SwiftNSOperatingSystemVersion
  ) -> Bool {
    guard lhs.majorVersion == rhs.majorVersion else {
      return lhs.majorVersion >= rhs.majorVersion
    }

    guard lhs.minorVersion == rhs.minorVersion else {
      return lhs.minorVersion >= rhs.minorVersion
    }

    return lhs.patchVersion >= rhs.patchVersion
  }
}
