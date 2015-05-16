//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

/// Returns 1 if the running OS version is greater than or equal to
/// major.minor.patchVersion and 0 otherwise.
///
/// This is a magic entrypoint known to the compiler. It is called in
/// generated code for API availability checking.
@_semantics("availability.osversion")
public func _stdlib_isOSVersionAtLeast(
  major: Builtin.Word,
  _ minor: Builtin.Word,
  _ patch: Builtin.Word
) -> Builtin.Int1 {
#if os(OSX) || os(iOS) || os(tvOS)
  let runningVersion = _swift_stdlib_operatingSystemVersion()
  let queryVersion = _SwiftNSOperatingSystemVersion(
    majorVersion: Int(major),
    minorVersion: Int(minor),
    patchVersion: Int(patch)
  )

  let result = runningVersion >= queryVersion
  
  return result.value
#else
  // FIXME: As yet, there is no obvious versioning standard for platforms other
  // than Darwin-based OS', so we just assume false for now. 
  // rdar://problem/18881232
  return false.value
#endif
}

extension _SwiftNSOperatingSystemVersion : Comparable { }

public func ==(
  left: _SwiftNSOperatingSystemVersion,
  right: _SwiftNSOperatingSystemVersion
) -> Bool {
  return left.majorVersion == right.majorVersion &&
         left.minorVersion == right.minorVersion &&
         left.patchVersion == right.patchVersion
}

/// Lexicographic comparison of version components.
public func <(
  left: _SwiftNSOperatingSystemVersion,
  right: _SwiftNSOperatingSystemVersion
) -> Bool {
  if left.majorVersion > right.majorVersion {
    return false
  }

  if left.majorVersion < right.majorVersion {
    return true
  }

  if left.minorVersion > right.minorVersion {
    return false
  }

  if left.minorVersion < right.minorVersion {
    return true
  }

  if left.patchVersion > right.patchVersion {
    return false
  }

  if left.patchVersion < right.patchVersion {
    return true
  }

  return false
}

public func >=(
  left: _SwiftNSOperatingSystemVersion,
  right: _SwiftNSOperatingSystemVersion
) -> Bool {
  if left.majorVersion < right.majorVersion {
    return false
  }

  if left.majorVersion > right.majorVersion {
    return true
  }

  if left.minorVersion < right.minorVersion {
    return false
  }

  if left.minorVersion > right.minorVersion {
    return true
  }

  if left.patchVersion < right.patchVersion {
    return false
  }

  if left.patchVersion > right.patchVersion {
    return true
  }

  return true
}
