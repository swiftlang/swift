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
@_semantics("availability.osversion")
@inlinable
public func _stdlib_isOSVersionAtLeast(
  _ major: Builtin.Word,
  _ minor: Builtin.Word,
  _ patch: Builtin.Word
) -> Builtin.Int1 {
#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
  if Int(major) == 9999 {
    return true._value
  }
  // The call to _swift_stdlib_operatingSystemVersion is used as an indicator
  // that this function was called by a compiler optimization pass. If it is
  // replaced that pass needs to be updated.
  let runningVersion = _swift_stdlib_operatingSystemVersion()
  
  let result =
    (runningVersion.majorVersion,runningVersion.minorVersion,runningVersion.patchVersion)
    >= (Int(major),Int(minor),Int(patch))

  return result._value
#else
  // FIXME: As yet, there is no obvious versioning standard for platforms other
  // than Darwin-based OSes, so we just assume false for now. 
  // rdar://problem/18881232
  return false._value
#endif
}


/// Returns the version number for the Swift Standard Library that the code
/// calling this was originally compiled with.
///
/// The version number is an arbitrary `Double` value that is monotonically
/// increasing between toolchain releases. It does not correlate with toolchain
/// or OS version numbers, and it is not a semantic version number.
@_alwaysEmitIntoClient @_transparent
public var _stdlibStaticVersion: Double {
  return 1001.0
}

/// Returns the version number for the Swift Standard Library that is currently
/// loaded.
///
/// The version number is an arbitrary `Double` value that is monotonically
/// increasing between toolchain releases. It does not correlate with toolchain
/// or OS version numbers, and it is not a semantic version number.
@_alwaysEmitIntoClient // Introduced in 5.1
public var _stdlibDynamicVersion: Double {
  if #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) {
    return _stdlibOpaqueVersion
  }
  else {
    // When linked with the 5.0 stdlib, we return this default value.
    return 1000.0
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
@usableFromInline
internal var _stdlibOpaqueVersion: Double {
  return _stdlibStaticVersion
}

