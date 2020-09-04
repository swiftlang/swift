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
@_effects(readnone)
public func _stdlib_isOSVersionAtLeast(
  _ major: Builtin.Word,
  _ minor: Builtin.Word,
  _ patch: Builtin.Word
) -> Builtin.Int1 {
#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
  if Int(major) == 9999 {
    return true._value
  }
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

#if os(macOS)
// This is a magic entry point known to the compiler. It is called in
// generated code for API availability checking.
@_semantics("availability.osversion")
@_effects(readnone)
public func _stdlib_isOSVersionAtLeastOrVariantVersionAtLeast(
  _ major: Builtin.Word,
  _ minor: Builtin.Word,
  _ patch: Builtin.Word,
  _ variantMajor: Builtin.Word,
  _ variantMinor: Builtin.Word,
  _ variantPatch: Builtin.Word
  ) -> Builtin.Int1 {
  return _stdlib_isOSVersionAtLeast(major, minor, patch)
}
#endif
