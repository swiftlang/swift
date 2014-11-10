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
/// This is a magic entrypoint known to the compiler. It is called in
/// generated code for API availability checking.
public func _stdlib_isOSVersionAtLeast(
  major: Builtin.Word,
  minor: Builtin.Word,
  patch: Builtin.Word
) -> Builtin.Int1 {
#if os(OSX) || os(iOS)
  let version = _swift_stdlib_operatingSystemVersion()

  let result =
    (version.majorVersion >= Int(major)) &&
    (version.minorVersion >= Int(minor)) &&
    (version.patchVersion >= Int(patch))
  
  return result.value
#else
  // FIXME: As yet, there is no obvious versioning standard for platforms other
  // than Darwin-based OS', so we just assume false for now. 
  // rdar://problem/18881232
  return false.value
#endif
}
