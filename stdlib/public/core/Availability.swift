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
///
/// This is marked @_transparent on iOS to work around broken availability
/// checking for iOS apps running on macOS (rdar://83378814). libswiftCore uses
/// the macOS platform identifier for its version check in that scenario,
/// causing all queries to return true. When this function is inlined into the
/// caller, the compiler embeds the correct platform identifier in the client
/// code, and we get the right answer.
///
/// @_transparent breaks the optimizer's ability to remove availability checks
/// that are unnecessary due to the current deployment target. We call through
/// to the _stdlib_isOSVersionAtLeast_AEIC function below to work around this,
/// as the optimizer is able to perform this optimization for a
/// @_alwaysEmitIntoClient function. We can't use @_alwaysEmitIntoClient
/// directly on this call because it would break ABI for existing apps.
///
/// `@_transparent` breaks the interpreter mode on macOS, as it creates a direct
/// reference to ___isPlatformVersionAtLeast from compiler-rt, and the
/// interpreter doesn't currently know how to load symbols from compiler-rt.
/// Since `@_transparent` is only necessary for iOS apps, we only apply it on
/// iOS, not any other which would inherit/remap iOS availability.
#if os(iOS) && !os(visionOS)
@_effects(readnone)
@_transparent
@_noLocks
public func _stdlib_isOSVersionAtLeast(
  _ major: Builtin.Word,
  _ minor: Builtin.Word,
  _ patch: Builtin.Word
) -> Builtin.Int1 {
  return _stdlib_isOSVersionAtLeast_AEIC(major, minor, patch)
}
#else
@_semantics("availability.osversion")
@_effects(readnone)
@_unavailableInEmbedded
#if hasFeature(Macros)
@_noLocks
#endif
public func _stdlib_isOSVersionAtLeast(
  _ major: Builtin.Word,
  _ minor: Builtin.Word,
  _ patch: Builtin.Word
) -> Builtin.Int1 {
  return _stdlib_isOSVersionAtLeast_AEIC(major, minor, patch)
}
#endif

@_semantics("availability.osversion")
@_effects(readnone)
@_alwaysEmitIntoClient
#if hasFeature(Macros)
@_noLocks
#endif
public func _stdlib_isOSVersionAtLeast_AEIC(
  _ major: Builtin.Word,
  _ minor: Builtin.Word,
  _ patch: Builtin.Word
) -> Builtin.Int1 {
#if (os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(visionOS)) && SWIFT_RUNTIME_OS_VERSIONING
  if Int(major) == 9999 {
    return true._value
  }

  let queryVersion = (Int(major), Int(minor), Int(patch))
  let major32 = Int32(truncatingIfNeeded:Int(queryVersion.0))
  let minor32 = Int32(truncatingIfNeeded:Int(queryVersion.1))
  let patch32 = Int32(truncatingIfNeeded:Int(queryVersion.2))

  // Defer to a builtin that calls clang's version checking builtin from
  // compiler-rt.
  let result32 = Int32(Builtin.targetOSVersionAtLeast(major32._value,
                                                      minor32._value,
                                                      patch32._value))
  return (result32 != (0 as Int32))._value
#else
  // FIXME: As yet, there is no obvious versioning standard for platforms other
  // than Darwin-based OSes, so we just assume false for now. 
  // rdar://problem/18881232
  return false._value
#endif
}

// Performs an availability check in macCatalyst code to support back
// deployment. This entry point takes in a variant OS version
// (i.e, an iOS version).
//
// This is not inlinable because we
// don't want to inline the messy implementation details of the
// compiler-rt support into apps and expose those as ABI surface.
//
// This is a magic entry point known to the compiler. It is called in
// generated code for API availability checking.

#if (os(macOS) || os(iOS) && targetEnvironment(macCatalyst)) && SWIFT_RUNTIME_OS_VERSIONING
@_semantics("availability.osversion")
@_effects(readnone)
@available(macOS 10.15, iOS 13.0, *)
#if hasFeature(Macros)
@_noLocks
#endif
public func _stdlib_isVariantOSVersionAtLeast(
  _ major: Builtin.Word,
  _ minor: Builtin.Word,
  _ patch: Builtin.Word
  ) -> Builtin.Int1 {
  if Int(major) == 9999 {
    return true._value
  }
  let queryVersion = (Int(major), Int(minor), Int(patch))
  let major32 = Int32(truncatingIfNeeded:Int(queryVersion.0))
  let minor32 = Int32(truncatingIfNeeded:Int(queryVersion.1))
  let patch32 = Int32(truncatingIfNeeded:Int(queryVersion.2))

  // Defer to a builtin that calls clang's version checking builtin from
  // compiler-rt.
  let result32 = Int32(Builtin.targetVariantOSVersionAtLeast(major32._value,
                                                             minor32._value,
                                                             patch32._value))
  return (result32 != (0 as Int32))._value
}
#endif

// Performs an availability check in zippered code to support back
// deployment. This entry point takes in both a primary OS version
// (i.e., a macOS version) and a variant OS version  (i.e, an iOS version).
//
// In a normal macOS process it will return 1 if the running OS version is
// greater than or equal to major.minor.patchVersion and 0 otherwise. For an
// macCatalyst process it will return 1 if the running macCatalyst version is greater
// than or equal to the passed-in variant version.
//
// Unlike _stdlib_isOSVersionAtLeast, this is not inlinable because we
// don't want to inline the messy implementation details of the
// compiler-rt support into apps and expose those as ABI surface.
//
// This is a magic entry point known to the compiler. It is called in
// generated code for API availability checking.

#if (os(macOS) || os(iOS) && targetEnvironment(macCatalyst)) && SWIFT_RUNTIME_OS_VERSIONING
@_semantics("availability.osversion")
@_effects(readnone)
@_unavailableInEmbedded
#if hasFeature(Macros)
@_noLocks
#endif
public func _stdlib_isOSVersionAtLeastOrVariantVersionAtLeast(
  _ major: Builtin.Word,
  _ minor: Builtin.Word,
  _ patch: Builtin.Word,
  _ variantMajor: Builtin.Word,
  _ variantMinor: Builtin.Word,
  _ variantPatch: Builtin.Word
  ) -> Builtin.Int1 {
  if Int(major) == 9999 {
    return true._value
  }
  let queryVersion = (Int(major), Int(minor), Int(patch))
  let queryVariantVersion =
    (Int(variantMajor), Int(variantMinor), Int(variantPatch))

  let major32 = UInt32(truncatingIfNeeded:Int(queryVersion.0))
  let minor32 = UInt32(truncatingIfNeeded:Int(queryVersion.1))
  let patch32 = UInt32(truncatingIfNeeded:Int(queryVersion.2))

  let variantMajor32 = UInt32(truncatingIfNeeded:Int(queryVariantVersion.0))
  let variantMinor32 = UInt32(truncatingIfNeeded:Int(queryVariantVersion.1))
  let variantPatch32 = UInt32(truncatingIfNeeded:Int(queryVariantVersion.2))

  // Defer to a builtin that calls clang's version checking builtin from
  // compiler-rt.
  let result32 = Int32(Builtin.targetOSVersionOrVariantOSVersionAtLeast(
          major32._value, minor32._value, patch32._value,
          variantMajor32._value, variantMinor32._value, variantPatch32._value))

  return (result32 != (0 as UInt32))._value
}
#endif

public typealias _SwiftStdlibVersion = SwiftShims._SwiftStdlibVersion

/// Return true if the main executable was linked with an SDK version
/// corresponding to the given Swift Stdlib release, or later. Otherwise, return
/// false.
///
/// This is useful to maintain compatibility with older binaries after a
/// behavioral change in the stdlib.
///
/// This function must not be called from inlinable code.
@inline(__always)
@_unavailableInEmbedded
internal func _isExecutableLinkedOnOrAfter(
  _ stdlibVersion: _SwiftStdlibVersion
) -> Bool {
#if SWIFT_RUNTIME_OS_VERSIONING
  return _swift_stdlib_isExecutableLinkedOnOrAfter(stdlibVersion)
#else
  return true
#endif
}

extension _SwiftStdlibVersion {
  @_alwaysEmitIntoClient
  public static var v5_6_0: Self { Self(_value: 0x050600) }

  @_alwaysEmitIntoClient
  public static var v5_7_0: Self { Self(_value: 0x050700) }

  // Note: As of now, there is no bincompat level defined for the versions
  // below. If you need to use one of these in a call to
  // `_isExecutableLinkedOnOrAfter`, then you'll need to define the
  // corresponding version in the runtime.
  @_alwaysEmitIntoClient
  public static var v5_8_0: Self { Self(_value: 0x050800) }
  @_alwaysEmitIntoClient
  public static var v5_9_0: Self { Self(_value: 0x050900) }
  @_alwaysEmitIntoClient
  public static var v5_10_0: Self { Self(_value: 0x050A00) }
  @_alwaysEmitIntoClient
  public static var v6_0_0: Self { Self(_value: 0x060000) }
  @_alwaysEmitIntoClient
  public static var v6_1_0: Self { Self(_value: 0x060100) }
  @_alwaysEmitIntoClient
  public static var v6_2_0: Self { Self(_value: 0x060200) }
  @_alwaysEmitIntoClient
  public static var v6_3_0: Self { Self(_value: 0x060300) }

  @available(SwiftStdlib 5.7, *)
  public static var current: Self { .v6_3_0 }
}

@available(SwiftStdlib 5.7, *)
@_unavailableInEmbedded
extension _SwiftStdlibVersion: CustomStringConvertible {
  @available(SwiftStdlib 5.7, *)
  public var description: String {
    let major = _value >> 16
    let minor = (_value >> 8) & 0xFF
    let patch = _value & 0xFF
    return "\(major).\(minor).\(patch)"
  }
}


