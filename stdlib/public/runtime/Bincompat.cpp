//===--- Bincompat.cpp - Binary compatibility checks. -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Checks for enabling binary compatibility workarounds.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Bincompat.h"
#include <stdint.h>

// If this is an Apple OS, use the Apple binary compatibility rules
#if __has_include(<mach-o/dyld_priv.h>)
  #include <mach-o/dyld_priv.h>
  #ifndef BINARY_COMPATIBILITY_APPLE
    #define BINARY_COMPATIBILITY_APPLE 1
  #endif
#else
  #undef BINARY_COMPATIBILITY_APPLE
#endif

namespace swift {

namespace runtime {

namespace bincompat {

// Should we mimic the old override behavior when scanning protocol conformance records?

// Old apps expect protocol conformances to override each other in a particular
// order.  Starting with Swift 5.4, that order has changed as a result of
// significant performance improvements to protocol conformance scanning.  If
// this returns `true`, the protocol conformance scan will do extra work to
// mimic the old override behavior.
bool workaroundProtocolConformanceReverseIteration() {
#if BINARY_COMPATIBILITY_APPLE
  // If this is a newer Apple OS ...
  if (__builtin_available(macOS 11.3, iOS 14.5, tvOS 14.5, watchOS 7.4, *)) {
    const dyld_build_version_t spring_2021_os_versions = {0xffffffff, 0x007e50301};
    // ... but the app was compiled before Spring 2021, use the legacy behavior.
    return !dyld_program_sdk_at_least(spring_2021_os_versions);
  } else {
    return false; // Use new (non-legacy) behavior on old Apple OSes
  }
#else
  return false; // Never use the legacy behavior on non-Apple OSes
#endif
}

// Should the dynamic cast operation crash when it sees
// a non-nullable Obj-C pointer with a null value?

// Obj-C does not strictly enforce non-nullability in all cases, so it is
// possible for Obj-C code to pass null pointers into Swift code even when
// declared non-nullable.  Such null pointers can lead to undefined behavior
// later on.  Starting in Swift 5.4, these unexpected null pointers are fatal
// runtime errors, but this is selectively disabled for old apps.
bool unexpectedObjCNullWhileCastingIsFatal() {
#if BINARY_COMPATIBILITY_APPLE
  // If this is a new enough Apple OS ...
  if (__builtin_available(macOS 11.3, iOS 14.5, tvOS 14.5, watchOS 7.4, *)) {
    const dyld_build_version_t spring_2021_os_versions = {0xffffffff, 0x007e50301};
    // ... use strict behavior for apps compiled on or after Spring 2021.
    return dyld_program_sdk_at_least(spring_2021_os_versions);
  } else {
    return false; // Use permissive behavior on old Apple OS
  }
#else
  return true;  // Always use the strict behavior on non-Apple OSes
#endif
}

// Should casting a nil optional to another optional
// use the legacy semantics?

// For consistency, starting with Swift 5.4, casting Optional<Int> to
// Optional<Optional<Int>> always wraps the source in another layer
// of Optional.
// Earlier versions of the Swift runtime did not do this if the source
// optional was nil.  In that case, the outer target optional would be
// set to nil.
bool useLegacyOptionalNilInjection() {
#if BINARY_COMPATIBILITY_APPLE
  // If this is a new enough Apple OS ...
  if (__builtin_available(macOS 11.3, iOS 14.5, tvOS 14.5, watchOS 7.4, *)) {
    const dyld_build_version_t spring_2021_os_versions = {0xffffffff, 0x007e50301};
    // It's using Spring 2021 or later SDK, so don't use the legacy behavior.
    return !dyld_program_sdk_at_least(spring_2021_os_versions);
  } else {
    return true; // Use the legacy behavior on old Apple OS
  }
#else
  return false;  // Always use the 5.4 behavior on non-Apple OSes
#endif
}

} // namespace bincompat

} // namespace runtime

} // namespace swift
