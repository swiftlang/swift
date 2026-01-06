//===--- magic-symbols-for-install-name.c - Magic linker directive symbols ===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A file containing magic symbols that instruct the linker to use a
// different install name when targeting older OSes. This file gets
// compiled into all of the libraries that are embedded for backward
// deployment.
//
// This file is specific to the Builtin_float library; there is a matching
// file for the standard library with the same name.
//
//===----------------------------------------------------------------------===//

#if defined(__APPLE__) && defined(__MACH__) && SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT

#include <mach-o/loader.h>
#include <TargetConditionals.h>
#include "swift/shims/Visibility.h"

// Builtin_float was split out from the Darwin library in macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0,
// visionOS 2.0 Prior to macOS 10.14.4, iOS 12.2, watchOS 5.2, tvOS 12.2, Darwin was supported as an
// embedded library starting in macOS (née OS X) 10.9, iOS 7.0, watchOS 2.0, tvOS 9.0. In order to
// support back deployment, add a magic symbol so that back deployment will link on Darwin instead, and
// the embedded version on particularly old deployment targets. On newer OS versions, Darwin re-exports
// Builtin_float which is where the APIs are implemented.

// The linker uses a specially formatted symbol to do the back deployment:
// $ld$previous$<install-name>$<compatibility-version>$<platform>$<start-version>$<end-version>$<symbol-name>$
// compatibility-version and symbol-name are left off to apply to all library versions and symbols.
// This symbol isn't a legal C identifier, so it needs to be specified with __asm.
#define DARWIN_RPATH_PREVIOUS_DIRECTIVE(platform, startVersion, endVersion) \
  SWIFT_RUNTIME_EXPORT const char ld_previous_rpath_ ## platform \
  __asm("$ld$previous$@rpath/libswiftDarwin.dylib$$" __STRING(platform) "$" __STRING(startVersion) "$" __STRING(endVersion) "$$"); \
  const char ld_previous_rpath_ ## platform = 0;
// Using the __STRING macro is important so that platform gets expanded before being stringified.
// The versions could just be #version, __STRING is only used for consistency.

#define DARWIN_PREVIOUS_DIRECTIVE(platform, startVersion, endVersion) \
  SWIFT_RUNTIME_EXPORT const char ld_previous_ ## platform \
  __asm("$ld$previous$/usr/lib/swift/libswiftDarwin.dylib$$" __STRING(platform) "$" __STRING(startVersion) "$" __STRING(endVersion) "$$"); \
  const char ld_previous_ ## platform = 0;

#if TARGET_OS_OSX || TARGET_OS_MACCATALYST
DARWIN_RPATH_PREVIOUS_DIRECTIVE(PLATFORM_MACOS, 10.9, 10.14.4)
DARWIN_PREVIOUS_DIRECTIVE(PLATFORM_MACOS, 10.14.4, 15.0)
DARWIN_PREVIOUS_DIRECTIVE(PLATFORM_MACCATALYST, 13.1, 18.0)
#elif TARGET_OS_IOS && !TARGET_OS_VISION
#if TARGET_OS_SIMULATOR
DARWIN_RPATH_PREVIOUS_DIRECTIVE(PLATFORM_IOSSIMULATOR, 7.0, 12.2)
DARWIN_PREVIOUS_DIRECTIVE(PLATFORM_IOSSIMULATOR, 12.2, 18.0)
#else
DARWIN_RPATH_PREVIOUS_DIRECTIVE(PLATFORM_IOS, 7.0, 12.2)
DARWIN_PREVIOUS_DIRECTIVE(PLATFORM_IOS, 12.2, 18.0)
#endif
#elif TARGET_OS_WATCH
#if TARGET_OS_SIMULATOR
DARWIN_RPATH_PREVIOUS_DIRECTIVE(PLATFORM_WATCHOSSIMULATOR, 2.0, 5.2)
DARWIN_PREVIOUS_DIRECTIVE(PLATFORM_WATCHOSSIMULATOR, 5.2, 11.0)
#else
DARWIN_RPATH_PREVIOUS_DIRECTIVE(PLATFORM_WATCHOS, 2.0, 5.2)
DARWIN_PREVIOUS_DIRECTIVE(PLATFORM_WATCHOS, 5.2, 11.0)
#endif
#elif TARGET_OS_TV
#if TARGET_OS_SIMULATOR
DARWIN_RPATH_PREVIOUS_DIRECTIVE(PLATFORM_TVOSSIMULATOR, 9.0, 12.2)
DARWIN_PREVIOUS_DIRECTIVE(PLATFORM_TVOSSIMULATOR, 12.2, 18.0)
#else
DARWIN_RPATH_PREVIOUS_DIRECTIVE(PLATFORM_TVOS, 9.0, 12.2)
DARWIN_PREVIOUS_DIRECTIVE(PLATFORM_TVOS, 12.2, 18.0)
#endif
#elif TARGET_OS_VISION
#if TARGET_OS_SIMULATOR
DARWIN_PREVIOUS_DIRECTIVE(PLATFORM_VISIONOSSIMULATOR, 1.0, 2.0);
#else
DARWIN_PREVIOUS_DIRECTIVE(PLATFORM_VISIONOS, 1.0, 2.0);
#endif
#endif

#endif // defined(__APPLE__) && defined(__MACH__) && SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT
