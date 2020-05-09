//===--- magic-symbols-for-install-name.c - Magic linker directive symbols ===//
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
//
// A file containing magic symbols that instruct the linker to use a
// different install name when targeting older OSes. This file gets
// compiled into all of the libraries that are embedded for backward
// deployment.
//
//===----------------------------------------------------------------------===//

#if defined(__APPLE__) && defined(__MACH__)

#include <Availability.h>
#include <TargetConditionals.h>
#include "../public/SwiftShims/Visibility.h"

#define RPATH_INSTALL_NAME_DIRECTIVE_IMPL2(name, major, minor) \
  SWIFT_RUNTIME_EXPORT const char install_name_ ## major ## _ ## minor \
  __asm("$ld$install_name$os" #major "." #minor "$@rpath/lib" #name ".dylib"); \
  const char install_name_ ## major ## _ ## minor = 0;

#define RPATH_INSTALL_NAME_DIRECTIVE_IMPL(name, major, minor) \
  RPATH_INSTALL_NAME_DIRECTIVE_IMPL2(name, major, minor)

#define RPATH_INSTALL_NAME_DIRECTIVE(major, minor) \
  RPATH_INSTALL_NAME_DIRECTIVE_IMPL(SWIFT_TARGET_LIBRARY_NAME, major, minor)


#if TARGET_OS_WATCH
  // Check watchOS first, because TARGET_OS_IPHONE includes watchOS.
  RPATH_INSTALL_NAME_DIRECTIVE( 2, 0)
  RPATH_INSTALL_NAME_DIRECTIVE( 2, 1)
  RPATH_INSTALL_NAME_DIRECTIVE( 2, 2)
  RPATH_INSTALL_NAME_DIRECTIVE( 3, 0)
  RPATH_INSTALL_NAME_DIRECTIVE( 3, 1)
  RPATH_INSTALL_NAME_DIRECTIVE( 3, 2)
  RPATH_INSTALL_NAME_DIRECTIVE( 4, 0)
  RPATH_INSTALL_NAME_DIRECTIVE( 4, 1)
  RPATH_INSTALL_NAME_DIRECTIVE( 4, 2)
  RPATH_INSTALL_NAME_DIRECTIVE( 4, 3)
  RPATH_INSTALL_NAME_DIRECTIVE( 5, 0)
  RPATH_INSTALL_NAME_DIRECTIVE( 5, 1)
#elif TARGET_OS_IPHONE
  RPATH_INSTALL_NAME_DIRECTIVE( 7, 0)
  RPATH_INSTALL_NAME_DIRECTIVE( 7, 1)
  RPATH_INSTALL_NAME_DIRECTIVE( 8, 0)
  RPATH_INSTALL_NAME_DIRECTIVE( 8, 1)
  RPATH_INSTALL_NAME_DIRECTIVE( 8, 2)
  RPATH_INSTALL_NAME_DIRECTIVE( 8, 3)
  RPATH_INSTALL_NAME_DIRECTIVE( 8, 4)
  RPATH_INSTALL_NAME_DIRECTIVE( 9, 0)
  RPATH_INSTALL_NAME_DIRECTIVE( 9, 1)
  RPATH_INSTALL_NAME_DIRECTIVE( 9, 2)
  RPATH_INSTALL_NAME_DIRECTIVE( 9, 3)
  RPATH_INSTALL_NAME_DIRECTIVE(10, 0)
  RPATH_INSTALL_NAME_DIRECTIVE(10, 1)
  RPATH_INSTALL_NAME_DIRECTIVE(10, 2)
  RPATH_INSTALL_NAME_DIRECTIVE(10, 3)
  RPATH_INSTALL_NAME_DIRECTIVE(11, 0)
  RPATH_INSTALL_NAME_DIRECTIVE(11, 1)
  RPATH_INSTALL_NAME_DIRECTIVE(11, 2)
  RPATH_INSTALL_NAME_DIRECTIVE(11, 3)
  RPATH_INSTALL_NAME_DIRECTIVE(11, 4)
  RPATH_INSTALL_NAME_DIRECTIVE(12, 0)
  RPATH_INSTALL_NAME_DIRECTIVE(12, 1)
#elif TARGET_OS_OSX
  RPATH_INSTALL_NAME_DIRECTIVE(10,  9)
  RPATH_INSTALL_NAME_DIRECTIVE(10, 10)
  RPATH_INSTALL_NAME_DIRECTIVE(10, 11)
  RPATH_INSTALL_NAME_DIRECTIVE(10, 12)
  RPATH_INSTALL_NAME_DIRECTIVE(10, 13)

  // When building with a deployment target of < macOS 10.14,
  // treat macOS 10.14 as an "older OS."
#if __MAC_OS_X_VERSION_MIN_REQUIRED < __MAC_10_14
  RPATH_INSTALL_NAME_DIRECTIVE(10, 14)
#endif

#else
  #error Unknown target.
#endif

#endif // defined(__APPLE__) && defined(__MACH__)
