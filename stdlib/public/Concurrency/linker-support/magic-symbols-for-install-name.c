//===--- magic-symbols-for-install-name.c - Magic linker directive symbols ===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
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
// This file is specific to the Concurrency library; there is a matching file
// for the standard library with the same name.
//
//===----------------------------------------------------------------------===//

#if defined(__APPLE__) && defined(__MACH__)

#include <Availability.h>
#include <TargetConditionals.h>
#include "../../SwiftShims/Visibility.h"

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
  RPATH_INSTALL_NAME_DIRECTIVE( 6, 0)
  RPATH_INSTALL_NAME_DIRECTIVE( 6, 1)
  RPATH_INSTALL_NAME_DIRECTIVE( 6, 2)
  RPATH_INSTALL_NAME_DIRECTIVE( 6, 3)
  RPATH_INSTALL_NAME_DIRECTIVE( 7, 0)
  RPATH_INSTALL_NAME_DIRECTIVE( 7, 1)
  RPATH_INSTALL_NAME_DIRECTIVE( 7, 2)
  RPATH_INSTALL_NAME_DIRECTIVE( 7, 3)
  RPATH_INSTALL_NAME_DIRECTIVE( 7, 4)
  RPATH_INSTALL_NAME_DIRECTIVE( 7, 5)
  RPATH_INSTALL_NAME_DIRECTIVE( 7, 6)
#elif TARGET_OS_IPHONE
  RPATH_INSTALL_NAME_DIRECTIVE(13, 0)
  RPATH_INSTALL_NAME_DIRECTIVE(13, 1)
  RPATH_INSTALL_NAME_DIRECTIVE(13, 2)
  RPATH_INSTALL_NAME_DIRECTIVE(13, 3)
  RPATH_INSTALL_NAME_DIRECTIVE(13, 4)
  RPATH_INSTALL_NAME_DIRECTIVE(13, 5)
  RPATH_INSTALL_NAME_DIRECTIVE(13, 6)
  RPATH_INSTALL_NAME_DIRECTIVE(13, 7)
  RPATH_INSTALL_NAME_DIRECTIVE(14, 0)
  RPATH_INSTALL_NAME_DIRECTIVE(14, 1)
  RPATH_INSTALL_NAME_DIRECTIVE(14, 2)
  RPATH_INSTALL_NAME_DIRECTIVE(14, 3)
  RPATH_INSTALL_NAME_DIRECTIVE(14, 4)
  RPATH_INSTALL_NAME_DIRECTIVE(14, 5)
  RPATH_INSTALL_NAME_DIRECTIVE(14, 6)
  RPATH_INSTALL_NAME_DIRECTIVE(14, 7)
#elif TARGET_OS_OSX
  RPATH_INSTALL_NAME_DIRECTIVE(10, 15)
  RPATH_INSTALL_NAME_DIRECTIVE(11,  0)
  RPATH_INSTALL_NAME_DIRECTIVE(11,  1)
  RPATH_INSTALL_NAME_DIRECTIVE(11,  2)
  RPATH_INSTALL_NAME_DIRECTIVE(11,  3)
  RPATH_INSTALL_NAME_DIRECTIVE(11,  4)
  RPATH_INSTALL_NAME_DIRECTIVE(11,  5)
#else
  #error Unknown target.
#endif

#endif // defined(__APPLE__) && defined(__MACH__)
