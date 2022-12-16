//===--- Win32.h - Win32 utility functions ----------------------*- C++ -*-===//
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
// Utility functions that are specific to the Windows port.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_WIN32_H
#define SWIFT_RUNTIME_WIN32_H

#ifdef _WIN32

#include "swift/shims/Visibility.h"

#include <wchar.h>

/// Convert a wide string to UTF-8.
///
/// @param str The string to convert.
///
/// @returns The string, converted to UTF-8.  The caller is responsible
///          for freeing this string with @c free() when done with it.
///
/// If @a str cannot be converted to UTF-8, @c nullptr is returned.
SWIFT_RUNTIME_STDLIB_INTERNAL
char *_swift_win32_copyUTF8FromWide(const wchar_t *str);

/// Convert a UTF-8 string to a wide string.
///
/// @param str The string to convert.
///
/// @returns The string, converted to UTF-16.  The caller is responsible
///          for freeing this string with @c free() when done with it.
///
/// If @a str cannot be converted to UTF-16, @c nullptr is returned.
SWIFT_RUNTIME_STDLIB_INTERNAL
wchar_t *_swift_win32_copyWideFromUTF8(const char *str);

#endif // defined(_WIN32)

#endif // SWIFT_RUNTIME_WIN32_H
