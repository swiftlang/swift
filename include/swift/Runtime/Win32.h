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

#include <functional>
#include <type_traits>

// For HANDLE
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>

#include <wchar.h>

/// Convert a wide string to UTF-8.
///
/// @param str The string to convert.
///
/// @returns The string, converted to UTF-8.  The caller is responsible
///          for freeing this string with @c free() when done with it.
///
/// If @a str cannot be converted to UTF-8, @c nullptr is returned.
SWIFT_RUNTIME_STDLIB_SPI
char *_swift_win32_copyUTF8FromWide(const wchar_t *str);

/// Convert a UTF-8 string to a wide string.
///
/// @param str The string to convert.
///
/// @returns The string, converted to UTF-16.  The caller is responsible
///          for freeing this string with @c free() when done with it.
///
/// If @a str cannot be converted to UTF-16, @c nullptr is returned.
SWIFT_RUNTIME_STDLIB_SPI
wchar_t *_swift_win32_copyWideFromUTF8(const char *str);

/// Configure the environment to allow calling into the Debug Help library.
///
/// \param body A function to invoke. This function attempts to first initialize
///   the Debug Help library. If it did so successfully, the handle used during
///   initialization is passed to this function and should be used with
///   subsequent calls to the Debug Help library. Do not close this handle.
/// \param context A caller-supplied value to pass to \a body.
///
/// On Windows, the Debug Help library (DbgHelp.lib) is not thread-safe. All
/// calls into it from the Swift runtime and stdlib should route through this
/// function.
///
/// This function sets the Debug Help library's options by calling
/// \c SymSetOptions() before \a body is invoked, and then resets them back to
/// their old value before returning. \a body can also call \c SymSetOptions()
/// if needed.
SWIFT_RUNTIME_STDLIB_SPI
void _swift_win32_withDbgHelpLibrary(
  void (* body)(HANDLE hProcess, void *context), void *context);

/// Configure the environment to allow calling into the Debug Help library.
///
/// \param body A function to invoke. This function attempts to first initialize
///   the Debug Help library. If it did so successfully, the handle used during
///   initialization is passed to this function and should be used with
///   subsequent calls to the Debug Help library. Do not close this handle.
///
/// On Windows, the Debug Help library (DbgHelp.lib) is not thread-safe. All
/// calls into it from the Swift runtime and stdlib should route through this
/// function.
///
/// This function sets the Debug Help library's options by calling
/// \c SymSetOptions() before \a body is invoked, and then resets them back to
/// their old value before returning. \a body can also call \c SymSetOptions()
/// if needed.
static inline void _swift_win32_withDbgHelpLibrary(
  const std::function<void(HANDLE /*hProcess*/)> &body) {
  _swift_win32_withDbgHelpLibrary([](HANDLE hProcess, void *context) {
    auto bodyp = reinterpret_cast<std::function<void(HANDLE)> *>(context);
    (* bodyp)(hProcess);
  }, const_cast<void *>(reinterpret_cast<const void *>(&body)));
}

/// Configure the environment to allow calling into the Debug Help library.
///
/// \param body A function to invoke. This function attempts to first initialize
///   the Debug Help library. If it did so successfully, the handle used during
///   initialization is passed to this function and should be used with
///   subsequent calls to the Debug Help library. Do not close this handle.
///
/// \returns Whatever is returned from \a body.
///
/// On Windows, the Debug Help library (DbgHelp.lib) is not thread-safe. All
/// calls into it from the Swift runtime and stdlib should route through this
/// function.
///
/// This function sets the Debug Help library's options by calling
/// \c SymSetOptions() before \a body is invoked, and then resets them back to
/// their old value before returning. \a body can also call \c SymSetOptions()
/// if needed.
template <
  typename Callable,
  typename ResultType = std::invoke_result_t<Callable&, HANDLE>,
  typename = std::enable_if_t<!std::is_same_v<void, ResultType>>
>
static inline ResultType _swift_win32_withDbgHelpLibrary(const Callable& body) {
  ResultType result;

  _swift_win32_withDbgHelpLibrary([&body, &result] (HANDLE hProcess) {
    result = body(hProcess);
  });

  return result;
}

#endif // defined(_WIN32)

#endif // SWIFT_RUNTIME_WIN32_H
