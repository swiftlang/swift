//===--- Win32.cpp - Win32 utility functions --------------------*- C++ -*-===//
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

#include "swift/Runtime/Debug.h"
#include "swift/Runtime/Win32.h"

#ifdef _WIN32

#include <windows.h>

char *
swift::win32::copyUTF8FromWide(const wchar_t *str) {
  char *result = nullptr;
  int len = ::WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS,
                                  str, -1,
                                  nullptr, 0,
                                  nullptr, nullptr);
  if (len <= 0) {
    swift::fatalError(0, "failed to convert string '%ls' "
                      "from wide to UTF-8: %lx\n",
                      str, ::GetLastError());
  }

  result = reinterpret_cast<char *>(std::malloc(len));
  if (!result) {
    swift::fatalError(0, "unable to allocate space to convert '%ls': %d\n",
                      str, errno);
  }

  len = ::WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS,
                              str, -1,
                              result, len,
                              nullptr, nullptr);

  if (len <= 0) {
    swift::fatalError(0, "failed to convert string '%ls' "
                      "from wide to UTF-8: %lx\n",
                      str, ::GetLastError());
  }

  return result;
}

wchar_t *
swift::win32::copyWideFromUTF8(const char *str) {
  wchar_t *result = nullptr;
  int len = ::MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
                                  str, -1,
                                  nullptr, 0);
  if (len <= 0) {
    swift::fatalError(0, "failed to convert string '%s' "
                      "from UTF-8 to wide: %lx\n",
                      str, ::GetLastError());
  }

  result = reinterpret_cast<wchar_t *>(std::malloc(len * sizeof(wchar_t)));
  if (!result) {
    swift::fatalError(0, "unable to allocate space to convert '%s': %d\n",
                      str, errno);
  }

  len = ::MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
                              str, -1,
                              result, len);
  if (len <= 0) {
    swift::fatalError(0, "failed to convert string '%s' "
                      "from UTF-8 to wide: %lx\n",
                      str, ::GetLastError());
  }

  return result;
}

#endif // defined(_WIN32)
