//===--- Paths.cpp - Swift Runtime path utility functions -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Functions that obtain paths that might be useful within the runtime.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"
#include "swift/Runtime/EnvironmentVariables.h"
#include "swift/Runtime/Debug.h"
#include "swift/Runtime/Paths.h"
#include "swift/Runtime/Win32.h"
#include "swift/Threading/Once.h"

#include <filesystem>

#if !defined(_WIN32) || defined(__CYGWIN__)
#include <sys/stat.h>

#include <dlfcn.h>
#else
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#include <psapi.h>
#endif

#include <cerrno>
#include <cstdlib>
#include <cstring>

namespace {

swift::once_t runtimePathToken;
const char *runtimePath;

swift::once_t rootPathToken;
const char *rootPath;

void _swift_initRuntimePath(void *);
void _swift_initRootPath(void *);
const char *_swift_getDefaultRootPath();
const char *_swift_getAuxExePathIn(const char *path, const char *name);

bool _swift_isPathSep(char ch) {
#ifdef _WIN32
  return ch == '/' || ch == '\\';
#else
  return ch == '/';
#endif
}

bool _swift_exists(const char *path);

#if !defined(_WIN32) || defined(__CYGWIN__)
#define PATHSEP_STR "/"
#define PATHSEP_CHR '/'
#else
#define PATHSEP_STR "\\"
#define PATHSEP_CHR '\\'
#endif

}

SWIFT_RUNTIME_EXPORT
const char *
swift_getRuntimePath()
{
  swift::once(runtimePathToken, _swift_initRuntimePath, nullptr);
  return runtimePath;
}

SWIFT_RUNTIME_EXPORT
const char *
swift_getRootPath()
{
  swift::once(rootPathToken, _swift_initRootPath, nullptr);
  return rootPath;
}

namespace {

const char *
_swift_getDefaultRootPath()
{
  const char *runtimePath = swift_getRuntimePath();
  size_t runtimePathLen = std::strlen(runtimePath);

  // Scan backwards until we find a path separator
  const char *ptr = runtimePath + runtimePathLen;
  while (ptr > runtimePath && !_swift_isPathSep(*--ptr));

  // Remove lib/swift/ if present
  if (ptr - runtimePath >= 10
      && _swift_isPathSep(ptr[-10])
      && std::strncmp(ptr - 9, "lib", 3) == 0
      && _swift_isPathSep(ptr[-6])
      && std::strncmp(ptr - 5, "swift", 5) == 0) {
    ptr -= 10;
  }

  // If the result is empty, return "./" or ".\\"
  if (ptr == runtimePath) {
    return "." PATHSEP_STR;
  }

  // Duplicate the string up to and including ptr
  size_t len = ptr - runtimePath + 1;
  char *thePath = (char *)malloc(len + 1);
  std::memcpy(thePath, runtimePath, len);
  thePath[len] = 0;

  return thePath;
}

void
_swift_initRootPath(void *)
{
  // SWIFT_ROOT overrides the path returned by this function
  const char *swiftRoot = swift::runtime::environment::SWIFT_ROOT();
  if (swiftRoot && *swiftRoot) {
    size_t len = std::strlen(swiftRoot);

    // Ensure that there's a trailing slash
    if (_swift_isPathSep(swiftRoot[len - 1])) {
      rootPath = swiftRoot;
    } else {
      char *thePath = (char *)malloc(len + 2);
      std::memcpy(thePath, swiftRoot, len);
      thePath[len] = PATHSEP_CHR;
      thePath[len + 1] = 0;

      rootPath = thePath;
    }
  } else {
    rootPath = _swift_getDefaultRootPath();
  }
}

#if _WIN32
/// Map an NT-style filename to a Win32 filename.
///
/// We can't use GetFinalPathNameByHandle() because there's no way to obtain
/// a handle (at least, not without using the internal NtCreateFile() API, which
/// we aren't supposed to be using).  Additionally, that function would resolve
/// symlinks, which we don't want to do here.
///
/// As a result, we use the approach demonstrated here:
///
///  https://learn.microsoft.com/en-us/windows/win32/memory/obtaining-a-file-name-from-a-file-handle
///
/// @param pszFilename The NT-style filename to convert.
///
/// @result A string, allocated using std::malloc(), containing the Win32-style
///         filename.
LPWSTR
_swift_win32NameFromNTName(LPWSTR pszFilename) {
  DWORD dwLen = GetLogicalDriveStringsW(0, NULL);
  if (!dwLen)
    return NULL;

  LPWSTR lpDriveStrings = (LPWSTR)std::malloc(dwLen * sizeof(WCHAR));
  if (!lpDriveStrings)
    return NULL;

  DWORD dwRet = GetLogicalDriveStringsW(dwLen, lpDriveStrings);
  if (!dwRet)
    return NULL;

  LPWSTR pszDrive = lpDriveStrings;
  while (*pszDrive) {
    size_t len = wcslen(pszDrive);
    if (len && pszDrive[len - 1] == '\\')
      pszDrive[len - 1] = 0;

    WCHAR ntPath[4096];
    dwRet = QueryDosDeviceW(pszDrive, ntPath, 4096);
    if (dwRet) {
      size_t ntLen = wcslen(ntPath);

      if (_wcsnicmp(pszFilename, ntPath, ntLen) == 0
          && pszFilename[ntLen] == '\\') {
        size_t fnLen = wcslen(pszFilename);
        size_t driveLen = wcslen(pszDrive);
        size_t pathLen = fnLen - ntLen;
        size_t newLen = driveLen + pathLen + 1;
        LPWSTR pszWin32Name = (LPWSTR)std::malloc(newLen * sizeof(WCHAR));
        if (!pszWin32Name) {
          std::free(lpDriveStrings);
          return NULL;
        }

        LPWSTR ptr = pszWin32Name;
        memcpy(ptr, pszDrive, driveLen * sizeof(WCHAR));
        ptr += driveLen;
        memcpy(ptr, pszFilename + ntLen, pathLen * sizeof(WCHAR));
        ptr += pathLen;
        *ptr = 0;

        std::free(lpDriveStrings);

        return pszWin32Name;
      }
    }

    pszDrive += len + 1;
  }

  std::free(lpDriveStrings);

  return _wcsdup(pszFilename);
}
#endif

}

SWIFT_RUNTIME_EXPORT
const char *
swift_getAuxiliaryExecutablePath(const char *name)
{
  const char *rootPath = swift_getRootPath();

  // Form <rootPath>/libexec/swift/
  size_t rootPathLen = std::strlen(rootPath);
  const char *libexecStr = PATHSEP_STR "libexec" PATHSEP_STR "swift" PATHSEP_STR;
  size_t libexecLen = std::strlen(libexecStr);
  char *libexecPath = (char *)malloc(rootPathLen + libexecLen + 1);
  std::memcpy(libexecPath, rootPath, rootPathLen);
  std::memcpy(libexecPath + rootPathLen, libexecStr, libexecLen + 1);

  // If libexec exists, look there
  if (_swift_exists(libexecPath)) {
    const char *result = _swift_getAuxExePathIn(libexecPath, name);

    free(libexecPath);

    return result;
  }

  free(libexecPath);

  // Otherwise, look in the root itself
  return _swift_getAuxExePathIn(rootPath, name);
}

namespace {

const char *
_swift_getAuxExePathIn(const char *path, const char *name)
{
  size_t pathLen = std::strlen(path);
  size_t nameLen = std::strlen(name);
  size_t extLen = 0;

#ifdef _WIN32
  if (nameLen > 4 && strcmp(name + nameLen - 4, ".exe") != 0)
    extLen = 4;
#endif

  // <path>/<name>[.exe]<NUL>
  size_t totalLen = pathLen + nameLen + extLen + 1;
  char *fullPath = (char *)malloc(totalLen);
  char *ptr = fullPath;
  std::memcpy(ptr, path, pathLen);
  ptr += pathLen;
  std::memcpy(ptr, name, nameLen);
  ptr += nameLen;

#ifdef _WIN32
  if (extLen) {
    std::memcpy(ptr, ".exe", 4);
    ptr += 4;
  }
#endif

  *ptr = 0;

  return fullPath;
}

#if !defined(_WIN32) || defined(__CYGWIN__)
void
_swift_initRuntimePath(void *) {
  const char *path;

#if APPLE_OS_SYSTEM
  path = dyld_image_path_containing_address(_swift_initRuntimePath);
#else
  Dl_info dli;
  int ret = ::dladdr((void *)_swift_initRuntimePath, &dli);

  if (!ret) {
    swift::fatalError(/* flags = */ 0,
                      "Unable to obtain Swift runtime path\n");
  }

  path = dli.dli_fname;
#endif

  runtimePath = ::strdup(path);
}
#else

void
_swift_initRuntimePath(void *) {
  const DWORD dwBufSize = 4096;
  LPWSTR lpFilename = (LPWSTR)std::malloc(dwBufSize * sizeof(WCHAR));

  DWORD dwRet = GetMappedFileNameW(GetCurrentProcess(),
                                   (void *)_swift_initRuntimePath,
                                   lpFilename,
                                   dwBufSize);
  if (!dwRet) {
    swift::fatalError(/* flags = */ 0,
                      "Unable to obtain Swift runtime path\n");
  }

  // GetMappedFileNameW() returns an NT-style path, not a Win32 path; that is,
  // it starts with \Device\DeviceName rather than a drive letter.
  LPWSTR lpWin32Filename = _swift_win32NameFromNTName(lpFilename);
  if (!lpWin32Filename) {
    swift::fatalError(/* flags = */ 0,
                      "Unable to obtain Win32 path for Swift runtime\n");
  }

  std::free(lpFilename);

  runtimePath = swift::win32::copyUTF8FromWide(lpWin32Filename);
  if (!runtimePath) {
    swift::fatalError(/* flags = */ 0,
                      "Unable to convert Swift runtime path to UTF-8: %lx, %d\n",
                      ::GetLastError(), errno);
  }

  std::free(lpWin32Filename);
}
#endif

/// Return true if a file exists at path.
///
/// On Windows, path will be in UTF-8 so can't be passed to _stat() or any
/// of the ANSI functions.
///
/// @param path The path to check
///
/// @result true iff there is a file at @a path
bool _swift_exists(const char *path)
{
#if !defined(_WIN32)
  struct stat st;
  return stat(path, &st) == 0;
#else
  wchar_t *wszPath = swift::win32::copyWideFromUTF8(path);
  bool result = GetFileAttributesW(wszPath) != INVALID_FILE_ATTRIBUTES;
  free(wszPath);
  return result;
#endif // defined(_WIN32)
}

}
