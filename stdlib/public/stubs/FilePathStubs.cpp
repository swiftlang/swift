//===--- FilePathStubs.cpp - Filesystem syscalls for FilePath -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  C++ implementation of `_swift_stdlib_FilePath_resolve`. Translated from
//  `FilePathResolve.swift`'s package-side `_resolveDarwin` / `_resolveLinux`
//  / `_resolveWindows`. Behavior is identical; only the language differs.
//
//===----------------------------------------------------------------------===//

#include "swift/shims/FilePath.h"

#include <cstdlib>
#include <cstring>
#include <cerrno>

#if defined(_WIN32)
  #define WIN32_LEAN_AND_MEAN
  #include <windows.h>
#elif defined(__APPLE__)
  #include <fcntl.h>
  #include <sys/attr.h>
  #include <sys/syscall.h>
  #include <unistd.h>
#else
  // POSIX (Linux, BSD, etc.). `realpath(p, NULL)` allocates with malloc.
  #include <limits.h>
  #include <stdlib.h>
  #include <unistd.h>
#endif

namespace {

// Allocate a NUL-terminated copy of (bytes, count) for the syscall.
// Returns nullptr on OOM.
template <typename CodeUnit>
static CodeUnit *makeNulTerminatedCopy(const CodeUnit *bytes, size_t count) {
  CodeUnit *out = static_cast<CodeUnit *>(
      std::malloc((count + 1) * sizeof(CodeUnit)));
  if (out == nullptr) return nullptr;
  if (count > 0) {
    std::memcpy(out, bytes, count * sizeof(CodeUnit));
  }
  out[count] = 0;
  return out;
}

} // anonymous namespace

extern "C"
int _swift_stdlib_FilePath_resolve(
    const _swift_filepath_codeunit *bytes, __swift_size_t count,
    _swift_filepath_codeunit **outBuf, __swift_size_t *outCount) {

#if defined(_WIN32)
  // Windows: CreateFileW + GetFinalPathNameByHandleW.
  wchar_t *input = makeNulTerminatedCopy(
      reinterpret_cast<const wchar_t *>(bytes), count);
  if (input == nullptr) return ERROR_NOT_ENOUGH_MEMORY;

  HANDLE handle = CreateFileW(
      input, 0,
      FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
      nullptr, OPEN_EXISTING,
      FILE_FLAG_BACKUP_SEMANTICS, nullptr);
  std::free(input);
  if (handle == INVALID_HANDLE_VALUE) {
    return static_cast<int>(GetLastError());
  }

  const DWORD flags = FILE_NAME_NORMALIZED | VOLUME_NAME_DOS;
  size_t capacity = 1024;
  for (;;) {
    wchar_t *buf = static_cast<wchar_t *>(
        std::malloc(capacity * sizeof(wchar_t)));
    if (buf == nullptr) {
      CloseHandle(handle);
      return ERROR_NOT_ENOUGH_MEMORY;
    }
    DWORD needed = GetFinalPathNameByHandleW(
        handle, buf, static_cast<DWORD>(capacity), flags);
    if (needed == 0) {
      DWORD err = GetLastError();
      std::free(buf);
      CloseHandle(handle);
      return static_cast<int>(err);
    }
    // On success `needed` is the length WITHOUT the NUL (so it fits with
    // room to spare). On buffer-too-small it's the required size INCLUDING
    // the NUL — grow and retry.
    if (static_cast<size_t>(needed) < capacity) {
      CloseHandle(handle);
      *outBuf = reinterpret_cast<_swift_filepath_codeunit *>(buf);
      *outCount = needed;
      return 0;
    }
    std::free(buf);
    capacity = static_cast<size_t>(needed) + 1;
  }

#elif defined(__APPLE__)
  // Darwin: getattrlistat with ATTR_CMN_FULLPATH. The kernel's no-FSOPT
  // default returns the firmlinked path (e.g. /Users/foo, not
  // /System/Volumes/Data/Users/foo). realpath(3) does NOT have that
  // behavior on Big Sur+ and is intentionally avoided.
  char *input = makeNulTerminatedCopy(
      reinterpret_cast<const char *>(bytes), count);
  if (input == nullptr) return ENOMEM;

  attrlist al;
  std::memset(&al, 0, sizeof(al));
  al.bitmapcount = ATTR_BIT_MAP_COUNT;
  al.commonattr  = ATTR_CMN_FULLPATH;

  const unsigned long options =
      FSOPT_ATTR_CMN_EXTENDED | FSOPT_RETURN_REALDEV;

  // Two attempts: 8 KiB suits any normal path, 32 KiB covers
  // long-path-enabled processes. Past that, the path is genuinely too long.
  static const size_t kBufSizes[] = { 8192, 32768 };
  for (size_t i = 0; i < sizeof(kBufSizes) / sizeof(kBufSizes[0]); ++i) {
    size_t bufSize = kBufSizes[i];
    char *raw = static_cast<char *>(std::malloc(bufSize));
    if (raw == nullptr) { std::free(input); return ENOMEM; }

    int rc = getattrlistat(AT_FDCWD, input, &al, raw, bufSize, options);
    if (rc != 0) {
      int err = errno;
      std::free(raw);
      if ((err == ERANGE || err == ENAMETOOLONG)
          && i + 1 < sizeof(kBufSizes) / sizeof(kBufSizes[0])) {
        continue;
      }
      std::free(input);
      return err;
    }

    // Buffer layout when ATTR_CMN_FULLPATH is the only attribute requested:
    //   raw[0..4]    uint32_t total bytes used (we ignore this header)
    //   raw[4..12]   attrreference_t for FULLPATH
    //                  .attr_dataoffset  offset relative to the start of
    //                                    the attrreference_t struct itself
    //                  .attr_length      bytes (includes trailing NUL)
    //   raw[4 + .attr_dataoffset ..]    the resolved C-string path
    const size_t attrrefOffset = sizeof(uint32_t);
    attrreference_t ref;
    std::memcpy(&ref, raw + attrrefOffset, sizeof(ref));
    const char *pathStart = raw + attrrefOffset
                          + static_cast<size_t>(ref.attr_dataoffset);
    size_t storedLength = static_cast<size_t>(ref.attr_length);
    // attr_length includes the NUL; strip it.
    size_t pathLen = (storedLength > 0) ? storedLength - 1 : 0;

    char *result = static_cast<char *>(std::malloc(
        (pathLen == 0 ? 1 : pathLen) * sizeof(char)));
    if (result == nullptr) {
      std::free(raw);
      std::free(input);
      return ENOMEM;
    }
    if (pathLen > 0) std::memcpy(result, pathStart, pathLen);
    std::free(raw);
    std::free(input);
    *outBuf = reinterpret_cast<_swift_filepath_codeunit *>(result);
    *outCount = pathLen;
    return 0;
  }
  std::free(input);
  return ENAMETOOLONG;

#else
  // POSIX: realpath(p, NULL) allocates with malloc; caller frees. Linux has
  // neither firmlinks nor Darwin-style anchor prefixes, so this is the
  // correct portable call.
  char *input = makeNulTerminatedCopy(
      reinterpret_cast<const char *>(bytes), count);
  if (input == nullptr) return ENOMEM;

  char *resolved = realpath(input, nullptr);
  std::free(input);
  if (resolved == nullptr) {
    return errno;
  }
  *outBuf = reinterpret_cast<_swift_filepath_codeunit *>(resolved);
  *outCount = std::strlen(resolved);
  return 0;
#endif
}
