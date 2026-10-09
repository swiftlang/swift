//===--- FilePath.h - Wrapper for FilePath syscalls -------------*- C++ -*-===//
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
//  Runtime stubs supporting filesystem-touching FilePath operations
//  (`resolve()`). Using the Darwin / Glibc / WinSDK module from the core
//  stdlib would create a circular dependency, so the syscall lives here.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_FILEPATH_H
#define SWIFT_STDLIB_SHIMS_FILEPATH_H

#include "SwiftStddef.h"
#include "SwiftStdint.h"
#include "Visibility.h"

#if __has_feature(nullability)
#pragma clang assume_nonnull begin
#endif

#ifdef __cplusplus
extern "C" {
#endif

// FilePath code unit: matches `FilePath.CodeUnit` exactly.
//   POSIX:   `Int8`         (== signed char)
//   Windows: `UInt16`       (== UTF-16 code unit; same width as wchar_t on Win)
#if defined(_WIN32)
typedef __swift_uint16_t _swift_filepath_codeunit;
#else
typedef __swift_int8_t   _swift_filepath_codeunit;
#endif

/// Resolve a filesystem path: produces an absolute, symlink-free path.
///
/// `bytes` / `count` describe the input path in `FilePath.CodeUnit`s, no NUL
/// (the impl makes its own NUL-terminated copy for syscall input).
///
/// On success: returns 0; `*outBuf` is a malloc'd buffer of `*outCount` code
/// units (no NUL); the caller must free it via `_swift_stdlib_free`.
/// On failure: returns `errno` (POSIX) or `GetLastError()` (Windows);
/// `*outBuf` and `*outCount` are unchanged.
///
/// The implementation grows its internal buffer on `ERANGE` /
/// `ENAMETOOLONG`; the caller does not need to retry.
SWIFT_RUNTIME_STDLIB_INTERNAL
int _swift_stdlib_FilePath_resolve(
    const _swift_filepath_codeunit *bytes, __swift_size_t count,
    _swift_filepath_codeunit *_Nullable *_Nonnull outBuf,
    __swift_size_t *outCount);

#ifdef __cplusplus
} // extern "C"
#endif

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif

#endif // SWIFT_STDLIB_SHIMS_FILEPATH_H
