//===--- Backtrace.cpp - Swift crash catching and backtracing support ---- ===//
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
// Definitions relating to backtracing.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_BACKTRACE_H
#define SWIFT_RUNTIME_BACKTRACE_H

#ifdef __linux__
#include <sys/types.h>
#include <sys/wait.h>

#include <signal.h>
#endif // defined(__linux__)

#include "swift/Runtime/Config.h"
#include "swift/Runtime/CrashInfo.h"

#include "swift/shims/Visibility.h"

#include <inttypes.h>

#ifdef __cplusplus
namespace swift {
namespace runtime {
namespace backtrace {
#endif

// Test if a given function is a Swift thunk of some sort.
//
// @param mangledName is the name of the symbol to test.
//
// @returns true if the function is a thunk.
SWIFT_RUNTIME_STDLIB_SPI
bool _swift_backtrace_isThunkFunction(const char *mangledName);

/// Try to demangle a symbol.
///
/// Unlike other entry points that do this, we try both Swift and C++ here.
///
/// @param mangledName is the symbol name to be demangled.
/// @param mangledNameLength is the length of this name.
/// @param outputBuffer is a pointer to a buffer in which to place the result.
/// @param outputBufferSize points to a variable that contains the size of the
/// output buffer.
///
/// If outputBuffer is nullptr, the function will allocate memory for the
/// result using malloc().  In this case, outputBufferSize may be nullptr;
/// if it is *not* nullptr, it will be set to the size of buffer that was
/// allocated.  This is not necessarily the length of the string (it may be
/// somewhat higher).
///
/// Otherwise, the result will be written into the output buffer, and the
/// size of the result will be written into outputBufferSize.  If the buffer
/// is too small, the result will be truncated, but outputBufferSize will
/// still be set to the number of bytes that would have been required to
/// copy out the full result (including a trailing NUL).
///
/// The unusual behaviour here is a consequence of the way the C++ ABI's
/// demangling function works.
///
/// @returns a pointer to the output if demangling was successful.
SWIFT_RUNTIME_STDLIB_SPI
char *_swift_backtrace_demangle(const char *mangledName,
                                size_t mangledNameLength,
                                char *outputBuffer,
                                size_t *outputBufferSize);
#ifdef __cplusplus
} // namespace backtrace
} // namespace runtime
} // namespace swift
#endif

#endif // SWIFT_RUNTIME_BACKTRACE_H
