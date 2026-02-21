//===--- Runtime.h - Swift runtime imports ----------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Things to drag in from the Swift runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BACKTRACING_RUNTIME_H
#define SWIFT_BACKTRACING_RUNTIME_H

#include <stdbool.h>
#include <stdlib.h>

#include "swift/Runtime/CrashInfo.h"

#ifdef __cplusplus
extern "C" {
#endif

// Can't import swift/Runtime/Debug.h because it assumes C++
void swift_reportWarning(uint32_t flags, const char *message);

// Returns true if the given function is a thunk function
bool _swift_backtrace_isThunkFunction(const char *rawName);

// Demangle the given raw name (supports Swift and C++)
char *_swift_backtrace_demangle(const char *rawName,
                                size_t rawNameLength,
                                char *outputBuffer,
                                size_t *outputBufferSize);

// Demangle the given Swift mangled identifier.
//
// An output buffer must be passed into which the demangled string will be written.
//
// The demangled result string is NOT null-terminated.
// The demangled string length is indicated through the outputBufferSize parameter.
//
// If the demangled result is truncated, the returned number will be greater than the
// initialized count written into the outputBufferSize.
//
// Introduced in Swift 6.3.
size_t _swift_runtime_demangle(
  const char *rawName, size_t rawNameLength,
  char *outputBuffer, size_t *outputBufferSize,
  size_t flags
);

// Demangle the given Swift mangled identifier.
// This function always allocates a new buffer for the result to be returned.

// The demangled result string is NOT null-terminated.
// The demangled string length is indicated through the outputBufferSize parameter.
//
// Introduced in Swift 6.3.
char *_swift_runtime_demangle_allocate(
  const char *rawName, size_t rawNameLength,
  size_t *outputBufferSize,
  size_t flags
);

#ifdef __cplusplus
}
#endif

#endif // SWIFT_BACKTRACING_RUNTIME_H
