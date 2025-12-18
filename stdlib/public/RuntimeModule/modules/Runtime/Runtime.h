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

// Demangle the given raw name (supports Swift and C++)
// 
// Optionally an output buffer may be passed into which the demangled string will be written.
// If null is passed as the 'outputBuffer' the runtime function will allocate a buffer and return it.
// If an 'outputBuffer' is passed, the output will be written into it, and the same buffer will be returned from this
//
// The demangled result string is NOT null-terminated. 
// The demangled string length is indicated through the outputBufferSize parameter.
// 
// Currently supported flags:
//  - 0: 'default'
//  - 1: '_swift_backtrace_demangle compatible mode', uses SimplifiedUIDemangleOptions for formatting
//  - *: unsupported values, result in immediate demangling failure.

// Introduced in Swift 6.3.
char *_swift_runtime_demangle(const char *rawName,
                              size_t rawNameLength,
                              char *outputBuffer,
                              size_t *outputBufferSize,
                              size_t flags);

#ifdef __cplusplus
}
#endif

#endif // SWIFT_BACKTRACING_RUNTIME_H
