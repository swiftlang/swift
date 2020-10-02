//===------------------------------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_STDLIB_SHIMS_ASSERTIONREPORTING_H_
#define SWIFT_STDLIB_SHIMS_ASSERTIONREPORTING_H_

#include "SwiftStdint.h"
#include "Visibility.h"

#if __has_feature(nullability)
#pragma clang assume_nonnull begin
#endif

#ifdef __cplusplus
extern "C" {
#endif

/// Report a fatal error to system console, stderr, and crash logs.
///
///     <prefix>: <message>: file <file>, line <line>\n
///
/// The message may be omitted by passing messageLength=0.
SWIFT_RUNTIME_STDLIB_API
void _swift_stdlib_reportFatalErrorInFile(
    const unsigned char *prefix, int prefixLength,
    const unsigned char *message, int messageLength,
    const unsigned char *file, int fileLength,
    __swift_uint32_t line,
    __swift_uint32_t flags);

/// Report a fatal error to system console, stderr, and crash logs.
///
///     <prefix>: <message>\n
SWIFT_RUNTIME_STDLIB_API
void _swift_stdlib_reportFatalError(
    const unsigned char *prefix, int prefixLength,
    const unsigned char *message, int messageLength,
    __swift_uint32_t flags);

/// Report a call to an unimplemented initializer.
///
///     <file>: <line>: <column>: fatal error: use of unimplemented
///     initializer '<initName>' for class '<className>'
SWIFT_RUNTIME_STDLIB_API
void _swift_stdlib_reportUnimplementedInitializerInFile(
    const unsigned char *className, int classNameLength,
    const unsigned char *initName, int initNameLength,
    const unsigned char *file, int fileLength,
    __swift_uint32_t line, __swift_uint32_t column,
    __swift_uint32_t flags);

/// Report a call to an unimplemented initializer.
///
///     fatal error: use of unimplemented initializer '<initName>'
///     for class 'className'
SWIFT_RUNTIME_STDLIB_API
void _swift_stdlib_reportUnimplementedInitializer(
    const unsigned char *className, int classNameLength,
    const unsigned char *initName, int initNameLength,
    __swift_uint32_t flags);

#ifdef __cplusplus
} // extern "C"
#endif

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif

#endif // SWIFT_STDLIB_SHIMS_ASSERTIONREPORTING_H_

