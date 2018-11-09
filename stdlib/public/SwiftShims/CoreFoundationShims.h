//===--- CoreFoundationShims.h - Access to CF for the core stdlib ---------===//
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
//  Using the CoreFoundation module in the core stdlib would create a
//  circular dependency, so instead we import these declarations as
//  part of SwiftShims.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_COREFOUNDATIONSHIMS_H
#define SWIFT_STDLIB_SHIMS_COREFOUNDATIONSHIMS_H

#include "SwiftStdint.h"
#include "Visibility.h"

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

#ifdef __OBJC2__
#if __LLP64__ || _GLIBCXX_LLP64
typedef unsigned long long _swift_shims_CFTypeID;
typedef unsigned long long _swift_shims_CFOptionFlags;
typedef unsigned long long _swift_shims_CFHashCode;
typedef signed long long _swift_shims_CFIndex;
#else
typedef unsigned long _swift_shims_CFTypeID;
typedef unsigned long _swift_shims_CFOptionFlags;
typedef unsigned long _swift_shims_CFHashCode;
typedef signed long _swift_shims_CFIndex;
#endif

typedef struct {
  _swift_shims_CFIndex location;
  _swift_shims_CFIndex length;
} _swift_shims_CFRange;

typedef id _swift_shims_CFStringRef;
typedef const struct _swift_shims_CFAllocator *_swift_shims_CFAllocatorRef;
typedef __swift_uint32_t _swift_shims_CFStringEncoding;
typedef _swift_shims_CFOptionFlags _swift_shims_CFStringCompareFlags;
typedef _swift_shims_CFIndex _swift_shims_CFComparisonResult;

// Consider creating SwiftMacTypes.h for these
typedef unsigned char _swift_shims_Boolean;
typedef __swift_uint16_t _swift_shims_UniChar;
typedef __swift_uint8_t _swift_shims_UInt8;

// Buffer is nullable in case the string is zero-length.
SWIFT_RUNTIME_STDLIB_API
void _swift_stdlib_CFStringGetCharacters(
    _swift_shims_CFStringRef _Nonnull theString, _swift_shims_CFRange range,
    _swift_shims_UniChar *_Nullable buffer);

SWIFT_RUNTIME_STDLIB_API
_swift_shims_CFIndex _swift_stdlib_CFStringGetBytes(
    _swift_shims_CFStringRef _Nonnull theString, _swift_shims_CFRange range,
    _swift_shims_CFStringEncoding encoding, _swift_shims_UInt8 lossByte,
    _swift_shims_Boolean isExternalRepresentation,
    _swift_shims_UInt8 *_Nonnull buffer, _swift_shims_CFIndex maxBufLen,
    _swift_shims_CFIndex *_Nullable usedBufLen);

SWIFT_RUNTIME_STDLIB_API
const _swift_shims_UniChar *_Nullable _swift_stdlib_CFStringGetCharactersPtr(
    _swift_shims_CFStringRef _Nonnull theString);

SWIFT_RUNTIME_STDLIB_API
_swift_shims_CFIndex _swift_stdlib_CFStringGetLength(
    _swift_shims_CFStringRef _Nonnull theString);

SWIFT_RUNTIME_STDLIB_API
__attribute__((ns_returns_retained))
_swift_shims_CFStringRef _Nonnull _swift_stdlib_CFStringCreateWithSubstring(
    _swift_shims_CFAllocatorRef _Nullable alloc,
    _swift_shims_CFStringRef _Nonnull str, _swift_shims_CFRange range);

SWIFT_RUNTIME_STDLIB_API
_swift_shims_UniChar _swift_stdlib_CFStringGetCharacterAtIndex(
    _swift_shims_CFStringRef _Nonnull theString, _swift_shims_CFIndex idx);

SWIFT_RUNTIME_STDLIB_API
__attribute__((ns_returns_retained))
_swift_shims_CFStringRef _Nonnull _swift_stdlib_CFStringCreateCopy(
    _swift_shims_CFAllocatorRef _Nullable alloc,
    _swift_shims_CFStringRef _Nonnull theString);

SWIFT_RUNTIME_STDLIB_API
__attribute__((ns_returns_retained))
_swift_shims_CFStringRef _Nonnull _swift_stdlib_CFStringCreateWithBytes(
    _swift_shims_CFAllocatorRef _Nullable alloc,
    const __swift_uint8_t *_Nonnull bytes, _swift_shims_CFIndex numBytes,
    _swift_shims_CFStringEncoding encoding,
    _swift_shims_Boolean isExternalRepresentation);

SWIFT_RUNTIME_STDLIB_API
const char *_Nullable _swift_stdlib_CFStringGetCStringPtr(
    _swift_shims_CFStringRef _Nonnull theString,
    _swift_shims_CFStringEncoding encoding);

SWIFT_RUNTIME_STDLIB_API
_swift_shims_CFStringRef _Nonnull _swift_stdlib_objcDebugDescription(
    id _Nonnull nsObject);
  
SWIFT_RUNTIME_STDLIB_API
_swift_shims_CFComparisonResult _swift_stdlib_CFStringCompare(
    _swift_shims_CFStringRef _Nonnull string,
    _swift_shims_CFStringRef _Nonnull string2);
  
SWIFT_RUNTIME_STDLIB_API
__swift_uint8_t _swift_stdlib_isNSString(id _Nonnull obj);

SWIFT_RUNTIME_STDLIB_API
_swift_shims_CFHashCode _swift_stdlib_CFStringHashNSString(id _Nonnull obj);

SWIFT_RUNTIME_STDLIB_API
_swift_shims_CFHashCode
_swift_stdlib_CFStringHashCString(const _swift_shims_UInt8 * _Nonnull bytes,
                                  _swift_shims_CFIndex length);
  
#endif // __OBJC2__

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#endif // SWIFT_STDLIB_SHIMS_COREFOUNDATIONSHIMS_H

