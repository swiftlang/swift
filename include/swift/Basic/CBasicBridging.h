//===--- CBasicBridging.h - header for the swift SILBridging module -------===//
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

#ifndef SWIFT_C_BASIC_BASICBRIDGING_H
#define SWIFT_C_BASIC_BASICBRIDGING_H

#include "swift/Basic/Compiler.h"

// NOTE: DO NOT #include any stdlib headers here. e.g. <stdint.h>. Those are
// part of "Darwin"/"Glibc" module, so when a Swift file imports this header,
// it causes importing the "Darwin"/"Glibc" overlay module. That violates
// layering. i.e. Darwin overlay is created by Swift compiler.

// NOTE: Partially ported from SwiftShim's SwiftStdint.h. We cannot include
// that header here because it belongs to the runtime, but we need the same
// logic for interoperability with Swift code in the compiler itself.
// stdint.h is provided by Clang, but it dispatches to libc's stdint.h.  As a
// result, using stdint.h here would pull in Darwin module (which includes
// libc). This creates a dependency cycle, so we can't use stdint.h in
// SwiftShims.
// On Linux, the story is different. We get the error message
// "/usr/include/x86_64-linux-gnu/sys/types.h:146:10: error: 'stddef.h' file not
// found"
// This is a known Clang/Ubuntu bug.

// Clang has been defining __INTxx_TYPE__ macros for a long time.
// __UINTxx_TYPE__ are defined only since Clang 3.5.
#if defined(_MSC_VER) && !defined(__clang__)
typedef __int64 __swiftc_int64_t;
typedef unsigned __int64 __swiftc_uint64_t;
typedef int __swiftc_int32_t;
typedef unsigned int __swiftc_uint32_t;
#elif !defined(__APPLE__) && !defined(__linux__) && !defined(__OpenBSD__) && !defined(__wasi__)
#include <stdint.h>
typedef int64_t __swiftc_int64_t;
typedef uint64_t __swiftc_uint64_t;
typedef int32_t __swiftc_int32_t;
typedef uint32_t __swiftc_uint32_t;
typedef intptr_t __swiftc_intptr_t;
typedef uintptr_t __swiftc_uintptr_t;
#else
typedef __INT64_TYPE__ __swiftc_int64_t;
#ifdef __UINT64_TYPE__
typedef __UINT64_TYPE__ __swiftc_uint64_t;
#else
typedef unsigned __INT64_TYPE__ __swiftc_uint64_t;
#endif

typedef __INT32_TYPE__ __swiftc_int32_t;
#ifdef __UINT32_TYPE__
typedef __UINT32_TYPE__ __swiftc_uint32_t;
#else
typedef unsigned __INT32_TYPE__ __swiftc_uint32_t;
#endif

#define __swiftc_join3(a,b,c) a ## b ## c

#define __swiftc_intn_t(n) __swiftc_join3(__swiftc_int, n, _t)
#define __swiftc_uintn_t(n) __swiftc_join3(__swiftc_uint, n, _t)

#if defined(_MSC_VER) && !defined(__clang__)
#if defined(_WIN64)
typedef __swiftc_int64_t SwiftInt;
typedef __swiftc_uint64_t SwiftUInt;
#elif defined(_WIN32)
typedef __swiftc_int32_t SwiftInt;
typedef __swiftc_uint32_t SwiftUInt;
#else
#error unknown windows pointer width
#endif
#else
typedef __swiftc_intn_t(__INTPTR_WIDTH__) SwiftInt;
typedef __swiftc_uintn_t(__INTPTR_WIDTH__) SwiftUInt;
#endif
#endif

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

#ifdef __cplusplus
extern "C" {

#define _Bool bool

#endif

typedef enum ENUM_EXTENSIBILITY_ATTR(open) BridgedFeature {
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description, Option) \
FeatureName,
#include "swift/Basic/Features.def"
} BridgedFeature;

SWIFT_BEGIN_ASSUME_NONNULL

typedef struct BridgedData {
  const char *_Nullable baseAddress;
  SwiftUInt size;
} BridgedData;

void BridgedData_free(BridgedData data);

//===----------------------------------------------------------------------===//
// Plugins
//===----------------------------------------------------------------------===//

/// Create a new root 'null' JSON value. Clients must call \c JSON_value_delete
/// after using it.
void *JSON_newValue();

/// Parse \p data as a JSON data and return the top-level value. Clients must
/// call \c JSON_value_delete after using it.
void *JSON_deserializedValue(BridgedData data);

/// Serialize a value and populate \p result with the result data. Clients
/// must call \c BridgedData_free after using the \p result.
void JSON_value_serialize(void *valuePtr, BridgedData *result);

/// Destroy and release the memory for \p valuePtr that is a result from
/// \c JSON_newValue() or \c JSON_deserializedValue() .
void JSON_value_delete(void *valuePtr);

_Bool JSON_value_getAsNull(void *valuePtr);
_Bool JSON_value_getAsBoolean(void *valuePtr, _Bool *result);
_Bool JSON_value_getAsString(void *valuePtr, BridgedData *result);
_Bool JSON_value_getAsDouble(void *valuePtr, double *result);
_Bool JSON_value_getAsInteger(void *valuePtr, long long *result);
_Bool JSON_value_getAsObject(void *valuePtr, void *_Nullable *_Nonnull result);
_Bool JSON_value_getAsArray(void *valuePtr, void *_Nullable *_Nonnull result);

unsigned long JSON_object_getSize(void *objectPtr);
BridgedData JSON_object_getKey(void *objectPtr, unsigned long i);
_Bool JSON_object_hasKey(void *objectPtr, const char *key);
void *JSON_object_getValue(void *objectPtr, const char *key);

long long JSON_array_getSize(void *arrayPtr);
void *JSON_array_getValue(void *arrayPtr, long long index);

void JSON_value_emplaceNull(void *valuePtr);
void JSON_value_emplaceBoolean(void *valuePtr, _Bool value);
void JSON_value_emplaceString(void *valuePtr, const char *value);
void JSON_value_emplaceDouble(void *valuePtr, double value);
void JSON_value_emplaceInteger(void *valuePtr, long long value);
void *JSON_value_emplaceNewObject(void *valuePtr);
void *JSON_value_emplaceNewArray(void *valuePtr);

void JSON_object_setNull(void *objectPtr, const char *key);
void JSON_object_setBoolean(void *objectPtr, const char *key, _Bool value);
void JSON_object_setString(void *objectPtr, const char *key, const char *value);
void JSON_object_setDouble(void *objectPtr, const char *key, double value);
void JSON_object_setInteger(void *objectPtr, const char *key, long long value);
void *JSON_object_setNewObject(void *objectPtr, const char *key);
void *JSON_object_setNewArray(void *objectPtr, const char *key);
void *JSON_object_setNewValue(void *objectPtr, const char *key);

void JSON_array_pushNull(void *arrayPtr);
void JSON_array_pushBoolean(void *arrayPtr, _Bool value);
void JSON_array_pushString(void *arrayPtr, const char *value);
void JSON_array_pushDouble(void *arrayPtr, double value);
void JSON_array_pushInteger(void *arrayPtr, long long value);
void *JSON_array_pushNewObject(void *arrayPtr);
void *JSON_array_pushNewArray(void *arrayPtr);
void *JSON_array_pushNewValue(void *arrayPtr);

#ifdef __cplusplus
}
#endif

SWIFT_END_NULLABILITY_ANNOTATIONS

#undef SWIFT_BEGIN_NULLABILITY_ANNOTATIONS
#undef SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // SWIFT_C_BASIC_BASICBRIDGING_H
