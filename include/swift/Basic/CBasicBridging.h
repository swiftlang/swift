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

#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#if __clang__
// Provide macros to temporarily suppress warning about the use of
// _Nullable and _Nonnull.
#define SWIFT_BEGIN_NULLABILITY_ANNOTATIONS                                    \
  _Pragma("clang diagnostic push")                                             \
      _Pragma("clang diagnostic ignored \"-Wnullability-extension\"")          \
          _Pragma("clang assume_nonnull begin")

#define SWIFT_END_NULLABILITY_ANNOTATIONS                                      \
  _Pragma("clang diagnostic pop") _Pragma("clang assume_nonnull end")
#else
#define SWIFT_BEGIN_NULLABILITY_ANNOTATIONS
#define SWIFT_END_NULLABILITY_ANNOTATIONS
#define _Nullable
#define _Nonnull
#endif

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

#ifdef __cplusplus
extern "C" {
#endif

typedef struct BridgedData {
  const char *_Nullable baseAddress;
  size_t size;
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

bool JSON_value_getAsNull(void *valuePtr);
bool JSON_value_getAsBoolean(void *valuePtr, bool *result);
bool JSON_value_getAsString(void *valuePtr, BridgedData *result);
bool JSON_value_getAsDouble(void *valuePtr, double *result);
bool JSON_value_getAsInteger(void *valuePtr, int64_t *result);
bool JSON_value_getAsObject(void *valuePtr, void *_Nullable *_Nonnull result);
bool JSON_value_getAsArray(void *valuePtr, void *_Nullable *_Nonnull result);

size_t JSON_object_getSize(void *objectPtr);
BridgedData JSON_object_getKey(void *objectPtr, size_t i);
bool JSON_object_hasKey(void *objectPtr, const char *key);
void *JSON_object_getValue(void *objectPtr, const char *key);

int64_t JSON_array_getSize(void *arrayPtr);
void *JSON_array_getValue(void *arrayPtr, int64_t index);

void JSON_value_emplaceNull(void *valuePtr);
void JSON_value_emplaceBoolean(void *valuePtr, bool value);
void JSON_value_emplaceString(void *valuePtr, const char *value);
void JSON_value_emplaceDouble(void *valuePtr, double value);
void JSON_value_emplaceInteger(void *valuePtr, int64_t value);
void *JSON_value_emplaceNewObject(void *valuePtr);
void *JSON_value_emplaceNewArray(void *valuePtr);

void JSON_object_setNull(void *objectPtr, const char *key);
void JSON_object_setBoolean(void *objectPtr, const char *key, bool value);
void JSON_object_setString(void *objectPtr, const char *key, const char *value);
void JSON_object_setDouble(void *objectPtr, const char *key, double value);
void JSON_object_setInteger(void *objectPtr, const char *key, int64_t value);
void *JSON_object_setNewObject(void *objectPtr, const char *key);
void *JSON_object_setNewArray(void *objectPtr, const char *key);
void *JSON_object_setNewValue(void *objectPtr, const char *key);

void JSON_array_pushNull(void *arrayPtr);
void JSON_array_pushBoolean(void *arrayPtr, bool value);
void JSON_array_pushString(void *arrayPtr, const char *value);
void JSON_array_pushDouble(void *arrayPtr, double value);
void JSON_array_pushInteger(void *arrayPtr, int64_t value);
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
