//===--- RuntimeShims.h - Access to runtime facilities for the core -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Runtime functions and structures needed by the core stdlib are
//  declared here.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_RUNTIMESHIMS_H
#define SWIFT_STDLIB_SHIMS_RUNTIMESHIMS_H

#include "SwiftStddef.h"
#include "SwiftStdint.h"

#ifdef __cplusplus
namespace swift { extern "C" {
#else
#define bool _Bool
#endif

/// Is this pointer a non-null unique reference to an object
/// that uses Swift reference counting?
bool _swift_isUniquelyReferencedNonObjC(const void *);

/// Is this non-null pointer a unique reference to an object
/// that uses Swift reference counting?
bool _swift_isUniquelyReferencedNonObjC_nonNull(const void *);

/// Is this non-null pointer a reference to an object that uses Swift
/// reference counting and is either uniquely referenced or pinned?
bool _swift_isUniquelyReferencedOrPinnedNonObjC_nonNull(const void *);

/// Is this non-null BridgeObject a unique reference to an object
/// that uses Swift reference counting?
bool _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
  __swift_uintptr_t bits);

/// Is this non-null BridgeObject a unique or pinned reference to an
/// object that uses Swift reference counting?
bool _swift_isUniquelyReferencedOrPinnedNonObjC_nonNull_bridgeObject(
  __swift_uintptr_t bits);

/// Is this native Swift pointer a non-null unique reference to
/// an object?
bool _swift_isUniquelyReferenced_native(const struct HeapObject *);

/// Is this native Swift pointer a non-null unique or pinned reference
/// to an object?
bool _swift_isUniquelyReferencedOrPinned_native(const struct HeapObject *);

/// Is this non-null native Swift pointer a unique reference to
/// an object?
bool _swift_isUniquelyReferenced_nonNull_native(const struct HeapObject *);

/// Does this non-null native Swift pointer refer to an object that
/// is either uniquely referenced or pinned?
bool _swift_isUniquelyReferencedOrPinned_nonNull_native(
                                                const struct HeapObject *);

bool _swift_usesNativeSwiftReferenceCounting_nonNull(const void *);
bool _swift_usesNativeSwiftReferenceCounting_class(const void *);

__swift_size_t _swift_class_getInstancePositiveExtentSize(const void *);

/// Return an NSString to be used as the Mirror summary of the object
void *_swift_objCMirrorSummary(const void * nsObject);

/// Call strtold_l with the C locale, swapping argument and return
/// types so we can operate on Float80.  Return NULL on overflow.
const char *_swift_stdlib_strtold_clocale(const char *nptr, void *outResult);
/// Call strtod_l with the C locale, swapping argument and return
/// types so we can operate constistently on Float80.  Return NULL on
/// overflow.
const char *_swift_stdlib_strtod_clocale(const char *nptr, double *outResult);
/// Call strtof_l with the C locale, swapping argument and return
/// types so we can operate constistently on Float80.  Return NULL on
/// overflow.
const char *_swift_stdlib_strtof_clocale(const char *nptr, float *outResult);

struct Metadata;
  
/// Return the superclass, if any.  The result is nullptr for root
/// classes and class protocol types.
const struct Metadata *_swift_getSuperclass_nonNull(
  const struct Metadata *);
  
void _swift_stdlib_flockfile_stdout(void);
void _swift_stdlib_funlockfile_stdout(void);

int _swift_stdlib_putc_stderr(int C);

__swift_size_t _swift_stdlib_getHardwareConcurrency();

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#endif // SWIFT_STDLIB_SHIMS_RUNTIMESHIMS_H

