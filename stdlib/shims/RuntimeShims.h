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
#endif

unsigned char _swift_isUniquelyReferencedNonObjC(const void *);
unsigned char _swift_isUniquelyReferencedNonObjC_nonNull(const void *);
unsigned char _swift_usesNativeSwiftReferenceCounting_nonNull(const void *);
unsigned char _swift_usesNativeSwiftReferenceCounting_class(const void *);
unsigned char _swift_isUniquelyReferenced_native_spareBits(__swift_uintptr_t bits);
unsigned char
_swift_isUniquelyReferenced_nonNull_native(const struct HeapObject *);
unsigned char _swift_isUniquelyReferenced_native(const struct HeapObject *);
__swift_size_t _swift_class_getInstanceSize_class(const void *);

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#endif // SWIFT_STDLIB_SHIMS_RUNTIMESHIMS_H

