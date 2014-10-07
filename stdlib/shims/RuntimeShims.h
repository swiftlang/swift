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
#ifndef SWIFT_STDLIB_SHIMS_RUNTIMESHIMS_H_
#define SWIFT_STDLIB_SHIMS_RUNTIMESHIMS_H_

#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

bool _swift_isUniquelyReferencedNative_nonNull(const void* object);
bool _swift_usesNativeSwiftReferenceCounting_nonNull(const void *object);
unsigned char _swift_isUniquelyReferenced(uintptr_t bits);

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#endif
