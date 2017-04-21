//===--- ImageInspectionShims.h ---------------------------------*- C++ -*-===//
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
// Abstraction of libdl functions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_IMAGEINSPECTIONSHIMS_H
#define SWIFT_STDLIB_SHIMS_IMAGEINSPECTIONSHIMS_H

#include "Visibility.h"
#include <dlfcn.h>

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

// A direct interface to dlerror() is not supported, instead an error argument
// is passed. If the error argument is passed non-NULL and an error string is
// set by the callee (from dlerror()), then it should be passed to free() by
// the caller after use.
SWIFT_RUNTIME_STDLIB_INTERFACE
void * _Nullable _swift_stdlib_dlopen(const char * _Nullable filename,
                                      int flags,
                                      char * _Nullable * _Nonnull error);

SWIFT_RUNTIME_STDLIB_INTERFACE
int _swift_stdlib_dlclose(void * _Nonnull handle,
                          char * _Nullable * _Nonnull error);

SWIFT_RUNTIME_STDLIB_INTERFACE
void * _Nullable _swift_stdlib_dlsym(void * _Nullable handle,
                                     const char * _Nonnull symbol,
                                     char * _Nullable * _Nonnull error);

SWIFT_RUNTIME_STDLIB_INTERFACE
int _swift_stdlib_dladdr(void * _Nonnull addr, Dl_info * _Nonnull info);

SWIFT_END_NULLABILITY_ANNOTATIONS

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#endif // SWIFT_STDLIB_SHIMS_IMAGEINSPECTIONSHIMS_H
