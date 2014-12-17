//===--- GlobalObjects.h - Statically-initialized objects -----------------===//
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
//  Objects that are allocated at global scope instead of on the heap,
//  and statically initialized to avoid synchronization costs, are
//  defined here.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_STDLIB_SHIMS_GLOBALOBJECTS_H_
#define SWIFT_STDLIB_SHIMS_GLOBALOBJECTS_H_

#include "SwiftStdint.h"
#include "HeapObject.h"

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

struct _SwiftArrayBodyStorage {
  __swift_intptr_t count;
  __swift_uintptr_t _capacityAndFlags;
};

struct _SwiftEmptyArrayStorage {
  struct HeapObject header;
  struct _SwiftArrayBodyStorage body;
};

extern struct _SwiftEmptyArrayStorage _swiftEmptyArrayStorage;

extern __swift_uint64_t _swift_stdlib_HashingDetail_fixedSeedOverride;

#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#endif
