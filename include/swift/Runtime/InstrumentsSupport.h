//===--- InstrumentsSupport.h - Support for Instruments.app -----*- C++ -*-===//
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
// Swift runtime support for instruments.app
// In the long run, they plan to use dyld to make this indirection go away.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_INSTRUMENTS_SUPPORT_H
#define SWIFT_RUNTIME_INSTRUMENTS_SUPPORT_H

namespace swift {

SWIFT_RUNTIME_EXPORT
HeapObject *(*_swift_allocObject)(HeapMetadata const *metadata,
                                             size_t requiredSize,
                                             size_t requiredAlignmentMask);

SWIFT_RUNTIME_EXPORT
BoxPair::Return (*_swift_allocBox)(Metadata const *type);

SWIFT_RUNTIME_EXPORT
void (*_swift_retain)(HeapObject *object);
SWIFT_RUNTIME_EXPORT
void (*_swift_retain_n)(HeapObject *object, uint32_t n);
SWIFT_RUNTIME_EXPORT
void (*_swift_nonatomic_retain)(HeapObject *object);
SWIFT_RUNTIME_EXPORT
HeapObject *(*_swift_tryRetain)(HeapObject *object);
SWIFT_RUNTIME_EXPORT
bool (*_swift_isDeallocating)(HeapObject *object);
SWIFT_RUNTIME_EXPORT
void (*_swift_release)(HeapObject *object);
SWIFT_RUNTIME_EXPORT
void (*_swift_release_n)(HeapObject *object, uint32_t n);
SWIFT_RUNTIME_EXPORT
void (*_swift_nonatomic_release)(HeapObject *object);

// liboainject on iOS 8 patches the function pointers below if present. 
// Do not reuse these names unless you do what oainject expects you to do.
typedef size_t AllocIndex;
SWIFT_RUNTIME_EXPORT
void *(*_swift_alloc)(AllocIndex idx);
SWIFT_RUNTIME_EXPORT
void *(*_swift_tryAlloc)(AllocIndex idx);
SWIFT_RUNTIME_EXPORT
void *(*_swift_slowAlloc)(size_t bytes, size_t alignMask,
                                     uintptr_t flags);
SWIFT_RUNTIME_EXPORT
void (*_swift_dealloc)(void *ptr, AllocIndex idx);
SWIFT_RUNTIME_EXPORT
void (*_swift_slowDealloc)(void *ptr, size_t bytes, size_t alignMask);
SWIFT_RUNTIME_EXPORT
size_t _swift_indexToSize(AllocIndex idx);
SWIFT_RUNTIME_EXPORT
int _swift_sizeToIndex(size_t size);
SWIFT_RUNTIME_EXPORT
void _swift_zone_init(void);

};

#endif
