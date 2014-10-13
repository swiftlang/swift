//===--- InstrumentsSupport.h - Support for Instruments.app ------ C++ -*--===//
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
// Swift runtime support for instruments.app
// In the long run, they plan to use dyld to make this indirection go away.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_INSTRUMENTS_SUPPORT_H
#define SWIFT_RUNTIME_INSTRUMENTS_SUPPORT_H

namespace swift {

extern "C" HeapObject *(*_swift_allocObject)(HeapMetadata const *metadata,
                                             size_t requiredSize,
                                             size_t requiredAlignmentMask);

extern "C" BoxPair::Return (*_swift_allocBox)(Metadata const *type);

extern "C" HeapObject *(*_swift_retain)(HeapObject *object);
extern "C" HeapObject *(*_swift_tryRetain)(HeapObject *object);
extern "C" bool (*_swift_isDeallocating)(HeapObject *object);
extern "C" void (*_swift_release)(HeapObject *object);


// liboainject on iOS 8 patches the function pointers below if present. 
// Do not reuse these names unless you do what oainject expects you to do.
typedef size_t AllocIndex;
extern "C" void *(*_swift_alloc)(AllocIndex idx);
extern "C" void *(*_swift_tryAlloc)(AllocIndex idx);
extern "C" void *(*_swift_slowAlloc)(size_t bytes, size_t alignMask,
                                     uintptr_t flags);
extern "C" void (*_swift_dealloc)(void *ptr, AllocIndex idx);
extern "C" void (*_swift_slowDealloc)(void *ptr, size_t bytes, size_t alignMask);
extern "C" size_t _swift_indexToSize(AllocIndex idx);
extern "C" int _swift_sizeToIndex(size_t size);
extern "C" void _swift_zone_init(void);

};

#endif
