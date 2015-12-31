//===--- ObjCBridge.h - Swift Language Objective-C Bridging ABI -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift ABI for interacting with Objective-C.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_OBJCBRIDGE_H
#define SWIFT_ABI_OBJCBRIDGE_H

#include "swift/Runtime/Config.h"
#include <cstdint>

struct objc_class;

namespace swift {

struct Metadata;
struct HeapObject;

} // end namespace swift

#if SWIFT_OBJC_INTEROP
#include <objc/objc.h>

// Redeclare APIs from the Objective-C runtime.
// These functions are not available through public headers, but are guaranteed
// to exist on OS X >= 10.9 and iOS >= 7.0.

extern "C" id objc_retain(id);
extern "C" void objc_release(id);
extern "C" id _objc_rootAutorelease(id);
extern "C" void objc_moveWeak(id*, id*);
extern "C" void objc_copyWeak(id*, id*);
extern "C" id objc_initWeak(id*, id);
extern "C" id objc_storeWeak(id*, id);
extern "C" void objc_destroyWeak(id*);
extern "C" id objc_loadWeakRetained(id*);

// Description of an Objective-C image.
// __DATA,__objc_imageinfo stores one of these.
typedef struct objc_image_info {
    uint32_t version; // currently 0
    uint32_t flags;
} objc_image_info;

// Class and metaclass construction from a compiler-generated memory image.
// cls and cls->isa must each be OBJC_MAX_CLASS_SIZE bytes.·
// Extra bytes not used the metadata must be zero.
// info is the same objc_image_info that would be emitted by a static compiler.
// Returns nil if a class with the same name already exists.
// Returns nil if the superclass is nil and the class is not marked as a root.
// Returns nil if the superclass is under construction.
// Do not call objc_registerClassPair().
OBJC_EXPORT Class objc_readClassPair(Class cls,
                                     const struct objc_image_info *info)
    __OSX_AVAILABLE_STARTING(__MAC_10_10, __IPHONE_8_0);


namespace swift {

// Root -dealloc implementation for classes with Swift reference counting.
// This function should be used to implement -dealloc in a root class with
// Swift reference counting. [super dealloc] MUST NOT be called after this,
// for the object will have already been deallocated by the time
// this function returns.
extern "C" void swift_rootObjCDealloc(HeapObject *self);

}

#endif /* SWIFT_OBJC_INTEROP */

#endif /* SWIFT_ABI_OBJCBRIDGE_H */
