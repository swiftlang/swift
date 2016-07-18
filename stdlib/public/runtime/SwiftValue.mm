//===--- SwiftValue.mm - Boxed Swift value class --------------------------===//
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
// This implements the Objective-C class that is used to carry Swift values
// that have been bridged to Objective-C objects without special handling.
// The class is opaque to user code, but is `NSObject`- and `NSCopying`-
// conforming and is understood by the Swift runtime for dynamic casting
// back to the contained type.
//
//===----------------------------------------------------------------------===//

#include "SwiftObject.h"
#include "SwiftValue.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/ObjCBridge.h"
#include "swift/Runtime/Debug.h"
#include "Private.h"
#include <objc/runtime.h>
#include <Foundation/Foundation.h>

#if !SWIFT_OBJC_INTEROP
#error "This file should only be compiled when ObjC interop is enabled."
#endif

using namespace swift;

// TODO: Making this a SwiftObject subclass would let us use Swift refcounting,
// but we would need to be able to emit SwiftValue's Objective-C class object
// with the Swift destructor pointer prefixed before it.
@interface SwiftValue : NSObject <NSCopying>

- (id)copyWithZone:(NSZone *)zone;

@end

static constexpr const size_t SwiftValueMetadataOffset
  = sizeof(Class); // isa pointer
static constexpr const size_t SwiftValueMinAlignMask
  = alignof(Class) - 1;
/* TODO: If we're able to become a SwiftObject subclass in the future,
 * change to this:
static constexpr const size_t SwiftValueMetadataOffset
  = sizeof(SwiftObject_s);
static constexpr const size_t SwiftValueMinAlignMask
  = alignof(SwiftObject_s) - 1;
 */

static Class _getSwiftValueClass() {
  auto theClass = [SwiftValue class];
  // Fixed instance size of SwiftValue should be same as object header.
  assert(class_getInstanceSize(theClass) == SwiftValueMetadataOffset
         && "unexpected size of SwiftValue?!");
  return theClass;
}

static Class getSwiftValueClass() {
  return SWIFT_LAZY_CONSTANT(_getSwiftValueClass());
}

static constexpr size_t getSwiftValueOffset(size_t alignMask) {
  return SwiftValueMetadataOffset + sizeof(const Metadata *)
    + alignMask & ~alignMask;
}

const Metadata *swift::getSwiftValueTypeMetadata(SwiftValue *v) {
  auto instanceBytes = reinterpret_cast<const char*>(v);
  const Metadata *result;
  memcpy(&result, instanceBytes + SwiftValueMetadataOffset,
         sizeof(result));
  return result;
}

std::pair<const Metadata *, const OpaqueValue *>
swift::getValueFromSwiftValue(SwiftValue *v) {
  auto instanceBytes = reinterpret_cast<const char*>(v);
  auto instanceType = getSwiftValueTypeMetadata(v);
  size_t alignMask = instanceType->getValueWitnesses()->getAlignmentMask()
    | SwiftValueMinAlignMask;
  auto instanceOffset = getSwiftValueOffset(alignMask);
  auto value = reinterpret_cast<const OpaqueValue *>(
    instanceBytes + instanceOffset);
  return {instanceType, value};
}

SwiftValue *swift::bridgeAnythingToSwiftValueObject(OpaqueValue *src,
                                                    const Metadata *srcType,
                                                    bool consume) {
  Class SwiftValueClass = getSwiftValueClass();
  
  // We lay out the metadata after the object header, and the value after
  // the metadata (rounded up to alignment).
  size_t alignMask = srcType->getValueWitnesses()->getAlignmentMask()
                   | SwiftValueMinAlignMask;
  size_t valueOffset = SwiftValueMetadataOffset + sizeof(const Metadata *)
    + alignMask & ~alignMask;
  
  size_t totalSize = valueOffset + srcType->getValueWitnesses()->size;
  
  void *instanceMemory = swift_slowAlloc(totalSize, alignMask);
  SwiftValue *instance
    = objc_constructInstance(SwiftValueClass, instanceMemory);
  /* TODO: If we're able to become a SwiftObject subclass in the future,
   * change to this:
  auto instance = swift_allocObject(SwiftValueClass, totalSize, alignMask);
   */
  
  auto instanceBytes = reinterpret_cast<char*>(instance);
  memcpy(instanceBytes + SwiftValueMetadataOffset, &srcType,
         sizeof(const Metadata*));
  auto instanceValue = reinterpret_cast<OpaqueValue *>(
    instanceBytes + valueOffset);
  
  if (consume)
    srcType->vw_initializeWithTake(instanceValue, src);
  else
    srcType->vw_initializeWithCopy(instanceValue, src);
  
  return instance;
}

SwiftValue *swift::getAsSwiftValue(id object) {
  // SwiftValue should have no subclasses or proxies. We can do an exact
  // class check.
  if (object_getClass(object) == getSwiftValueClass())
    return object;
  return nil;
}

@implementation SwiftValue

+ (instancetype)allocWithZone:(NSZone *)zone {
  swift::crash("SwiftValue cannot be instantiated");
}

- (id)copyWithZone:(NSZone *)zone {
  // Instances are immutable, so we can just retain.
  return objc_retain(self);
  
  /* TODO: If we're able to become a SwiftObject subclass in the future,
   * change to this:
   swift_retain((HeapObject*)self);
   return self;
   */
}

// Since we allocate using Swift's allocator to properly handle alignment,
// we need to deallocate ourselves instead of delegating to
// -[NSObject dealloc].
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wobjc-missing-super-calls"
- (void)dealloc {
  // TODO: If we're able to become a SwiftObject subclass in the future,
  // this should move to the heap metadata destructor function.
  
  // Destroy the contained value.
  auto instanceBytes = reinterpret_cast<char*>(self);
  auto instanceType = getSwiftValueTypeMetadata(self);
  size_t alignMask = instanceType->getValueWitnesses()->getAlignmentMask()
    | SwiftValueMinAlignMask;
  auto instanceOffset = getSwiftValueOffset(alignMask);
  auto value = reinterpret_cast<OpaqueValue *>(
    instanceBytes + instanceOffset);
  instanceType->vw_destroy(value);
  
  // Deallocate ourselves.
  objc_destructInstance(self);
  auto totalSize = instanceOffset = instanceType->getValueWitnesses()->size;
  swift_slowDealloc(self, totalSize, alignMask);
}
#pragma clang diagnostic pop

// Private methods for debugging purposes.

- (const Metadata *)_swiftTypeMetadata {
  return getSwiftValueTypeMetadata(self);
}
- (const OpaqueValue *)_swiftValue {
  return getValueFromSwiftValue(self).second;
}

// TODO: Forward description, debugDescription, isEqual:, hash, etc. to
// corresponding operations on the boxed Swift type.

@end

// TODO: We could pick specialized SwiftValue subclasses for trivial types
// or for types with known size and alignment characteristics. Probably
// not enough of a real perf bottleneck to be worth it...
