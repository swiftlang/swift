//===--- SwiftValue.mm - Boxed Swift value class --------------------------===//
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
// This implements the Objective-C class that is used to carry Swift values
// that have been bridged to Objective-C objects without special handling.
// The class is opaque to user code, but is `NSObject`- and `NSCopying`-
// conforming and is understood by the Swift runtime for dynamic casting
// back to the contained type.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"

#if SWIFT_OBJC_INTEROP
#include "SwiftObject.h"
#include "SwiftValue.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/ObjCBridge.h"
#include "swift/Runtime/Debug.h"
#include "Private.h"
#include "SwiftHashableSupport.h"
#include <objc/runtime.h>
#include <Foundation/Foundation.h>

using namespace swift;
using namespace swift::hashable_support;

// TODO: Making this a SwiftObject subclass would let us use Swift refcounting,
// but we would need to be able to emit _SwiftValue's Objective-C class object
// with the Swift destructor pointer prefixed before it.
//
// The layout of `_SwiftValue` is:
// - object header,
// - `SwiftValueHeader` instance,
// - the payload, tail-allocated (the Swift value contained in this box).
@interface _SwiftValue : NSObject <NSCopying>

- (id)copyWithZone:(NSZone *)zone;

@end

/// The fixed-size ivars of `_SwiftValue`.  The actual boxed value is
/// tail-allocated.
struct SwiftValueHeader {
  /// The type of the value contained in the `_SwiftValue` box.
  const Metadata *type;

  /// The base type that introduces the `Hashable` conformance.
  /// This member is only available for native Swift errors.
  /// This member is lazily-initialized.
  /// Instead of using it directly, call `getHashableBaseType()`.
  mutable std::atomic<const Metadata *> hashableBaseType;

  /// The witness table for `Hashable` conformance.
  /// This member is only available for native Swift errors.
  /// This member is lazily-initialized.
  /// Instead of using it directly, call `getHashableConformance()`.
  mutable std::atomic<const hashable_support::HashableWitnessTable *>
      hashableConformance;

  /// Get the base type that conforms to `Hashable`.
  /// Returns NULL if the type does not conform.
  const Metadata *getHashableBaseType() const;

  /// Get the `Hashable` protocol witness table for the contained type.
  /// Returns NULL if the type does not conform.
  const hashable_support::HashableWitnessTable *getHashableConformance() const;

  SwiftValueHeader()
      : hashableBaseType(nullptr), hashableConformance(nullptr) {}
};

const Metadata *SwiftValueHeader::getHashableBaseType() const {
  if (auto type = hashableBaseType.load(std::memory_order_acquire)) {
    if (reinterpret_cast<uintptr_t>(type) == 1) {
      return nullptr;
    }
    return type;
  }

  const Metadata *expectedType = nullptr;
  const Metadata *hashableBaseType = findHashableBaseType(type);
  this->hashableBaseType.compare_exchange_strong(
      expectedType, hashableBaseType ? hashableBaseType
                                     : reinterpret_cast<const Metadata *>(1),
      std::memory_order_acq_rel);
  return type;
}

const hashable_support::HashableWitnessTable *
SwiftValueHeader::getHashableConformance() const {
  if (auto wt = hashableConformance.load(std::memory_order_acquire)) {
    if (reinterpret_cast<uintptr_t>(wt) == 1) {
      return nullptr;
    }
    return wt;
  }

  const HashableWitnessTable *expectedWT = nullptr;
  const HashableWitnessTable *wt =
      reinterpret_cast<const HashableWitnessTable *>(
          swift_conformsToProtocol(type, &HashableProtocolDescriptor));
  hashableConformance.compare_exchange_strong(
      expectedWT, wt ? wt : reinterpret_cast<const HashableWitnessTable *>(1),
      std::memory_order_acq_rel);
  return wt;
}

static constexpr const size_t SwiftValueHeaderOffset
  = sizeof(Class); // isa pointer
static constexpr const size_t SwiftValueMinAlignMask
  = alignof(Class) - 1;
/* TODO: If we're able to become a SwiftObject subclass in the future,
 * change to this:
static constexpr const size_t SwiftValueHeaderOffset
  = sizeof(HeapObject);
static constexpr const size_t SwiftValueMinAlignMask
  = alignof(HeapObject) - 1;
 */

static Class _getSwiftValueClass() {
  auto theClass = [_SwiftValue class];
  // Fixed instance size of _SwiftValue should be same as object header.
  assert(class_getInstanceSize(theClass) == SwiftValueHeaderOffset
         && "unexpected size of _SwiftValue?!");
  return theClass;
}

static Class getSwiftValueClass() {
  return SWIFT_LAZY_CONSTANT(_getSwiftValueClass());
}

static constexpr size_t getSwiftValuePayloadOffset(size_t alignMask) {
  return (SwiftValueHeaderOffset + sizeof(SwiftValueHeader) + alignMask) &
         ~alignMask;
}

static SwiftValueHeader *getSwiftValueHeader(_SwiftValue *v) {
  auto instanceBytes = reinterpret_cast<char *>(v);
  return reinterpret_cast<SwiftValueHeader *>(instanceBytes +
                                              SwiftValueHeaderOffset);
}

static OpaqueValue *getSwiftValuePayload(_SwiftValue *v, size_t alignMask) {
  auto instanceBytes = reinterpret_cast<char *>(v);
  return reinterpret_cast<OpaqueValue *>(instanceBytes +
                                         getSwiftValuePayloadOffset(alignMask));
}

static size_t getSwiftValuePayloadAlignMask(const Metadata *type) {
  return type->getValueWitnesses()->getAlignmentMask() | SwiftValueMinAlignMask;
}

const Metadata *swift::getSwiftValueTypeMetadata(_SwiftValue *v) {
  return getSwiftValueHeader(v)->type;
}

std::pair<const Metadata *, const OpaqueValue *>
swift::getValueFromSwiftValue(_SwiftValue *v) {
  auto instanceType = getSwiftValueTypeMetadata(v);
  size_t alignMask = getSwiftValuePayloadAlignMask(instanceType);
  return {instanceType, getSwiftValuePayload(v, alignMask)};
}

_SwiftValue *swift::bridgeAnythingToSwiftValueObject(OpaqueValue *src,
                                                    const Metadata *srcType,
                                                    bool consume) {
  size_t alignMask = getSwiftValuePayloadAlignMask(srcType);

  size_t totalSize =
      getSwiftValuePayloadOffset(alignMask) + srcType->getValueWitnesses()->size;

  void *instanceMemory = swift_slowAlloc(totalSize, alignMask);
  _SwiftValue *instance
    = objc_constructInstance(getSwiftValueClass(), instanceMemory);
  /* TODO: If we're able to become a SwiftObject subclass in the future,
   * change to this:
  auto instance = swift_allocObject(getSwiftValueClass(), totalSize,
                                    alignMask);
   */

  auto header = getSwiftValueHeader(instance);
  new (header) SwiftValueHeader();
  header->type = srcType;

  auto payload = getSwiftValuePayload(instance, alignMask);
  if (consume)
    srcType->vw_initializeWithTake(payload, src);
  else
    srcType->vw_initializeWithCopy(payload, src);

  return instance;
}

_SwiftValue *swift::getAsSwiftValue(id object) {
  // _SwiftValue should have no subclasses or proxies. We can do an exact
  // class check.
  if (object_getClass(object) == getSwiftValueClass())
    return object;
  return nil;
}

bool
swift::findSwiftValueConformances(const ProtocolDescriptorList &protocols,
                                  const WitnessTable **tablesBuffer) {
  Class cls = nullptr;

  // Note that currently we never modify tablesBuffer because
  // _SwiftValue doesn't conform to any protocols that need witness tables.

  for (size_t i = 0, e = protocols.NumProtocols; i != e; ++i) {
    auto protocol = protocols[i];

    // _SwiftValue does conform to AnyObject.
    switch (protocol->Flags.getSpecialProtocol()) {
    case SpecialProtocol::AnyObject:
      continue;

    case SpecialProtocol::Error:
      return false;

    case SpecialProtocol::None:
      break;
    }

    // Otherwise, it only conforms to ObjC protocols.  We specifically
    // don't want to say that _SwiftValue conforms to the Swift protocols
    // that NSObject conforms to because that would create a situation
    // where arguably an arbitrary type would conform to those protocols
    // by way of coercion through _SwiftValue.  Eventually we want to
    // change _SwiftValue to not be an NSObject subclass at all.

    if (protocol->Flags.getDispatchStrategy() != ProtocolDispatchStrategy::ObjC)
      return false;

    if (!cls) cls = _getSwiftValueClass();

    // Check whether the class conforms to the protocol.
    if (![cls conformsToProtocol: (Protocol*) protocol])
      return false;
  }

  return true;
}

@implementation _SwiftValue

+ (instancetype)allocWithZone:(NSZone *)zone {
  swift::crash("_SwiftValue cannot be instantiated");
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
  auto instanceType = getSwiftValueTypeMetadata(self);
  size_t alignMask = getSwiftValuePayloadAlignMask(instanceType);
  getSwiftValueHeader(self)->~SwiftValueHeader();
  instanceType->vw_destroy(getSwiftValuePayload(self, alignMask));

  // Deallocate ourselves.
  objc_destructInstance(self);
  auto totalSize = getSwiftValuePayloadOffset(alignMask) +
                   instanceType->getValueWitnesses()->size;
  swift_slowDealloc(self, totalSize, alignMask);
}
#pragma clang diagnostic pop

- (BOOL)isEqual:(id)other {
  if (self == other) {
    return YES;
  }

  if (!other) {
    return NO;
  }

  if (![other isKindOfClass:getSwiftValueClass()]) {
    return NO;
  }

  auto selfHeader = getSwiftValueHeader(self);
  auto otherHeader = getSwiftValueHeader(other);

  auto hashableBaseType = selfHeader->getHashableBaseType();
  if (!hashableBaseType ||
      otherHeader->getHashableBaseType() != hashableBaseType) {
    return NO;
  }

  auto hashableConformance = selfHeader->getHashableConformance();
  if (!hashableConformance) {
    return NO;
  }

  return swift_stdlib_Hashable_isEqual_indirect(
      getSwiftValuePayload(self,
                           getSwiftValuePayloadAlignMask(selfHeader->type)),
      getSwiftValuePayload(other,
                           getSwiftValuePayloadAlignMask(otherHeader->type)),
      hashableBaseType, hashableConformance);
}

- (NSUInteger)hash {
  auto selfHeader = getSwiftValueHeader(self);
  auto hashableConformance = selfHeader->getHashableConformance();
  if (!hashableConformance) {
    return (NSUInteger)self;
  }
  return swift_stdlib_Hashable_hashValue_indirect(
      getSwiftValuePayload(self,
                           getSwiftValuePayloadAlignMask(selfHeader->type)),
      selfHeader->type, hashableConformance);
}

static NSString *getValueDescription(_SwiftValue *self) {
  String tmp;
  const Metadata *type;
  const OpaqueValue *value;
  std::tie(type, value) = getValueFromSwiftValue(self);

  // Copy the value, since it will be consumed by getSummary.
  ValueBuffer copyBuf;
  auto copy = type->vw_initializeBufferWithCopy(&copyBuf,
                                              const_cast<OpaqueValue*>(value));
  swift_getSummary(&tmp, copy, type);
  type->vw_deallocateBuffer(&copyBuf);
  return convertStringToNSString(&tmp);
}

- (NSString *)description {
  return getValueDescription(self);
}
- (NSString *)debugDescription {
  return getValueDescription(self);
}

// Private methods for debugging purposes.

- (const Metadata *)_swiftTypeMetadata {
  return getSwiftValueTypeMetadata(self);
}
- (NSString *)_swiftTypeName {
  TwoWordPair<const char *, uintptr_t> typeName
    = swift_getTypeName(getSwiftValueTypeMetadata(self), true);

  return [NSString stringWithUTF8String: typeName.first];
}
- (const OpaqueValue *)_swiftValue {
  return getValueFromSwiftValue(self).second;
}

@end
#endif

// TODO: We could pick specialized _SwiftValue subclasses for trivial types
// or for types with known size and alignment characteristics. Probably
// not enough of a real perf bottleneck to be worth it...
