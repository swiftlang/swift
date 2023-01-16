//===--- RuntimeValueWitness.cpp - Value Witness Runtime Implementation---===//
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
// Implementations of runtime determined value witness functions
// This file is intended to be statically linked into executables until it is
// fully added to the runtime.
//
//===----------------------------------------------------------------------===//

#include "BytecodeLayouts.h"
#include "../../public/runtime/WeakReference.h"
#include "../../public/SwiftShims/swift/shims/HeapObject.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/ABI/System.h"
#include "swift/Runtime/Error.h"
#include "swift/Runtime/HeapObject.h"
#include "llvm/Support/SwapByteOrder.h"
#include <cstdint>
#if SWIFT_OBJC_INTEROP
#include "swift/Runtime/ObjCBridge.h"
#include <Block.h>
#endif

using namespace swift;

// Pointers my have spare bits stored in the high and low bits. Mask them out
// before we pass them to retain/release functions
#define MASK_PTR(x)                                                            \
  ((__swift_uintptr_t)x & ~heap_object_abi::SwiftSpareBitsMask)

static const size_t layoutStringHeaderSize = 4;

/// Get the generic argument vector for the passed in metadata
///
/// NB: We manually compute the offset instead of using Metadata::getGenericArgs
/// because Metadata::getGenericArgs checks the isSwift bit which we cannot do
/// in a compatibility library. Once we merge this into the runtime, we can
/// safely use Metadata::getGenericArgs. For our purposes right now anyways,
/// struct and enums always have their generic argument vector at offset + 2 so
/// we can hard code that.
const Metadata **getGenericArgs(Metadata *metadata) {
  return ((const Metadata **)metadata) + 2;
}

/// Given a pointer and an offset, read the requested data and increment the
/// offset
template <typename T>
T readBytes(const uint8_t *typeLayout, size_t &i) {
  T returnVal = *(const T *)(typeLayout + i);
  for (size_t j = 0; j < sizeof(T); j++) {
    returnVal <<= 8;
    returnVal |= *(typeLayout + i + j);
  }
  i += sizeof(T);
  return returnVal;
}

template <>
uint8_t readBytes<uint8_t>(const uint8_t *typeLayout, size_t &i) {
  uint8_t returnVal = *(typeLayout + i);
  i += 1;
  return returnVal;
}

/// Given a pointer, a value, and an offset, write the value at the given
/// offset in big-endian order
template <typename T>
void writeBytes(uint8_t *typeLayout, size_t i, T value) {
  for (size_t j = sizeof(T) - 1; j > 0; j--) {
    typeLayout[i + j] = (uint8_t)(value & 0xff);
    value >>= 8;
  }
  typeLayout[i] = (uint8_t)(value & 0xff);
}

Metadata *getExistentialTypeMetadata(OpaqueValue *object) {
  return reinterpret_cast<Metadata**>(object)[NumWords_ValueBuffer];
}

typedef void (*DestrFn)(void*);

struct DestroyFuncAndMask {
  DestrFn fn;
  uintptr_t mask;
  bool isIndirect;
};

void skipDestroy(void* ignore) { }

void existential_destroy(OpaqueValue* object) {
  auto* metadata = getExistentialTypeMetadata(object);

  metadata->vw_destroy(object);
}

const DestroyFuncAndMask destroyTable[] = {
    {(DestrFn)&skipDestroy, UINTPTR_MAX, false},
    {(DestrFn)&swift_errorRelease, UINTPTR_MAX, true},
    {(DestrFn)&swift_release, ~heap_object_abi::SwiftSpareBitsMask, true},
    {(DestrFn)&swift_unownedRelease, ~heap_object_abi::SwiftSpareBitsMask,
     true},
    {(DestrFn)&swift_weakDestroy, UINTPTR_MAX, false},
    {(DestrFn)&swift_unknownObjectRelease, ~heap_object_abi::SwiftSpareBitsMask,
     true},
    {(DestrFn)&swift_unknownObjectUnownedDestroy, UINTPTR_MAX, false},
    {(DestrFn)&swift_unknownObjectWeakDestroy, UINTPTR_MAX, false},
    {(DestrFn)&swift_bridgeObjectRelease, ~heap_object_abi::SwiftSpareBitsMask,
     true},
#if SWIFT_OBJC_INTEROP
    {(DestrFn)&_Block_release, UINTPTR_MAX, true},
    {(DestrFn)&objc_release, UINTPTR_MAX, true},
#else
  {nullptr, UINTPTR_MAX, true},
  {nullptr, UINTPTR_MAX, true},
#endif
    // TODO: how to handle Custom?
    {nullptr, UINTPTR_MAX, true},
    {nullptr, UINTPTR_MAX, true},
    {nullptr, UINTPTR_MAX, true},
    {(DestrFn)&existential_destroy, UINTPTR_MAX, false},
};

__attribute__((weak)) extern "C" void
swift_generic_destroy(void *address, void *metadata) {
  uint8_t *addr = (uint8_t *)address;
  Metadata *typedMetadata = (Metadata *)metadata;

  const uint8_t *typeLayout = typedMetadata->getLayoutString();

  // fixed data is 32 bytes
  size_t offset = layoutStringHeaderSize;

  do {
    uint32_t skip = readBytes<uint32_t>(typeLayout, offset);
    uint8_t tag = ((uint8_t)(skip >> 24));
    skip &= ~(0xff << 24);
    addr += skip;

    if (SWIFT_UNLIKELY(tag == (uint8_t)RefCountingKind::Witness)) {
      auto typePtr = readBytes<uintptr_t>(typeLayout, offset);
      auto *type = reinterpret_cast<Metadata*>(typePtr);
      type->vw_destroy((OpaqueValue *)addr);
      addr += type->vw_size();
    } else {
      const auto &destroyFunc = destroyTable[tag];
      if (SWIFT_LIKELY(destroyFunc.isIndirect)) {
        destroyFunc.fn((void *)((*(uintptr_t *)addr) & destroyFunc.mask));
      } else {
        destroyFunc.fn(((void *)addr));
      }
    }
  } while (typeLayout[offset] != 0);
}

struct RetainFuncAndMask {
  void* fn;
  uintptr_t mask;
  bool isSingle;
};

void* Block_copyForwarder(void** dest, const void** src) {
  *dest = _Block_copy(*src);
  return *dest;
}

typedef void* (*RetainFn)(void*);
typedef void* (*CopyInitFn)(void*, void*);

void* skipRetain(void* ignore) { return nullptr; }
void* existential_initializeWithCopy(OpaqueValue* dest, OpaqueValue* src) {
  auto* metadata = getExistentialTypeMetadata(src);

  return metadata->vw_initializeWithCopy(dest, src);
}

const RetainFuncAndMask retainTable[] = {
  {(void*)&skipRetain, UINTPTR_MAX, true},
  {(void*)&swift_errorRetain, UINTPTR_MAX, true},
  {(void*)&swift_retain, ~heap_object_abi::SwiftSpareBitsMask, true},
  {(void*)&swift_unownedRetain, ~heap_object_abi::SwiftSpareBitsMask, true},
  {(void*)&swift_weakCopyInit, UINTPTR_MAX, false},
  {(void*)&swift_unknownObjectRetain, ~heap_object_abi::SwiftSpareBitsMask, true},
  {(void*)&swift_unknownObjectUnownedCopyInit, UINTPTR_MAX, false},
  {(void*)&swift_unknownObjectWeakCopyInit, UINTPTR_MAX, false},
  {(void*)&swift_bridgeObjectRetain, ~heap_object_abi::SwiftSpareBitsMask, true},
#if SWIFT_OBJC_INTEROP
  {(void*)&Block_copyForwarder, UINTPTR_MAX, false},
  {(void*)&objc_retain, UINTPTR_MAX, true},
#else
  {nullptr, UINTPTR_MAX, true},
  {nullptr, UINTPTR_MAX, true},
#endif
  // TODO: how to handle Custom?
  {nullptr, UINTPTR_MAX, true},
  {nullptr, UINTPTR_MAX, true},
  {nullptr, UINTPTR_MAX, true},
  {(void*)&existential_initializeWithCopy, UINTPTR_MAX, false},
};

__attribute__((weak)) extern "C" void
swift_generic_initWithCopy(void *dest, void *src, void *metadata) {
  uintptr_t addrOffset = 0;
  Metadata *typedMetadata = (Metadata *)metadata;
  const uint8_t *typeLayout = typedMetadata->getLayoutString();

  size_t size = typedMetadata->vw_size();

  auto offset = layoutStringHeaderSize;

  memcpy(dest, src, size);

  do {
    uint32_t skip = readBytes<uint32_t>(typeLayout, offset);
    auto tag = static_cast<uint8_t>(skip >> 24);
    skip &= ~(0xff << 24);
    addrOffset += skip;

    if (SWIFT_UNLIKELY(tag == (uint8_t)RefCountingKind::Witness)) {
      auto typePtr = readBytes<uintptr_t>(typeLayout, offset);
      auto *type = reinterpret_cast<Metadata*>(typePtr);
      type->vw_initializeWithCopy((OpaqueValue*)((uintptr_t)dest + addrOffset),
                                  (OpaqueValue*)((uintptr_t)src + addrOffset));
      addrOffset += type->vw_size();
    } else {
      const auto &retainFunc = retainTable[tag];
      if (SWIFT_LIKELY(retainFunc.isSingle)) {
        ((RetainFn)retainFunc.fn)(
            *(void **)(((uintptr_t)dest + addrOffset) & retainFunc.mask));
      } else {
        ((CopyInitFn)retainFunc.fn)((void*)((uintptr_t)dest + addrOffset), (void*)((uintptr_t)src + addrOffset));
      }
    }
  } while (typeLayout[offset] != 0);
}

__attribute__((weak)) extern "C" void
swift_generic_initWithTake(void *dest, void *src, void *metadata) {
  Metadata *typedMetadata = (Metadata *)metadata;
  const uint8_t *typeLayout = typedMetadata->getLayoutString();
  size_t size = typedMetadata->vw_size();

  memcpy(dest, src, size);

  if (SWIFT_LIKELY(!typedMetadata->getValueWitnesses()->isBitwiseTakable())) {
    return;
  }

  auto offset = layoutStringHeaderSize;
  uintptr_t addrOffset = 0;

  do {
    uint32_t skip = readBytes<uint32_t>(typeLayout, offset);
    auto tag = static_cast<RefCountingKind>(skip >> 24);
    skip &= ~(0xff << 24);
    addrOffset += skip;

    switch (tag) {
    case RefCountingKind::NativeWeak:
      swift_weakTakeInit((WeakReference *)((uintptr_t)dest + addrOffset),
                         (WeakReference *)((uintptr_t)src + addrOffset));
      break;
    case RefCountingKind::UnknownWeak:
      swift_unknownObjectWeakTakeInit((WeakReference*)((uintptr_t)dest + addrOffset),
                                      (WeakReference*)((uintptr_t)src + addrOffset));
      break;
    case RefCountingKind::Witness: {
      auto typePtr = readBytes<uintptr_t>(typeLayout, offset);
      auto *type = reinterpret_cast<Metadata*>(typePtr);
      if (SWIFT_UNLIKELY(!type->getValueWitnesses()->isBitwiseTakable())) {
        type->vw_initializeWithTake((OpaqueValue*)((uintptr_t)dest + addrOffset),
                                    (OpaqueValue*)((uintptr_t)src + addrOffset));
      }
      addrOffset += type->vw_size();
      break;
    }
    case RefCountingKind::Existential: {
      auto *type = getExistentialTypeMetadata((OpaqueValue*)((uintptr_t)src + addrOffset));
      if (SWIFT_UNLIKELY(!type->getValueWitnesses()->isBitwiseTakable())) {
        type->vw_initializeWithTake((OpaqueValue*)((uintptr_t)dest + addrOffset),
                                    (OpaqueValue*)((uintptr_t)src + addrOffset));
      }
      break;
    }
    default:
      break;
    }
  } while (typeLayout[offset] != 0);
}

__attribute__((weak)) extern "C" void
swift_generic_assignWithCopy(void *dest, void *src, void *metadata) {
  swift_generic_destroy(dest, metadata);
  swift_generic_initWithCopy(dest, src, metadata);
}

__attribute__((weak)) extern "C" void
swift_generic_assignWithTake(void *dest, void *src, void *metadata) {
  swift_generic_destroy(dest, metadata);
  swift_generic_initWithTake(dest, src, metadata);
}

__attribute__((weak)) extern "C" void
swift_generic_instantiateLayoutString(const uint8_t* layoutStr,
                                      Metadata* type) {
  size_t offset = 0;
  const auto refCountSize = readBytes<uint32_t>(layoutStr, offset);

  const size_t genericDescOffset = layoutStringHeaderSize + refCountSize + 4;
  offset = genericDescOffset;

  uint32_t genericRefCountSize = 0;
  do {
    const auto tagAndIdx = readBytes<uint32_t>(layoutStr, offset);
    const auto tag = (uint8_t)(tagAndIdx >> 24);
    const auto index = tagAndIdx & ~(0xff << 24);

    if (tag == 2) {
      offset += 4;
      const Metadata *genericType = getGenericArgs(type)[index];
      if (genericType->getTypeContextDescriptor()->hasLayoutString()) {
        const uint8_t *genericLayoutStr = genericType->getLayoutString();
        size_t countOffset = 25;
        genericRefCountSize +=
            readBytes<uint32_t>(genericLayoutStr, countOffset);
      } else if (genericType->isClassObject()) {
        genericRefCountSize += sizeof(uint32_t);
      } else {
        genericRefCountSize += sizeof(uint32_t) + sizeof(uintptr_t);
      }
    }
  } while (layoutStr[offset] != 0);

  const auto instancedLayoutStrSize =
      layoutStringHeaderSize + refCountSize + genericRefCountSize + 4 + 1;

  uint8_t *instancedLayoutStr = (uint8_t*)calloc(instancedLayoutStrSize, sizeof(uint8_t));

  writeBytes<uint32_t>(instancedLayoutStr, 0,
                       refCountSize + genericRefCountSize);

  offset = genericDescOffset;
  size_t layoutStrOffset = layoutStringHeaderSize;
  size_t instancedLayoutStrOffset = layoutStringHeaderSize;
  uint32_t skipBytes = 0;
  do {
    const auto tagAndIdx = readBytes<uint32_t>(layoutStr, offset);
    const auto tag = (uint8_t)(tagAndIdx >> 24);
    const auto index = tagAndIdx & ~(0xff << 24);

    if (tag == 1) {
      memcpy((void *)(layoutStr + layoutStrOffset),
             (void *)(instancedLayoutStr + instancedLayoutStrOffset), index);
      layoutStrOffset += index;
      instancedLayoutStrOffset += index;
      if (skipBytes) {
        size_t firstRCOffset = instancedLayoutStrOffset;
        auto firstRC = readBytes<uint32_t>(instancedLayoutStr, firstRCOffset);
        firstRC += skipBytes;
        writeBytes(instancedLayoutStr, firstRCOffset, firstRC);
        skipBytes = 0;
      }
    } else if (tag == 2) {
      skipBytes = readBytes<uint32_t>(layoutStr, offset);
      const Metadata *genericType = getGenericArgs(type)[index];
      if (genericType->getTypeContextDescriptor()->hasLayoutString()) {
        const uint8_t *genericLayoutStr = genericType->getLayoutString();
        size_t countOffset = 0;
        auto genericRefCountSize =
            readBytes<uint32_t>(genericLayoutStr, countOffset);
        if (genericRefCountSize > 0) {
          memcpy((void *)(genericLayoutStr + layoutStringHeaderSize),
                 (void *)(instancedLayoutStr + instancedLayoutStrOffset),
                 genericRefCountSize);
          if (skipBytes) {
            size_t firstRCOffset = instancedLayoutStrOffset;
            auto firstRC =
                readBytes<uint32_t>(instancedLayoutStr, firstRCOffset);
            firstRC += skipBytes;
            writeBytes(instancedLayoutStr, firstRCOffset, firstRC);
            skipBytes = 0;
          }

          instancedLayoutStrOffset += genericRefCountSize;
          size_t trailingBytesOffset = layoutStringHeaderSize + genericRefCountSize;
          skipBytes +=
              readBytes<uint32_t>(genericLayoutStr, trailingBytesOffset);
        }
      } else if (genericType->isClassObject()) {
        uint32_t op = static_cast<uint32_t>(RefCountingKind::Unknown) << 24;
        op |= (skipBytes & ~(0xff << 24));

        writeBytes<uint32_t>(instancedLayoutStr, instancedLayoutStrOffset, op);

        instancedLayoutStrOffset += sizeof(uint32_t);

        skipBytes = 0;
      } else {
        const ValueWitnessTable *vwt = genericType->getValueWitnesses();
        if (vwt->isPOD()) {
          skipBytes += vwt->getSize();
          continue;
        }

        uint32_t op = static_cast<uint32_t>(RefCountingKind::Witness) << 24;
        op |= (skipBytes & ~(0xff << 24));

        writeBytes<uint32_t>(instancedLayoutStr, instancedLayoutStrOffset, op);

        instancedLayoutStrOffset += sizeof(uint32_t);

        writeBytes<uintptr_t>(instancedLayoutStr, instancedLayoutStrOffset, reinterpret_cast<uintptr_t>(genericType));
        instancedLayoutStrOffset += sizeof(uintptr_t);

        skipBytes = 0;
      }
    }
  } while (layoutStr[offset] != 0);

  size_t trailingBytesOffset = layoutStringHeaderSize + refCountSize;
  skipBytes += readBytes<uint32_t>(layoutStr, trailingBytesOffset);

  if (skipBytes > 0) {
    writeBytes<uint32_t>(
        instancedLayoutStr,
        layoutStringHeaderSize + refCountSize + genericRefCountSize, skipBytes);
  }

  type->setLayoutString(instancedLayoutStr);

  fprintf(stderr, "==== Instantiated: ");
  for (size_t i = 0; i < instancedLayoutStrSize; i++) {
    fprintf(stderr, "\\%02x", instancedLayoutStr[i]);
  }
  fprintf(stderr, "\n");
}

// Allow this library to get force-loaded by autolinking
__attribute__((weak, visibility("hidden"))) extern "C" char
    _swift_FORCE_LOAD_$_swiftCompatibilityBytecodeLayouts = 0;
