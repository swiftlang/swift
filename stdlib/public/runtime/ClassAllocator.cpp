//===--- ClassAllocator.cpp - Per-class custom allocators -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Finding and calling the allocator of a class declared with `@allocator`
// (see ClassAllocator.h). The allocator is found from the object's metadata:
// the final free of an object can happen in any module and on any thread
// (for example in the last swift_unownedRelease), and the metadata is all
// that is available there.
//
//===----------------------------------------------------------------------===//

#include "ClassAllocator.h"
#include "swift/Runtime/Debug.h"
#include "swift/Runtime/Heap.h"
#include "swift/Threading/Mutex.h"
#include "swift/shims/LibcShims.h"
#include <cstdint>

using namespace swift;

//===----------------------------------------------------------------------===//
// Swift-CC thunks implemented in the stdlib (core/ClassAllocator.swift). The
// trailing arguments are the allocator's metatype (a value), then the generic
// arguments: its metadata and its ClassAllocator witness table.
//===----------------------------------------------------------------------===//

/// `A.allocate(byteCount:alignment:for:)`
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
void *_swift_stdlib_ClassAllocator_allocate(intptr_t byteCount,
                                            intptr_t alignment,
                                            const Metadata *type,
                                            const Metadata *allocatorValue,
                                            const Metadata *A,
                                            const WitnessTable *AConformance);

/// `A.deallocate(_:byteCount:alignment:for:)`
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
void _swift_stdlib_ClassAllocator_deallocate(void *pointer,
                                             intptr_t byteCount,
                                             intptr_t alignment,
                                             const Metadata *type,
                                             const Metadata *allocatorValue,
                                             const Metadata *A,
                                             const WitnessTable *AConformance);

/// `A.usableSize(of:for:) ?? -1`
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
intptr_t
_swift_stdlib_ClassAllocator_usableSize(const void *pointer,
                                        const Metadata *type,
                                        const Metadata *allocatorValue,
                                        const Metadata *A,
                                        const WitnessTable *AConformance);

//===----------------------------------------------------------------------===//
// Lookup
//===----------------------------------------------------------------------===//

namespace {
struct ResolvedClassAllocator {
  const Metadata *Type = nullptr;
  const WitnessTable *Conformance = nullptr;

  explicit operator bool() const { return Type != nullptr; }
};
} // end anonymous namespace

/// The allocator of instances of \p cls: the one declared by the nearest
/// class in its superclass chain whose descriptor records one. Every class in
/// the chain up to that one has ClassFlags::HasCustomAllocator set; if the
/// flag is missing, a superclass compiled separately no longer declares an
/// allocator, and instances use the default allocator.
static ResolvedClassAllocator lookUpClassAllocator(const ClassMetadata *cls) {
  for (; cls; cls = cls->Superclass) {
    if (!cls->isTypeMetadata() ||
        !(cls->getFlags() & ClassFlags::HasCustomAllocator))
      return {};
    if (auto *record = cls->getDescription()->getClassAllocator()) {
      auto result = record->Accessor();
      return {
          static_cast<const Metadata *>(result.AllocatorType),
          static_cast<const WitnessTable *>(result.ClassAllocatorConformance)};
    }
  }
  return {};
}

// The lookup is cached in the runtime-reserved field of the class metadata,
// which classes with ClassFlags::HasCustomAllocator use for nothing else: 0
// means not looked up yet, 1 means no allocator, and other values index the
// table of distinct allocators.
enum : uint16_t {
  NotLookedUp = 0,
  NoClassAllocator = 1,
  FirstAllocatorIndex = 2,
};

static constexpr unsigned MaxAllocators = 1024;

/// Entries are written once, under AllocatorsLock, before their index is
/// published (with release ordering) in a class's metadata, and never change.
static ResolvedClassAllocator Allocators[MaxAllocators];
static unsigned NumAllocators = FirstAllocatorIndex;
static LazyMutex AllocatorsLock;

static uint16_t cacheIndexFor(ResolvedClassAllocator allocator) {
  if (!allocator)
    return NoClassAllocator;
  uint16_t index = NotLookedUp;
  AllocatorsLock.withLock([&] {
    for (unsigned i = FirstAllocatorIndex; i < NumAllocators; ++i) {
      if (Allocators[i].Type == allocator.Type &&
          Allocators[i].Conformance == allocator.Conformance) {
        index = i;
        return;
      }
    }
    // A full table only disables caching for further allocators.
    if (NumAllocators == MaxAllocators)
      return;
    Allocators[NumAllocators] = allocator;
    index = NumAllocators++;
  });
  return index;
}

static ResolvedClassAllocator findClassAllocator(const ClassMetadata *cls) {
  uint16_t index = __atomic_load_n(&cls->Reserved, __ATOMIC_ACQUIRE);
  if (SWIFT_LIKELY(index >= FirstAllocatorIndex && index < MaxAllocators))
    return Allocators[index];
  if (index == NoClassAllocator)
    return {};

  auto allocator = lookUpClassAllocator(cls);
  // Racing threads store the same index.
  if (uint16_t newIndex = cacheIndexFor(allocator))
    __atomic_store_n(const_cast<uint16_t *>(&cls->Reserved), newIndex,
                     __ATOMIC_RELEASE);
  return allocator;
}

void *class_allocator::allocate(const ClassMetadata *cls, size_t size,
                                size_t alignMask) {
  auto allocator = findClassAllocator(cls);
  if (!allocator)
    return nullptr;
  void *memory = _swift_stdlib_ClassAllocator_allocate(
      (intptr_t)size, (intptr_t)alignMask + 1, cls, allocator.Type,
      allocator.Type, allocator.Conformance);
  if (SWIFT_UNLIKELY(!memory))
    swift_abortAllocationFailure(size, alignMask);
  return memory;
}

void class_allocator::deallocate(HeapObject *object, size_t size,
                                 size_t alignMask) {
  auto *cls = static_cast<const ClassMetadata *>(object->metadata);
  // The allocation begins the instance address point before the object.
  void *allocation =
      reinterpret_cast<char *>(object) - cls->getInstanceAddressPoint();
  auto allocator = findClassAllocator(cls);
  if (!allocator) {
    swift_slowDealloc(allocation, size, alignMask);
    return;
  }
  _swift_stdlib_ClassAllocator_deallocate(
      allocation, (intptr_t)size, (intptr_t)alignMask + 1, cls, allocator.Type,
      allocator.Type, allocator.Conformance);
}

//===----------------------------------------------------------------------===//
// Exported entry points
//===----------------------------------------------------------------------===//

/// The number of usable bytes from \p object to the end of its allocation,
/// for computing the capacity of tail-allocated storage
/// (ManagedBuffer.capacity). Uses the class allocator's usableSize(of:for:) if
/// the object's class has one, and malloc_size otherwise.
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_API
size_t swift_objectAllocationSize(const void *object) {
  auto *heapObject = reinterpret_cast<const HeapObject *>(object);
  if (SWIFT_LIKELY(!class_allocator::hasClassAllocator(heapObject->metadata)))
    return _swift_stdlib_malloc_size(object);

  auto *cls = static_cast<const ClassMetadata *>(heapObject->metadata);
  size_t addressPoint = cls->getInstanceAddressPoint();
  auto *allocation = reinterpret_cast<const char *>(object) - addressPoint;
  auto allocator = findClassAllocator(cls);
  if (!allocator)
    return _swift_stdlib_malloc_size(allocation) - addressPoint;
  intptr_t size = _swift_stdlib_ClassAllocator_usableSize(
      allocation, cls, allocator.Type, allocator.Type, allocator.Conformance);
  if (size < 0) {
    auto className = swift_getTypeName(cls, /*qualified=*/true);
    auto allocatorName = swift_getTypeName(allocator.Type, /*qualified=*/true);
    swift::fatalError(
        /*flags=*/0,
        "the allocation size of an instance of %.*s is unknown: its class "
        "allocator %.*s does not implement usableSize(of:for:)\n",
        (int)className.length, className.data, (int)allocatorName.length,
        allocatorName.data);
  }
  return (size_t)size - addressPoint;
}
