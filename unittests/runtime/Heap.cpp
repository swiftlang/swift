//===--- Heap.cpp - Heap tests --------------------------------------------===//
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

#include "swift/Runtime/Heap.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include <gtest/gtest.h>

using namespace swift;

void shouldAlloc(size_t size, size_t alignMask) {
  void *ptr = swift::swift_slowAlloc(size, alignMask);
  EXPECT_NE(ptr, (void *)NULL)
    << "Allocation failed for size " << size << " and alignment mask "
    << alignMask << ".";
  swift::swift_slowDealloc(ptr, size, alignMask);
}

void shouldAlloc(size_t size) {
  shouldAlloc(size, 0);
  shouldAlloc(size, 1);
  shouldAlloc(size, 3);
  shouldAlloc(size, 7);
  shouldAlloc(size, 15);
  shouldAlloc(size, 31);
  shouldAlloc(size, 63);
  shouldAlloc(size, 4095);
}

TEST(HeapTest, slowAlloc) {
  shouldAlloc(1);
  shouldAlloc(8);
  shouldAlloc(32);
  shouldAlloc(1093);
}

void shouldAllocTyped(size_t size, size_t alignMask, swift::MallocTypeId typeId) {
  void *ptr = swift::swift_slowAllocTyped(size, alignMask, typeId);
  EXPECT_NE(ptr, (void *)NULL)
    << "Typed allocation failed for size " << size << " and alignment mask "
    << alignMask << ".";
  swift::swift_slowDealloc(ptr, size, alignMask);
}

void shouldAllocTyped(size_t size, swift::MallocTypeId typeId) {
  shouldAlloc(size, 0);
  shouldAlloc(size, 1);
  shouldAlloc(size, 3);
  shouldAlloc(size, 7);
  shouldAlloc(size, 15);
  shouldAlloc(size, 31);
  shouldAlloc(size, 63);
  shouldAlloc(size, 4095);
}

void shouldAllocTyped(size_t size) {
  shouldAllocTyped(size, 42);
}

TEST(HeapTest, slowAllocTyped) {
  shouldAllocTyped(1);
  shouldAllocTyped(8);
  shouldAllocTyped(32);
  shouldAllocTyped(1093);
}

TEST(HeapTest, allocObjectInitializesInstancePrefix) {
  constexpr size_t PrefixSize = 2 * sizeof(void *);
  constexpr size_t InstanceAlignment = 4 * sizeof(void *);
  constexpr size_t AddressPoint = InstanceAlignment;
  constexpr size_t DescriptorStorageSize =
      sizeof(ClassDescriptor) + sizeof(ClassInstancePrefixDescriptor) +
      PrefixSize + alignof(ClassInstancePrefixDescriptor);

  auto *descriptorStorage =
      static_cast<unsigned char *>(std::calloc(1, DescriptorStorageSize));
  ASSERT_NE(descriptorStorage, nullptr);
  auto *descriptor = reinterpret_cast<ClassDescriptor *>(descriptorStorage);

  TypeContextDescriptorFlags typeFlags;
  typeFlags.class_setHasInstancePrefix(true);
  descriptor->Flags =
      ContextDescriptorFlags(ContextDescriptorKind::Class,
                             /*isGeneric=*/false,
                             /*isUnique=*/true,
                             /*hasInvertibleProtocols=*/false,
                             typeFlags.getOpaqueValue());

  ClassInstancePrefixDescriptor *prefixDescriptor =
    const_cast<decltype(prefixDescriptor)>(descriptor->getInstancePrefixDescriptor());
  auto *prefixTemplate =
      reinterpret_cast<unsigned char *>(prefixDescriptor + 1);
  std::array<unsigned char, PrefixSize> expected;
  for (size_t index = 0; index != expected.size(); ++index)
    expected[index] = static_cast<unsigned char>(index * 17 + 3);
  std::memcpy(prefixTemplate, expected.data(), expected.size());

  prefixDescriptor->Version = ClassInstancePrefixDescriptor::CurrentVersion;
  prefixDescriptor->PrefixSizeInWords = PrefixSize / sizeof(void *);
  using PrefixTemplatePointer = decltype(prefixDescriptor->PrefixTemplate);
  new (&prefixDescriptor->PrefixTemplate) PrefixTemplatePointer(prefixTemplate);

  FullMetadata<ClassMetadata> metadata = {
      {{nullptr}, {nullptr}, {&VALUE_WITNESS_SYM(Bo)}},
      {{nullptr}, ClassFlags::UsesSwiftRefcounting, 0, 0, 0, 0, 0, 0}
  };
  metadata.setDescription(descriptor);
  metadata.setInstanceAddressPoint(AddressPoint);
  metadata.setInstanceSize(AddressPoint + sizeof(HeapObject));
  metadata.setInstanceAlignMask(InstanceAlignment - 1);

  auto *object =
      swift_allocObject(&metadata, metadata.getInstanceSize(),
                        metadata.getInstanceAlignMask());
  EXPECT_EQ(object->metadata, &metadata);
  auto *allocationBase =
      reinterpret_cast<unsigned char *>(object) - AddressPoint;
  auto *prefixAddress = allocationBase + AddressPoint - PrefixSize;
  EXPECT_EQ(std::memcmp(prefixAddress, expected.data(), expected.size()), 0);

  swift_deallocObject(object, metadata.getInstanceSize(),
                      metadata.getInstanceAlignMask());
  std::free(descriptorStorage);
}
