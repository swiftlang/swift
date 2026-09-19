//===--- InstancePrefix.cpp - Instance prefix tests -----------------------===//
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

#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Once.h"
#include "gtest/gtest.h"
#include <array>
#include <cstring>

using namespace swift;

namespace {
ContextDescriptorFlags descriptorFlags(bool hasPrefix, bool hasVTable = false) {
  TypeContextDescriptorFlags flags;
  flags.class_setHasInstancePrefix(hasPrefix);
  flags.class_setHasVTable(hasVTable);
  return ContextDescriptorFlags(ContextDescriptorKind::Class,
                                /*isGeneric=*/false, /*isUnique=*/true,
                                /*hasInvertibleProtocols=*/false,
                                flags.getOpaqueValue());
}

struct PrefixDescriptor {
  alignas(ClassDescriptor) unsigned char Storage
      [sizeof(ClassDescriptor) + sizeof(ClassInstancePrefixDescriptor)]{};
  ClassDescriptor &Class = *reinterpret_cast<ClassDescriptor *>(Storage);
  ClassInstancePrefixDescriptor &Prefix =
      *reinterpret_cast<ClassInstancePrefixDescriptor *>(
          Storage + sizeof(ClassDescriptor));
  std::array<uintptr_t, 2> Template{{0x1234, 0x5678}};

  PrefixDescriptor() {
    Class.Flags = descriptorFlags(true);
    Prefix.Version = ClassInstancePrefixDescriptor::CurrentVersion;
    Prefix.PrefixSizeInWords = Template.size();
    new (&Prefix.PrefixTemplate) decltype(Prefix.PrefixTemplate)(
        Template.data());
  }
};
} // namespace

TEST(InstancePrefixTest, AbsentDescriptor) {
  alignas(ClassDescriptor) unsigned char storage[sizeof(ClassDescriptor)]{};
  auto &descriptor = *reinterpret_cast<ClassDescriptor *>(storage);
  descriptor.Flags = descriptorFlags(false);
  EXPECT_FALSE(descriptor.hasInstancePrefix());
  EXPECT_EQ(nullptr, descriptor.getInstancePrefixDescriptor());
}

TEST(InstancePrefixTest, Descriptor) {
  PrefixDescriptor descriptor;
  ASSERT_TRUE(descriptor.Class.hasInstancePrefix());
  auto *prefix = descriptor.Class.getInstancePrefixDescriptor();
  ASSERT_EQ(&descriptor.Prefix, prefix);
  EXPECT_EQ(0, prefix->Version);
  EXPECT_EQ(2, prefix->PrefixSizeInWords);
  EXPECT_EQ(descriptor.Template.data(), prefix->PrefixTemplate.get());
}

TEST(InstancePrefixTest, DescriptorAfterVTable) {
  struct Layout {
    ClassDescriptor Class;
    TargetVTableDescriptorHeader<InProcess> VTable;
    std::array<MethodDescriptor, 2> Methods;
    ClassInstancePrefixDescriptor Prefix;
  };
  alignas(Layout) unsigned char storage[sizeof(Layout)]{};
  auto &descriptor = *reinterpret_cast<Layout *>(storage);
  descriptor.Class.Flags = descriptorFlags(true, true);
  descriptor.VTable.VTableSize = descriptor.Methods.size();
  EXPECT_EQ(&descriptor.VTable, descriptor.Class.getVTableDescriptor());
  EXPECT_EQ(descriptor.Methods.data(),
            descriptor.Class.getMethodDescriptors().data());
  EXPECT_EQ(&descriptor.Prefix, descriptor.Class.getInstancePrefixDescriptor());
}

namespace {
SWIFT_CC(swift) void destroyPrefixedObject(SWIFT_CONTEXT HeapObject *object) {
  auto *metadata = static_cast<const ClassMetadata *>(object->metadata);
  swift_deallocObject(object, metadata->getInstanceSize(),
                      metadata->getInstanceAlignMask());
}

struct PrefixAllocation : PrefixDescriptor {
  FullMetadata<ClassMetadata> Metadata = {
      {{nullptr}, {&destroyPrefixedObject}, {&VALUE_WITNESS_SYM(Bo)}},
      {{nullptr}, ClassFlags::UsesSwiftRefcounting, 0, 0, 0, 0, 0, 0}};

  explicit PrefixAllocation(size_t addressPoint = 4 * sizeof(void *),
                            size_t alignment = 4 * sizeof(void *)) {
    Metadata.setDescription(&Class);
    Metadata.setInstanceAddressPoint(addressPoint);
    Metadata.setInstanceSize(addressPoint + sizeof(HeapObject));
    Metadata.setInstanceAlignMask(alignment - 1);
  }

  HeapObject *allocate() {
    return swift_allocObject(&Metadata, Metadata.getInstanceSize(),
                             Metadata.getInstanceAlignMask());
  }
};
} // namespace

TEST(InstancePrefixTest, Allocation) {
  for (size_t alignment : {2 * sizeof(void *), 4 * sizeof(void *)}) {
    PrefixAllocation fixture(alignment, alignment);
    auto *object = fixture.allocate();
    EXPECT_EQ(&fixture.Metadata, object->metadata);
    EXPECT_EQ(0U, reinterpret_cast<uintptr_t>(object) & (alignment - 1));
    auto *prefix =
        reinterpret_cast<unsigned char *>(object) - sizeof(fixture.Template);
    EXPECT_EQ(0, std::memcmp(prefix, fixture.Template.data(),
                             sizeof(fixture.Template)));
    // Each allocation receives its own copy of the immutable template.
    prefix[0] ^= 0xff;
    auto *second = fixture.allocate();
    EXPECT_EQ(0,
              std::memcmp(reinterpret_cast<unsigned char *>(second) -
                              sizeof(fixture.Template),
                          fixture.Template.data(), sizeof(fixture.Template)));
    swift_release(second);
    swift_release(object);
  }
}

TEST(InstancePrefixTest, AddressPointWithoutTemplate) {
  PrefixAllocation fixture;
  fixture.Class.Flags = descriptorFlags(false);
  auto *object = fixture.allocate();
  EXPECT_EQ(&fixture.Metadata, object->metadata);
  swift_release(object);
  fixture.Metadata.setDescription(nullptr);
  object = fixture.allocate();
  EXPECT_EQ(&fixture.Metadata, object->metadata);
  swift_release(object);
}

TEST(InstancePrefixTest, ZeroAddressPoint) {
  PrefixAllocation fixture(0, alignof(HeapObject));
  fixture.Metadata.setDescription(nullptr);
  auto *object = fixture.allocate();
  EXPECT_EQ(&fixture.Metadata, object->metadata);
  swift_release(object);
}

TEST(InstancePrefixTest, UnsupportedVersion) {
  PrefixAllocation fixture;
  ++fixture.Prefix.Version;
  EXPECT_DEATH_IF_SUPPORTED(fixture.allocate(),
                            "unsupported class instance prefix descriptor");
}

TEST(InstancePrefixTest, InvalidTemplateSize) {
  PrefixAllocation fixture;
  fixture.Prefix.PrefixSizeInWords = 0;
  EXPECT_DEATH_IF_SUPPORTED(fixture.allocate(),
                            "class instance prefix size 0 does not fit");
  fixture.Prefix.PrefixSizeInWords = 5;
  EXPECT_DEATH_IF_SUPPORTED(fixture.allocate(),
                            "class instance prefix size .* does not fit");
}

TEST(InstancePrefixTest, InvalidAllocationSize) {
  for (bool hasPrefix : {false, true}) {
    PrefixAllocation fixture;
    fixture.Class.Flags = descriptorFlags(hasPrefix);
    fixture.Metadata.setInstanceSize(
        fixture.Metadata.getInstanceAddressPoint() - 1);
    EXPECT_DEATH_IF_SUPPORTED(fixture.allocate(),
                              "does not fit in allocation size");
    fixture.Metadata.setInstanceSize(
        fixture.Metadata.getInstanceAddressPoint() + sizeof(HeapObject) - 1);
    EXPECT_DEATH_IF_SUPPORTED(fixture.allocate(),
                              "does not fit in allocation size");
  }
}

TEST(InstancePrefixTest, InvalidAlignment) {
  for (bool hasPrefix : {false, true}) {
    PrefixAllocation fixture(2 * sizeof(void *), 4 * sizeof(void *));
    fixture.Class.Flags = descriptorFlags(hasPrefix);
    EXPECT_DEATH_IF_SUPPORTED(fixture.allocate(),
                              "does not preserve alignment");
  }
}

// Unit tests enable assertions independently of the runtime they link against.
#ifdef SWIFT_RUNTIME_TESTS_ENABLE_ASSERTIONS
TEST(InstancePrefixTest, RejectStackInitialization) {
  PrefixAllocation fixture;
  HeapObject object;
  EXPECT_DEATH_IF_SUPPORTED(
      swift_initStackObject(&fixture.Metadata, &object),
      "cannot initialize a prefixed class object on the stack");
}

TEST(InstancePrefixTest, RejectStaticInitialization) {
  PrefixAllocation fixture;
  // swift_initStaticObject requires a once token immediately before the object.
  struct {
    swift_once_t Token{};
    HeapObject Object;
  } storage;
  EXPECT_DEATH_IF_SUPPORTED(
      swift_initStaticObject(&fixture.Metadata, &storage.Object),
      "cannot initialize a prefixed static class object");
}
#endif
