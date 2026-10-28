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

#include "runtime/Private.h"
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

namespace {
struct PrefixLayout : PrefixAllocation {
  char Name[16] = "PrefixLayout";
#if SWIFT_OBJC_INTEROP
  // The field-layout routine only reads and writes this header of ObjC RO data.
  alignas(void *) struct {
    uint32_t Flags = 0;
    uint32_t InstanceStart = 0;
    uint32_t InstanceSize = 0;
  } ROData;
#endif

  explicit PrefixLayout(size_t prefixSize = 2 * sizeof(void *))
      : PrefixAllocation(0, alignof(HeapObject)) {
    Metadata.setInstanceAddressPoint(prefixSize);
    new (&Class.Name) decltype(Class.Name)(Name);
#if SWIFT_OBJC_INTEROP
    Metadata.Data =
        reinterpret_cast<uintptr_t>(&ROData) | SWIFT_CLASS_IS_SWIFT_MASK;
#endif
  }

  size_t layout(const TypeLayout &field) {
    const TypeLayout *fields[] = {&field};
    size_t offset = 0;
    initClassFieldOffsetVector(&Metadata, 1, fields, &offset);
    return offset;
  }
};
} // namespace

TEST(InstancePrefixTest, DynamicLayout) {
  const size_t alignment = 4 * sizeof(void *);
  TypeLayout field(sizeof(void *), sizeof(void *),
                   ValueWitnessFlags().withAlignmentMask(alignment - 1), 0);
  const struct {
    size_t PrefixSize;
    size_t AddressPoint;
  } cases[] = {{0, 0},
               {2 * sizeof(void *), alignment},
               {5 * sizeof(void *), 2 * alignment}};
  for (auto test : cases) {
    PrefixLayout fixture(test.PrefixSize);
    size_t expectedAddressPoint = test.AddressPoint;
    EXPECT_EQ(alignment, fixture.layout(field));
    EXPECT_EQ(expectedAddressPoint, fixture.Metadata.getInstanceAddressPoint());
    EXPECT_EQ(expectedAddressPoint + alignment + sizeof(void *),
              fixture.Metadata.getInstanceSize());
    EXPECT_EQ(alignment - 1, fixture.Metadata.getInstanceAlignMask());
#if SWIFT_OBJC_INTEROP
    EXPECT_EQ(alignment + sizeof(void *), fixture.ROData.InstanceSize);
#endif
  }
}

TEST(InstancePrefixTest, SubclassLayout) {
  TypeLayout field(sizeof(void *), sizeof(void *),
                   ValueWitnessFlags().withAlignmentMask(alignof(void *) - 1),
                   0);
  PrefixLayout superclass;
  EXPECT_EQ(sizeof(HeapObject), superclass.layout(field));
  // The subclass's prefix is complete, including the superclass's words.
  PrefixLayout subclass(4 * sizeof(void *));
  subclass.Metadata.Superclass = &superclass.Metadata;
  EXPECT_EQ(sizeof(HeapObject) + sizeof(void *), subclass.layout(field));
  EXPECT_EQ(4 * sizeof(void *), subclass.Metadata.getInstanceAddressPoint());
  EXPECT_EQ(4 * sizeof(void *) + sizeof(HeapObject) + 2 * sizeof(void *),
            subclass.Metadata.getInstanceSize());
}

TEST(InstancePrefixTest, SubclassPreservesAlignment) {
  const size_t alignment = 4 * sizeof(void *);
  TypeLayout alignedField(sizeof(void *), sizeof(void *),
                          ValueWitnessFlags().withAlignmentMask(alignment - 1),
                          0);
  PrefixLayout superclass;
  EXPECT_EQ(alignment, superclass.layout(alignedField));

  TypeLayout field(sizeof(void *), sizeof(void *),
                   ValueWitnessFlags().withAlignmentMask(alignof(void *) - 1),
                   0);
  PrefixLayout subclass(5 * sizeof(void *));
  subclass.Metadata.Superclass = &superclass.Metadata;
  EXPECT_EQ(alignment + sizeof(void *), subclass.layout(field));
  EXPECT_EQ(alignment - 1, subclass.Metadata.getInstanceAlignMask());
  EXPECT_EQ(2 * alignment, subclass.Metadata.getInstanceAddressPoint());
  EXPECT_EQ(3 * alignment + 2 * sizeof(void *),
            subclass.Metadata.getInstanceSize());
}

TEST(InstancePrefixTest, PrefixSizeOverflow) {
  PrefixLayout fixture;
  // The positive extent fits in InstanceSize; adding the prefix does not.
  TypeLayout field(UINT32_MAX - sizeof(HeapObject), UINT32_MAX,
                   ValueWitnessFlags().withAlignmentMask(0), 0);
  EXPECT_DEATH_IF_SUPPORTED(fixture.layout(field),
                            "exceeds the 32-bit InstanceSize");
}

TEST(InstancePrefixTest, PrefixAlignmentOverflow) {
  PrefixLayout fixture(UINT32_MAX);
  TypeLayout field(
      1, 1, ValueWitnessFlags().withAlignmentMask(alignof(void *) - 1), 0);
  // On 32-bit targets rounding the address point overflows size_t. On 64-bit
  // targets the completed allocation size exceeds the metadata field instead.
  const char *message =
      sizeof(size_t) == sizeof(uint32_t)
          ? "layout size that is too large to be representable"
          : "exceeds the 32-bit InstanceSize";
  EXPECT_DEATH_IF_SUPPORTED(fixture.layout(field), message);
}

TEST(InstancePrefixTest, PrefixAllocationSizeOverflow) {
  PrefixLayout fixture;
  TypeLayout field(SIZE_MAX - sizeof(HeapObject), SIZE_MAX,
                   ValueWitnessFlags().withAlignmentMask(0), 0);
  EXPECT_DEATH_IF_SUPPORTED(fixture.layout(field),
                            "exceeds the 32-bit InstanceSize");
}

TEST(InstancePrefixTest, GenericPattern) {
  struct DescriptorLayout {
    ClassDescriptor Class;
    TypeGenericContextDescriptorHeader Generic;
    GenericParamDescriptor Parameter;
    ClassInstancePrefixDescriptor Prefix;
  };
  // Generic metadata allocations are permanent; keep their descriptor alive.
  alignas(DescriptorLayout) static unsigned char
      storage[sizeof(DescriptorLayout)]{};
  auto &descriptor = *reinterpret_cast<DescriptorLayout *>(storage);
  static std::array<uintptr_t, 2> prefixTemplate{{0x1234, 0x5678}};
  TypeContextDescriptorFlags flags;
  flags.class_setHasInstancePrefix(true);
  descriptor.Class.Flags = ContextDescriptorFlags(
      ContextDescriptorKind::Class, /*isGeneric=*/true, /*isUnique=*/true,
      /*hasInvertibleProtocols=*/false, flags.getOpaqueValue());
  descriptor.Class.MetadataNegativeSizeInWords =
      sizeof(ClassMetadata::HeaderType) / sizeof(void *);
  descriptor.Class.MetadataPositiveSizeInWords =
      sizeof(ClassMetadata) / sizeof(void *) + 1;
  descriptor.Class.NumImmediateMembers = 1;
  descriptor.Generic.Base.NumParams = 1;
  descriptor.Generic.Base.NumKeyArguments = 1;
  descriptor.Parameter = GenericParamDescriptor::implicit();
  descriptor.Prefix.Version = ClassInstancePrefixDescriptor::CurrentVersion;
  descriptor.Prefix.PrefixSizeInWords = prefixTemplate.size();
  using PrefixTemplatePointer = decltype(descriptor.Prefix.PrefixTemplate);
  new (&descriptor.Prefix.PrefixTemplate)
      PrefixTemplatePointer(prefixTemplate.data());
  ASSERT_EQ(&descriptor.Prefix, descriptor.Class.getInstancePrefixDescriptor());

  struct PatternLayout {
    GenericClassMetadataPattern Pattern;
    GenericMetadataPartialPattern Extra;
    std::array<uintptr_t, 32> Data;
  };
  alignas(PatternLayout) unsigned char patternStorage[sizeof(PatternLayout)]{};
  auto &pattern = *reinterpret_cast<PatternLayout *>(patternStorage);
  pattern.Pattern.Flags = ClassFlags::UsesSwiftRefcounting;
  pattern.Pattern.InstancePrefixSizeInWords = prefixTemplate.size();
  pattern.Pattern.PatternFlags.setHasExtraDataPattern(true);
  // Reserve space for ObjC's class RO data, metaclass and metaclass RO data.
  pattern.Pattern.ClassRODataOffset = 0;
  pattern.Pattern.MetaclassObjectOffset = 12;
  pattern.Pattern.MetaclassRODataOffset = 20;
  pattern.Extra.SizeInWords = pattern.Data.size();
  new (&pattern.Extra.Pattern) decltype(pattern.Extra.Pattern)(
      pattern.Data.data());

  const Metadata *arguments[] = {&METADATA_SYM(Bi64_).base};
  auto *metadata = swift_allocateGenericClassMetadata(
      &descriptor.Class, arguments, &pattern.Pattern);
  EXPECT_EQ(2 * sizeof(void *), metadata->getInstanceAddressPoint());
  auto **words = reinterpret_cast<const Metadata **>(metadata);
  EXPECT_EQ(arguments[0], words[descriptor.Class.getGenericArgumentOffset()]);

  const size_t alignment = 4 * sizeof(void *);
  TypeLayout field(sizeof(void *), sizeof(void *),
                   ValueWitnessFlags().withAlignmentMask(alignment - 1), 0);
  const TypeLayout *fields[] = {&field};
  size_t offset = 0;
  initClassFieldOffsetVector(metadata, 1, fields, &offset);
  EXPECT_EQ(alignment, offset);
  EXPECT_EQ(alignment, metadata->getInstanceAddressPoint());
  EXPECT_EQ(2 * alignment + sizeof(void *), metadata->getInstanceSize());
  auto *object = swift_allocObject(metadata, metadata->getInstanceSize(),
                                   metadata->getInstanceAlignMask());
  EXPECT_EQ(0, std::memcmp(reinterpret_cast<unsigned char *>(object) -
                               sizeof(prefixTemplate),
                           prefixTemplate.data(), sizeof(prefixTemplate)));
  swift_deallocUninitializedObject(object, metadata->getInstanceSize(),
                                   metadata->getInstanceAlignMask());
}
