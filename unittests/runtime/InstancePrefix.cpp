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

#include "swift/ABI/Metadata.h"
#include "gtest/gtest.h"
#include <array>

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
