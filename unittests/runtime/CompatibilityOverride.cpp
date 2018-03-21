//===--- CompatibilityOverride.cpp - Compatibility override tests ---------===//
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

#include "../../stdlib/public/runtime/CompatibilityOverride.h"
#include "swift/Runtime/Casting.h"
#include "swift/Runtime/Metadata.h"
#include "gtest/gtest.h"

#include <stdio.h>

using namespace swift;

bool EnableOverride;
bool Ran;

const Metadata *
GetTypeByMangledNameOverrideFunc(const char *typeNameStart,
                                 size_t typeNameLength,
                                 size_t numberOfLevels,
                                 size_t *parametersPerLevel,
                                 const Metadata * const *flatSubstitutions,
                                 GetTypeByMangledNameOriginal originalImpl) {
  if (!EnableOverride)
    return originalImpl(typeNameStart,
                        typeNameLength,
                        numberOfLevels,
                        parametersPerLevel,
                        flatSubstitutions);
  Ran = true;
  return nullptr;
}

bool DynamicCastOverrideFunc(OpaqueValue *dest, OpaqueValue *src,
                             const Metadata *srcType,
                             const Metadata *targetType,
                             DynamicCastFlags flags,
                             DynamicCastOriginal originalImpl) {
  if (!EnableOverride)
    return originalImpl(dest, src, srcType, targetType, flags);
  Ran = true;
  return true;
}

const WitnessTable *
ConformsToProtocolOverrideFunc(const Metadata * const type,
                               const ProtocolDescriptor *protocol,
                               ConformsToProtocolOriginal originalImpl) {
  if (!EnableOverride)
    return originalImpl(type, protocol);
  Ran = true;
  return nullptr;
}

struct OverrideSection {
  uintptr_t version;
  
  GetTypeByMangledNameOverride mangledNameOverride;
  DynamicCastOverride dynamicCastOverride;
  ConformsToProtocolOverride conformsToProtocolOverride;
};

OverrideSection Overrides __attribute__((section("__DATA,__swift_lite"))) = {
  0,
  GetTypeByMangledNameOverrideFunc,
  DynamicCastOverrideFunc,
  ConformsToProtocolOverrideFunc
};

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
const Metadata * _Nullable
swift_getTypeByMangledName(const char *typeNameStart, size_t typeNameLength,
                           size_t numberOfLevels,
                           size_t *parametersPerLevel,
                           const Metadata * const *flatSubstitutions);

class CompatibilityOverrideTest : public ::testing::Test {
protected:
  virtual void SetUp() {
    // These checks are a bit pointless, as long as you trust the section
    // attribute to work correctly, but they ensure that the override
    // section actually gets linked into the test executable. If it's not
    // used then it disappears and the real tests begin to fail.
    ASSERT_EQ(Overrides.version, static_cast<uintptr_t>(0));
    ASSERT_TRUE(Overrides.mangledNameOverride == GetTypeByMangledNameOverrideFunc);
    ASSERT_TRUE(Overrides.dynamicCastOverride == DynamicCastOverrideFunc);
    
    EnableOverride = true;
    Ran = false;
  }
  
  virtual void TearDown() {
    EnableOverride = false;
    ASSERT_TRUE(Ran);
  }
};

TEST_F(CompatibilityOverrideTest, test_swift_getTypeByMangledName) {
  auto Result = swift_getTypeByMangledName("", 0, 0, nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCast) {
  auto Result = swift_dynamicCast(nullptr, nullptr, nullptr, nullptr,
                                  DynamicCastFlags::Default);
  ASSERT_TRUE(Result);
}

TEST_F(CompatibilityOverrideTest, test_swift_conformsToProtocol) {
  auto Result = swift_conformsToProtocol(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);  
}
