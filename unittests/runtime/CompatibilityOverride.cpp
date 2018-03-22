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

static const Metadata *
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

static bool DynamicCastOverrideFunc(OpaqueValue *dest, OpaqueValue *src,
                                    const Metadata *srcType,
                                    const Metadata *targetType,
                                    DynamicCastFlags flags,
                                    DynamicCastOriginal originalImpl) {
  if (!EnableOverride)
    return originalImpl(dest, src, srcType, targetType, flags);
  Ran = true;
  return true;
}

#define CAST_FUNC(name, rettype, objtype, metatypetype) \
  static const rettype *name ## OverrideFunc(           \
    const objtype *object,                              \
    const metatypetype *targetType,                     \
    name ## Original originalImpl) {                    \
    if (!EnableOverride)                                \
      return originalImpl(object, targetType);          \
    Ran = true;                                         \
    return nullptr;                                     \
  }

#define CAST(name) CAST_FUNC(name, void, void, ClassMetadata)
#define FOREIGN_CAST(name) CAST_FUNC(name, void, void, ForeignClassMetadata)
#define CLASSMETATYPE_CAST(name) \
  CAST_FUNC(name, ClassMetadata, ClassMetadata, ClassMetadata)
  #define UNKNOWN_CAST(name) CAST_FUNC(name, void, void, Metadata)
#define METATYPE_CAST(name) CAST_FUNC(name, Metadata, Metadata, Metadata)

CAST(DynamicCastClass)
CAST(DynamicCastClassUnconditional)
CAST(DynamicCastObjCClass)
CAST(DynamicCastObjCClassUnconditional)
FOREIGN_CAST(DynamicCastForeignClass)
FOREIGN_CAST(DynamicCastForeignClassUnconditional)
UNKNOWN_CAST(DynamicCastUnknownClass)
UNKNOWN_CAST(DynamicCastUnknownClassUnconditional)
METATYPE_CAST(DynamicCastMetatype)
METATYPE_CAST(DynamicCastMetatypeUnconditional)
CLASSMETATYPE_CAST(DynamicCastObjCClassMetatype)
CLASSMETATYPE_CAST(DynamicCastObjCClassMetatypeUnconditional)
CLASSMETATYPE_CAST(DynamicCastForeignClassMetatype)
CLASSMETATYPE_CAST(DynamicCastForeignClassMetatypeUnconditional)

static const WitnessTable *
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
  
#define FIELD(typename) typename typename ## Fptr
  FIELD(GetTypeByMangledNameOverride);
  FIELD(DynamicCastOverride);
  FIELD(DynamicCastClassOverride);
  FIELD(DynamicCastClassUnconditionalOverride);
  FIELD(DynamicCastObjCClassOverride);
  FIELD(DynamicCastObjCClassUnconditionalOverride);
  FIELD(DynamicCastForeignClassOverride);
  FIELD(DynamicCastForeignClassUnconditionalOverride);
  FIELD(DynamicCastUnknownClassOverride);
  FIELD(DynamicCastUnknownClassUnconditionalOverride);
  FIELD(DynamicCastMetatypeOverride);
  FIELD(DynamicCastMetatypeUnconditionalOverride);
  FIELD(DynamicCastObjCClassMetatypeOverride);
  FIELD(DynamicCastObjCClassMetatypeUnconditionalOverride);
  FIELD(DynamicCastForeignClassMetatypeOverride);
  FIELD(DynamicCastForeignClassMetatypeUnconditionalOverride);
  FIELD(ConformsToProtocolOverride);
};

OverrideSection Overrides __attribute__((section("__DATA,__swift_lite"))) = {
  0,
  GetTypeByMangledNameOverrideFunc,
  DynamicCastOverrideFunc,
  DynamicCastClassOverrideFunc,
  DynamicCastClassUnconditionalOverrideFunc,
  DynamicCastObjCClassOverrideFunc,
  DynamicCastObjCClassUnconditionalOverrideFunc,
  DynamicCastForeignClassOverrideFunc,
  DynamicCastForeignClassUnconditionalOverrideFunc,
  DynamicCastUnknownClassOverrideFunc,
  DynamicCastUnknownClassUnconditionalOverrideFunc,
  DynamicCastMetatypeOverrideFunc,
  DynamicCastMetatypeUnconditionalOverrideFunc,
  DynamicCastObjCClassMetatypeOverrideFunc,
  DynamicCastObjCClassMetatypeUnconditionalOverrideFunc,
  DynamicCastForeignClassMetatypeOverrideFunc,
  DynamicCastForeignClassMetatypeUnconditionalOverrideFunc,
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
    // This check is a bit pointless, as long as you trust the section
    // attribute to work correctly, but it ensures that the override
    // section actually gets linked into the test executable. If it's not
    // used then it disappears and the real tests begin to fail.
    ASSERT_EQ(Overrides.version, static_cast<uintptr_t>(0));
    
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

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCastClass) {
  auto Result = swift_dynamicCastClass(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCastClassUnconditional) {
  auto Result = swift_dynamicCastClassUnconditional(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCastObjCClass) {
  auto Result = swift_dynamicCastObjCClass(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCastObjCClassUnconditional) {
  auto Result = swift_dynamicCastObjCClassUnconditional(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCastForeignClass) {
  auto Result = swift_dynamicCastForeignClass(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCastForeignClassUnconditional) {
  auto Result = swift_dynamicCastForeignClassUnconditional(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCastUnknownClass) {
  auto Result = swift_dynamicCastUnknownClass(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCastUnknownClassUnconditional) {
  auto Result = swift_dynamicCastUnknownClassUnconditional(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCastMetatype) {
  auto Result = swift_dynamicCastMetatype(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCastMetatypeUnconditional) {
  auto Result = swift_dynamicCastMetatypeUnconditional(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCastObjCClassMetatype) {
  auto Result = swift_dynamicCastObjCClassMetatype(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCastObjCClassMetatypeUnconditional) {
  auto Result = swift_dynamicCastObjCClassMetatypeUnconditional(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCastForeignClassMetatype) {
  auto Result = swift_dynamicCastForeignClassMetatype(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest,
       test_swift_dynamicCastForeignClassMetatypeUnconditional) {
  auto Result = swift_dynamicCastForeignClassMetatypeUnconditional(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

TEST_F(CompatibilityOverrideTest, test_swift_conformsToProtocol) {
  auto Result = swift_conformsToProtocol(nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);  
}
