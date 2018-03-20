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

bool RanBefore;
bool RanAfter;

TypeInfo GetTypeByMangledNameOverrideFunc(StringRef typeName,
                                          SubstGenericParameterFn substGenericParam,
                                          GetTypeByMangledNameOriginal originalImpl) {
  RanBefore = true;
  auto Result = originalImpl(typeName, substGenericParam);
  RanAfter = true;
  return Result;
}

bool DynamicCastOverrideFunc(OpaqueValue *dest, OpaqueValue *src,
                             const Metadata *srcType,
                             const Metadata *targetType,
                             DynamicCastFlags flags,
                             DynamicCastOriginal originalImpl) {
  RanBefore = true;
  auto Result = originalImpl(dest, src, srcType, targetType, flags);
  RanAfter = true;
  return Result;
}

struct OverrideSection {
  uintptr_t version;
  
  GetTypeByMangledNameOverride mangledNameOverride;
  DynamicCastOverride dynamicCastOverride;
};

OverrideSection Overrides __attribute__((section("__DATA,__swift_lite"))) = {
  0,
  GetTypeByMangledNameOverrideFunc,
  DynamicCastOverrideFunc
};

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
const Metadata * _Nullable
swift_getTypeByMangledName(const char *typeNameStart, size_t typeNameLength,
                           size_t numberOfLevels,
                           size_t *parametersPerLevel,
                           const Metadata * const *flatSubstitutions);

TEST(CompatibilityOverride, test_overrideSectionContents) {
  // These checks are a bit pointless, as long as you trust the section
  // attribute to work correctly, but they ensure that the override
  // section actually gets linked into the test executable. If it's not
  // used then it disappears and the real tests begin to fail.
  ASSERT_EQ(Overrides.version, static_cast<uintptr_t>(0));
  ASSERT_TRUE(Overrides.mangledNameOverride == GetTypeByMangledNameOverrideFunc);
  ASSERT_TRUE(Overrides.dynamicCastOverride == DynamicCastOverrideFunc);
}

TEST(CompatibilityOverride, test_swift_getTypeByMangledName) {
  RanBefore = false;
  RanAfter = false;
  auto Result = swift_getTypeByMangledName("Si", 2, 0, nullptr, nullptr);
  ASSERT_TRUE(RanBefore);
  ASSERT_TRUE(RanAfter);
  ASSERT_NE(Result, nullptr);
}

TEST(CompatibilityOverride, test_swift_dynamicCast) {
  auto Int = swift_getTypeByMangledName("Si", 2, 0, nullptr, nullptr);
  intptr_t x = 0xdeadbeef;
  intptr_t y;
  
  RanBefore = false;
  RanAfter = false;
  auto Result = swift_dynamicCast(reinterpret_cast<OpaqueValue *>(&x),
                                  reinterpret_cast<OpaqueValue *>(&y),
                                  Int, Int, DynamicCastFlags::Default);
  ASSERT_TRUE(RanBefore);
  ASSERT_TRUE(RanAfter);
  ASSERT_TRUE(Result);
}
