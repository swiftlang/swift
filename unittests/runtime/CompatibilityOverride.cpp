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

#if defined(__APPLE__) && defined(__MACH__)

#include "../../stdlib/public/runtime/CompatibilityOverride.h"
#include "swift/Runtime/Casting.h"
#include "swift/Runtime/Metadata.h"
#include "gtest/gtest.h"

#include <stdio.h>

using namespace swift;

bool EnableOverride;
bool Ran;

namespace  {
  template<typename T>
  T getEmptyValue() {
    return (T)0;
  }

  template<>
  MetadataResponse getEmptyValue<MetadataResponse>() {
    return MetadataResponse{nullptr, MetadataState::Complete};
  }
}

#define OVERRIDE(name, ret, attrs, namespace, typedArgs, namedArgs) \
  static ret name ## Override(COMPATIBILITY_UNPAREN typedArgs,      \
                          Original_ ## name originalImpl) {         \
    if (!EnableOverride)                                            \
      return originalImpl namedArgs;                                \
    Ran = true;                                                     \
    return getEmptyValue<ret>();                                    \
  }
#include "../../stdlib/public/runtime/CompatibilityOverride.def"


struct OverrideSection {
  uintptr_t version;
  
#define OVERRIDE(name, ret, attrs, namespace, typedArgs, namedArgs) \
  Override_ ## name name;
#include "../../stdlib/public/runtime/CompatibilityOverride.def"
};

OverrideSection Overrides __attribute__((section("__DATA,__swift_hooks"))) = {
  0,
#define OVERRIDE(name, ret, attrs, namespace, typedArgs, namedArgs) \
  name ## Override,
#include "../../stdlib/public/runtime/CompatibilityOverride.def"
};

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

TEST_F(CompatibilityOverrideTest, test_swift_dynamicCast) {
  auto Result = swift_dynamicCast(nullptr, nullptr, nullptr, nullptr,
                                  DynamicCastFlags::Default);
  ASSERT_FALSE(Result);
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

TEST_F(CompatibilityOverrideTest, test_swift_getAssociatedTypeWitnessSlow) {
  auto Result = swift_getAssociatedTypeWitnessSlow(MetadataState::Complete,
                                                   nullptr, nullptr,
                                                   nullptr, nullptr);
  ASSERT_EQ(Result.Value, nullptr);
  ASSERT_EQ(Result.State, MetadataState::Complete);
}

TEST_F(CompatibilityOverrideTest,
       test_swift_getAssociatedConformanceWitnessSlow) {
  auto Result = swift_getAssociatedConformanceWitnessSlow(
                                                   nullptr, nullptr, nullptr,
                                                   nullptr, nullptr);
  ASSERT_EQ(Result, nullptr);
}

#endif
