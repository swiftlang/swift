//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "MetadataObjectBuilder.h"
#include "swift/Runtime/Casting.h"
#include "swift/Runtime/ExistentialContainer.h"
#include "swift/Runtime/Metadata.h"
#include "swift/shims/_SwiftCOMShims.h"
#include "gtest/gtest.h"
#include <cstring>
#include <tuple>

using namespace swift;

namespace {
const ModuleContextDescriptor *module() {
  return buildGlobalModuleContextDescriptor([] { return "COMDynamicCast"; });
}

template <unsigned ID>
const ProtocolDescriptor *interface() {
  return buildGlobalObject<ProtocolDescriptor>([](AnyObjectBuilder &builder) {
    ProtocolContextDescriptorFlags flags;
    flags.setClassConstraint(ProtocolClassConstraint::Any);
    flags.setSpecialProtocol(SpecialProtocol::COM);
    auto contextFlags = ContextDescriptorFlags(
        ContextDescriptorKind::Protocol, /*generic=*/false, /*unique=*/true,
        /*hasInvertibleProtocols=*/false, flags.getOpaqueValue());
    builder.add32(contextFlags.getIntValue());
    builder.addRelativeIndirectReference(module(), /*addend=*/1);
    builder.addRelativeReferenceToString(ID == 1   ? "ISource"
                                         : ID == 2 ? "ITarget"
                                                   : "IMissing");
    builder.add32(0); // NumRequirementsInSignature
    builder.add32(0); // NumRequirements
    builder.add32(0); // AssociatedTypeNames
    uint8_t iid[16] = {};
    iid[15] = ID;
    builder.addBytes(iid, sizeof(iid));
  });
}

const ExistentialTypeMetadata *metadata(const ProtocolDescriptor *protocol) {
  ProtocolDescriptorRef protocols[] = {
      ProtocolDescriptorRef::forSwift(protocol)};
  return swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                          /*superclass=*/nullptr, 1, protocols);
}

struct InterfaceVTable {
  _SwiftCOMQueryInterfaceFunction QueryInterface;
  _SwiftCOMLifetimeFunction AddRef;
  _SwiftCOMLifetimeFunction Release;
};

struct TestObject;
struct TestInterface {
  const InterfaceVTable *VTable;
  TestObject *Object;
};

using QueryResult = decltype(swift::QueryInterface(nullptr, nullptr, nullptr));
constexpr QueryResult NoInterface = static_cast<int32_t>(0x80004002u);

struct TestObject {
  TestInterface Source;
  TestInterface Target;
  unsigned References = 1;
  unsigned Queries = 0;
  unsigned AddRefs = 0;
  unsigned Releases = 0;
  QueryResult Success = 0;
  bool NullResult = false;

  static uint32_t __SWIFT_STDCALL AddRef(void *value) {
    auto *object = static_cast<TestInterface *>(value)->Object;
    ++object->AddRefs;
    return ++object->References;
  }

  static uint32_t __SWIFT_STDCALL Release(void *value) {
    auto *object = static_cast<TestInterface *>(value)->Object;
    ++object->Releases;
    return --object->References;
  }

  static QueryResult __SWIFT_STDCALL QueryInterface(void *value,
                                                    const void *iid,
                                                    void **result) {
    auto *object = static_cast<TestInterface *>(value)->Object;
    ++object->Queries;
    *result = nullptr;
    if (!std::memcmp(iid, interface<1>()->getCOMInterfaceID(), 16))
      *result = &object->Source;
    else if (!std::memcmp(iid, interface<2>()->getCOMInterfaceID(), 16))
      *result = &object->Target;
    else
      return NoInterface;

    if (object->NullResult)
      *result = nullptr;
    else
      AddRef(*result);
    return object->Success;
  }
};

class COMDynamicCastTest : public testing::Test {
protected:
  const InterfaceVTable VTable{TestObject::QueryInterface, TestObject::AddRef,
                               TestObject::Release};
  TestObject Object{{&VTable, &Object}, {&VTable, &Object}};
  const ExistentialTypeMetadata *SourceMetadata = metadata(interface<1>());
  const ExistentialTypeMetadata *TargetMetadata = metadata(interface<2>());
  const ExistentialTypeMetadata *MissingMetadata = metadata(interface<3>());
  const ExistentialTypeMetadata *AnyMetadata = &METADATA_SYM(ANY_MANGLING);
  void *Source = &Object.Source;
  OpaqueExistentialContainer Erased{};
  OpaqueValue *SourceLocation = reinterpret_cast<OpaqueValue *>(&Source);
  const Metadata *SourceType = SourceMetadata;

  void prepareSource(bool erase) {
    if (erase) {
      ASSERT_TRUE(swift_dynamicCast(reinterpret_cast<OpaqueValue *>(&Erased),
                                    SourceLocation, SourceMetadata, AnyMetadata,
                                    DynamicCastFlags::TakeOnSuccess));
      SourceLocation = reinterpret_cast<OpaqueValue *>(&Erased);
      SourceType = AnyMetadata;
    }
    ASSERT_EQ(1U, Object.References);
    ASSERT_EQ(0U, Object.Queries);
  }

  bool castTo(void *&destination, const Metadata *target,
              DynamicCastFlags flags = DynamicCastFlags::Default) {
    return swift_dynamicCast(reinterpret_cast<OpaqueValue *>(&destination),
                             SourceLocation, SourceType, target, flags);
  }

  void destroySource() { SourceType->vw_destroy(SourceLocation); }

  void destroyResult(void *&value, const Metadata *type) {
    type->vw_destroy(reinterpret_cast<OpaqueValue *>(&value));
  }

  void TearDown() override {
    EXPECT_EQ(0U, Object.References);
    EXPECT_EQ(Object.AddRefs + 1, Object.Releases);
  }
};

// Exercise each ownership policy both directly and through an Any container.
class COMDynamicCastOwnershipTest
    : public COMDynamicCastTest,
      public testing::WithParamInterface<std::tuple<DynamicCastFlags, bool>> {};

TEST_P(COMDynamicCastOwnershipTest, SuccessOwnsAdjustedInterface) {
  auto [flags, erased] = GetParam();
  ASSERT_NO_FATAL_FAILURE(prepareSource(erased));
  auto addRefs = Object.AddRefs;
  auto releases = Object.Releases;
  bool take = flags & DynamicCastFlags::TakeOnSuccess;
  void *destination = nullptr;

  ASSERT_TRUE(castTo(destination, TargetMetadata, flags));
  EXPECT_EQ(&Object.Target, destination);
  EXPECT_NE(&Object.Source, destination);
  EXPECT_EQ(1U, Object.Queries);
  EXPECT_EQ(addRefs + 1, Object.AddRefs);
  EXPECT_EQ(releases + (take ? 1 : 0), Object.Releases);
  EXPECT_EQ(take ? 1U : 2U, Object.References);

  destroyResult(destination, TargetMetadata);
  if (!take)
    destroySource();
}

TEST_P(COMDynamicCastOwnershipTest, FailurePreservesDestination) {
  auto [flags, erased] = GetParam();
  ASSERT_NO_FATAL_FAILURE(prepareSource(erased));
  auto addRefs = Object.AddRefs;
  auto releases = Object.Releases;
  bool destroy = flags & DynamicCastFlags::DestroyOnFailure;
  int sentinel = 0;
  void *destination = &sentinel;

  EXPECT_FALSE(castTo(destination, MissingMetadata, flags));
  EXPECT_EQ(&sentinel, destination);
  EXPECT_EQ(1U, Object.Queries);
  EXPECT_EQ(addRefs, Object.AddRefs);
  EXPECT_EQ(releases + (destroy ? 1 : 0), Object.Releases);
  EXPECT_EQ(destroy ? 0U : 1U, Object.References);
  if (!destroy)
    destroySource();
}

TEST_P(COMDynamicCastOwnershipTest, SuccessWithNullResultFails) {
  auto [flags, erased] = GetParam();
  ASSERT_NO_FATAL_FAILURE(prepareSource(erased));
  Object.NullResult = true;
  int sentinel = 0;
  void *destination = &sentinel;

  EXPECT_FALSE(castTo(destination, TargetMetadata, flags));
  EXPECT_EQ(&sentinel, destination);
  EXPECT_EQ(1U, Object.Queries);
  EXPECT_EQ((flags & DynamicCastFlags::DestroyOnFailure) ? 0U : 1U,
            Object.References);
  if (!(flags & DynamicCastFlags::DestroyOnFailure))
    destroySource();
}

INSTANTIATE_TEST_SUITE_P(
    Ownership, COMDynamicCastOwnershipTest,
    testing::Combine(testing::Values(DynamicCastFlags::Default,
                                     DynamicCastFlags::TakeOnSuccess,
                                     DynamicCastFlags::DestroyOnFailure,
                                     DynamicCastFlags::TakeOnSuccess |
                                         DynamicCastFlags::DestroyOnFailure),
                     testing::Bool()));

TEST_F(COMDynamicCastTest, UnconditionalTakingCast) {
  void *destination = nullptr;
  ASSERT_TRUE(castTo(destination, TargetMetadata,
                     DynamicCastFlags::Unconditional |
                         DynamicCastFlags::TakeOnSuccess));
  EXPECT_EQ(&Object.Target, destination);
  EXPECT_EQ(1U, Object.Queries);
  EXPECT_EQ(1U, Object.AddRefs);
  EXPECT_EQ(1U, Object.Releases);
  EXPECT_EQ(1U, Object.References);
  destroyResult(destination, TargetMetadata);
}

TEST_F(COMDynamicCastTest, NonnegativeStatusSucceeds) {
  Object.Success = 1;
  void *destination = nullptr;
  ASSERT_TRUE(castTo(destination, TargetMetadata));
  EXPECT_EQ(&Object.Target, destination);
  destroyResult(destination, TargetMetadata);
  destroySource();
}

TEST_F(COMDynamicCastTest, QueryFromAdjustedInterface) {
  Source = &Object.Target;
  SourceType = TargetMetadata;
  void *destination = nullptr;
  ASSERT_TRUE(castTo(destination, SourceMetadata));
  EXPECT_EQ(&Object.Source, destination);
  EXPECT_EQ(1U, Object.Queries);
  destroyResult(destination, SourceMetadata);
  destroySource();
}

TEST_F(COMDynamicCastTest, NullSourceDoesNotQuery) {
  Source = nullptr;
  int sentinel = 0;
  void *destination = &sentinel;
  EXPECT_FALSE(castTo(destination, TargetMetadata));
  EXPECT_EQ(&sentinel, destination);
  EXPECT_EQ(0U, Object.Queries);
  TestObject::Release(&Object.Source);
}

TEST_F(COMDynamicCastTest, NonCOMSourceDoesNotQuery) {
  int64_t value = 0;
  int sentinel = 0;
  void *destination = &sentinel;
  EXPECT_FALSE(swift_dynamicCast(reinterpret_cast<OpaqueValue *>(&destination),
                                 reinterpret_cast<OpaqueValue *>(&value),
                                 &METADATA_SYM(Bi64_).base, TargetMetadata,
                                 DynamicCastFlags::Default));
  EXPECT_EQ(&sentinel, destination);
  EXPECT_EQ(0U, Object.Queries);
  destroySource();
}

} // namespace
