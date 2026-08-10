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

#include "swift/Runtime/Casting.h"
#include "swift/Runtime/ExistentialContainer.h"
#include "swift/Runtime/Metadata.h"
#include <gtest/gtest.h>

using namespace swift;

namespace {
constexpr int32_t S_OK = 0;
constexpr int32_t E_NOINTERFACE = 0x80004002;

const uint8_t IID_ISource[16] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x11,
};
const uint8_t IID_ITarget[16] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x22,
};
const uint8_t IID_IMissing[16] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x33,
};

struct ModuleContextDescriptorStorage {
  ContextDescriptorFlags Flags;
  TargetRelativeContextPointer<InProcess> Parent;
  RelativeDirectPointer<const char, /*nullable=*/false> Name;

  explicit ModuleContextDescriptorStorage(const char *name)
      : Flags(ContextDescriptorKind::Module, /*isGeneric=*/false,
              /*isUnique=*/true, /*hasInvertibleProtocols=*/false,
              /*kindSpecificFlags=*/0),
        Parent(nullptr), Name(name) {}

  ModuleContextDescriptor *getDescriptor() {
    return reinterpret_cast<ModuleContextDescriptor *>(this);
  }
};

static_assert(sizeof(ModuleContextDescriptorStorage) == sizeof(ModuleContextDescriptor));

ModuleContextDescriptorStorage ModuleStorage{"COMDynamicCast"};

struct COMProtocolDescriptorStorage {
  ContextDescriptorFlags Flags;
  TargetRelativeContextPointer<InProcess> Parent;
  RelativeDirectPointer<const char, /*nullable=*/false> Name;
  uint32_t NumRequirementsInSignature;
  uint32_t NumRequirements;
  RelativeDirectPointer<const char, /*nullable=*/true> AssociatedTypeNames;
  TargetCOMInterfaceID<InProcess> InterfaceID;

  static ContextDescriptorFlags getFlags() {
    ProtocolContextDescriptorFlags protocolFlags;
    protocolFlags.setClassConstraint(ProtocolClassConstraint::Any);
    protocolFlags.setSpecialProtocol(SpecialProtocol::COM);
    return ContextDescriptorFlags(ContextDescriptorKind::Protocol,
                                  /*isGeneric=*/false, /*isUnique=*/true,
                                  /*hasInvertibleProtocols=*/false,
                                  protocolFlags.getOpaqueValue());
  }

  COMProtocolDescriptorStorage(const char *name, const uint8_t *iid)
      : Flags(getFlags()), Parent(ModuleStorage.getDescriptor()), Name(name),
        NumRequirementsInSignature(0), NumRequirements(0),
        AssociatedTypeNames(nullptr) {
    std::memcpy(InterfaceID.Bytes, iid, sizeof(InterfaceID.Bytes));
  }

  ProtocolDescriptor *getDescriptor() {
    return reinterpret_cast<ProtocolDescriptor *>(this);
  }
};

static_assert(sizeof(COMProtocolDescriptorStorage) == sizeof(ProtocolDescriptor) + sizeof(TargetCOMInterfaceID<InProcess>));

COMProtocolDescriptorStorage
SourceProtocolStorage{"SourceInterface", IID_ISource};

COMProtocolDescriptorStorage
TargetProtocolStorage{"TargetInterface", IID_ITarget};

COMProtocolDescriptorStorage
MissingProtocolStorage{"MissingInterface", IID_IMissing};

struct PlainProtocolDescriptorStorage {
  ContextDescriptorFlags Flags;
  TargetRelativeContextPointer<InProcess> Parent;
  RelativeDirectPointer<const char, /*nullable=*/false> Name;
  uint32_t NumRequirementsInSignature;
  uint32_t NumRequirements;
  RelativeDirectPointer<const char, /*nullable=*/true> AssociatedTypeNames;

  static ContextDescriptorFlags getFlags() {
    ProtocolContextDescriptorFlags protocolFlags;
    protocolFlags.setClassConstraint(ProtocolClassConstraint::Any);
    return ContextDescriptorFlags(ContextDescriptorKind::Protocol,
                                  /*isGeneric=*/false, /*isUnique=*/true,
                                  /*hasInvertibleProtocols=*/false,
                                  protocolFlags.getOpaqueValue());
  }

  explicit PlainProtocolDescriptorStorage(const char *name)
      : Flags(getFlags()), Parent(ModuleStorage.getDescriptor()), Name(name),
        NumRequirementsInSignature(0), NumRequirements(0),
        AssociatedTypeNames(nullptr) {}

  ProtocolDescriptor *getDescriptor() {
    return reinterpret_cast<ProtocolDescriptor *>(this);
  }
};

static_assert(sizeof(PlainProtocolDescriptorStorage) == sizeof(ProtocolDescriptor));

// Marker-ness is an AST property; at runtime this is an ordinary non-COM
// protocol descriptor alongside the one ABI-bearing interface descriptor.
PlainProtocolDescriptorStorage MarkerProtocolStorage{"MarkerProtocol"};

const ExistentialTypeMetadata *
getCOMExistentialMetadata(ProtocolDescriptor *protocol) {
  ProtocolDescriptorRef protocols[] = {
      ProtocolDescriptorRef::forSwift(protocol),
  };
  auto *metadata =
      swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                       /*superclass=*/nullptr, 1, protocols);
  return cast<ExistentialTypeMetadata>(metadata);
}

struct COMExistentialMetadataWithMarker {
  ExistentialTypeMetadata::HeaderType Header;
  ExistentialTypeMetadata Metadata;
  ProtocolDescriptorRef Protocols[2];

  COMExistentialMetadataWithMarker(const ValueWitnessTable *valueWitnesses,
                                   ProtocolDescriptor *markerProtocol,
                                   ProtocolDescriptor *interfaceProtocol)
      : Header{valueWitnesses},
        Metadata(ExistentialTypeFlags()
                     .withNumWitnessTables(0)
                     .withClassConstraint(ProtocolClassConstraint::Any)
                     .withSpecialProtocol(SpecialProtocol::COM)),
        Protocols{
            ProtocolDescriptorRef::forSwift(markerProtocol),
            ProtocolDescriptorRef::forSwift(interfaceProtocol),
        } {
    Metadata.NumProtocols = 2;
  }
};

struct COMTestObject;

struct COMTestInterface {
  void **VTable;
  COMTestObject *Object;
};

struct COMTestObject {
  COMTestInterface Source;
  COMTestInterface Target;
  unsigned RefCount = 1;
  unsigned QueryInterfaceCalls = 0;
  unsigned AddRefCalls = 0;
  unsigned ReleaseCalls = 0;
};

uint32_t AddRef(void *pUnk) {
  auto *interface = static_cast<COMTestInterface *>(pUnk);
  ++interface->Object->AddRefCalls;
  return ++interface->Object->RefCount;
}

uint32_t Release(void *pUnk) {
  auto *interface = static_cast<COMTestInterface *>(pUnk);
  ++interface->Object->ReleaseCalls;
  return --interface->Object->RefCount;
}

int32_t QueryInterface(void *pUnk, const uint8_t *riid, void **lpvObject) {
  auto *interface = static_cast<COMTestInterface *>(pUnk);
  auto *object = interface->Object;
  ++object->QueryInterfaceCalls;
  *lpvObject = nullptr;

  if (std::memcmp(riid, IID_ITarget, sizeof(IID_ITarget)))
    return E_NOINTERFACE;

  *lpvObject = &object->Target;
  AddRef(*lpvObject);
  return S_OK;
}

class COMDynamicCastTest : public testing::Test {
protected:
  void *VTable[3] = {
      reinterpret_cast<void *>(&QueryInterface),
      reinterpret_cast<void *>(&AddRef),
      reinterpret_cast<void *>(&Release),
  };
  COMTestObject Object{
      {VTable, &Object},
      {VTable, &Object},
  };

  const ExistentialTypeMetadata *SourceMetadata =
      getCOMExistentialMetadata(SourceProtocolStorage.getDescriptor());
  const ExistentialTypeMetadata *TargetMetadata =
      getCOMExistentialMetadata(TargetProtocolStorage.getDescriptor());
  const ExistentialTypeMetadata *MissingMetadata =
      getCOMExistentialMetadata(MissingProtocolStorage.getDescriptor());
  COMExistentialMetadataWithMarker MarkerTargetStorage{
      TargetMetadata->getValueWitnesses(),
      MarkerProtocolStorage.getDescriptor(),
      TargetProtocolStorage.getDescriptor()
  };

  void *source() { return &Object.Source; }
};

TEST_F(COMDynamicCastTest, ConditionalCastUsesQueryInterface) {
  void *sourceValue = source();
  void *destination = nullptr;

  EXPECT_TRUE(swift_dynamicCast(reinterpret_cast<OpaqueValue *>(&destination),
                                reinterpret_cast<OpaqueValue *>(&sourceValue),
                                SourceMetadata, TargetMetadata,
                                DynamicCastFlags::Default));
  EXPECT_EQ(&Object.Target, destination);
  EXPECT_EQ(1U, Object.QueryInterfaceCalls);
  EXPECT_EQ(1U, Object.AddRefCalls);
  EXPECT_EQ(2U, Object.RefCount);

  TargetMetadata->vw_destroy(reinterpret_cast<OpaqueValue *>(&destination));
  EXPECT_EQ(1U, Object.ReleaseCalls);
  EXPECT_EQ(1U, Object.RefCount);

  SourceMetadata->vw_destroy(reinterpret_cast<OpaqueValue *>(&sourceValue));
  EXPECT_EQ(2U, Object.ReleaseCalls);
  EXPECT_EQ(0U, Object.RefCount);
}

TEST_F(COMDynamicCastTest, ConditionalCastFailurePreservesSource) {
  void *sourceValue = source();
  void *destination = reinterpret_cast<void *>(uintptr_t(1));

  EXPECT_FALSE(swift_dynamicCast(reinterpret_cast<OpaqueValue *>(&destination),
                                 reinterpret_cast<OpaqueValue *>(&sourceValue),
                                 SourceMetadata, MissingMetadata,
                                 DynamicCastFlags::Default));
  EXPECT_EQ(reinterpret_cast<void *>(uintptr_t(1)), destination);
  EXPECT_EQ(1U, Object.QueryInterfaceCalls);
  EXPECT_EQ(0U, Object.AddRefCalls);
  EXPECT_EQ(0U, Object.ReleaseCalls);
  EXPECT_EQ(1U, Object.RefCount);

  SourceMetadata->vw_destroy(reinterpret_cast<OpaqueValue *>(&sourceValue));
  EXPECT_EQ(0U, Object.RefCount);
}

TEST_F(COMDynamicCastTest, FailedConsumingCastDestroysSource) {
  void *sourceValue = source();
  void *destination = nullptr;

  EXPECT_FALSE(swift_dynamicCast(reinterpret_cast<OpaqueValue *>(&destination),
                                 reinterpret_cast<OpaqueValue *>(&sourceValue),
                                 SourceMetadata, MissingMetadata,
                                 DynamicCastFlags::DestroyOnFailure));
  EXPECT_EQ(nullptr, destination);
  EXPECT_EQ(1U, Object.QueryInterfaceCalls);
  EXPECT_EQ(0U, Object.AddRefCalls);
  EXPECT_EQ(1U, Object.ReleaseCalls);
  EXPECT_EQ(0U, Object.RefCount);
}

TEST_F(COMDynamicCastTest, UnconditionalTakingCastBalancesSourceOwnership) {
  void *sourceValue = source();
  void *destination = nullptr;

  EXPECT_TRUE(swift_dynamicCast(reinterpret_cast<OpaqueValue *>(&destination),
                                reinterpret_cast<OpaqueValue *>(&sourceValue),
                                SourceMetadata, TargetMetadata,
                                DynamicCastFlags::Unconditional | DynamicCastFlags::TakeOnSuccess));
  EXPECT_EQ(&Object.Target, destination);
  EXPECT_EQ(1U, Object.QueryInterfaceCalls);
  EXPECT_EQ(1U, Object.AddRefCalls);
  EXPECT_EQ(1U, Object.ReleaseCalls);
  EXPECT_EQ(1U, Object.RefCount);

  TargetMetadata->vw_destroy(reinterpret_cast<OpaqueValue *>(&destination));
  EXPECT_EQ(2U, Object.ReleaseCalls);
  EXPECT_EQ(0U, Object.RefCount);
}

TEST_F(COMDynamicCastTest, ProbeReleasesSuccessfulQueryResult) {
  void *sourceValue = source();
  void *probe = nullptr;

  ASSERT_TRUE(swift_dynamicCast(reinterpret_cast<OpaqueValue *>(&probe),
                                reinterpret_cast<OpaqueValue *>(&sourceValue),
                                SourceMetadata, TargetMetadata,
                                DynamicCastFlags::Default));
  TargetMetadata->vw_destroy(reinterpret_cast<OpaqueValue *>(&probe));

  EXPECT_EQ(1U, Object.QueryInterfaceCalls);
  EXPECT_EQ(1U, Object.AddRefCalls);
  EXPECT_EQ(1U, Object.ReleaseCalls);
  EXPECT_EQ(1U, Object.RefCount);

  SourceMetadata->vw_destroy(reinterpret_cast<OpaqueValue *>(&sourceValue));
  EXPECT_EQ(0U, Object.RefCount);
}

TEST_F(COMDynamicCastTest, CastUnwrapsCOMExistentialStoredInAny) {
  void *sourceValue = source();
  OpaqueExistentialContainer erased{};
  const ExistentialTypeMetadata *anyMetadata = &METADATA_SYM(ANY_MANGLING);

  ASSERT_TRUE(swift_dynamicCast(reinterpret_cast<OpaqueValue *>(&erased),
                                reinterpret_cast<OpaqueValue *>(&sourceValue),
                                SourceMetadata, anyMetadata,
                                DynamicCastFlags::Default));
  EXPECT_EQ(1U, Object.AddRefCalls);
  EXPECT_EQ(2U, Object.RefCount);

  void *destination = nullptr;
  ASSERT_TRUE(swift_dynamicCast(reinterpret_cast<OpaqueValue *>(&destination),
                                reinterpret_cast<OpaqueValue *>(&erased),
                                anyMetadata, TargetMetadata,
                                DynamicCastFlags::Default));
  EXPECT_EQ(&Object.Target, destination);
  EXPECT_EQ(1U, Object.QueryInterfaceCalls);
  EXPECT_EQ(2U, Object.AddRefCalls);
  EXPECT_EQ(3U, Object.RefCount);

  TargetMetadata->vw_destroy(reinterpret_cast<OpaqueValue *>(&destination));
  anyMetadata->vw_destroy(reinterpret_cast<OpaqueValue *>(&erased));
  SourceMetadata->vw_destroy(reinterpret_cast<OpaqueValue *>(&sourceValue));
  EXPECT_EQ(3U, Object.ReleaseCalls);
  EXPECT_EQ(0U, Object.RefCount);
}

TEST_F(COMDynamicCastTest, CastIgnoresMarkerProtocolDescriptor) {
  void *sourceValue = source();
  void *destination = nullptr;

  ASSERT_TRUE(swift_dynamicCast(reinterpret_cast<OpaqueValue *>(&destination),
                                reinterpret_cast<OpaqueValue *>(&sourceValue),
                                SourceMetadata, &MarkerTargetStorage.Metadata,
                                DynamicCastFlags::Default));
  EXPECT_EQ(&Object.Target, destination);
  EXPECT_EQ(1U, Object.QueryInterfaceCalls);

  MarkerTargetStorage.Metadata.vw_destroy(
      reinterpret_cast<OpaqueValue *>(&destination));
  SourceMetadata->vw_destroy(reinterpret_cast<OpaqueValue *>(&sourceValue));
  EXPECT_EQ(0U, Object.RefCount);
}
}
