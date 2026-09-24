//===--- COMMetadata.cpp - COM existential metadata tests -----------------===//
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
#include "swift/Runtime/Metadata.h"
#include "swift/shims/_SwiftCOMShims.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {
const ModuleContextDescriptor *module() {
  return buildGlobalModuleContextDescriptor([] { return "COMMetadata"; });
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
    builder.addRelativeReferenceToString(ID == 1 ? "IFirst" : "ISecond");
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

struct Interface {
  const InterfaceVTable *VTable;
  unsigned References = 1;
  unsigned AddRefs = 0;
  unsigned Releases = 0;
};

uint32_t __SWIFT_STDCALL addRef(void *value) {
  auto *interface = static_cast<Interface *>(value);
  ++interface->AddRefs;
  return ++interface->References;
}

uint32_t __SWIFT_STDCALL release(void *value) {
  auto *interface = static_cast<Interface *>(value);
  ++interface->Releases;
  return --interface->References;
}
} // namespace

TEST(COMMetadataTest, InterfaceIdentity) {
  const auto *first = interface<1>();
  const auto *second = interface<2>();
  ASSERT_NE(nullptr, first->getCOMInterfaceID());
  ASSERT_NE(nullptr, second->getCOMInterfaceID());
  EXPECT_EQ(1, first->getCOMInterfaceID()[15]);
  EXPECT_EQ(2, second->getCOMInterfaceID()[15]);
  auto *native =
      buildGlobalProtocolDescriptor(module(), [] { return "Native"; });
  EXPECT_EQ(nullptr, native->getCOMInterfaceID());
}

TEST(COMMetadataTest, SharedValueWitnesses) {
  const auto *first = metadata(interface<1>());
  const auto *second = metadata(interface<2>());
  EXPECT_NE(first, second);
  EXPECT_EQ(first->getValueWitnesses(), second->getValueWitnesses());
  EXPECT_EQ(first, metadata(interface<1>()));
  EXPECT_EQ(ExistentialTypeRepresentation::COM, first->getRepresentation());
  EXPECT_EQ(0U, first->Flags.getNumWitnessTables());

  const auto *witnesses = first->getValueWitnesses();
  EXPECT_EQ(sizeof(void *), witnesses->getSize());
  EXPECT_EQ(sizeof(void *), witnesses->getStride());
  EXPECT_EQ(alignof(void *), witnesses->getAlignment());
  EXPECT_FALSE(witnesses->isPOD());
  EXPECT_TRUE(witnesses->isBitwiseTakable());
}

TEST(COMMetadataTest, Ownership) {
  const auto *type = metadata(interface<1>());
  const InterfaceVTable vtable{nullptr, addRef, release};
  Interface object{&vtable};
  void *source = &object;
  void *destination = nullptr;
  type->vw_initializeWithCopy(reinterpret_cast<OpaqueValue *>(&destination),
                              reinterpret_cast<OpaqueValue *>(&source));
  EXPECT_EQ(source, destination);
  EXPECT_EQ(2U, object.References);
  EXPECT_EQ(1U, object.AddRefs);
  EXPECT_EQ(0U, object.Releases);

  type->vw_destroy(reinterpret_cast<OpaqueValue *>(&destination));
  EXPECT_EQ(1U, object.References);
  EXPECT_EQ(1U, object.Releases);
  type->vw_destroy(reinterpret_cast<OpaqueValue *>(&source));
  EXPECT_EQ(0U, object.References);
  EXPECT_EQ(2U, object.Releases);
}

TEST(COMMetadataTest, OptionalRepresentation) {
  const auto *type = metadata(interface<1>());
  EXPECT_EQ(1U, type->getValueWitnesses()->getNumExtraInhabitants());
  // No lifetime operation may be performed while inspecting an enum tag.
  Interface object{nullptr};
  void *payload = &object;
  EXPECT_EQ(0U, type->vw_getEnumTagSinglePayload(
                    reinterpret_cast<OpaqueValue *>(&payload), 1));
  type->vw_storeEnumTagSinglePayload(reinterpret_cast<OpaqueValue *>(&payload),
                                     1, 1);
  EXPECT_EQ(nullptr, payload);
  EXPECT_EQ(1U, type->vw_getEnumTagSinglePayload(
                    reinterpret_cast<OpaqueValue *>(&payload), 1));
}
