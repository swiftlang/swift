//===--- COMSwiftObject.cpp - COM identity tests --------------------------===//
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

#include "MetadataObjectBuilder.h"
#include "swift/Runtime/Casting.h"
#include "swift/Runtime/ExistentialContainer.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/shims/_SwiftCOMShims.h"
#include "gtest/gtest.h"
#include <cstring>
#include <tuple>

using namespace swift;

namespace {
const ModuleContextDescriptor *module() {
  return buildGlobalModuleContextDescriptor([] { return "COMSwiftObject"; });
}

template <bool IsBase = false>
const ClassDescriptor *nativeClass() {
  return buildGlobalObject<ClassDescriptor>([](AnyObjectBuilder &builder) {
    auto flags = ContextDescriptorFlags(
        ContextDescriptorKind::Class, /*generic=*/false, /*unique=*/true,
        /*hasInvertibleProtocols=*/false, /*kindSpecific=*/0);
    builder.add32(flags.getIntValue());
    builder.addRelativeIndirectReference(module(), /*addend=*/1);
    builder.addRelativeReferenceToString(IsBase ? "BaseObject"
                                                : "NativeObject");
    // No generic parameters, stored fields, or resilient metadata machinery.
    for (size_t offset = 12; offset < sizeof(ClassDescriptor); offset += 4)
      builder.add32(0);
  });
}

struct NativeObject : HeapObject {
  bool *Destroyed;
};

SWIFT_CC(swift) void destroyObject(SWIFT_CONTEXT HeapObject *value) {
  auto *object = static_cast<NativeObject *>(value);
  *object->Destroyed = true;
  swift_deallocObject(object, sizeof(NativeObject), alignof(NativeObject) - 1);
}

FullMetadata<ClassMetadata> BaseMetadata = {
    {{nullptr}, {&destroyObject}, {&VALUE_WITNESS_SYM(Bo)}},
    {{nullptr}, ClassFlags::UsesSwiftRefcounting, 0, 0, 0, 0, 0, 0}};
FullMetadata<ClassMetadata> NativeMetadata = {
    {{nullptr}, {&destroyObject}, {&VALUE_WITNESS_SYM(Bo)}},
    {{nullptr}, ClassFlags::UsesSwiftRefcounting, 0, 0, 0, 0, 0, 0}};
const FullMetadata<ClassMetadata> OtherMetadata = {
    {{nullptr}, {&destroyObject}, {&VALUE_WITNESS_SYM(Bo)}},
    {{nullptr}, ClassFlags::UsesSwiftRefcounting, 0, 0, 0, 0, 0, 0}};

using QueryResult = decltype(swift::QueryInterface(nullptr, nullptr, nullptr));
constexpr QueryResult NoInterface = static_cast<int32_t>(0x80004002u);

struct IdentityVTable {
  _SwiftCOMQueryInterfaceFunction QueryInterface;
  _SwiftCOMLifetimeFunction AddRef;
  _SwiftCOMLifetimeFunction Release;
  void *(__SWIFT_STDCALL *Object)(void *);
  const Metadata *(__SWIFT_STDCALL *Metadata)(void *);
};

struct Adapter {
  struct Interface {
    const IdentityVTable *VTable;
    Adapter *Owner;
  } Source, Identity;
  unsigned References = 1;
  unsigned Queries = 0;
  unsigned AddRefs = 0;
  unsigned Releases = 0;
  unsigned ObjectReads = 0;
  unsigned MetadataReads = 0;
  QueryResult Status = 0;
  bool NullInterface = false;
  bool Destroyed = false;
  HeapObject *OwnedObject;
  void *ReportedObject;
  const swift::Metadata *ReportedMetadata = &NativeMetadata;

  explicit Adapter(const IdentityVTable *vtable)
      : Source{vtable, this}, Identity{vtable, this} {
    NativeMetadata.setDescription(nativeClass());
    auto *object = static_cast<NativeObject *>(swift_allocObject(
        &NativeMetadata, sizeof(NativeObject), alignof(NativeObject) - 1));
    object->Destroyed = &Destroyed;
    OwnedObject = object;
    ReportedObject = object;
  }

  static uint32_t __SWIFT_STDCALL AddRef(void *value) {
    auto &adapter = *static_cast<Interface *>(value)->Owner;
    ++adapter.AddRefs;
    return ++adapter.References;
  }

  static uint32_t __SWIFT_STDCALL Release(void *value) {
    auto &adapter = *static_cast<Interface *>(value)->Owner;
    ++adapter.Releases;
    auto count = --adapter.References;
    if (!count)
      swift_release(adapter.OwnedObject);
    return count;
  }

  static QueryResult __SWIFT_STDCALL QueryInterface(void *value,
                                                    const void *iid,
                                                    void **result) {
    auto &adapter = *static_cast<Interface *>(value)->Owner;
    ++adapter.Queries;
    *result = nullptr;
    // {8E369447-5188-5ADA-B9EC-8FCB732D226B}, in native GUID layout.
    const uint32_t data1 = 0x8e369447;
    const uint16_t data2 = 0x5188, data3 = 0x5ada;
    const uint8_t data4[] = {0xb9, 0xec, 0x8f, 0xcb, 0x73, 0x2d, 0x22, 0x6b};
    EXPECT_EQ(0U, reinterpret_cast<uintptr_t>(iid) % alignof(uint32_t));
    auto *bytes = static_cast<const char *>(iid);
    if (std::memcmp(bytes, &data1, 4) || std::memcmp(bytes + 4, &data2, 2) ||
        std::memcmp(bytes + 6, &data3, 2) || std::memcmp(bytes + 8, data4, 8))
      return NoInterface;
    if (adapter.Status < 0 || adapter.NullInterface)
      return adapter.Status;
    *result = &adapter.Identity;
    AddRef(*result);
    return adapter.Status;
  }

  static void *__SWIFT_STDCALL GetObject(void *value) {
    auto &interface = *static_cast<Interface *>(value);
    auto &adapter = *interface.Owner;
    EXPECT_EQ(&adapter.Identity, &interface);
    EXPECT_GT(adapter.References, 0U);
    ++adapter.ObjectReads;
    return adapter.ReportedObject;
  }

  static const swift::Metadata *__SWIFT_STDCALL GetMetadata(void *value) {
    auto &interface = *static_cast<Interface *>(value);
    auto &adapter = *interface.Owner;
    EXPECT_EQ(&adapter.Identity, &interface);
    EXPECT_GT(adapter.References, 0U);
    ++adapter.MetadataReads;
    return adapter.ReportedMetadata;
  }
};

template <unsigned ID = 1>
const ProtocolDescriptor *sourceProtocol() {
  return buildGlobalObject<ProtocolDescriptor>([](AnyObjectBuilder &builder) {
    ProtocolContextDescriptorFlags flags;
    flags.setClassConstraint(ProtocolClassConstraint::Any);
    flags.setSpecialProtocol(SpecialProtocol::COM);
    auto contextFlags = ContextDescriptorFlags(
        ContextDescriptorKind::Protocol, /*generic=*/false, /*unique=*/true,
        /*hasInvertibleProtocols=*/false, flags.getOpaqueValue());
    builder.add32(contextFlags.getIntValue());
    builder.addRelativeIndirectReference(module(), /*addend=*/1);
    builder.addRelativeReferenceToString(ID == 1 ? "ISource" : "ITarget");
    builder.add32(0); // NumRequirementsInSignature
    builder.add32(0); // NumRequirements
    builder.add32(0); // AssociatedTypeNames
    uint8_t iid[16] = {};
    iid[15] = ID;
    builder.addBytes(iid, sizeof(iid));
  });
}

const ExistentialTypeMetadata *existential(const ProtocolDescriptor *protocol) {
  ProtocolDescriptorRef protocols[] = {
      ProtocolDescriptorRef::forSwift(protocol)};
  return swift_getExistentialTypeMetadata(ProtocolClassConstraint::Any,
                                          /*superclass=*/nullptr, 1, protocols);
}

template <bool Conforming = true>
const ProtocolDescriptor *nativeProtocol() {
  return buildGlobalObject<ProtocolDescriptor>([](AnyObjectBuilder &builder) {
    ProtocolContextDescriptorFlags flags;
    flags.setClassConstraint(ProtocolClassConstraint::Any);
    auto contextFlags = ContextDescriptorFlags(
        ContextDescriptorKind::Protocol, /*generic=*/false, /*unique=*/true,
        /*hasInvertibleProtocols=*/false, flags.getOpaqueValue());
    builder.add32(contextFlags.getIntValue());
    builder.addRelativeIndirectReference(module(), /*addend=*/1);
    builder.addRelativeReferenceToString(Conforming ? "Native" : "Missing");
    builder.add32(0); // NumRequirementsInSignature
    builder.add32(0); // NumRequirements
    builder.add32(0); // AssociatedTypeNames
  });
}

const ProtocolConformanceRecord *nativeConformance() {
  static const auto *record = buildGlobalObject<ProtocolConformanceRecord>(
      [](AnyObjectBuilder &builder) {
        auto conformance =
            builder.createSubobject<ProtocolConformanceDescriptor>();
        auto witness = builder.createSubobject<WitnessTable>();
        // Filled after the descriptor is allocated.
        witness.addPointer(nullptr);
        conformance.addRelativeIndirectReference(nativeProtocol(),
                                                 /*addend=*/1);
        conformance.addRelativeIndirectReference(nativeClass());
        conformance.addRelativeReference(witness.ref());
        conformance.add32(ConformanceFlags()
                              .withTypeReferenceKind(
                                  TypeReferenceKind::IndirectTypeDescriptor)
                              .getIntValue());
        builder.addRelativeReference(conformance.ref());
      });
  static const bool registered = [&] {
    const auto *descriptor = record->get();
    auto *witness =
        const_cast<WitnessTable *>(descriptor->getWitnessTablePattern());
    std::memcpy(witness, &descriptor, sizeof(descriptor));
    swift_registerProtocolConformances(record, record + 1);
    return true;
  }();
  (void)registered;
  return record;
}

class COMSwiftCastTest : public testing::Test {
protected:
  const IdentityVTable VTable{Adapter::QueryInterface, Adapter::AddRef,
                              Adapter::Release, Adapter::GetObject,
                              Adapter::GetMetadata};
  Adapter Object{&VTable};

  void TearDown() override {
    if (Object.References)
      Adapter::Release(&Object.Source);
    EXPECT_EQ(0U, Object.References);
    EXPECT_EQ(Object.AddRefs + 1, Object.Releases);
    EXPECT_TRUE(Object.Destroyed);
  }

  const ExistentialTypeMetadata *SourceType = existential(sourceProtocol());
  const Metadata *AnyType = &METADATA_SYM(ANY_MANGLING);
  void *Source = &Object.Source;
  OpaqueExistentialContainer Erased{};
  OpaqueValue *SourceLocation = reinterpret_cast<OpaqueValue *>(&Source);
  const Metadata *SourceMetadata = SourceType;

  void SetUp() override {
    NativeMetadata.setDescription(nativeClass());
    BaseMetadata.setDescription(nativeClass<true>());
    NativeMetadata.Superclass = &BaseMetadata;
    nativeConformance();
  }

  void prepareSource(bool erase) {
    if (erase) {
      ASSERT_TRUE(swift_dynamicCast(reinterpret_cast<OpaqueValue *>(&Erased),
                                    SourceLocation, SourceType, AnyType,
                                    DynamicCastFlags::TakeOnSuccess));
      EXPECT_EQ(SourceType, Erased.Type);
      SourceLocation = reinterpret_cast<OpaqueValue *>(&Erased);
      SourceMetadata = AnyType;
    }
    ASSERT_EQ(0U, Object.Queries);
    ASSERT_EQ(1U, Object.References);
  }

  bool castTo(OpaqueValue *destination, const Metadata *target,
              DynamicCastFlags flags) {
    return swift_dynamicCast(destination, SourceLocation, SourceMetadata,
                             target, flags);
  }

  void destroySource() { SourceMetadata->vw_destroy(SourceLocation); }
};

class COMSwiftCastOwnershipTest
    : public COMSwiftCastTest,
      public testing::WithParamInterface<std::tuple<DynamicCastFlags, bool>> {
protected:
  void checkInvalidIdentity() {
    auto [flags, erased] = GetParam();
    ASSERT_NO_FATAL_FAILURE(prepareSource(erased));
    int sentinel = 0;
    void *result = &sentinel;
    EXPECT_FALSE(castTo(reinterpret_cast<OpaqueValue *>(&result),
                        &NativeMetadata, flags));
    EXPECT_EQ(&sentinel, result);
    EXPECT_EQ(1U, Object.Queries);
    EXPECT_EQ(1U, Object.AddRefs);
    EXPECT_EQ(1U, Object.ObjectReads);
    EXPECT_EQ(1U, Object.MetadataReads);
    bool destroy = flags & DynamicCastFlags::DestroyOnFailure;
    EXPECT_EQ(destroy ? 2U : 1U, Object.Releases);
    EXPECT_EQ(destroy, Object.Destroyed);
    if (!destroy)
      destroySource();
  }
};

TEST_P(COMSwiftCastOwnershipTest, NativeClassOwnsResult) {
  auto [flags, erased] = GetParam();
  ASSERT_NO_FATAL_FAILURE(prepareSource(erased));
  HeapObject *result = nullptr;
  ASSERT_TRUE(
      castTo(reinterpret_cast<OpaqueValue *>(&result), &NativeMetadata, flags));
  EXPECT_EQ(Object.OwnedObject, result);
  EXPECT_NE(static_cast<void *>(&Object.Source), result);
  EXPECT_EQ(1U, Object.Queries);
  EXPECT_EQ(1U, Object.ObjectReads);
  EXPECT_EQ(1U, Object.MetadataReads);
  EXPECT_EQ(1U, Object.AddRefs);
  bool take = flags & DynamicCastFlags::TakeOnSuccess;
  EXPECT_EQ(take ? 0U : 1U, Object.References);
  EXPECT_EQ(take ? 2U : 1U, Object.Releases);
  if (!take)
    destroySource();
  EXPECT_FALSE(Object.Destroyed);
  EXPECT_EQ(1U, swift_retainCount(result));
  swift_release(result);
  EXPECT_TRUE(Object.Destroyed);
}

TEST_P(COMSwiftCastOwnershipTest, NativeSuperclass) {
  auto [flags, erased] = GetParam();
  ASSERT_NO_FATAL_FAILURE(prepareSource(erased));
  HeapObject *result = nullptr;
  ASSERT_TRUE(
      castTo(reinterpret_cast<OpaqueValue *>(&result), &BaseMetadata, flags));
  EXPECT_EQ(Object.OwnedObject, result);
  if (!(flags & DynamicCastFlags::TakeOnSuccess))
    destroySource();
  EXPECT_FALSE(Object.Destroyed);
  swift_release(result);
  EXPECT_TRUE(Object.Destroyed);
}

TEST_P(COMSwiftCastOwnershipTest, NativeProtocol) {
  auto [flags, erased] = GetParam();
  ASSERT_NO_FATAL_FAILURE(prepareSource(erased));
  const auto *target = existential(nativeProtocol());
  struct {
    OpaqueExistentialContainer Container;
    const WitnessTable *Witness;
  } result{};
  ASSERT_TRUE(castTo(reinterpret_cast<OpaqueValue *>(&result), target, flags));
  EXPECT_EQ(&NativeMetadata, result.Container.Type);
  EXPECT_EQ(nativeConformance()->get(), result.Witness->getDescription());
  auto *value = target->projectValue(reinterpret_cast<OpaqueValue *>(&result));
  EXPECT_EQ(Object.OwnedObject, *reinterpret_cast<HeapObject **>(value));
  if (!(flags & DynamicCastFlags::TakeOnSuccess))
    destroySource();
  EXPECT_FALSE(Object.Destroyed);
  target->vw_destroy(reinterpret_cast<OpaqueValue *>(&result));
  EXPECT_TRUE(Object.Destroyed);
}

TEST_P(COMSwiftCastOwnershipTest, UnrelatedClass) {
  auto [flags, erased] = GetParam();
  ASSERT_NO_FATAL_FAILURE(prepareSource(erased));
  int sentinel = 0;
  void *result = &sentinel;
  EXPECT_FALSE(
      castTo(reinterpret_cast<OpaqueValue *>(&result), &OtherMetadata, flags));
  EXPECT_EQ(&sentinel, result);
  EXPECT_EQ(1U, Object.Queries);
  EXPECT_EQ(1U, Object.AddRefs);
  bool destroy = flags & DynamicCastFlags::DestroyOnFailure;
  EXPECT_EQ(destroy ? 2U : 1U, Object.Releases);
  EXPECT_EQ(destroy, Object.Destroyed);
  if (!destroy)
    destroySource();
}

TEST_P(COMSwiftCastOwnershipTest, MissingNativeConformance) {
  auto [flags, erased] = GetParam();
  ASSERT_NO_FATAL_FAILURE(prepareSource(erased));
  struct {
    OpaqueExistentialContainer Container;
    const WitnessTable *Witness;
  } result{};
  EXPECT_FALSE(castTo(reinterpret_cast<OpaqueValue *>(&result),
                      existential(nativeProtocol<false>()), flags));
  EXPECT_EQ(1U, Object.Queries);
  EXPECT_EQ(1U, Object.AddRefs);
  bool destroy = flags & DynamicCastFlags::DestroyOnFailure;
  EXPECT_EQ(destroy ? 2U : 1U, Object.Releases);
  EXPECT_EQ(destroy, Object.Destroyed);
  if (!destroy)
    destroySource();
}

TEST_P(COMSwiftCastOwnershipTest, UnsupportedIdentity) {
  auto [flags, erased] = GetParam();
  ASSERT_NO_FATAL_FAILURE(prepareSource(erased));
  Object.Status = NoInterface;
  int sentinel = 0;
  void *result = &sentinel;
  EXPECT_FALSE(
      castTo(reinterpret_cast<OpaqueValue *>(&result), &NativeMetadata, flags));
  EXPECT_EQ(&sentinel, result);
  EXPECT_EQ(1U, Object.Queries);
  EXPECT_EQ(0U, Object.AddRefs);
  EXPECT_EQ(0U, Object.ObjectReads);
  EXPECT_EQ(0U, Object.MetadataReads);
  bool destroy = flags & DynamicCastFlags::DestroyOnFailure;
  EXPECT_EQ(destroy ? 1U : 0U, Object.Releases);
  EXPECT_EQ(destroy, Object.Destroyed);
  if (!destroy)
    destroySource();
}

TEST_P(COMSwiftCastOwnershipTest, MismatchedMetadata) {
  Object.ReportedMetadata = &OtherMetadata;
  checkInvalidIdentity();
}

TEST_P(COMSwiftCastOwnershipTest, NullObject) {
  Object.ReportedObject = nullptr;
  checkInvalidIdentity();
}

TEST_P(COMSwiftCastOwnershipTest, NullMetadata) {
  Object.ReportedMetadata = nullptr;
  checkInvalidIdentity();
}

TEST_P(COMSwiftCastOwnershipTest, NonClassMetadata) {
  Object.ReportedMetadata = &METADATA_SYM(Bi64_).base;
  checkInvalidIdentity();
}

TEST_P(COMSwiftCastOwnershipTest, NullQueryResult) {
  auto [flags, erased] = GetParam();
  ASSERT_NO_FATAL_FAILURE(prepareSource(erased));
  Object.NullInterface = true;
  int sentinel = 0;
  void *result = &sentinel;
  EXPECT_FALSE(
      castTo(reinterpret_cast<OpaqueValue *>(&result), &NativeMetadata, flags));
  EXPECT_EQ(&sentinel, result);
  EXPECT_EQ(1U, Object.Queries);
  EXPECT_EQ(0U, Object.AddRefs);
  EXPECT_EQ(0U, Object.ObjectReads);
  EXPECT_EQ(0U, Object.MetadataReads);
  bool destroy = flags & DynamicCastFlags::DestroyOnFailure;
  EXPECT_EQ(destroy ? 1U : 0U, Object.Releases);
  EXPECT_EQ(destroy, Object.Destroyed);
  if (!destroy)
    destroySource();
}

INSTANTIATE_TEST_SUITE_P(
    Ownership, COMSwiftCastOwnershipTest,
    testing::Combine(testing::Values(DynamicCastFlags::Default,
                                     DynamicCastFlags::TakeOnSuccess,
                                     DynamicCastFlags::DestroyOnFailure,
                                     DynamicCastFlags::TakeOnSuccess |
                                         DynamicCastFlags::DestroyOnFailure),
                     testing::Bool()));

TEST_F(COMSwiftCastTest, FailedCOMQueryDoesNotRecoverIdentity) {
  void *result = nullptr;
  // The adapter only exposes ISwiftObject, so querying ITarget from the
  // adjusted identity address must fail without a second identity query.
  Source = &Object.Identity;
  EXPECT_FALSE(castTo(reinterpret_cast<OpaqueValue *>(&result),
                      existential(sourceProtocol<2>()),
                      DynamicCastFlags::Default));
  EXPECT_EQ(1U, Object.Queries);
  EXPECT_EQ(0U, Object.ObjectReads);
  EXPECT_EQ(0U, Object.AddRefs);
  destroySource();
}

TEST_F(COMSwiftCastTest, UnconditionalTakingCast) {
  HeapObject *result = nullptr;
  ASSERT_TRUE(castTo(reinterpret_cast<OpaqueValue *>(&result), &NativeMetadata,
                     DynamicCastFlags::Unconditional |
                         DynamicCastFlags::TakeOnSuccess));
  EXPECT_EQ(Object.OwnedObject, result);
  EXPECT_EQ(0U, Object.References);
  EXPECT_FALSE(Object.Destroyed);
  swift_release(result);
}

TEST_F(COMSwiftCastTest, NonnegativeStatus) {
  Object.Status = 1;
  HeapObject *result = nullptr;
  ASSERT_TRUE(castTo(reinterpret_cast<OpaqueValue *>(&result), &NativeMetadata,
                     DynamicCastFlags::Default));
  EXPECT_EQ(Object.OwnedObject, result);
  destroySource();
  EXPECT_FALSE(Object.Destroyed);
  swift_release(result);
}

TEST_F(COMSwiftCastTest, NullSource) {
  Source = nullptr;
  int sentinel = 0;
  void *result = &sentinel;
  EXPECT_FALSE(castTo(reinterpret_cast<OpaqueValue *>(&result), &NativeMetadata,
                      DynamicCastFlags::Default));
  EXPECT_EQ(&sentinel, result);
  EXPECT_EQ(0U, Object.Queries);
}

} // namespace
