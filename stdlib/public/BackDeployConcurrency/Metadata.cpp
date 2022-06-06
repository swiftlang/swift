//===--- Metadata.cpp - Exclusivity tracking ------------------------------===//
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
//
// This implements the runtime support for metadata accessors that aren't
// available in Swift runtimes prior to macOS 12/iOS 15 era.
//
//===----------------------------------------------------------------------===//
#include <cinttypes>

#include "ConcurrencyRuntime.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/Metadata.h"
#include "llvm/Support/AllocatorBase.h"

using namespace swift;

namespace swift {

SWIFT_EXPORT_FROM(swift_Concurrency)
const FunctionTypeMetadata *
swift_getFunctionTypeMetadataGlobalActorStandalone(
    FunctionTypeFlags flags, FunctionMetadataDifferentiabilityKind diffKind,
    const Metadata *const *parameters, const uint32_t *parameterFlags,
    const Metadata *result, const Metadata *globalActor);

}

namespace {

constexpr size_t roundUpToAlignment(size_t offset, size_t alignment) {
  return ((offset + alignment - 1) & ~(alignment - 1));
}

class MetadataAllocator : public llvm::AllocatorBase<MetadataAllocator> {
private:
  uint16_t Tag;

public:
  constexpr MetadataAllocator(uint16_t tag) : Tag(tag) {}
  MetadataAllocator() = delete;

  void Reset() {}

  LLVM_ATTRIBUTE_RETURNS_NONNULL void *Allocate(size_t size, size_t alignment) {
    return malloc(size);
  }
  using AllocatorBase<MetadataAllocator>::Allocate;

  void Deallocate(const void *Ptr, size_t size, size_t Alignment) {
    free(const_cast<void *>(Ptr));
  }
  using AllocatorBase<MetadataAllocator>::Deallocate;

  void PrintStats() const {}

  MetadataAllocator withTag(uint16_t Tag) {
    MetadataAllocator Allocator = *this;
    Allocator.Tag = Tag;
    return Allocator;
  }
};

template <uint16_t StaticTag>
class TaggedMetadataAllocator : public MetadataAllocator {
public:
  constexpr TaggedMetadataAllocator() : MetadataAllocator(StaticTag) {}
};

template <class EntryTy, uint16_t Tag>
using SimpleGlobalCache =
    StableAddressConcurrentReadableHashMap<EntryTy,
                                           TaggedMetadataAllocator<Tag>>;

class FunctionCacheEntry {
public:
  FullMetadata<FunctionTypeMetadata> Data;

  struct Key {
    const FunctionTypeFlags Flags;
    const FunctionMetadataDifferentiabilityKind DifferentiabilityKind;
    const Metadata *const *Parameters;
    const uint32_t *ParameterFlags;
    const Metadata *Result;
    const Metadata *GlobalActor;

    FunctionTypeFlags getFlags() const { return Flags; }

    FunctionMetadataDifferentiabilityKind getDifferentiabilityKind() const {
      return DifferentiabilityKind;
    }

    const Metadata *getParameter(unsigned index) const {
      assert(index < Flags.getNumParameters());
      return Parameters[index];
    }
    const Metadata *getResult() const { return Result; }

    const uint32_t *getParameterFlags() const {
      return ParameterFlags;
    }

    ::ParameterFlags getParameterFlags(unsigned index) const {
      assert(index < Flags.getNumParameters());
      auto flags = Flags.hasParameterFlags() ? ParameterFlags[index] : 0;
      return ParameterFlags::fromIntValue(flags);
    }

    const Metadata *getGlobalActor() const { return GlobalActor; }

    friend llvm::hash_code hash_value(const Key &key) {
      auto hash = llvm::hash_combine(
          key.Flags.getIntValue(),
          key.DifferentiabilityKind.getIntValue(),
          key.Result, key.GlobalActor);
      for (unsigned i = 0, e = key.getFlags().getNumParameters(); i != e; ++i) {
        hash = llvm::hash_combine(hash, key.getParameter(i));
        hash = llvm::hash_combine(hash, key.getParameterFlags(i).getIntValue());
      }
      return hash;
    }
  };

  FunctionCacheEntry(const Key &key);

  intptr_t getKeyIntValueForDump() {
    return 0; // No single meaningful value here.
  }

  bool matchesKey(const Key &key) const {
    if (key.getFlags().getIntValue() != Data.Flags.getIntValue())
      return false;
    if (key.getDifferentiabilityKind().Value !=
        Data.getDifferentiabilityKind().Value)
      return false;
    if (key.getResult() != Data.ResultType)
      return false;
    if (key.getGlobalActor() != Data.getGlobalActor())
      return false;
    for (unsigned i = 0, e = key.getFlags().getNumParameters(); i != e; ++i) {
      if (key.getParameter(i) != Data.getParameter(i))
        return false;
      if (key.getParameterFlags(i).getIntValue() !=
          Data.getParameterFlags(i).getIntValue())
        return false;
    }
    return true;
  }

  friend llvm::hash_code hash_value(const FunctionCacheEntry &value) {
    Key key = {value.Data.Flags, value.Data.getDifferentiabilityKind(),
               value.Data.getParameters(), value.Data.getParameterFlags(),
               value.Data.ResultType, value.Data.getGlobalActor()};
    return hash_value(key);
  }

  static size_t getExtraAllocationSize(const Key &key) {
    return getExtraAllocationSize(key.Flags);
  }

  size_t getExtraAllocationSize() const {
    return getExtraAllocationSize(Data.Flags);
  }

  static size_t getExtraAllocationSize(const FunctionTypeFlags &flags) {
    const auto numParams = flags.getNumParameters();
    auto size = numParams * sizeof(FunctionTypeMetadata::Parameter);
    if (flags.hasParameterFlags())
      size += numParams * sizeof(uint32_t);
    if (flags.isDifferentiable())
      size = roundUpToAlignment(size, sizeof(void *)) +
          sizeof(FunctionMetadataDifferentiabilityKind);
    if (flags.hasGlobalActor())
      size = roundUpToAlignment(size, sizeof(void *)) + sizeof(Metadata *);
    return roundUpToAlignment(size, sizeof(void *));
  }
};

} // end anonymous namespace

FunctionCacheEntry::FunctionCacheEntry(const Key &key) {
  auto flags = key.getFlags();

  // Pick a value witness table appropriate to the function convention.
  // All function types of a given convention have the same value semantics,
  // so they share a value witness table.
  switch (flags.getConvention()) {
  case FunctionMetadataConvention::Swift:
    if (!flags.isEscaping()) {
      Data.ValueWitnesses = &VALUE_WITNESS_SYM(NOESCAPE_FUNCTION_MANGLING);
    } else {
      Data.ValueWitnesses = &VALUE_WITNESS_SYM(FUNCTION_MANGLING);
    }
    break;

  case FunctionMetadataConvention::Thin:
  case FunctionMetadataConvention::CFunctionPointer:
    Data.ValueWitnesses = &VALUE_WITNESS_SYM(THIN_FUNCTION_MANGLING);
    break;

  case FunctionMetadataConvention::Block:
#if SWIFT_OBJC_INTEROP
    // Blocks are ObjC objects, so can share the AnyObject value
    // witnesses (stored as "BO" rather than "yXl" for ABI compat).
    Data.ValueWitnesses = &VALUE_WITNESS_SYM(BO);
#else
    assert(false && "objc block without objc interop?");
#endif
    break;
  }

  unsigned numParameters = flags.getNumParameters();

  Data.setKind(MetadataKind::Function);
  Data.Flags = flags;
  Data.ResultType = key.getResult();
  if (flags.hasGlobalActor())
    *Data.getGlobalActorAddr() = key.getGlobalActor();
  if (flags.isDifferentiable())
    *Data.getDifferentiabilityKindAddress() = key.getDifferentiabilityKind();

  for (unsigned i = 0; i < numParameters; ++i) {
    Data.getParameters()[i] = key.getParameter(i);
    if (flags.hasParameterFlags())
      Data.getParameterFlags()[i] = key.getParameterFlags(i).getIntValue();
  }
}

// Provide a definition and static initializer for the fixed seed. This
// initializer should always be zero to ensure its value can never appear to be
// non-zero, even during dynamic initialization.
uint64_t llvm::hashing::detail::fixed_seed_override = 0;

/// The uniquing structure for function type metadata with a global actor.
static SimpleGlobalCache<FunctionCacheEntry, FunctionTypesTag> FunctionTypes;

const FunctionTypeMetadata *
swift::swift_getFunctionTypeMetadataGlobalActorStandalone(
    FunctionTypeFlags flags, FunctionMetadataDifferentiabilityKind diffKind,
    const Metadata *const *parameters, const uint32_t *parameterFlags,
    const Metadata *result, const Metadata *globalActor) {
  assert(flags.hasGlobalActor());
    assert(flags.hasGlobalActor());
  FunctionCacheEntry::Key key = {
    flags, diffKind, parameters, parameterFlags, result, globalActor
  };
  return &FunctionTypes.getOrInsert(key).first->Data;

}
