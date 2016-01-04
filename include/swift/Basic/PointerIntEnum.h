//===--- PointerIntEnum.h -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/PointerLikeTypeTraits.h"
#include <cassert>
#include <climits>
#include <cstdlib>
#include <cstring>
#include <type_traits>
#include <utility>

namespace swift {

/// A pointer sized ADT that is able to compactly represent a Swift like enum
/// that can contain both Integer and Pointer payloads.
///
/// This is done by taking the ideas behind PointerIntPair and taking advantage
/// of an additional property that we already use in the runtime: namely that on
/// all modern OSes that we care about, the zero page is not allocated since it
/// is used as a copy on write "zeroed" page. This enables us to distinguish
/// whether or not we have a pointer or an index by restricting the size of
/// indices to be less than 4096. Most modern OSes (including Darwin) do not map
/// the zero page. That means that if the lower 61 bits of the uintptr_t is less
/// than 4096, then we have an index and a pointer otherwise.
///
/// Given this limitation, we store integers greater than 4096 out of line in a
/// malloced integer. This is a good trade-off for certain types of compiler
/// optimizations like relative array indexing where it is unlikely for someone
/// to address more than 1 page worth of items at a time. But it is important to
/// degrade gracefully in such a case.
///
/// In order to support these use cases, the c++ enum class that we use to
/// define our type must have a specific form:
///
///    enum class EnumTy : uint64_t {
///      Invalid = 0,
///
///      // PointerKinds
///      Ptr1 = 1,
///      ...
///      PtrN = N,
///      LastPointerKind = PtrN,
///
///      // Index Kinds
///      //
///      // This is an index >= 4096, requiring us to malloc memory. It needs
///      // to be able to be stored in at most 3 bits, implying it must be >= 7.
///
///      LargeIndex = 7,
///      Index1 = 8,
///      Index2 = 9,
///      Index3 = 10,
///      Index4 = 11,
///      Index5 = 12,
///      LastIndexKind = Index5,
///    };
///
/// In words, we have the following requirements:
///
/// 1. An Invalid case must be defined as being zero.
/// 2. We can only no more than N PointerKinds where N is the number of tagged
///    pointer bits that we have.
/// 3. LargeIndex must be equal to ((1 << NumTaggedBits)-1).
/// 4. All index kinds must be greater than LargeIndex.
///
/// \tparam EnumTy The enum type that is used for cases
/// \tparam PointerTy The pointer like type used for pointer payloads.
/// \tparam NumPointerKindBits The number of bits that can be used for pointer
/// kinds. Must be no more than the number of tagged bits in PointerTy.
/// \tparam NumIndexKindBits The number of bits that can be used for index
/// kinds.
/// \tparam PtrTraits The pointer traits of PointerTy
/// \tparam ScribbleMemory Instead of freeing any malloced memory, scribble the
/// memory. This enables us to test that memory is properly being
/// deallocated. Should only be set to true during unit testing.
template <typename EnumTy, typename PointerTy, unsigned NumPointerKindBits,
          unsigned NumIndexKindBits,
          typename PtrTraits = llvm::PointerLikeTypeTraits<PointerTy>,
          bool ScribbleMemory = false>
class PointerIntEnum {
  /// If we have stored a pointer, this gives the offset of the kind in Index.
  static constexpr unsigned PointerKindBitOffset =
      sizeof(uintptr_t) * CHAR_BIT - NumPointerKindBits;

  /// This is a mask for the lower PointerBitWidth - NumPointerKindBits bits of
  /// Index.
  static constexpr uintptr_t PointerBitMask =
      (uintptr_t(1) << PointerKindBitOffset) - 1;

  /// A bit mask used to grab index kind bits from a large index.
  static constexpr uint64_t IndexKindBitMask =
      (uint64_t(1) << NumIndexKindBits) - 1;

  /// This is the offset to the index kind bits at the top of projection.
  static constexpr unsigned IndexKindBitOffset =
      sizeof(uintptr_t) * CHAR_BIT - NumIndexKindBits;

  /// This is a mask that can be used to strip off the index kind from the top
  /// of Index.
  static constexpr uintptr_t IndexKindOffsetBitMask =
      (uintptr_t(1) << IndexKindBitOffset) - 1;

  /// This is the max index that a projection can represent without
  /// mallocing. The zero page on modern OSes is never mapped so, we can use
  /// this value to determine if we have a pointer or an index.
  ///
  /// We also use this as a mask to grab the index bits from a PointerIntEnum
  /// with an index kind.
  static constexpr uintptr_t MaxSmallIndex = (uintptr_t(1) << 12) - 1;

  /// The pointer sized type used for the actual storage.
  ///
  /// Never access this directly. Instead use the following methods:
  ///
  /// * getRawKind(): Returns the actual kind stored in the kind bits. This
  ///   means it will return LargeIndex.
  /// * getKind(): Same as RawKind except if the kind is LargeIndex, will
  ///   discover the real underlying kind in the malloced memory.
  /// * getIndex(): Asserts if getKind() is a pointer storing kind.
  /// * getRawPointer(): Returns the underlying pointer as a void *. Asserts if
  ///   getKind() is an index storing kind.
  /// * getPointer(): Returns the underlying pointer cast into
  ///   PointerTy. Asserts if getKind() is an index storing kind.
  uintptr_t Index;

public:
  PointerIntEnum() : PointerIntEnum(EnumTy::Invalid, nullptr) {}

  PointerIntEnum(EnumTy Kind, unsigned NewIndex) {
    initWithIndex(Kind, NewIndex);
  }
  PointerIntEnum(EnumTy Kind, PointerTy Ptr) {
    initWithPointer(Kind, PtrTraits::getAsVoidPointer(Ptr));
  }

  PointerIntEnum(PointerIntEnum &&P) : Index() { std::swap(Index, P.Index); }
  PointerIntEnum(const PointerIntEnum &P) : Index() { *this = P; }

  ~PointerIntEnum() {
    // If we have a large index, free the index.
    if (getRawKind() != EnumTy::LargeIndex)
      return;
    freeMemory();
  }

  PointerIntEnum &operator=(const PointerIntEnum &P) {
    // If we already have a raw kind, we need to free memory.
    if (getRawKind() == EnumTy::LargeIndex)
      freeMemory();

    auto NewRawKind = P.getRawKind();
    if (NewRawKind == EnumTy::LargeIndex ||
        NewRawKind > EnumTy::LastPointerKind) {
      initWithIndex(P.getKind(), P.getIndex());
      return *this;
    }

    initWithPointer(P.getKind(), P.getRawPointer());
    return *this;
  }

  void operator=(PointerIntEnum &&P) { std::swap(Index, P.Index); }

  bool isValid() const { return getRawKind() != EnumTy::Invalid; }

  bool operator==(const PointerIntEnum &Other) const {
    assert((isValid() && Other.isValid()) &&
           "Can not compare valid projections");
    auto Kind1 = getRawKind();

    // First make sure that the raw kinds line up.
    if (Kind1 != Other.getRawKind()) {
      return false;
    }

    // Then if we don't have a large index just compare index.
    if (Kind1 != EnumTy::LargeIndex)
      return Index == Other.Index;
    // Otherwise, we need to grab the actual index pointer from the memory that
    // we malloced.
    return getIndex() == Other.getIndex();
  }

  bool operator!=(const PointerIntEnum &Other) const {
    return !(*this == Other);
  }

  /// Convenience method for getting the raw underlying kind.
  EnumTy getKind() const {
    // First grab the bits of projection excluding the top 3 bits. If these bits
    // take on a value <= 4095, then we have a small index.
    if ((Index & IndexKindOffsetBitMask) <= MaxSmallIndex) {
      return EnumTy(unsigned(Index >> IndexKindBitOffset));
    }

    // Otherwise, we have some sort of pointer. If the kind is not a kind for a
    // large pointer, return the kind.
    auto Kind = EnumTy(unsigned(Index >> PointerKindBitOffset));
    if (Kind != EnumTy::LargeIndex)
      return Kind;

    // Ok, we *do* have an index type, but the index is >= 2047. Thus we know
    // that the Index is really a pointer to a single uint64_t value that was
    // malloced and stores our index. Grab the kind from the first
    // NumIndexKindBits (currently 4) bits of the 64 bit word.
    uint64_t Value;
    memcpy(&Value, getRawPointer(), sizeof(Value));
    return EnumTy(unsigned(Value & IndexKindBitMask));
  }

  /// Convenience method for getting the underlying index. Assumes that this
  /// projection is valid. Otherwise it asserts.
  unsigned getIndex() const {
    assert(unsigned(getRawKind()) > unsigned(EnumTy::LastPointerKind) &&
           "Not an index new projection kind");
    // Just return the bottom 11 bits if we have a small index.
    if (getRawKind() != EnumTy::LargeIndex)
      return unsigned(Index & MaxSmallIndex);

    // Otherwise, we have a large index. Convert our index into a pointer
    uint64_t Value;
    memcpy(&Value, getRawPointer(), sizeof(Value));
    return unsigned(Value >> NumIndexKindBits);
  }

  /// Convenience method for getting the raw underlying index as a pointer.
  void *getRawPointer() const {
    assert((unsigned(getRawKind()) <= unsigned(EnumTy::LastPointerKind) ||
            getRawKind() == EnumTy::LargeIndex) &&
           "Not a pointer projection kind");
    // We assume that all of the types of pointers that are stored are 8 bit
    // aligned. We store out pointer in the bottom 61 bits, so just shift out by
    // 3 and reinterpret_cast to a PointerTy .
    return reinterpret_cast<void *>(Index << NumPointerKindBits);
  }

  PointerTy getPointer() const {
    return PtrTraits::getFromVoidPointer(getRawPointer());
  }

  /// Convenience method for getting the raw underlying kind. This means that we
  /// will return LargeIndex as a kind instead of returning the kind from the
  /// lower bits of the malloced large index.
  EnumTy getRawKind() const {
    // First grab the bits of projection excluding the top 3 bits. If these bits
    // take on a value <= 2047, then we have a small index.
    if ((Index & IndexKindOffsetBitMask) <= MaxSmallIndex) {
      return EnumTy(unsigned(Index >> IndexKindBitOffset));
    }

    // Otherwise, we have some sort of pointer.
    return EnumTy(unsigned(Index >> PointerKindBitOffset));
  }

private:
  /// Initialize this PointerIntEnum with the kind \p Kind and the Pointer \p
  /// Ptr.
  ///
  /// This is an internal helper routine that should not be used directly since
  /// it does not properly handle freeing memory.
  void initWithIndex(EnumTy Kind, unsigned NewIndex) {
    // If new index is less than the max Small Index, then quickly initialize.
    if (NewIndex <= MaxSmallIndex) {
      // Initialize Index with NewIndex.
      Index = NewIndex;
      // We store the Kind in the upper 4 bits.
      Index |= uintptr_t(Kind) << IndexKindBitOffset;
      return;
    }

    // We store the index, shifted to the left by 4 bits and the kind in the
    // bottom 4 bits.
    uint64_t FinalNewIndex = uint64_t(NewIndex) << NumIndexKindBits;
    FinalNewIndex |= unsigned(Kind);

    // If we have a large index, malloc the memory and initialize it with our
    // new index.
    initWithPointer(EnumTy::LargeIndex, new uint64_t(FinalNewIndex));
  }

  /// Initialize this PointerIntEnum with the kind \p Kind and the Pointer \p
  /// Ptr.
  ///
  /// This is an internal helper routine that should not be used directly since
  /// it does not properly handle freeing memory.
  void initWithPointer(EnumTy Kind, void *Ptr) {
    // Make sure the pointer is at least 8 bit aligned.
    assert((uintptr_t(Ptr) & ((1 << NumPointerKindBits) - 1)) == 0);
    Index = uintptr_t(Ptr) >> NumPointerKindBits;
    Index |= uintptr_t(Kind) << PointerKindBitOffset;
  }

  /// If we have an index payload that is greater than 4096, this routine frees
  /// the malloced memory.
  void freeMemory() {
    assert(getRawKind() == EnumTy::LargeIndex &&
           "Freeing memory of a non-large index enum");
    void *Ptr = getRawPointer();
    if (ScribbleMemory) {
      uint64_t SentinelValue = -1ULL;
      memcpy(Ptr, &SentinelValue, sizeof(SentinelValue));
      return;
    }
    delete reinterpret_cast<uint64_t *>(getRawPointer());
  }
};

} // end swift namespace
