//===--- RelativePointer.h - Relative Pointer Support -----------*- C++ -*-===//
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
//
// Some data structures emitted by the Swift compiler use relative indirect
// addresses in order to minimize startup cost for a process. By referring to
// the offset of the global offset table entry for a symbol, instead of directly
// referring to the symbol, compiler-emitted data structures avoid requiring
// unnecessary relocation at dynamic linking time. This header contains types
// to help dereference these relative addresses.
//
//===----------------------------------------------------------------------===//

#include <cstdint>

namespace swift {

namespace detail {

/// Apply a relative offset to a base pointer. The offset is applied to the base
/// pointer using sign-extended, wrapping arithmetic.
template<typename BasePtrTy, typename Offset>
static inline uintptr_t applyRelativeOffset(BasePtrTy *basePtr, Offset offset) {
  static_assert(std::is_integral<Offset>::value &&
                std::is_signed<Offset>::value,
                "offset type should be signed integer");

  auto base = reinterpret_cast<uintptr_t>(basePtr);
  // We want to do wrapping arithmetic, but with a sign-extended
  // offset. To do this in C, we need to do signed promotion to get
  // the sign extension, but we need to perform arithmetic on unsigned values,
  // since signed overflow is undefined behavior.
  auto extendOffset = (uintptr_t)(intptr_t)offset;
  return base + extendOffset;
}

} // namespace detail

/// A relative reference to an object stored in memory. The reference may be
/// direct or indirect, and uses the low bit of the (assumed at least
/// 2-byte-aligned) pointer to differentiate.
template<typename ValueTy, bool Nullable = false, typename Offset = int32_t>
class RelativeIndirectablePointer {
private:
  static_assert(std::is_integral<Offset>::value &&
                std::is_signed<Offset>::value,
                "offset type should be signed integer");
  
  /// The relative offset of the pointer's memory from the `this` pointer.
  /// If the low bit is clear, this is a direct reference; otherwise, it is
  /// an indirect reference.
  Offset RelativeOffsetPlusIndirect;

  /// RelativePointers should appear in statically-generated metadata. They
  /// shouldn't be constructed or copied.
  RelativeIndirectablePointer() = delete;
  RelativeIndirectablePointer(RelativeIndirectablePointer &&) = delete;
  RelativeIndirectablePointer(const RelativeIndirectablePointer &) = delete;
  RelativeIndirectablePointer &operator=(RelativeIndirectablePointer &&)
    = delete;
  RelativeIndirectablePointer &operator=(const RelativeIndirectablePointer &)
    = delete;

public:
  const ValueTy *get() const & {
    // Check for null.
    if (Nullable && RelativeOffsetPlusIndirect == 0)
      return nullptr;
    
    Offset offsetPlusIndirect = RelativeOffsetPlusIndirect;
    uintptr_t address = detail::applyRelativeOffset(this,
                                                    offsetPlusIndirect & ~1);

    // If the low bit is set, then this is an indirect address. Otherwise,
    // it's direct.
    if (offsetPlusIndirect & 1) {
      return *reinterpret_cast<const ValueTy * const *>(address);
    } else {
      return reinterpret_cast<const ValueTy *>(address);
    }
  }
  
  operator const ValueTy* () const & {
    return get();
  }

  const ValueTy &operator*() const & {
    return *get();
  }

  const ValueTy *operator->() const & {
    return get();
  }
};

/// A relative reference to a function, intended to reference private metadata
/// functions for the current executable or dynamic library image from
/// position-independent constant data.
template<typename T, bool Nullable, typename Offset>
class RelativeDirectPointerImpl {
private:
  /// The relative offset of the function's entry point from *this.
  Offset RelativeOffset;

  /// RelativePointers should appear in statically-generated metadata. They
  /// shouldn't be constructed or copied.
  RelativeDirectPointerImpl() = delete;
  RelativeDirectPointerImpl(RelativeDirectPointerImpl &&) = delete;
  RelativeDirectPointerImpl(const RelativeDirectPointerImpl &) = delete;
  RelativeDirectPointerImpl &operator=(RelativeDirectPointerImpl &&)
    = delete;
  RelativeDirectPointerImpl &operator=(const RelativeDirectPointerImpl&)
    = delete;

public:
  using ValueTy = T;
  using PointerTy = T*;

  PointerTy get() const & {
    // Check for null.
    if (Nullable && RelativeOffset == 0)
      return nullptr;
    
    // The value is addressed relative to `this`.
    uintptr_t absolute = detail::applyRelativeOffset(this, RelativeOffset);
    return reinterpret_cast<PointerTy>(absolute);
  }

  /// A zero relative offset encodes a null reference.
  bool isNull() const & {
    return RelativeOffset == 0;
  }
};

/// A direct relative reference to an object.
template<typename T, bool Nullable = true, typename Offset = int32_t>
class RelativeDirectPointer :
  private RelativeDirectPointerImpl<T, Nullable, Offset>
{
  using super = RelativeDirectPointerImpl<T, Nullable, Offset>;
public:
  using super::get;

  operator typename super::PointerTy() const & {
    return this->get();
  }

  const typename super::ValueTy &operator*() const & {
    return *this->get();
  }

  const typename super::ValueTy *operator->() const & {
    return this->get();
  }

  using super::isNull;
};

/// A specialization of RelativeDirectPointer for function pointers,
/// allowing for calls.
template<typename RetTy, typename...ArgTy, bool Nullable, typename Offset>
class RelativeDirectPointer<RetTy (ArgTy...), Nullable, Offset> :
  private RelativeDirectPointerImpl<RetTy (ArgTy...), Nullable, Offset>
{
  using super = RelativeDirectPointerImpl<RetTy (ArgTy...), Nullable, Offset>;
public:
  using super::get;

  operator typename super::PointerTy() const & {
    return this->get();
  }

  RetTy operator()(ArgTy...arg) {
    return this->get()(std::forward<ArgTy>(arg)...);
  }

  using super::isNull;
};

/// A direct relative reference to an aligned object, with an additional
/// tiny integer value crammed into its low bits.
template<typename PointeeTy, typename IntTy, typename Offset = int32_t>
class RelativeDirectPointerIntPair {
  Offset RelativeOffsetPlusInt;

  /// RelativePointers should appear in statically-generated metadata. They
  /// shouldn't be constructed or copied.
  RelativeDirectPointerIntPair() = delete;
  RelativeDirectPointerIntPair(RelativeDirectPointerIntPair &&) = delete;
  RelativeDirectPointerIntPair(const RelativeDirectPointerIntPair &) = delete;
  RelativeDirectPointerIntPair &operator=(RelativeDirectPointerIntPair &&)
    = delete;
  RelativeDirectPointerIntPair &operator=(const RelativeDirectPointerIntPair&)
    = delete;

  static Offset getMask() {
    static_assert(alignof(PointeeTy) >= alignof(Offset),
                 "pointee alignment must be at least as strict as offset type");

    return alignof(Offset) - 1;
  }

public:
  using ValueTy = PointeeTy;
  using PointerTy = PointeeTy*;

  PointerTy getPointer() const & {
    // The value is addressed relative to `this`.
    uintptr_t absolute = detail::applyRelativeOffset(this,
                                            RelativeOffsetPlusInt & ~getMask());
    return reinterpret_cast<PointerTy>(absolute);
  }

  IntTy getInt() const & {
    return IntTy(RelativeOffsetPlusInt & getMask());
  }
};

}

