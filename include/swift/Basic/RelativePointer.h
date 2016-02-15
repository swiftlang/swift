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

/// Measure the relative offset between two pointers. This measures
/// (referent - base) using wrapping arithmetic. The result is truncated if
/// Offset is smaller than a pointer, with an assertion that the
/// pre-truncation result is a sign extension of the truncated result.
template<typename Offset, typename A, typename B>
static inline Offset measureRelativeOffset(A *referent, B *base) {
  static_assert(std::is_integral<Offset>::value &&
                std::is_signed<Offset>::value,
                "offset type should be signed integer");

  auto distance = (uintptr_t)referent - (uintptr_t)base;
  // Truncate as unsigned, then wrap around to signed.
  auto truncatedDistance =
    (Offset)(typename std::make_unsigned<Offset>::type)distance;
  // Assert that the truncation didn't discard any non-sign-extended bits.
  assert((intptr_t)truncatedDistance == (intptr_t)distance
         && "pointers are too far apart to fit in offset type");
  return truncatedDistance;
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
  /// Allow construction and reassignment from an absolute pointer.
  /// These always produce a direct relative offset.
  RelativeIndirectablePointer(ValueTy *absolute)
  : RelativeOffsetPlusIndirect(
      Nullable && absolute == nullptr
        ? 0
        : detail::measureRelativeOffset<Offset>(absolute, this)) {
    if (!Nullable)
      assert(absolute != nullptr &&
             "constructing non-nullable relative pointer from null");
  }
  
  RelativeIndirectablePointer &operator=(ValueTy *absolute) & {
    if (!Nullable)
      assert(absolute != nullptr &&
             "constructing non-nullable relative pointer from null");
      
    RelativeOffsetPlusIndirect = Nullable && absolute == nullptr
      ? 0
      : detail::measureRelativeOffset<Offset>(absolute, this);
    return *this;
  }

  const ValueTy *get() const & {
    static_assert(alignof(ValueTy) >= 2 && alignof(Offset) >= 2,
                  "alignment of value and offset must be at least 2 to "
                  "make room for indirectable flag");
  
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

  /// A zero relative offset encodes a null reference.
  bool isNull() const & {
    return RelativeOffsetPlusIndirect == 0;
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

public:
  using ValueTy = T;
  using PointerTy = T*;

  // Allow construction and reassignment from an absolute pointer.
  RelativeDirectPointerImpl(PointerTy absolute)
    : RelativeOffset(Nullable && absolute == nullptr
                       ? 0
                       : detail::measureRelativeOffset<Offset>(absolute, this))
  {
    if (!Nullable)
      assert(absolute != nullptr &&
             "constructing non-nullable relative pointer from null");
  }
  explicit constexpr RelativeDirectPointerImpl(std::nullptr_t)
  : RelativeOffset (0) {
    static_assert(Nullable, "can't construct non-nullable pointer from null");
  }
  
  RelativeDirectPointerImpl &operator=(PointerTy absolute) & {
    if (!Nullable)
      assert(absolute != nullptr &&
             "constructing non-nullable relative pointer from null");
    RelativeOffset = Nullable && absolute == nullptr
      ? 0
      : detail::measureRelativeOffset<Offset>(absolute, this);
    return *this;
  }
  
  // Can copy-construct by recalculating the relative offset at the new
  // position.
  RelativeDirectPointerImpl(const RelativeDirectPointerImpl &p) {
    *this = p.get();
  }

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
  using super::super;
  
  RelativeDirectPointer &operator=(T *absolute) & {
    super::operator=(absolute);
    return *this;
  }

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
  using super::super;

  RelativeDirectPointer &operator=(RetTy (*absolute)(ArgTy...)) & {
    super::operator=(absolute);
    return *this;
  }
  
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

// Type aliases for "far" relative pointers, which need to be able to reach
// across the full address space instead of only across a single small-code-
// model image.

template<typename T, bool Nullable = false>
using FarRelativeIndirectablePointer =
  RelativeIndirectablePointer<T, Nullable, intptr_t>;

template<typename T, bool Nullable = false>
using FarRelativeDirectPointer = RelativeDirectPointer<T, Nullable, intptr_t>;

}

