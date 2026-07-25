//===--- TypeInfoStorage.h - TypeInfo storage definitions -------*- C++ -*-===//
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

#ifndef SWIFT_AST_TYPEINFOSTORAGE_H
#define SWIFT_AST_TYPEINFOSTORAGE_H

#include "swift/Basic/ClusteredBitVector.h"
#include "swift/Basic/InlineBitfield.h"
#include <cstdint>

namespace swift {
namespace irgen {

class FixedTypeInfo;
class TypeInfo;

/// In IRGen, we use Swift's ClusteredBitVector data structure to
/// store vectors of spare bits.
using SpareBitVector = ClusteredBitVector;

enum class ElementLayoutKind {
  /// The element is known to require no storage in the aggregate.
  /// Its offset in the aggregate is always statically zero.
  Empty,

  /// The element is known to require no storage in the aggregate.
  /// But it has an offset in the aggregate. This is to support getting the
  /// offset of tail allocated storage using MemoryLayout<>.offset(of:).
  EmptyTailAllocatedCType,

  /// The element can be positioned at a fixed offset within the
  /// aggregate.
  Fixed,

  /// The element cannot be positioned at a fixed offset within the
  /// aggregate.
  NonFixed,

  /// The element is an object lacking a fixed size but located at
  /// offset zero.  This is necessary because LLVM forbids even a
  /// 'gep 0' on an unsized type.
  InitialNonFixedSize

  // IncompleteKind comes here
};

struct ElementLayoutStorage {
  /// The offset in bytes from the start of the struct.
  unsigned ByteOffset;

  /// The offset in bytes from the start of the struct, except EmptyFields are
  /// placed at the current byte offset instead of 0. For the purpose of the
  /// final layout empty fields are placed at offset 0, that however creates a
  /// whole slew of special cases to deal with. Instead of dealing with these
  /// special cases during layout, we pretend that empty fields are placed
  /// just like any other field at the current offset.
  unsigned ByteOffsetForLayout;

  /// The index of this element, either in the LLVM struct (if fixed)
  /// or in the non-fixed elements array (if non-fixed).
  unsigned Index : 28;

  /// Whether this element is known to be trivially destructible in the local
  /// resilience domain.
  unsigned IsTriviallyDestroyable : 1;

  /// The kind of layout performed for this element.
  unsigned TheKind : 3;

  ElementLayoutStorage()
      : TheKind(unsigned(ElementLayoutKind::InitialNonFixedSize) + 1) {}
};

struct RecordFieldStorage {
  /// The range of explosion indexes for this element.
  unsigned Begin;
  unsigned End;
};

enum class SpecialTypeInfoKind : uint8_t {
  Unimplemented,

  None,

  /// Everything after this is statically fixed-size.
  Fixed,
  Weak,

  /// Everything after this is loadable.
  Loadable,
  Reference,

  Last_Kind = Reference
};
enum : unsigned {
  NumSpecialTypeInfoKindBits =
      countBitsUsed(static_cast<unsigned>(SpecialTypeInfoKind::Last_Kind))
};

// clang-format off
union TypeInfoBitfields {
  uint64_t OpaqueBits;

  SWIFT_INLINE_BITFIELD_BASE(TypeInfo,
                           bitmax(NumSpecialTypeInfoKindBits,8)+6+1+1+1+1+3+1+1,
    /// The kind of supplemental API this type has, if any.
    Kind : bitmax(NumSpecialTypeInfoKindBits,8),

    /// The storage alignment of this type in log2 bytes.
    AlignmentShift : 6,

    /// Whether this type is known to be trivially destructible.
    TriviallyDestroyable : 1,
    
    /// Whether this type is known to be bitwise-takable.
    BitwiseTakable : 1,

    /// Whether this type is known to be bitwise-borrowable.
    BitwiseBorrowable : 1,

    /// Whether this type is known to be copyable.
    Copyable : 1,

    /// An arbitrary discriminator for the subclass.  This is useful for e.g.
    /// distinguishing between different TypeInfos that all implement the same
    /// kind of type.
    /// FIXME -- Create TypeInfoNodes.def and get rid of this field.
    SubclassKind : 3,

    /// Whether this type can be assumed to have a fixed size from all
    /// resilience domains.
    AlwaysFixedSize : 1,

    /// Whether this type is ABI-accessible from this SILModule.
    ABIAccessible : 1
  );

  /// FixedTypeInfo will use the remaining bits for the size.
  ///
  /// NOTE: Until one can define statically sized inline arrays in the
  /// language, defining an extremely large object is quite impractical.
  /// For now: "4 GiB should be more than good enough."
  SWIFT_INLINE_BITFIELD_FULL(FixedTypeInfo, TypeInfo, 32,
    : NumPadBits,

    /// The storage size of this type in bytes.  This may be zero even
    /// for well-formed and complete types, such as a trivial enum or
    /// tuple.
    Size : 32
  );
};
// clang-format on

} // namespace irgen
} // namespace swift

#endif // SWIFT_AST_TYPEINFOSTORAGE_H
