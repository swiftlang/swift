//===--- ValueWitnessTable.h - Value witness table ABI ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This files defines ValueWitnessTable, a structure attached to type
// metadata objects (class Metadata) which tells generic code (whether
// generated or in the Swift runtime) how to lay out, copy, move, and
// destroy values of the type.
//
// This class uses the target-layout infrastructure; ValueWitnessTable is
// a typedef for the in-process application of the layout.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_VALUEWITNESSTABLE_H
#define SWIFT_ABI_VALUEWITNESSTABLE_H

#include "swift/Runtime/Config.h"
#include "swift/ABI/TargetLayout.h"
#include "swift/ABI/MetadataValues.h"

namespace swift {

template <typename Runtime>
struct TargetTypeLayout;
template <class Runtime> struct TargetEnumValueWitnessTable;
template <typename Runtime> struct TargetMetadata;
using Metadata = TargetMetadata<InProcess>;

/// Storage for an arbitrary value.  In C/C++ terms, this is an
/// 'object', because it is rooted in memory.
///
/// The context dictates what type is actually stored in this object,
/// and so this type is intentionally incomplete.
///
/// An object can be in one of two states:
///  - An uninitialized object has a completely unspecified state.
///  - An initialized object holds a valid value of the type.
struct OpaqueValue;

/// A fixed-size buffer for local values.  It is capable of owning
/// (possibly in side-allocated memory) the storage necessary
/// to hold a value of an arbitrary type.  Because it is fixed-size,
/// it can be allocated in places that must be agnostic to the
/// actual type: for example, within objects of existential type,
/// or for local variables in generic functions.
///
/// The context dictates its type, which ultimately means providing
/// access to a value witness table by which the value can be
/// accessed and manipulated.
///
/// A buffer can directly store three pointers and is pointer-aligned.
/// Three pointers is a sweet spot for Swift, because it means we can
/// store a structure containing a pointer, a size, and an owning
/// object, which is a common pattern in code due to ARC.  In a GC
/// environment, this could be reduced to two pointers without much loss.
///
/// A buffer can be in one of three states:
///  - An unallocated buffer has a completely unspecified state.
///  - An allocated buffer has been initialized so that it
///    owns uninitialized value storage for the stored type.
///  - An initialized buffer is an allocated buffer whose value
///    storage has been initialized.
template <typename Runtime>
struct TargetValueBuffer {
  TargetPointer<Runtime, void> PrivateData[NumWords_ValueBuffer];
};
using ValueBuffer = TargetValueBuffer<InProcess>;

/// Can a value with the given size and alignment be allocated inline?
constexpr inline bool canBeInline(bool isBitwiseTakable, size_t size,
                                  size_t alignment) {
  return isBitwiseTakable && size <= sizeof(ValueBuffer) &&
         alignment <= alignof(ValueBuffer);
}

template <class T>
constexpr inline bool canBeInline(bool isBitwiseTakable) {
  return canBeInline(isBitwiseTakable, sizeof(T), alignof(T));
}

template <typename Runtime> struct TargetValueWitnessTable;
using ValueWitnessTable = TargetValueWitnessTable<InProcess>;

template <typename Runtime> class TargetValueWitnessTypes;
using ValueWitnessTypes = TargetValueWitnessTypes<InProcess>;

template <typename Runtime>
class TargetValueWitnessTypes {
public:
  using StoredPointer = typename Runtime::StoredPointer;

// Note that, for now, we aren't strict about 'const'.
#define WANT_ALL_VALUE_WITNESSES
#define DATA_VALUE_WITNESS(lowerId, upperId, type)
#define FUNCTION_VALUE_WITNESS(lowerId, upperId, returnType, paramTypes) \
  typedef returnType (*lowerId ## Unsigned) paramTypes; \
  typedef TargetSignedPointer<Runtime, lowerId ## Unsigned \
  __ptrauth_swift_value_witness_function_pointer( \
    SpecialPointerAuthDiscriminators::upperId)> lowerId;
#define MUTABLE_VALUE_TYPE TargetPointer<Runtime, OpaqueValue>
#define IMMUTABLE_VALUE_TYPE ConstTargetPointer<Runtime, OpaqueValue>
#define MUTABLE_BUFFER_TYPE TargetPointer<Runtime, ValueBuffer>
#define IMMUTABLE_BUFFER_TYPE ConstTargetPointer<Runtime, ValueBuffer>
#define TYPE_TYPE ConstTargetPointer<Runtime, Metadata>
#define SIZE_TYPE StoredSize
#define INT_TYPE int
#define UINT_TYPE unsigned
#define VOID_TYPE void
#include "swift/ABI/ValueWitness.def"

  // Handle the data witnesses explicitly so we can use more specific
  // types for the flags enums.
  typedef typename Runtime::StoredSize size;
  typedef typename Runtime::StoredSize stride;
  typedef TargetValueWitnessFlags<typename Runtime::StoredSize> flags;
  typedef uint32_t extraInhabitantCount;
};

/// A value-witness table.  A value witness table is built around
/// the requirements of some specific type.  The information in
/// a value-witness table is intended to be sufficient to lay out
/// and manipulate values of an arbitrary type.
template <typename Runtime> struct TargetValueWitnessTable {
  // For the meaning of all of these witnesses, consult the comments
  // on their associated typedefs, above.

#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) \
  typename TargetValueWitnessTypes<Runtime>::LOWER_ID LOWER_ID;
#define FUNCTION_VALUE_WITNESS(LOWER_ID, UPPER_ID, RET, PARAMS) \
  typename TargetValueWitnessTypes<Runtime>::LOWER_ID LOWER_ID;

#include "swift/ABI/ValueWitness.def"

  using StoredSize = typename Runtime::StoredSize;

  /// Is the external type layout of this type incomplete?
  bool isIncomplete() const {
    return flags.isIncomplete();
  }

  /// Would values of a type with the given layout requirements be
  /// allocated inline?
  static bool isValueInline(bool isBitwiseTakable, StoredSize size,
                            StoredSize alignment) {
    return (isBitwiseTakable && size <= sizeof(TargetValueBuffer<Runtime>) &&
            alignment <= alignof(TargetValueBuffer<Runtime>));
  }

  /// Are values of this type allocated inline?
  bool isValueInline() const {
    return flags.isInlineStorage();
  }

  /// Is this type POD?
  bool isPOD() const {
    return flags.isPOD();
  }

  /// Is this type bitwise-takable?
  bool isBitwiseTakable() const {
    return flags.isBitwiseTakable();
  }

  /// Return the size of this type.  Unlike in C, this has not been
  /// padded up to the alignment; that value is maintained as
  /// 'stride'.
  StoredSize getSize() const {
    return size;
  }

  /// Return the stride of this type.  This is the size rounded up to
  /// be a multiple of the alignment.
  StoredSize getStride() const {
    return stride;
  }

  /// Return the alignment required by this type, in bytes.
  StoredSize getAlignment() const {
    return flags.getAlignment();
  }

  /// The alignment mask of this type.  An offset may be rounded up to
  /// the required alignment by adding this mask and masking by its
  /// bit-negation.
  ///
  /// For example, if the type needs to be 8-byte aligned, the value
  /// of this witness is 0x7.
  StoredSize getAlignmentMask() const {
    return flags.getAlignmentMask();
  }

  /// The number of extra inhabitants, that is, bit patterns that do not form
  /// valid values of the type, in this type's binary representation.
  unsigned getNumExtraInhabitants() const {
    return extraInhabitantCount;
  }

  /// Assert that this value witness table is an enum value witness table
  /// and return it as such.
  ///
  /// This has an awful name because it's supposed to be internal to
  /// this file.  Code outside this file should use LLVM's cast/dyn_cast.
  /// We don't want to use those here because we need to avoid accidentally
  /// introducing ABI dependencies on LLVM structures.
  const TargetEnumValueWitnessTable<Runtime> *_asEVWT() const;

  /// Get the type layout record within this value witness table.
  const TargetTypeLayout<Runtime> *getTypeLayout() const {
    return reinterpret_cast<const TargetTypeLayout<Runtime> *>(&size);
  }

  /// Check whether this metadata is complete.
  bool checkIsComplete() const;

  /// "Publish" the layout of this type to other threads.  All other stores
  /// to the value witness table (including its extended header) should have
  /// happened before this is called.
  void publishLayout(const TargetTypeLayout<Runtime> &layout);
};

/// A value-witness table with enum entry points.
/// These entry points are available only if the HasEnumWitnesses flag bit is
/// set in the 'flags' field.
template <class Runtime>
struct TargetEnumValueWitnessTable : TargetValueWitnessTable<Runtime> {
#define WANT_ONLY_ENUM_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) \
  ValueWitnessTypes::LOWER_ID LOWER_ID;
#define FUNCTION_VALUE_WITNESS(LOWER_ID, UPPER_ID, RET, PARAMS) \
  ValueWitnessTypes::LOWER_ID LOWER_ID;

#include "swift/ABI/ValueWitness.def"

  constexpr TargetEnumValueWitnessTable()
    : TargetValueWitnessTable<Runtime>{},
      getEnumTag(nullptr),
      destructiveProjectEnumData(nullptr),
      destructiveInjectEnumTag(nullptr) {}
  constexpr TargetEnumValueWitnessTable(
          const TargetValueWitnessTable<Runtime> &base,
          ValueWitnessTypes::getEnumTagUnsigned getEnumTag,
          ValueWitnessTypes::destructiveProjectEnumDataUnsigned
            destructiveProjectEnumData,
          ValueWitnessTypes::destructiveInjectEnumTagUnsigned
            destructiveInjectEnumTag)
    : TargetValueWitnessTable<Runtime>(base),
      getEnumTag(getEnumTag),
      destructiveProjectEnumData(destructiveProjectEnumData),
      destructiveInjectEnumTag(destructiveInjectEnumTag) {}

  static bool classof(const TargetValueWitnessTable<Runtime> *table) {
    return table->flags.hasEnumWitnesses();
  }
};
using EnumValueWitnessTable =
  TargetEnumValueWitnessTable<InProcess>;

template <class Runtime>
inline const TargetEnumValueWitnessTable<Runtime> *
TargetValueWitnessTable<Runtime>::_asEVWT() const {
  assert(TargetEnumValueWitnessTable<Runtime>::classof(this));
  return static_cast<const TargetEnumValueWitnessTable<Runtime> *>(this);
}

/// A type layout record. This is the subset of the value witness table that is
/// necessary to perform dependent layout of generic value types. It excludes
/// the value witness functions and includes only the size, alignment,
/// extra inhabitants, and miscellaneous flags about the type.
template <typename Runtime>
struct TargetTypeLayout {
  typename TargetValueWitnessTypes<Runtime>::size size;
  typename TargetValueWitnessTypes<Runtime>::stride stride;
  typename TargetValueWitnessTypes<Runtime>::flags flags;
  typename TargetValueWitnessTypes<Runtime>::extraInhabitantCount
      extraInhabitantCount;

private:
  void _static_assert_layout();
public:
  TargetTypeLayout() = default;
  constexpr TargetTypeLayout(
      typename TargetValueWitnessTypes<Runtime>::size size,
      typename TargetValueWitnessTypes<Runtime>::stride stride,
      typename TargetValueWitnessTypes<Runtime>::flags flags,
      typename TargetValueWitnessTypes<Runtime>::extraInhabitantCount xiCount)
      : size(size), stride(stride), flags(flags),
        extraInhabitantCount(xiCount) {}

  const TargetTypeLayout *getTypeLayout() const { return this; }

  /// The number of extra inhabitants, that is, bit patterns that do not form
  /// valid values of the type, in this type's binary representation.
  unsigned getNumExtraInhabitants() const {
    return extraInhabitantCount;
  }

  bool hasExtraInhabitants() const {
    return extraInhabitantCount != 0;
  }
};
using TypeLayout = TargetTypeLayout<InProcess>;

template <typename Runtime>
inline void TargetTypeLayout<Runtime>::_static_assert_layout() {
#define CHECK_TYPE_LAYOUT_OFFSET(FIELD)                                        \
  static_assert(offsetof(TargetValueWitnessTable<Runtime>, FIELD) -            \
                        offsetof(TargetValueWitnessTable<Runtime>, size) ==    \
                    offsetof(TargetTypeLayout<Runtime>, FIELD),                \
                "layout of " #FIELD " in TypeLayout doesn't match "            \
                "value witness table")
  CHECK_TYPE_LAYOUT_OFFSET(size);
  CHECK_TYPE_LAYOUT_OFFSET(flags);
  CHECK_TYPE_LAYOUT_OFFSET(extraInhabitantCount);
  CHECK_TYPE_LAYOUT_OFFSET(stride);

  #undef CHECK_TYPE_LAYOUT_OFFSET
}

} // end namespace swift

#endif
