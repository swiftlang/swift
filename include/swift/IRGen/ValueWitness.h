//===--- ValueWitness.h - Enumeration of value witnesses --------*- C++ -*-===//
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
// This file defines the list of witnesses required to attest that a
// type is a value type.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_VALUEWITNESS_H
#define SWIFT_IRGEN_VALUEWITNESS_H

namespace swift {
namespace irgen {

/// The members required to attest that a type is a value type.
///
/// Logically, there are three basic data operations we must support
/// on arbitrary types:
///   - initializing an object by copying another
///   - changing an object to be a copy of another
///   - destroying an object
///
/// As an optimization to permit efficient transfers of data, the
/// "copy" operations each have an analogous "take" operation which
/// implicitly destroys the source object.
///
/// Therefore there are five basic data operations:
///   initWithCopy(T*, T*)
///   initWithTake(T*, T*)
///   assignWithCopy(T*, T*)
///   assignWithTake(T*, T*)
///   destroy(T*)
///
/// As a further optimization, for every T*, there is a related
/// operation which replaces that T* with a B*, combinatorially.  This
/// makes 18 operations, except that some of these operations are
/// fairly unlikely and so do not merit optimized entries, due to
/// the common code patterns of the two use cases:
///   - Existential code usually doesn't work directly with T*s
///     because pointers into existential objects are not generally
///     reliable.
///   - Generic code works with T*s a fair amount, but it usually
///     doesn't have to deal with B*s after initialization
///     because initialization returns a reliable pointer.
/// This leads us to the following conclusions:
//    - Operations to copy a B* to a T* are very unlikely
///     to be used (-4 operations).
///   - Assignments involving two B*s are only likely in
///     existential code, where we won't have the right
///     typing guarantees to use them (-2 operations).
/// Furthermore, take-initializing a buffer from a buffer is just a
/// memcpy of the buffer (-1), and take-assigning a buffer from a
/// buffer is just a destroy and a memcpy (-1).
///
/// This leaves us with 12 data operations, to which we add the
/// meta-operation 'sizeAndAlign' for a total of 13.
enum class ValueWitness : unsigned {
#define WANT_ALL_VALUE_WITNESSES
#define VALUE_WITNESS(lowerId, upperId) upperId,
#define BEGIN_VALUE_WITNESS_RANGE(rangeId, upperId) First_##rangeId = upperId,
#define END_VALUE_WITNESS_RANGE(rangeId, upperId) Last_##rangeId = upperId,
#include "swift/ABI/ValueWitness.def"
};

// The namespaces here are to force the enumerators to be scoped.  We don't
// use 'enum class' because we want the enumerators to convert freely
// to uint64_t.
namespace ValueWitnessFlags {
  enum : uint64_t {
    AlignmentMask       = 0x0FFFF,
    IsNonPOD            = 0x10000,
    IsNonInline         = 0x20000,
    
    /// Flags pertaining to enum representation.
    Enum_FlagMask = 0xC0000,
    
    /// If Flags & Enum_FlagMask == Enum_IsOpaque, then the type does not
    /// support any optimized representation in enums.
    Enum_IsOpaque = 0x00000,
    /// If Flags & Enum_FlagMask == Enum_HasExtraInhabitants, then the type
    /// has "extra inhabitants" of its binary representation which do not form
    /// valid values of the type, such as null in a class type. The
    /// ExtraInhabitants value witnesses are present in the value witness table.
    Enum_HasExtraInhabitants = 0x40000,
    /// If Flags & Enum_FlagMask == Enum_HasSpareBits, then the type has
    /// unused bits in its binary representation. This implies
    /// HasExtraInhabitants. Both the ExtraInhabitants and SpareBits value
    /// witnesses are present in the value witness table.
    Enum_HasSpareBits = 0xC0000,

    IsNonBitwiseTakable = 0x100000,

    /// If Flags & HasEnumWitnesses, then enum value witnesses are present in
    /// the value witness table.
    HasEnumWitnesses = 0x00200000,
  };
}
  
namespace ExtraInhabitantFlags {
  enum : uint64_t {
    NumExtraInhabitantsMask = 0x7FFFFFFFULL,
    
    NumSpareBitsMask   = 0x0000FFFF00000000ULL,
    NumSpareBitsShift  = 32,
    
    SpareBitsShiftMask = 0xFFFF000000000000ULL,
    SpareBitsShiftShift = 48,
  };
}
 
enum {
  NumRequiredValueWitnesses
    = unsigned(ValueWitness::Last_RequiredValueWitness) + 1,
  NumRequiredValueWitnessFunctions
    = unsigned(ValueWitness::Last_RequiredValueWitnessFunction) + 1,
  
  MaxNumValueWitnesses
    = unsigned(ValueWitness::Last_ValueWitness) + 1,
  MaxNumTypeLayoutWitnesses
    = unsigned(ValueWitness::Last_TypeLayoutWitness)
      - unsigned(ValueWitness::First_TypeLayoutWitness)
      + 1,
};

static inline bool isValueWitnessFunction(ValueWitness witness) {
#define WANT_ALL_VALUE_WITNESSES 1
#define FUNCTION_VALUE_WITNESS(name, Name, ret, args) \
  if (witness == ValueWitness::Name) \
    return true;
#define DATA_VALUE_WITNESS(name, Name, ty)
#include "swift/ABI/ValueWitness.def"

  return false;
}

const char *getValueWitnessName(ValueWitness witness);

} // end namespace irgen
} // end namespace swift

#endif
