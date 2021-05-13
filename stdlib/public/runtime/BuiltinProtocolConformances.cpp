//===--- BuiltinProtocolConformances.cpp ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Definitions of some builtin protocol witnesses.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/BuiltinProtocolConformances.h"
#include "swift/Runtime/Casting.h"
#include "swift/Runtime/Debug.h"
#include "swift/Runtime/Metadata.h"

#include <vector>

using namespace swift;

using StaticInfixWitness = SWIFT_CC(swift) bool(OpaqueValue *, OpaqueValue *,
                                                SWIFT_CONTEXT const Metadata *,
                                                const Metadata *,
                                                const WitnessTable *);

#if defined(__ELF__)
#define CONFORMANCE_PRIVATE_DATA(NAME) \
          __asm( \
            "  .type " SYMBOL(NAME) ", @object\n" \
            "  .local " SYMBOL(NAME) "\n" \
            "  .comm " SYMBOL(NAME) ", 128, 16\n" \
          );
#elif defined(__MACH__)
#define CONFORMANCE_PRIVATE_DATA(NAME) \
          __asm( \
            "  .zerofill __DATA, __bss, " SYMBOL(NAME) ", 128, 4\n" \
          );
#elif defined(_WIN32)
#define CONFORMANCE_PRIVATE_DATA(NAME) \
          __asm( \
            "  .lcomm " SYMBOL(NAME) ", 128, 16\n" \
          );
#endif

//===----------------------------------------------------------------------===//
// Associated Conformances
//===----------------------------------------------------------------------===//

SWIFT_ASSOCIATED_CONFORMANCE
_AssociatedConformance _swift_tupleComparable_associatedConformance = {
  // Header bits for the demangler.
  255,
  7,

  // This is a direct relative reference to the base accessor for tuple
  // Equatable defined below.
  {
    0
  },

  // Null
  0
};

//===----------------------------------------------------------------------===//
// Builtin Conformance Initializer
//===----------------------------------------------------------------------===//

// Equatable Information
extern "C" const ProtocolDescriptor EQUATABLE_DESCRIPTOR;
extern "C" const MethodDescriptor EQUATABLE_EE_METHOD_DESCRIPTOR;
extern "C" const char __swift_tupleEquatable_private;

// Comparable Information
extern "C" const ProtocolDescriptor COMPARABLE_DESCRIPTOR;
extern "C" const MethodDescriptor COMPARABLE_BASE_CONFORMANCE_DESCRIPTOR;
extern "C" const MethodDescriptor COMPARABLE_LT_METHOD_DESCRIPTOR;
extern "C" const MethodDescriptor COMPARBALE_LTE_METHOD_DESCRIPTOR;
extern "C" const MethodDescriptor COMPARABLE_GTE_METHOD_DESCRIPTOR;
extern "C" const MethodDescriptor COMPARABLE_GT_METHOD_DESCRIPTOR;
extern "C" const char __swift_tupleComparable_private;

// Hashable Information
extern "C" const ProtocolDescriptor HASHABLE_DESCRIPTOR;
extern "C" const MethodDescriptor HASHABLE_BASE_CONFORMANCE_DESCRIPTOR;
extern "C" const MethodDescriptor HASHABLE_HASHVALUE_METHOD_DESCRIPTOR;
extern "C" const MethodDescriptor HASHABLE_HASH_METHOD_DESCRIPTOR;
extern "C" const MethodDescriptor HASHABLE_RAWHASHVALUE_METHOD_DESCRIPTOR;
extern "C" const char __swift_tupleHashable_private;

// This is needed to properly emplace relative references within the data
// structures defined in this file. Because we can't directly express relative
// references within C++, we have to calculate the offsets ourselves. A
// previous version used assembly to work around this, but the use of assembly
// is very non-portable and makes it incredibly easy to get things wrong.
__attribute__((__constructor__))
static void initializeBuiltinConformances() {
  //---------------------------------//
  // Tuple Equatable Initialization
  //---------------------------------//

  // Setup the protocol descriptor.
  _swift_tupleEquatable_conf.protocol.set(&EQUATABLE_DESCRIPTOR);

  // Setup the == witness (MethodDescriptor + Impl).
  _swift_tupleEquatable_conf.resilientWitnesses[0].methodDescriptor.set(
      &EQUATABLE_EE_METHOD_DESCRIPTOR);
  _swift_tupleEquatable_conf.resilientWitnesses[0].impl.set(
      reinterpret_cast<void *>(_swift_tupleEquatable_equals));

  // Setup the private data.
  _swift_tupleEquatable_conf.privateData.set(&__swift_tupleEquatable_private);

  //---------------------------------//
  // Tuple Comparable Initialization
  //---------------------------------//

  // Setup the protocol descriptor.
  _swift_tupleComparable_conf.protocol.set(&COMPARABLE_DESCRIPTOR);

  // Setup the associated conformance witness (MethodDescriptor + Impl).
  _swift_tupleComparable_conf.resilientWitnesses[0].methodDescriptor.set(
      &COMPARABLE_BASE_CONFORMANCE_DESCRIPTOR);
  _swift_tupleComparable_conf.resilientWitnesses[0].impl.set(
      reinterpret_cast<void *>(&_swift_tupleComparable_associatedConformance));

  // Setup the < witness (MethodDescriptor + Impl).
  _swift_tupleComparable_conf.resilientWitnesses[1].methodDescriptor.set(
      &COMPARABLE_LT_METHOD_DESCRIPTOR);
  _swift_tupleComparable_conf.resilientWitnesses[1].impl.set(
      reinterpret_cast<void *>(_swift_tupleComparable_lessThan));

  // Setup the <= witness (MethodDescriptor + Impl).
  _swift_tupleComparable_conf.resilientWitnesses[2].methodDescriptor.set(
      &COMPARBALE_LTE_METHOD_DESCRIPTOR);
  _swift_tupleComparable_conf.resilientWitnesses[2].impl.set(
      reinterpret_cast<void *>(_swift_tupleComparable_lessThanOrEqual));

  // Setup the >= witness (MethodDescriptor + Impl).
  _swift_tupleComparable_conf.resilientWitnesses[3].methodDescriptor.set(
      &COMPARABLE_GTE_METHOD_DESCRIPTOR);
  _swift_tupleComparable_conf.resilientWitnesses[3].impl.set(
      reinterpret_cast<void *>(_swift_tupleComparable_greaterThanOrEqual));

  // Setup the > witness (MethodDescriptor + Impl).
  _swift_tupleComparable_conf.resilientWitnesses[4].methodDescriptor.set(
      &COMPARABLE_GT_METHOD_DESCRIPTOR);
  _swift_tupleComparable_conf.resilientWitnesses[4].impl.set(
      reinterpret_cast<void *>(_swift_tupleComparable_greaterThan));

  // Setup the private data.
  _swift_tupleComparable_conf.privateData.set(&__swift_tupleComparable_private);

  // Setup the associated conformances accessor implementation.
  _swift_tupleComparable_associatedConformance.baseAccessor.set(
      reinterpret_cast<void *>(_swift_tupleComparable_baseAccessorEquatable));

  //---------------------------------//
  // Tuple Hashable Initialization
  //---------------------------------//

  // Setup the protocol descriptor.
  _swift_tupleHashable_conf.protocol.set(&HASHABLE_DESCRIPTOR);

  // Setup the associated conformance witness (MethodDescriptor + Impl).
  _swift_tupleHashable_conf.resilientWitnesses[0].methodDescriptor.set(
      &HASHABLE_BASE_CONFORMANCE_DESCRIPTOR);
  // NOTE: We intentionally use the Comparable implementation for this because
  // the implementation is the same for both Hashable and Comparable. Both want
  // to grab the Equatable table from its elements whose witness table is
  // located in the same place for both protocols.
  _swift_tupleHashable_conf.resilientWitnesses[0].impl.set(
      reinterpret_cast<void *>(&_swift_tupleComparable_associatedConformance));

  // Setup the hashValue witness (MethodDescriptor + Impl).
  _swift_tupleHashable_conf.resilientWitnesses[1].methodDescriptor.set(
      &HASHABLE_HASHVALUE_METHOD_DESCRIPTOR);
  _swift_tupleHashable_conf.resilientWitnesses[1].impl.set(
      reinterpret_cast<void *>(&_swift_tupleHashable_hashValue));

  // Setup the hash(into:) witness (MethodDescriptor + Impl).
  _swift_tupleHashable_conf.resilientWitnesses[2].methodDescriptor.set(
      &HASHABLE_HASH_METHOD_DESCRIPTOR);
  _swift_tupleHashable_conf.resilientWitnesses[2].impl.set(
      reinterpret_cast<void *>(&_swift_tupleHashable_hash));

  // Setup the _rawHashValue witness (MethodDescriptor + Impl).
  _swift_tupleHashable_conf.resilientWitnesses[3].methodDescriptor.set(
      &HASHABLE_RAWHASHVALUE_METHOD_DESCRIPTOR);
  // NOTE: Because we're not setting the implementation pointer (which is
  // currently 0), that means we're requesting the default implementation for
  // this witness.

  // Setup the private data.
  _swift_tupleHashable_conf.privateData.set(&__swift_tupleHashable_private);
}

//===----------------------------------------------------------------------===//
// Tuple Equatable Conformance
//===----------------------------------------------------------------------===//

CONFORMANCE_PRIVATE_DATA("__swift_tupleEquatable_private")
SWIFT_RUNTIME_EXPORT
_ConformanceDescriptor<1> swift::_swift_tupleEquatable_conf = {
  // This is an indirectable relative reference to the Equatable protocol
  // descriptor.
  {
    0
  },

  // 769 is the MetadataKind::Tuple
  769,

  // This indicates that we have no witness table pattern. We use a generic
  // witness table for builtin conformances.
  0,

  // 196640 are the ConformanceFlags with the type reference bit set to
  // MetadataKind, the has resilient witness bit, and the generic witness table
  // bit.
  196640,

  // This 1 is the ResilientWitnessesHeader indicating we have 1 resilient
  // witness.
  1,

  // This is our resilient witness list.
  {
    {
      // This is an indirectable relative reference to the Equatable ==
      // method descriptor.
      {
        0
      },

      // This is a direct relative reference to the equals witness defined below.
      {
        0
      }
    }
  },

  // The witness table size in words.
  0,

  // The witness table private size in words & requires instantiation.
  1,

  // The witness table instantiator function (we don't have one).
  0,

  // This is a direct relative reference to the private data for the
  // conformance.
  {
    0
  }
};

/// The protocol witness for static Swift.Equatable.== infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Equatable in Swift.
SWIFT_CC(swift)
bool swift::_swift_tupleEquatable_equals(OpaqueValue *tuple1,
                                         OpaqueValue *tuple2,
                                         SWIFT_CONTEXT Metadata *swiftSelf,
                                         Metadata *Self, void *witnessTable) {
  auto tuple = cast<TupleTypeMetadata>(Self);
  auto table = reinterpret_cast<void**>(witnessTable);

  // Loop through all elements, and check if both tuples element is equal.
  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);

    // Get the element conformance from the private data in the witness table.
    auto conformance = reinterpret_cast<const WitnessTable *>(table[-1 - i]);

    // Get the respective values from both tuples.
    auto value1 = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple1) + elt.Offset);
    auto value2 = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple2) + elt.Offset);

    // Grab the specific witness for this element type.
    auto equatableTable = reinterpret_cast<void * const *>(conformance);
    auto equalsWitness = equatableTable[WitnessTableFirstRequirementOffset];
    auto equals = reinterpret_cast<StaticInfixWitness *>(equalsWitness);

    // Call the equal function
    auto result = equals(value1, value2, elt.Type, elt.Type, conformance);

    // If the values aren't equal, this tuple isn't equal. :)
    if (!result)
      return false;
  }

  // Otherwise this tuple has value equality with all elements.
  return true;
}

//===----------------------------------------------------------------------===//
// Tuple Comparable Conformance
//===----------------------------------------------------------------------===//

CONFORMANCE_PRIVATE_DATA("__swift_tupleComparable_private")
SWIFT_RUNTIME_EXPORT
_ConformanceDescriptor<5> swift::_swift_tupleComparable_conf = {
  // This is an indirectable relative reference to the Comparable protocol
  // descriptor.
  {
    0
  },

  // 769 is the MetadataKind::Tuple
  769,

  // This indicates that we have no witness table pattern. We use a generic
  // witness table for builtin conformances.
  0,

  // 196640 are the ConformanceFlags with the type reference bit set to
  // MetadataKind, the has resilient witness bit, and the generic witness table
  // bit.
  196640,

  // This 5 is the ResilientWitnessesHeader indicating we have 5 resilient
  // witnesses.
  5,

  // This is our resilient witness list.
  {
    {
      // This is an indirectable relative reference to the Comparable base
      // conformance for Equatable.
      {
        0
      },

      // This is a direct relative reference to the associated conformance for
      // Equatable defined above.
      {
        0
      }
    },
    {
      // This is an indirectable relative reference to the Comparable.<
      // method descriptor.
      {
        0
      },

      // This is a direct relative reference to the < witness defined below.
      {
        0
      }
    },
    {
      // This is an indirectable relative reference to the Comparable.<=
      // method descriptor.
      {
        0
      },

      // This is a direct relative reference to the <= witness defined below.
      {
        0
      }
    },
    {
      // This is an indirectable relative reference to the Comparable.>=
      // method descriptor.
      {
        0
      },

      // This is a direct relative reference to the >= witness defined below.
      {
        0
      }
    },
    {
      // This is an indirectable relative reference to the Comparable.>
      // method descriptor.
      {
        0
      },

      // This is a direct relative reference to the > witness defined below.
      {
        0
      }
    }
  },

  // The witness table size in words.
  0,

  // The witness table private size in words & requires instantiation.
  1,

  // The witness table instantiator function (we don't have one).
  0,

  // This is a direct relative reference to the private data for the
  // conformance.
  {
    0
  }
};

SWIFT_CC(swift)
const WitnessTable *
swift::_swift_tupleComparable_baseAccessorEquatable(Metadata *assocType,
                                                    Metadata *conformingType,
                                                    void **witnessTable) {
  auto tuple = cast<TupleTypeMetadata>(assocType);
  std::vector<void *> instantiationArgs;
  instantiationArgs.reserve(tuple->NumElements);

  // Fill the instantiationArgs with the element Equatable tables.
  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    // Get the element's Comparable table.
    auto comparableTable = reinterpret_cast<void **>(witnessTable[-1 - i]);

    // The Equatable table is the first requirement, thus it'll be right after
    // the conformance descriptor.
    auto equatableTable = comparableTable[WitnessTableFirstRequirementOffset];

    instantiationArgs.push_back(equatableTable);
  }

  auto tupleEquatableConf = reinterpret_cast<ProtocolConformanceDescriptor *>(
      &_swift_tupleEquatable_conf);
  // Finally, call getWitnessTable to realize the tuple's Equatable table.
  auto equatableTable = swift_getWitnessTable(tupleEquatableConf,
                                              assocType,
                                              instantiationArgs.data());

  return equatableTable;
}

SWIFT_CC(swift)
bool swift::_swift_tupleComparable_lessThan(OpaqueValue *tuple1,
                                            OpaqueValue *tuple2,
                                            SWIFT_CONTEXT Metadata *swiftSelf,
                                            Metadata *Self,
                                            void *witnessTable) {
  auto tuple = cast<TupleTypeMetadata>(Self);
  auto table = reinterpret_cast<void**>(witnessTable);

  // Loop through all elements, and check if both tuples element is equal.
  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);

    // Get the element conformance from the private data in the witness table.
    auto conformance = reinterpret_cast<const WitnessTable *>(table[-1 - i]);

    // Get the respective values from both tuples.
    auto value1 = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple1) + elt.Offset);
    auto value2 = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple2) + elt.Offset);

    // First, grab the equatable conformance to prepare to check if the elements
    // are equal. (Since this require Equatable conformance, the witness table
    // is right after the conformance descriptor, which is at index 0.)
    auto comparableTable = reinterpret_cast<void * const *>(conformance);
    auto equatableTable = reinterpret_cast<void * const *>(comparableTable[1]);
    auto equalsWitness = equatableTable[WitnessTableFirstRequirementOffset];
    auto equals = reinterpret_cast<StaticInfixWitness *>(equalsWitness);

    // Call the equal function.
    auto isEqual = equals(value1, value2, elt.Type, elt.Type,
                        reinterpret_cast<const WitnessTable *>(equatableTable));

    // If these are equal, skip to the next element.
    if (isEqual)
      continue;

    // Now that we know they are not equal, we can call their less than function
    // and return the result.
    auto lessThanWitness = comparableTable[WitnessTableFirstRequirementOffset + 1];
    auto lessThan = reinterpret_cast<StaticInfixWitness *>(lessThanWitness);

    // Call the less than function.
    auto isLessThan = lessThan(value1, value2, elt.Type, elt.Type, conformance);

    return isLessThan;
  }

  // Otherwise these tuples are completely equal, thus they are not less than
  // each other.
  return false;
}

SWIFT_CC(swift)
bool swift::_swift_tupleComparable_lessThanOrEqual(OpaqueValue *tuple1,
                                                   OpaqueValue *tuple2,
                                              SWIFT_CONTEXT Metadata *swiftSelf,
                                                   Metadata *Self,
                                                   void *witnessTable) {
  auto tuple = cast<TupleTypeMetadata>(Self);
  auto table = reinterpret_cast<void**>(witnessTable);

  // Loop through all elements, and check if both tuples element is equal.
  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);

    // Get the element conformance from the private data in the witness table.
    auto conformance = reinterpret_cast<const WitnessTable *>(table[-1 - i]);

    // Get the respective values from both tuples.
    auto value1 = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple1) + elt.Offset);
    auto value2 = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple2) + elt.Offset);

    // First, grab the equatable conformance to prepare to check if the elements
    // are equal. (Since this require Equatable conformance, the witness table
    // is right after the conformance descriptor, which is at index 0.)
    auto comparableTable = reinterpret_cast<void * const *>(conformance);
    auto equatableTable = reinterpret_cast<void * const *>(comparableTable[1]);
    auto equalsWitness = equatableTable[WitnessTableFirstRequirementOffset];
    auto equals = reinterpret_cast<StaticInfixWitness *>(equalsWitness);

    // Call the equal function.
    auto isEqual = equals(value1, value2, elt.Type, elt.Type,
                        reinterpret_cast<const WitnessTable *>(equatableTable));

    // If these are equal, skip to the next element.
    if (isEqual)
      continue;

    // Now that we know they are not equal, we can call their less than or equal
    // function and return the result.
    auto lessThanOrEqualWitness = 
        comparableTable[WitnessTableFirstRequirementOffset + 2];
    auto lessThanOrEqual = 
        reinterpret_cast<StaticInfixWitness *>(lessThanOrEqualWitness);

    // Call the less than function.
    auto isLessThanOrEqual = lessThanOrEqual(value1, value2, elt.Type, elt.Type,
                                             conformance);

    return isLessThanOrEqual;
  }

  // Otherwise these tuples are completely equal.
  return true;
}

SWIFT_CC(swift)
bool swift::_swift_tupleComparable_greaterThanOrEqual(OpaqueValue *tuple1,
                                                      OpaqueValue *tuple2,
                                              SWIFT_CONTEXT Metadata *swiftSelf,
                                                      Metadata *Self,
                                                      void *witnessTable) {
  auto tuple = cast<TupleTypeMetadata>(Self);
  auto table = reinterpret_cast<void**>(witnessTable);

  // Loop through all elements, and check if both tuples element is equal.
  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);

    // Get the element conformance from the private data in the witness table.
    auto conformance = reinterpret_cast<const WitnessTable *>(table[-1 - i]);

    // Get the respective values from both tuples.
    auto value1 = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple1) + elt.Offset);
    auto value2 = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple2) + elt.Offset);

    // First, grab the equatable conformance to prepare to check if the elements
    // are equal. (Since this require Equatable conformance, the witness table
    // is right after the conformance descriptor, which is at index 0.)
    auto comparableTable = reinterpret_cast<void * const *>(conformance);
    auto equatableTable = reinterpret_cast<void * const *>(comparableTable[1]);
    auto equalsWitness = equatableTable[WitnessTableFirstRequirementOffset];
    auto equals = reinterpret_cast<StaticInfixWitness *>(equalsWitness);

    // Call the equal function.
    auto isEqual = equals(value1, value2, elt.Type, elt.Type,
                        reinterpret_cast<const WitnessTable *>(equatableTable));

    // If these are equal, skip to the next element.
    if (isEqual)
      continue;

    // Now that we know they are not equal, we can call their greater than or
    // equal function and return the result.
    auto greaterThanOrEqualWitness =
        comparableTable[WitnessTableFirstRequirementOffset + 3];
    auto greaterThanOrEqual =
        reinterpret_cast<StaticInfixWitness *>(greaterThanOrEqualWitness);

    // Call the greater than or equal function.
    auto isGreaterThanOrEqual = greaterThanOrEqual(value1, value2, elt.Type,
                                                   elt.Type, conformance);

    return isGreaterThanOrEqual;
  }

  // Otherwise these tuples are completely equal.
  return true;
}

SWIFT_CC(swift)
bool swift::_swift_tupleComparable_greaterThan(OpaqueValue *tuple1,
                                               OpaqueValue *tuple2,
                                              SWIFT_CONTEXT Metadata *swiftSelf,
                                               Metadata *Self,
                                               void *witnessTable) {
  auto tuple = cast<TupleTypeMetadata>(Self);
  auto table = reinterpret_cast<void**>(witnessTable);

  // Loop through all elements, and check if both tuples element is equal.
  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);

    // Get the element conformance from the private data in the witness table.
    auto conformance = reinterpret_cast<const WitnessTable *>(table[-1 - i]);

    // Get the respective values from both tuples.
    auto value1 = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple1) + elt.Offset);
    auto value2 = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple2) + elt.Offset);

    // First, grab the equatable conformance to prepare to check if the elements
    // are equal. (Since this require Equatable conformance, the witness table
    // is right after the conformance descriptor, which is at index 0.)
    auto comparableTable = reinterpret_cast<void * const *>(conformance);
    auto equatableTable = reinterpret_cast<void * const *>(comparableTable[1]);
    auto equalsWitness = equatableTable[WitnessTableFirstRequirementOffset];
    auto equals = reinterpret_cast<StaticInfixWitness *>(equalsWitness);

    // Call the equal function.
    auto isEqual = equals(value1, value2, elt.Type, elt.Type,
                        reinterpret_cast<const WitnessTable *>(equatableTable));

    // If these are equal, skip to the next element.
    if (isEqual)
      continue;

    // Now that we know they are not equal, we can call their greater than
    // function and return the result.
    auto greaterThanWitness =
        comparableTable[WitnessTableFirstRequirementOffset + 4];
    auto greaterThan =
        reinterpret_cast<StaticInfixWitness *>(greaterThanWitness);

    // Call the greater than function.
    auto isGreaterThan = greaterThan(value1, value2, elt.Type, elt.Type,
                                     conformance);

    return isGreaterThan;
  }

  // Otherwise these tuples are completely equal, thus they are not greater than
  // each other.
  return false;
}

//===----------------------------------------------------------------------===//
// Tuple Hashable Conformance
//===----------------------------------------------------------------------===//

extern "C" SWIFT_CC(swift)
intptr_t SWIFT_HASHVALUE_FUNC(OpaqueValue *value, Metadata *Self,
                              void *witnessTable);

extern "C" SWIFT_CC(swift)
void SWIFT_HASHER_COMBINE_FUNC(OpaqueValue *value, const Metadata *Self,
                               const WitnessTable *witnessTable,
                               SWIFT_CONTEXT OpaqueValue *hasher);

CONFORMANCE_PRIVATE_DATA("__swift_tupleHashable_private")
SWIFT_RUNTIME_EXPORT
_ConformanceDescriptor<4> swift::_swift_tupleHashable_conf = {
  // This is an indirectable relative reference to the Hashable protocol
  // descriptor.
  {
    0
  },

  // 769 is the MetadataKind::Tuple
  769,

  // This indicates that we have no witness table pattern. We use a generic
  // witness table for builtin conformances.
  0,

  // 196640 are the ConformanceFlags with the type reference bit set to
  // MetadataKind, the has resilient witness bit, and the generic witness table
  // bit.
  196640,

  // This 4 is the ResilientWitnessesHeader indicating we have 4 resilient
  // witnesses.
  4,

  // This is our resilient witness list.
  {
    {
      // This is an indirectable relative reference to the Hashable base
      // conformance for Equatable.
      {
        0
      },

      // This is a direct relative reference to the associated conformance for
      // Equatable defined above.
      {
        0
      }
    },
    {
      // This is an indirectable relative reference to the Hashable hashValue
      // method descriptor.
      {
        0
      },

      // This is a direct relative reference to the hashValue witness defined
      // below.
      {
        0
      }
    },
    {
      // This is an indirectable relative reference to the Hashable hash(into:)
      // method descriptor.
      {
        0
      },

      // This is a direct relative reference to the hash witness defined below.
      {
        0
      }
    },
    {
      // This is an indirectable relative reference to the Hashable
      // _rawHashValue method descriptor.
      {
        0
      },

      // This is requesting the default implemenation of _rawHashValue.
      {
        0
      }
    }
  },

  // The witness table size in words.
  0,

  // The witness table private size in words & requires instantiation.
  1,

  // The witness table instantiator function (we don't have one).
  0,

  // This is a direct relative reference to the private data for the
  // conformance.
  {
    0
  }
};

SWIFT_CC(swift)
intptr_t swift::_swift_tupleHashable_hashValue(SWIFT_CONTEXT OpaqueValue *tuple,
                                               Metadata *Self,
                                               void *witnessTable) {
  return SWIFT_HASHVALUE_FUNC(tuple, Self, witnessTable);
}

SWIFT_CC(swift)
void swift::_swift_tupleHashable_hash(OpaqueValue *hasher,
                                      SWIFT_CONTEXT OpaqueValue *tuple,
                                      Metadata *Self, void *witnessTable) {
  auto tupleTy = cast<TupleTypeMetadata>(Self);
  auto table = reinterpret_cast<void**>(witnessTable);

  // Loop through all elements and hash them into the Hasher.
  for (size_t i = 0; i != tupleTy->NumElements; i += 1) {
    auto elt = tupleTy->getElement(i);

    // Get the element conformance from the private data in the witness table.
    auto conformance = reinterpret_cast<const WitnessTable *>(table[-1 - i]);

    // Get the element value from the tuple.
    auto value = reinterpret_cast<OpaqueValue *>(
                  reinterpret_cast<char *>(tuple) + elt.Offset);

    // Call the combine function on the hasher for this element value and we're
    // done!
    SWIFT_HASHER_COMBINE_FUNC(value, elt.Type, conformance, hasher);
  }
}
