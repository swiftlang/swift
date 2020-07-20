//===--- BuiltinProtocolConformances.cpp ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
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

//===----------------------------------------------------------------------===//
// Tuple Equatable Conformance
//===----------------------------------------------------------------------===//

#if defined(__ELF__)
// Create a GOT equivalent for the Equatable reference.
__asm(
  "  .type got." EQUATABLE_DESCRIPTOR_SYMBOL ", @object\n"
  "  .section .data.rel.ro\n"
  "  .p2align 3\n"
  "got." EQUATABLE_DESCRIPTOR_SYMBOL ":\n"
  "  .quad (" EQUATABLE_DESCRIPTOR_SYMBOL ")\n"
  "  .size got." EQUATABLE_DESCRIPTOR_SYMBOL ", 8\n"
);

// Create a GOT equivalent for the Equatable.== method descriptor.
__asm(
  "  .type got." EQUATABLE_EE_METHOD_DESCRIPTOR ", @object\n"
  "  .p2align 3\n"
  "got." EQUATABLE_EE_METHOD_DESCRIPTOR ":\n"
  "  .quad (" EQUATABLE_EE_METHOD_DESCRIPTOR ")\n"
  "  .size got." EQUATABLE_EE_METHOD_DESCRIPTOR ", 8\n"
);
#endif

// Define the conformance descriptor for tuple Equatable. We do this in
// assembly to work around relative reference issues.
__asm(
  #if defined(__ELF__)
  "  .type __swift_tupleEquatable_private, @object\n"
  "  .local __swift_tupleEquatable_private\n"
  "  .comm __swift_tupleEquatable_private, 128, 16\n"
  "  .protected " TUPLE_EQUATABLE_CONF "\n"
  "  .type " TUPLE_EQUATABLE_CONF ", @object\n"
  "  .section .rodata\n"
  #elif defined(__MACH__)
  "  .zerofill __DATA, __bss, __swift_tupleEquatable_private, 128, 4\n"
  "  .section __TEXT, __const\n"
  #endif
  "  .globl " TUPLE_EQUATABLE_CONF "\n"
  "  .p2align 2\n"
  TUPLE_EQUATABLE_CONF ":\n"
  #if defined(__ELF__)
  // This is an indirectable relative reference to the GOT equivalent for the
  // Equatable protocol descriptor, hence why we add 1 to indicate indirect.
  "  .long (got." EQUATABLE_DESCRIPTOR_SYMBOL " - \
              (" TUPLE_EQUATABLE_CONF ")) + 1\n"
  #elif defined(__MACH__)
  "  .long " EQUATABLE_DESCRIPTOR_SYMBOL "@GOTPCREL + 5\n"
  #endif
  // 769 is the MetadataKind::Tuple
  "  .long 769\n"
  // This indicates that we have no witness table pattern. We use a generic
  // witness table for builtin conformances.
  "  .long 0\n"
  // 196640 are the ConformanceFlags with the type reference bit set to
  // MetadataKind, the has resilient witness bit, and the generic witness table
  // bit.
  "  .long 196640\n"
  // This 1 is the ResilientWitnessesHeader indicating we have 1 resilient
  // witness.
  "  .long 1\n"
  #if defined(__ELF__)
  // This is an indirectable relative reference to the GOT equivalent for the
  // Equatable.== method descriptor, hence why we add 1 to indicate indirect.
  "  .long ((got." EQUATABLE_EE_METHOD_DESCRIPTOR " - \
              (" TUPLE_EQUATABLE_CONF ")) - 20) + 1\n"
  #elif defined(__MACH__)
  "  .long " EQUATABLE_EE_METHOD_DESCRIPTOR "@GOTPCREL + 5\n"
  #endif
  // This is a direct relative reference to the equals witness defined below.
  "  .long ((" TUPLE_EQUATABLE_EQUALS ") - (" TUPLE_EQUATABLE_CONF ")) - 24\n"
  // The witness table size in words.
  "  .short 0\n"
  // The witness table private size in words & requires instantiation.
  "  .short 1\n"
  // The witness table instantiator function.
  "  .long 0\n"
  // This is a direct relative reference to the private data for the
  // conformance.
  "  .long (__swift_tupleEquatable_private - (" TUPLE_EQUATABLE_CONF ")) - 36\n"
  #if defined(__ELF__)
  "  .size " TUPLE_EQUATABLE_CONF ", 40\n"
  #endif
);

extern const ProtocolConformanceDescriptor _swift_tupleEquatable_conf;

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
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

#if defined(__ELF__)
// Create a GOT equivalent for the Comparable reference.
__asm(
  "  .type got." COMPARABLE_DESCRIPTOR_SYMBOL ", @object\n"
  "  .section .data.rel.ro\n"
  "  .p2align 3\n"
  "got." COMPARABLE_DESCRIPTOR_SYMBOL ":\n"
  "  .quad (" COMPARABLE_DESCRIPTOR_SYMBOL ")\n"
  "  .size got." COMPARABLE_DESCRIPTOR_SYMBOL ", 8\n"
);

// Create a GOT equivalent for the Comparable base conformance to Equatable.
__asm(
  "  .type got." COMPARABLE_BASE_CONFORMANCE_DESCRIPTOR ", @object\n"
  "  .p2align 3\n"
  "got." COMPARABLE_BASE_CONFORMANCE_DESCRIPTOR ":\n"
  "  .quad (" COMPARABLE_BASE_CONFORMANCE_DESCRIPTOR ")\n"
  "  .size got." COMPARABLE_BASE_CONFORMANCE_DESCRIPTOR ", 8\n"
);

// Create a GOT equivalent for the Comparable.< method descriptor.
__asm(
  "  .type got." COMPARABLE_LT_METHOD_DESCRIPTOR ", @object\n"
  "  .p2align 3\n"
  "got." COMPARABLE_LT_METHOD_DESCRIPTOR ":\n"
  "  .quad (" COMPARABLE_LT_METHOD_DESCRIPTOR ")\n"
  "  .size got." COMPARABLE_LT_METHOD_DESCRIPTOR ", 8\n"
);

// Create a GOT equivalent for the Comparable.<= method descriptor.
__asm(
  "  .type got." COMPARBALE_LTE_METHOD_DESCRIPTOR ", @object\n"
  "  .p2align 3\n"
  "got." COMPARBALE_LTE_METHOD_DESCRIPTOR ":\n"
  "  .quad (" COMPARBALE_LTE_METHOD_DESCRIPTOR ")\n"
  "  .size got." COMPARBALE_LTE_METHOD_DESCRIPTOR ", 8\n"
);

// Create a GOT equivalent for the Comparable.>= method descriptor.
__asm(
  "  .type got." COMPARABLE_GTE_METHOD_DESCRIPTOR ", @object\n"
  "  .p2align 3\n"
  "got." COMPARABLE_GTE_METHOD_DESCRIPTOR ":\n"
  "  .quad (" COMPARABLE_GTE_METHOD_DESCRIPTOR ")\n"
  "  .size got." COMPARABLE_GTE_METHOD_DESCRIPTOR ", 8\n"
);

// Create a GOT equivalent for the Comparable.> method descriptor.
__asm(
  "  .type got." COMPARABLE_GT_METHOD_DESCRIPTOR ", @object\n"
  "  .p2align 3\n"
  "got." COMPARABLE_GT_METHOD_DESCRIPTOR ":\n"
  "  .quad (" COMPARABLE_GT_METHOD_DESCRIPTOR ")\n"
  "  .size got." COMPARABLE_GT_METHOD_DESCRIPTOR ", 8\n"
);
#endif

// Define the associated conformance structure for tuple Comparable. We do this
// in assembly to work around relative reference issues.
__asm(
  #if defined(__ELF__)
  "  .hidden \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\"\n"
  "  .type \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\", @object\n"
  "  .section swift5_typeref, \"a\""
  "  .weak \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\""
  #elif defined(__MACH__)
  "  .private_extern \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\"\n"
  "  .section __TEXT, __swift5_typeref\n"
  "  .globl \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\"\n"
  "  .weak_definition \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\"\n"
  #endif
  "  .p2align 1\n"
  "\"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\":\n"
  "  .byte 255\n"
  "  .byte 7\n"
  // This is a direct relative reference to the base accessor for Equatable
  // defined below.
  "  .long ((" TUPLE_COMPARABLE_BASEACCESSOREQUATABLE ") - \
            (\"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\")) - 2\n"
  // This 0 is our null terminator.
  "  .byte 0\n"
  #if defined (__ELF__)
  "  .size \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\", 7\n"
  #endif
);

// Define the conformance descriptor for tuple Comparable. We do this in
// assembly to work around relative reference issues.
__asm(
  #if defined(__ELF__)
  "  .type __swift_tupleComparable_private, @object\n"
  "  .local __swift_tupleComparable_private\n"
  "  .comm __swift_tupleComparable_private, 128, 16\n"
  "  .protected " TUPLE_COMPARABLE_CONF "\n"
  "  .type " TUPLE_COMPARABLE_CONF ", @object\n"
  "  .section .rodata\n"
  #elif defined(__MACH__)
  "  .zerofill __DATA, __bss, __swift_tupleComparable_private, 128, 4\n"
  "  .section __TEXT, __const\n"
  #endif
  "  .globl " TUPLE_COMPARABLE_CONF "\n"
  "  .p2align 2\n"
  TUPLE_COMPARABLE_CONF ":\n"
  #if defined(__ELF__)
  // This is an indirectable relative reference to the GOT equivalent for the
  // Comparable protocol descriptor, hence why we add 1 to indicate indirect.
  "  .long (got." COMPARABLE_DESCRIPTOR_SYMBOL " - \
              (" TUPLE_COMPARABLE_CONF ")) + 1\n"
  #elif defined(__MACH__)
  "  .long " COMPARABLE_DESCRIPTOR_SYMBOL "@GOTPCREL + 5\n"
  #endif
  // 769 is the MetadataKind::Tuple
  "  .long 769\n"
  // This indicates that we have no witness table pattern. We use a generic
  // witness table for builtin conformances.
  "  .long 0\n"
  // 196640 are the ConformanceFlags with the type reference bit set to
  // MetadataKind, the has resilient witness bit, and the generic witness table
  // bit.
  "  .long 196640\n"
  // This 5 is the ResilientWitnessesHeader indicating we have 5 resilient
  // witnesses.
  "  .long 5\n"
  #if defined(__ELF__)
  // This is an indirectable relative reference to the GOT equivalent for the
  // Comparable base conformance for Equatable, hence why we add 1 to indicate
  // indirect.
  "  .long ((got." COMPARABLE_BASE_CONFORMANCE_DESCRIPTOR " - \
              (" TUPLE_COMPARABLE_CONF ")) - 20) + 1\n"
  #elif defined(__MACH__)
  "  .long " COMPARABLE_BASE_CONFORMANCE_DESCRIPTOR "@GOTPCREL + 5\n"
  #endif
  // This is a direct relative reference to the associated conformance for
  // Equatable defined above in assembly. NOTE: This is minus 23 because the
  // associated conformance structure is 1 aligned.
  "  .long ((\"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\") - \
            (" TUPLE_COMPARABLE_CONF ")) - 23\n"
  #if defined(__ELF__)
  // This is an indirectable relative reference to the GOT equivalent for the
  // Comparable.< method descriptor, hence why we add 1 to indicate indirect.
  "  .long ((got." COMPARABLE_LT_METHOD_DESCRIPTOR " - \
              (" TUPLE_COMPARABLE_CONF ")) - 28) + 1\n"
  #elif defined(__MACH__)
  "  .long " COMPARABLE_LT_METHOD_DESCRIPTOR "@GOTPCREL + 5\n"
  #endif
  // This is a direct relative reference to the less than witness defined below.
  "  .long ((" TUPLE_COMPARABLE_LESSTHAN ") - (" TUPLE_COMPARABLE_CONF ")) - 32\n"
  #if defined(__ELF__)
  // This is an indirectable relative reference to the GOT equivalent for the
  // Comparable.<= method descriptor, hence why we add 1 to indicate
  // indirect.
  "  .long ((got." COMPARBALE_LTE_METHOD_DESCRIPTOR " - \
              (" TUPLE_COMPARABLE_CONF ")) - 36) + 1\n"
  #elif defined(__MACH__)
  "  .long " COMPARBALE_LTE_METHOD_DESCRIPTOR "@GOTPCREL + 5\n"
  #endif
  // This is a direct relative reference to the less than or equal witness
  // defined below.
  "  .long ((" TUPLE_COMPARABLE_LESSTHANOREQUAL ") - \
              (" TUPLE_COMPARABLE_CONF ")) - 40\n"
  #if defined(__ELF__)
  // This is an indirectable relative reference to the GOT equivalent for the
  // Comparable.>= method descriptor, hence why we add 1 to indicate
  // indirect.
  "  .long ((got." COMPARABLE_GTE_METHOD_DESCRIPTOR " - \
              (" TUPLE_COMPARABLE_CONF ")) - 44) + 1\n"
  #elif defined(__MACH__)
  "  .long " COMPARABLE_GTE_METHOD_DESCRIPTOR "@GOTPCREL + 5\n"
  #endif
  // This is a direct relative reference to the greater than or equal witness
  // defined below.
  "  .long ((" TUPLE_COMPARABLE_GREATERTHANOREQUAL ") - \
              (" TUPLE_COMPARABLE_CONF ")) - 48\n"
  #if defined(__ELF__)
  // This is an indirectable relative reference to the GOT equivalent for the
  // Comparable.> method descriptor, hence why we add 1 to indicate
  // indirect.
  "  .long ((got." COMPARABLE_GT_METHOD_DESCRIPTOR " - \
              (" TUPLE_COMPARABLE_CONF ")) - 52) + 1\n"
  #elif defined(__MACH__)
  "  .long " COMPARABLE_GT_METHOD_DESCRIPTOR "@GOTPCREL + 5\n"
  #endif
  // This is a direct relative reference to the greater than witness defined
  // below.
  "  .long ((" TUPLE_COMPARABLE_GREATERTHAN ") - \
              (" TUPLE_COMPARABLE_CONF ")) - 56\n"
  // The witness table size in words.
  "  .short 0\n"
  // The witness table private size in words & requires instantiation.
  "  .short 1\n"
  // The witness table instantiator function.
  "  .long 0\n"
  // This is a direct relative reference to the private data for the
  // conformance.
  "  .long (__swift_tupleComparable_private - (" TUPLE_COMPARABLE_CONF ")) - 68\n"
  #if defined(__ELF__)
  "  .size " TUPLE_COMPARABLE_CONF ", 72\n"
  #endif
);

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
const WitnessTable *
_swift_tupleComparable_baseAccessorEquatable(Metadata *assocType,
                                             Metadata *conformingType,
                                             void **witnessTable) {
  auto tuple = cast<TupleTypeMetadata>(assocType);
  std::vector<void *> instantiationArgs;

  // Fill the instantiationArgs with the element Equatable tables.
  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    // Get the element's Comparable table.
    auto comparableTable = reinterpret_cast<void **>(witnessTable[-1 - i]);

    // The Equatable table is the first requirement, thus it'll be right after
    // the conformance descriptor.
    auto equatableTable = comparableTable[WitnessTableFirstRequirementOffset];

    instantiationArgs.push_back(equatableTable);
  }

  // Finally, call getWitnessTable to realize the tuple's Equatable table.
  auto equatableTable = swift_getWitnessTable(&_swift_tupleEquatable_conf,
                                              assocType,
                                              instantiationArgs.data());

  return equatableTable;
}

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
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

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
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

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
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

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
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
