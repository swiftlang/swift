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

using namespace swift;

using StaticInfixWitness = SWIFT_CC(swift) bool(OpaqueValue *, OpaqueValue *,
                                                SWIFT_CONTEXT const Metadata *,
                                                const Metadata *,
                                                const WitnessTable *);

//===----------------------------------------------------------------------===//
// Tuple Equatable Conformance
//===----------------------------------------------------------------------===//

extern const ProtocolDescriptor
PROTOCOL_DESCRIPTOR_SYM(SWIFT_EQUATABLE_MANGLING);

#if defined(__ELF__)
// Create a GOT equivalent for the Equatable reference.
__asm(
  "  .type got.$sSQMp, @object\n"
  "  .section .data.rel.ro\n"
  "  .p2align 3\n"
  "got.$sSQMp:\n"
  "  .quad ($sSQMp)\n"
  "  .size got.$sSQMp, 8\n"
);
#endif

// Define the conformance descriptor for tuple Equatable. We do this in
// assembly to work around relative reference issues.
__asm(
  #if defined(__ELF__)
  "  .protected " TUPLE_EQUATABLE_CONF "\n"
  "  .type " TUPLE_EQUATABLE_CONF ", @object\n"
  "  .section .rodata\n"
  #elif defined(__MACH__)
  "  .section __TEXT,__const\n"
  #endif
  "  .globl " TUPLE_EQUATABLE_CONF "\n"
  "  .p2align 2\n"
  TUPLE_EQUATABLE_CONF ":\n"
  #if defined(__ELF__)
  // This is an indirectable relative reference to the GOT equivalent for the
  // Equatable protocol descriptor, hence why we add 1 to indicate indirect.
  "  .long (got.$sSQMp - (" TUPLE_EQUATABLE_CONF ")) + 1\n"
  #elif defined(__MACH__)
  "  .long _$sSQMp@GOTPCREL + 5\n"
  #endif
  // 769 is the MetadataKind::Tuple
  "  .long 769\n"
  // This is a direct relative reference to the witness table defined below.
  "  .long ((" TUPLE_EQUATABLE_WT ") - (" TUPLE_EQUATABLE_CONF ")) - 8\n"
  // 32 are the ConformanceFlags with the type reference bit set to MetadataKind.
  "  .long 32\n"
  #if defined(__ELF__)
  "  .size " TUPLE_EQUATABLE_CONF ", 16\n"
  #endif
);

extern const ProtocolConformanceDescriptor _swift_tupleEquatable_conf;

SWIFT_RUNTIME_EXPORT
const _WitnessTable<1> swift::_swift_tupleEquatable_wt = {
  &_swift_tupleEquatable_conf,
  {
    reinterpret_cast<void *>(_swift_tupleEquatable_equals)
  }
};

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
bool swift::_swift_tupleEquatable_equals(OpaqueValue *tuple1,
                                         OpaqueValue *tuple2,
                                         SWIFT_CONTEXT Metadata *swiftSelf,
                                         Metadata *Self, void *witnessTable) {
  auto tuple = cast<TupleTypeMetadata>(Self);

  // Loop through all elements, and check if both tuples element is equal.
  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);

    // Ensure we actually have a conformance to Equatable for this element type.
    auto equatable = &PROTOCOL_DESCRIPTOR_SYM(SWIFT_EQUATABLE_MANGLING);
    auto equatableConformance = swift_conformsToProtocol(elt.Type, equatable);

    // If we don't have a conformance then something somewhere messed up in
    // deciding that this tuple type was Equatable...??
    if (!equatableConformance)
      fatalError(0,
        "tuple element must be Equatable when evaluating tuple equalness");

    // Get the respective values from both tuples.
    auto value1 = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple1) + elt.Offset);
    auto value2 = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple2) + elt.Offset);

    // Grab the specific witness for this element type.
    auto equatableTable
        = reinterpret_cast<void * const *>(equatableConformance);
    auto equalsWitness = equatableTable[WitnessTableFirstRequirementOffset];
    auto equals = reinterpret_cast<StaticInfixWitness *>(equalsWitness);

    // Call the equal function.
    auto isEqual = equals(value1, value2, elt.Type, elt.Type,
                          equatableConformance);

    // If the values aren't equal, this tuple isn't equal. :)
    if (!isEqual)
      return false;
  }

  // Otherwise this tuple has value equality with all elements.
  return true;
}

//===----------------------------------------------------------------------===//
// Tuple Comparable Conformance
//===----------------------------------------------------------------------===//

extern const ProtocolDescriptor
PROTOCOL_DESCRIPTOR_SYM(SWIFT_COMPARABLE_MANGLING);

#if defined(__ELF__)
// Create a GOT equivalent for the Comparable reference.
__asm(
  "  .type got.$sSLMp, @object\n"
  "  .section .data.rel.ro\n"
  "  .p2align 3\n"
  "got.$sSLMp:\n"
  "  .quad ($sSLMp)\n"
  "  .size got.$sSLMp, 8\n"
);
#endif

// Define the conformance descriptor for tuple Comparable. We do this in
// assembly to work around relative reference issues.
__asm(
  #if defined(__ELF__)
  "  .protected " TUPLE_COMPARABLE_CONF "\n"
  "  .type " TUPLE_COMPARABLE_CONF ", @object\n"
  "  .section .rodata\n"
  #elif defined(__MACH__)
  "  .section __TEXT,__const\n"
  #endif
  "  .globl " TUPLE_COMPARABLE_CONF "\n"
  "  .p2align 2\n"
  TUPLE_COMPARABLE_CONF ":\n"
  #if defined(__ELF__)
  // This is an indirectable relative reference to the GOT equivalent for the
  // Equatable protocol descriptor, hence why we add 1 to indicate indirect.
  "  .long (got.$sSLMp - (" TUPLE_COMPARABLE_CONF ")) + 1\n"
  #elif defined(__MACH__)
  "  .long _$sSLMp@GOTPCREL + 5\n"
  #endif
  // 769 is the MetadataKind::Tuple
  "  .long 769\n"
  // This is a direct relative reference to the witness table defined below.
  "  .long ((" TUPLE_COMPARABLE_WT ") - (" TUPLE_COMPARABLE_CONF ")) - 8\n"
  // 32 are the ConformanceFlags with the type reference bit set to MetadataKind.
  "  .long 32\n"
  #if defined(__ELF__)
  "  .size " TUPLE_COMPARABLE_CONF ", 16\n"
  #endif
);

extern const ProtocolConformanceDescriptor _swift_tupleComparable_conf;

// static (extension in Swift):Swift.Comparable.<= infix(A, A) -> Swift.Bool
#define SWIFT_COMPARABLE_LTE_DEFAULT_IMPL $sSLsE2leoiySbx_xtFZ
// static (extension in Swift):Swift.Comparable.>= infix(A, A) -> Swift.Bool
#define SWIFT_COMPARABLE_GTE_DEFAULT_IMPL $sSLsE2geoiySbx_xtFZ
// static (extension in Swift):Swift.Comparable.> infix(A, A) -> Swift.Bool
#define SWIFT_COMPARABLE_GT_DEFAULT_IMPL $sSLsE1goiySbx_xtFZ

// These are all function values that we reinterpret in the witness table.
extern void *SWIFT_COMPARABLE_LTE_DEFAULT_IMPL;
extern void *SWIFT_COMPARABLE_GTE_DEFAULT_IMPL;
extern void *SWIFT_COMPARABLE_GT_DEFAULT_IMPL;

SWIFT_RUNTIME_EXPORT
const _DependentWitnessTable<1, 4> swift::_swift_tupleComparable_wt = {
  &_swift_tupleComparable_conf,
  {
    reinterpret_cast<const WitnessTable *>(&_swift_tupleEquatable_wt)
  },
  {
    reinterpret_cast<void *>(_swift_tupleComparable_lessThan),
    reinterpret_cast<void *>(&SWIFT_COMPARABLE_LTE_DEFAULT_IMPL),
    reinterpret_cast<void *>(&SWIFT_COMPARABLE_GTE_DEFAULT_IMPL),
    reinterpret_cast<void *>(&SWIFT_COMPARABLE_GT_DEFAULT_IMPL)
  }
};

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
bool swift::_swift_tupleComparable_lessThan(OpaqueValue *tuple1,
                                            OpaqueValue *tuple2,
                                            SWIFT_CONTEXT Metadata *swiftSelf,
                                           Metadata *Self, void *witnessTable) {
  auto tuple = cast<TupleTypeMetadata>(Self);

  // Loop through all elements, and compare both tuple elements, if possible.
  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);

    // Ensure we actually have a conformance to Comparable for this element type.
    auto comparable = &PROTOCOL_DESCRIPTOR_SYM(SWIFT_COMPARABLE_MANGLING);
    auto comparableConformance = swift_conformsToProtocol(elt.Type, comparable);

    // If we don't have a conformance then something somewhere messed up in
    // deciding that this tuple type was Comparable...??
    if (!comparableConformance)
      fatalError(0, "tuple element must be Comparable when comparing tuples");

    // Get the respective values from both tuples.
    auto value1 = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple1) + elt.Offset);
    auto value2 = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple2) + elt.Offset);

    auto comparableTable
        = reinterpret_cast<void * const *>(comparableConformance);

    // First, grab the equatable conformance to prepare to check if the elements
    // are equal. (Since this requires Equatable conformane, the witness table
    // is right after the conformance descriptor, which is at index 0.)
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
    auto lessThanWitness
        = comparableTable[1 + WitnessTableFirstRequirementOffset];
    auto lessThan = reinterpret_cast<StaticInfixWitness *>(lessThanWitness);

    // Call the less than function.
    auto isLessThan = lessThan(value1, value2, elt.Type, elt.Type,
                               comparableConformance);

    return isLessThan;
  }

  // Otherwise these tuples are completely equal, thus they are not less than
  // each other.
  return false;
}
