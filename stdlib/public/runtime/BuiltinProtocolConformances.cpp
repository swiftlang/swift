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

//===----------------------------------------------------------------------===//
// Tuple Hashable Conformance
//===----------------------------------------------------------------------===//

extern const ProtocolDescriptor
PROTOCOL_DESCRIPTOR_SYM(SWIFT_HASHABLE_MANGLING);

#if defined(__ELF__)
// Create a GOT equivalent for the Hashable reference.
__asm(
  "  .type got.$sSHMp, @object\n"
  "  .section .data.rel.ro\n"
  "  .p2align 3\n"
  "got.$sSHMp:\n"
  "  .quad ($sSHMp)\n"
  "  .size got.$sSHMp, 8\n"
);
#endif

// Define the conformance descriptor for tuple Hashable. We do this in
// assembly to work around relative reference issues.
__asm(
  #if defined(__ELF__)
  "  .protected " TUPLE_HASHABLE_CONF "\n"
  "  .type " TUPLE_HASHABLE_CONF ", @object\n"
  "  .section .rodata\n"
  #elif defined(__MACH__)
  "  .section __TEXT,__const\n"
  #endif
  "  .globl " TUPLE_HASHABLE_CONF "\n"
  "  .p2align 2\n"
  TUPLE_HASHABLE_CONF ":\n"
  #if defined(__ELF__)
  // This is an indirectable relative reference to the GOT equivalent for the
  // Hashable protocol descriptor, hence why we add 1 to indicate indirect.
  "  .long (got.$sSHMp - (" TUPLE_HASHABLE_CONF ")) + 1\n"
  #elif defined(__MACH__)
  "  .long _$sSHMp@GOTPCREL + 5\n"
  #endif
  // 769 is the MetadataKind::Tuple
  "  .long 769\n"
  // This is a direct relative reference to the witness table defined below.
  "  .long ((" TUPLE_HASHABLE_WT ") - (" TUPLE_HASHABLE_CONF ")) - 8\n"
  // 32 are the ConformanceFlags with the type reference bit set to MetadataKind.
  "  .long 32\n"
  #if defined(__ELF__)
  "  .size " TUPLE_HASHABLE_CONF ", 16\n"
  #endif
);

extern const ProtocolConformanceDescriptor _swift_tupleHashable_conf;

// Swift._hashValue<A where A: Swift.Hashable>(for: A) -> Swift.Int
#define SWIFT_HASHVALUE_FUNC $ss10_hashValue3forSix_tSHRzlF
// (extension in Swift):
// Swift.Hashable._rawHashValue(
//   seed: Swift.Int
// ) -> Swift.Int
#define SWIFT_HASHABLE_RAWHASHVALUE_DEFAULT_IMPL \
        $sSHsE13_rawHashValue4seedS2i_tF
// Swift.Hasher.combine<A where A: Swift.Hashable>(A) -> ()
#define SWIFT_HASHER_COMBINE_FUNC $ss6HasherV7combineyyxSHRzlF

// These are all function values that we reinterpret in the witness table.
extern void *SWIFT_HASHVALUE_FUNC;
extern void *SWIFT_HASHABLE_RAWHASHVALUE_DEFAULT_IMPL;
extern void *SWIFT_HASHER_COMBINE_FUNC;

SWIFT_RUNTIME_EXPORT
const _DependentWitnessTable<1, 3> swift::_swift_tupleHashable_wt = {
  &_swift_tupleHashable_conf,
  {
    reinterpret_cast<const WitnessTable *>(&_swift_tupleEquatable_wt)
  },
  {
    reinterpret_cast<void *>(_swift_tupleHashable_hashValue),
    reinterpret_cast<void *>(_swift_tupleHashable_hash),
    reinterpret_cast<void *>(&SWIFT_HASHABLE_RAWHASHVALUE_DEFAULT_IMPL)
  }
};

using HashValueFn = SWIFT_CC(swift) intptr_t(OpaqueValue *value, Metadata *Self,
                                             void *witnessTable);
using HasherCombineFn = SWIFT_CC(swift) void(OpaqueValue *value,
                                             const Metadata *Self,
                                             const WitnessTable *witnessTable,
                                             SWIFT_CONTEXT OpaqueValue *hasher);

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
intptr_t swift::_swift_tupleHashable_hashValue(SWIFT_CONTEXT OpaqueValue *tuple,
                                               Metadata *Self,
                                               void *witnessTable) {
  auto _hashValue = reinterpret_cast<HashValueFn *>(&SWIFT_HASHVALUE_FUNC);
  return _hashValue(tuple, Self, witnessTable);
}

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
void swift::_swift_tupleHashable_hash(OpaqueValue *hasher,
                                      SWIFT_CONTEXT OpaqueValue *tuple,
                                      Metadata *Self, void *witnessTable) {
  auto tupleTy = cast<TupleTypeMetadata>(Self);

  // Loop through all elements, and hash them into the Hasher.
  for (size_t i = 0; i != tupleTy->NumElements; i += 1) {
    auto elt = tupleTy->getElement(i);

    // Ensure we actually have a conformance to Hashable for this element type.
    auto hashable = &PROTOCOL_DESCRIPTOR_SYM(SWIFT_HASHABLE_MANGLING);
    auto hashableConformance = swift_conformsToProtocol(elt.Type, hashable);

    // If we don't have a conformance then something somewhere messed up in
    // deciding that this tuple type was Hashable...??
    if (!hashableConformance)
      fatalError(0, "tuple element must be Hashable when hashing a tuple.");

    // Get the element value from the tuple.
    auto value = reinterpret_cast<OpaqueValue *>(
                    reinterpret_cast<char *>(tuple) + elt.Offset);

    auto hasherCombine
        = reinterpret_cast<HasherCombineFn *>(&SWIFT_HASHER_COMBINE_FUNC);

    // Call the combine function on the hasher for this element value and we're
    // done!
    hasherCombine(value, elt.Type, hashableConformance, hasher);
  }
}
