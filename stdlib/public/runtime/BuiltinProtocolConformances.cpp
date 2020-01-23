//===--- BuiltinProtocolWitnessTables.cpp ---------------------------------===//
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

template <typename Fn>
static Optional<bool> enumerateTuple(OpaqueValue *tuple1, OpaqueValue *tuple2,
                                     Metadata *type,
                                     const ProtocolDescriptor *protocol,
                                     Fn func) {
  auto tuple = cast<TupleTypeMetadata>(type);

  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);

    // Ensure we actually have a conformance to said protocol for this element
    // type.
    auto conformance = swift_conformsToProtocol(elt.Type, protocol);

    // If we don't have a conformance then something somewhere messed up in
    // deciding that this tuple type conformed to this protocol...??
    if (!conformance)
      fatalError(0,
        "getting tuple element conformance to protocol it does not conform to\n");

    // Get the respective values from both tuples.
    auto val1 = reinterpret_cast<OpaqueValue *>(
                  reinterpret_cast<char *>(tuple1) + elt.Offset);
    auto val2 = reinterpret_cast<OpaqueValue *>(
                  reinterpret_cast<char *>(tuple2) + elt.Offset);

    auto table = reinterpret_cast<void * const *>(conformance);

    auto result = func(val1, val2, elt.Type, table);

    // If we've returned a value here, return that.
    if (result)
      return *result;
  }

  // Otherwise, let the handler figure out what to do next.
  return None;
}

extern const ProtocolDescriptor
PROTOCOL_DESCRIPTOR_SYM(SWIFT_EQUATABLE_MANGLING);

extern const ProtocolConformanceDescriptor
BUILTIN_PROTOCOL_CONFORMANCE_DESCRIPTOR_SYM(VARIADIC_TUPLE_MANGLING,
                                            SWIFT_EQUATABLE_MANGLING);

SWIFT_RUNTIME_EXPORT
const _WitnessTable<1> swift::
BUILTIN_PROTOCOL_WITNESS_TABLE_SYM(VARIADIC_TUPLE_MANGLING,
                                   SWIFT_EQUATABLE_MANGLING) = {
  &BUILTIN_PROTOCOL_CONFORMANCE_DESCRIPTOR_SYM(VARIADIC_TUPLE_MANGLING,
                                               SWIFT_EQUATABLE_MANGLING),
  {
    reinterpret_cast<void *>(
      BUILTIN_PROTOCOL_WITNESS_SYM(VARIADIC_TUPLE_MANGLING,
                                   SWIFT_EQUATABLE_MANGLING,
                                   SWIFT_EQUAL_OPERATOR_MANGLING))
  }
};

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
bool swift::
BUILTIN_PROTOCOL_WITNESS_SYM(VARIADIC_TUPLE_MANGLING,
                             SWIFT_EQUATABLE_MANGLING,
                             SWIFT_EQUAL_OPERATOR_MANGLING)
(OpaqueValue *tuple1, OpaqueValue *tuple2, SWIFT_CONTEXT Metadata *swiftSelf,
 Metadata *Self, void *witnessTable) {
  auto equatable = &PROTOCOL_DESCRIPTOR_SYM(SWIFT_EQUATABLE_MANGLING);

  auto func = [](OpaqueValue *val1, OpaqueValue *val2, const Metadata *eltType,
                 void * const *table) -> Optional<bool> {
    using Fn = SWIFT_CC(swift) bool(OpaqueValue *, OpaqueValue *,
                                    SWIFT_CONTEXT const Metadata *,
                                    const Metadata *, const WitnessTable *);

    // Grab the equals witness and call it.
    auto equalsWitness = table[WitnessTableFirstRequirementOffset];
    auto equalsFunc = reinterpret_cast<Fn *>(equalsWitness);
    auto equals = equalsFunc(val1, val2, eltType, eltType,
                             reinterpret_cast<const WitnessTable *>(table));

    // If the values aren't equal, this tuple isn't equal. :)
    if (!equals)
      return false;

    // Otherwise, check the next element.
    return None;
  };

  auto result = enumerateTuple(tuple1, tuple2, Self, equatable, func);

  if (result)
    return *result;

  // Otherwise this tuple has value equality with all elements.
  return true;
}

extern const ProtocolDescriptor
PROTOCOL_DESCRIPTOR_SYM(SWIFT_COMPARABLE_MANGLING);

extern const ProtocolConformanceDescriptor
BUILTIN_PROTOCOL_CONFORMANCE_DESCRIPTOR_SYM(VARIADIC_TUPLE_MANGLING,
                                            SWIFT_COMPARABLE_MANGLING);

#define SWIFT_COMPARABLE_LTE_DEFAULT_IMPL $sSLsE2leoiySbx_xtFZ
#define SWIFT_COMPARABLE_GTE_DEFAULT_IMPL $sSLsE2geoiySbx_xtFZ
#define SWIFT_COMPARABLE_GT_DEFAULT_IMPL $sSLsE1goiySbx_xtFZ

// These are all function values, that we reinterpret in the witness table.
extern void *SWIFT_COMPARABLE_LTE_DEFAULT_IMPL;
extern void *SWIFT_COMPARABLE_GTE_DEFAULT_IMPL;
extern void *SWIFT_COMPARABLE_GT_DEFAULT_IMPL;

SWIFT_RUNTIME_EXPORT
const _DependentWitnessTable<1, 4> swift::
BUILTIN_PROTOCOL_WITNESS_TABLE_SYM(VARIADIC_TUPLE_MANGLING,
                                   SWIFT_COMPARABLE_MANGLING) = {
  &BUILTIN_PROTOCOL_CONFORMANCE_DESCRIPTOR_SYM(VARIADIC_TUPLE_MANGLING,
                                               SWIFT_EQUATABLE_MANGLING),
  {
    reinterpret_cast<const WitnessTable *>(
      &BUILTIN_PROTOCOL_WITNESS_TABLE_SYM(VARIADIC_TUPLE_MANGLING,
                                          SWIFT_EQUATABLE_MANGLING))
  },
  {
    reinterpret_cast<void *>(
      BUILTIN_PROTOCOL_WITNESS_SYM(VARIADIC_TUPLE_MANGLING,
                                   SWIFT_COMPARABLE_MANGLING,
                                   SWIFT_LT_OPERATOR_MANGLING)),
    reinterpret_cast<void *>(&SWIFT_COMPARABLE_LTE_DEFAULT_IMPL),
    reinterpret_cast<void *>(&SWIFT_COMPARABLE_GTE_DEFAULT_IMPL),
    reinterpret_cast<void *>(&SWIFT_COMPARABLE_GT_DEFAULT_IMPL)
  }
};

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
bool swift::
BUILTIN_PROTOCOL_WITNESS_SYM(VARIADIC_TUPLE_MANGLING,
                             SWIFT_COMPARABLE_MANGLING,
                             SWIFT_LT_OPERATOR_MANGLING)
(OpaqueValue *tuple1, OpaqueValue *tuple2, SWIFT_CONTEXT Metadata *swiftSelf,
 Metadata *Self, void *witnessTable) {
  auto comparable = &PROTOCOL_DESCRIPTOR_SYM(SWIFT_COMPARABLE_MANGLING);

  auto func = [](OpaqueValue *val1, OpaqueValue *val2, const Metadata *eltType,
                 void * const *table) -> Optional<bool> {
    using Fn = SWIFT_CC(swift) bool(OpaqueValue *, OpaqueValue *,
                                    SWIFT_CONTEXT const Metadata *,
                                    const Metadata *, const WitnessTable *);

    // First, grab the equatable conformance to prepare to check if the elements
    // are equal. (Since this requires Equatable conformance, the witness table
    // is right after the conformance descriptor, which is at index 0.)
    auto equatableTable = reinterpret_cast<void * const *>(table[1]);
    auto equalsWitness = equatableTable[WitnessTableFirstRequirementOffset];
    auto equalsFunc = reinterpret_cast<Fn *>(equalsWitness);
    auto equals = equalsFunc(val1, val2, eltType, eltType,
                             reinterpret_cast<const WitnessTable *>(
                              equatableTable));

    // If these are equal, skip (or in this case, return None to indicate a skip)
    if (equals)
      return None;

    // Now that we know they are not equal, we can call their comparable and
    // return the result.
    auto lessThanWitness = table[1 + WitnessTableFirstRequirementOffset];
    auto lessThanFunc = reinterpret_cast<Fn *>(lessThanWitness);
    return lessThanFunc(val1, val2, eltType, eltType,
                        reinterpret_cast<const WitnessTable *>(table));
  };

  auto result = enumerateTuple(tuple1, tuple2, Self, comparable, func);

  if (result)
    return *result;

  // Otherwise all the elements are equal, thus this tuple is not less than the
  // other.
  return false;
}
