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
  auto tuple = cast<TupleTypeMetadata>(Self);

  // Loop through all elements, and check if both tuples element is equal.
  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);

    // Ensure we actually have a conform to Equatable for this element type.
    auto equatable = &PROTOCOL_DESCRIPTOR_SYM(SWIFT_EQUATABLE_MANGLING);
    auto conformance = swift_conformsToProtocol(elt.Type, equatable);

    // If we don't have a conformance then something somewhere messed up in
    // deciding that this tuple type was Equatable...??
    if (!conformance)
      fatalError(0,
        "tuple element must be equatable when evaluating tuple equalness");

    // Get the respective values from both tuples.
    auto val1 = reinterpret_cast<OpaqueValue *>(
                  reinterpret_cast<char *>(tuple1) + elt.Offset);
    auto val2 = reinterpret_cast<OpaqueValue *>(
                  reinterpret_cast<char *>(tuple2) + elt.Offset);

    // Grab the specific witness for this element type.
    auto table = reinterpret_cast<void * const *>(conformance);
    auto witness = table[WitnessTableFirstRequirementOffset];
    using Fn = SWIFT_CC(swift) bool(OpaqueValue *, OpaqueValue *,
                                    SWIFT_CONTEXT const Metadata *,
                                    const Metadata *, const WitnessTable *);
    auto func = reinterpret_cast<Fn *>(witness);

    // Call the witness
    auto result = func(val1, val2, elt.Type, elt.Type, conformance);

    // If the values aren't equal, this tuple isn't equal. :)
    if (!result)
      return false;
  }

  // Otherwise this tuple has value equality with all elements.
  return true;
}
