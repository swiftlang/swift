//===--- KnownProtocolWitnessTables.cpp -----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Definitions of some builtin protocol witness table objects.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/KnownProtocolWitnessTables.h"

using namespace swift;

extern const ProtocolDescriptor
PROTOCOL_DESCRIPTOR_SYM(SWIFT_EQUATABLE_MANGLING);

const _WitnessTable swift::
BUILTIN_PROTOCOL_WITNESS_TABLE_SYM(EMPTY_TUPLE_MANGLING,
                                   SWIFT_EQUATABLE_MANGLING) = {
  &BUILTIN_PROTOCOL_CONFORMANCE_DESCRIPTOR_SYM(EMPTY_TUPLE_MANGLING,
                                               SWIFT_EQUATABLE_MANGLING),
  reinterpret_cast<void *>(BUILTIN_PROTOCOL_WITNESS_SYM(EMPTY_TUPLE_MANGLING,
                                                        SWIFT_EQUATABLE_MANGLING,
                                                  SWIFT_EQUAL_OPERATOR_MANGLING))
};

// This can technically be implemented in the stdlib, but I opted for just
// keeping everything defined together. Maybe when there's more builtin
// conformances it might make sense to migrate to the stdlib.
bool swift::
BUILTIN_PROTOCOL_WITNESS_SYM(EMPTY_TUPLE_MANGLING,
                             SWIFT_EQUATABLE_MANGLING,
                             SWIFT_EQUAL_OPERATOR_MANGLING)
(Metadata *swiftSelf, Metadata *existentialSelf, void *witnessTable) {
  return true;
}

const _ProtocolConformanceDescriptor swift::
BUILTIN_PROTOCOL_CONFORMANCE_DESCRIPTOR_SYM(EMPTY_TUPLE_MANGLING,
                                            SWIFT_EQUATABLE_MANGLING) = {
    (int32_t)(
      (intptr_t)&PROTOCOL_DESCRIPTOR_SYM(SWIFT_EQUATABLE_MANGLING) - 
      (intptr_t)&BUILTIN_PROTOCOL_CONFORMANCE_DESCRIPTOR_SYM(
                    EMPTY_TUPLE_MANGLING, SWIFT_EQUATABLE_MANGLING)),
    (int32_t)(
      (intptr_t)&METADATA_SYM(EMPTY_TUPLE_MANGLING) -
      (intptr_t)&BUILTIN_PROTOCOL_CONFORMANCE_DESCRIPTOR_SYM(
                    EMPTY_TUPLE_MANGLING, SWIFT_EQUATABLE_MANGLING) -
      4 /* Relative to the field above */),
    (int32_t)(
      (intptr_t)&BUILTIN_PROTOCOL_WITNESS_TABLE_SYM(
                    EMPTY_TUPLE_MANGLING, SWIFT_EQUATABLE_MANGLING) -
      (intptr_t)&BUILTIN_PROTOCOL_CONFORMANCE_DESCRIPTOR_SYM(
                    EMPTY_TUPLE_MANGLING, SWIFT_EQUATABLE_MANGLING) -
      8 /* Relative to the other two fields above */),
    (int32_t)ConformanceFlags()
        .withTypeReferenceKind(TypeReferenceKind::DirectTypeMetadata)
        .getIntValue()
};

