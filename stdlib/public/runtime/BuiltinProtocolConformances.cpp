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

// Create a GOT equivalent for the Equatable.== method descriptor.
__asm(
  "  .type got.$sSQ2eeoiySbx_xtFZTq, @object\n"
  "  .p2align 3\n"
  "got.$sSQ2eeoiySbx_xtFZTq:\n"
  "  .quad ($sSQ2eeoiySbx_xtFZTq)\n"
  "  .size got.$sSQ2eeoiySbx_xtFZTq, 8\n"
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
  "  .long (got.$sSQMp - (" TUPLE_EQUATABLE_CONF ")) + 1\n"
  #elif defined(__MACH__)
  "  .long _$sSQMp@GOTPCREL + 5\n"
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
  "  .long ((got.$sSQ2eeoiySbx_xtFZTq - (" TUPLE_EQUATABLE_CONF ")) - 20) + 1\n"
  #elif defined(__MACH__)
  "  .long _$sSQ2eeoiySbx_xtFZTq@GOTPCREL + 5\n"
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
    using Fn = SWIFT_CC(swift) bool(OpaqueValue *, OpaqueValue *,
                                    SWIFT_CONTEXT const Metadata *,
                                    const Metadata *, const WitnessTable *);
    auto equals = reinterpret_cast<Fn *>(equalsWitness);

    // Call the equal function
    auto result = equals(value1, value2, elt.Type, elt.Type, conformance);

    // If the values aren't equal, this tuple isn't equal. :)
    if (!result)
      return false;
  }

  // Otherwise this tuple has value equality with all elements.
  return true;
}
