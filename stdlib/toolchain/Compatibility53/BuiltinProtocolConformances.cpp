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

#include "Overrides.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/BuiltinProtocolConformances.h"
#include "swift/Runtime/Casting.h"
#include <dlfcn.h>
#include <mach-o/dyld.h>

using namespace swift;

static const ProtocolDescriptor *getEquatableDescriptor() {
  auto descriptor = SWIFT_LAZY_CONSTANT(
    reinterpret_cast<const ProtocolDescriptor *>(
                     dlsym(RTLD_DEFAULT, "$sSQMp")));
  return descriptor;
}

static const WitnessTable *conformsToProtocol(const Metadata *type,
                                        const ProtocolDescriptor *protocol) {
  using Fn = const WitnessTable *(const Metadata *, const ProtocolDescriptor *);
  auto func = SWIFT_LAZY_CONSTANT(
    reinterpret_cast<Fn *>(
      dlsym(RTLD_DEFAULT, "swift_conformsToProtocol")));
  return func(type, protocol);
}

#define TUPLE_EQUATABLE_WT SYMBOL("_swift_tupleEquatable_wt")

// Define the conformance descriptor for tuple Equatable. We do this in
// assembly to work around relative reference issues.
__asm(
  "  .section __DATA,__data\n"
  "  .globl " TUPLE_EQUATABLE_CONF "\n"
  "  .p2align 2\n"
  TUPLE_EQUATABLE_CONF ":\n"
  // This is an indirectable relative reference to the Equatable protocol
  // descriptor. However, this is 0 here because the compatibility libraries
  // can't have a dependency on libswiftCore (which is where Equatable lives).
  "  .long 0\n"
  // 769 is the MetadataKind::Tuple
  "  .long 769\n"
  // This is a direct relative reference to the witness table defined below.
  "  .long ((" TUPLE_EQUATABLE_WT ") - (" TUPLE_EQUATABLE_CONF ")) - 8\n"
  // 32 are the ConformanceFlags with the type reference bit set to MetadataKind.
  "  .long 32\n"
);

extern const ProtocolConformanceDescriptor _swift_tupleEquatable_conf;

// Due to the fact that the compatibility libraries can't have a hard
// dependency to libswiftCore (which is where the Equatable protocol desciptor
// lives), we have to manually implant this before calling any user code.
__attribute__((constructor))
void _emplaceEquatableDescriptor() {
  auto tupleEquatableConf = const_cast<int32_t *>(
    reinterpret_cast<const int32_t *>(&_swift_tupleEquatable_conf));
  auto equatable = getEquatableDescriptor();

  // This is an indirectable pointer.
  *tupleEquatableConf = intptr_t(equatable) - intptr_t(tupleEquatableConf);
}

template<unsigned int NumWitnesses>
struct _WitnessTable {
  const ProtocolConformanceDescriptor *Conformance;
  const void *Witnesses[NumWitnesses];
};

SWIFT_RUNTIME_EXPORT
const _WitnessTable<1> _swift_tupleEquatable_wt = {
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
    auto equatable = getEquatableDescriptor();
    auto conformance = conformsToProtocol(elt.Type, equatable);

    // If we don't have a conformance then something somewhere messed up in
    // deciding that this tuple type was Equatable...??
    if (!conformance)
      swift_unreachable("Tuple equality requires that all elements be \
        Equatable.");

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

    // Call the equal function.
    auto result = equals(value1, value2, elt.Type, elt.Type, conformance);

    // If the values aren't equal, this tuple isn't equal. :)
    if (!result)
      return false;
  }

  // Otherwise this tuple has value equality with all elements.
  return true;
}
