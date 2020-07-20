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

static const ProtocolDescriptor *getComparableDescriptor() {
  auto descriptor = SWIFT_LAZY_CONSTANT(
    reinterpret_cast<const ProtocolDescriptor *>(
                     dlsym(RTLD_DEFAULT, "$sSLMp")));
  return descriptor;
}

static const ProtocolDescriptor *getHashableDescriptor() {
  auto descriptor = SWIFT_LAZY_CONSTANT(
    reinterpret_cast<const ProtocolDescriptor *>(
                     dlsym(RTLD_DEFAULT, "$sSHMp")));
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

template<unsigned int NumWitnesses>
struct _WitnessTable {
  const ProtocolConformanceDescriptor *Conformance;
  const void *Witnesses[NumWitnesses];
};

using StaticInfixWitness = SWIFT_CC(swift) bool(OpaqueValue *, OpaqueValue *,
                                                SWIFT_CONTEXT const Metadata *,
                                                const Metadata *,
                                                const WitnessTable *);

//===----------------------------------------------------------------------===//
// Tuple Equatable Conformance
//===----------------------------------------------------------------------===//

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
void _emplaceTupleEquatableDescriptor() {
  auto tupleEquatableConf = const_cast<int32_t *>(
    reinterpret_cast<const int32_t *>(&_swift_tupleEquatable_conf));
  auto equatable = getEquatableDescriptor();

  // This is an indirectable pointer.
  *tupleEquatableConf = intptr_t(equatable) - intptr_t(tupleEquatableConf);
}

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
    auto equals = reinterpret_cast<StaticInfixWitness *>(equalsWitness);

    // Call the equal function.
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

#define TUPLE_COMPARABLE_WT SYMBOL("_swift_tupleComparable_wt")

// Define the conformance descriptor for tuple Comparable. We do this in
// assembly to work around relative reference issues.
__asm(
  "  .section __DATA,__data\n"
  "  .globl " TUPLE_COMPARABLE_CONF "\n"
  "  .p2align 2\n"
  TUPLE_COMPARABLE_CONF ":\n"
  // This is an indirectable relative reference to the Comparable protocol
  // descriptor. However, this is 0 here because the compatibility libraries
  // can't have a dependency on libswiftCore (which is where Comparable lives).
  "  .long 0\n"
  // 769 is the MetadataKind::Tuple
  "  .long 769\n"
  // This is a direct relative reference to the witness table defined below.
  "  .long ((" TUPLE_COMPARABLE_WT ") - (" TUPLE_COMPARABLE_CONF ")) - 8\n"
  // 32 are the ConformanceFlags with the type reference bit set to MetadataKind.
  "  .long 32\n"
);

extern const ProtocolConformanceDescriptor _swift_tupleComparable_conf;

// Due to the fact that the compatibility libraries can't have a hard
// dependency to libswiftCore (which is where the Comparable protocol desciptor
// lives), we have to manually implant this before calling any user code.
__attribute__((constructor))
void _emplaceTupleComparableDescriptor() {
  auto tupleComparableConf = const_cast<int32_t *>(
    reinterpret_cast<const int32_t *>(&_swift_tupleComparable_conf));
  auto comparable = getComparableDescriptor();

  // This is an indirectable pointer.
  *tupleComparableConf = intptr_t(comparable) - intptr_t(tupleComparableConf);
}

// The base Equatable protocol is itself a requirement, thus the requirement
// count is 5 (Equatable + 4 operators) and the witness is the tuple Equatable
// table.
SWIFT_RUNTIME_EXPORT
const _WitnessTable<5> _swift_tupleComparable_wt = {
  &_swift_tupleComparable_conf,
  {
    reinterpret_cast<const void *>(&_swift_tupleEquatable_wt),
    reinterpret_cast<void *>(_swift_tupleComparable_lessThan),
    reinterpret_cast<void *>(_swift_tupleComparable_lessThanOrEqual),
    reinterpret_cast<void *>(_swift_tupleComparable_greaterThanOrEqual),
    reinterpret_cast<void *>(_swift_tupleComparable_greaterThan)
  }
};

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
bool swift::_swift_tupleComparable_lessThan(OpaqueValue *tuple1,
                                            OpaqueValue *tuple2,
                                            SWIFT_CONTEXT Metadata *swiftSelf,
                                            Metadata *Self, void *witnessTable) {
  auto tuple = cast<TupleTypeMetadata>(Self);

  // Loop through all elements, and check if both tuples element is equal.
  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);

    // Ensure we actually have a conformance to Comparable for this element type.
    auto comparable = getComparableDescriptor();
    auto conformance = conformsToProtocol(elt.Type, comparable);

    // If we don't have a conformance then something somewhere messed up in
    // deciding that this tuple type was Comparable...??
    if (!conformance)
      swift_unreachable("Tuple comparability requires that all elements \
        be Comparable.");

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
    auto lessThanWitness = comparableTable[1 + WitnessTableFirstRequirementOffset];
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

  // Loop through all elements, and check if both tuples element is equal.
  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);

    // Ensure we actually have a conformance to Comparable for this element type.
    auto comparable = getComparableDescriptor();
    auto conformance = conformsToProtocol(elt.Type, comparable);

    // If we don't have a conformance then something somewhere messed up in
    // deciding that this tuple type was Comparable...??
    if (!conformance)
      swift_unreachable("Tuple comparability requires that all elements \
        be Comparable.");

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

  // Loop through all elements, and check if both tuples element is equal.
  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);

    // Ensure we actually have a conformance to Comparable for this element type.
    auto comparable = getComparableDescriptor();
    auto conformance = conformsToProtocol(elt.Type, comparable);

    // If we don't have a conformance then something somewhere messed up in
    // deciding that this tuple type was Comparable...??
    if (!conformance)
      swift_unreachable("Tuple comparability requires that all elements \
        be Comparable.");

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

  // Loop through all elements, and check if both tuples element is equal.
  for (size_t i = 0; i != tuple->NumElements; i += 1) {
    auto elt = tuple->getElement(i);

    // Ensure we actually have a conformance to Comparable for this element type.
    auto comparable = getComparableDescriptor();
    auto conformance = conformsToProtocol(elt.Type, comparable);

    // If we don't have a conformance then something somewhere messed up in
    // deciding that this tuple type was Comparable...??
    if (!conformance)
      swift_unreachable("Tuple comparability requires that all elements \
        be Comparable.");

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

#define TUPLE_HASHABLE_WT SYMBOL("_swift_tupleHashable_wt")

// Define the conformance descriptor for tuple Hashable. We do this in
// assembly to work around relative reference issues.
__asm(
  "  .section __DATA,__data\n"
  "  .globl " TUPLE_HASHABLE_CONF "\n"
  "  .p2align 2\n"
  TUPLE_HASHABLE_CONF ":\n"
  // This is an indirectable relative reference to the Hashable protocol
  // descriptor. However, this is 0 here because the compatibility libraries
  // can't have a dependency on libswiftCore (which is where Hashable lives).
  "  .long 0\n"
  // 769 is the MetadataKind::Tuple
  "  .long 769\n"
  // This is a direct relative reference to the witness table defined below.
  "  .long ((" TUPLE_HASHABLE_WT ") - (" TUPLE_HASHABLE_CONF ")) - 8\n"
  // 32 are the ConformanceFlags with the type reference bit set to MetadataKind.
  "  .long 32\n"
);

extern const ProtocolConformanceDescriptor _swift_tupleHashable_conf;

// Due to the fact that the compatibility libraries can't have a hard
// dependency to libswiftCore (which is where the Hashable protocol desciptor
// lives), we have to manually implant this before calling any user code.
__attribute__((constructor))
void _emplaceTupleHashableDescriptor() {
  auto tupleHashableConf = const_cast<int32_t *>(
    reinterpret_cast<const int32_t *>(&_swift_tupleHashable_conf));
  auto hashable = getHashableDescriptor();

  // This is an indirectable pointer.
  *tupleHashableConf = intptr_t(hashable) - intptr_t(tupleHashableConf);
}

// The base Equatable protocol is itself a requirement, thus the requirement
// count is 4 (Equatable + hashValue + hash(into:) + _rawHashValue) and the
// witness is the tuple Equatable table.
SWIFT_RUNTIME_EXPORT
_WitnessTable<4> _swift_tupleHashable_wt = {
  &_swift_tupleHashable_conf,
  {
    reinterpret_cast<const void *>(&_swift_tupleEquatable_wt),
    reinterpret_cast<void *>(_swift_tupleHashable_hashValue),
    reinterpret_cast<void *>(_swift_tupleHashable_hash),
    nullptr
  }
};

static void *get_rawHashValueDefaultImplFunc() {
  auto impl = SWIFT_LAZY_CONSTANT(
    dlsym(RTLD_DEFAULT, "$sSHsE13_rawHashValue4seedS2i_tF"));
  return impl;
}

// Due to the fact that the compatibility libraries can't have a hard
// dependency to libswiftCore (which is where the _rawHashValue default impl
// lives), we have to manually implant this before calling any user code.
__attribute__((constructor))
void _emplaceTupleHashable_rawHashValueDefaultImpl() {
  _swift_tupleHashable_wt.Witnesses[3] = get_rawHashValueDefaultImplFunc();
}

using HashValueFn = SWIFT_CC(swift) intptr_t(OpaqueValue *value, Metadata *Self,
                                             void *witnessTable);
using HasherCombineFn = SWIFT_CC(swift) void(OpaqueValue *value,
                                             const Metadata *Self,
                                             const WitnessTable *witnessTable,
                                             SWIFT_CONTEXT OpaqueValue *hasher);

static HashValueFn *get_hashValueFunc() {
  auto descriptor = SWIFT_LAZY_CONSTANT(
    reinterpret_cast<HashValueFn *>(
                     dlsym(RTLD_DEFAULT, STR(SWIFT_HASHVALUE_FUNC))));
  return descriptor;
}

static HasherCombineFn *getHashCombineFunc() {
  auto descriptor = SWIFT_LAZY_CONSTANT(
    reinterpret_cast<HasherCombineFn *>(
                     dlsym(RTLD_DEFAULT, STR(SWIFT_HASHER_COMBINE_FUNC))));
  return descriptor;
}

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
intptr_t swift::_swift_tupleHashable_hashValue(SWIFT_CONTEXT OpaqueValue *tuple,
                                          Metadata *Self, void *witnessTable) {
  auto _hashValue = get_hashValueFunc();
  return _hashValue(tuple, Self, witnessTable);
}

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
void swift::_swift_tupleHashable_hash(OpaqueValue *hasher,
                                      SWIFT_CONTEXT OpaqueValue *tuple,
                                      Metadata *Self, void *witnessTable) {
  auto tupleTy = cast<TupleTypeMetadata>(Self);

  // Loop through all elements and hash them into the Hasher.
  for (size_t i = 0; i != tupleTy->NumElements; i += 1) {
    auto elt = tupleTy->getElement(i);

    // Ensure we actually have a conformance to Hashable for this element type.
    auto hashable = getHashableDescriptor();
    auto conformance = conformsToProtocol(elt.Type, hashable);

    // If we don't have a conformance then something somewhere messed up in
    // deciding that this tuple type was Hashable...??
    if (!conformance)
      swift_unreachable("Tuple hasing requires that all elements be Hashable.");

    // Get the element value from the tuple.
    auto value = reinterpret_cast<OpaqueValue *>(
                  reinterpret_cast<char *>(tuple) + elt.Offset);

    auto hasherCombine = getHashCombineFunc();

    // Call the combine function on the hasher for this element value and we're
    // done!
    hasherCombine(value, elt.Type, conformance, hasher);
  }
}
