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

// Elf indirect symbol references.
#if defined(__ELF__)
#define INDIRECT_RELREF_GOTPCREL(SYMBOL) SYMBOL "@GOTPCREL + 1"
#endif

// MachO indirect symbol references.
#if defined(__MACH__)

// 64 bit arm MachO
#if defined(__aarch64__)
#define INDIRECT_RELREF_GOTPCREL(SYMBOL) SYMBOL "@GOT - . + 1"

// 32 bit arm MachO
#elif defined(__arm__)
// MachO doesn't support @GOT like relocations for 32 bit arm.
#define INDIRECT_RELREF_GOTPCREL(SYMBOL) "L" SYMBOL "$non_lazy_ptr - . + 1"
#endif

// 64 bit x86_64 MachO
#if defined(__x86_64__)
// The + 4 is required for all x86_64 MachO GOTPC relocations.
#define INDIRECT_RELREF_GOTPCREL(SYMBOL) SYMBOL "@GOTPCREL + 4 + 1"

// 32 bit x86 MachO
#elif defined(__i386__)
// MachO doesn't support @GOT like relocations for 32 bit x86.
#define INDIRECT_RELREF_GOTPCREL(SYMBOL) "L" SYMBOL "$non_lazy_ptr - . + 1"
#endif
#endif

// Windows native indirect symbol references.
#if defined(_WIN32)
#define INDIRECT_RELREF_GOTPCREL(SYMBOL) "__imp_" SYMBOL " - . + 1"
#endif

//===----------------------------------------------------------------------===//
// Tuple Equatable Conformance
//===----------------------------------------------------------------------===//

// For 32 bit ARM and i386 (specifically armv7, armv7s, and armv7k), emit
// non-lazy pointer stubs to indirectly reference. Darwin doesn't support @GOT
// syntax for those archs.
#if defined(__MACH__) && \
  ((defined(__arm__) && !defined(__aarch64__)) || defined(__i386__))
__asm(
#if defined(__arm__)
  "  .section __DATA, __nl_symbol_ptr, non_lazy_symbol_pointers\n"
#elif defined(__i386__)
  "  .section __IMPORT, __pointers, non_lazy_symbol_pointers\n"
#endif
  "  .p2align 2\n"
  "L" EQUATABLE_DESCRIPTOR_SYMBOL "$non_lazy_ptr:\n"
  "  .indirect_symbol " EQUATABLE_DESCRIPTOR_SYMBOL "\n"
  "  .long 0\n"
  "L" EQUATABLE_EE_METHOD_DESCRIPTOR "$non_lazy_ptr:\n"
  "  .indirect_symbol " EQUATABLE_EE_METHOD_DESCRIPTOR "\n"
  "  .long 0\n"
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
  #elif defined(_WIN32)
  "  .lcomm __swift_tupleEquatable_private, 128, 16\n"
  "  .section .rdata, \"dr\"\n"
  #pragma comment(linker, "/EXPORT:_swift_tupleEquatable_conf,DATA")
  #endif
  "  .globl " TUPLE_EQUATABLE_CONF "\n"
  "  .p2align 2\n"
  TUPLE_EQUATABLE_CONF ":\n"
  // This is an indirectable relative reference to the GOT entry for the
  // Equatable protocol descriptor.
  "  .long " INDIRECT_RELREF_GOTPCREL(EQUATABLE_DESCRIPTOR_SYMBOL) "\n"
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
  // This is an indirectable relative reference to the GOT entry for the
  // Equatable == method descriptor.
  "  .long " INDIRECT_RELREF_GOTPCREL(EQUATABLE_EE_METHOD_DESCRIPTOR) "\n"
  // This is a direct relative reference to the equals witness defined below.
  "  .long (" TUPLE_EQUATABLE_EQUALS ") - .\n"
  // The witness table size in words.
  "  .short 0\n"
  // The witness table private size in words & requires instantiation.
  "  .short 1\n"
  // The witness table instantiator function.
  "  .long 0\n"
  // This is a direct relative reference to the private data for the
  // conformance.
  "  .long __swift_tupleEquatable_private - .\n"
  #if defined(__ELF__)
  "  .size " TUPLE_EQUATABLE_CONF ", 40\n"
  #endif
);

extern "C" const ProtocolConformanceDescriptor _swift_tupleEquatable_conf;

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

// For 32 bit ARM and i386 (specifically armv7, armv7s, and armv7k), emit
// non-lazy pointer stubs to indirectly reference. Darwin doesn't support @GOT
// syntax for those archs.
#if defined(__MACH__) && \
  ((defined(__arm__) && !defined(__aarch64__)) || defined(__i386__))
__asm(
#if defined(__arm__)
  "  .section __DATA, __nl_symbol_ptr, non_lazy_symbol_pointers\n"
#elif defined(__i386__)
  "  .section __IMPORT, __pointers, non_lazy_symbol_pointers\n"
#endif
  "  .p2align 2\n"
  "L" COMPARABLE_DESCRIPTOR_SYMBOL "$non_lazy_ptr:\n"
  "  .indirect_symbol " COMPARABLE_DESCRIPTOR_SYMBOL "\n"
  "  .long 0\n"
  "L" COMPARABLE_BASE_CONFORMANCE_DESCRIPTOR "$non_lazy_ptr:\n"
  "  .indirect_symbol " COMPARABLE_BASE_CONFORMANCE_DESCRIPTOR "\n"
  "  .long 0\n"
  "L" COMPARABLE_LT_METHOD_DESCRIPTOR "$non_lazy_ptr:\n"
  "  .indirect_symbol " COMPARABLE_LT_METHOD_DESCRIPTOR "\n"
  "  .long 0\n"
  "L" COMPARBALE_LTE_METHOD_DESCRIPTOR "$non_lazy_ptr:\n"
  "  .indirect_symbol " COMPARBALE_LTE_METHOD_DESCRIPTOR "\n"
  "  .long 0\n"
  "L" COMPARABLE_GTE_METHOD_DESCRIPTOR "$non_lazy_ptr:\n"
  "  .indirect_symbol " COMPARABLE_GTE_METHOD_DESCRIPTOR "\n"
  "  .long 0\n"
  "L" COMPARABLE_GT_METHOD_DESCRIPTOR "$non_lazy_ptr:\n"
  "  .indirect_symbol " COMPARABLE_GT_METHOD_DESCRIPTOR "\n"
  "  .long 0\n"
);
#endif

// Define the associated conformance structure for tuple Comparable. We do this
// in assembly to work around relative reference issues.
__asm(
  #if defined(__ELF__)
  "  .hidden \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\"\n"
  "  .type \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\", @object\n"
  "  .section swift5_typeref, \"a\"\n"
  "  .weak \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\"\n"
  #elif defined(__MACH__)
  "  .private_extern \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\"\n"
  "  .section __TEXT, __swift5_typeref\n"
  "  .globl \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\"\n"
  "  .weak_definition \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\"\n"
  #elif defined(_WIN32)
  "  .section .sw5tyrf$B, \"dr\", discard, \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\"\n"
  "  .globl \"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\"\n"
  #endif
  "  .p2align 1\n"
  "\"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\":\n"
  "  .byte 255\n"
  "  .byte 7\n"
  // This is a direct relative reference to the base accessor for Equatable
  // defined below.
  "  .long (" TUPLE_COMPARABLE_BASEACCESSOREQUATABLE ") - .\n"
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
  #elif defined(_WIN32)
  "  .lcomm __swift_tupleComparable_private, 128, 16\n"
  "  .section .rdata, \"dr\"\n"
  #pragma comment(linker, "/EXPORT:_swift_tupleComparable_conf,DATA")
  #endif
  "  .globl " TUPLE_COMPARABLE_CONF "\n"
  "  .p2align 2\n"
  TUPLE_COMPARABLE_CONF ":\n"
  // This is an indirectable relative reference to the GOT entry for the
  // Comparable protocol descriptor.
  "  .long " INDIRECT_RELREF_GOTPCREL(COMPARABLE_DESCRIPTOR_SYMBOL) "\n"
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
  // This is an indirectable relative reference to the GOT entry for the
  // Comparable base conformance for Equatable.
  "  .long " INDIRECT_RELREF_GOTPCREL(COMPARABLE_BASE_CONFORMANCE_DESCRIPTOR) "\n"
  // This is a direct relative reference to the associated conformance for
  // Equatable defined above in assembly. NOTE: We + 1 here because the
  // associated conformance structure is 1 aligned.
  "  .long (\"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\") - . + 1\n"
  // This is an indirectable relative reference to the GOT entry for the
  // Comparable.< method descriptor.
  "  .long " INDIRECT_RELREF_GOTPCREL(COMPARABLE_LT_METHOD_DESCRIPTOR) "\n"
  // This is a direct relative reference to the less than witness defined below.
  "  .long (" TUPLE_COMPARABLE_LESSTHAN ") - .\n"
  // This is an indirectable relative reference to the GOT entry for the
  // Comparable.<= method descriptor.
  "  .long " INDIRECT_RELREF_GOTPCREL(COMPARBALE_LTE_METHOD_DESCRIPTOR) "\n"
  // This is a direct relative reference to the less than or equal witness
  // defined below.
  "  .long (" TUPLE_COMPARABLE_LESSTHANOREQUAL ") - .\n"
  // This is an indirectable relative reference to the GOT entry for the
  // Comparable.>= method descriptor.
  "  .long " INDIRECT_RELREF_GOTPCREL(COMPARABLE_GTE_METHOD_DESCRIPTOR) "\n"
  // This is a direct relative reference to the greater than or equal witness
  // defined below.
  "  .long (" TUPLE_COMPARABLE_GREATERTHANOREQUAL ") - .\n"
  // This is an indirectable relative reference to the GOT entry for the
  // Comparable.> method descriptor.
  "  .long " INDIRECT_RELREF_GOTPCREL(COMPARABLE_GT_METHOD_DESCRIPTOR) "\n"
  // This is a direct relative reference to the greater than witness defined
  // below.
  "  .long (" TUPLE_COMPARABLE_GREATERTHAN ") - .\n"
  // The witness table size in words.
  "  .short 0\n"
  // The witness table private size in words & requires instantiation.
  "  .short 1\n"
  // The witness table instantiator function.
  "  .long 0\n"
  // This is a direct relative reference to the private data for the
  // conformance.
  "  .long __swift_tupleComparable_private - .\n"
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

//===----------------------------------------------------------------------===//
// Tuple Hashable Conformance
//===----------------------------------------------------------------------===//

// For 32 bit ARM and i386 (specifically armv7, armv7s, and armv7k), emit
// non-lazy pointer stubs to indirectly reference. Darwin doesn't support @GOT
// syntax for those archs.
#if defined(__MACH__) && \
  ((defined(__arm__) && !defined(__aarch64__)) || defined(__i386__))
__asm(
#if defined(__arm__)
  "  .section __DATA, __nl_symbol_ptr, non_lazy_symbol_pointers\n"
#elif defined(__i386__)
  "  .section __IMPORT, __pointers, non_lazy_symbol_pointers\n"
#endif
  "  .p2align 2\n"
  "L" HASHABLE_DESCRIPTOR_SYMBOL "$non_lazy_ptr:\n"
  "  .indirect_symbol " HASHABLE_DESCRIPTOR_SYMBOL "\n"
  "  .long 0\n"
  "L" HASHABLE_BASE_CONFORMANCE_DESCRIPTOR "$non_lazy_ptr:\n"
  "  .indirect_symbol " HASHABLE_BASE_CONFORMANCE_DESCRIPTOR "\n"
  "  .long 0\n"
  "L" HASHABLE_HASHVALUE_METHOD_DESCRIPTOR "$non_lazy_ptr:\n"
  "  .indirect_symbol " HASHABLE_HASHVALUE_METHOD_DESCRIPTOR "\n"
  "  .long 0\n"
  "L" HASHABLE_HASH_METHOD_DESCRIPTOR "$non_lazy_ptr:\n"
  "  .indirect_symbol " HASHABLE_HASH_METHOD_DESCRIPTOR "\n"
  "  .long 0\n"
  "L" HASHABLE_RAWHASHVALUE_METHOD_DESCRIPTOR "$non_lazy_ptr:\n"
  "  .indirect_symbol " HASHABLE_RAWHASHVALUE_METHOD_DESCRIPTOR "\n"
  "  .long 0\n"
);
#endif

// Define the conformance descriptor for tuple Hashable. We do this in
// assembly to work around relative reference issues.
__asm(
  #if defined(__ELF__)
  "  .type __swift_tupleHashable_private, @object\n"
  "  .local __swift_tupleHashable_private\n"
  "  .comm __swift_tupleHashable_private, 128, 16\n"
  "  .protected " TUPLE_HASHABLE_CONF "\n"
  "  .type " TUPLE_HASHABLE_CONF ", @object\n"
  "  .section .rodata\n"
  #elif defined(__MACH__)
  "  .zerofill __DATA, __bss, __swift_tupleHashable_private, 128, 4\n"
  "  .section __TEXT, __const\n"
  #elif defined(_WIN32)
  "  .lcomm __swift_tupleHashable_private, 128, 16\n"
  "  .section .rdata, \"dr\"\n"
  #pragma comment(linker, "/EXPORT:_swift_tupleHashable_conf,DATA")
  #endif
  "  .globl " TUPLE_HASHABLE_CONF "\n"
  "  .p2align 2\n"
  TUPLE_HASHABLE_CONF ":\n"
  // This is an indirectable relative reference to the GOT entry for the
  // Hashable protocol descriptor.
  "  .long " INDIRECT_RELREF_GOTPCREL(HASHABLE_DESCRIPTOR_SYMBOL) "\n"
  // 769 is the MetadataKind::Tuple
  "  .long 769\n"
  // This indicates that we have no witness table pattern. We use a generic
  // witness table for builtin conformances.
  "  .long 0\n"
  // 196640 are the ConformanceFlags with the type reference bit set to
  // MetadataKind, the has resilient witness bit, and the generic witness table
  // bit.
  "  .long 196640\n"
  // This 4 is the ResilientWitnessesHeader indicating we have 4 resilient
  // witnesses.
  "  .long 4\n"
  // This is an indirectable relative reference to the GOT entry for the
  // Hashable base conformance for Equatable.
  "  .long " INDIRECT_RELREF_GOTPCREL(HASHABLE_BASE_CONFORMANCE_DESCRIPTOR) "\n"
  // This is a direct relative reference to the associated conformance for
  // Equatable defined above in assembly. NOTE: We intentionally use the
  // Comparable implementation for this because the implementation is the same
  // for both Hashable and Comparable. Both want to grab the Equatable table
  // from its elements whose witness table is located in the same place for both
  // protocols. NOTE: We + 1 here because the associated conformance
  // structure is 1 aligned.
  "  .long (\"" TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE "\") - . + 1\n"
  // This is an indirectable relative reference to the GOT entry for the
  // Hashable.hashValue method descriptor.
  "  .long " INDIRECT_RELREF_GOTPCREL(HASHABLE_HASHVALUE_METHOD_DESCRIPTOR) "\n"
  // This is a direct relative reference to the hashValue witness defined below.
  "  .long (" TUPLE_HASHABLE_HASHVALUE ") - .\n"
  // This is an indirectable relative reference to the GOT entry for the
  // Hashable.hash(into:) method descriptor.
  "  .long " INDIRECT_RELREF_GOTPCREL(HASHABLE_HASH_METHOD_DESCRIPTOR) "\n"
  // This is a direct relative reference to the hash(into:) witness defined below.
  "  .long (" TUPLE_HASHABLE_HASH ") - .\n"
  // This is an indirectable relative reference to the GOT equivalent for the
  // Hashable._rawHashValue method descriptor.
  "  .long " INDIRECT_RELREF_GOTPCREL(HASHABLE_RAWHASHVALUE_METHOD_DESCRIPTOR) "\n"
  // This 0 indicates that we are requesting the default implementation for the
  // _rawHashValue getter.
  "  .long 0\n"
  // The witness table size in words.
  "  .short 0\n"
  // The witness table private size in words & requires instantiation.
  "  .short 1\n"
  // The witness table instantiator function.
  "  .long 0\n"
  // This is a direct relative reference to the private data for the
  // conformance.
  "  .long __swift_tupleHashable_private - .\n"
  #if defined(__ELF__)
  "  .size " TUPLE_HASHABLE_CONF ", 64\n"
  #endif
);

extern "C" SWIFT_CC(swift)
intptr_t SWIFT_HASHVALUE_FUNC(OpaqueValue *value, Metadata *Self,
                              void *witnessTable);

extern "C" SWIFT_CC(swift)
void SWIFT_HASHER_COMBINE_FUNC(OpaqueValue *value, const Metadata *Self,
                               const WitnessTable *witnessTable,
                               SWIFT_CONTEXT OpaqueValue *hasher);

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
intptr_t _swift_tupleHashable_hashValue(SWIFT_CONTEXT OpaqueValue *tuple,
                                        Metadata *Self, void *witnessTable) {
  return SWIFT_HASHVALUE_FUNC(tuple, Self, witnessTable);
}

SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
void _swift_tupleHashable_hash(OpaqueValue *hasher,
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
