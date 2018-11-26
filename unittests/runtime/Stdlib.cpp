//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Metadata.h"
#include "swift/Demangling/ManglingMacros.h"

using namespace swift;

// Placeholders for Swift functions that the C++ runtime references
// but that the test code does not link to.

// AnyHashable

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
void _swift_makeAnyHashableUsingDefaultRepresentation(
  const OpaqueValue *value,
  const void *anyHashableResultPointer,
  const Metadata *T,
  const WitnessTable *hashableWT
) {
  abort();
}

SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss11AnyHashableVMn[1] = {0};

// SwiftHashableSupport

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
bool _swift_stdlib_Hashable_isEqual_indirect(
  const void *lhsValue, const void *rhsValue, const Metadata *type,
  const void *wt) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
intptr_t _swift_stdlib_Hashable_hashValue_indirect(
  const void *value, const Metadata *type, const void *wt) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
void _swift_convertToAnyHashableIndirect(
  OpaqueValue *source, OpaqueValue *destination, const Metadata *sourceType,
  const void *sourceConformance) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
bool _swift_anyHashableDownCastConditionalIndirect(
  OpaqueValue *source, OpaqueValue *destination, const Metadata *targetType) {
  abort();
}

// Casting

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
void _swift_arrayDownCastIndirect(OpaqueValue *destination,
                                  OpaqueValue *source,
                                  const Metadata *sourceValueType,
                                  const Metadata *targetValueType) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
bool _swift_arrayDownCastConditionalIndirect(OpaqueValue *destination,
                                             OpaqueValue *source,
                                             const Metadata *sourceValueType,
                                             const Metadata *targetValueType) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
void _swift_setDownCastIndirect(OpaqueValue *destination,
                                OpaqueValue *source,
                                const Metadata *sourceValueType,
                                const Metadata *targetValueType,
                                const void *sourceValueHashable,
                                const void *targetValueHashable) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
bool _swift_setDownCastConditionalIndirect(OpaqueValue *destination,
                                           OpaqueValue *source,
                                           const Metadata *sourceValueType,
                                           const Metadata *targetValueType,
                                           const void *sourceValueHashable,
                                           const void *targetValueHashable) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
void _swift_dictionaryDownCastIndirect(OpaqueValue *destination,
                                       OpaqueValue *source,
                                       const Metadata *sourceKeyType,
                                       const Metadata *sourceValueType,
                                       const Metadata *targetKeyType,
                                       const Metadata *targetValueType,
                                       const void *sourceKeyHashable,
                                       const void *targetKeyHashable) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
bool _swift_dictionaryDownCastConditionalIndirect(OpaqueValue *destination,
                                        OpaqueValue *source,
                                        const Metadata *sourceKeyType,
                                        const Metadata *sourceValueType,
                                        const Metadata *targetKeyType,
                                        const Metadata *targetValueType,
                                        const void *sourceKeyHashable,
                                        const void *targetKeyHashable) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
void _bridgeNonVerbatimBoxedValue(const OpaqueValue *sourceValue,
                                  OpaqueValue *destValue,
                                  const Metadata *nativeType) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
void _bridgeNonVerbatimFromObjectiveCToAny(HeapObject *sourceValue,
                                           OpaqueValue *destValue) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
bool swift_unboxFromSwiftValueWithType(OpaqueValue *source,
                                       OpaqueValue *result,
                                       const Metadata *destinationType) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
bool swift_swiftValueConformsTo(const Metadata *destinationType) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_API
HeapObject *$Ss27_bridgeAnythingToObjectiveCyyXlxlF(OpaqueValue *src, const Metadata *srcType) {
  abort();
}

// ErrorObject

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
int $Ss13_getErrorCodeySiSPyxGs0B0RzlF(void *) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
void *$Ss23_getErrorDomainNSStringyyXlSPyxGs0B0RzlF(void *) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
void *$Ss29_getErrorUserInfoNSDictionaryyyXlSgSPyxGs0B0RzlF(void *) {
  abort();
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
void *$Ss32_getErrorEmbeddedNSErrorIndirectyyXlSgSPyxGs0B0RzlF(void *) {
  abort();
}

// Hashable

SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $SkMp[1] = {0};

// Array

SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $SSaMn[1] = {0};

// Dictionary

SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $SsSdVMn[1] = {0};

// Set

SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $SsSeVMn[1] = {0};

// Mirror

// protocol witness table for Swift._ClassSuperMirror : Swift._Mirror in Swift
SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss17_ClassSuperMirrorVs01_C0sWP[1] = {0};

// type metadata accessor for Swift._ClassSuperMirror
SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss17_ClassSuperMirrorVMa[1] = {0};

// protocol witness table for Swift._MetatypeMirror : Swift._Mirror in Swift
SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss15_MetatypeMirrorVs01_B0sWP[1] = {0};

// type metadata accessor for Swift._MetatypeMirror
SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss15_MetatypeMirrorVMa[1] = {0};

// protocol witness table for Swift._EnumMirror : Swift._Mirror in Swift
SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss11_EnumMirrorVs01_B0sWP[1] = {0};

// type metadata accessor for Swift._EnumMirror
SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss11_EnumMirrorVMa[1] = {0};

// protocol witness table for Swift._OpaqueMirror : Swift._Mirror in Swift
SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss13_OpaqueMirrorVs01_B0sWP[1] = {0};

// type metadata accessor for Swift._OpaqueMirror
SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss13_OpaqueMirrorVMa[1] = {0};

// protocol witness table for Swift._StructMirror : Swift._Mirror in Swift
SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss13_StructMirrorVs01_B0sWP[1] = {0};

// type metadata accessor for Swift._StructMirror
SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss13_StructMirrorVMa[1] = {0};

// protocol witness table for Swift._TupleMirror : Swift._Mirror in Swift
SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss12_TupleMirrorVs01_B0sWP[1] = {0};

// type metadata accessor for Swift._TupleMirror
SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss12_TupleMirrorVMa[1] = {0};

// protocol witness table for Swift._ClassMirror : Swift._Mirror in Swift
SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss12_ClassMirrorVs01_B0sWP[1] = {0};

// type metadata accessor for Swift._ClassMirror
SWIFT_RUNTIME_STDLIB_INTERNAL
const long long $Ss12_ClassMirrorVMa[1] = {0};

// KeyPath

SWIFT_RUNTIME_STDLIB_SPI
const HeapObject *swift_getKeyPathImpl(const void *p, const void *a) {
  abort();
}
