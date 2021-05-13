//===--- BuiltinProtocolConformances.h --------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift runtime support for builtin protocol witnesses and related items.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_BUILTINPROTOCOLCONFORMANCES_H
#define SWIFT_RUNTIME_BUILTINPROTOCOLCONFORMANCES_H

#include "swift/ABI/Metadata.h"
#include "swift/Basic/RelativePointer.h"
#include <cstdint>

namespace swift {

#define STR(a) #a
#define XSTR(a) STR(a)
#define SYMBOL(name) XSTR(__USER_LABEL_PREFIX__) name

template<typename T>
struct _RelativeIndirectablePointer {
  int32_t offset;

  void set(const T *ptr, bool isIndirect = false) {
    offset = intptr_t(ptr) - intptr_t(this);

    if (isIndirect) {
      offset += 1;
    }
  }
};

template<typename T>
struct _RelativeDirectPointer {
  int32_t offset;

  void set(const T *ptr) {
    offset = intptr_t(ptr) - intptr_t(this);
  }
};

struct _ResilientWitness {
  _RelativeIndirectablePointer<MethodDescriptor> methodDescriptor;
  _RelativeDirectPointer<void> impl;
};

template<int numResilientWitnesses>
struct _ConformanceDescriptor {
  // Base conformance descriptor fields.
  _RelativeIndirectablePointer<ProtocolDescriptor> protocol;
  int32_t typeRef;
  int32_t witnessTablePattern;
  int32_t flags;

  // Resilient Witness fields.
  int32_t resilientWitnessHeader;
  _ResilientWitness resilientWitnesses[numResilientWitnesses];

  // Generic witness table fields.
  int16_t witnessTableSize;
  int16_t privateSizeAndRequiresInstantiation;
  int32_t witnessTableInstantiator;
  _RelativeDirectPointer<char> privateData;
};

template<>
struct _ConformanceDescriptor<0> {
  // Base conformance descriptor fields.
  int32_t protocol;
  int32_t typeRef;
  int32_t witnessTablePattern;
  int32_t flags;
};

#if defined(__ELF__)
#define SWIFT_ASSOCIATED_CONFORMANCE \
          __attribute__((__visibility__("hidden"))) \
          __attribute__((__weak__)) \
          __attribute__((__aligned__(2)))

#elif defined(__MACH__)
#define SWIFT_ASSOCIATED_CONFORMANCE \
          __attribute__((__visibility__("hidden"))) \
          __attribute__((__weak__)) \
          __attribute__((__aligned__(2)))

#elif defined(_WIN32)
#define SWIFT_ASSOCIATED_CONFORMANCE \
          __declspec(align(2))
#endif

#if defined(_WIN32)
#pragma pack(push, 1)
#endif
struct _AssociatedConformance {
  uint8_t byte0;
  uint8_t byte1;
  _RelativeDirectPointer<void> baseAccessor;
  uint8_t null;
}
#if defined(__ELF__) || defined(__MACH__)
__attribute__((packed))
#endif
; // Ending struct semicolon
#if defined(_WIN32)
#pragma pack(pop)
#endif

static_assert(sizeof(_AssociatedConformance) == 7,
              "_AssociatedConformance should be packed!");

//===----------------------------------------------------------------------===//
// Tuple Equatable Conformance
//===----------------------------------------------------------------------===//

#define EQUATABLE_DESCRIPTOR $sSQMp
#define EQUATABLE_EE_METHOD_DESCRIPTOR $sSQ2eeoiySbx_xtFZTq

SWIFT_RUNTIME_EXPORT
_ConformanceDescriptor<1> _swift_tupleEquatable_conf;

/// The protocol witness for static Swift.Equatable.== infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Equatable in Swift.
SWIFT_CC(swift)
bool _swift_tupleEquatable_equals(OpaqueValue *tuple1, OpaqueValue *tuple2,
                                  SWIFT_CONTEXT Metadata *swiftSelf,
                                  Metadata *Self, void *witnessTable);

//===----------------------------------------------------------------------===//
// Tuple Comparable Conformance
//===----------------------------------------------------------------------===//

#define COMPARABLE_DESCRIPTOR $sSLMp
#define COMPARABLE_BASE_CONFORMANCE_DESCRIPTOR $sSLSQTb
#define COMPARABLE_LT_METHOD_DESCRIPTOR $sSL1loiySbx_xtFZTq
#define COMPARBALE_LTE_METHOD_DESCRIPTOR $sSL2leoiySbx_xtFZTq
#define COMPARABLE_GTE_METHOD_DESCRIPTOR $sSL2geoiySbx_xtFZTq
#define COMPARABLE_GT_METHOD_DESCRIPTOR $sSL1goiySbx_xtFZTq

SWIFT_RUNTIME_EXPORT
_ConformanceDescriptor<5> _swift_tupleComparable_conf;

SWIFT_CC(swift)
const WitnessTable *
_swift_tupleComparable_baseAccessorEquatable(Metadata *assocType,
                                             Metadata *conformingType,
                                             void **witnessTable);

/// The protocol witness for static Swift.Comparable.< infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Comparable in Swift.
SWIFT_CC(swift)
bool _swift_tupleComparable_lessThan(OpaqueValue *tuple1, OpaqueValue *tuple2,
                                     SWIFT_CONTEXT Metadata *swiftSelf,
                                     Metadata *Self, void *witnessTable);

/// The protocol witness for static Swift.Comparable.<= infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Comparable in Swift.
SWIFT_CC(swift)
bool _swift_tupleComparable_lessThanOrEqual(OpaqueValue *tuple1,
                                            OpaqueValue *tuple2,
                                            SWIFT_CONTEXT Metadata *swiftSelf,
                                            Metadata *Self, void *witnessTable);

/// The protocol witness for static Swift.Comparable.>= infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Comparable in Swift.
SWIFT_CC(swift)
bool _swift_tupleComparable_greaterThanOrEqual(OpaqueValue *tuple1,
                                               OpaqueValue *tuple2,
                                              SWIFT_CONTEXT Metadata *swiftSelf,
                                            Metadata *Self, void *witnessTable);

/// The protocol witness for static Swift.Comparable.> infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Comparable in Swift.
SWIFT_CC(swift)
bool _swift_tupleComparable_greaterThan(OpaqueValue *tuple1, OpaqueValue *tuple2,
                                        SWIFT_CONTEXT Metadata *swiftSelf,
                                        Metadata *Self, void *witnessTable);

//===----------------------------------------------------------------------===//
// Tuple Hashable Conformance
//===----------------------------------------------------------------------===//

#define HASHABLE_DESCRIPTOR $sSHMp
#define HASHABLE_BASE_CONFORMANCE_DESCRIPTOR $sSHSQTb
#define HASHABLE_HASHVALUE_METHOD_DESCRIPTOR $sSH9hashValueSivgTq
#define HASHABLE_HASH_METHOD_DESCRIPTOR $sSH4hash4intoys6HasherVz_tFTq
#define HASHABLE_RAWHASHVALUE_METHOD_DESCRIPTOR $sSH13_rawHashValue4seedS2i_tFTq

// Swift._hashValue<A where A: Swift.Hashable>(for: A) -> Swift.Int
#define SWIFT_HASHVALUE_FUNC $ss10_hashValue3forSix_tSHRzlF
// Swift.Hasher.combine<A where A: Swift.Hashable>(A) -> ()
#define SWIFT_HASHER_COMBINE_FUNC $ss6HasherV7combineyyxSHRzlF

SWIFT_RUNTIME_EXPORT
_ConformanceDescriptor<4> _swift_tupleHashable_conf;

/// The protocol witness for Swift.Hashable.hashValue.getter: Swift.Int in
/// conformance (A...): Swift.Hashable in Swift.
SWIFT_CC(swift)
intptr_t _swift_tupleHashable_hashValue(SWIFT_CONTEXT OpaqueValue *tuple,
                                        Metadata *Self, void *witnessTable);

/// The protocol witness for Swift.Hashable.hash(into:) in conformance
/// (A...): Swift.Hashable in Swift.
SWIFT_CC(swift)
void _swift_tupleHashable_hash(OpaqueValue *hasher,
                               SWIFT_CONTEXT OpaqueValue *tuple,
                               Metadata *Self, void *witnessTable);

} // end namespace swift

#endif