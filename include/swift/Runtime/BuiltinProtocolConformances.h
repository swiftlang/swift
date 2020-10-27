//===--- BuiltinProtocolWitnessTable.h --------------------------*- C++ -*-===//
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
// Swift runtime support for builtin protocol witnesses and related items.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_BUILTINPROTOCOLCONFORMANCES_H
#define SWIFT_RUNTIME_BUILTINPROTOCOLCONFORMANCES_H

#include "swift/ABI/Metadata.h"

namespace swift {

#define STR(a) #a
#define XSTR(a) STR(a)
#define SYMBOL(name) XSTR(__USER_LABEL_PREFIX__) name

#define PROTOCOL_DESCRIPTOR_MANGLING Mp

#define PROTOCOL_DESCRIPTOR_SYM(Proto) \
          MANGLE_SYM(MANGLING_CONCAT2(Proto, PROTOCOL_DESCRIPTOR_MANGLING))

//===----------------------------------------------------------------------===//
// Tuple Equatable Conformance
//===----------------------------------------------------------------------===//

// public protocol Equatable {}
#define SWIFT_EQUATABLE_MANGLING SQ

#define EQUATABLE_DESCRIPTOR PROTOCOL_DESCRIPTOR_SYM(SWIFT_EQUATABLE_MANGLING)

#define EQUATABLE_DESCRIPTOR_SYMBOL SYMBOL("$sSQMp")
#define EQUATABLE_EE_METHOD_DESCRIPTOR SYMBOL("$sSQ2eeoiySbx_xtFZTq")

#define TUPLE_EQUATABLE_CONF SYMBOL("_swift_tupleEquatable_conf")
#define TUPLE_EQUATABLE_EQUALS SYMBOL("_swift_tupleEquatable_equals")

/// The protocol witness for static Swift.Equatable.== infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Equatable in Swift.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
bool _swift_tupleEquatable_equals(OpaqueValue *tuple1, OpaqueValue *tuple2,
                                  SWIFT_CONTEXT Metadata *swiftSelf,
                                  Metadata *Self, void *witnessTable);

//===----------------------------------------------------------------------===//
// Tuple Comparable Conformance
//===----------------------------------------------------------------------===//

// public protocol Comparable {}
#define SWIFT_COMPARABLE_MANGLING SL

#define COMPARABLE_DESCRIPTOR PROTOCOL_DESCRIPTOR_SYM(SWIFT_COMPARABLE_MANGLING)

#define COMPARABLE_DESCRIPTOR_SYMBOL SYMBOL("$sSLMp")

#define COMPARABLE_BASE_CONFORMANCE_DESCRIPTOR SYMBOL("$sSLSQTb")
#define COMPARABLE_LT_METHOD_DESCRIPTOR SYMBOL("$sSL1loiySbx_xtFZTq")
#define COMPARBALE_LTE_METHOD_DESCRIPTOR SYMBOL("$sSL2leoiySbx_xtFZTq")
#define COMPARABLE_GTE_METHOD_DESCRIPTOR SYMBOL("$sSL2geoiySbx_xtFZTq")
#define COMPARABLE_GT_METHOD_DESCRIPTOR SYMBOL("$sSL1goiySbx_xtFZTq")

#define TUPLE_COMPARABLE_CONF SYMBOL("_swift_tupleComparable_conf")
#define TUPLE_COMPARABLE_ASSOCIATEDCONFORMANCE \
          SYMBOL("associated conformance _swift_tupleComparable")
#define TUPLE_COMPARABLE_BASEACCESSOREQUATABLE \
          SYMBOL("_swift_tupleComparable_baseAccessorEquatable")
#define TUPLE_COMPARABLE_LESSTHAN SYMBOL("_swift_tupleComparable_lessThan")
#define TUPLE_COMPARABLE_LESSTHANOREQUAL \
          SYMBOL("_swift_tupleComparable_lessThanOrEqual")
#define TUPLE_COMPARABLE_GREATERTHANOREQUAL \
          SYMBOL("_swift_tupleComparable_greaterThanOrEqual")
#define TUPLE_COMPARABLE_GREATERTHAN \
          SYMBOL("_swift_tupleComparable_greaterThan")

/// The protocol witness for static Swift.Comparable.< infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Comparable in Swift.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
bool _swift_tupleComparable_lessThan(OpaqueValue *tuple1, OpaqueValue *tuple2,
                                     SWIFT_CONTEXT Metadata *swiftSelf,
                                     Metadata *Self, void *witnessTable);

/// The protocol witness for static Swift.Comparable.<= infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Comparable in Swift.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
bool _swift_tupleComparable_lessThanOrEqual(OpaqueValue *tuple1,
                                            OpaqueValue *tuple2,
                                            SWIFT_CONTEXT Metadata *swiftSelf,
                                            Metadata *Self, void *witnessTable);

/// The protocol witness for static Swift.Comparable.>= infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Comparable in Swift.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
bool _swift_tupleComparable_greaterThanOrEqual(OpaqueValue *tuple1,
                                               OpaqueValue *tuple2,
                                              SWIFT_CONTEXT Metadata *swiftSelf,
                                            Metadata *Self, void *witnessTable);

/// The protocol witness for static Swift.Comparable.> infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Comparable in Swift.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
bool _swift_tupleComparable_greaterThan(OpaqueValue *tuple1, OpaqueValue *tuple2,
                                        SWIFT_CONTEXT Metadata *swiftSelf,
                                        Metadata *Self, void *witnessTable);

//===----------------------------------------------------------------------===//
// Tuple Hashable Conformance
//===----------------------------------------------------------------------===//

// public protocol Hashable {}
#define SWIFT_HASHABLE_MANGLING SH

#define HASHABLE_DESCRIPTOR PROTOCOL_DESCRIPTOR_SYM(SWIFT_HASHABLE_MANGLING)

#define HASHABLE_DESCRIPTOR_SYMBOL SYMBOL("$sSHMp")

// Swift._hashValue<A where A: Swift.Hashable>(for: A) -> Swift.Int
#define SWIFT_HASHVALUE_FUNC $ss10_hashValue3forSix_tSHRzlF
// Swift.Hasher.combine<A where A: Swift.Hashable>(A) -> ()
#define SWIFT_HASHER_COMBINE_FUNC $ss6HasherV7combineyyxSHRzlF

#define HASHABLE_BASE_CONFORMANCE_DESCRIPTOR SYMBOL("$sSHSQTb")
#define HASHABLE_HASHVALUE_METHOD_DESCRIPTOR SYMBOL("$sSH9hashValueSivgTq")
#define HASHABLE_HASH_METHOD_DESCRIPTOR SYMBOL("$sSH4hash4intoys6HasherVz_tFTq")
#define HASHABLE_RAWHASHVALUE_METHOD_DESCRIPTOR \
          SYMBOL("$sSH13_rawHashValue4seedS2i_tFTq")

#define TUPLE_HASHABLE_CONF SYMBOL("_swift_tupleHashable_conf")
#define TUPLE_HASHABLE_HASHVALUE SYMBOL("_swift_tupleHashable_hashValue")
#define TUPLE_HASHABLE_HASH SYMBOL("_swift_tupleHashable_hash")

/// The protocol witness for Swift.Hashable.hashValue.getter: Swift.Int in
/// conformance (A...): Swift.Hashable in Swift.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
intptr_t _swift_tupleHashable_hashValue(SWIFT_CONTEXT OpaqueValue *tuple,
                                        Metadata *Self, void *witnessTable);

/// The protocol witness for Swift.Hashable.hash(into:) in conformance
/// (A...): Swift.Hashable in Swift.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
void _swift_tupleHashable_hash(OpaqueValue *hasher,
                               SWIFT_CONTEXT OpaqueValue *tuple,
                               Metadata *Self, void *witnessTable);

} // end namespace swift

#endif
