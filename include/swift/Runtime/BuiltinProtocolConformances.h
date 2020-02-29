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

#include "swift/ABI/Metadata.h"

namespace swift {

// public protocol Equatable {}
#define SWIFT_EQUATABLE_MANGLING SQ
// public protocol Comparable {}
#define SWIFT_COMPARABLE_MANGLING SL

#define PROTOCOL_DESCRIPTOR_MANGLING Mp

#define PROTOCOL_DESCRIPTOR_SYM(Proto) \
          MANGLE_SYM(MANGLING_CONCAT2(Proto, PROTOCOL_DESCRIPTOR_MANGLING))

// MachO requires a leading underscore for symbols
#if defined(__MACH__)
#define SYMBOL(name) "_" name
#else
#define SYMBOL(name) name
#endif

#define TUPLE_EQUATABLE_CONF SYMBOL("_swift_tupleEquatable_conf")
#define TUPLE_EQUATABLE_WT SYMBOL("_swift_tupleEquatable_wt")

#define TUPLE_COMPARABLE_CONF SYMBOL("_swift_tupleComparable_conf")
#define TUPLE_COMPARABLE_WT SYMBOL("_swift_tupleComparable_wt")

template<unsigned int NumWitnesses>
struct _WitnessTable {
  const ProtocolConformanceDescriptor *Conformance;
  const void *Witnesses[NumWitnesses];
};

template<unsigned int NumWitnessTables, unsigned int NumWitnesses>
struct _DependentWitnessTable {
  const ProtocolConformanceDescriptor *Conformance;
  const WitnessTable *WitnessTables[NumWitnessTables];
  const void *Witnesses[NumWitnesses];
};

/// The builtin protocol conformance witness table for (A...): Swift.Equatable
/// in Swift.
SWIFT_RUNTIME_EXPORT
const _WitnessTable<1> _swift_tupleEquatable_wt;

/// The protocol witness for static Swift.Equatable.== infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Equatable in Swift.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
bool _swift_tupleEquatable_equals(OpaqueValue *tuple1, OpaqueValue *tuple2,
                                  SWIFT_CONTEXT Metadata *swiftSelf,
                                  Metadata *Self, void *witnessTable);


/// The builtin protocol conformance witness table for (A...): Swift.Comparable
/// in Swift.
SWIFT_RUNTIME_EXPORT
const _DependentWitnessTable<1, 4> _swift_tupleComparable_wt;

/// The protocol witness for static Swift.Comparable.< infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Comparable in Swift.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
bool _swift_tupleComparable_lessThan(OpaqueValue *tuple1, OpaqueValue *tuple2,
                                     SWIFT_CONTEXT Metadata *swiftSelf,
                                     Metadata *Self, void *witnessTable);


} // end namespace swift
