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

#define PROTOCOL_DESCRIPTOR_MANGLING Mp

#define PROTOCOL_DESCRIPTOR_SYM(Proto) \
          MANGLE_SYM(MANGLING_CONCAT2(Proto, PROTOCOL_DESCRIPTOR_MANGLING))

template<unsigned int NumWitnesses>
struct _WitnessTable {
  const ProtocolConformanceDescriptor *Conformance;
  const void *Witnesses[NumWitnesses];
};

/// The builtin protocol conformance witness table for (A...) : Swift.Equatable
// in Swift.
SWIFT_RUNTIME_EXPORT
const _WitnessTable<1> _swift_tupleEquatable_wt;

/// The protocol witness for static Swift.Equatable.== infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Equatable in Swift.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
bool _swift_tupleEquatable_equals(OpaqueValue *tuple1, OpaqueValue *tuple2,
                                  SWIFT_CONTEXT Metadata *swiftSelf,
                                  Metadata *Self, void *witnessTable);

// MachO requires a leading underscore for symbols.
#if defined(__MACH__)
#define TUPLE_EQUATABLE_CONF "__swift_tupleEquatable_conf"
#define TUPLE_EQUATABLE_WT "__swift_tupleEquatable_wt"
#else
#define TUPLE_EQUATABLE_CONF "_swift_tupleEquatable_conf"
#define TUPLE_EQUATABLE_WT "_swift_tupleEquatable_wt"
#endif

} // end namespace swift
