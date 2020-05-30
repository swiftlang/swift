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

// public protocol Equatable {}
#define SWIFT_EQUATABLE_MANGLING SQ

#define PROTOCOL_DESCRIPTOR_MANGLING Mp

#define PROTOCOL_DESCRIPTOR_SYM(Proto) \
          MANGLE_SYM(MANGLING_CONCAT2(Proto, PROTOCOL_DESCRIPTOR_MANGLING))

#define EQUATABLE_PROTOCOL_DESCRIPTOR \
          PROTOCOL_DESCRIPTOR_SYM(SWIFT_EQUATABLE_MANGLING)

#define TUPLE_EQUATABLE_CONF SYMBOL("_swift_tupleEquatable_conf")
#define TUPLE_EQUATABLE_EQUALS SYMBOL("_swift_tupleEquatable_equals")

/// The protocol witness for static Swift.Equatable.== infix(A, A) -> Swift.Bool
/// in conformance (A...): Swift.Equatable in Swift.
SWIFT_RUNTIME_EXPORT SWIFT_CC(swift)
bool _swift_tupleEquatable_equals(OpaqueValue *tuple1, OpaqueValue *tuple2,
                                  SWIFT_CONTEXT Metadata *swiftSelf,
                                  Metadata *Self, void *witnessTable);

} // end namespace swift

#endif
