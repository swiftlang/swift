//===------------------------------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_SWIFT_EQUATABLE_SUPPORT_H
#define SWIFT_RUNTIME_SWIFT_EQUATABLE_SUPPORT_H

#include "swift/Runtime/Metadata.h"
#include <stdint.h>

namespace swift {
namespace equatable_support {

extern "C" const ProtocolDescriptor PROTOCOL_DESCR_SYM(SQ);
static constexpr auto &EquatableProtocolDescriptor = PROTOCOL_DESCR_SYM(SQ);

struct EquatableWitnessTable;

/// Calls `Equatable.==` through a `Equatable` (not Equatable!) witness
/// table.
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
bool _swift_stdlib_Equatable_isEqual_indirect(
    const void *lhsValue, const void *rhsValue, const Metadata *type,
    const EquatableWitnessTable *wt);

} // namespace equatable_support
} // namespace swift

#endif

