//===------ TypeCheckFullyInhabited.h -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Semantic analysis for ConvertibleToBytes and ConvertibleFromBytes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_TYPECHECKFULLYINHABITED_H
#define SWIFT_SEMA_TYPECHECKFULLYINHABITED_H

namespace swift {
class ProtocolConformance;

void checkConvertibleToBytesConformance(ProtocolConformance *conformance);
void checkConvertibleFromBytesConformance(ProtocolConformance *conformance);
} // end namespace swift

#endif // !SWIFT_SEMA_TYPECHECKFULLYINHABITED_H
