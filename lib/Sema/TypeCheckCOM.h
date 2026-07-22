//===--- TypeCheckCOM.h - Type checking for COM interop -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_TYPECHECKCOM_H
#define SWIFT_SEMA_TYPECHECKCOM_H

#include <cstdint>

namespace swift {
class NominalTypeDecl;
class ProtocolConformance;
enum class KnownProtocolKind : uint8_t;

namespace com {
/// Produce an implicit conformance of \p NTD to the COM protocol \p KP if it
/// is valid to do so.
ProtocolConformance *
deriveImplicitConformance(NominalTypeDecl *NTD, KnownProtocolKind KP);
}
} // end namespace swift

#endif // SWIFT_SEMA_TYPECHECKCOM_H
