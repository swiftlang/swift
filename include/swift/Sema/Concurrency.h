//===--- Concurrency.h ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This file defines concurrency utility routines from Sema that are used by
/// later parts of the compiler like the pass pipeline.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_CONCURRENCY_H
#define SWIFT_SEMA_CONCURRENCY_H

#include <optional>

namespace swift {

class DeclContext;
class SourceFile;
class NominalTypeDecl;
class VarDecl;

enum class DiagnosticBehavior: uint8_t;

/// If any of the imports in this source file was @preconcurrency but there were
/// no diagnostics downgraded or suppressed due to that @preconcurrency, suggest
/// that the attribute be removed.
void diagnoseUnnecessaryPreconcurrencyImports(SourceFile &sf);

/// Determine whether the given nominal type has an explicit Sendable
/// conformance (regardless of its availability).
bool hasExplicitSendableConformance(NominalTypeDecl *nominal,
                                    bool applyModuleDefault = true);

/// Diagnose the use of an instance property of non-sendable type from an
/// nonisolated deinitializer within an actor-isolated type.
///
/// \returns true iff a diagnostic was emitted for this reference.
bool diagnoseNonSendableFromDeinit(
    SourceLoc refLoc, VarDecl *var, DeclContext *dc);

/// Determinate the appropriate diagnostic behavior when referencing
/// the given nominal type from the given declaration context.
std::optional<DiagnosticBehavior>
getConcurrencyDiagnosticBehaviorLimit(NominalTypeDecl *nominal,
                                      const DeclContext *fromDC,
                                      bool ignoreExplicitConformance = false);

} // namespace swift

#endif
