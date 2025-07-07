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

struct DiagnosticBehavior;

/// If any of the imports in this source file was @preconcurrency but there were
/// no diagnostics downgraded or suppressed due to that @preconcurrency, suggest
/// that the attribute be removed.
void diagnoseUnnecessaryPreconcurrencyImports(SourceFile &sf);

/// Diagnose the use of an instance property of non-Sendable type from an
/// nonisolated deinitializer within an actor-isolated type.
///
/// \returns true iff a diagnostic was emitted for this reference.
bool diagnoseNonSendableFromDeinit(
    SourceLoc refLoc, VarDecl *var, DeclContext *dc);

} // namespace swift

#endif
