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

#ifndef SWIFT_AST_CONCURRENCY_H
#define SWIFT_AST_CONCURRENCY_H

#include "swift/AST/DiagnosticEngine.h"

#include <optional>

namespace swift {

/// Find the imported module that treats the given nominal type as "preconcurrency", or return `nullptr`
/// if there is no such module.
ModuleDecl *moduleImportForPreconcurrency(NominalTypeDecl *nominal,
                                          const DeclContext *fromDC);

/// Determinate the appropriate diagnostic behavior to used when emitting
/// concurrency diagnostics when referencing the given nominal type from the
/// given declaration context.
std::optional<DiagnosticBehavior>
getConcurrencyDiagnosticBehaviorLimit(NominalTypeDecl *nominal,
                                      const DeclContext *fromDC,
                                      bool ignoreExplicitConformance = false);

/// Determine whether the given nominal type has an explicit Sendable
/// conformance (regardless of its availability).
bool hasExplicitSendableConformance(NominalTypeDecl *nominal,
                                    bool applyModuleDefault = true);

} // namespace swift

#endif
