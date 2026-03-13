//===--- Concurrency.cpp --------------------------------------------------===//
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

#include "swift/AST/Concurrency.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"

using namespace swift;

ModuleDecl *swift::moduleImportForPreconcurrency(
    NominalTypeDecl *nominal, const DeclContext *fromDC) {
  // If the declaration itself has the @preconcurrency attribute,
  // respect it.
  if (nominal->getAttrs().hasAttribute<PreconcurrencyAttr>()) {
    return nominal->getParentModule();
  }

  // Determine whether this nominal type is visible via a @preconcurrency
  // import.
  auto import = nominal->findImport(fromDC);
  auto sourceFile = fromDC->getParentSourceFile();

  if (!import || !import->options.contains(ImportFlags::Preconcurrency))
    return nullptr;

  if (sourceFile)
    sourceFile->setImportUsedPreconcurrency(*import);

  return import->module.importedModule;
}

std::optional<DiagnosticBehavior>
swift::getConcurrencyDiagnosticBehaviorLimit(NominalTypeDecl *nominal,
                                             const DeclContext *fromDC,
                                             bool ignoreExplicitConformance) {
  ModuleDecl *importedModule = moduleImportForPreconcurrency(nominal, fromDC);
  if (!importedModule)
    return std::nullopt;

  // When the type is explicitly non-Sendable, @preconcurrency imports
  // downgrade the diagnostic to a warning in Swift 6.
  if (!ignoreExplicitConformance && hasExplicitSendableConformance(nominal))
    return DiagnosticBehavior::Warning;

  // When the type is implicitly non-Sendable, `@preconcurrency` suppresses
  // diagnostics until the imported module enables Swift 6.
  return importedModule->isConcurrencyChecked() ? DiagnosticBehavior::Warning
                                                : DiagnosticBehavior::Ignore;
}

/// Determine whether the given nominal type has an explicit Sendable
/// conformance (regardless of its availability).
bool swift::hasExplicitSendableConformance(NominalTypeDecl *nominal,
                                           bool applyModuleDefault) {
  ASTContext &ctx = nominal->getASTContext();
  auto nominalModule = nominal->getParentModule();

  // In a concurrency-checked module, a missing conformance is equivalent to
  // an explicitly unavailable one. If we want to apply this rule, do so now.
  if (applyModuleDefault && nominalModule->isConcurrencyChecked())
    return true;

  // Look for any conformance to `Sendable`.
  auto proto = ctx.getProtocol(KnownProtocolKind::Sendable);
  if (!proto)
    return false;

  // Look for a conformance. If it's present and not (directly) missing,
  // we're done.
  auto conformance = lookupConformance(nominal->getDeclaredInterfaceType(),
                                       proto, /*allowMissing=*/true);
  return conformance &&
         !(isa<BuiltinProtocolConformance>(conformance.getConcrete()) &&
           cast<BuiltinProtocolConformance>(conformance.getConcrete())
               ->isMissing());
}
