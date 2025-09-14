//===--- TypeCheckEmbedded.cpp - Embedded ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements type checking support for Embedded Swift.
//
//===----------------------------------------------------------------------===//

#include "TypeCheckEmbedded.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Effects.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Types.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Bridging/ASTGen.h"

using namespace swift;

std::optional<DiagnosticBehavior>
swift::shouldDiagnoseEmbeddedLimitations(const DeclContext *dc, SourceLoc loc,
                                         bool wasAlwaysEmbeddedError) {
  // In Embedded Swift, things that were always errors will still be emitted
  // as errors. Use "unspecified" so we don't change anything.
  if (dc->getASTContext().LangOpts.hasFeature(Feature::Embedded) &&
      wasAlwaysEmbeddedError) {
    return DiagnosticBehavior::Unspecified;
  }

  // Check one of the Embedded restriction diagnostics that is ignored by
  // default. If it's still ignored, we won't diagnose anything.
  // limitations.
  auto &diags = dc->getASTContext().Diags;
  if (diags.isIgnoredDiagnostic(diag::untyped_throws_in_embedded_swift.ID))
    return std::nullopt;

#if SWIFT_BUILD_SWIFT_SYNTAX
  // If we are not in Embedded Swift, check whether the location we are
  // diagnosing at is likely to be active when compiling Embedded Swift. If not,
  // suppress the diagnostic.
  auto sourceFile = dc->getParentSourceFile();
  if (!dc->getASTContext().LangOpts.hasFeature(Feature::Embedded) &&
      sourceFile &&
      !swift_ASTGen_activeInEmbeddedSwift(sourceFile->getASTContext(),
                                          sourceFile->getExportedSourceFile(),
                                          loc)) {
    return std::nullopt;
  }
#endif

  // If this was always an error in Embedded Swift, we aren't in Embedded Swift
  // now, so downgrade to a warning.
  if (wasAlwaysEmbeddedError)
    return DiagnosticBehavior::Warning;

  // Leave it as-is.
  return DiagnosticBehavior::Unspecified;
}

/// Check embedded restrictions in the signature of the given function.
void swift::checkEmbeddedRestrictionsInSignature(
    const AbstractFunctionDecl *func) {
  // If we are not supposed to diagnose Embedded Swift limitations, do nothing.
  auto behavior = shouldDiagnoseEmbeddedLimitations(func, func->getLoc());
  if (!behavior)
    return;

  // Untyped throws is not permitted.
  SourceLoc throwsLoc = func->getThrowsLoc();
  if (throwsLoc.isValid() && !func->getThrownTypeRepr() &&
      !func->hasPolymorphicEffect(EffectKind::Throws)) {
    diagnoseUntypedThrowsInEmbedded(func, throwsLoc);
  }
}

void swift::diagnoseUntypedThrowsInEmbedded(
    const DeclContext *dc, SourceLoc throwsLoc) {
  // If we are not supposed to diagnose Embedded Swift limitations, do nothing.
  auto behavior = shouldDiagnoseEmbeddedLimitations(dc, throwsLoc);
  if (!behavior)
    return;

  dc->getASTContext().Diags.diagnose(
      throwsLoc, diag::untyped_throws_in_embedded_swift)
    .limitBehavior(*behavior)
    .fixItInsertAfter(throwsLoc, "(<#thrown error type#>)");
}
