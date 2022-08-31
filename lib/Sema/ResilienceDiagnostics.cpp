//===--- ResilienceDiagnostics.cpp - Resilience Inlineability Diagnostics -===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements diagnostics for fragile functions, like those with
// @inlinable, @_alwaysEmitIntoClient, or @_backDeploy.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "TypeCheckAvailability.h"
#include "TypeCheckAccess.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeDeclFinder.h"

using namespace swift;

bool TypeChecker::diagnoseInlinableDeclRefAccess(SourceLoc loc,
                                                 const ValueDecl *D,
                                                 const ExportContext &where) {
  auto fragileKind = where.getFragileFunctionKind();
  if (fragileKind.kind == FragileFunctionKind::None)
    return false;

  // Local declarations are OK.
  if (D->getDeclContext()->isLocalContext())
    return false;

  auto *DC = where.getDeclContext();

  // Public declarations are OK, even if they're SPI or came from an
  // implementation-only import. We'll diagnose exportability violations
  // from diagnoseDeclRefExportability().
  if (D->getFormalAccessScope(/*useDC=*/nullptr,
                              fragileKind.allowUsableFromInline).isPublic())
    return false;

  auto &Context = DC->getASTContext();

  // Dynamic declarations were mistakenly not checked in Swift 4.2.
  // Do enforce the restriction even in pre-Swift-5 modes if the module we're
  // building is resilient, though.
  if (D->shouldUseObjCDispatch() && !Context.isSwiftVersionAtLeast(5) &&
      !DC->getParentModule()->isResilient()) {
    return false;
  }

  DowngradeToWarning downgradeToWarning = DowngradeToWarning::No;

  // Swift 4.2 did not perform any checks for type aliases.
  if (isa<TypeAliasDecl>(D)) {
    if (!Context.isSwiftVersionAtLeast(4, 2))
      return false;
    if (!Context.isSwiftVersionAtLeast(5))
      downgradeToWarning = DowngradeToWarning::Yes;
  }

  auto diagName = D->getName();
  bool isAccessor = false;

  // Swift 4.2 did not check accessor accessibility.
  if (auto accessor = dyn_cast<AccessorDecl>(D)) {
    isAccessor = true;

    if (!Context.isSwiftVersionAtLeast(5))
      downgradeToWarning = DowngradeToWarning::Yes;

    // For accessors, diagnose with the name of the storage instead of the
    // implicit '_'.
    diagName = accessor->getStorage()->getName();
  }

  // Swift 5.0 did not check the underlying types of local typealiases.
  if (isa<TypeAliasDecl>(DC) && !Context.isSwiftVersionAtLeast(6))
    downgradeToWarning = DowngradeToWarning::Yes;

  auto diagID = diag::resilience_decl_unavailable;
  if (downgradeToWarning == DowngradeToWarning::Yes)
    diagID = diag::resilience_decl_unavailable_warn;

  Context.Diags.diagnose(loc, diagID, D->getDescriptiveKind(), diagName,
                         D->getFormalAccessScope().accessLevelForDiagnostics(),
                         fragileKind.getSelector(), isAccessor);

  if (fragileKind.allowUsableFromInline) {
    Context.Diags.diagnose(D, diag::resilience_decl_declared_here,
                           D->getDescriptiveKind(), diagName, isAccessor);
  } else {
    Context.Diags.diagnose(D, diag::resilience_decl_declared_here_public,
                           D->getDescriptiveKind(), diagName, isAccessor);
  }

  return (downgradeToWarning == DowngradeToWarning::No);
}

static bool diagnoseTypeAliasDeclRefExportability(SourceLoc loc,
                                                  const TypeAliasDecl *TAD,
                                                  const ExportContext &where) {
  assert(where.mustOnlyReferenceExportedDecls());

  auto *D = TAD->getUnderlyingType()->getAnyNominal();
  if (!D)
    return false;

  auto ignoredDowngradeToWarning = DowngradeToWarning::No;
  auto originKind =
      getDisallowedOriginKind(D, where, ignoredDowngradeToWarning);
  if (originKind == DisallowedOriginKind::None)
    return false;

  auto definingModule = D->getModuleContext();
  ASTContext &ctx = definingModule->getASTContext();
  auto fragileKind = where.getFragileFunctionKind();

  if (fragileKind.kind == FragileFunctionKind::None) {
    auto reason = where.getExportabilityReason();
    ctx.Diags
        .diagnose(loc, diag::typealias_desugars_to_type_from_hidden_module,
                  TAD->getName(), definingModule->getNameStr(), D->getNameStr(),
                  static_cast<unsigned>(*reason), definingModule->getName(),
                  static_cast<unsigned>(originKind))
        .warnUntilSwiftVersion(6);
  } else {
    ctx.Diags
        .diagnose(loc,
                  diag::inlinable_typealias_desugars_to_type_from_hidden_module,
                  TAD->getName(), definingModule->getNameStr(), D->getNameStr(),
                  fragileKind.getSelector(), definingModule->getName(),
                  static_cast<unsigned>(originKind))
        .warnUntilSwiftVersion(6);
  }
  D->diagnose(diag::kind_declared_here, DescriptiveDeclKind::Type);

  if (originKind == DisallowedOriginKind::ImplicitlyImported &&
      !ctx.LangOpts.isSwiftVersionAtLeast(6))
    ctx.Diags.diagnose(loc, diag::missing_import_inserted,
                       definingModule->getName());

  return true;
}

static bool diagnoseValueDeclRefExportability(SourceLoc loc, const ValueDecl *D,
                                              const ExportContext &where) {
  assert(where.mustOnlyReferenceExportedDecls());

  auto definingModule = D->getModuleContext();
  auto downgradeToWarning = DowngradeToWarning::No;

  auto originKind = getDisallowedOriginKind(
      D, where, downgradeToWarning);
  if (originKind == DisallowedOriginKind::None)
    return false;

  ASTContext &ctx = definingModule->getASTContext();

  auto fragileKind = where.getFragileFunctionKind();
  auto reason = where.getExportabilityReason();

  if (fragileKind.kind == FragileFunctionKind::None) {
    auto errorOrWarning = downgradeToWarning == DowngradeToWarning::Yes?
                              diag::decl_from_hidden_module_warn:
                              diag::decl_from_hidden_module;
    ctx.Diags.diagnose(loc, errorOrWarning,
                       D->getDescriptiveKind(),
                       D->getName(),
                       static_cast<unsigned>(*reason),
                       definingModule->getName(),
                       static_cast<unsigned>(originKind));

    D->diagnose(diag::kind_declared_here, DescriptiveDeclKind::Type);
  } else {
    // Only implicitly imported decls should be reported as a warning,
    // and only for language versions below Swift 6.
    assert(downgradeToWarning == DowngradeToWarning::No ||
           originKind == DisallowedOriginKind::ImplicitlyImported &&
           "Only implicitly imported decls should be reported as a warning.");
    auto errorOrWarning = downgradeToWarning == DowngradeToWarning::Yes?
                              diag::inlinable_decl_ref_from_hidden_module_warn:
                              diag::inlinable_decl_ref_from_hidden_module;

    ctx.Diags.diagnose(loc, errorOrWarning,
                       D->getDescriptiveKind(), D->getName(),
                       fragileKind.getSelector(), definingModule->getName(),
                       static_cast<unsigned>(originKind));

    if (originKind == DisallowedOriginKind::ImplicitlyImported &&
        downgradeToWarning == DowngradeToWarning::Yes)
      ctx.Diags.diagnose(loc, diag::missing_import_inserted,
                         definingModule->getName());
  }

  return true;
}

bool TypeChecker::diagnoseDeclRefExportability(SourceLoc loc,
                                               const ValueDecl *D,
                                               const ExportContext &where) {
  // Accessors cannot have exportability that's different than the storage,
  // so skip them for now.
  if (isa<AccessorDecl>(D))
    return false;

  if (!where.mustOnlyReferenceExportedDecls())
    return false;

  if (diagnoseValueDeclRefExportability(loc, D, where))
    return true;

  if (auto *TAD = dyn_cast<TypeAliasDecl>(D))
    if (diagnoseTypeAliasDeclRefExportability(loc, TAD, where))
      return true;

  return false;
}

bool
TypeChecker::diagnoseConformanceExportability(SourceLoc loc,
                                              const RootProtocolConformance *rootConf,
                                              const ExtensionDecl *ext,
                                              const ExportContext &where,
                                              bool useConformanceAvailabilityErrorsOption) {
  if (!where.mustOnlyReferenceExportedDecls())
    return false;

  auto originKind = getDisallowedOriginKind(ext, where);
  if (originKind == DisallowedOriginKind::None)
    return false;

  ModuleDecl *M = ext->getParentModule();
  ASTContext &ctx = M->getASTContext();

  auto reason = where.getExportabilityReason();
  if (!reason.hasValue())
    reason = ExportabilityReason::General;

  ctx.Diags.diagnose(loc, diag::conformance_from_implementation_only_module,
                     rootConf->getType(),
                     rootConf->getProtocol()->getName(),
                     static_cast<unsigned>(*reason),
                     M->getName(),
                     static_cast<unsigned>(originKind))
      .warnUntilSwiftVersionIf((useConformanceAvailabilityErrorsOption &&
                                !ctx.LangOpts.EnableConformanceAvailabilityErrors) ||
                               originKind == DisallowedOriginKind::ImplicitlyImported,
                               6);

    if (originKind == DisallowedOriginKind::ImplicitlyImported &&
        !ctx.LangOpts.isSwiftVersionAtLeast(6))
      ctx.Diags.diagnose(loc, diag::missing_import_inserted,
                         M->getName());
  return true;
}
