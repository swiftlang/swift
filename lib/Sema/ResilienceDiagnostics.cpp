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
// This file implements diagnostics for @inlinable.
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

bool TypeChecker::diagnoseInlinableDeclRef(SourceLoc loc,
                                           ConcreteDeclRef declRef,
                                           const DeclContext *DC,
                                           FragileFunctionKind Kind) {
  assert(Kind.kind != FragileFunctionKind::None);

  const ValueDecl *D = declRef.getDecl();
  // Do some important fast-path checks that apply to all cases.

  // Type parameters are OK.
  if (isa<AbstractTypeParamDecl>(D))
    return false;

  // Check whether the declaration is accessible.
  if (diagnoseInlinableDeclRefAccess(loc, D, DC, Kind))
    return true;

  // Check whether the declaration comes from a publically-imported module.
  // Skip this check for accessors because the associated property or subscript
  // will also be checked, and will provide a better error message.
  if (!isa<AccessorDecl>(D))
    if (diagnoseDeclRefExportability(loc, declRef, DC,
                                     None, Kind))
      return true;

  return false;
}

bool TypeChecker::diagnoseInlinableDeclRefAccess(SourceLoc loc,
                                                 const ValueDecl *D,
                                                 const DeclContext *DC,
                                                 FragileFunctionKind Kind) {
  assert(Kind.kind != FragileFunctionKind::None);

  // Local declarations are OK.
  if (D->getDeclContext()->isLocalContext())
    return false;

  // Public declarations or SPI used from SPI are OK.
  if (D->getFormalAccessScope(/*useDC=*/nullptr,
                              Kind.allowUsableFromInline).isPublic() &&
      !(D->isSPI() && !DC->getInnermostDeclarationDeclContext()->isSPI()))
    return false;

  auto &Context = DC->getASTContext();

  // Dynamic declarations were mistakenly not checked in Swift 4.2.
  // Do enforce the restriction even in pre-Swift-5 modes if the module we're
  // building is resilient, though.
  if (D->shouldUseObjCDispatch() && !Context.isSwiftVersionAtLeast(5) &&
      !DC->getParentModule()->isResilient()) {
    return false;
  }

  // Property initializers that are not exposed to clients are OK.
  if (auto pattern = dyn_cast<PatternBindingInitializer>(DC)) {
    auto bindingIndex = pattern->getBindingIndex();
    auto *varDecl = pattern->getBinding()->getAnchoringVarDecl(bindingIndex);
    if (!varDecl->isInitExposedToClients())
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

  // Swift 4.2 did not check accessor accessiblity.
  if (auto accessor = dyn_cast<AccessorDecl>(D)) {
    isAccessor = true;

    if (!Context.isSwiftVersionAtLeast(5))
      downgradeToWarning = DowngradeToWarning::Yes;

    // For accessors, diagnose with the name of the storage instead of the
    // implicit '_'.
    diagName = accessor->getStorage()->getName();
  }

  // Swift 5.0 did not check the underlying types of local typealiases.
  // FIXME: Conditionalize this once we have a new language mode.
  if (isa<TypeAliasDecl>(DC))
    downgradeToWarning = DowngradeToWarning::Yes;

  auto diagID = diag::resilience_decl_unavailable;
  if (downgradeToWarning == DowngradeToWarning::Yes)
    diagID = diag::resilience_decl_unavailable_warn;

  Context.Diags.diagnose(
           loc, diagID,
           D->getDescriptiveKind(), diagName,
           D->getFormalAccessScope().accessLevelForDiagnostics(),
           static_cast<unsigned>(Kind.kind),
           isAccessor);

  if (Kind.allowUsableFromInline) {
    Context.Diags.diagnose(D, diag::resilience_decl_declared_here,
                           D->getDescriptiveKind(), diagName, isAccessor);
  } else {
    Context.Diags.diagnose(D, diag::resilience_decl_declared_here_public,
                           D->getDescriptiveKind(), diagName, isAccessor);
  }

  return (downgradeToWarning == DowngradeToWarning::No);
}

static bool diagnoseDeclExportability(SourceLoc loc, const ValueDecl *D,
                                      const SourceFile &userSF,
                                      const DeclContext *userDC,
                                      Optional<ExportabilityReason> reason,
                                      FragileFunctionKind fragileKind) {
  if (fragileKind.kind == FragileFunctionKind::None && !reason.hasValue())
    return false;

  auto definingModule = D->getModuleContext();

  auto downgradeToWarning = DowngradeToWarning::No;
  auto originKind = getDisallowedOriginKind(
      D, userSF, userDC->getInnermostDeclarationDeclContext(),
      downgradeToWarning);
  if (originKind == DisallowedOriginKind::None)
    return false;

  ASTContext &ctx = definingModule->getASTContext();

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
    ctx.Diags.diagnose(loc, diag::inlinable_decl_ref_from_hidden_module,
                       D->getDescriptiveKind(), D->getName(),
                       static_cast<unsigned>(fragileKind.kind),
                       definingModule->getName(),
                       static_cast<unsigned>(originKind));
  }
  return true;
}

bool
TypeChecker::diagnoseConformanceExportability(SourceLoc loc,
                                              const RootProtocolConformance *rootConf,
                                              const SourceFile &userSF,
                                              const DeclContext *userDC,
                                              Optional<ExportabilityReason> reason,
                                              FragileFunctionKind fragileKind) {
  if (fragileKind.kind == FragileFunctionKind::None && !reason.hasValue())
    return false;

  auto originKind = getDisallowedOriginKind(
      rootConf->getDeclContext()->getAsDecl(),
      userSF, userDC->getInnermostDeclarationDeclContext());
  if (originKind == DisallowedOriginKind::None)
    return false;

  if (!reason.hasValue())
    reason = ExportabilityReason::General;

  ModuleDecl *M = rootConf->getDeclContext()->getParentModule();
  ASTContext &ctx = M->getASTContext();
  ctx.Diags.diagnose(loc, diag::conformance_from_implementation_only_module,
                     rootConf->getType(),
                     rootConf->getProtocol()->getName(),
                     static_cast<unsigned>(*reason),
                     M->getName(),
                     static_cast<unsigned>(originKind));
  return true;
}

bool
TypeChecker::diagnoseDeclRefExportability(SourceLoc loc,
                                          ConcreteDeclRef declRef,
                                          const DeclContext *DC,
                                          Optional<ExportabilityReason> reason,
                                          FragileFunctionKind fragileKind) {
  // We're only interested in diagnosing uses from source files.
  auto userSF = DC->getParentSourceFile();
  if (!userSF)
    return false;

  const ValueDecl *D = declRef.getDecl();
  if (diagnoseDeclExportability(loc, D, *userSF, DC, reason, fragileKind))
    return true;
  return false;
}
