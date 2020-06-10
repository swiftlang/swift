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
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeDeclFinder.h"

using namespace swift;

/// A uniquely-typed boolean to reduce the chances of accidentally inverting
/// a check.
enum class DowngradeToWarning: bool {
  No,
  Yes
};

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
    if (diagnoseDeclRefExportability(loc, declRef, DC, Kind))
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

  // Public non-SPI declarations are OK.
  if (D->getFormalAccessScope(/*useDC=*/nullptr,
                              Kind.allowUsableFromInline).isPublic() &&
      !D->isSPI())
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
                                      FragileFunctionKind fragileKind) {
  assert(fragileKind.kind != FragileFunctionKind::None);

  auto definingModule = D->getModuleContext();

  bool isImplementationOnly =
    userSF.isImportedImplementationOnly(definingModule);
  if (!isImplementationOnly && !userSF.isImportedAsSPI(D))
    return false;

  // TODO: different diagnostics
  ASTContext &ctx = definingModule->getASTContext();
  ctx.Diags.diagnose(loc, diag::inlinable_decl_ref_from_hidden_module,
                     D->getDescriptiveKind(), D->getName(),
                     static_cast<unsigned>(fragileKind.kind),
                     definingModule->getName(),
                     static_cast<unsigned>(!isImplementationOnly));
  return true;
}

static bool
diagnoseGenericArgumentsExportability(SourceLoc loc,
                                      SubstitutionMap subs,
                                      const SourceFile &userSF) {
  bool hadAnyIssues = false;
  for (ProtocolConformanceRef conformance : subs.getConformances()) {
    if (!conformance.isConcrete())
      continue;
    const ProtocolConformance *concreteConf = conformance.getConcrete();

    SubstitutionMap subConformanceSubs =
        concreteConf->getSubstitutions(userSF.getParentModule());
    diagnoseGenericArgumentsExportability(loc, subConformanceSubs, userSF);

    const RootProtocolConformance *rootConf =
        concreteConf->getRootConformance();
    ModuleDecl *M = rootConf->getDeclContext()->getParentModule();
    if (!userSF.isImportedImplementationOnly(M))
      continue;

    ASTContext &ctx = M->getASTContext();
    ctx.Diags.diagnose(loc, diag::conformance_from_implementation_only_module,
                       rootConf->getType(),
                       rootConf->getProtocol()->getName(), 0, M->getName());
    hadAnyIssues = true;
  }
  return hadAnyIssues;
}

void TypeChecker::diagnoseGenericTypeExportability(SourceLoc Loc, Type T,
                                                   const DeclContext *DC) {
  const SourceFile *SF = DC->getParentSourceFile();
  if (!SF)
    return;

  // FIXME: It would be nice to highlight just the part of the type that's
  // problematic, but unfortunately the TypeRepr doesn't have the
  // information we need and the Type doesn't easily map back to it.
  if (auto *BGT = dyn_cast<BoundGenericType>(T.getPointer())) {
    ModuleDecl *useModule = SF->getParentModule();
    auto subs = T->getContextSubstitutionMap(useModule, BGT->getDecl());
    (void)diagnoseGenericArgumentsExportability(Loc, subs, *SF);
  } else if (auto *TAT = dyn_cast<TypeAliasType>(T.getPointer())) {
    auto subs = TAT->getSubstitutionMap();
    (void)diagnoseGenericArgumentsExportability(Loc, subs, *SF);
  }
}

bool
TypeChecker::diagnoseDeclRefExportability(SourceLoc loc,
                                          ConcreteDeclRef declRef,
                                          const DeclContext *DC,
                                          FragileFunctionKind fragileKind) {
  // We're only interested in diagnosing uses from source files.
  auto userSF = DC->getParentSourceFile();
  if (!userSF)
    return false;

  // If the source file doesn't have any implementation-only imports,
  // we can fast-path this.  In the current language design, we never
  // need to consider the possibility of implementation-only imports
  // from other source files in the module (or indirectly in other modules).
  // TODO: maybe check whether D is from a bridging header?
  if (!userSF->hasImplementationOnlyImports())
    return false;

  const ValueDecl *D = declRef.getDecl();
  if (diagnoseDeclExportability(loc, D, *userSF, fragileKind))
    return true;
  if (diagnoseGenericArgumentsExportability(loc, declRef.getSubstitutions(),
                                            *userSF)) {
    return true;
  }
  return false;
}
