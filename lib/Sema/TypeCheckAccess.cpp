//===--- TypeCheckAccess.cpp - Type Checking for Access Control -----------===//
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
// This file implements access control checking.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "TypeCheckAccess.h"
#include "swift/AST/AccessScopeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ParameterList.h"

using namespace swift;

#define DEBUG_TYPE "TypeCheckAccess"

namespace {

class TypeAccessScopeDiagnoser : private ASTWalker {
  AccessScope accessScope;
  const DeclContext *useDC;
  bool treatUsableFromInlineAsPublic;
  const ComponentIdentTypeRepr *offendingType = nullptr;

  bool walkToTypeReprPre(TypeRepr *TR) override {
    // Exit early if we've already found a problem type.
    if (offendingType)
      return false;

    auto CITR = dyn_cast<ComponentIdentTypeRepr>(TR);
    if (!CITR)
      return true;

    const ValueDecl *VD = CITR->getBoundDecl();
    if (!VD)
      return true;

    if (VD->getFormalAccessScope(useDC, treatUsableFromInlineAsPublic)
        != accessScope)
      return true;

    offendingType = CITR;
    return false;
  }

  bool walkToTypeReprPost(TypeRepr *T) override {
    // Exit early if we've already found a problem type.
    return offendingType != nullptr;
  }

  explicit TypeAccessScopeDiagnoser(AccessScope accessScope,
                                    const DeclContext *useDC,
                                    bool treatUsableFromInlineAsPublic)
    : accessScope(accessScope), useDC(useDC),
      treatUsableFromInlineAsPublic(treatUsableFromInlineAsPublic) {}

public:
  static const TypeRepr *findTypeWithScope(TypeRepr *TR,
                                           AccessScope accessScope,
                                           const DeclContext *useDC,
                                           bool treatUsableFromInlineAsPublic) {
    assert(!accessScope.isPublic() &&
           "why would we need to find a public access scope?");
    if (TR == nullptr)
      return nullptr;
    TypeAccessScopeDiagnoser diagnoser(accessScope, useDC,
                                       treatUsableFromInlineAsPublic);
    TR->walk(diagnoser);
    return diagnoser.offendingType;
  }
};

} // end anonymous namespace

/// Checks if the access scope of the type described by \p TL contains
/// \p contextAccessScope. If it isn't, calls \p diagnose with a TypeRepr
/// representing the offending part of \p TL.
///
/// If \p contextAccessScope is null, checks that \p TL is only made up of
/// public types.
///
/// The TypeRepr passed to \p diagnose may be null, in which case a particular
/// part of the type that caused the problem could not be found. The DeclContext
/// is never null.
void AccessControlCheckerBase::checkTypeAccessImpl(
    TypeLoc TL, AccessScope contextAccessScope,
    const DeclContext *useDC,
    llvm::function_ref<CheckTypeAccessCallback> diagnose) {
  if (!TC.getLangOpts().EnableAccessControl)
    return;
  if (!TL.getType())
    return;
  // Don't spend time checking local declarations; this is always valid by the
  // time we get to this point.
  if (!contextAccessScope.isPublic() &&
      contextAccessScope.getDeclContext()->isLocalContext())
    return;

  // TypeRepr checking is more accurate, but we must also look at TypeLocs
  // without a TypeRepr, for example for 'var' declarations with an inferred
  // type.
  auto typeAccessScope =
    (TL.getTypeRepr()
     ? TypeReprAccessScopeChecker::getAccessScope(TL.getTypeRepr(),
                                                  useDC,
                                                  checkUsableFromInline)
     : TypeAccessScopeChecker::getAccessScope(TL.getType(),
                                              useDC,
                                              checkUsableFromInline));

  // Note: This means that the type itself is invalid for this particular
  // context, because it references declarations from two incompatible scopes.
  // In this case we should have diagnosed the bad reference already.
  if (!typeAccessScope.hasValue())
    return;

  auto shouldComplainAboutAccessScope =
      [contextAccessScope](AccessScope scope) -> bool {
    if (scope.isPublic())
      return false;
    if (scope.hasEqualDeclContextWith(contextAccessScope))
      return false;
    if (contextAccessScope.isChildOf(scope))
      return false;
    return true;
  };

  if (!shouldComplainAboutAccessScope(typeAccessScope.getValue()))
    return;

  auto downgradeToWarning = DowngradeToWarning::No;
  if (!checkUsableFromInline) {
    // Swift 3.0 wasn't nearly as strict as checking types because it didn't
    // look at the TypeRepr at all except to highlight a particular part of the
    // type in diagnostics, and looked through typealiases in other cases.
    // Approximate this behavior by running our non-TypeRepr-based check again
    // and downgrading to a warning when the checks disagree.
    if (TC.getLangOpts().isSwiftVersion3()) {
      auto typeOnlyAccessScope =
        TypeAccessScopeChecker::getAccessScope(
            TL.getType(), useDC,
            /*treatUsableFromInlineAsPublic*/false,
            /*canonicalizeParents*/true);
      if (typeOnlyAccessScope.hasValue()) {
        // If Swift 4 would have complained about a private type, but Swift 4
        // would only diagnose an internal type, complain about the Swift 3
        // offense first to avoid confusing users.
        if (shouldComplainAboutAccessScope(typeOnlyAccessScope.getValue()))
          typeAccessScope = typeOnlyAccessScope;
        else
          downgradeToWarning = DowngradeToWarning::Yes;
      }
    }

    // Swift 3.0.0 mistakenly didn't diagnose any issues when the context
    // access scope represented a private or fileprivate level.
    if (!contextAccessScope.isPublic() &&
        !isa<ModuleDecl>(contextAccessScope.getDeclContext()) &&
        TC.getLangOpts().isSwiftVersion3()) {
      downgradeToWarning = DowngradeToWarning::Yes;
    }
  }

  const TypeRepr *complainRepr =
        TypeAccessScopeDiagnoser::findTypeWithScope(
            TL.getTypeRepr(),
            *typeAccessScope,
            useDC, checkUsableFromInline);
  diagnose(*typeAccessScope, complainRepr, downgradeToWarning);
}

/// Checks if the access scope of the type described by \p TL is valid for the
/// type to be the type of \p context. If it isn't, calls \p diagnose with a
/// TypeRepr representing the offending part of \p TL.
///
/// The TypeRepr passed to \p diagnose may be null, in which case a particular
/// part of the type that caused the problem could not be found.
void AccessControlCheckerBase::checkTypeAccess(
    TypeLoc TL, const ValueDecl *context,
    llvm::function_ref<CheckTypeAccessCallback> diagnose) {
  assert(!isa<ParamDecl>(context));
  const DeclContext *DC = context->getDeclContext();
  AccessScope contextAccessScope =
    context->getFormalAccessScope(
      nullptr, checkUsableFromInline);
  checkTypeAccessImpl(TL, contextAccessScope,
                      DC, diagnose);
}

/// Highlights the given TypeRepr, and adds a note pointing to the type's
/// declaration if possible.
///
/// Just flushes \p diag as is if \p complainRepr is null.
static void highlightOffendingType(TypeChecker &TC, InFlightDiagnostic &diag,
                                   const TypeRepr *complainRepr) {
  if (!complainRepr) {
    diag.flush();
    return;
  }

  diag.highlight(complainRepr->getSourceRange());
  diag.flush();

  if (auto CITR = dyn_cast<ComponentIdentTypeRepr>(complainRepr)) {
    const ValueDecl *VD = CITR->getBoundDecl();
    TC.diagnose(VD, diag::kind_declared_here, DescriptiveDeclKind::Type);
  }
}

void AccessControlCheckerBase::checkRequirementAccess(
    ArrayRef<RequirementRepr> requirements,
    AccessScope accessScope,
    const DeclContext *useDC,
    llvm::function_ref<CheckTypeAccessCallback> diagnose) {
  for (auto &requirement : requirements) {
    switch (requirement.getKind()) {
    case RequirementReprKind::TypeConstraint:
      checkTypeAccessImpl(requirement.getSubjectLoc(),
                          accessScope, useDC, diagnose);
      checkTypeAccessImpl(requirement.getConstraintLoc(),
                          accessScope, useDC, diagnose);
      break;
    case RequirementReprKind::LayoutConstraint:
      checkTypeAccessImpl(requirement.getSubjectLoc(),
                          accessScope, useDC, diagnose);
      break;
    case RequirementReprKind::SameType:
      checkTypeAccessImpl(requirement.getFirstTypeLoc(),
                          accessScope, useDC, diagnose);
      checkTypeAccessImpl(requirement.getSecondTypeLoc(),
                          accessScope, useDC, diagnose);
      break;
    }
  }
}

void AccessControlCheckerBase::checkGenericParamAccess(
    const GenericParamList *params,
    const Decl *owner,
    AccessScope accessScope,
    AccessLevel contextAccess) {
  if (!params)
    return;

  if (checkUsableFromInline) {
    if (auto *VD = dyn_cast<ValueDecl>(owner)) {
      if (VD->getFormalAccess() != AccessLevel::Internal)
        return;
      if (!VD->isUsableFromInline())
        return;
    }
  }

  // This must stay in sync with diag::generic_param_access.
  enum class ACEK {
    Parameter = 0,
    Requirement
  } accessControlErrorKind;
  auto minAccessScope = AccessScope::getPublic();
  const TypeRepr *complainRepr = nullptr;
  auto downgradeToWarning = DowngradeToWarning::Yes;

  auto callbackACEK = ACEK::Parameter;

  auto callback = [&](AccessScope typeAccessScope,
                      const TypeRepr *thisComplainRepr,
                      DowngradeToWarning thisDowngrade) {
    if (typeAccessScope.isChildOf(minAccessScope) ||
        (thisDowngrade == DowngradeToWarning::No &&
         downgradeToWarning == DowngradeToWarning::Yes) ||
        (!complainRepr &&
         typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
      minAccessScope = typeAccessScope;
      complainRepr = thisComplainRepr;
      accessControlErrorKind = callbackACEK;
      downgradeToWarning = thisDowngrade;
    }
  };

  auto *DC = owner->getDeclContext();

  for (auto param : *params) {
    if (param->getInherited().empty())
      continue;
    assert(param->getInherited().size() == 1);
    checkTypeAccessImpl(param->getInherited().front(), accessScope,
                        DC, callback);
  }
  callbackACEK = ACEK::Requirement;

  checkRequirementAccess(params->getRequirements(),
                         accessScope, DC, callback);

  if (minAccessScope.isPublic())
    return;

  if (checkUsableFromInline) {
    auto diagID = diag::generic_param_usable_from_inline;
    if (!TC.Context.isSwiftVersionAtLeast(5))
      diagID = diag::generic_param_usable_from_inline_warn;
    auto diag = TC.diagnose(owner,
                            diagID,
                            owner->getDescriptiveKind(),
                            accessControlErrorKind == ACEK::Requirement);
    highlightOffendingType(TC, diag, complainRepr);
    return;
  }

  auto minAccess = minAccessScope.accessLevelForDiagnostics();

  bool isExplicit =
    owner->getAttrs().hasAttribute<AccessControlAttr>() ||
    isa<ProtocolDecl>(owner->getDeclContext());
  auto diagID = diag::generic_param_access;
  if (downgradeToWarning == DowngradeToWarning::Yes)
    diagID = diag::generic_param_access_warn;
  auto diag = TC.diagnose(owner, diagID,
                          owner->getDescriptiveKind(), isExplicit,
                          contextAccess, minAccess,
                          isa<FileUnit>(owner->getDeclContext()),
                          accessControlErrorKind == ACEK::Requirement);
  highlightOffendingType(TC, diag, complainRepr);
}

void AccessControlCheckerBase::checkGenericParamAccess(
    const GenericParamList *params,
    const ValueDecl *owner) {
  checkGenericParamAccess(params, owner,
                          owner->getFormalAccessScope(nullptr,
                                                      checkUsableFromInline),
                          owner->getFormalAccess());
}

/// Checks the given declaration's signature does not reference any other
/// declarations that are less visible than the declaration itself.
///
/// \p D must be a ValueDecl or a Decl that can appear in a type context.
void AccessControlChecker::check(Decl *D) {
  if (D->isInvalid() || D->isImplicit())
    return;

  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::PrecedenceGroup:
  case DeclKind::Module:
    llvm_unreachable("cannot appear in a type context");

  case DeclKind::Param:
  case DeclKind::GenericTypeParam:
  case DeclKind::MissingMember:
    llvm_unreachable("does not have access control");

  case DeclKind::IfConfig:
  case DeclKind::PoundDiagnostic:
    // Does not have access control.
  case DeclKind::EnumCase:
    // Handled at the EnumElement level.
  case DeclKind::Var:
    // Handled at the PatternBindingDecl level.
  case DeclKind::Destructor:
    // Always correct.
    return;

  case DeclKind::PatternBinding: {
    auto PBD = cast<PatternBindingDecl>(D);
    bool isTypeContext = PBD->getDeclContext()->isTypeContext();

    llvm::DenseSet<const VarDecl *> seenVars;
    for (auto entry : PBD->getPatternList())
    entry.getPattern()->forEachNode([&](const Pattern *P) {
      if (auto *NP = dyn_cast<NamedPattern>(P)) {
        // Only check individual variables if we didn't check an enclosing
        // TypedPattern.
        const VarDecl *theVar = NP->getDecl();
        if (seenVars.count(theVar) || theVar->isInvalid())
          return;

        checkTypeAccess(TypeLoc::withoutLoc(theVar->getType()),
                        theVar,
                        [&](AccessScope typeAccessScope,
                            const TypeRepr *complainRepr,
                            DowngradeToWarning downgradeToWarning) {
          auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
          bool isExplicit =
            theVar->getAttrs().hasAttribute<AccessControlAttr>() ||
            isa<ProtocolDecl>(theVar->getDeclContext());
          auto theVarAccess = isExplicit
            ? theVar->getFormalAccess()
            : typeAccessScope.requiredAccessForDiagnostics();
          auto diagID = diag::pattern_type_access_inferred;
          if (downgradeToWarning == DowngradeToWarning::Yes)
            diagID = diag::pattern_type_access_inferred_warn;
          auto diag = TC.diagnose(P->getLoc(), diagID,
                                  theVar->isLet(),
                                  isTypeContext,
                                  isExplicit,
                                  theVarAccess,
                                  isa<FileUnit>(theVar->getDeclContext()),
                                  typeAccess,
                                  theVar->getType());
        });
        return;
      }

      auto *TP = dyn_cast<TypedPattern>(P);
      if (!TP)
        return;

      // FIXME: We need an access level to check against, so we pull one out of
      // some random VarDecl in the pattern. They're all going to be the same,
      // but still, ick.
      const VarDecl *anyVar = nullptr;
      TP->forEachVariable([&](VarDecl *V) {
        seenVars.insert(V);
        anyVar = V;
      });
      if (!anyVar)
        return;

      checkTypeAccess(TP->getTypeLoc(), anyVar,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning) {
        auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
        bool isExplicit =
          anyVar->getAttrs().hasAttribute<AccessControlAttr>() ||
          isa<ProtocolDecl>(anyVar->getDeclContext());
        auto diagID = diag::pattern_type_access;
        if (downgradeToWarning == DowngradeToWarning::Yes)
          diagID = diag::pattern_type_access_warn;
        auto anyVarAccess = isExplicit
          ? anyVar->getFormalAccess()
          : typeAccessScope.requiredAccessForDiagnostics();
        auto diag = TC.diagnose(P->getLoc(), diagID,
                                anyVar->isLet(),
                                isTypeContext,
                                isExplicit,
                                anyVarAccess,
                                isa<FileUnit>(anyVar->getDeclContext()),
                                typeAccess);
        highlightOffendingType(TC, diag, complainRepr);
      });
    });
    return;
  }

  case DeclKind::TypeAlias: {
    auto TAD = cast<TypeAliasDecl>(D);

    checkTypeAccess(TAD->getUnderlyingTypeLoc(), TAD,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *complainRepr,
                        DowngradeToWarning downgradeToWarning) {
      auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
      bool isExplicit =
        TAD->getAttrs().hasAttribute<AccessControlAttr>() ||
        isa<ProtocolDecl>(TAD->getDeclContext());
      auto diagID = diag::type_alias_underlying_type_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::type_alias_underlying_type_access_warn;
      auto aliasAccess = isExplicit
        ? TAD->getFormalAccess()
        : typeAccessScope.requiredAccessForDiagnostics();
      auto diag = TC.diagnose(TAD, diagID,
                              isExplicit, aliasAccess,
                              typeAccess, isa<FileUnit>(TAD->getDeclContext()));
      highlightOffendingType(TC, diag, complainRepr);
    });

    return;
  }

  case DeclKind::AssociatedType: {
    auto assocType = cast<AssociatedTypeDecl>(D);

    // This must stay in sync with diag::associated_type_access.
    enum {
      ACEK_DefaultDefinition = 0,
      ACEK_Requirement
    } accessControlErrorKind;
    auto minAccessScope = AccessScope::getPublic();
    const TypeRepr *complainRepr = nullptr;
    auto downgradeToWarning = DowngradeToWarning::No;

    std::for_each(assocType->getInherited().begin(),
                  assocType->getInherited().end(),
                  [&](TypeLoc requirement) {
      checkTypeAccess(requirement, assocType,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *thisComplainRepr,
                          DowngradeToWarning downgradeDiag) {
        if (typeAccessScope.isChildOf(minAccessScope) ||
            (!complainRepr &&
             typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
          minAccessScope = typeAccessScope;
          complainRepr = thisComplainRepr;
          accessControlErrorKind = ACEK_Requirement;
          downgradeToWarning = downgradeDiag;
        }
      });
    });
    checkTypeAccess(assocType->getDefaultDefinitionLoc(), assocType,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *thisComplainRepr,
                        DowngradeToWarning downgradeDiag) {
      if (typeAccessScope.isChildOf(minAccessScope) ||
          (!complainRepr &&
           typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
        minAccessScope = typeAccessScope;
        complainRepr = thisComplainRepr;
        accessControlErrorKind = ACEK_DefaultDefinition;
        downgradeToWarning = downgradeDiag;
      }
    });

    if (auto *trailingWhereClause = assocType->getTrailingWhereClause()) {
      checkRequirementAccess(trailingWhereClause->getRequirements(),
                             assocType->getFormalAccessScope(),
                             assocType->getDeclContext(),
                             [&](AccessScope typeAccessScope,
                                 const TypeRepr *thisComplainRepr,
                                 DowngradeToWarning downgradeDiag) {
        if (typeAccessScope.isChildOf(minAccessScope) ||
            (!complainRepr &&
             typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
          minAccessScope = typeAccessScope;
          complainRepr = thisComplainRepr;
          accessControlErrorKind = ACEK_Requirement;
          downgradeToWarning = downgradeDiag;

          // Swift versions before 5.0 did not check requirements on the
          // protocol's where clause, so emit a warning.
          if (!TC.Context.isSwiftVersionAtLeast(5))
            downgradeToWarning = DowngradeToWarning::Yes;
        }
      });
    }

    if (!minAccessScope.isPublic()) {
      auto minAccess = minAccessScope.accessLevelForDiagnostics();
      auto diagID = diag::associated_type_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::associated_type_access_warn;
      auto diag = TC.diagnose(assocType, diagID,
                              assocType->getFormalAccess(),
                              minAccess, accessControlErrorKind);
      highlightOffendingType(TC, diag, complainRepr);
    }
    return;
  }

  case DeclKind::Enum: {
    auto ED = cast<EnumDecl>(D);

    checkGenericParamAccess(ED->getGenericParams(), ED);

    if (ED->hasRawType()) {
      Type rawType = ED->getRawType();
      auto rawTypeLocIter = std::find_if(ED->getInherited().begin(),
                                         ED->getInherited().end(),
                                         [&](TypeLoc inherited) {
        if (!inherited.wasValidated())
          return false;
        return inherited.getType().getPointer() == rawType.getPointer();
      });
      if (rawTypeLocIter == ED->getInherited().end())
        return;
      checkTypeAccess(*rawTypeLocIter, ED,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning) {
        auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
        bool isExplicit = ED->getAttrs().hasAttribute<AccessControlAttr>();
        auto diagID = diag::enum_raw_type_access;
        if (downgradeToWarning == DowngradeToWarning::Yes)
          diagID = diag::enum_raw_type_access_warn;
        auto enumDeclAccess = isExplicit
          ? ED->getFormalAccess()
          : typeAccessScope.requiredAccessForDiagnostics();
        auto diag = TC.diagnose(ED, diagID, isExplicit,
                                enumDeclAccess, typeAccess,
                                isa<FileUnit>(ED->getDeclContext()));
        highlightOffendingType(TC, diag, complainRepr);
      });
    }

    return;
  }

  case DeclKind::Struct: {
    auto SD = cast<StructDecl>(D);
    checkGenericParamAccess(SD->getGenericParams(), SD);
    return;
  }

  case DeclKind::Class: {
    auto CD = cast<ClassDecl>(D);

    checkGenericParamAccess(CD->getGenericParams(), CD);

    if (CD->hasSuperclass()) {
      const NominalTypeDecl *superclassDecl =
          CD->getSuperclass()->getAnyNominal();
      // Be slightly defensive here in the presence of badly-ordered
      // inheritance clauses.
      auto superclassLocIter = std::find_if(CD->getInherited().begin(),
                                            CD->getInherited().end(),
                                            [&](TypeLoc inherited) {
        if (!inherited.wasValidated())
          return false;
        Type ty = inherited.getType();
        if (ty->is<ProtocolCompositionType>())
          if (auto superclass = ty->getExistentialLayout().explicitSuperclass)
            ty = superclass;
        return ty->getAnyNominal() == superclassDecl;
      });
      // Sanity check: we couldn't find the superclass for whatever reason
      // (possibly because it's synthetic or something), so don't bother
      // checking it.
      if (superclassLocIter == CD->getInherited().end())
        return;

      auto outerDowngradeToWarning = DowngradeToWarning::No;
      if (superclassDecl->isGenericContext() &&
          !TC.getLangOpts().isSwiftVersionAtLeast(5)) {
        // Swift 4 failed to properly check this if the superclass was generic,
        // because the above loop was too strict.
        outerDowngradeToWarning = DowngradeToWarning::Yes;
      }

      checkTypeAccess(*superclassLocIter, CD,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning) {
        auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
        bool isExplicit = CD->getAttrs().hasAttribute<AccessControlAttr>();
        auto diagID = diag::class_super_access;
        if (downgradeToWarning == DowngradeToWarning::Yes ||
            outerDowngradeToWarning == DowngradeToWarning::Yes)
          diagID = diag::class_super_access_warn;
        auto classDeclAccess = isExplicit
          ? CD->getFormalAccess()
          : typeAccessScope.requiredAccessForDiagnostics();

        auto diag = TC.diagnose(CD, diagID, isExplicit, classDeclAccess,
                                typeAccess,
                                isa<FileUnit>(CD->getDeclContext()),
                                superclassLocIter->getTypeRepr() != complainRepr);
        highlightOffendingType(TC, diag, complainRepr);
      });
    }

    return;
  }

  case DeclKind::Protocol: {
    auto proto = cast<ProtocolDecl>(D);

    // This must stay in sync with diag::protocol_access.
    enum {
      PCEK_Refine = 0,
      PCEK_Requirement
    } protocolControlErrorKind;

    auto minAccessScope = AccessScope::getPublic();
    const TypeRepr *complainRepr = nullptr;
    auto downgradeToWarning = DowngradeToWarning::No;

    std::for_each(proto->getInherited().begin(),
                  proto->getInherited().end(),
                  [&](TypeLoc requirement) {
      checkTypeAccess(requirement, proto,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *thisComplainRepr,
                          DowngradeToWarning downgradeDiag) {
        if (typeAccessScope.isChildOf(minAccessScope) ||
            (!complainRepr &&
             typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
          minAccessScope = typeAccessScope;
          complainRepr = thisComplainRepr;
          protocolControlErrorKind = PCEK_Refine;
          downgradeToWarning = downgradeDiag;
        }
      });
    });

    if (auto *trailingWhereClause = proto->getTrailingWhereClause()) {
      checkRequirementAccess(trailingWhereClause->getRequirements(),
                             proto->getFormalAccessScope(),
                             proto->getDeclContext(),
                             [&](AccessScope typeAccessScope,
                                 const TypeRepr *thisComplainRepr,
                                 DowngradeToWarning downgradeDiag) {
        if (typeAccessScope.isChildOf(minAccessScope) ||
            (!complainRepr &&
             typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
          minAccessScope = typeAccessScope;
          complainRepr = thisComplainRepr;
          protocolControlErrorKind = PCEK_Requirement;
          downgradeToWarning = downgradeDiag;

          // Swift versions before 5.0 did not check requirements on the
          // protocol's where clause, so emit a warning.
          if (!TC.Context.isSwiftVersionAtLeast(5))
            downgradeToWarning = DowngradeToWarning::Yes;
        }
      });
    }

    if (!minAccessScope.isPublic()) {
      auto minAccess = minAccessScope.accessLevelForDiagnostics();
      bool isExplicit = proto->getAttrs().hasAttribute<AccessControlAttr>();
      auto protoAccess = isExplicit
          ? proto->getFormalAccess()
          : minAccessScope.requiredAccessForDiagnostics();
      auto diagID = diag::protocol_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::protocol_access_warn;
      auto diag = TC.diagnose(proto, diagID,
                              isExplicit, protoAccess,
                              protocolControlErrorKind, minAccess,
                              isa<FileUnit>(proto->getDeclContext()));
      highlightOffendingType(TC, diag, complainRepr);
    }
    return;
  }

  case DeclKind::Subscript: {
    auto SD = cast<SubscriptDecl>(D);

    auto minAccessScope = AccessScope::getPublic();
    const TypeRepr *complainRepr = nullptr;
    auto downgradeToWarning = DowngradeToWarning::No;
    bool problemIsElement = false;

    for (auto &P : *SD->getIndices()) {
      checkTypeAccess(P->getTypeLoc(), SD,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *thisComplainRepr,
                          DowngradeToWarning downgradeDiag) {
        if (typeAccessScope.isChildOf(minAccessScope) ||
            (!complainRepr &&
             typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
          minAccessScope = typeAccessScope;
          complainRepr = thisComplainRepr;
          downgradeToWarning = downgradeDiag;
        }
      });
    }

    checkTypeAccess(SD->getElementTypeLoc(), SD,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *thisComplainRepr,
                        DowngradeToWarning downgradeDiag) {
      if (typeAccessScope.isChildOf(minAccessScope) ||
          (!complainRepr &&
           typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
        minAccessScope = typeAccessScope;
        complainRepr = thisComplainRepr;
        downgradeToWarning = downgradeDiag;
        problemIsElement = true;
      }
    });

    if (!minAccessScope.isPublic()) {
      auto minAccess = minAccessScope.accessLevelForDiagnostics();
      bool isExplicit =
        SD->getAttrs().hasAttribute<AccessControlAttr>() ||
        isa<ProtocolDecl>(SD->getDeclContext());
      auto diagID = diag::subscript_type_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::subscript_type_access_warn;
      auto subscriptDeclAccess = isExplicit
        ? SD->getFormalAccess()
        : minAccessScope.requiredAccessForDiagnostics();
      auto diag = TC.diagnose(SD, diagID,
                              isExplicit,
                              subscriptDeclAccess,
                              minAccess,
                              problemIsElement);
      highlightOffendingType(TC, diag, complainRepr);
    }
    return;
  }

  case DeclKind::Accessor:
    return;

  case DeclKind::Func:
  case DeclKind::Constructor: {
    auto fn = cast<AbstractFunctionDecl>(D);
    bool isTypeContext = fn->getDeclContext()->isTypeContext();

    checkGenericParamAccess(fn->getGenericParams(), fn);

    // This must stay in sync with diag::function_type_access.
    enum {
      FK_Function = 0,
      FK_Method,
      FK_Initializer
    };

    auto minAccessScope = AccessScope::getPublic();
    const TypeRepr *complainRepr = nullptr;
    auto downgradeToWarning = DowngradeToWarning::No;

    for (auto *P : *fn->getParameters()) {
      checkTypeAccess(P->getTypeLoc(), fn,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *thisComplainRepr,
                          DowngradeToWarning downgradeDiag) {
        if (typeAccessScope.isChildOf(minAccessScope) ||
            (!complainRepr &&
             typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
          minAccessScope = typeAccessScope;
          complainRepr = thisComplainRepr;
          downgradeToWarning = downgradeDiag;
        }
      });
    }

    bool problemIsResult = false;
    if (auto FD = dyn_cast<FuncDecl>(fn)) {
      checkTypeAccess(FD->getBodyResultTypeLoc(), FD,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *thisComplainRepr,
                          DowngradeToWarning downgradeDiag) {
        if (typeAccessScope.isChildOf(minAccessScope) ||
            (!complainRepr &&
             typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
          minAccessScope = typeAccessScope;
          complainRepr = thisComplainRepr;
          downgradeToWarning = downgradeDiag;
          problemIsResult = true;
        }
      });
    }

    if (!minAccessScope.isPublic()) {
      auto minAccess = minAccessScope.accessLevelForDiagnostics();
      auto functionKind = isa<ConstructorDecl>(fn)
        ? FK_Initializer
        : isTypeContext ? FK_Method : FK_Function;
      bool isExplicit =
        fn->getAttrs().hasAttribute<AccessControlAttr>() ||
        isa<ProtocolDecl>(fn->getDeclContext());
      auto diagID = diag::function_type_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::function_type_access_warn;
      auto fnAccess = isExplicit
        ? fn->getFormalAccess()
        : minAccessScope.requiredAccessForDiagnostics();
      auto diag = TC.diagnose(fn, diagID,
                              isExplicit,
                              fnAccess,
                              isa<FileUnit>(fn->getDeclContext()),
                              minAccess,
                              functionKind,
                              problemIsResult);
      highlightOffendingType(TC, diag, complainRepr);
    }
    return;
  }

  case DeclKind::EnumElement: {
    auto EED = cast<EnumElementDecl>(D);

    if (!EED->hasAssociatedValues())
      return;
    for (auto &P : *EED->getParameterList()) {
      checkTypeAccess(P->getTypeLoc(), EED,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning) {
        auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
        auto diagID = diag::enum_case_access;
        if (downgradeToWarning == DowngradeToWarning::Yes)
          diagID = diag::enum_case_access_warn;
        auto diag = TC.diagnose(EED, diagID,
                                EED->getFormalAccess(), typeAccess);
        highlightOffendingType(TC, diag, complainRepr);
      });
    }

    return;
  }
  }
}

/// Checks the given declaration's signature does not reference any other
/// declarations that are not @usableFromInline or public.
///
/// \p D must be a ValueDecl or a Decl that can appear in a type context.
void UsableFromInlineChecker::check(Decl *D) {
  if (!TC.Context.isSwiftVersionAtLeast(4, 2))
    return;

  if (D->isInvalid() || D->isImplicit())
    return;

  if (auto *VD = dyn_cast<ValueDecl>(D)) {
    if (VD->getFormalAccess() != AccessLevel::Internal)
      return;
    if (!VD->isUsableFromInline())
      return;
  }

  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::PrecedenceGroup:
  case DeclKind::Module:
    llvm_unreachable("cannot appear in a type context");

  case DeclKind::Param:
  case DeclKind::GenericTypeParam:
  case DeclKind::MissingMember:
    llvm_unreachable("does not have access control");

  case DeclKind::IfConfig:
  case DeclKind::PoundDiagnostic:
    // Does not have access control.
  case DeclKind::EnumCase:
    // Handled at the EnumElement level.
  case DeclKind::Var:
    // Handled at the PatternBindingDecl level.
  case DeclKind::Destructor:
    // Always correct.
    return;

  case DeclKind::PatternBinding: {
    auto PBD = cast<PatternBindingDecl>(D);
    bool isTypeContext = PBD->getDeclContext()->isTypeContext();

    llvm::DenseSet<const VarDecl *> seenVars;
    for (auto entry : PBD->getPatternList())
    entry.getPattern()->forEachNode([&](const Pattern *P) {
      if (auto *NP = dyn_cast<NamedPattern>(P)) {
        // Only check individual variables if we didn't check an enclosing
        // TypedPattern.
        const VarDecl *theVar = NP->getDecl();
        if (theVar->getFormalAccess() != AccessLevel::Internal)
          return;
        if (!theVar->isUsableFromInline())
          return;
        if (seenVars.count(theVar) || theVar->isInvalid())
          return;

        checkTypeAccess(TypeLoc::withoutLoc(theVar->getType()),
                        theVar,
                        [&](AccessScope typeAccessScope,
                            const TypeRepr *complainRepr,
                            DowngradeToWarning downgradeToWarning) {
          auto diagID = diag::pattern_type_not_usable_from_inline_inferred;
          if (!TC.Context.isSwiftVersionAtLeast(5))
            diagID = diag::pattern_type_not_usable_from_inline_inferred_warn;
          TC.diagnose(P->getLoc(),
                      diagID,
                      theVar->isLet(),
                      isTypeContext,
                      theVar->getType());
        });
        return;
      }

      auto *TP = dyn_cast<TypedPattern>(P);
      if (!TP)
        return;

      // FIXME: We need an access level to check against, so we pull one out of
      // some random VarDecl in the pattern. They're all going to be the same,
      // but still, ick.
      const VarDecl *anyVar = nullptr;
      TP->forEachVariable([&](VarDecl *V) {
        seenVars.insert(V);
        anyVar = V;
      });
      if (!anyVar)
        return;
      if (anyVar->getFormalAccess() != AccessLevel::Internal)
        return;
      if (!anyVar->isUsableFromInline())
        return;

      checkTypeAccess(TP->getTypeLoc(), anyVar,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning) {
        auto diagID = diag::pattern_type_not_usable_from_inline;
        if (!TC.Context.isSwiftVersionAtLeast(5))
          diagID = diag::pattern_type_not_usable_from_inline_warn;
        auto diag = TC.diagnose(P->getLoc(),
                                diagID,
                                anyVar->isLet(),
                                isTypeContext);
        highlightOffendingType(TC, diag, complainRepr);
      });
    });
    return;
  }

  case DeclKind::TypeAlias: {
    auto TAD = cast<TypeAliasDecl>(D);

    checkTypeAccess(TAD->getUnderlyingTypeLoc(), TAD,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *complainRepr,
                        DowngradeToWarning downgradeToWarning) {
      auto diagID = diag::type_alias_underlying_type_not_usable_from_inline;
      if (!TC.Context.isSwiftVersionAtLeast(5))
        diagID = diag::type_alias_underlying_type_not_usable_from_inline_warn;
      auto diag = TC.diagnose(TAD, diagID);
      highlightOffendingType(TC, diag, complainRepr);
    });

    return;
  }

  case DeclKind::AssociatedType: {
    auto assocType = cast<AssociatedTypeDecl>(D);

    // This must stay in sync with diag::associated_type_not_usable_from_inline.
    enum {
      ACEK_DefaultDefinition = 0,
      ACEK_Requirement
    };

    std::for_each(assocType->getInherited().begin(),
                  assocType->getInherited().end(),
                  [&](TypeLoc requirement) {
      checkTypeAccess(requirement, assocType,
                      [&](AccessScope typeAccessScope,
            const TypeRepr *complainRepr,
            DowngradeToWarning downgradeDiag) {
        auto diagID = diag::associated_type_not_usable_from_inline;
        if (!TC.Context.isSwiftVersionAtLeast(5))
          diagID = diag::associated_type_not_usable_from_inline_warn;
        auto diag = TC.diagnose(assocType, diagID, ACEK_Requirement);
        highlightOffendingType(TC, diag, complainRepr);
      });
    });
    checkTypeAccess(assocType->getDefaultDefinitionLoc(), assocType,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *complainRepr,
                        DowngradeToWarning downgradeDiag) {
      auto diagID = diag::associated_type_not_usable_from_inline;
      if (!TC.Context.isSwiftVersionAtLeast(5))
        diagID = diag::associated_type_not_usable_from_inline_warn;
      auto diag = TC.diagnose(assocType, diagID, ACEK_DefaultDefinition);
      highlightOffendingType(TC, diag, complainRepr);
    });

    if (auto *trailingWhereClause = assocType->getTrailingWhereClause()) {
      auto accessScope =
        assocType->getFormalAccessScope(nullptr);
      checkRequirementAccess(trailingWhereClause->getRequirements(),
                             accessScope,
                             assocType->getDeclContext(),
                             [&](AccessScope typeAccessScope,
                                 const TypeRepr *complainRepr,
                                 DowngradeToWarning downgradeDiag) {
        auto diagID = diag::associated_type_not_usable_from_inline;
        if (!TC.Context.isSwiftVersionAtLeast(5))
          diagID = diag::associated_type_not_usable_from_inline_warn;
        auto diag = TC.diagnose(assocType, diagID, ACEK_Requirement);
        highlightOffendingType(TC, diag, complainRepr);
      });
    }

    return;
  }

  case DeclKind::Enum: {
    auto ED = cast<EnumDecl>(D);

    checkGenericParamAccess(ED->getGenericParams(), ED);

    if (ED->hasRawType()) {
      Type rawType = ED->getRawType();
      auto rawTypeLocIter = std::find_if(ED->getInherited().begin(),
                                         ED->getInherited().end(),
                                         [&](TypeLoc inherited) {
        if (!inherited.wasValidated())
          return false;
        return inherited.getType().getPointer() == rawType.getPointer();
      });
      if (rawTypeLocIter == ED->getInherited().end())
        return;
      checkTypeAccess(*rawTypeLocIter, ED,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning) {
        auto diagID = diag::enum_raw_type_not_usable_from_inline;
        if (!TC.Context.isSwiftVersionAtLeast(5))
          diagID = diag::enum_raw_type_not_usable_from_inline_warn;
        auto diag = TC.diagnose(ED, diagID);
        highlightOffendingType(TC, diag, complainRepr);
      });
    }

    return;
  }

  case DeclKind::Struct: {
    auto SD = cast<StructDecl>(D);
    checkGenericParamAccess(SD->getGenericParams(), SD);
    return;
  }

  case DeclKind::Class: {
    auto CD = cast<ClassDecl>(D);

    checkGenericParamAccess(CD->getGenericParams(), CD);

    if (CD->hasSuperclass()) {
      const NominalTypeDecl *superclassDecl =
          CD->getSuperclass()->getAnyNominal();
      // Be slightly defensive here in the presence of badly-ordered
      // inheritance clauses.
      auto superclassLocIter = std::find_if(CD->getInherited().begin(),
                                            CD->getInherited().end(),
                                            [&](TypeLoc inherited) {
        if (!inherited.wasValidated())
          return false;
        Type ty = inherited.getType();
        if (ty->is<ProtocolCompositionType>())
          if (auto superclass = ty->getExistentialLayout().explicitSuperclass)
            ty = superclass;
        return ty->getAnyNominal() == superclassDecl;
      });
      // Sanity check: we couldn't find the superclass for whatever reason
      // (possibly because it's synthetic or something), so don't bother
      // checking it.
      if (superclassLocIter == CD->getInherited().end())
        return;

      checkTypeAccess(*superclassLocIter, CD,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning) {
        auto diagID = diag::class_super_not_usable_from_inline;
        if (!TC.Context.isSwiftVersionAtLeast(5))
          diagID = diag::class_super_not_usable_from_inline_warn;
        auto diag = TC.diagnose(CD, diagID,
                                superclassLocIter->getTypeRepr() != complainRepr);
        highlightOffendingType(TC, diag, complainRepr);
      });
    }

    return;
  }

  case DeclKind::Protocol: {
    auto proto = cast<ProtocolDecl>(D);

    // This must stay in sync with diag::protocol_usable_from_inline.
    enum {
      PCEK_Refine = 0,
      PCEK_Requirement
    };

    std::for_each(proto->getInherited().begin(),
                  proto->getInherited().end(),
                  [&](TypeLoc requirement) {
      checkTypeAccess(requirement, proto,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeDiag) {
        auto diagID = diag::protocol_usable_from_inline;
        if (!TC.Context.isSwiftVersionAtLeast(5))
          diagID = diag::protocol_usable_from_inline_warn;
        auto diag = TC.diagnose(proto, diagID, PCEK_Refine);
        highlightOffendingType(TC, diag, complainRepr);
      });
    });

    if (auto *trailingWhereClause = proto->getTrailingWhereClause()) {
      auto accessScope = proto->getFormalAccessScope(nullptr,
                                                     /*checkUsableFromInline*/true);
      checkRequirementAccess(trailingWhereClause->getRequirements(),
                             accessScope,
                             proto->getDeclContext(),
                             [&](AccessScope typeAccessScope,
                                 const TypeRepr *complainRepr,
                                 DowngradeToWarning downgradeDiag) {
        auto diagID = diag::protocol_usable_from_inline;
        if (!TC.Context.isSwiftVersionAtLeast(5))
          diagID = diag::protocol_usable_from_inline_warn;
        auto diag = TC.diagnose(proto, diagID, PCEK_Requirement);
        highlightOffendingType(TC, diag, complainRepr);
      });
    }

    return;
  }

  case DeclKind::Subscript: {
    auto SD = cast<SubscriptDecl>(D);

    for (auto &P : *SD->getIndices()) {
      checkTypeAccess(P->getTypeLoc(), SD,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeDiag) {
        auto diagID = diag::subscript_type_usable_from_inline;
        if (!TC.Context.isSwiftVersionAtLeast(5))
          diagID = diag::subscript_type_usable_from_inline_warn;
        auto diag = TC.diagnose(SD, diagID,
                                /*problemIsElement=*/false);
        highlightOffendingType(TC, diag, complainRepr);
      });
    }

    checkTypeAccess(SD->getElementTypeLoc(), SD,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *complainRepr,
                        DowngradeToWarning downgradeDiag) {
      auto diagID = diag::subscript_type_usable_from_inline;
      if (!TC.Context.isSwiftVersionAtLeast(5))
        diagID = diag::subscript_type_usable_from_inline_warn;
      auto diag = TC.diagnose(SD, diagID,
                              /*problemIsElement=*/true);
      highlightOffendingType(TC, diag, complainRepr);
    });

    return;
  }

  case DeclKind::Accessor:
    return;

  case DeclKind::Func:
  case DeclKind::Constructor: {
    auto fn = cast<AbstractFunctionDecl>(D);
    bool isTypeContext = fn->getDeclContext()->isTypeContext();

    checkGenericParamAccess(fn->getGenericParams(), fn);

    // This must stay in sync with diag::function_type_usable_from_inline.
    enum {
      FK_Function = 0,
      FK_Method,
      FK_Initializer
    };

    auto functionKind = isa<ConstructorDecl>(fn)
      ? FK_Initializer
      : isTypeContext ? FK_Method : FK_Function;

    for (auto *P : *fn->getParameters()) {
      checkTypeAccess(P->getTypeLoc(), fn,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeDiag) {
        auto diagID = diag::function_type_usable_from_inline;
        if (!TC.Context.isSwiftVersionAtLeast(5))
          diagID = diag::function_type_usable_from_inline_warn;
        auto diag = TC.diagnose(fn, diagID, functionKind,
                                /*problemIsResult=*/false);
        highlightOffendingType(TC, diag, complainRepr);
      });
    }

    if (auto FD = dyn_cast<FuncDecl>(fn)) {
      checkTypeAccess(FD->getBodyResultTypeLoc(), FD,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeDiag) {
        auto diagID = diag::function_type_usable_from_inline;
        if (!TC.Context.isSwiftVersionAtLeast(5))
          diagID = diag::function_type_usable_from_inline_warn;
        auto diag = TC.diagnose(fn, diagID, functionKind,
                                /*problemIsResult=*/true);
        highlightOffendingType(TC, diag, complainRepr);
      });
    }

    return;
  }

  case DeclKind::EnumElement: {
    auto EED = cast<EnumElementDecl>(D);

    if (!EED->hasAssociatedValues())
      return;
    for (auto &P : *EED->getParameterList()) {
      checkTypeAccess(P->getTypeLoc(), EED,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning) {
        auto diagID = diag::enum_case_usable_from_inline;
        if (!TC.Context.isSwiftVersionAtLeast(5))
          diagID = diag::enum_case_usable_from_inline_warn;
        auto diag = TC.diagnose(EED, diagID);
        highlightOffendingType(TC, diag, complainRepr);
      });
    }

    return;
  }
  }
}
