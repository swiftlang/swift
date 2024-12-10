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

#include "TypeCheckAccess.h"
#include "TypeAccessScopeChecker.h"
#include "TypeCheckAvailability.h"
#include "TypeChecker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Import.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclObjC.h"

using namespace swift;

#define DEBUG_TYPE "TypeCheckAccess"

namespace {

/// Calls \p callback for each type in each requirement provided by
/// \p source.
static void forAllRequirementTypes(
    WhereClauseOwner &&source,
    llvm::function_ref<void(Type, TypeRepr *)> callback) {
  std::move(source).visitRequirements(TypeResolutionStage::Interface,
      [&](const Requirement &req, RequirementRepr *reqRepr) {
    switch (req.getKind()) {
    case RequirementKind::SameShape:
    case RequirementKind::Conformance:
    case RequirementKind::SameType:
    case RequirementKind::Superclass:
      callback(req.getFirstType(),
               RequirementRepr::getFirstTypeRepr(reqRepr));
      callback(req.getSecondType(),
               RequirementRepr::getSecondTypeRepr(reqRepr));
      break;

    case RequirementKind::Layout:
      callback(req.getFirstType(),
               RequirementRepr::getFirstTypeRepr(reqRepr));
      break;
    }
    return false;
  });
}

/// \see checkTypeAccess
using CheckTypeAccessCallback =
    void(AccessScope, const TypeRepr *, DowngradeToWarning, ImportAccessLevel);

class AccessControlCheckerBase {
protected:
  bool checkUsableFromInline;

  void checkTypeAccessImpl(
      Type type, TypeRepr *typeRepr, AccessScope contextAccessScope,
      const DeclContext *useDC, bool mayBeInferred,
      llvm::function_ref<CheckTypeAccessCallback> diagnose);

  void checkTypeAccess(
      Type type, TypeRepr *typeRepr, const ValueDecl *context,
      bool mayBeInferred,
      llvm::function_ref<CheckTypeAccessCallback> diagnose);

  void checkTypeAccess(
      const TypeLoc &TL, const ValueDecl *context, bool mayBeInferred,
      llvm::function_ref<CheckTypeAccessCallback> diagnose) {
    return checkTypeAccess(TL.getType(), TL.getTypeRepr(), context,
                           mayBeInferred, diagnose);
  }

  void checkRequirementAccess(
      WhereClauseOwner &&source,
      AccessScope accessScope,
      const DeclContext *useDC,
      llvm::function_ref<CheckTypeAccessCallback> diagnose) {
    forAllRequirementTypes(std::move(source), [&](Type type, TypeRepr *typeRepr) {
      checkTypeAccessImpl(type, typeRepr, accessScope, useDC,
                          /*mayBeInferred*/false, diagnose);
    });
  }

  AccessControlCheckerBase(bool checkUsableFromInline)
      : checkUsableFromInline(checkUsableFromInline) {}

public:
  void checkGenericParamAccess(
    const GenericContext *ownerCtx,
    const Decl *ownerDecl,
    AccessScope accessScope,
    AccessLevel contextAccess);

  void checkGenericParamAccess(
    const GenericContext *ownerCtx,
    const ValueDecl *ownerDecl);

  void checkGlobalActorAccess(const Decl *D);
};

} // end anonymous namespace

/// Searches the given type representation for a `DeclRefTypeRepr` that is
/// bound to a type declaration with the given access scope. The type
/// representation is searched in source order. For example, nodes in
/// `A<T>.B<U>` will be checked in the following order: `ATBU`.
static const DeclRefTypeRepr *
findTypeDeclWithAccessScope(TypeRepr *TR, AccessScope accessScope,
                            const DeclContext *useDC,
                            bool treatUsableFromInlineAsPublic) {
  if (!TR) {
    return nullptr;
  }

  class Finder : public ASTWalker {
    AccessScope accessScope;
    const DeclContext *useDC;
    bool treatUsableFromInlineAsPublic;

    const DeclRefTypeRepr *theFind;

  public:
    explicit Finder(AccessScope accessScope, const DeclContext *useDC,
                    bool treatUsableFromInlineAsPublic)
        : accessScope(accessScope), useDC(useDC),
          treatUsableFromInlineAsPublic(treatUsableFromInlineAsPublic),
          theFind(nullptr) {}

    const DeclRefTypeRepr *getFind() const { return theFind; }

    MacroWalking getMacroWalkingBehavior() const override {
      return MacroWalking::ArgumentsAndExpansion;
    }

    QualifiedIdentTypeReprWalkingScheme
    getQualifiedIdentTypeReprWalkingScheme() const override {
      return QualifiedIdentTypeReprWalkingScheme::SourceOrderRecursive;
    }

    PreWalkAction walkToTypeReprPre(TypeRepr *TR) override {
      auto *declRefTR = dyn_cast<DeclRefTypeRepr>(TR);
      if (!declRefTR)
        return Action::Continue();

      const ValueDecl *VD = declRefTR->getBoundDecl();
      if (!VD)
        return Action::Continue();

      if (VD->getFormalAccessScope(useDC, treatUsableFromInlineAsPublic) !=
          accessScope)
        return Action::Continue();

      theFind = declRefTR;
      return Action::Stop();
    }
  } walker(accessScope, useDC, treatUsableFromInlineAsPublic);

  TR->walk(walker);

  return walker.getFind();
}

/// Checks if the access scope of the type described by \p TL contains
/// \p contextAccessScope. If it isn't, calls \p diagnose with a TypeRepr
/// representing the offending part of \p TL.
///
/// The TypeRepr passed to \p diagnose may be null, in which case a particular
/// part of the type that caused the problem could not be found. The DeclContext
/// is never null.
///
/// If \p type might be partially inferred even when \p typeRepr is present
/// (such as for properties), pass \c true for \p mayBeInferred. (This does not
/// include implicitly providing generic parameters for the Self type, such as
/// using `Array` to mean `Array<Element>` in an extension of Array.) If
/// \p typeRepr is known to be absent, it's okay to pass \c false for
/// \p mayBeInferred.
void AccessControlCheckerBase::checkTypeAccessImpl(
    Type type, TypeRepr *typeRepr, AccessScope contextAccessScope,
    const DeclContext *useDC, bool mayBeInferred,
    llvm::function_ref<CheckTypeAccessCallback> diagnose) {

  auto &Context = useDC->getASTContext();
  if (Context.isAccessControlDisabled())
    return;
  // Don't spend time checking local declarations; this is always valid by the
  // time we get to this point.
  if (contextAccessScope.isInContext() &&
      contextAccessScope.getDeclContext()->isLocalContext())
    return;

  // Report where it was imported from.
  if (contextAccessScope.isPublicOrPackage()) {
    auto report = [&](const DeclRefTypeRepr *typeRepr, const ValueDecl *VD) {
      // Remember that the module defining the decl must be imported publicly.
      recordRequiredImportAccessLevelForDecl(
          VD, useDC, contextAccessScope.accessLevelForDiagnostics(),
          [&](AttributedImport<ImportedModule> attributedImport) {
            SourceLoc diagLoc =
                typeRepr ? typeRepr->getLoc() : extractNearestSourceLoc(useDC);
            ModuleDecl *importedVia = attributedImport.module.importedModule,
                       *sourceModule = VD->getModuleContext();
            Context.Diags.diagnose(diagLoc, diag::module_api_import, VD,
                                   importedVia, sourceModule,
                                   importedVia == sourceModule,
                                   /*isImplicit*/ !typeRepr);
          });
    };

    if (typeRepr) {
      typeRepr->walk(DeclRefTypeReprFinder([&](const DeclRefTypeRepr *TR) {
        const ValueDecl *VD = TR->getBoundDecl();
        report(TR, VD);
        return true;
      }));
    } else if (type) {
      type.walk(SimpleTypeDeclFinder([&](const ValueDecl *VD) {
        report(/*typeRepr=*/nullptr, VD);
        return TypeWalker::Action::Continue;
      }));
    }
  };

  AccessScope problematicAccessScope = AccessScope::getPublic();

  if (type) {
    std::optional<AccessScope> typeAccessScope =
        TypeAccessScopeChecker::getAccessScope(type, useDC,
                                               checkUsableFromInline);

    // Note: This means that the type itself is invalid for this particular
    // context, because it references declarations from two incompatible scopes.
    // In this case we should have diagnosed the bad reference already.
    if (!typeAccessScope.has_value())
      return;

    problematicAccessScope = *typeAccessScope;
  }

  auto downgradeToWarning = DowngradeToWarning::No;

  // Check if type can be referenced in this context.
  // - hasEqualDeclContextWith checks for matching scopes (public to public,
  //   internal to the same module, private to same scope, etc.)
  // - isChildOf checks for use of public in internal, etc.
  if (contextAccessScope.hasEqualDeclContextWith(problematicAccessScope) ||
      contextAccessScope.isChildOf(problematicAccessScope)) {

    // /Also/ check the TypeRepr, if present. This can be important when we're
    // unable to preserve typealias sugar that's present in the TypeRepr.
    if (!typeRepr)
      return;

    auto typeReprAccessScope =
        TypeAccessScopeChecker::getAccessScope(typeRepr, useDC,
                                               checkUsableFromInline);
    if (!typeReprAccessScope.has_value())
      return;

    if (contextAccessScope.hasEqualDeclContextWith(*typeReprAccessScope) ||
        contextAccessScope.isChildOf(*typeReprAccessScope)) {
      // Only if both the Type and the TypeRepr follow the access rules can
      // we exit; otherwise we have to emit a diagnostic.
      return;
    }
    problematicAccessScope = *typeReprAccessScope;
  } else {
    // The type violates the rules of access control (with or without taking the
    // TypeRepr into account).

    if (typeRepr && mayBeInferred &&
        !Context.LangOpts.isSwiftVersionAtLeast(5) &&
        !useDC->getParentModule()->isResilient()) {
      // Swift 4.2 and earlier didn't check the Type when a TypeRepr was
      // present. However, this is a major hole when generic parameters are
      // inferred:
      //
      //   public let foo: Optional = VeryPrivateStruct()
      //
      // Downgrade the error to a warning in this case for source compatibility.
      std::optional<AccessScope> typeReprAccessScope =
          TypeAccessScopeChecker::getAccessScope(typeRepr, useDC,
                                                 checkUsableFromInline);
      assert(typeReprAccessScope && "valid Type but not valid TypeRepr?");
      if (contextAccessScope.hasEqualDeclContextWith(*typeReprAccessScope) ||
          contextAccessScope.isChildOf(*typeReprAccessScope)) {
        downgradeToWarning = DowngradeToWarning::Yes;
      }
    }
  }

  const DeclRefTypeRepr *complainRepr = findTypeDeclWithAccessScope(
      typeRepr, problematicAccessScope, useDC, checkUsableFromInline);

  ImportAccessLevel complainImport = std::nullopt;
  if (complainRepr) {
    const ValueDecl *VD = complainRepr->getBoundDecl();
    assert(VD &&
           "findTypeDeclWithAccessScope should return bound TypeReprs only");
    complainImport = VD->getImportAccessFrom(useDC);

    // Don't complain about an import that doesn't restrict the access
    // level of the decl. This can happen with imported `package` decls.
    if (complainImport.has_value() &&
        complainImport->accessLevel >= VD->getFormalAccess())
      complainImport = std::nullopt;
  }

  diagnose(problematicAccessScope, complainRepr, downgradeToWarning,
           complainImport);
}

/// Checks if the access scope of the type described by \p TL is valid for the
/// type to be the type of \p context. If it isn't, calls \p diagnose with a
/// TypeRepr representing the offending part of \p TL.
///
/// The TypeRepr passed to \p diagnose may be null, in which case a particular
/// part of the type that caused the problem could not be found.
///
/// If \p type might be partially inferred even when \p typeRepr is present
/// (such as for properties), pass \c true for \p mayBeInferred. (This does not
/// include implicitly providing generic parameters for the Self type, such as
/// using `Array` to mean `Array<Element>` in an extension of Array.) If
/// \p typeRepr is known to be absent, it's okay to pass \c false for
/// \p mayBeInferred.
void AccessControlCheckerBase::checkTypeAccess(
    Type type, TypeRepr *typeRepr, const ValueDecl *context, bool mayBeInferred,
    llvm::function_ref<CheckTypeAccessCallback> diagnose) {
  assert(!isa<ParamDecl>(context));
  const DeclContext *DC = context->getDeclContext();
  AccessScope contextAccessScope =
    context->getFormalAccessScope(
      context->getDeclContext(), checkUsableFromInline);

  checkTypeAccessImpl(type, typeRepr, contextAccessScope, DC, mayBeInferred,
                      diagnose);
}

/// Highlights the given TypeRepr, and adds a note pointing to the type's
/// declaration if possible.
///
/// Just flushes \p diag as is if \p complainRepr is null.
static void highlightOffendingType(InFlightDiagnostic &diag,
                                   const TypeRepr *complainRepr) {
  if (!complainRepr) {
    diag.flush();
    return;
  }

  diag.highlight(complainRepr->getSourceRange());
  diag.flush();

  if (auto *declRefTR = dyn_cast<DeclRefTypeRepr>(complainRepr)) {
    const ValueDecl *VD = declRefTR->getBoundDecl();
    VD->diagnose(diag::kind_declared_here, DescriptiveDeclKind::Type);
  }
}

/// Emit a note on \p limitImport when it restricted the access level
/// of a type.
static void noteLimitingImport(const Decl *userDecl, ASTContext &ctx,
                               const ImportAccessLevel limitImport,
                               const Decl *complainDecl) {
  if (!limitImport.has_value())
    return;

  assert(limitImport->accessLevel != AccessLevel::Public &&
         "a public import shouldn't limit the access level of a decl");

  if (complainDecl) {
    // When using an IDE in a large file the decl_import_via_here note
    // may be easy to miss on the import. Duplicate the information on the
    // error line as well so it can't be missed.
    if (userDecl)
      userDecl->diagnose(diag::decl_import_via_local, complainDecl,
                         limitImport->accessLevel,
                         limitImport->module.importedModule);

    if (limitImport->importLoc.isValid())
      ctx.Diags.diagnose(limitImport->importLoc, diag::decl_import_via_here,
                         complainDecl, limitImport->accessLevel,
                         limitImport->module.importedModule);
  } else if (limitImport->importLoc.isValid()) {
    ctx.Diags.diagnose(limitImport->importLoc, diag::module_imported_here,
                       limitImport->module.importedModule,
                       limitImport->accessLevel);
  }
}

static void noteLimitingImport(const Decl *userDecl, ASTContext &ctx,
                               const ImportAccessLevel limitImport,
                               const TypeRepr *complainRepr) {
  const Decl *complainDecl = nullptr;
  if (auto *declRefTR = dyn_cast_or_null<DeclRefTypeRepr>(complainRepr))
    complainDecl = declRefTR->getBoundDecl();

  noteLimitingImport(userDecl, ctx, limitImport, complainDecl);
}

static void noteLimitingImport(const Decl *userDecl,
                               const ImportAccessLevel limitImport,
                               const TypeRepr *complainRepr) {
  noteLimitingImport(userDecl, userDecl->getASTContext(), limitImport,
                     complainRepr);
}

void AccessControlCheckerBase::checkGenericParamAccess(
    const GenericContext *ownerCtx,
    const Decl *ownerDecl,
    AccessScope accessScope,
    AccessLevel contextAccess) {
  if (!ownerCtx->isGenericContext())
    return;

 // This must stay in sync with diag::generic_param_access.
  enum class ACEK {
    Parameter = 0,
    Requirement
  } accessControlErrorKind;
  auto minAccessScope = AccessScope::getPublic();
  const TypeRepr *complainRepr = nullptr;
  auto downgradeToWarning = DowngradeToWarning::Yes;
  ImportAccessLevel minImportLimit = std::nullopt;

  auto callbackACEK = ACEK::Parameter;

  auto callback = [&](AccessScope typeAccessScope,
                      const TypeRepr *thisComplainRepr,
                      DowngradeToWarning thisDowngrade,
                      ImportAccessLevel importLimit) {
    if (typeAccessScope.isChildOf(minAccessScope) ||
        (thisDowngrade == DowngradeToWarning::No &&
         downgradeToWarning == DowngradeToWarning::Yes) ||
        (!complainRepr &&
         typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
      minAccessScope = typeAccessScope;
      complainRepr = thisComplainRepr;
      accessControlErrorKind = callbackACEK;
      downgradeToWarning = thisDowngrade;
      minImportLimit = importLimit;
    }
  };

  auto *DC = ownerDecl->getDeclContext();

  if (auto params = ownerCtx->getGenericParams()) {
    for (auto param : *params) {
      auto inheritedEntries = param->getInherited().getEntries();
      if (inheritedEntries.empty())
        continue;
      assert(inheritedEntries.size() == 1);
      checkTypeAccessImpl(inheritedEntries.front().getType(),
                          inheritedEntries.front().getTypeRepr(), accessScope,
                          DC, /*mayBeInferred*/ false, callback);
    }
  }

  callbackACEK = ACEK::Requirement;

  if (ownerCtx->getTrailingWhereClause()) {
    checkRequirementAccess(WhereClauseOwner(
                             const_cast<GenericContext *>(ownerCtx)),
                           accessScope, DC, callback);
  }

  if (minAccessScope.isPublic())
    return;

  // FIXME: Promote these to an error in the next -swift-version break.
  if (isa<SubscriptDecl>(ownerDecl) || isa<TypeAliasDecl>(ownerDecl))
    downgradeToWarning = DowngradeToWarning::Yes;

  auto &Context = ownerDecl->getASTContext();
  if (checkUsableFromInline) {
    if (!Context.isSwiftVersionAtLeast(5))
      downgradeToWarning = DowngradeToWarning::Yes;

    auto diagID = diag::generic_param_usable_from_inline;
    if (downgradeToWarning == DowngradeToWarning::Yes)
      diagID = diag::generic_param_usable_from_inline_warn;
    auto diag =
        Context.Diags.diagnose(ownerDecl, diagID, ownerDecl->getDescriptiveKind(),
                               accessControlErrorKind == ACEK::Requirement);
    highlightOffendingType(diag, complainRepr);
    noteLimitingImport(/*userDecl*/nullptr, Context, minImportLimit, complainRepr);
    return;
  }

  auto minAccess = minAccessScope.accessLevelForDiagnostics();

  bool isExplicit =
    ownerDecl->getAttrs().hasAttribute<AccessControlAttr>() ||
    isa<ProtocolDecl>(DC);
  auto diagID = diag::generic_param_access;
  if (downgradeToWarning == DowngradeToWarning::Yes)
    diagID = diag::generic_param_access_warn;
  auto diag = Context.Diags.diagnose(
      ownerDecl, diagID, ownerDecl->getDescriptiveKind(), isExplicit,
      contextAccess, minAccess, isa<FileUnit>(DC),
      accessControlErrorKind == ACEK::Requirement);
  highlightOffendingType(diag, complainRepr);
  noteLimitingImport(/*userDecl*/nullptr, Context, minImportLimit, complainRepr);
}

void AccessControlCheckerBase::checkGenericParamAccess(
    const GenericContext *ownerCtx,
    const ValueDecl *ownerDecl) {
  checkGenericParamAccess(ownerCtx, ownerDecl,
                          ownerDecl->getFormalAccessScope(
                              nullptr, checkUsableFromInline),
                          ownerDecl->getFormalAccess());
}

void AccessControlCheckerBase::checkGlobalActorAccess(const Decl *D) {
  auto VD = dyn_cast<ValueDecl>(D);
  if (!VD)
    return;

  auto globalActorAttr = D->getGlobalActorAttr();
  if (!globalActorAttr)
    return;

  auto customAttr = globalActorAttr->first;
  auto globalActorDecl = globalActorAttr->second;
  checkTypeAccess(
      customAttr->getType(), customAttr->getTypeRepr(), VD,
      /*mayBeInferred*/ false,
      [&](AccessScope typeAccessScope, const TypeRepr *complainRepr,
          DowngradeToWarning downgradeToWarning, ImportAccessLevel importLimit) {
        if (checkUsableFromInline) {
          auto diag = D->diagnose(diag::global_actor_not_usable_from_inline,
                                  VD);
          highlightOffendingType(diag, complainRepr);
          noteLimitingImport(D, importLimit, complainRepr);
          return;
        }

        auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
        bool isExplicit = D->getAttrs().hasAttribute<AccessControlAttr>();
        auto declAccess = isExplicit
                              ? VD->getFormalAccess()
                              : typeAccessScope.requiredAccessForDiagnostics();
        auto diag = D->diagnose(diag::global_actor_access, declAccess, VD,
                                typeAccess, globalActorDecl->getName());
        highlightOffendingType(diag, complainRepr);
        noteLimitingImport(D, importLimit, complainRepr);
      });
}

namespace {
class AccessControlChecker : public AccessControlCheckerBase,
                             public DeclVisitor<AccessControlChecker> {
public:

  AccessControlChecker(bool allowUsableFromInline)
    : AccessControlCheckerBase(allowUsableFromInline) {}

  AccessControlChecker()
      : AccessControlCheckerBase(/*checkUsableFromInline=*/false) {}

  void visit(Decl *D) {
    if (D->isInvalid() || D->isImplicit())
      return;

    DeclVisitor<AccessControlChecker>::visit(D);
    checkGlobalActorAccess(D);
    checkAttachedMacrosAccess(D);
  }

  // Force all kinds to be handled at a lower level.
  void visitDecl(Decl *D) = delete;
  void visitValueDecl(ValueDecl *D) = delete;

#define UNREACHABLE(KIND, REASON) \
  void visit##KIND##Decl(KIND##Decl *D) { \
    llvm_unreachable(REASON); \
  }
  UNREACHABLE(Import, "cannot appear in a type context")
  UNREACHABLE(Extension, "cannot appear in a type context")
  UNREACHABLE(TopLevelCode, "cannot appear in a type context")
  UNREACHABLE(Operator, "cannot appear in a type context")
  UNREACHABLE(PrecedenceGroup, "cannot appear in a type context")
  UNREACHABLE(Module, "cannot appear in a type context")

  UNREACHABLE(PoundDiagnostic, "does not have access control")
  UNREACHABLE(Param, "does not have access control")
  UNREACHABLE(GenericTypeParam, "does not have access control")
  UNREACHABLE(Missing, "does not have access control")
  UNREACHABLE(MissingMember, "does not have access control")
  UNREACHABLE(MacroExpansion, "does not have access control")

  UNREACHABLE(BuiltinTuple, "BuiltinTupleDecl should not show up here")

#undef UNREACHABLE

#define UNINTERESTING(KIND) \
  void visit##KIND##Decl(KIND##Decl *D) {}

  UNINTERESTING(EnumCase) // Handled at the EnumElement level.
  UNINTERESTING(Var) // Handled at the PatternBinding level.
  UNINTERESTING(Destructor) // Always correct.
  UNINTERESTING(Accessor) // Handled by the Var or Subscript.

  /// \see visitPatternBindingDecl
  void checkNamedPattern(const NamedPattern *NP, bool isTypeContext,
                         const llvm::DenseSet<const VarDecl *> &seenVars) {
    const VarDecl *theVar = NP->getDecl();
    if (seenVars.count(theVar) || theVar->isInvalid())
      return;

    Type interfaceType = theVar->getInterfaceType();
    checkTypeAccess(interfaceType, nullptr, theVar,
                    /*mayBeInferred*/false,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *complainRepr,
                        DowngradeToWarning downgradeToWarning,
                        ImportAccessLevel importLimit) {
      auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
      bool isExplicit = theVar->getAttrs().hasAttribute<AccessControlAttr>() ||
                        isa<ProtocolDecl>(theVar->getDeclContext());
      auto theVarAccess =
          isExplicit ? theVar->getFormalAccess()
                     : typeAccessScope.requiredAccessForDiagnostics();
      auto diagID = diag::pattern_type_access_inferred;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::pattern_type_access_inferred_warn;
      auto &DE = theVar->getASTContext().Diags;
      DE.diagnose(NP->getLoc(), diagID, theVar->isLet(),
                  isTypeContext, isExplicit, theVarAccess,
                  isa<FileUnit>(theVar->getDeclContext()),
                  typeAccess, interfaceType);

      // As we pass in a null typeRepr the complainRepr will always be null.
      // Extract the module import from the interface type instead.
      const Decl *complainDecl = nullptr;
      ImportAccessLevel complainImport = std::nullopt;
      interfaceType.walk(SimpleTypeDeclFinder([&](const ValueDecl *VD) {
        ImportAccessLevel import = VD->getImportAccessFrom(theVar->getDeclContext());
        if (import.has_value() && import->accessLevel < VD->getFormalAccess()) {
          complainDecl = VD;
          complainImport = import;
          return TypeWalker::Action::Stop;
        }
        return TypeWalker::Action::Continue;
      }));

      noteLimitingImport(theVar, theVar->getASTContext(), complainImport,
                         complainDecl);
    });
  }

  void checkTypedPattern(const TypedPattern *TP, bool isTypeContext,
                         llvm::DenseSet<const VarDecl *> &seenVars) {
    VarDecl *anyVar = nullptr;
    TP->forEachVariable([&](VarDecl *V) {
      seenVars.insert(V);
      anyVar = V;
    });
    if (!anyVar)
      return;

    checkTypeAccess(TP->hasType() ? TP->getType() : Type(),
                    TP->getTypeRepr(), anyVar, /*mayBeInferred*/true,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *complainRepr,
                        DowngradeToWarning downgradeToWarning,
                        ImportAccessLevel importLimit) {
      auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
      bool isExplicit = anyVar->getAttrs().hasAttribute<AccessControlAttr>() ||
                        isa<ProtocolDecl>(anyVar->getDeclContext());
      auto diagID = diag::pattern_type_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::pattern_type_access_warn;
      auto anyVarAccess =
          isExplicit ? anyVar->getFormalAccess()
                     : typeAccessScope.requiredAccessForDiagnostics();
      auto &DE = anyVar->getASTContext().Diags;
      auto diag = DE.diagnose(
          TP->getLoc(), diagID, anyVar->isLet(), isTypeContext, isExplicit,
          anyVarAccess, isa<FileUnit>(anyVar->getDeclContext()), typeAccess);
      highlightOffendingType(diag, complainRepr);
      noteLimitingImport(anyVar, importLimit, complainRepr);
    });

    // Check the property wrapper types.
    for (auto attr : anyVar->getAttachedPropertyWrappers()) {
      checkTypeAccess(attr->getType(), attr->getTypeRepr(), anyVar,
                      /*mayBeInferred=*/false,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning,
                          ImportAccessLevel importLimit) {
        auto typeAccess = typeAccessScope.accessLevelForDiagnostics();

        bool isExplicit =
            anyVar->getAttrs().hasAttribute<AccessControlAttr>() ||
            isa<ProtocolDecl>(anyVar->getDeclContext());
        auto anyVarAccess =
            isExplicit ? anyVar->getFormalAccess()
                       : typeAccessScope.requiredAccessForDiagnostics();
        auto diag = anyVar->diagnose(diag::property_wrapper_type_access,
                                     anyVar->isLet(),
                                     isTypeContext,
                                     isExplicit,
                                     anyVarAccess,
                                     isa<FileUnit>(anyVar->getDeclContext()),
                                     typeAccess);
        highlightOffendingType(diag, complainRepr);
        noteLimitingImport(anyVar, importLimit, complainRepr);
      });
    }
  }

  void visitPatternBindingDecl(PatternBindingDecl *PBD) {
    bool isTypeContext = PBD->getDeclContext()->isTypeContext();

    llvm::DenseSet<const VarDecl *> seenVars;
    for (auto idx : range(PBD->getNumPatternEntries())) {
      PBD->getPattern(idx)->forEachNode([&](const Pattern *P) {
        if (auto *NP = dyn_cast<NamedPattern>(P)) {
          // Only check individual variables if we didn't check an enclosing
          // TypedPattern.
          checkNamedPattern(NP, isTypeContext, seenVars);
          return;
        }

        auto *TP = dyn_cast<TypedPattern>(P);
        if (!TP)
          return;
        checkTypedPattern(TP, isTypeContext, seenVars);
      });
      seenVars.clear();
    }
  }

  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    checkGenericParamAccess(TAD, TAD);

    checkTypeAccess(TAD->getUnderlyingType(),
                    TAD->getUnderlyingTypeRepr(), TAD, /*mayBeInferred*/false,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *complainRepr,
                        DowngradeToWarning downgradeToWarning,
                        ImportAccessLevel importLimit) {
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
      auto diag = TAD->diagnose(diagID, isExplicit, aliasAccess, typeAccess,
                                isa<FileUnit>(TAD->getDeclContext()));
      highlightOffendingType(diag, complainRepr);
      noteLimitingImport(TAD, importLimit, complainRepr);
    });
  }

  void visitOpaqueTypeDecl(OpaqueTypeDecl *OTD) {
    // TODO(opaque): The constraint class/protocols on the opaque interface, as
    // well as the naming decl for the opaque type, need to be accessible.
  }

  void visitAssociatedTypeDecl(AssociatedTypeDecl *assocType) {
    // This must stay in sync with diag::associated_type_access.
    enum {
      ACEK_DefaultDefinition = 0,
      ACEK_Requirement
    } accessControlErrorKind;
    auto minAccessScope = AccessScope::getPublic();
    const TypeRepr *complainRepr = nullptr;
    auto downgradeToWarning = DowngradeToWarning::No;
    ImportAccessLevel minImportLimit = std::nullopt;

    for (TypeLoc requirement : assocType->getInherited().getEntries()) {
      checkTypeAccess(requirement, assocType, /*mayBeInferred*/false,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *thisComplainRepr,
                          DowngradeToWarning downgradeDiag,
                          ImportAccessLevel importLimit) {
        if (typeAccessScope.isChildOf(minAccessScope) ||
            (!complainRepr &&
             typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
          minAccessScope = typeAccessScope;
          complainRepr = thisComplainRepr;
          accessControlErrorKind = ACEK_Requirement;
          downgradeToWarning = downgradeDiag;
          minImportLimit = importLimit;
        }
      });
    }
    checkTypeAccess(assocType->getDefaultDefinitionType(),
                    assocType->getDefaultDefinitionTypeRepr(), assocType,
                    /*mayBeInferred*/false,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *thisComplainRepr,
                        DowngradeToWarning downgradeDiag,
                        ImportAccessLevel importLimit) {
      if (typeAccessScope.isChildOf(minAccessScope) ||
          (!complainRepr &&
           typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
        minAccessScope = typeAccessScope;
        complainRepr = thisComplainRepr;
        accessControlErrorKind = ACEK_DefaultDefinition;
        downgradeToWarning = downgradeDiag;
        minImportLimit = importLimit;
      }
    });

    checkRequirementAccess(assocType,
                           assocType->getFormalAccessScope(),
                           assocType->getDeclContext(),
                           [&](AccessScope typeAccessScope,
                               const TypeRepr *thisComplainRepr,
                               DowngradeToWarning downgradeDiag,
                               ImportAccessLevel importLimit) {
      if (typeAccessScope.isChildOf(minAccessScope) ||
          (!complainRepr &&
           typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
        minAccessScope = typeAccessScope;
        complainRepr = thisComplainRepr;
        accessControlErrorKind = ACEK_Requirement;
        downgradeToWarning = downgradeDiag;
        minImportLimit = importLimit;

        // Swift versions before 5.0 did not check requirements on the
        // protocol's where clause, so emit a warning.
        if (!assocType->getASTContext().isSwiftVersionAtLeast(5))
          downgradeToWarning = DowngradeToWarning::Yes;
      }
    });

    if (!minAccessScope.isPublic()) {
      auto minAccess = minAccessScope.accessLevelForDiagnostics();
      auto diagID = diag::associated_type_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::associated_type_access_warn;
      auto diag = assocType->diagnose(diagID, assocType->getFormalAccess(),
                                      minAccess, accessControlErrorKind);
      highlightOffendingType(diag, complainRepr);
      noteLimitingImport(assocType, minImportLimit, complainRepr);
    }
  }

  void visitEnumDecl(EnumDecl *ED) {
    checkGenericParamAccess(ED, ED);

    if (ED->hasRawType()) {
      Type rawType = ED->getRawType();
      auto inheritedEntries = ED->getInherited().getEntries();
      auto rawTypeLocIter = std::find_if(inheritedEntries.begin(),
                                         inheritedEntries.end(),
                                         [&](TypeLoc inherited) {
        if (!inherited.wasValidated())
          return false;
        return inherited.getType().getPointer() == rawType.getPointer();
      });
      if (rawTypeLocIter == inheritedEntries.end())
        return;
      checkTypeAccess(rawType, rawTypeLocIter->getTypeRepr(), ED,
                      /*mayBeInferred*/false,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning,
                          ImportAccessLevel importLimit) {
        auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
        bool isExplicit = ED->getAttrs().hasAttribute<AccessControlAttr>();
        auto diagID = diag::enum_raw_type_access;
        if (downgradeToWarning == DowngradeToWarning::Yes)
          diagID = diag::enum_raw_type_access_warn;
        auto enumDeclAccess = isExplicit
          ? ED->getFormalAccess()
          : typeAccessScope.requiredAccessForDiagnostics();
        auto diag = ED->diagnose(diagID, isExplicit, enumDeclAccess, typeAccess,
                                 isa<FileUnit>(ED->getDeclContext()));
        highlightOffendingType(diag, complainRepr);
        noteLimitingImport(ED, importLimit, complainRepr);
      });
    }
  }

  void visitStructDecl(StructDecl *SD) {
    checkGenericParamAccess(SD, SD);
  }

  void visitClassDecl(ClassDecl *CD) {
    checkGenericParamAccess(CD, CD);

    if (const NominalTypeDecl *superclassDecl = CD->getSuperclassDecl()) {
      // Be slightly defensive here in the presence of badly-ordered
      // inheritance clauses.
      auto inheritedEntries = CD->getInherited().getEntries();
      auto superclassLocIter = std::find_if(inheritedEntries.begin(),
                                            inheritedEntries.end(),
                                            [&](TypeLoc inherited) {
        if (!inherited.wasValidated())
          return false;
        Type ty = inherited.getType();
        if (ty->is<ProtocolCompositionType>())
          if (auto superclass = ty->getExistentialLayout().explicitSuperclass)
            ty = superclass;
        return ty->getAnyNominal() == superclassDecl;
      });
      // Soundness check: we couldn't find the superclass for whatever reason
      // (possibly because it's synthetic or something), so don't bother
      // checking it.
      if (superclassLocIter == inheritedEntries.end())
        return;

      auto outerDowngradeToWarning = DowngradeToWarning::No;
      if (superclassDecl->isGenericContext() &&
          !CD->getASTContext().isSwiftVersionAtLeast(5)) {
        // Swift 4 failed to properly check this if the superclass was generic,
        // because the above loop was too strict.
        outerDowngradeToWarning = DowngradeToWarning::Yes;
      }

      checkTypeAccess(CD->getSuperclass(), superclassLocIter->getTypeRepr(), CD,
                      /*mayBeInferred*/false,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning,
                          ImportAccessLevel importLimit) {
        auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
        bool isExplicit = CD->getAttrs().hasAttribute<AccessControlAttr>();
        auto diagID = diag::class_super_access;
        if (downgradeToWarning == DowngradeToWarning::Yes ||
            outerDowngradeToWarning == DowngradeToWarning::Yes)
          diagID = diag::class_super_access_warn;
        auto classDeclAccess = isExplicit
          ? CD->getFormalAccess()
          : typeAccessScope.requiredAccessForDiagnostics();

        auto diag =
            CD->diagnose(diagID, isExplicit, classDeclAccess, typeAccess,
                         isa<FileUnit>(CD->getDeclContext()),
                         superclassLocIter->getTypeRepr() != complainRepr);
        highlightOffendingType(diag, complainRepr);
        noteLimitingImport(CD, importLimit, complainRepr);
      });
    }
  }

  void visitProtocolDecl(ProtocolDecl *proto) {
    // This must stay in sync with diag::protocol_access.
    enum {
      PCEK_Refine = 0,
      PCEK_Requirement
    } protocolControlErrorKind;

    auto minAccessScope = AccessScope::getPublic();
    const TypeRepr *complainRepr = nullptr;
    auto downgradeToWarning = DowngradeToWarning::No;
    ImportAccessLevel minImportLimit = std::nullopt;
    DescriptiveDeclKind declKind = DescriptiveDeclKind::Protocol;

    // FIXME: Hack to ensure that we've computed the types involved here.
    auto inheritedTypes = proto->getInherited();
    for (auto i : inheritedTypes.getIndices()) {
      (void)inheritedTypes.getResolvedType(i);
    }

    auto declKindForType = [](Type type) -> DescriptiveDeclKind {
      // If this is an existential type, use the decl kind of
      // its constraint type.
      if (auto existential = type->getAs<ExistentialType>())
        type = existential->getConstraintType();

      if (isa<TypeAliasType>(type.getPointer()))
        return DescriptiveDeclKind::TypeAlias;
      else if (auto nominal = type->getAnyNominal())
        return nominal->getDescriptiveKind();
      else
        return DescriptiveDeclKind::Type;
    };

    for (TypeLoc requirement : proto->getInherited().getEntries()) {
      checkTypeAccess(requirement, proto, /*mayBeInferred*/false,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *thisComplainRepr,
                          DowngradeToWarning downgradeDiag,
                          ImportAccessLevel importLimit) {
        if (typeAccessScope.isChildOf(minAccessScope) ||
            (!complainRepr &&
             typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
          minAccessScope = typeAccessScope;
          complainRepr = thisComplainRepr;
          protocolControlErrorKind = PCEK_Refine;
          downgradeToWarning = downgradeDiag;
          minImportLimit = importLimit;
          declKind = declKindForType(requirement.getType());
        }
      });
    }

    forAllRequirementTypes(proto, [&](Type type, TypeRepr *typeRepr) {
      checkTypeAccess(
          type, typeRepr, proto,
          /*mayBeInferred*/ false,
          [&](AccessScope typeAccessScope, const TypeRepr *thisComplainRepr,
              DowngradeToWarning downgradeDiag, ImportAccessLevel importLimit) {
            if (typeAccessScope.isChildOf(minAccessScope) ||
                (!complainRepr &&
                 typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
              minAccessScope = typeAccessScope;
              complainRepr = thisComplainRepr;
              protocolControlErrorKind = PCEK_Requirement;
              downgradeToWarning = downgradeDiag;
              minImportLimit = importLimit;
              declKind = declKindForType(type);
              // Swift versions before 5.0 did not check requirements on the
              // protocol's where clause, so emit a warning.
              if (!proto->getASTContext().isSwiftVersionAtLeast(5))
                downgradeToWarning = DowngradeToWarning::Yes;
            }
          });
    });

    if (!minAccessScope.isPublic()) {
      auto minAccess = minAccessScope.accessLevelForDiagnostics();
      bool isExplicit = proto->getAttrs().hasAttribute<AccessControlAttr>();
      auto protoAccess = isExplicit
          ? proto->getFormalAccess()
          : minAccessScope.requiredAccessForDiagnostics();
      auto diagID = diag::protocol_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::protocol_access_warn;
      auto diag = proto->diagnose(
          diagID, isExplicit, protoAccess, protocolControlErrorKind, minAccess,
          isa<FileUnit>(proto->getDeclContext()), declKind);
      highlightOffendingType(diag, complainRepr);
      noteLimitingImport(proto, minImportLimit, complainRepr);
    }
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    checkGenericParamAccess(SD, SD);

    auto minAccessScope = AccessScope::getPublic();
    const TypeRepr *complainRepr = nullptr;
    auto downgradeToWarning = DowngradeToWarning::No;
    ImportAccessLevel minImportLimit = std::nullopt;
    bool problemIsElement = false;

    for (auto &P : *SD->getIndices()) {
      checkTypeAccess(
          P->getInterfaceType(), P->getTypeRepr(), SD, /*mayBeInferred*/ false,
          [&](AccessScope typeAccessScope, const TypeRepr *thisComplainRepr,
              DowngradeToWarning downgradeDiag, ImportAccessLevel importLimit) {
            if (typeAccessScope.isChildOf(minAccessScope) ||
                (!complainRepr &&
                 typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
              minAccessScope = typeAccessScope;
              complainRepr = thisComplainRepr;
              downgradeToWarning = downgradeDiag;
              minImportLimit = importLimit;
            }
          });
    }

    checkTypeAccess(SD->getElementInterfaceType(), SD->getElementTypeRepr(),
                    SD, /*mayBeInferred*/false,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *thisComplainRepr,
                        DowngradeToWarning downgradeDiag,
                        ImportAccessLevel importLimit) {
      if (typeAccessScope.isChildOf(minAccessScope) ||
          (!complainRepr &&
           typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
        minAccessScope = typeAccessScope;
        complainRepr = thisComplainRepr;
        downgradeToWarning = downgradeDiag;
        problemIsElement = true;
        minImportLimit = importLimit;
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
      auto diag = SD->diagnose(diagID, isExplicit, subscriptDeclAccess,
                               minAccess, problemIsElement);
      highlightOffendingType(diag, complainRepr);
      noteLimitingImport(SD, minImportLimit, complainRepr);
    }
  }

  void visitAbstractFunctionDecl(AbstractFunctionDecl *fn) {
    bool isTypeContext = fn->getDeclContext()->isTypeContext();

    checkGenericParamAccess(fn, fn);

    // This must stay in sync with diag::function_type_access.
    enum {
      FK_Function = 0,
      FK_Method,
      FK_Initializer
    };

    // This must stay in sync with diag::function_type_access
    enum {
      EK_Parameter = 0,
      EK_ThrownError,
      EK_Result
    };

    auto minAccessScope = AccessScope::getPublic();
    const TypeRepr *complainRepr = nullptr;
    auto downgradeToWarning = DowngradeToWarning::No;
    ImportAccessLevel minImportLimit = std::nullopt;
    unsigned entityKind = EK_Parameter;

    bool hasInaccessibleParameterWrapper = false;
    for (auto *P : *fn->getParameters()) {
      // Check for inaccessible API property wrappers attached to the parameter.
      if (P->hasExternalPropertyWrapper()) {
        auto wrapperAttrs = P->getAttachedPropertyWrappers();
        for (auto index : indices(wrapperAttrs)) {
          auto wrapperType = P->getAttachedPropertyWrapperType(index);
          auto wrapperTypeRepr = wrapperAttrs[index]->getTypeRepr();
          checkTypeAccess(wrapperType, wrapperTypeRepr, fn, /*mayBeInferred*/ false,
              [&](AccessScope typeAccessScope, const TypeRepr *thisComplainRepr,
                  DowngradeToWarning downgradeDiag,
                  ImportAccessLevel importLimit) {
                if (typeAccessScope.isChildOf(minAccessScope) ||
                    (!complainRepr &&
                     typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
                  minAccessScope = typeAccessScope;
                  complainRepr = thisComplainRepr;
                  downgradeToWarning = downgradeDiag;
                  minImportLimit = importLimit;
                  hasInaccessibleParameterWrapper = true;
                }
              });
        }
      }

      checkTypeAccess(
          P->getInterfaceType(), P->getTypeRepr(), fn, /*mayBeInferred*/ false,
          [&](AccessScope typeAccessScope, const TypeRepr *thisComplainRepr,
              DowngradeToWarning downgradeDiag, ImportAccessLevel importLimit) {
            if (typeAccessScope.isChildOf(minAccessScope) ||
                (!complainRepr &&
                 typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
              minAccessScope = typeAccessScope;
              complainRepr = thisComplainRepr;
              downgradeToWarning = downgradeDiag;
              minImportLimit = importLimit;
            }
          });
    }

    if (auto thrownTypeRepr = fn->getThrownTypeRepr()) {
      checkTypeAccess(
        fn->getThrownInterfaceType(), thrownTypeRepr, fn,
        /*mayBeInferred*/ false,
        [&](AccessScope typeAccessScope, const TypeRepr *thisComplainRepr,
            DowngradeToWarning downgradeDiag, ImportAccessLevel importLimit) {
          if (typeAccessScope.isChildOf(minAccessScope) ||
              (!complainRepr &&
               typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
            minAccessScope = typeAccessScope;
            complainRepr = thisComplainRepr;
            downgradeToWarning = downgradeDiag;
            minImportLimit = importLimit;
            entityKind = EK_ThrownError;
          }
      });
    }


    if (auto FD = dyn_cast<FuncDecl>(fn)) {
      checkTypeAccess(FD->getResultInterfaceType(), FD->getResultTypeRepr(),
                      FD, /*mayBeInferred*/false,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *thisComplainRepr,
                          DowngradeToWarning downgradeDiag,
                          ImportAccessLevel importLimit) {
        if (typeAccessScope.isChildOf(minAccessScope) ||
            (!complainRepr &&
             typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
          minAccessScope = typeAccessScope;
          complainRepr = thisComplainRepr;
          downgradeToWarning = downgradeDiag;
          minImportLimit = importLimit;
          entityKind = EK_Result;
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
      auto fnAccess = isExplicit
        ? fn->getFormalAccess()
        : minAccessScope.requiredAccessForDiagnostics();

      auto diagID = diag::function_type_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::function_type_access_warn;
      auto diag = fn->diagnose(diagID, isExplicit, fnAccess,
                               isa<FileUnit>(fn->getDeclContext()), minAccess,
                               functionKind, entityKind,
                               hasInaccessibleParameterWrapper);
      highlightOffendingType(diag, complainRepr);
      noteLimitingImport(fn, minImportLimit, complainRepr);
    }
  }

  void visitEnumElementDecl(EnumElementDecl *EED) {
    if (!EED->hasAssociatedValues())
      return;
    for (auto &P : *EED->getParameterList()) {
      checkTypeAccess(
          P->getInterfaceType(), P->getTypeRepr(), EED, /*mayBeInferred*/ false,
          [&](AccessScope typeAccessScope, const TypeRepr *complainRepr,
              DowngradeToWarning downgradeToWarning,
              ImportAccessLevel importLimit) {
            auto typeAccess = typeAccessScope.accessLevelForDiagnostics();
            auto diagID = diag::enum_case_access;
            if (downgradeToWarning == DowngradeToWarning::Yes)
              diagID = diag::enum_case_access_warn;
            auto diag =
                EED->diagnose(diagID, EED->getFormalAccess(), typeAccess);
            highlightOffendingType(diag, complainRepr);
            noteLimitingImport(EED, importLimit, complainRepr);
          });
    }
  }

  void visitMacroDecl(MacroDecl *MD) {
    checkGenericParamAccess(MD, MD);

    auto minAccessScope = AccessScope::getPublic();
    const TypeRepr *complainRepr = nullptr;
    auto downgradeToWarning = DowngradeToWarning::No;
    ImportAccessLevel minImportLimit = std::nullopt;
    bool problemIsResult = false;

    if (MD->parameterList) {
      for (auto *P : *MD->parameterList) {
        checkTypeAccess(
            P->getInterfaceType(), P->getTypeRepr(), MD, /*mayBeInferred*/ false,
            [&](AccessScope typeAccessScope, const TypeRepr *thisComplainRepr,
                DowngradeToWarning downgradeDiag,
                ImportAccessLevel importLimit) {
              if (typeAccessScope.isChildOf(minAccessScope) ||
                  (!complainRepr &&
                   typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
                minAccessScope = typeAccessScope;
                complainRepr = thisComplainRepr;
                downgradeToWarning = downgradeDiag;
                minImportLimit = importLimit;
              }
            });
      }
    }

    checkTypeAccess(MD->getResultInterfaceType(), MD->resultType.getTypeRepr(),
                    MD, /*mayBeInferred*/false,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *thisComplainRepr,
                        DowngradeToWarning downgradeDiag,
                        ImportAccessLevel importLimit) {
      if (typeAccessScope.isChildOf(minAccessScope) ||
          (!complainRepr &&
           typeAccessScope.hasEqualDeclContextWith(minAccessScope))) {
        minAccessScope = typeAccessScope;
        complainRepr = thisComplainRepr;
        downgradeToWarning = downgradeDiag;
        minImportLimit = importLimit;
        problemIsResult = true;
      }
    });

    if (!minAccessScope.isPublic()) {
      auto minAccess = minAccessScope.accessLevelForDiagnostics();
      bool isExplicit =
        MD->getAttrs().hasAttribute<AccessControlAttr>();
      auto diagID = diag::macro_type_access;
      if (downgradeToWarning == DowngradeToWarning::Yes)
        diagID = diag::macro_type_access_warn;
      auto macroDeclAccess = isExplicit
        ? MD->getFormalAccess()
        : minAccessScope.requiredAccessForDiagnostics();
      auto diag = MD->diagnose(diagID, isExplicit, macroDeclAccess,
                               minAccess, problemIsResult);
      highlightOffendingType(diag, complainRepr);
      noteLimitingImport(MD, minImportLimit, complainRepr);
    }
  }

  void checkAttachedMacrosAccess(const Decl *D) {
    for (auto customAttrC : D->getExpandedAttrs().getAttributes<CustomAttr>()) {
      auto customAttr = const_cast<CustomAttr *>(customAttrC);
      auto *macroDecl = D->getResolvedMacro(customAttr);
      if (macroDecl) {
        diagnoseDeclAvailability(
            macroDecl, customAttr->getTypeRepr()->getSourceRange(), nullptr,
            ExportContext::forDeclSignature(const_cast<Decl *>(D)),
            std::nullopt);
      }
    }
  }
};

class UsableFromInlineChecker : public AccessControlCheckerBase,
                                public DeclVisitor<UsableFromInlineChecker> {
public:
  UsableFromInlineChecker()
      : AccessControlCheckerBase(/*checkUsableFromInline=*/true) {}

  static bool shouldSkipChecking(const ValueDecl *VD) {
    if (VD->getFormalAccess() != AccessLevel::Internal &&
        VD->getFormalAccess() != AccessLevel::Package)
      return true;
    return !VD->isUsableFromInline();
  };

  void visit(Decl *D) {
    if (!D->getASTContext().isSwiftVersionAtLeast(4, 2))
      return;

    if (D->isInvalid() || D->isImplicit())
      return;

    if (auto *VD = dyn_cast<ValueDecl>(D))
      if (shouldSkipChecking(VD))
        return;

    DeclVisitor<UsableFromInlineChecker>::visit(D);
    checkGlobalActorAccess(D);
  }

  // Force all kinds to be handled at a lower level.
  void visitDecl(Decl *D) = delete;
  void visitValueDecl(ValueDecl *D) = delete;

#define UNREACHABLE(KIND, REASON) \
  void visit##KIND##Decl(KIND##Decl *D) { \
    llvm_unreachable(REASON); \
  }
  UNREACHABLE(Import, "cannot appear in a type context")
  UNREACHABLE(Extension, "cannot appear in a type context")
  UNREACHABLE(TopLevelCode, "cannot appear in a type context")
  UNREACHABLE(Operator, "cannot appear in a type context")
  UNREACHABLE(PrecedenceGroup, "cannot appear in a type context")
  UNREACHABLE(Module, "cannot appear in a type context")

  UNREACHABLE(Param, "does not have access control")
  UNREACHABLE(GenericTypeParam, "does not have access control")
  UNREACHABLE(Missing, "does not have access control")
  UNREACHABLE(MissingMember, "does not have access control")
  UNREACHABLE(MacroExpansion, "does not have access control")
  UNREACHABLE(BuiltinTuple, "BuiltinTupleDecl should not show up here")
#undef UNREACHABLE

#define UNINTERESTING(KIND) \
  void visit##KIND##Decl(KIND##Decl *D) {}

  UNINTERESTING(PoundDiagnostic) // Does not have access control.
  UNINTERESTING(EnumCase) // Handled at the EnumElement level.
  UNINTERESTING(Var) // Handled at the PatternBinding level.
  UNINTERESTING(Destructor) // Always correct.
  UNINTERESTING(Accessor) // Handled by the Var or Subscript.
  UNINTERESTING(OpaqueType) // Handled by the Var or Subscript.

  /// If \p VD's layout is exposed by a @frozen struct or class, return said
  /// struct or class.
  ///
  /// Stored instance properties in @frozen structs and classes must always use
  /// public/@usableFromInline types. In these cases, check the access against
  /// the struct instead of the VarDecl, and customize the diagnostics.
  static const ValueDecl *
  getFixedLayoutStructContext(const VarDecl *VD) {
    if (VD->isLayoutExposedToClients())
      return dyn_cast<NominalTypeDecl>(VD->getDeclContext());

    return nullptr;
  }

  /// \see visitPatternBindingDecl
  void checkNamedPattern(const NamedPattern *NP,
                         bool isTypeContext,
                         const llvm::DenseSet<const VarDecl *> &seenVars) {
    const VarDecl *theVar = NP->getDecl();
    auto *fixedLayoutStructContext = getFixedLayoutStructContext(theVar);
    if (!fixedLayoutStructContext && shouldSkipChecking(theVar))
      return;
    // Only check individual variables if we didn't check an enclosing
    // TypedPattern.
    if (seenVars.count(theVar) || theVar->isInvalid())
      return;

    checkTypeAccess(
        theVar->getInterfaceType(), nullptr,
        fixedLayoutStructContext ? fixedLayoutStructContext : theVar,
        /*mayBeInferred*/ false,
        [&](AccessScope typeAccessScope, const TypeRepr *complainRepr,
            DowngradeToWarning downgradeToWarning,
            ImportAccessLevel importLimit) {
          auto &Ctx = theVar->getASTContext();
          auto diagID = diag::pattern_type_not_usable_from_inline_inferred;
          if (fixedLayoutStructContext) {
            diagID = diag::pattern_type_not_usable_from_inline_inferred_frozen;
          } else if (!Ctx.isSwiftVersionAtLeast(5)) {
            diagID = diag::pattern_type_not_usable_from_inline_inferred_warn;
          }
          Ctx.Diags.diagnose(NP->getLoc(), diagID, theVar->isLet(),
                             isTypeContext, theVar->getInterfaceType());
          noteLimitingImport(theVar, importLimit, complainRepr);
        });
  }

  /// \see visitPatternBindingDecl
  void checkTypedPattern(const TypedPattern *TP,
                         bool isTypeContext,
                         llvm::DenseSet<const VarDecl *> &seenVars) {
    // FIXME: We need an access level to check against, so we pull one out
    // of some random VarDecl in the pattern. They're all going to be the
    // same, but still, ick.
    VarDecl *anyVar = nullptr;
    TP->forEachVariable([&](VarDecl *V) {
      seenVars.insert(V);
      anyVar = V;
    });
    if (!anyVar)
      return;
    auto *fixedLayoutStructContext = getFixedLayoutStructContext(anyVar);
    if (!fixedLayoutStructContext && shouldSkipChecking(anyVar))
      return;

    checkTypeAccess(
        TP->hasType() ? TP->getType() : Type(),
        TP->getTypeRepr(),
        fixedLayoutStructContext ? fixedLayoutStructContext : anyVar,
        /*mayBeInferred*/ true,
        [&](AccessScope typeAccessScope, const TypeRepr *complainRepr,
            DowngradeToWarning downgradeToWarning,
            ImportAccessLevel importLimit) {
          auto &Ctx = anyVar->getASTContext();
          auto diagID = diag::pattern_type_not_usable_from_inline;
          if (fixedLayoutStructContext)
            diagID = diag::pattern_type_not_usable_from_inline_frozen;
          else if (!Ctx.isSwiftVersionAtLeast(5))
            diagID = diag::pattern_type_not_usable_from_inline_warn;
          auto diag = Ctx.Diags.diagnose(TP->getLoc(), diagID, anyVar->isLet(),
                                         isTypeContext);
          highlightOffendingType(diag, complainRepr);
          noteLimitingImport(anyVar, importLimit, complainRepr);
        });

    for (auto attr : anyVar->getAttachedPropertyWrappers()) {
      checkTypeAccess(attr->getType(), attr->getTypeRepr(),
                      fixedLayoutStructContext ? fixedLayoutStructContext
                                               : anyVar,
                      /*mayBeInferred*/false,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning,
                          ImportAccessLevel importLimit) {
        auto diag = anyVar->diagnose(
            diag::property_wrapper_type_not_usable_from_inline,
            anyVar->isLet(), isTypeContext);
        highlightOffendingType(diag, complainRepr);
        noteLimitingImport(anyVar, importLimit, complainRepr);
      });
    }
  }

  void visitPatternBindingDecl(PatternBindingDecl *PBD) {
    bool isTypeContext = PBD->getDeclContext()->isTypeContext();

    llvm::DenseSet<const VarDecl *> seenVars;
    for (auto idx : range(PBD->getNumPatternEntries())) {
      PBD->getPattern(idx)->forEachNode([&](const Pattern *P) {
        if (auto *NP = dyn_cast<NamedPattern>(P)) {
          checkNamedPattern(NP, isTypeContext, seenVars);
          return;
        }

        auto *TP = dyn_cast<TypedPattern>(P);
        if (!TP)
          return;
        checkTypedPattern(TP, isTypeContext, seenVars);
      });
      seenVars.clear();
    }
  }

  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    checkGenericParamAccess(TAD, TAD);

    checkTypeAccess(TAD->getUnderlyingType(),
                    TAD->getUnderlyingTypeRepr(), TAD, /*mayBeInferred*/false,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *complainRepr,
                        DowngradeToWarning downgradeToWarning,
                        ImportAccessLevel importLimit) {
      auto diagID = diag::type_alias_underlying_type_not_usable_from_inline;
      if (!TAD->getASTContext().isSwiftVersionAtLeast(5))
        diagID = diag::type_alias_underlying_type_not_usable_from_inline_warn;
      auto diag = TAD->diagnose(diagID);
      highlightOffendingType(diag, complainRepr);
      noteLimitingImport(TAD, importLimit, complainRepr);
    });
  }

  void visitAssociatedTypeDecl(AssociatedTypeDecl *assocType) {
    // This must stay in sync with diag::associated_type_not_usable_from_inline.
    enum {
      ACEK_DefaultDefinition = 0,
      ACEK_Requirement
    };

    for (TypeLoc requirement : assocType->getInherited().getEntries()) {
      checkTypeAccess(requirement, assocType, /*mayBeInferred*/false,
                      [&](AccessScope typeAccessScope,
            const TypeRepr *complainRepr,
            DowngradeToWarning downgradeDiag,
                          ImportAccessLevel importLimit) {
        auto diagID = diag::associated_type_not_usable_from_inline;
        if (!assocType->getASTContext().isSwiftVersionAtLeast(5))
          diagID = diag::associated_type_not_usable_from_inline_warn;
        auto diag = assocType->diagnose(diagID, ACEK_Requirement);
        highlightOffendingType(diag, complainRepr);
        noteLimitingImport(assocType, importLimit, complainRepr);
      });
    }
    checkTypeAccess(assocType->getDefaultDefinitionType(),
                    assocType->getDefaultDefinitionTypeRepr(), assocType,
                     /*mayBeInferred*/false,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *complainRepr,
                        DowngradeToWarning downgradeDiag,
                        ImportAccessLevel importLimit) {
      auto diagID = diag::associated_type_not_usable_from_inline;
      if (!assocType->getASTContext().isSwiftVersionAtLeast(5))
        diagID = diag::associated_type_not_usable_from_inline_warn;
      auto diag = assocType->diagnose(diagID, ACEK_DefaultDefinition);
      highlightOffendingType(diag, complainRepr);
      noteLimitingImport(assocType, importLimit, complainRepr);
    });

    if (assocType->getTrailingWhereClause()) {
      auto accessScope =
        assocType->getFormalAccessScope(nullptr);
      checkRequirementAccess(assocType,
                             accessScope,
                             assocType->getDeclContext(),
                             [&](AccessScope typeAccessScope,
                                 const TypeRepr *complainRepr,
                                 DowngradeToWarning downgradeDiag,
                                 ImportAccessLevel importLimit) {
        auto diagID = diag::associated_type_not_usable_from_inline;
        if (!assocType->getASTContext().isSwiftVersionAtLeast(5))
          diagID = diag::associated_type_not_usable_from_inline_warn;
        auto diag = assocType->diagnose(diagID, ACEK_Requirement);
        highlightOffendingType(diag, complainRepr);
        noteLimitingImport(assocType, importLimit, complainRepr);
      });
    }
  }

  void visitEnumDecl(const EnumDecl *ED) {
    checkGenericParamAccess(ED, ED);

    if (ED->hasRawType()) {
      Type rawType = ED->getRawType();
      auto inheritedEntries = ED->getInherited().getEntries();
      auto rawTypeLocIter = std::find_if(inheritedEntries.begin(),
                                         inheritedEntries.end(),
                                         [&](TypeLoc inherited) {
        if (!inherited.wasValidated())
          return false;
        return inherited.getType().getPointer() == rawType.getPointer();
      });
      if (rawTypeLocIter == inheritedEntries.end())
        return;
      checkTypeAccess(rawType, rawTypeLocIter->getTypeRepr(), ED,
                       /*mayBeInferred*/false,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning,
                          ImportAccessLevel importLimit) {
        auto diagID = diag::enum_raw_type_not_usable_from_inline;
        if (!ED->getASTContext().isSwiftVersionAtLeast(5))
          diagID = diag::enum_raw_type_not_usable_from_inline_warn;
        auto diag = ED->diagnose(diagID);
        highlightOffendingType(diag, complainRepr);
        noteLimitingImport(ED, importLimit, complainRepr);
      });
    }
  }

  void visitStructDecl(StructDecl *SD) {
    checkGenericParamAccess(SD, SD);
  }

  void visitClassDecl(ClassDecl *CD) {
    checkGenericParamAccess(CD, CD);

    if (CD->hasSuperclass()) {
      const NominalTypeDecl *superclassDecl = CD->getSuperclassDecl();
      auto inheritedEntries = CD->getInherited().getEntries();
      // Be slightly defensive here in the presence of badly-ordered
      // inheritance clauses.
      auto superclassLocIter = std::find_if(inheritedEntries.begin(),
                                            inheritedEntries.end(),
                                            [&](TypeLoc inherited) {
        if (!inherited.wasValidated())
          return false;
        Type ty = inherited.getType();
        if (ty->is<ProtocolCompositionType>())
          if (auto superclass = ty->getExistentialLayout().explicitSuperclass)
            ty = superclass;
        return ty->getAnyNominal() == superclassDecl;
      });
      // Soundness check: we couldn't find the superclass for whatever reason
      // (possibly because it's synthetic or something), so don't bother
      // checking it.
      if (superclassLocIter == inheritedEntries.end())
        return;

      checkTypeAccess(CD->getSuperclass(), superclassLocIter->getTypeRepr(), CD,
                       /*mayBeInferred*/false,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeToWarning,
                          ImportAccessLevel importLimit) {
        auto diagID = diag::class_super_not_usable_from_inline;
        if (!CD->getASTContext().isSwiftVersionAtLeast(5))
          diagID = diag::class_super_not_usable_from_inline_warn;
        auto diag = CD->diagnose(diagID, superclassLocIter->getTypeRepr() !=
                                             complainRepr);
        highlightOffendingType(diag, complainRepr);
        noteLimitingImport(CD, importLimit, complainRepr);
      });
    }
  }

  void visitProtocolDecl(ProtocolDecl *proto) {
    // This must stay in sync with diag::protocol_usable_from_inline.
    enum {
      PCEK_Refine = 0,
      PCEK_Requirement
    };

    for (TypeLoc requirement : proto->getInherited().getEntries()) {
      checkTypeAccess(requirement, proto, /*mayBeInferred*/false,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeDiag,
                          ImportAccessLevel importLimit) {
        auto diagID = diag::protocol_usable_from_inline;
        if (!proto->getASTContext().isSwiftVersionAtLeast(5))
          diagID = diag::protocol_usable_from_inline_warn;
        auto diag = proto->diagnose(diagID, PCEK_Refine);
        highlightOffendingType(diag, complainRepr);
        noteLimitingImport(proto, importLimit, complainRepr);
      });
    }

    if (proto->getTrailingWhereClause()) {
      auto accessScope = proto->getFormalAccessScope(nullptr,
                                                     /*checkUsableFromInline*/true);
      checkRequirementAccess(proto,
                             accessScope,
                             proto->getDeclContext(),
                             [&](AccessScope typeAccessScope,
                                 const TypeRepr *complainRepr,
                                 DowngradeToWarning downgradeDiag,
                                 ImportAccessLevel importLimit) {
        auto diagID = diag::protocol_usable_from_inline;
        if (!proto->getASTContext().isSwiftVersionAtLeast(5))
          diagID = diag::protocol_usable_from_inline_warn;
        auto diag = proto->diagnose(diagID, PCEK_Requirement);
        highlightOffendingType(diag, complainRepr);
        noteLimitingImport(proto, importLimit, complainRepr);
      });
    }
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    checkGenericParamAccess(SD, SD);

    for (auto &P : *SD->getIndices()) {
      checkTypeAccess(
          P->getInterfaceType(), P->getTypeRepr(), SD, /*mayBeInferred*/ false,
          [&](AccessScope typeAccessScope, const TypeRepr *complainRepr,
              DowngradeToWarning downgradeDiag,
              ImportAccessLevel importLimit) {
            auto diagID = diag::subscript_type_usable_from_inline;
            if (!SD->getASTContext().isSwiftVersionAtLeast(5))
              diagID = diag::subscript_type_usable_from_inline_warn;
            auto diag = SD->diagnose(diagID, /*problemIsElement=*/false);
            highlightOffendingType(diag, complainRepr);
            noteLimitingImport(SD, importLimit, complainRepr);
          });
    }

    checkTypeAccess(SD->getElementInterfaceType(), SD->getElementTypeRepr(),
                    SD, /*mayBeInferred*/false,
                    [&](AccessScope typeAccessScope,
                        const TypeRepr *complainRepr,
                        DowngradeToWarning downgradeDiag,
                        ImportAccessLevel importLimit) {
      auto diagID = diag::subscript_type_usable_from_inline;
      if (!SD->getASTContext().isSwiftVersionAtLeast(5))
        diagID = diag::subscript_type_usable_from_inline_warn;
      auto diag = SD->diagnose(diagID, /*problemIsElement=*/true);
      highlightOffendingType(diag, complainRepr);
      noteLimitingImport(SD, importLimit, complainRepr);
    });
  }

  void visitAbstractFunctionDecl(AbstractFunctionDecl *fn) {
    bool isTypeContext = fn->getDeclContext()->isTypeContext();

    checkGenericParamAccess(fn, fn);

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
      // Check for inaccessible API property wrappers attached to the parameter.
      if (P->hasExternalPropertyWrapper()) {
        auto wrapperAttrs = P->getAttachedPropertyWrappers();
        for (auto index : indices(wrapperAttrs)) {
          auto wrapperType = P->getAttachedPropertyWrapperType(index);
          auto wrapperTypeRepr = wrapperAttrs[index]->getTypeRepr();
          checkTypeAccess(wrapperType, wrapperTypeRepr, fn, /*mayBeInferred*/ false,
              [&](AccessScope typeAccessScope, const TypeRepr *complainRepr,
                  DowngradeToWarning downgradeDiag,
                  ImportAccessLevel importLimit) {
                auto diagID = diag::function_type_usable_from_inline;
                if (!fn->getASTContext().isSwiftVersionAtLeast(5))
                  diagID = diag::function_type_usable_from_inline_warn;
                auto diag = fn->diagnose(diagID, functionKind,
                                         /*problemIsResult=*/false,
                                         /*inaccessibleWrapper=*/true);
                highlightOffendingType(diag, complainRepr);
                noteLimitingImport(fn, importLimit, complainRepr);
              });
        }
      }

      checkTypeAccess(
          P->getInterfaceType(), P->getTypeRepr(), fn, /*mayBeInferred*/ false,
          [&](AccessScope typeAccessScope, const TypeRepr *complainRepr,
              DowngradeToWarning downgradeDiag,
              ImportAccessLevel importLimit) {
            auto diagID = diag::function_type_usable_from_inline;
            if (!fn->getASTContext().isSwiftVersionAtLeast(5))
              diagID = diag::function_type_usable_from_inline_warn;
            auto diag = fn->diagnose(diagID, functionKind,
                                     /*problemIsResult=*/false,
                                     /*inaccessibleWrapper=*/false);
            highlightOffendingType(diag, complainRepr);
            noteLimitingImport(fn, importLimit, complainRepr);
          });
    }

    if (auto FD = dyn_cast<FuncDecl>(fn)) {
      checkTypeAccess(FD->getResultInterfaceType(), FD->getResultTypeRepr(),
                      FD, /*mayBeInferred*/false,
                      [&](AccessScope typeAccessScope,
                          const TypeRepr *complainRepr,
                          DowngradeToWarning downgradeDiag,
                          ImportAccessLevel importLimit) {
        auto diagID = diag::function_type_usable_from_inline;
        if (!fn->getASTContext().isSwiftVersionAtLeast(5))
          diagID = diag::function_type_usable_from_inline_warn;
        auto diag = fn->diagnose(diagID, functionKind,
                                 /*problemIsResult=*/true,
                                 /*inaccessibleWrapper=*/false);
        highlightOffendingType(diag, complainRepr);
        noteLimitingImport(fn, importLimit, complainRepr);
      });
    }
  }

  void visitMacroDecl(MacroDecl *MD) {
    // FIXME: Check access of macro generic parameters, parameters and result
  }

  void visitEnumElementDecl(EnumElementDecl *EED) {
    if (!EED->hasAssociatedValues())
      return;
    for (auto &P : *EED->getParameterList()) {
      checkTypeAccess(
          P->getInterfaceType(), P->getTypeRepr(), EED, /*mayBeInferred*/ false,
          [&](AccessScope typeAccessScope, const TypeRepr *complainRepr,
              DowngradeToWarning downgradeToWarning,
              ImportAccessLevel importLimit) {
            auto diagID = diag::enum_case_usable_from_inline;
            if (!EED->getASTContext().isSwiftVersionAtLeast(5))
              diagID = diag::enum_case_usable_from_inline_warn;
            auto diag = EED->diagnose(diagID);
            highlightOffendingType(diag, complainRepr);
            noteLimitingImport(EED, importLimit, complainRepr);
          });
    }
  }
};

bool isFragileClangDecl(const clang::Decl *decl);

bool isFragileClangType(clang::QualType type) {
  if (type.isNull())
    return true;
  auto underlyingTypePtr = type->getUnqualifiedDesugaredType();
  // Objective-C types are compatible with library
  // evolution.
  if (underlyingTypePtr->isObjCObjectPointerType())
    return false;
  // Builtin clang types are compatible with library evolution.
  if (underlyingTypePtr->isBuiltinType())
    return false;
  // Pointers to non-fragile types are non-fragile.
  if (underlyingTypePtr->isPointerType())
    return isFragileClangType(underlyingTypePtr->getPointeeType());
  if (auto tagDecl = underlyingTypePtr->getAsTagDecl())
    return isFragileClangDecl(tagDecl);
  return true;
}

bool isFragileClangDecl(const clang::Decl *decl) {
  // Namespaces by themselves don't impact ABI.
  if (isa<clang::NamespaceDecl>(decl))
    return false;
  // Objective-C type declarations are compatible with library evolution.
  if (isa<clang::ObjCContainerDecl>(decl))
    return false;
  if (auto *fd = dyn_cast<clang::FunctionDecl>(decl)) {
    if (auto *ctorDecl = dyn_cast<clang::CXXConstructorDecl>(fd))
      return isFragileClangDecl(ctorDecl->getParent());
    if (!isa<clang::CXXMethodDecl>(decl) &&
        !isFragileClangType(fd->getDeclaredReturnType())) {
      for (const auto *param : fd->parameters()) {
        if (isFragileClangType(param->getType()))
          return true;
      }
      // A global function whose return and parameter types are compatible with
      // library evolution is compatible with library evolution.
      return false;
    }
  }
  if (auto *md = dyn_cast<clang::ObjCMethodDecl>(decl)) {
    if (!isFragileClangType(md->getReturnType())) {
      for (const auto *param : md->parameters()) {
        if (isFragileClangType(param->getType()))
          return true;
      }
      // An Objective-C method whose return and parameter types are compatible
      // with library evolution is compatible with library evolution.
      return false;
    }
  }
  // An Objective-C property whose can be compatible
  // with library evolution if its type is compatible.
  if (auto *pd = dyn_cast<clang::ObjCPropertyDecl>(decl))
    return isFragileClangType(pd->getType());
  if (auto *typedefDecl = dyn_cast<clang::TypedefNameDecl>(decl))
    return isFragileClangType(typedefDecl->getUnderlyingType());
  if (auto *rd = dyn_cast<clang::RecordDecl>(decl)) {
    auto cxxRecordDecl = dyn_cast<clang::CXXRecordDecl>(rd);
    if (!cxxRecordDecl)
      return false;
    return !cxxRecordDecl->isCLike() &&
           !cxxRecordDecl->getDeclContext()->isExternCContext();
  }
  if (auto *varDecl = dyn_cast<clang::VarDecl>(decl))
    return isFragileClangType(varDecl->getType());
  if (auto *fieldDecl = dyn_cast<clang::FieldDecl>(decl))
    return isFragileClangType(fieldDecl->getType()) ||
           isFragileClangDecl(fieldDecl->getParent());
  return true;
}

bool isFragileClangNode(const ClangNode &node) {
  auto *decl = node.getAsDecl();
  if (!decl)
    return false;
  return isFragileClangDecl(decl);
}

} // end anonymous namespace

/// Returns the kind of origin, implementation-only import or SPI declaration,
/// that restricts exporting \p decl from the given file and context.
///
/// Local variant to swift::getDisallowedOriginKind for downgrade to warnings.
DisallowedOriginKind
swift::getDisallowedOriginKind(const Decl *decl,
                               const ExportContext &where,
                               DowngradeToWarning &downgradeToWarning) {
  downgradeToWarning = DowngradeToWarning::No;
  ModuleDecl *M = decl->getModuleContext();
  auto *SF = where.getDeclContext()->getParentSourceFile();

  RestrictedImportKind howImported = SF->getRestrictedImportKind(M);
  if (howImported != RestrictedImportKind::None) {
    // Temporarily downgrade implementation-only exportability in SPI to
    // a warning.
    if (where.isSPI() &&
        where.getFragileFunctionKind().kind == FragileFunctionKind::None &&
        !SF->getASTContext().LangOpts.EnableSPIOnlyImports)
      downgradeToWarning = DowngradeToWarning::Yes;

    if (where.isSPI() && howImported == RestrictedImportKind::SPIOnly)
      return DisallowedOriginKind::None;

    // Before Swift 6, implicit imports were not reported unless an
    // implementation-only import was also present. Downgrade to a warning
    // just in this case.
    if (howImported == RestrictedImportKind::MissingImport &&
        !SF->getASTContext().isSwiftVersionAtLeast(6) &&
        !SF->hasImportsWithFlag(ImportFlags::ImplementationOnly)) {
      downgradeToWarning = DowngradeToWarning::Yes;
    }

    // Even if the current module is @_implementationOnly, Swift should
    // not report an error in the cases where the decl is also exported from
    // a non @_implementationOnly module. Thus, we check to see if there is
    // a visible access path to the Clang decl, and only error out in case
    // there is none.
    auto filter = ModuleDecl::ImportFilter(
        {ModuleDecl::ImportFilterKind::Exported,
         ModuleDecl::ImportFilterKind::Default,
         ModuleDecl::ImportFilterKind::ShadowedByCrossImportOverlay});
    SmallVector<ImportedModule, 4> sfImportedModules;
    SF->getImportedModules(sfImportedModules, filter);
    if (auto clangDecl = decl->getClangDecl()) {
      for (auto redecl : clangDecl->redecls()) {
        if (auto tagReDecl = dyn_cast<clang::TagDecl>(redecl)) {
          // This is a forward declaration. We ignore visibility of those.
          if (tagReDecl->getBraceRange().isInvalid()) {
            continue;
          }
        }
        auto owningModule = redecl->getOwningModule();
        if (!owningModule)
          continue;
        auto moduleWrapper =
            decl->getASTContext().getClangModuleLoader()->getWrapperForModule(
                owningModule);
        auto visibleAccessPath =
            find_if(sfImportedModules, [&moduleWrapper](auto importedModule) {
              return importedModule.importedModule == moduleWrapper ||
                     !importedModule.importedModule
                          ->isImportedImplementationOnly(moduleWrapper);
            });
        if (visibleAccessPath != sfImportedModules.end()) {
          return DisallowedOriginKind::None;
        }
      }
    }

    // Restrictively imported, cannot be reexported.
    switch (howImported) {
    case RestrictedImportKind::MissingImport:
      return DisallowedOriginKind::MissingImport;
    case RestrictedImportKind::SPIOnly:
      return DisallowedOriginKind::SPIOnly;
    case RestrictedImportKind::ImplementationOnly:
      return DisallowedOriginKind::ImplementationOnly;
    default:
      llvm_unreachable("RestrictedImportKind isn't handled");
    }
  } else if ((decl->isSPI() || decl->isAvailableAsSPI()) && !where.isSPI()) {
    if (decl->isAvailableAsSPI() && !decl->isSPI()) {
      // Allowing unavailable context to use @_spi_available decls.
      // Decls with @_spi_available aren't hidden entirely from public interfaces,
      // thus public interfaces may still refer them. Be forgiving here so public
      // interfaces can compile.
      if (where.getUnavailablePlatformKind().has_value())
        return DisallowedOriginKind::None;
      // We should only diagnose SPI_AVAILABLE usage when the library level is API.
      // Using SPI_AVAILABLE symbols in private frameworks or executable targets
      // should be allowed.
      if (auto *mod = where.getDeclContext()->getParentModule()) {
        if (mod->getLibraryLevel() != LibraryLevel::API) {
          return DisallowedOriginKind::None;
        }
      }
    }
    // SPI can only be exported in SPI.
    return where.getDeclContext()->getParentModule() == M ?
      DisallowedOriginKind::SPILocal :
      DisallowedOriginKind::SPIImported;
  }

  // C++ APIs do not support library evolution.
  if (SF->getASTContext().LangOpts.EnableCXXInterop && where.getDeclContext() &&
      where.getDeclContext()->getAsDecl() &&
      where.getDeclContext()->getAsDecl()->getModuleContext()->isResilient() &&
      !where.getDeclContext()
           ->getAsDecl()
           ->getModuleContext()
           ->getUnderlyingModuleIfOverlay() &&
      decl->hasClangNode() && !decl->getModuleContext()->isSwiftShimsModule() &&
      isFragileClangNode(decl->getClangNode()) &&
      !SF->getASTContext().LangOpts.hasFeature(
          Feature::AssumeResilientCxxTypes))
    return DisallowedOriginKind::FragileCxxAPI;

  // Report non-public import last as it can be ignored by the caller.
  // See \c diagnoseValueDeclRefExportability.
  auto importSource = decl->getImportAccessFrom(where.getDeclContext());
  if (importSource.has_value() &&
      importSource->accessLevel < AccessLevel::Public)
    return DisallowedOriginKind::NonPublicImport;

  return DisallowedOriginKind::None;
}

namespace {

/// Diagnose declarations whose signatures refer to unavailable types.
class DeclAvailabilityChecker : public DeclVisitor<DeclAvailabilityChecker> {
  ExportContext Where;

  void checkType(Type type, const TypeRepr *typeRepr, const Decl *context,
                 ExportabilityReason reason = ExportabilityReason::General,
                 DeclAvailabilityFlags flags = std::nullopt) {
    // Don't bother checking errors.
    if (type && type->hasError())
      return;
    
    // If the decl which references this type is unavailable on the current
    // platform, don't diagnose the availability of the type.
    if (Where.getAvailability().isUnavailable())
      return;

    diagnoseTypeAvailability(typeRepr, type, context->getLoc(),
                             Where.withReason(reason), flags);
  }

  /// Identify the AsyncSequence.flatMap set of functions from the
  /// _Concurrency module.
  static bool isAsyncSequenceFlatMap(const GenericContext *gc) {
    auto func = dyn_cast<FuncDecl>(gc);
    if (!func)
      return false;

    auto proto = func->getDeclContext()->getSelfProtocolDecl();
    if (!proto ||
        !proto->isSpecificProtocol(KnownProtocolKind::AsyncSequence))
      return false;

    ASTContext &ctx = proto->getASTContext();
    if (func->getModuleContext()->getName() != ctx.Id_Concurrency)
      return false;

    return !func->getName().isSimpleName("flatMap");
  }

  void checkGenericParams(const GenericContext *ownerCtx,
                          const ValueDecl *ownerDecl) {
    if (!ownerCtx->isGenericContext())
      return;

    if (auto params = ownerCtx->getGenericParams()) {
      for (auto param : *params) {
        auto inheritedEntries = param->getInherited().getEntries();
        if (inheritedEntries.empty())
          continue;
        assert(inheritedEntries.size() == 1);
        auto inherited = inheritedEntries.front();
        checkType(inherited.getType(), inherited.getTypeRepr(), ownerDecl);
      }
    }

    if (ownerCtx->getTrailingWhereClause()) {
      // Ignore the where clause for AsyncSequence.flatMap from the
      // _Concurrency module. This is an egregious hack to allow us to
      // use overloading tricks to retain the behavior previously
      // afforded by rethrowing conformances.
      if (isAsyncSequenceFlatMap(ownerCtx))
        return;

      forAllRequirementTypes(WhereClauseOwner(
                               const_cast<GenericContext *>(ownerCtx)),
                             [&](Type type, TypeRepr *typeRepr) {
        checkType(type, typeRepr, ownerDecl);
      });
    }
  }

public:
  explicit DeclAvailabilityChecker(ExportContext where)
    : Where(where) {}

  void checkGlobalActor(Decl *D) {
    auto globalActor = D->getGlobalActorAttr();
    if (!globalActor)
      return;

    // Avoid checking the availability for a @MainActor constraint since it does
    // not carry an inherent ABI impact.
    if (globalActor->second->isMainActor())
      return;

    auto customAttr = globalActor->first;
    checkType(customAttr->getType(), customAttr->getTypeRepr(), D);
  }

  void visit(Decl *D) {
    DeclVisitor<DeclAvailabilityChecker>::visit(D);
    checkGlobalActor(D);
  }
  
  // Force all kinds to be handled at a lower level.
  void visitDecl(Decl *D) = delete;
  void visitValueDecl(ValueDecl *D) = delete;

#define UNREACHABLE(KIND, REASON) \
  void visit##KIND##Decl(KIND##Decl *D) { \
    llvm_unreachable(REASON); \
  }
  UNREACHABLE(Import, "not applicable")
  UNREACHABLE(TopLevelCode, "not applicable")
  UNREACHABLE(Module, "not applicable")
  UNREACHABLE(Missing, "not applicable")

  UNREACHABLE(Param, "handled by the enclosing declaration")
  UNREACHABLE(GenericTypeParam, "handled by the enclosing declaration")
  UNREACHABLE(MissingMember, "handled by the enclosing declaration")
  UNREACHABLE(MacroExpansion, "handled by the enclosing declaration")
#undef UNREACHABLE

#define UNINTERESTING(KIND) \
  void visit##KIND##Decl(KIND##Decl *D) {}

  UNINTERESTING(PrefixOperator) // Does not reference other decls.
  UNINTERESTING(PostfixOperator) // Does not reference other decls.
  UNINTERESTING(PoundDiagnostic) // Not applicable.
  UNINTERESTING(EnumCase) // Handled at the EnumElement level.
  UNINTERESTING(Destructor) // Always correct.
  UNINTERESTING(Accessor) // Handled by the Var or Subscript.
  UNINTERESTING(OpaqueType) // TODO

  // Handled at the PatternBinding level; if the pattern has a simple
  // "name: TheType" form, we can get better results by diagnosing the TypeRepr.
  UNINTERESTING(Var)


  /// \see visitPatternBindingDecl
  void checkNamedPattern(const NamedPattern *NP,
                         const llvm::DenseSet<const VarDecl *> &seenVars) {
    const VarDecl *theVar = NP->getDecl();

    // Only check the type of individual variables if we didn't check an
    // enclosing TypedPattern.
    if (seenVars.count(theVar))
      return;

    checkType(theVar->getValueInterfaceType(), /*typeRepr*/nullptr, theVar);

    for (auto attr : theVar->getAttachedPropertyWrappers()) {
      checkType(attr->getType(), attr->getTypeRepr(), theVar,
                ExportabilityReason::PropertyWrapper);
    }
  }

  /// \see visitPatternBindingDecl
  void checkTypedPattern(PatternBindingDecl *PBD,
                         const TypedPattern *TP,
                         llvm::DenseSet<const VarDecl *> &seenVars) {
    // FIXME: We need to figure out if this is a stored or computed property,
    // so we pull out some random VarDecl in the pattern. They're all going to
    // be the same, but still, ick.
    const VarDecl *anyVar = nullptr;
    TP->forEachVariable([&](VarDecl *V) {
      seenVars.insert(V);
      anyVar = V;
    });

    checkType(TP->hasType() ? TP->getType() : Type(),
              TP->getTypeRepr(), anyVar ? (Decl *)anyVar : (Decl *)PBD);

    // Check the property wrapper types.
    if (anyVar) {
      for (auto attr : anyVar->getAttachedPropertyWrappers()) {
        checkType(attr->getType(), attr->getTypeRepr(), anyVar,
                  ExportabilityReason::PropertyWrapper);
      }

      if (auto attr = anyVar->getAttachedResultBuilder()) {
        checkType(anyVar->getResultBuilderType(),
                  attr->getTypeRepr(), anyVar,
                  ExportabilityReason::ResultBuilder);
      }
    }
  }

  void visitPatternBindingDecl(PatternBindingDecl *PBD) {
    llvm::DenseSet<const VarDecl *> seenVars;
    for (auto idx : range(PBD->getNumPatternEntries())) {
      PBD->getPattern(idx)->forEachNode([&](const Pattern *P) {
        if (auto *NP = dyn_cast<NamedPattern>(P)) {
          checkNamedPattern(NP, seenVars);
          return;
        }

        auto *TP = dyn_cast<TypedPattern>(P);
        if (!TP)
          return;
        checkTypedPattern(PBD, TP, seenVars);
      });
      seenVars.clear();
    }
  }

  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    checkGenericParams(TAD, TAD);
    checkType(TAD->getUnderlyingType(),
              TAD->getUnderlyingTypeRepr(), TAD);
  }

  void visitAssociatedTypeDecl(AssociatedTypeDecl *assocType) {
    for (TypeLoc requirement : assocType->getInherited().getEntries()) {
      checkType(requirement.getType(), requirement.getTypeRepr(),
                assocType);
    }
    checkType(assocType->getDefaultDefinitionType(),
              assocType->getDefaultDefinitionTypeRepr(), assocType);

    if (assocType->getTrailingWhereClause()) {
      forAllRequirementTypes(assocType,
                             [&](Type type, TypeRepr *typeRepr) {
        checkType(type, typeRepr, assocType);
      });
    }
  }

  void visitNominalTypeDecl(const NominalTypeDecl *nominal) {
    checkGenericParams(nominal, nominal);

    DeclAvailabilityFlags flags =
        DeclAvailabilityFlag::AllowPotentiallyUnavailableProtocol;

    // Allow the `AnyColorBox` class in SwiftUI to inherit from a less available
    // super class as a one-off source compatibility exception. Availability
    // checking generally doesn't support a more available class deriving from
    // a less available base class in a library evolution enabled module, even
    // when the base class is available at the deployment target, but this
    // declaration slipped in when the compiler wasn't able to diagnose it and
    // can't be changed.
    if (nominal->getName().is("AnyColorBox") &&
        (nominal->getModuleContext()->getName().is("SwiftUI") ||
         nominal->getModuleContext()->getName().is("SwiftUICore")))
      flags |= DeclAvailabilityFlag::
          AllowPotentiallyUnavailableAtOrBelowDeploymentTarget;

    for (TypeLoc inherited : nominal->getInherited().getEntries()) {
      checkType(inherited.getType(), inherited.getTypeRepr(), nominal,
                ExportabilityReason::Inheritance, flags);
    }
  }

  void visitProtocolDecl(ProtocolDecl *proto) {
    for (TypeLoc requirement : proto->getInherited().getEntries()) {
      checkType(requirement.getType(), requirement.getTypeRepr(), proto,
                ExportabilityReason::General);
    }

    if (proto->getTrailingWhereClause()) {
      forAllRequirementTypes(proto, [&](Type type, TypeRepr *typeRepr) {
        checkType(type, typeRepr, proto);
      });
    }
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    checkGenericParams(SD, SD);

    for (auto &P : *SD->getIndices()) {
      checkType(P->getInterfaceType(), P->getTypeRepr(), SD);
    }
    checkType(SD->getElementInterfaceType(), SD->getElementTypeRepr(), SD);
  }

  void visitAbstractFunctionDecl(AbstractFunctionDecl *fn) {
    checkGenericParams(fn, fn);

    for (auto *P : *fn->getParameters()) {
      auto wrapperAttrs = P->getAttachedPropertyWrappers();
      for (auto index : indices(wrapperAttrs)) {
        auto wrapperType = P->getAttachedPropertyWrapperType(index);
        checkType(wrapperType, wrapperAttrs[index]->getTypeRepr(), fn);
      }

      if (auto attr = P->getAttachedResultBuilder())
        checkType(P->getResultBuilderType(), attr->getTypeRepr(), fn);

      checkType(P->getInterfaceType(), P->getTypeRepr(), fn);
    }

    if (auto thrownTypeRepr = fn->getThrownTypeRepr()) {
      checkType(fn->getThrownInterfaceType(), thrownTypeRepr, fn);
    }
  }

  void visitFuncDecl(FuncDecl *FD) {
    visitAbstractFunctionDecl(FD);
    checkType(FD->getResultInterfaceType(), FD->getResultTypeRepr(), FD);

    if (auto attr = FD->getAttachedResultBuilder()) {
      checkType(FD->getResultBuilderType(),
                attr->getTypeRepr(), FD,
                ExportabilityReason::ResultBuilder);
    }
  }

  void visitEnumElementDecl(EnumElementDecl *EED) {
    if (!EED->hasAssociatedValues())
      return;
    for (auto &P : *EED->getParameterList())
      checkType(P->getInterfaceType(), P->getTypeRepr(), EED);
  }

  void visitMacroDecl(MacroDecl *MD) {
    checkGenericParams(MD, MD);

    if (MD->parameterList) {
      for (auto P : *MD->parameterList) {
        checkType(P->getInterfaceType(), P->getTypeRepr(), MD);
      }
    }
    checkType(MD->getResultInterfaceType(), MD->resultType.getTypeRepr(), MD);
  }

  void checkConstrainedExtensionRequirements(ExtensionDecl *ED,
                                             bool hasExportedMembers) {
    if (!ED->getTrailingWhereClause())
      return;

    ExportabilityReason reason =
        hasExportedMembers ? ExportabilityReason::ExtensionWithPublicMembers
                           : ExportabilityReason::ExtensionWithConditionalConformances;

    forAllRequirementTypes(ED, [&](Type type, TypeRepr *typeRepr) {
      checkType(type, typeRepr, ED, reason);
    });
  }

  void visitExtensionDecl(ExtensionDecl *ED) {
    auto extendedType = ED->getExtendedNominal();
    assert(extendedType && "valid extension with no extended type?");
    if (!extendedType)
      return;

    // The rules here are tricky.
    //
    // 1) If the extension defines conformances, the conformed-to protocols
    // must be exported.
    for (TypeLoc inherited : ED->getInherited().getEntries()) {
      checkType(inherited.getType(), inherited.getTypeRepr(), ED,
                ExportabilityReason::Inheritance,
                DeclAvailabilityFlag::AllowPotentiallyUnavailableProtocol);
    }

    auto wasWhere = Where;

    // 2) If the extension contains exported members, the as-written
    // extended type should be exportable.
    bool hasExportedMembers = llvm::any_of(ED->getMembers(),
                                           [](const Decl *member) -> bool {
      auto *valueMember = dyn_cast<ValueDecl>(member);
      if (!valueMember)
        return false;
      return isExported(valueMember);
    });

    Where = wasWhere.withExported(hasExportedMembers);
    checkType(ED->getExtendedType(), ED->getExtendedTypeRepr(), ED,
              ExportabilityReason::ExtensionWithPublicMembers);

    // 3) If the extension contains exported members or defines conformances,
    // the 'where' clause must only name exported types.
    Where = wasWhere.withExported(hasExportedMembers ||
                                  !ED->getInherited().empty());
    checkConstrainedExtensionRequirements(ED, hasExportedMembers);

    // If we haven't already visited the extended nominal visit it here.
    // This logic is too wide but prevents false reports of an unused public
    // import. We should instead check for public generic requirements
    // similarly to ShouldPrintForModuleInterface::shouldPrint.
    if (!hasExportedMembers && !ED->getInherited().empty()) {
      auto DC = Where.getDeclContext();

      // Remember that the module defining the extended type must be imported
      // publicly.
      recordRequiredImportAccessLevelForDecl(
          extendedType, DC, AccessLevel::Public,
          [&](AttributedImport<ImportedModule> attributedImport) {
            ModuleDecl *importedVia = attributedImport.module.importedModule,
                       *sourceModule = ED->getModuleContext();
            ED->diagnose(diag::module_api_import, ED, importedVia, sourceModule,
                         importedVia == sourceModule,
                         /*isImplicit*/ false);
          });
    }
  }

  void checkPrecedenceGroup(const PrecedenceGroupDecl *PGD,
                            const Decl *refDecl, SourceLoc diagLoc,
                            SourceRange refRange) {
    // Bail on invalid precedence groups. This can happen when the user spells a
    // relation element that doesn't actually exist.
    if (!PGD) {
      return;
    }

    const SourceFile *SF = refDecl->getDeclContext()->getParentSourceFile();
    ModuleDecl *M = PGD->getModuleContext();
    RestrictedImportKind howImported = SF->getRestrictedImportKind(M);
    if (howImported == RestrictedImportKind::None)
      return;

    auto &DE = PGD->getASTContext().Diags;
    auto diag =
        DE.diagnose(diagLoc, diag::decl_from_hidden_module, PGD,
                    static_cast<unsigned>(ExportabilityReason::General), M->getName(),
                    static_cast<unsigned>(DisallowedOriginKind::ImplementationOnly)
                    );
    if (refRange.isValid())
      diag.highlight(refRange);
    diag.flush();
    PGD->diagnose(diag::name_declared_here, PGD->getName());
  }

  void visitInfixOperatorDecl(InfixOperatorDecl *IOD) {
    if (auto *precedenceGroup = IOD->getPrecedenceGroup()) {
      if (!IOD->getPrecedenceGroupName().empty()) {
        checkPrecedenceGroup(precedenceGroup, IOD, IOD->getLoc(),
                             IOD->getPrecedenceGroupLoc());
      }
    }
  }

  void visitPrecedenceGroupDecl(PrecedenceGroupDecl *PGD) {
    llvm::for_each(PGD->getLowerThan(),
                   [&](const PrecedenceGroupDecl::Relation &relation) {
      checkPrecedenceGroup(relation.Group, PGD, PGD->getLowerThanLoc(),
                           relation.NameLoc);
    });
    llvm::for_each(PGD->getHigherThan(),
                   [&](const PrecedenceGroupDecl::Relation &relation) {
      checkPrecedenceGroup(relation.Group, PGD, PGD->getHigherThanLoc(),
                           relation.NameLoc);
    });
  }
};

} // end anonymous namespace

static void checkExtensionGenericParamAccess(const ExtensionDecl *ED) {
  auto *AA = ED->getAttrs().getAttribute<AccessControlAttr>();
  if (!AA)
    return;
  AccessLevel userSpecifiedAccess = AA->getAccess();

  AccessScope desiredAccessScope = AccessScope::getPublic();
  switch (userSpecifiedAccess) {
  case AccessLevel::Private:
    assert((ED->isInvalid() ||
            ED->getDeclContext()->isModuleScopeContext()) &&
           "non-top-level extensions make 'private' != 'fileprivate'");
    LLVM_FALLTHROUGH;
  case AccessLevel::FilePrivate: {
    const DeclContext *DC = ED->getModuleScopeContext();
    bool isPrivate = (userSpecifiedAccess == AccessLevel::Private);
    desiredAccessScope = AccessScope(DC, isPrivate);
    break;
  }
  case AccessLevel::Internal:
    desiredAccessScope = AccessScope(ED->getModuleContext());
    break;
  case AccessLevel::Package:
    desiredAccessScope =
        AccessScope(ED->getPackageContext(/*lookupIfNotCurrent*/ true));
    break;
  case AccessLevel::Public:
  case AccessLevel::Open:
    break;
  }

  AccessControlChecker().checkGenericParamAccess(
      ED, ED, desiredAccessScope, userSpecifiedAccess);
}

DisallowedOriginKind swift::getDisallowedOriginKind(const Decl *decl,
                                                    const ExportContext &where) {
  auto downgradeToWarning = DowngradeToWarning::No;
  return getDisallowedOriginKind(decl, where, downgradeToWarning);
}

void swift::recordRequiredImportAccessLevelForDecl(
    const Decl *decl, const DeclContext *dc, AccessLevel accessLevel,
    RequiredImportAccessLevelCallback remark) {
  auto sf = dc->getParentSourceFile();
  if (!sf)
    return;

  auto definingModule = decl->getModuleContext();
  if (definingModule == dc->getParentModule())
    return;

  sf->registerRequiredAccessLevelForModule(definingModule, accessLevel);

  if (auto attributedImport = sf->getImportAccessLevel(definingModule)) {
    auto importedModule = attributedImport->module.importedModule;

    // Ignore submodules, same behavior from `getModuleContext` above.
    importedModule = importedModule->getTopLevelModule();

    // If the defining module is transitively imported, mark the locally
    // imported  module as requiring the minimum access level too.
    if (importedModule != definingModule)
      sf->registerRequiredAccessLevelForModule(importedModule, accessLevel);

    if (dc->getASTContext().LangOpts.EnableModuleApiImportRemarks)
      remark(*attributedImport);
  }
}

void swift::diagnoseUnnecessaryPublicImports(SourceFile &SF) {
  ASTContext &ctx = SF.getASTContext();
  if (ctx.TypeCheckerOpts.SkipFunctionBodies != FunctionBodySkipping::None)
    return;

  for (const auto &import : SF.getImports()) {
    // Only check imports with an explicit access level above internal.
    if (!import.accessLevelRange.isValid() ||
        import.accessLevel <= AccessLevel::Internal)
      continue;

    // Ignore submodules as we associate decls with the top module.
    // The top module will be reported if it's not used.
    auto importedModule = import.module.importedModule;
    if (importedModule->getTopLevelModule() != importedModule)
      continue;

    // Ignore @_exported as by themselves the import is meaningful.
    if (import.options.contains(ImportFlags::Exported))
      continue;

    AccessLevel levelUsed = SF.getMaxAccessLevelUsingImport(importedModule);
    if (import.accessLevel <= levelUsed)
      continue;

    auto diagId = import.accessLevel == AccessLevel::Public ?
                                          diag::remove_public_import :
                                          diag::remove_package_import;
    auto inFlight = ctx.Diags.diagnose(import.importLoc,
                                       diagId,
                                       importedModule->getName());

    if (levelUsed == AccessLevel::Package) {
      inFlight.fixItReplace(import.accessLevelRange, "package");
    } else if (ctx.LangOpts.hasFeature(Feature::InternalImportsByDefault)) {
      // Let it default to internal.
      inFlight.fixItRemove(import.accessLevelRange);
    } else {
      inFlight.fixItReplace(import.accessLevelRange, "internal");
    }
  }
}

/// Register the type extended by \p ED as being used in a package decl if
/// any member is a package decl. This patches a hole in the warnings on
/// superfluously public imports which usually relies on exportability checking
/// that is not currently executed for package decls.
void registerPackageAccessForPackageExtendedType(ExtensionDecl *ED) {
  auto extendedType = ED->getExtendedNominal();
  if (!extendedType)
    return;

  bool hasPackageMembers = llvm::any_of(ED->getMembers(),
                                        [](const Decl *member) -> bool {
    auto *VD = dyn_cast<ValueDecl>(member);
    if (!VD)
      return false;

    AccessScope accessScope =
        VD->getFormalAccessScope(nullptr,
                                 /*treatUsableFromInlineAsPublic*/true);
    return accessScope.isPackage();
  });
  if (!hasPackageMembers)
    return;

  DeclContext *DC = ED->getDeclContext();

  // Remember that the module defining the decl must be imported with at least
  // package visibility.
  recordRequiredImportAccessLevelForDecl(
      extendedType, DC, AccessLevel::Package,
      [&](AttributedImport<ImportedModule> attributedImport) {
        ModuleDecl *importedVia = attributedImport.module.importedModule,
                   *sourceModule = ED->getModuleContext();
        ED->diagnose(diag::module_api_import, ED, importedVia, sourceModule,
                     importedVia == sourceModule,
                     /*isImplicit*/ false);
      });
}

void swift::checkAccessControl(Decl *D) {
  if (isa<ValueDecl>(D) || isa<PatternBindingDecl>(D)) {
    bool allowInlineable =
        D->getDeclContext()->isInSpecializeExtensionContext();
    AccessControlChecker(allowInlineable).visit(D);
    UsableFromInlineChecker().visit(D);
  } else if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
    checkExtensionGenericParamAccess(ED);
    registerPackageAccessForPackageExtendedType(ED);
  }

  if (isa<AccessorDecl>(D))
    return;

  auto where = ExportContext::forDeclSignature(D);
  if (where.isImplicit())
    return;

  DeclAvailabilityChecker(where).visit(D);
}
