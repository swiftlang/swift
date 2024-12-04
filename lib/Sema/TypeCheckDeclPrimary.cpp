//===--- TypeCheckDeclPrimary.cpp - Type Checking for Primary Files -------===//
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
// This file implements type checking for primary files, that is, files whose
// declarations we're planning to emit. This exhaustively triggers diagnostics
// and type checking of all delayed bodies in those files.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"
#include "DerivedConformances.h"
#include "MiscDiagnostics.h"
#include "TypeCheckAccess.h"
#include "TypeCheckAvailability.h"
#include "TypeCheckConcurrency.h"
#include "TypeCheckDecl.h"
#include "TypeCheckMacros.h"
#include "TypeCheckObjC.h"
#include "TypeCheckType.h"
#include "TypeChecker.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/AccessNotes.h"
#include "swift/AST/AccessScope.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/MacroDefinition.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeDifferenceVisitor.h"
#include "swift/AST/TypeWalker.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Statistic.h"
#include "swift/Bridging/MacroEvaluation.h"
#include "swift/Parse/Lexer.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Strings.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/DJB.h"

using namespace swift;

#define DEBUG_TYPE "TypeCheckDeclPrimary"

static Type containsParameterizedProtocolType(Type inheritedTy) {
  if (inheritedTy->is<ParameterizedProtocolType>()) {
    return inheritedTy;
  }

  if (auto *compositionTy = inheritedTy->getAs<ProtocolCompositionType>()) {
    for (auto memberTy : compositionTy->getMembers()) {
      if (auto paramTy = containsParameterizedProtocolType(memberTy))
        return paramTy;
    }
  }

  return Type();
}

class CheckRepressions {
  llvm::PointerUnion<const TypeDecl *, const ExtensionDecl *> declUnion;
  ASTContext &ctx;

  llvm::DenseSet<RepressibleProtocolKind> seen;

public:
  CheckRepressions(
      llvm::PointerUnion<const TypeDecl *, const ExtensionDecl *> declUnion,
      ASTContext &ctx)
      : declUnion(declUnion), ctx(ctx) {}

  template <typename... ArgTypes>
  InFlightDiagnostic diagnoseInvalid(TypeRepr &repr, ArgTypes &&...Args) {
    auto &diags = ctx.Diags;
    repr.setInvalid();
    return diags.diagnose(std::forward<ArgTypes>(Args)...);
  }

  /// Record the repressed kind indicated by the provided
  /// InheritedTypeResult.Suppressed (i.e. \p ty and \p repr) if it is in fact
  /// repressed or return the inverted type.
  Type add(Type ty, InverseTypeRepr &repr,
           llvm::PointerUnion<const TypeDecl *, const ExtensionDecl *> decl) {
    if (!ty || ty->hasError())
      return Type();
    assert(!ty->is<ExistentialMetatypeType>());
    auto kp = ty->getKnownProtocol();
    if (!kp) {
      diagnoseInvalid(repr, repr.getLoc(), diag::inverse_type_not_invertible,
                      ty);
      return Type();
    }
    auto ipk = getInvertibleProtocolKind(*kp);
    if (ipk) {
      return ProtocolCompositionType::getInverseOf(ctx, *ipk);
    }
    auto rpk = getRepressibleProtocolKind(*kp);
    if (!rpk) {
      diagnoseInvalid(repr, repr.getLoc(),
                      diag::suppress_nonsuppressable_protocol,
                      ctx.getProtocol(*kp));
      return Type();
    }
    if (auto *extension = dyn_cast<const ExtensionDecl *>(decl)) {
      diagnoseInvalid(repr, extension,
                      diag::suppress_inferrable_protocol_extension,
                      ctx.getProtocol(*kp));
      return Type();
    }
    if (!seen.insert(*rpk).second) {
      diagnoseInvalid(repr, repr.getLoc(),
                      diag::suppress_already_suppressed_protocol,
                      ctx.getProtocol(getKnownProtocolKind(*rpk)));
    }
    return Type();
  }
};

/// If the extension adds a conformance to an invertible protocol, ensure that
/// it does not add a conformance to any other protocol. So these are illegal:
///
///     extension S: Copyable & P {}
///     extension S: Q, Copyable {}
///
/// This policy is in place because extensions adding a conformance to an
/// invertible protocol do _not_ add default requirements on generic parameters,
/// so it would be confusing to mix them together in the same extension.
static void checkExtensionAddsSoloInvertibleProtocol(const ExtensionDecl *ext) {
  auto localConfs = ext->getLocalConformances();
  if (localConfs.size() <= 1)
    return;

  for (auto *conf : localConfs) {
    if (auto ip = conf->getProtocol()->getInvertibleProtocolKind()) {
      ext->diagnose(diag::extension_conforms_to_invertible_and_others,
                    getInvertibleProtocolKindName(*ip));
    }
  }
}

/// Check the inheritance clause of a type declaration or extension thereof.
///
/// This routine performs detailed checking of the inheritance clause of the
/// given type or extension. It need only be called within the primary source
/// file.
static void checkInheritanceClause(
    llvm::PointerUnion<const TypeDecl *, const ExtensionDecl *> declUnion) {
  auto inheritedTypes = InheritedTypes(declUnion);
  auto inheritedClause = inheritedTypes.getEntries();
  const ExtensionDecl *ext = nullptr;
  const TypeDecl *typeDecl = nullptr;
  const Decl *decl;
  if ((ext = declUnion.dyn_cast<const ExtensionDecl *>())) {
    decl = ext;

    // Protocol extensions cannot have inheritance clauses.
    if (auto proto = ext->getExtendedProtocolDecl()) {
      if (!inheritedClause.empty()) {
        ext->diagnose(diag::extension_protocol_inheritance,
                 proto->getName())
          .highlight(SourceRange(inheritedClause.front().getSourceRange().Start,
                                 inheritedClause.back().getSourceRange().End));
        return;
      }
    }
  } else {
    typeDecl = declUnion.get<const TypeDecl *>();
    decl = typeDecl;
  }

  // Can this declaration's inheritance clause contain a class or
  // subclass existential?
  bool canHaveSuperclass = (isa<ClassDecl>(decl) ||
                            (isa<ProtocolDecl>(decl) &&
                             !cast<ProtocolDecl>(decl)->isObjC()));

  ASTContext &ctx = decl->getASTContext();
  auto &diags = ctx.Diags;

  CheckRepressions checkRepressions(declUnion, ctx);

  // Check all of the types listed in the inheritance clause.
  Type superclassTy;
  SourceRange superclassRange;
  std::optional<std::pair<unsigned, SourceRange>> inheritedAnyObject;
  for (unsigned i = 0, n = inheritedClause.size(); i != n; ++i) {
    auto &inherited = inheritedClause[i];

    // Validate the type.
    InheritedTypeRequest request{declUnion, i, TypeResolutionStage::Interface};
    auto result = evaluateOrDefault(ctx.evaluator, request,
                                    InheritedTypeResult::forDefault());

    Type inheritedTy;
    switch (result) {
    case InheritedTypeResult::Inherited:
      inheritedTy = result.getInheritedType();
      break;
    case InheritedTypeResult::Suppressed: {
      auto pair = result.getSuppressed();

      auto inverted = checkRepressions.add(pair.first, *pair.second, declUnion);
      if (!inverted)
        continue;
      inheritedTy = inverted;
      break;
    }
    case InheritedTypeResult::Default:
      continue;
    }

    // If we couldn't resolve an the inherited type, or it contains an error,
    // ignore it.
    if (!inheritedTy || inheritedTy->hasError())
      continue;

    // For generic parameters and associated types, the GSB checks constraints;
    // however, we still want to fire off the requests to produce diagnostics
    // in some circular validation cases.
    if (isa<GenericTypeParamDecl>(decl) ||
        isa<AssociatedTypeDecl>(decl))
      continue;

    // Check whether we inherited from 'AnyObject' twice.
    // Other redundant-inheritance scenarios are checked below, the
    // GenericSignatureBuilder (for protocol inheritance) or the
    // ConformanceLookupTable (for protocol conformance).
    if (inheritedTy->isAnyObject()) {
      // Warn inherited AnyObject written as 'class' as deprecated
      // for Swift >= 5.
      auto sourceRange = inherited.getSourceRange();
      bool isWrittenAsClass =
          isa<ProtocolDecl>(decl) &&
          Lexer::getTokenAtLocation(ctx.SourceMgr, sourceRange.Start)
              .is(tok::kw_class);
      if (ctx.LangOpts.isSwiftVersionAtLeast(5) && isWrittenAsClass) {
        diags
            .diagnose(sourceRange.Start,
                      diag::anyobject_class_inheritance_deprecated)
            .fixItReplace(sourceRange, "AnyObject");
      }

      if (inheritedAnyObject) {
        // If the first occurrence was written as 'class', downgrade the error
        // to a warning in such case for backward compatibility with
        // Swift <= 4.
        auto knownIndex = inheritedAnyObject->first;
        auto knownRange = inheritedAnyObject->second;
        SourceRange removeRange = inheritedTypes.getRemovalRange(knownIndex);
        if (!ctx.LangOpts.isSwiftVersionAtLeast(5) &&
            isa<ProtocolDecl>(decl) &&
            Lexer::getTokenAtLocation(ctx.SourceMgr, knownRange.Start)
              .is(tok::kw_class)) {
          SourceLoc classLoc = knownRange.Start;

          diags.diagnose(classLoc, diag::duplicate_anyobject_class_inheritance)
            .fixItRemoveChars(removeRange.Start, removeRange.End);
        } else {
          diags.diagnose(inherited.getSourceRange().Start,
                 diag::duplicate_inheritance, inheritedTy)
            .fixItRemoveChars(removeRange.Start, removeRange.End);
        }
        continue;
      }

      // Note that we saw inheritance from 'AnyObject'.
      inheritedAnyObject = { i, inherited.getSourceRange() };
    }

    if (auto paramTy = containsParameterizedProtocolType(inheritedTy)) {
      if (!isa<ProtocolDecl>(decl)) {
        decl->diagnose(diag::inheritance_from_parameterized_protocol,
                       paramTy);
      }
      continue;
    }

    if (inheritedTy->isConstraintType()) {
      auto layout = inheritedTy->getExistentialLayout();

      // An inverse on an extension is an error.
      if (isa<ExtensionDecl>(decl)) {
        auto canInheritedTy = inheritedTy->getCanonicalType();
        if (auto pct = canInheritedTy->getAs<ProtocolCompositionType>()) {
          for (auto inverse : pct->getInverses()) {
            decl->diagnose(diag::inverse_extension,
                           getProtocolName(getKnownProtocolKind(inverse)));
          }
        }
      }

      // Subclass existentials are not allowed except on classes and
      // non-@objc protocols.
      if (layout.explicitSuperclass &&
          !canHaveSuperclass) {
        decl->diagnose(diag::inheritance_from_protocol_with_superclass,
                       inheritedTy);
        continue;
      }

      // Classes and protocols can inherit from subclass existentials.
      // For classes, we check for a duplicate superclass below.
      // For protocols, the requirement machine emits a requirement
      // conflict instead.
      if (isa<ProtocolDecl>(decl))
        continue;

      // AnyObject is not allowed except on protocols.
      if (layout.hasExplicitAnyObject && !isa<ClassDecl>(decl)) {
        decl->diagnose(diag::inheritance_from_anyobject);
        continue;
      }

      // If the existential did not have a class constraint, we're done.
      if (!layout.explicitSuperclass)
        continue;

      assert(isa<ClassDecl>(decl));
      assert(canHaveSuperclass);
      inheritedTy = layout.explicitSuperclass;
    }

    // If this is an enum inheritance clause, check for a raw type.
    if (auto enumDecl = dyn_cast<EnumDecl>(decl)) {
      // Check if we already had a raw type.
      if (superclassTy) {
        if (superclassTy->isEqual(inheritedTy)) {
          auto removeRange = inheritedTypes.getRemovalRange(i);
          diags.diagnose(inherited.getSourceRange().Start,
                         diag::duplicate_inheritance, inheritedTy)
            .fixItRemoveChars(removeRange.Start, removeRange.End);
        } else {
          diags.diagnose(inherited.getSourceRange().Start,
                         diag::multiple_enum_raw_types, superclassTy,
                         inheritedTy)
            .highlight(superclassRange);
        }
        continue;
      }

      // Noncopyable types cannot have a raw type until there is support for
      // generics, since the raw type here is only useful if we'll generate
      // a conformance to RawRepresentable, which is currently disabled.
      if (!enumDecl->canBeCopyable()) {
        // TODO: getRemovalRange is not yet aware of ~Copyable entries so it
        // will accidentally delete commas or colons that are needed.
        diags.diagnose(inherited.getSourceRange().Start,
                       diag::enum_raw_type_nonconforming_and_noncopyable,
                       enumDecl->getDeclaredInterfaceType(), inheritedTy)
             .highlight(inherited.getSourceRange());
      }
      
      // If this is not the first entry in the inheritance clause, complain.
      if (i > 0) {
        auto removeRange = inheritedTypes.getRemovalRange(i);

        diags.diagnose(inherited.getSourceRange().Start,
                       diag::raw_type_not_first, inheritedTy)
          .fixItRemoveChars(removeRange.Start, removeRange.End)
          .fixItInsert(inheritedClause[0].getSourceRange().Start,
                       inheritedTy.getString() + ", ");
      }

      // Save the raw type locally.
      superclassTy = inheritedTy;
      superclassRange = inherited.getSourceRange();
      continue;
    }

    // If this is a class type, it may be the superclass. We end up here when
    // the inherited type is either itself a class, or when it is a subclass
    // existential via the existential type path above.
    if (inheritedTy->getClassOrBoundGenericClass()) {
      // First, check if we already had a superclass.
      if (superclassTy) {
        // FIXME: Check for shadowed protocol names, i.e., NSObject?

        if (superclassTy->isEqual(inheritedTy)) {
          // Duplicate superclass.
          auto removeRange = inheritedTypes.getRemovalRange(i);
          diags.diagnose(inherited.getSourceRange().Start,
                         diag::duplicate_inheritance, inheritedTy)
            .fixItRemoveChars(removeRange.Start, removeRange.End);
        } else {
          // Complain about multiple inheritance.
          // Don't emit a Fix-It here. The user has to think harder about this.
          diags.diagnose(inherited.getSourceRange().Start,
                         diag::multiple_inheritance, superclassTy, inheritedTy)
            .highlight(superclassRange);
        }
        continue;
      }

      // If this is not the first entry in the inheritance clause, complain.
      if (isa<ClassDecl>(decl) && i > 0) {
        auto removeRange = inheritedTypes.getRemovalRange(i);
        diags.diagnose(inherited.getSourceRange().Start,
                       diag::superclass_not_first, inheritedTy)
          .fixItRemoveChars(removeRange.Start, removeRange.End)
          .fixItInsert(inheritedClause[0].getSourceRange().Start,
                       inheritedTy.getString() + ", ");

        // Fall through to record the superclass.
      }

      if (canHaveSuperclass) {
        // Record the superclass.
        superclassTy = inheritedTy;
        superclassRange = inherited.getSourceRange();
        continue;
      }
    }

    // We can't inherit from a non-class, non-protocol type.
    decl->diagnose(canHaveSuperclass
                   ? diag::inheritance_from_non_protocol_or_class
                   : diag::inheritance_from_non_protocol,
                   inheritedTy);
    // FIXME: Note pointing to the declaration 'inheritedTy' references?
  }
}

static void installCodingKeysIfNecessary(NominalTypeDecl *NTD) {
  auto req =
    ResolveImplicitMemberRequest{NTD, ImplicitMemberAction::ResolveCodingKeys};
  (void)evaluateOrDefault(NTD->getASTContext().evaluator, req, {});
}

// TODO(distributed): same ugly hack as Codable does...
static void installDistributedActorIfNecessary(NominalTypeDecl *NTD) {
  auto req =
    ResolveImplicitMemberRequest{NTD, ImplicitMemberAction::ResolveDistributedActor};
  (void)evaluateOrDefault(NTD->getASTContext().evaluator, req, {});
}

// Check for static properties that produce empty option sets
// using a rawValue initializer with a value of '0'
static void checkForEmptyOptionSet(const VarDecl *VD) {
  // Check if property is a 'static let'
  if (!VD->isStatic() || !VD->isLet())
    return;
  
  auto DC = VD->getDeclContext();
  
  // Make sure property is of same type as the type it is declared in
  if (!VD->getInterfaceType()->isEqual(DC->getSelfInterfaceType()))
    return;
  
  // Make sure this type conforms to OptionSet
  bool conformsToOptionSet =
    (bool)TypeChecker::conformsToKnownProtocol(DC->getSelfTypeInContext(),
                                               KnownProtocolKind::OptionSet);
  
  if (!conformsToOptionSet)
    return;
  
  auto PBD = VD->getParentPatternBinding();
  if (!PBD)
    return;
  
  auto initIndex = PBD->getPatternEntryIndexForVarDecl(VD);
  auto init = PBD->getInit(initIndex);

  // Make sure property is being set with a constructor
  auto ctor = dyn_cast_or_null<CallExpr>(init);
  if (!ctor)
    return;
  auto ctorCalledVal = ctor->getCalledValue();
  if (!ctorCalledVal)
    return;
  if (!isa<ConstructorDecl>(ctorCalledVal))
    return;
  
  // Make sure it is calling the rawValue constructor
  auto *args = ctor->getArgs();
  if (!args->isUnary())
    return;
  if (args->getLabel(0) != VD->getASTContext().Id_rawValue)
    return;
  
  // Make sure the rawValue parameter is a '0' integer literal
  auto intArg = dyn_cast<IntegerLiteralExpr>(args->getExpr(0));
  if (!intArg)
    return;
  if (intArg->getValue() != 0)
    return;
  
  VD->diagnose(diag::option_set_zero_constant, VD->getName());
  VD->diagnose(diag::option_set_empty_set_init)
    .fixItReplace(args->getSourceRange(), "([])");
}

template<typename T>
static void diagnoseDuplicateDecls(T &&decls) {
  llvm::SmallDenseMap<DeclBaseName, const ValueDecl *> names;
  for (auto *current : decls) {
    if (!current->hasName() || current->isImplicit())
      continue;

    auto found = names.insert(std::make_pair(current->getBaseName(), current));
    if (!found.second) {
      auto *other = found.first->second;

      current->getASTContext().Diags.diagnoseWithNotes(
        current->diagnose(diag::invalid_redecl, current), [&]() {
        other->diagnose(diag::invalid_redecl_prev, other);
      });

      // Mark the decl as invalid. This is needed to avoid emitting a
      // duplicate diagnostic when running redeclaration checking in
      // the case where the VarDecl is part of the enclosing context,
      // e.g `let (x, x) = (0, 0)`.
      if (!isa<GenericTypeParamDecl>(current))
        current->setInvalid();
    }
  }
}

/// Check the inheritance clauses generic parameters along with any
/// requirements stored within the generic parameter list.
static void checkGenericParams(GenericContext *ownerCtx) {
  const auto genericParams = ownerCtx->getGenericParams();
  if (!genericParams)
    return;

  auto *decl = ownerCtx->getAsDecl();
  bool hasPack = false;

  for (auto gp : *genericParams) {
    // Diagnose generic types with a parameter packs if VariadicGenerics
    // is not enabled.
    if (gp->isParameterPack()) {
      // Variadic nominal types require runtime support.
      if (isa<NominalTypeDecl>(decl)) {
        TypeChecker::checkAvailability(
            gp->getSourceRange(),
            ownerCtx->getASTContext().getVariadicGenericTypeAvailability(),
            diag::availability_variadic_type_only_version_newer,
            ownerCtx);
      }

      // Variadic nominal and type alias types can only have a single
      // parameter pack.
      if (hasPack && isa<GenericTypeDecl>(decl)) {
        gp->diagnose(diag::more_than_one_pack_in_type);
      }

      hasPack = true;
    }

    if (gp->isValue()) {
      // Value generic nominal types require runtime support.
      if (isa<NominalTypeDecl>(decl)) {
        TypeChecker::checkAvailability(
          gp->getSourceRange(),
          ownerCtx->getASTContext().getValueGenericTypeAvailability(),
          diag::availability_value_generic_type_only_version_newer,
          ownerCtx);
      }
    }

    TypeChecker::checkDeclAttributes(gp);
    checkInheritanceClause(gp);
  }

  // Force resolution of interface types written in requirements here.
  WhereClauseOwner(ownerCtx)
      .visitRequirements(TypeResolutionStage::Interface,
                         [](Requirement, RequirementRepr *) { return false; });

  // Check for duplicate generic parameter names.
  TypeChecker::checkShadowedGenericParams(ownerCtx);
}

template <typename T>
static void checkOperatorOrPrecedenceGroupRedeclaration(
    T *decl, Diag<> diagID, Diag<> noteID,
    llvm::function_ref<TinyPtrVector<T *>(OperatorLookupDescriptor)>
        lookupOthers) {
  if (decl->isInvalid())
    return;

  auto *currentFile = decl->getDeclContext()->getParentSourceFile();
  assert(currentFile);

  auto *module = currentFile->getParentModule();
  auto &ctx = module->getASTContext();
  auto desc = OperatorLookupDescriptor::forModule(module, decl->getName());
  auto otherDecls = lookupOthers(desc);
  for (auto *other : otherDecls) {
    if (other == decl || other->isInvalid())
      continue;

    bool shouldDiagnose = false;
    if (currentFile == other->getDeclContext()->getParentSourceFile()) {
      // For a same-file redeclaration, make sure we get the diagnostic ordering
      // to be sensible.
      if (decl->getLoc().isValid() && other->getLoc().isValid() &&
          ctx.SourceMgr.isBeforeInBuffer(decl->getLoc(), other->getLoc())) {
        std::swap(decl, other);
      }
      shouldDiagnose = true;
    } else {
      // If the declarations are in different files, only diagnose if we've
      // enabled the new operator lookup behaviour where decls in the current
      // module are now favored over imports.
      shouldDiagnose = ctx.LangOpts.EnableNewOperatorLookup;
    }

    if (shouldDiagnose) {
      ctx.Diags.diagnose(decl, diagID);
      ctx.Diags.diagnose(other, noteID);
      decl->setInvalid();
      return;
    }
  }
}

static void checkRedeclaration(OperatorDecl *op) {
  checkOperatorOrPrecedenceGroupRedeclaration<OperatorDecl>(
      op, diag::operator_redeclared, diag::previous_operator_decl,
      [&](OperatorLookupDescriptor desc) {
        DirectOperatorLookupRequest req{desc, op->getFixity()};
        return evaluateOrDefault(op->getASTContext().evaluator, req, {});
      });
}

static void checkRedeclaration(PrecedenceGroupDecl *group) {
  checkOperatorOrPrecedenceGroupRedeclaration<PrecedenceGroupDecl>(
      group, diag::precedence_group_redeclared,
      diag::previous_precedence_group_decl, [&](OperatorLookupDescriptor desc) {
        DirectPrecedenceGroupLookupRequest req{desc};
        return evaluateOrDefault(group->getASTContext().evaluator, req, {});
      });
}

/// Check whether \c current is a redeclaration.
evaluator::SideEffect
CheckRedeclarationRequest::evaluate(Evaluator &eval, ValueDecl *current,
                                    NominalTypeDecl *SelfNominalType) const {
  // Ignore invalid and anonymous declarations.
  if (current->isInvalid() || !current->hasName())
    return std::make_tuple<>();

  // If this declaration isn't from a source file, don't check it.
  // FIXME: Should restrict this to the source file we care about.
  DeclContext *currentDC = current->getDeclContext();
  SourceFile *currentFile = currentDC->getParentSourceFile();
  if (!currentFile)
    return std::make_tuple<>();

  if (auto func = dyn_cast<AbstractFunctionDecl>(current)) {
    if (func->isDistributedThunk()) {
      return std::make_tuple<>();
    }
  }

  auto &ctx = current->getASTContext();

  // Find other potential definitions.
  SmallVector<ValueDecl *, 4> otherDefinitions;
  if (currentDC->isTypeContext()) {
    // Look within a type context.
    if (auto nominal = currentDC->getSelfNominalTypeDecl()) {
      auto found = nominal->lookupDirect(current->getBaseName());
      otherDefinitions.append(found.begin(), found.end());
    }
  } else if (currentDC->isLocalContext()) {
    if (!current->isImplicit()) {
      ASTScope::lookupLocalDecls(currentFile, current->getBaseName(),
                                 current->getLoc(),
                                 /*stopAfterInnermostBraceStmt=*/true,
                                 otherDefinitions);
    }
  } else {
    assert(currentDC->isModuleScopeContext());
    // Look within a module context.
    currentFile->getParentModule()->lookupValue(current->getBaseName(),
                                                NLKind::QualifiedLookup,
                                                otherDefinitions);
  }

  // Compare this signature against the signature of other
  // declarations with the same name.
  OverloadSignature currentSig = current->getOverloadSignature();
  CanType currentSigType = current->getOverloadSignatureType();
  ModuleDecl *currentModule = current->getModuleContext();
  for (auto other : otherDefinitions) {
    // Skip invalid declarations and ourselves.
    //
    // FIXME: Breaking a cycle here with hasInterfaceType() is bogus.
    if (current == other || (other->hasInterfaceType() && other->isInvalid()))
      continue;

    auto *otherDC = other->getDeclContext();

    // Skip declarations in other modules.
    if (currentModule != otherDC->getParentModule())
      continue;

    // If both declarations are in the same file, only diagnose the second one.
    if (currentFile == otherDC->getParentSourceFile())
      if (current->getLoc().isValid() &&
          ctx.SourceMgr.isBeforeInBuffer(
            current->getLoc(), other->getLoc()))
        continue;

    // Don't compare methods vs. non-methods (which only happens with
    // operators).
    if (currentDC->isTypeContext() != otherDC->isTypeContext())
      continue;

    // In local context, only consider exact name matches.
    if (currentDC->isLocalContext() &&
        current->getName() != other->getName())
      continue;

    // Check whether the overload signatures conflict (ignoring the type for
    // now).
    auto otherSig = other->getOverloadSignature();
    if (!conflicting(currentSig, otherSig))
      continue;

    // Skip inaccessible declarations in other files.
    // In practice, this means we will warn on a private declaration that
    // shadows a non-private one, but only in the file where the shadowing
    // happens. We will warn on conflicting non-private declarations in both
    // files.
    if (!currentDC->isLocalContext() &&
        !other->isAccessibleFrom(currentDC))
      continue;

    // Skip invalid declarations.
    if (other->isInvalid())
      continue;

    // Allow redeclarations of typealiases in different constrained
    // extensions.
    if (isa<TypeAliasDecl>(current) &&
        isa<TypeAliasDecl>(other) &&
        currentDC != otherDC &&
        currentDC->getGenericSignatureOfContext().getCanonicalSignature() !=
        otherDC->getGenericSignatureOfContext().getCanonicalSignature())
      continue;

    // Thwart attempts to override the same declaration more than once.
    const auto *currentOverride = current->getOverriddenDecl();
    const auto *otherOverride = other->getOverriddenDecl();
    const auto *otherInit = dyn_cast<ConstructorDecl>(other);
    if (currentOverride && currentOverride == otherOverride &&
        !(otherInit && otherInit->isImplicit())) {
      current->diagnose(diag::multiple_override, current->getName());
      other->diagnose(diag::multiple_override_prev, other->getName());
      current->setInvalid();
      break;
    }

    // Get the overload signature type.
    CanType otherSigType = other->getOverloadSignatureType();

    bool wouldBeSwift5Redeclaration = false;
    auto isRedeclaration = conflicting(ctx, currentSig, currentSigType,
                                       otherSig, otherSigType,
                                       &wouldBeSwift5Redeclaration);
    // If there is another conflict, complain.
    if (isRedeclaration || wouldBeSwift5Redeclaration) {
      // If we're currently looking at a .sil and the conflicting declaration
      // comes from a .sib, don't error since we won't be considering the sil
      // from the .sib. So it's fine for the .sil to shadow it, since that's the
      // one we want.
      if (currentFile->Kind == SourceFileKind::SIL) {
        auto *otherFile = dyn_cast<SerializedASTFile>(
            other->getDeclContext()->getModuleScopeContext());
        if (otherFile && otherFile->isSIB())
          continue;
      }

      // Signatures are the same, but interface types are not. We must
      // have a type that we've massaged as part of signature
      // interface type generation.
      if (current->getInterfaceType()->isEqual(other->getInterfaceType())) {
        if (currentDC->isTypeContext() == other->getDeclContext()->isTypeContext()) {
          auto currFnTy = current->getInterfaceType()->getAs<AnyFunctionType>();
          auto otherFnTy = other->getInterfaceType()->getAs<AnyFunctionType>();
          if (currFnTy && otherFnTy && currentDC->isTypeContext()) {
            currFnTy = currFnTy->getResult()->getAs<AnyFunctionType>();
            otherFnTy = otherFnTy->getResult()->getAs<AnyFunctionType>();
          }
          
          if (currFnTy && otherFnTy) {
            ArrayRef<AnyFunctionType::Param> currParams = currFnTy->getParams();
            ArrayRef<AnyFunctionType::Param> otherParams = otherFnTy->getParams();
            
            if (currParams.size() == otherParams.size()) {
              auto diagnosed = false;
              for (unsigned i : indices(currParams)) {
                  
                bool currIsIUO = false;
                bool otherIsIUO = false;
                bool optionalRedecl = false;
                
                if (currParams[i].getPlainType()->getOptionalObjectType()) {
                  optionalRedecl = true;
                  auto *param = swift::getParameterAt(current, i);
                  assert(param);
                  if (param->isImplicitlyUnwrappedOptional())
                    currIsIUO = true;
                }
                
                if (otherParams[i].getPlainType()->getOptionalObjectType()) {
                  auto *param = swift::getParameterAt(other, i);
                  assert(param);
                  if (param->isImplicitlyUnwrappedOptional())
                    otherIsIUO = true;
                }
                else {
                  optionalRedecl = false;
                }
                
                if (optionalRedecl && currIsIUO != otherIsIUO) {
                  ctx.Diags.diagnoseWithNotes(
                    current->diagnose(diag::invalid_redecl, current), [&]() {
                    other->diagnose(diag::invalid_redecl_prev, other);
                  });
                  current->diagnose(diag::invalid_redecl_by_optionality_note,
                                    otherIsIUO, currIsIUO);
                  
                  current->setInvalid();
                  diagnosed = true;
                  break;
                }
              }

              if (diagnosed)
                break;
            }
          }
        }
      }

      // If the conflicting declarations have non-overlapping availability and,
      // we allow the redeclaration to proceed if...
      //
      // - they are initializers with different failability,
      bool isAcceptableVersionBasedChange = false;
      {
        const auto *currentInit = dyn_cast<ConstructorDecl>(current);
        const auto *otherInit = dyn_cast<ConstructorDecl>(other);
        if (currentInit && otherInit &&
            (currentInit->isFailable() !=
             otherInit->isFailable())) {
          isAcceptableVersionBasedChange = true;
        }
      }
      // - one throws and the other does not,
      {
        const auto *currentAFD = dyn_cast<AbstractFunctionDecl>(current);
        const auto *otherAFD = dyn_cast<AbstractFunctionDecl>(other);
        if (currentAFD && otherAFD &&
            currentAFD->hasThrows() != otherAFD->hasThrows()) {
          isAcceptableVersionBasedChange = true;
        }
      }
      // - or they are computed properties of different types,
      {
        const auto *currentVD = dyn_cast<VarDecl>(current);
        const auto *otherVD = dyn_cast<VarDecl>(other);
        if (currentVD && otherVD &&
            !currentVD->hasStorage() &&
            !otherVD->hasStorage() &&
            !currentVD->getInterfaceType()->isEqual(
              otherVD->getInterfaceType())) {
          isAcceptableVersionBasedChange = true;
        }
      }

      if (isAcceptableVersionBasedChange) {
        class AvailabilityRange {
          std::optional<llvm::VersionTuple> introduced;
          std::optional<llvm::VersionTuple> obsoleted;

        public:
          static AvailabilityRange from(const ValueDecl *VD) {
            AvailabilityRange result;
            for (auto *attr : VD->getAttrs().getAttributes<AvailableAttr>()) {
              if (attr->isLanguageVersionSpecific()) {
                if (attr->Introduced)
                  result.introduced = attr->Introduced;
                if (attr->Obsoleted)
                  result.obsoleted = attr->Obsoleted;
              }
            }
            return result;
          }

          bool fullyPrecedes(const AvailabilityRange &other) const {
            if (!obsoleted.has_value())
              return false;
            if (!other.introduced.has_value())
              return false;
            return *obsoleted <= *other.introduced;
          }

          bool overlaps(const AvailabilityRange &other) const {
            return !fullyPrecedes(other) && !other.fullyPrecedes(*this);
          }
        };

        auto currentAvail = AvailabilityRange::from(current);
        auto otherAvail = AvailabilityRange::from(other);
        if (!currentAvail.overlaps(otherAvail))
          continue;
      }

      // If both are VarDecls, and both have exactly the same type, then
      // matching the Swift 4 behaviour (i.e. just emitting the future-compat
      // warning) will result in SILGen crashes due to both properties mangling
      // the same, so it's better to just follow the Swift 5 behaviour and emit
      // the actual error.
      if (wouldBeSwift5Redeclaration && isa<VarDecl>(current) &&
          isa<VarDecl>(other) &&
          current->getInterfaceType()->isEqual(other->getInterfaceType())) {
        wouldBeSwift5Redeclaration = false;
      }

      // Distributed declarations cannot be overloaded on async-ness only,
      // because it'd cause problems with the always async distributed thunks.
      // Provide an extra diagnostic if this is the case we're facing.
      bool diagnoseDistributedAsyncOverload = false;
      if (auto func = dyn_cast<AbstractFunctionDecl>(other)) {
        diagnoseDistributedAsyncOverload = func->isDistributed();
      } else  if (auto var = dyn_cast<VarDecl>(other)) {
        diagnoseDistributedAsyncOverload = var->isDistributed();
      }

      // If this isn't a redeclaration in the current version of Swift, but
      // would be in Swift 5 mode, emit a warning instead of an error.
      if (wouldBeSwift5Redeclaration) {
        current->diagnose(diag::invalid_redecl_swift5_warning, current);
        other->diagnose(diag::invalid_redecl_prev, other);
      } else {
        const auto *otherInit = dyn_cast<ConstructorDecl>(other);
        // Provide a better description for implicit initializers.
        if (otherInit && otherInit->isImplicit()) {
          // Skip conflicts with inherited initializers, which only happen
          // when the current declaration is within an extension. The override
          // checker should have already taken care of emitting a more
          // productive diagnostic.
          if (!other->getOverriddenDecl())
            current->diagnose(diag::invalid_redecl_init, current,
                              otherInit->isMemberwiseInitializer());
        } else if (current->isImplicit() || other->isImplicit()) {
          // If both declarations are implicit, we do not diagnose anything
          // as it would lead to misleading diagnostics and it's likely that
          // there's nothing actionable about it due to its implicit nature.
          // Special case for this is property wrappers or lazy variables.
          //
          // Otherwise, if 'current' is implicit, then we diagnose 'other'
          // since 'other' is a redeclaration of 'current'. Similarly, if
          // 'other' is implicit, we diagnose 'current'.
          const Decl *declToDiagnose = nullptr;
          if (current->isImplicit() && other->isImplicit()) {
            // If 'current' is a property wrapper backing storage property
            // or projected value property, then diagnose the wrapped
            // property.
            if (auto VD = dyn_cast<VarDecl>(current)) {
              if (auto originalWrappedProperty =
                      VD->getOriginalWrappedProperty()) {
                declToDiagnose = originalWrappedProperty;
              }

              // If 'current' is a synthesized lazy storage variable and
              // 'other' is synthesized projected value variable, then
              // diagnose the wrapped property.
              //
              // TODO: We should probably emit a diagnostic note on the lazy
              // variable as well, but there is currently no way to grab the
              // lazy property from its backing storage.
              if (VD->isLazyStorageProperty()) {
                if (auto otherVar = dyn_cast<VarDecl>(other)) {
                  if (auto originalWrappedProperty =
                          otherVar->getOriginalWrappedProperty()) {
                    declToDiagnose = originalWrappedProperty;
                  }
                }
              }
            }
          } else {
            declToDiagnose = current->isImplicit() ? other : current;
          }

          if (declToDiagnose) {
            // Figure out if the declaration we've redeclared is a
            // synthesized witness for a protocol requirement.
            bool isProtocolRequirement = false;
            if (auto VD = dyn_cast<ValueDecl>(current->isImplicit() ? current
                                                                    : other)) {
              isProtocolRequirement = llvm::any_of(
                  VD->getSatisfiedProtocolRequirements(), [&](ValueDecl *req) {
                    return req->getName() == VD->getName();
                  });
            }
            declToDiagnose->diagnose(diag::invalid_redecl_implicit,
                                     current->getDescriptiveKind(),
                                     isProtocolRequirement, other);

            // Emit a specialized note if the one of the declarations is
            // the backing storage property ('_foo') or projected value
            // property ('$foo') for a wrapped property. The backing or
            // projected var has the same source location as the wrapped
            // property we diagnosed above, so we don't need to extract
            // the original property.
            const VarDecl *varToDiagnose = nullptr;
            auto kind = PropertyWrapperSynthesizedPropertyKind::Backing;
            if (auto currentVD = dyn_cast<VarDecl>(current)) {
              if (auto currentKind =
                      currentVD->getPropertyWrapperSynthesizedPropertyKind()) {
                varToDiagnose = currentVD;
                kind = *currentKind;
              }
            }
            if (auto otherVD = dyn_cast<VarDecl>(other)) {
              if (auto otherKind =
                      otherVD->getPropertyWrapperSynthesizedPropertyKind()) {
                varToDiagnose = otherVD;
                kind = *otherKind;
              }
            }

            if (varToDiagnose) {
              assert(declToDiagnose);
              varToDiagnose->diagnose(
                  diag::invalid_redecl_implicit_wrapper, varToDiagnose,
                  kind == PropertyWrapperSynthesizedPropertyKind::Backing);
            }

            current->setInvalid();
          }
        } else {
          ctx.Diags.diagnoseWithNotes(
            current->diagnose(diag::invalid_redecl, current), [&]() {

            // Add a specialized note about the 'other' overload
            if (diagnoseDistributedAsyncOverload) {
              other->diagnose(diag::distributed_func_cannot_overload_on_async_only, other);
            } else {
              other->diagnose(diag::invalid_redecl_prev, other);
            }
          });

          current->setInvalid();
        }
      }

      break;
    }
  }
  return std::make_tuple<>();
}

static std::optional<unsigned> getParamIndex(const ParameterList *paramList,
                                             const ParamDecl *decl) {
  ArrayRef<ParamDecl *> params = paramList->getArray();
  for (unsigned i = 0; i < params.size(); ++i) {
    if (params[i] == decl) return i;
  }
  return std::nullopt;
}

static void checkInheritedDefaultValueRestrictions(ParamDecl *PD) {
  assert(PD->getDefaultArgumentKind() == DefaultArgumentKind::Inherited);

  auto *DC = PD->getInnermostDeclContext();
  const SourceFile *SF = DC->getParentSourceFile();
  assert((SF && SF->Kind == SourceFileKind::Interface || PD->isImplicit()) &&
         "explicit inherited default argument outside of a module interface?");

  // The containing decl should be a designated initializer.
  auto ctor = dyn_cast<ConstructorDecl>(DC);
  if (!ctor || ctor->isConvenienceInit()) {
    PD->diagnose(diag::inherited_default_value_not_in_designated_constructor);
    return;
  }

  // The decl it overrides should also be a designated initializer.
  auto overridden = ctor->getOverriddenDecl();
  if (!overridden || overridden->isConvenienceInit()) {
    PD->diagnose(
        diag::inherited_default_value_used_in_non_overriding_constructor);
    if (overridden)
      overridden->diagnose(diag::overridden_here);
    return;
  }

  // The corresponding parameter should have a default value.
  std::optional<unsigned> idx = getParamIndex(ctor->getParameters(), PD);
  assert(idx && "containing decl does not contain param?");
  ParamDecl *equivalentParam = overridden->getParameters()->get(*idx);
  if (equivalentParam->getDefaultArgumentKind() == DefaultArgumentKind::None) {
    PD->diagnose(diag::corresponding_param_not_defaulted);
    equivalentParam->diagnose(diag::inherited_default_param_here);
  }
}

static bool checkExpressionMacroDefaultValueRestrictions(ParamDecl *param) {
  assert(param->getDefaultArgumentKind() ==
         DefaultArgumentKind::ExpressionMacro);
  auto &ctx = param->getASTContext();
  auto *initExpr = param->getStructuralDefaultExpr();
  assert(initExpr);

#if SWIFT_BUILD_SWIFT_SYNTAX
  auto *DC = param->getInnermostDeclContext();
  const SourceFile *SF = DC->getParentSourceFile();
  return swift_Macros_checkDefaultArgumentMacroExpression(
      &ctx.Diags, SF->getExportedSourceFile(),
      initExpr->getLoc().getOpaquePointerValue());
#else
  ctx.Diags.diagnose(initExpr->getLoc(), diag::macro_unsupported);
  return false;
#endif
}

void TypeChecker::notePlaceholderReplacementTypes(Type writtenType,
                                                  Type inferredType) {
  assert(writtenType && inferredType &&
         "Must provide both written and inferred types");
  assert(writtenType->hasPlaceholder() && "Written type has no placeholder?");

  class PlaceholderNotator
      : public CanTypeDifferenceVisitor<PlaceholderNotator> {
  public:
    bool visitDifferentComponentTypes(CanType t1, CanType t2) {
      // Never replace anything the user wrote with an error type.
      if (t2->hasError() || t2->hasUnresolvedType()) {
        return false;
      }

      auto *placeholder = t1->getAs<PlaceholderType>();
      if (!placeholder) {
        return false;
      }

      if (auto *origRepr =
              placeholder->getOriginator().dyn_cast<TypeRepr *>()) {
        assert(isa<PlaceholderTypeRepr>(origRepr));
        t1->getASTContext()
            .Diags
            .diagnose(origRepr->getLoc(),
                      diag::replace_placeholder_with_inferred_type, t2)
            .fixItReplace(origRepr->getSourceRange(), t2.getString());
      }
      return false;
    }

    bool check(Type t1, Type t2) {
      return !visit(t1->getCanonicalType(), t2->getCanonicalType());
    };
  };

  PlaceholderNotator().check(writtenType, inferredType);
}

/// Check the default arguments that occur within this parameter list.
static void checkDefaultArguments(ParameterList *params) {
  // Force the default values in case they produce diagnostics.
  for (auto *param : *params) {
    auto ifacety = param->getInterfaceType();
    auto *expr = param->getTypeCheckedDefaultExpr();

    // Force captures since this can emit diagnostics.
    (void) param->getDefaultArgumentCaptureInfo();

    // If the default argument has isolation, it must match the
    // isolation of the decl context.
    (void)param->getInitializerIsolation();

    if (!ifacety->hasPlaceholder()) {
      continue;
    }

    // Placeholder types are banned for all parameter decls. We try to use the
    // freshly-checked default expression's contextual type to suggest a
    // reasonable type to insert.
    param->diagnose(diag::placeholder_type_not_allowed_in_parameter)
        .highlight(param->getSourceRange());
    if (expr && !expr->getType()->hasError()) {
      TypeChecker::notePlaceholderReplacementTypes(
          ifacety, expr->getType()->mapTypeOutOfContext());
    }
  }
}

void swift::checkVariadicParameters(ParameterList *params, DeclContext *dc) {
  bool lastWasVariadic = false;

  for (auto *param : *params) {
    if (lastWasVariadic) {
      if (param->getArgumentName().empty()) {
        if (isa<AbstractClosureExpr>(dc))
          param->diagnose(diag::closure_unlabeled_parameter_following_variadic_parameter);
        else
          param->diagnose(diag::unlabeled_parameter_following_variadic_parameter);
      }

      lastWasVariadic = false;
    }

    if (!param->isVariadic() &&
        !param->getInterfaceType()->is<PackExpansionType>())
      continue;

    if (param->isDefaultArgument())
      param->diagnose(diag::parameter_vararg_default);

    // Enum elements don't allow old-style variadics.
    if (param->isVariadic() &&
        isa<EnumElementDecl>(dc)) {
      param->diagnose(diag::enum_element_ellipsis);
      continue;
    }

    lastWasVariadic = true;
  }
}

Expr *DefaultArgumentExprRequest::evaluate(Evaluator &evaluator,
                                           ParamDecl *param) const {
  if (param->getDefaultArgumentKind() == DefaultArgumentKind::Inherited) {
    // Inherited default arguments don't have expressions, but we need to
    // perform a couple of semantic checks to make sure they're valid.
    checkInheritedDefaultValueRestrictions(param);
    return nullptr;
  }

  auto &ctx = param->getASTContext();
  auto paramTy = param->getTypeInContext();
  auto *initExpr = param->getStructuralDefaultExpr();
  assert(initExpr);

  auto isMacroExpansionExpr =
      param->getDefaultArgumentKind() == DefaultArgumentKind::ExpressionMacro;

  if (isMacroExpansionExpr &&
      !checkExpressionMacroDefaultValueRestrictions(param))
    return new (ctx) ErrorExpr(initExpr->getSourceRange(), ErrorType::get(ctx));

  // If the param has an error type, there's no point type checking the default
  // expression, unless we are type checking for code completion, in which case
  // the default expression might contain the code completion token.
  if (paramTy->hasError() && !ctx.CompletionCallback)
    return new (ctx) ErrorExpr(initExpr->getSourceRange(), ErrorType::get(ctx));

  auto *dc = param->getDefaultArgumentInitContext();
  assert(dc);

  if (!TypeChecker::typeCheckParameterDefault(initExpr, dc, paramTy,
                                              param->isAutoClosure(),
                                              /*atCallerSide=*/false)) {
    return new (ctx) ErrorExpr(initExpr->getSourceRange(), ErrorType::get(ctx));
  }

  // Expression macro default arguments are checked at caller side
  if (isMacroExpansionExpr)
    return initExpr;

  // Walk the checked initializer and contextualize any closures
  // we saw there.
  TypeChecker::contextualizeInitializer(dc, initExpr);
  TypeChecker::checkInitializerEffects(dc, initExpr);

  return initExpr;
}

Type DefaultArgumentTypeRequest::evaluate(Evaluator &evaluator,
                                          ParamDecl *param) const {
  if (auto *expr = param->getTypeCheckedDefaultExpr()) {
    // If the request failed, let's not propagate ErrorType up.
    return isa<ErrorExpr>(expr) ? Type() : expr->getType();
  }
  return Type();
}

Initializer *
DefaultArgumentInitContextRequest::evaluate(Evaluator &eval,
                                            ParamDecl *param) const {
  auto &ctx = param->getASTContext();
  auto *parentDC = param->getDeclContext();
  auto *paramList = getParameterList(cast<ValueDecl>(parentDC->getAsDecl()));

  // In order to compute the initializer context for this parameter, we need to
  // know its index in the parameter list. Therefore iterate over the parameters
  // looking for it and fill in the other parameter's contexts while we're here.
  Initializer *result = nullptr;
  for (auto idx : indices(*paramList)) {
    auto *otherParam = paramList->get(idx);

    // If this param doesn't need a context, we're done.
    if (!otherParam->hasDefaultExpr() && !otherParam->getStoredProperty())
      continue;

    // If this param already has a context, continue using it.
    if (otherParam->getCachedDefaultArgumentInitContext())
      continue;

    // Create a new initializer context. If this is for the parameter that
    // kicked off the request, make a note of it for when we return. Otherwise
    // cache the result ourselves.
    auto *initDC = new (ctx) DefaultArgumentInitializer(parentDC, idx);
    if (param == otherParam) {
      result = initDC;
    } else {
      eval.cacheOutput(DefaultArgumentInitContextRequest{otherParam},
                       std::move(initDC));
    }
  }
  assert(result && "Didn't create init context?");
  return result;
}

/// Check the requirements in the where clause of the given \c atd
/// to ensure that they don't introduce additional 'Self' requirements.
static void checkProtocolSelfRequirements(ProtocolDecl *proto,
                                          AssociatedTypeDecl *atd) {
  WhereClauseOwner(atd).visitRequirements(
      TypeResolutionStage::Interface,
      [proto](const Requirement &req, RequirementRepr *reqRepr) {
        switch (req.getKind()) {
        case RequirementKind::SameShape:
        case RequirementKind::Conformance:
        case RequirementKind::Layout:
        case RequirementKind::Superclass:
          if (reqRepr &&
              req.getFirstType()->isEqual(proto->getSelfInterfaceType())) {
            auto &diags = proto->getASTContext().Diags;
            diags.diagnose(reqRepr->getSubjectRepr()->getLoc(),
                           diag::protocol_where_clause_self_requirement);
          }

          return false;

        case RequirementKind::SameType:
          return false;
        }
        llvm_unreachable("unhandled kind");
      });
}

/// For now, DynamicSelfType can only appear at the top level of a
/// function result type, possibly wrapped in an optional type.
///
/// In the future, we could generalize it to allow it in any
/// covariant position, so that for example a class method could
/// return '() -> Self'.
static void checkDynamicSelfType(ValueDecl *decl, Type type) {
  if (!type->hasDynamicSelfType())
    return;

  if (auto objectTy = type->getOptionalObjectType())
    type = objectTy;

  if (type->is<DynamicSelfType>())
    return;

  if (isa<FuncDecl>(decl))
    decl->diagnose(diag::dynamic_self_invalid_method);
  else if (isa<VarDecl>(decl))
    decl->diagnose(diag::dynamic_self_invalid_property);
  else {
    assert(isa<SubscriptDecl>(decl));
    decl->diagnose(diag::dynamic_self_invalid_subscript);
  }
}

/// Build a default initializer string for the given pattern.
///
/// This string is suitable for display in diagnostics.
static std::optional<std::string>
buildDefaultInitializerString(DeclContext *dc, Pattern *pattern) {
  switch (pattern->getKind()) {
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#define PATTERN(Id, Parent)
#include "swift/AST/PatternNodes.def"
    return std::nullopt;
  case PatternKind::Any:
    return std::nullopt;

  case PatternKind::Named: {
    if (!pattern->hasType())
      return std::nullopt;

    // Special-case the various types we might see here.
    auto type = pattern->getType();

    // For literal-convertible types, form the corresponding literal.
#define CHECK_LITERAL_PROTOCOL(Kind, String)                                       \
  if (TypeChecker::conformsToKnownProtocol(type, KnownProtocolKind::Kind)) \
    return std::string(String);

    CHECK_LITERAL_PROTOCOL(ExpressibleByArrayLiteral, "[]")
    CHECK_LITERAL_PROTOCOL(ExpressibleByDictionaryLiteral, "[:]")
    CHECK_LITERAL_PROTOCOL(ExpressibleByUnicodeScalarLiteral, "\"\"")
    CHECK_LITERAL_PROTOCOL(ExpressibleByExtendedGraphemeClusterLiteral, "\"\"")
    CHECK_LITERAL_PROTOCOL(ExpressibleByFloatLiteral, "0.0")
    CHECK_LITERAL_PROTOCOL(ExpressibleByIntegerLiteral, "0")
    CHECK_LITERAL_PROTOCOL(ExpressibleByStringLiteral, "\"\"")
#undef CHECK_LITERAL_PROTOCOL

    // For optional types, use 'nil'.
    if (type->getOptionalObjectType())
      return std::string("nil");

    return std::nullopt;
  }

  case PatternKind::Paren: {
    if (auto sub = buildDefaultInitializerString(
            dc, cast<ParenPattern>(pattern)->getSubPattern())) {
      return "(" + *sub + ")";
    }

    return std::nullopt;
  }

  case PatternKind::Tuple: {
    std::string result = "(";
    bool first = true;
    for (auto elt : cast<TuplePattern>(pattern)->getElements()) {
      if (auto sub = buildDefaultInitializerString(dc, elt.getPattern())) {
        if (first) {
          first = false;
        } else {
          result += ", ";
        }

        result += *sub;
      } else {
        return std::nullopt;
      }
    }
    result += ")";
    return result;
  }

  case PatternKind::Typed:
    return buildDefaultInitializerString(
        dc, cast<TypedPattern>(pattern)->getSubPattern());

  case PatternKind::Binding:
    return buildDefaultInitializerString(
        dc, cast<BindingPattern>(pattern)->getSubPattern());
  }

  llvm_unreachable("Unhandled PatternKind in switch.");
}

/// Create a fix-it string for the 'decodable_suggest_overriding_init_here' and
/// optionally, the 'codable_suggest_overriding_init_here' diagnostics.
static std::string getFixItStringForDecodable(ClassDecl *CD,
                                              bool includeEncodeTo) {
  auto &ctx = CD->getASTContext();
  SourceLoc indentationLoc = CD->getBraces().End;
  StringRef extraIndentation;
  StringRef indentation = Lexer::getIndentationForLine(
      ctx.SourceMgr, indentationLoc, &extraIndentation);
  std::string fixItStringToReturn;
  {
    llvm::raw_string_ostream out(fixItStringToReturn);
    ExtraIndentStreamPrinter printer(out, indentation);

    printer.printNewline();
    printer << "override init(from decoder: Decoder) throws";

    // Add a dummy body.
    auto printDummyBody = [&]() {
      printer << " {";
      printer.printNewline();
      printer << extraIndentation << getCodePlaceholder();
      printer.printNewline();
      printer << "}";
    };

    printDummyBody();

    if (includeEncodeTo) {
      printer.printNewline();
      printer.printNewline();
      printer << "override func encode(to encoder: Encoder) throws";
      printDummyBody();
    }
  }

  return fixItStringToReturn;
}

/// Diagnose a class that does not have any initializers.
static void diagnoseClassWithoutInitializers(ClassDecl *classDecl) {
  ASTContext &C = classDecl->getASTContext();
  C.Diags.diagnose(classDecl, diag::class_without_init,
                   classDecl->isExplicitActor(),
                   classDecl->getDeclaredType());

  // HACK: We've got a special case to look out for and diagnose specifically to
  // improve the experience of seeing this, and mitigate some confusion.
  //
  // For a class A which inherits from Decodable class B, class A may have
  // additional members which prevent default initializer synthesis (and
  // inheritance of other initializers). The user may have assumed that this
  // case would synthesize Encodable/Decodable conformance for class A the same
  // way it may have for class B, or other classes.
  //
  // It is helpful to suggest here that the user may have forgotten to override
  // init(from:) (and encode(to:), if applicable) in a note, before we start
  // listing the members that prevented initializer synthesis.
  if (auto *superclassDecl = classDecl->getSuperclassDecl()) {
    auto *decodableProto = C.getProtocol(KnownProtocolKind::Decodable);
    auto superclassType = superclassDecl->getDeclaredInterfaceType();
    auto ref = lookupConformance(superclassType, decodableProto);
    if (ref) {
      // super conforms to Decodable, so we've failed to inherit init(from:).
      // Let's suggest overriding it here.
      //
      // We're going to diagnose on the concrete init(from:) decl if it exists
      // and isn't implicit; otherwise, on the subclass itself.
      ValueDecl *diagDest = classDecl;
      DeclNameRef initFrom(
          { C, DeclBaseName::createConstructor(), { C.Id_from } });
      auto result =
          TypeChecker::lookupMember(superclassDecl, superclassType, initFrom,
                                    classDecl->getLoc(),
                                    NameLookupFlags::IgnoreAccessControl);

      if (!result.empty() && !result.front().getValueDecl()->isImplicit())
        diagDest = result.front().getValueDecl();

      auto diagName = diag::decodable_suggest_overriding_init_here;
      auto shouldEmitFixItForEncodeTo = false;

      // This is also a bit of a hack, but the best place we've got at the
      // moment to suggest this.
      //
      // If the superclass also conforms to Encodable, it's quite
      // likely that the user forgot to override its encode(to:). In this case,
      // we can produce a slightly different diagnostic to suggest doing so.
      auto *encodableProto = C.getProtocol(KnownProtocolKind::Encodable);
      auto ref = lookupConformance(superclassType, encodableProto);
      if (ref) {
        // We only want to produce this version of the diagnostic if the
        // subclass doesn't directly implement encode(to:).
        // The direct lookup here won't see an encode(to:) if it is inherited
        // from the superclass.
        auto encodeTo = DeclName(C, C.Id_encode, C.Id_to);
        if (classDecl->lookupDirect(encodeTo).empty()) {
          diagName = diag::codable_suggest_overriding_init_here;
          shouldEmitFixItForEncodeTo = true;
        }
      }

      auto insertionLoc =
          Lexer::getLocForEndOfLine(C.SourceMgr, classDecl->getBraces().Start);
      auto fixItString =
          getFixItStringForDecodable(classDecl, shouldEmitFixItForEncodeTo);
      C.Diags.diagnose(diagDest, diagName)
          .fixItInsert(insertionLoc, fixItString);
    }
  }

  // Lazily construct a mapping from backing storage properties to the
  // declared properties.
  bool computedBackingToOriginalVars = false;
  llvm::SmallDenseMap<VarDecl *, VarDecl *> backingToOriginalVars;
  auto getOriginalVar = [&](VarDecl *var) -> VarDecl * {
    // If we haven't computed the mapping yet, do so now.
    if (!computedBackingToOriginalVars) {
      for (auto member : classDecl->getMembers()) {
        if (auto var = dyn_cast<VarDecl>(member)) {
          if (auto backingVar = var->getPropertyWrapperBackingProperty()) {
            backingToOriginalVars[backingVar] = var;
          }
        }
      }

      computedBackingToOriginalVars = true;
    }

    auto known = backingToOriginalVars.find(var);
    if (known == backingToOriginalVars.end())
      return nullptr;

    return known->second;
  };

  for (auto member : classDecl->getMembers()) {
    auto pbd = dyn_cast<PatternBindingDecl>(member);
    if (!pbd)
      continue;

    if (pbd->isStatic() || !pbd->hasStorage() ||
        pbd->isDefaultInitializable() || pbd->isInvalid())
      continue;
   
    for (auto idx : range(pbd->getNumPatternEntries())) {
      if (pbd->isInitialized(idx)) continue;

      auto *pattern = pbd->getPattern(idx);
      SmallVector<VarDecl *, 4> vars;
      pattern->collectVariables(vars);
      if (vars.empty()) continue;

      // Replace the variables we found with the originals for diagnostic
      // purposes.
      for (auto &var : vars) {
        if (auto originalVar = getOriginalVar(var))
          var = originalVar;
      }

      auto varLoc = vars[0]->getLoc();

      std::optional<InFlightDiagnostic> diag;
      switch (vars.size()) {
      case 1:
        diag.emplace(C.Diags.diagnose(varLoc, diag::note_no_in_class_init_1,
                                      vars[0]->getName()));
        break;
      case 2:
        diag.emplace(C.Diags.diagnose(varLoc, diag::note_no_in_class_init_2,
                                      vars[0]->getName(), vars[1]->getName()));
        break;
      case 3:
        diag.emplace(C.Diags.diagnose(varLoc, diag::note_no_in_class_init_3plus,
                                      vars[0]->getName(), vars[1]->getName(),
                                      vars[2]->getName(), false));
        break;
      default:
        diag.emplace(C.Diags.diagnose(varLoc, diag::note_no_in_class_init_3plus,
                                      vars[0]->getName(), vars[1]->getName(),
                                      vars[2]->getName(), true));
        break;
      }

      if (auto defaultValueSuggestion =
              buildDefaultInitializerString(classDecl, pattern))
        diag->fixItInsertAfter(pattern->getEndLoc(),
                               " = " + *defaultValueSuggestion);
    }
  }
}

static void maybeDiagnoseClassWithoutInitializers(ClassDecl *classDecl) {
  if (auto *SF = classDecl->getParentSourceFile()) {
    // Allow classes without initializers in SIL and module interface files.
    switch (SF->Kind) {
    case SourceFileKind::SIL:
    case SourceFileKind::Interface:
      return;
    case SourceFileKind::DefaultArgument:
    case SourceFileKind::Library:
    case SourceFileKind::Main:
    case SourceFileKind::MacroExpansion:
      break;
    }
  }

  // Some heuristics to skip emitting a diagnostic if the class is already
  // irreperably busted.
  if (classDecl->isInvalid() ||
      classDecl->inheritsSuperclassInitializers())
    return;

  auto *superclassDecl = classDecl->getSuperclassDecl();
  if (superclassDecl &&
      superclassDecl->getModuleContext() != classDecl->getModuleContext() &&
      superclassDecl->hasMissingDesignatedInitializers())
    return;

  for (auto member : classDecl->lookupDirect(DeclBaseName::createConstructor())) {
    auto ctor = dyn_cast<ConstructorDecl>(member);
    if (ctor && ctor->isDesignatedInit())
      return;
  }

  diagnoseClassWithoutInitializers(classDecl);
}

/// Determines if a given TypeLoc is module qualified by checking if it's
/// of the form `<Module>.<Type>`.
static bool isModuleQualified(TypeRepr *repr, ModuleDecl *module) {
  auto qualIdentTR = dyn_cast<QualifiedIdentTypeRepr>(repr);
  if (!qualIdentTR) {
    return false;
  }

  // FIXME(ModQual): This needs to be updated once we have an explicit
  //                 module qualification syntax.
  return qualIdentTR->getRoot()->isSimpleUnqualifiedIdentifier(
      module->getName());
}

/// If the provided type is an AttributedTypeRepr, unwraps it and provides both
/// the casted AttributedTypeRepr and the underlying type.
/// If the provided type is _not_ an AttributedTypeRepr, returns it unmodified
/// and returns `nullptr` for the attributed type.
static TypeRepr *unwrapAttributedRepr(TypeRepr *repr) {
  if (auto attr = dyn_cast<AttributedTypeRepr>(repr)) {
    return attr->getTypeRepr();
  }
  return repr;
}

/// Determines if this extension declares a conformance of a type declared
/// outside this module to a protocol declared outside this module (but only
/// in library evolution mode)
///
/// Since there can only be one conformance of a type to a given protocol,
/// it's not supported to declare these conformances because any other framework
/// (including the source framework) can declare this conformance, which would
/// result in duplicate symbols at runtime.
static void diagnoseRetroactiveConformances(
    ExtensionDecl *ext, DiagnosticEngine &diags) {
  ModuleDecl *module = ext->getParentModule();

  // Disable this for the Foundation module because of the interplay with
  // _ObjectiveCBridgeable.
  if (module->isFoundationModule()) {
    return;
  }

  // Don't warn for this if we see it in module interfaces.
  if (ext->getParentSourceFile()->Kind == SourceFileKind::Interface) {
    return;
  }

  Type extendedType = ext->getExtendedType();
  NominalTypeDecl *extendedNominalDecl = ext->getExtendedNominal();
  if (!extendedNominalDecl) {
    return;
  }

  ModuleDecl *extTypeModule = extendedNominalDecl->getParentModule();

  // If the type comes from the __ObjC clang header module, don't warn.
  if (extTypeModule->getName().is(CLANG_HEADER_MODULE_NAME)) {
    return;
  }

  // At this point, we know we're extending a type declared outside this module.
  // We better only be conforming it to protocols declared within this module.
  llvm::SmallSetVector<ProtocolDecl *, 8> externalProtocols;
  llvm::SmallSet<ProtocolDecl *, 8> protocolsWithRetroactiveAttr;
  for (const InheritedEntry &entry : ext->getInherited().getEntries()) {
    if (entry.getType().isNull() || !entry.getTypeRepr()) {
      continue;
    }

    auto proto =
        dyn_cast_or_null<ProtocolDecl>(entry.getType()->getAnyNominal());
    if (!proto) {  
      continue;
    }
    
    // As a fallback, to support previous language versions, also allow
    // this through if the protocol has been explicitly module-qualified.
    TypeRepr *repr = unwrapAttributedRepr(entry.getTypeRepr());
    if (isModuleQualified(repr, proto->getParentModule())) {
      continue;
    }

    proto->walkInheritedProtocols([&](ProtocolDecl *decl) {

      // Get the original conformance of the extended type to this protocol.
      auto conformanceRef = lookupConformance(extendedType, decl);
      if (!conformanceRef.isConcrete()) {
        return TypeWalker::Action::Continue;
      }
      auto conformance = conformanceRef.getConcrete();

      // If that conformance came from this extension, then we warn. Otherwise
      // we will have diagnosed it on the extension that actually declares this
      // specific conformance.
      if (conformance->getDeclContext() != ext) {
        return TypeWalker::Action::Continue;
      }

      // If this isn't a retroactive conformance, skip it.
      if (!conformance->isRetroactive()) {
        // However, if this is the protocol in the inherited type entry,
        // check to make sure it's not erroneously marked @retroactive when it's
        // not actually retroactive.
        if (decl == proto && entry.isRetroactive()) {
          auto loc =
              entry.getTypeRepr()->findAttrLoc(TypeAttrKind::Retroactive);

          bool typeInSamePackage = extTypeModule->inSamePackage(module);
          bool typeIsSameModule =
              extTypeModule->isSameModuleLookingThroughOverlays(module);

          auto declForDiag = (typeIsSameModule || typeInSamePackage)
                                 ? extendedNominalDecl
                                 : proto;
          bool isSameModule = declForDiag->getParentModule()
                                  ->isSameModuleLookingThroughOverlays(module);

          diags
              .diagnose(loc, diag::retroactive_attr_does_not_apply, declForDiag,
                        isSameModule)
              .warnUntilSwiftVersion(6)
              .fixItRemove(SourceRange(loc, loc.getAdvancedLoc(1)));
          return TypeWalker::Action::Stop;
        }
        return TypeWalker::Action::Continue;
      }

      // If it's marked @retroactive, no need to warn.
      if (entry.isRetroactive()) {
        // Note that we encountered this protocol through a conformance marked
        // @retroactive in case multiple clauses cause the protocol to be
        // inherited.
        protocolsWithRetroactiveAttr.insert(decl);
        return TypeWalker::Action::Continue;
      }

      // If we've come this far, we know this extension is the first declaration
      // of the conformance of the extended type to this protocol.
      externalProtocols.insert(decl);

      return TypeWalker::Action::Continue;
    });
  }

  // Remove protocols that are reachable through a @retroactive conformance.
  for (auto *proto : protocolsWithRetroactiveAttr) {
    externalProtocols.remove(proto);
  }

  // If we didn't find any violations, we're done.
  if (externalProtocols.empty()) {
    return;
  }

  // Diagnose the list of protocols we're introducing a conformance to.

  llvm::SmallString<32> protocolList;
  {
    llvm::raw_svector_ostream os(protocolList);
    llvm::interleaveComma(externalProtocols, os, [&os](ProtocolDecl *proto) {
      os << "'" << proto->getName() << "'";
    });
  }

  ext->diagnose(
      diag::extension_retroactive_conformance,
      extendedNominalDecl->getName(),
      externalProtocols.size() == 1,
      protocolList.str(),
      extTypeModule->getName());

  // Now build up a set of fix-its to force the user to explicitly specify the
  // declared conformances.

  auto diag = ext->diagnose(diag::extension_retroactive_conformance_silence);

  // First, find if we can insert `@retroactive ` within this extension
  // declaration to silence the warning. Each one of these gets removed from the
  // externalProtocols list, and that might end up being all of them.
  for (const InheritedEntry &entry : ext->getInherited().getEntries()) {
    auto protoDecl =
        dyn_cast_or_null<ProtocolDecl>(entry.getType()->getAnyNominal());
    TypeRepr *repr = unwrapAttributedRepr(entry.getTypeRepr());
    if (protoDecl && externalProtocols.remove(protoDecl)) {
      llvm::SmallString<32> qualifiedName;
      llvm::raw_svector_ostream os(qualifiedName);
      os << "@retroactive " << protoDecl->getName();
      diag.fixItReplace(
        repr->getSourceRange(), qualifiedName.str());
    }
  }

  // If we've hit every protocol declared by this extension, we're good to go.
  if (externalProtocols.empty()) {
    return;
  }

  // Otherwise, we've got inherited protocols that are being implicitly declared
  // by this extension. Create explicit declarations for these with
  // '@retroactive' conformances.

  {
    llvm::SmallString<32> additionalExtensions;
    llvm::raw_svector_ostream os(additionalExtensions);
    for (ProtocolDecl *proto : externalProtocols) {
      os << "extension " << extendedNominalDecl->getName() << ": "
        << "@retroactive " << proto->getName()
        << " {}\n";
    }
    diag.fixItInsert(ext->getStartLoc(), additionalExtensions.str());
  }
}

void TypeChecker::diagnoseDuplicateBoundVars(Pattern *pattern) {
  SmallVector<VarDecl *, 2> boundVars;
  pattern->collectVariables(boundVars);

  diagnoseDuplicateDecls(boundVars);
}

void TypeChecker::diagnoseDuplicateCaptureVars(CaptureListExpr *expr) {
  SmallVector<VarDecl *, 2> captureListVars;
  for (auto &capture : expr->getCaptureList())
    captureListVars.push_back(capture.getVar());

  diagnoseDuplicateDecls(captureListVars);
}

static StringRef prettyPrintAttrs(const ValueDecl *VD,
                                  ArrayRef<const DeclAttribute *> attrs,
                                  SmallVectorImpl<char> &out) {
  llvm::raw_svector_ostream os(out);
  StreamPrinter printer(os);

  PrintOptions opts = PrintOptions::printDeclarations();
  VD->getAttrs().print(printer, opts, attrs, VD);
  return StringRef(out.begin(), out.size()).drop_back();
}

static void diagnoseChangesByAccessNote(
    ValueDecl *VD,
    ArrayRef<const DeclAttribute *> attrs,
    Diag<StringRef, StringRef, DescriptiveDeclKind> diagID,
    Diag<StringRef> fixItID,
    llvm::function_ref<void(InFlightDiagnostic, StringRef)> addFixIts) {
  if (!VD->getASTContext().LangOpts.shouldRemarkOnAccessNoteSuccess() ||
      attrs.empty())
    return;

  // Generate string containing all attributes.
  SmallString<64> attrString;
  auto attrText = prettyPrintAttrs(VD, attrs, attrString);

  SourceLoc fixItLoc;

  auto reason = VD->getModuleContext()->getAccessNotes().Reason;
  auto diag = VD->diagnose(diagID, reason, attrText, VD->getDescriptiveKind());
  for (auto attr : attrs) {
    diag.highlight(attr->getRangeWithAt());
    if (fixItLoc.isInvalid())
      fixItLoc = attr->getRangeWithAt().Start;
  }
  diag.flush();

  if (!fixItLoc)
    fixItLoc = VD->getAttributeInsertionLoc(true);

  addFixIts(VD->getASTContext().Diags.diagnose(fixItLoc, fixItID, attrText),
            attrString);
}

template <typename Attr>
static void addOrRemoveAttr(ValueDecl *VD, const AccessNotesFile &notes,
                            std::optional<bool> expected,
                            SmallVectorImpl<DeclAttribute *> &removedAttrs,
                            llvm::function_ref<Attr *()> willCreate) {
  if (!expected) return;

  auto attr = VD->getAttrs().getAttribute<Attr>();
  if (*expected == (attr != nullptr)) return;

  if (*expected) {
    attr = willCreate();
    attr->setAddedByAccessNote();
    VD->getAttrs().add(attr);

    // Arrange for us to emit a remark about this attribute after type checking
    // has ensured it's valid.
    if (auto SF = VD->getDeclContext()->getParentSourceFile())
      SF->AttrsAddedByAccessNotes[VD].push_back(attr);
  } else {
    removedAttrs.push_back(attr);
    VD->getAttrs().removeAttribute(attr);
  }
}

InFlightDiagnostic
swift::softenIfAccessNote(const Decl *D, const DeclAttribute *attr,
                          InFlightDiagnostic &diag) {
  const ValueDecl *VD = dyn_cast<ValueDecl>(D);
  if (!VD || !attr || !attr->getAddedByAccessNote())
    return std::move(diag);

  SmallString<32> attrString;
  auto attrText = prettyPrintAttrs(VD, llvm::ArrayRef(attr), attrString);

  ASTContext &ctx = D->getASTContext();
  auto behavior = ctx.LangOpts.getAccessNoteFailureLimit();
  return std::move(diag.wrapIn(diag::wrap_invalid_attr_added_by_access_note,
                               D->getModuleContext()->getAccessNotes().Reason,
                               ctx.AllocateCopy(attrText), D->getDescriptiveKind())
                        .limitBehavior(behavior));
}

static void applyAccessNote(ValueDecl *VD, const AccessNote &note,
                            const AccessNotesFile &notes) {
  ASTContext &ctx = VD->getASTContext();
  SmallVector<DeclAttribute *, 2> removedAttrs;

  addOrRemoveAttr<ObjCAttr>(VD, notes, note.ObjC, removedAttrs, [&]{
    return ObjCAttr::create(ctx, note.ObjCName, false);
  });

  addOrRemoveAttr<DynamicAttr>(VD, notes, note.Dynamic, removedAttrs, [&]{
    return new (ctx) DynamicAttr(true);
  });

  // FIXME: If we ever have more attributes, we'll need to sort removedAttrs by
  // SourceLoc. As it is, attrs are always before modifiers, so we're okay now.

  diagnoseChangesByAccessNote(VD, removedAttrs,
                              diag::attr_removed_by_access_note,
                              diag::fixit_attr_removed_by_access_note,
                              [&](InFlightDiagnostic diag, StringRef code) {
    for (auto attr : llvm::reverse(removedAttrs))
      diag.fixItRemove(attr->getRangeWithAt());
  });

  if (note.ObjCName) {
    auto newName = note.ObjCName.value();

    // addOrRemoveAttr above guarantees there's an ObjCAttr on this decl.
    auto attr = VD->getAttrs().getAttribute<ObjCAttr>();
    assert(attr && "ObjCName set, but ObjCAttr not true or did not apply???");

    if (!attr->hasName()) {
      // There was already an @objc attribute with no selector. Set it.
      attr->setName(newName, true);

      if (!ctx.LangOpts.shouldRemarkOnAccessNoteSuccess())
        return;

      VD->diagnose(diag::attr_objc_name_changed_by_access_note,
                   notes.Reason, VD->getDescriptiveKind(), newName);

      auto fixIt =
          VD->diagnose(diag::fixit_attr_objc_name_changed_by_access_note);
      fixDeclarationObjCName(fixIt, VD, ObjCSelector(), newName);
    }
    else if (attr->getName() != newName) {
      // There was already an @objc
      auto behavior = ctx.LangOpts.getAccessNoteFailureLimit();

      VD->diagnose(diag::attr_objc_name_conflicts_with_access_note,
                   notes.Reason, VD->getDescriptiveKind(),
                   attr->getName().value(), newName)
          .highlight(attr->getRangeWithAt())
          .limitBehavior(behavior);
    }
  }
}

void TypeChecker::applyAccessNote(ValueDecl *VD) {
  (void)evaluateOrDefault(VD->getASTContext().evaluator,
                          ApplyAccessNoteRequest{VD}, {});
}

void swift::diagnoseAttrsAddedByAccessNote(SourceFile &SF) {
  if (!SF.getASTContext().LangOpts.shouldRemarkOnAccessNoteSuccess())
    return;

  for (auto declAndAttrs : SF.AttrsAddedByAccessNotes) {
    auto D = declAndAttrs.getFirst();
    SmallVector<DeclAttribute *, 4> sortedAttrs;
    llvm::append_range(sortedAttrs, declAndAttrs.getSecond());

    // Filter out invalid attributes.
    sortedAttrs.erase(
      llvm::remove_if(sortedAttrs, [](DeclAttribute *attr) {
        assert(attr->getAddedByAccessNote());
        return attr->isInvalid();
      }), sortedAttrs.end());
    if (sortedAttrs.empty()) continue;

    // Sort attributes by name.
    llvm::sort(sortedAttrs, [](DeclAttribute * first, DeclAttribute * second) {
      return first->getAttrName() < second->getAttrName();
    });
    sortedAttrs.erase(std::unique(sortedAttrs.begin(), sortedAttrs.end()),
                      sortedAttrs.end());

    diagnoseChangesByAccessNote(D, sortedAttrs, diag::attr_added_by_access_note,
                                diag::fixit_attr_added_by_access_note,
                                [=](InFlightDiagnostic diag, StringRef code) {
      diag.fixItInsert(D->getAttributeInsertionLoc(/*isModifier=*/true), code);
    });
  }
}

evaluator::SideEffect
ApplyAccessNoteRequest::evaluate(Evaluator &evaluator, ValueDecl *VD) const {
  AccessNotesFile &notes = VD->getModuleContext()->getAccessNotes();
  if (auto note = notes.lookup(VD))
    applyAccessNote(VD, *note.get(), notes);
  return {};
}

static void diagnoseWrittenPlaceholderTypes(ASTContext &Ctx,
                                            const Pattern *P,
                                            Expr *init) {
  // Make sure we have a user-written type annotation.
  auto *TP = dyn_cast<TypedPattern>(P);
  if (!TP || !TP->getTypeRepr()) {
    return;
  }

  auto *repr = TP->getTypeRepr()->getWithoutParens();
  if (auto *PTR = dyn_cast<PlaceholderTypeRepr>(repr)) {
    Ctx.Diags.diagnose(P->getLoc(),
                       diag::placeholder_type_not_allowed_in_pattern)
        .highlight(P->getSourceRange());
    if (init && !init->getType()->hasError()) {
      auto initTy = init->getType()->mapTypeOutOfContext();
      Ctx.Diags
          .diagnose(PTR->getLoc(),
                    diag::replace_placeholder_with_inferred_type, initTy)
          .fixItReplace(PTR->getSourceRange(), initTy.getString());
    }
  }
}

/// Make sure that every protocol conformance requirement on 'Self' is
/// directly stated in the protocol's inheritance clause.
///
/// This disallows protocol declarations where a conformance requirement on
/// 'Self' is implied by some other requirement, such as this:
///
///    protocol Other { ... }
///    protocol Foo { associatedtype A : Other }
///    protocol Bar {
///      associatedtype A : Foo where Self == A.A
///    }
///
/// Since name lookup is upstream of generic signature computation, we
/// want 'Other' to appear in the inheritance clause of 'Bar', so that
/// name lookup on Bar can find members of Other.
static void checkProtocolRefinementRequirements(ProtocolDecl *proto) {
  auto &ctx = proto->getASTContext();
  auto selfTy = proto->getSelfInterfaceType();
  auto genericSig = proto->getGenericSignature();

  // Check for circular inheritance; the HasCircularInheritedProtocolsRequest
  // will diagnose an error in that case, and we skip all remaining checks.
  if (proto->hasCircularInheritedProtocols())
    return;

  // If we make a ~P marking but our protocol Self type still conforms to P,
  // diagnose an error.
  //
  // FIXME: This duplicates logic from computeRequirementDiagnostics().
  // Get the list of written inverses.
  InvertibleProtocolSet inverses;
  bool anyObject = false;
  (void) getDirectlyInheritedNominalTypeDecls(proto, inverses, anyObject);

  for (auto ip : inverses) {
    auto kp = getKnownProtocolKind(ip);
    auto *otherProto = ctx.getProtocol(kp);
    if (!genericSig->requiresProtocol(selfTy, otherProto))
      continue;

    ctx.Diags.diagnose(proto,
                       diag::inverse_generic_but_also_conforms,
                       selfTy, getProtocolName(kp));
  }

  auto requiredProtos = genericSig->getRequiredProtocols(selfTy);
  for (auto *otherProto : requiredProtos) {
    // Every protocol 'P' has an implied requirement 'Self : P'; skip it.
    if (otherProto == proto)
      continue;

    // SIMDScalar in the standard library currently emits this warning for:
    // 'Hashable', 'Encodable', and 'Decodable'. This is unfortunate, but we
    // cannot fix it as it would alter the ABI of the protocol. Silence these
    // warnings specifically for those cases.
    if (proto->isSpecificProtocol(KnownProtocolKind::SIMDScalar) &&
        (otherProto->isSpecificProtocol(KnownProtocolKind::Hashable) ||
        otherProto->isSpecificProtocol(KnownProtocolKind::Encodable) ||
        otherProto->isSpecificProtocol(KnownProtocolKind::Decodable))) {
      continue;
    }

    // GenericSignature::getRequiredProtocols() canonicalizes the protocol
    // list by dropping protocols that are inherited by other protocols in
    // the list. Any protocols that remain in the list other than 'proto'
    // itself are implied by a conformance requirement on 'Self', but are
    // not (transitively) inherited by 'proto'.
    proto->diagnose(diag::missing_protocol_refinement, proto, otherProto);
  }
}

static void dumpGenericSignature(ASTContext &ctx, GenericContext *GC) {
  if (ctx.TypeCheckerOpts.DebugGenericSignatures) {
    if (auto sig = GC->getGenericSignature()) {
      llvm::errs() << "\n";
      if (auto *VD = dyn_cast_or_null<ValueDecl>(GC->getAsDecl())) {
        VD->dumpRef(llvm::errs());
        llvm::errs() << "\n";
      } else {
        GC->printContext(llvm::errs());
      }
      llvm::errs() << "Generic signature: ";
      PrintOptions Opts;
      Opts.ProtocolQualifiedDependentMemberTypes = true;
      Opts.PrintInverseRequirements =
          !ctx.TypeCheckerOpts.DebugInverseRequirements;
      sig->print(llvm::errs(), Opts);
      llvm::errs() << "\n";
      llvm::errs() << "Canonical generic signature: ";
      sig.getCanonicalSignature()->print(llvm::errs(), Opts);
      llvm::errs() << "\n";
    }
  }
}

namespace {
class DeclChecker : public DeclVisitor<DeclChecker> {
public:
  ASTContext &Ctx;
  SourceFile *SF;

  explicit DeclChecker(ASTContext &ctx, SourceFile *SF) : Ctx(ctx), SF(SF) {}

  ASTContext &getASTContext() const { return Ctx; }
  void addDelayedFunction(AbstractFunctionDecl *AFD) {
    if (!SF)
      return;

    while (auto *ESF = SF->getEnclosingSourceFile())
      SF = ESF;

    SF->addDelayedFunction(AFD);
  }

  void visit(Decl *decl) {
    // Visit auxiliary decls first.
    // We don't do this for members of classes because it happens as part of
    // visiting their ABI members.
    if (!isa<ClassDecl>(decl->getDeclContext())) {
      decl->visitAuxiliaryDecls([&](Decl *auxiliaryDecl) {
        this->visit(auxiliaryDecl);
      }, /*visitFreestandingExpanded=*/false);
    }

    if (auto *Stats = Ctx.Stats)
      ++Stats->getFrontendCounters().NumDeclsTypechecked;

    FrontendStatsTracer StatsTracer(getASTContext().Stats,
                                    "typecheck-decl", decl);
    PrettyStackTraceDecl StackTrace("type-checking", decl);

    if (auto VD = dyn_cast<ValueDecl>(decl))
      TypeChecker::applyAccessNote(VD);

    DeclVisitor<DeclChecker>::visit(decl);

    TypeChecker::checkExistentialTypes(decl);

    if (auto VD = dyn_cast<ValueDecl>(decl)) {
      TypeChecker::checkForForbiddenPrefix(Ctx, VD->getBaseName());

      // Force some requests, which can produce diagnostics.

      // Check redeclaration.
      (void)evaluateOrDefault(
          Ctx.evaluator,
          CheckRedeclarationRequest{
              VD, VD->getDeclContext()->getSelfNominalTypeDecl()},
          {});

      // Compute access level.
      (void) VD->getFormalAccess();

      // Compute overrides.
      checkOverrideActorIsolation(VD);

      // Check whether the member is @objc or dynamic.
      (void) VD->isObjC();
      (void) VD->isDynamic();

      // Check for actor isolation of top-level and local declarations.
      // Declarations inside types are handled in checkConformancesInContext()
      // to avoid cycles involving associated type inference.
      if (!VD->getDeclContext()->isTypeContext())
        (void) getActorIsolation(VD);

      // If this is a member of a nominal type, don't allow it to have a name of
      // "Type" or "Protocol" since we reserve the X.Type and X.Protocol
      // expressions to mean something builtin to the language.  We *do* allow
      // these if they are escaped with backticks though.
      if (VD->getDeclContext()->isTypeContext() &&
          (VD->getName().isSimpleName(Ctx.Id_Type) ||
           VD->getName().isSimpleName(Ctx.Id_Protocol)) &&
          VD->getNameLoc().isValid() &&
          Ctx.SourceMgr.extractText({VD->getNameLoc(), 1}) != "`") {
        auto &DE = Ctx.Diags;
        DE.diagnose(VD->getNameLoc(), diag::reserved_member_name,
                    VD, VD->getBaseIdentifier().str());
        DE.diagnose(VD->getNameLoc(), diag::backticks_to_escape)
            .fixItReplace(VD->getNameLoc(),
                          "`" + VD->getBaseName().userFacingName().str() + "`");
      }

      // Expand extension macros.
      if (auto *nominal = dyn_cast<NominalTypeDecl>(VD)) {
        (void)evaluateOrDefault(
            Ctx.evaluator,
            ExpandExtensionMacros{nominal},
            { });
      }
    }
  }


  //===--------------------------------------------------------------------===//
  // Visit Methods.
  //===--------------------------------------------------------------------===//

  void visitGenericTypeParamDecl(GenericTypeParamDecl *D) {
    llvm_unreachable("cannot reach here");
  }
  
  void visitImportDecl(ImportDecl *ID) {
    TypeChecker::checkDeclAttributes(ID);

    // Force the lookup of decls referenced by a scoped import in case it emits
    // diagnostics.
    (void)ID->getDecls();

    auto target = ID->getModule();
    if (target && // module would be nil if loading fails
        !Ctx.LangOpts.PackageName.empty() &&
        Ctx.LangOpts.PackageName == target->getPackageName().str() &&
        !target->isNonSwiftModule() && // target is a Swift module
        target->isNonUserModule()) { // target module is in distributed SDK
      // If reached here, a binary module (.swiftmodule) instead of interface of the
      // target was loaded for the main module, where both belong to the same package;
      // this is an expected behavior, but it should have been loaded from the local
      // build directory, not from distributed SDK. In such case, we show a warning.
      Ctx.Diags.diagnose(ID,
                         diag::in_package_module_not_compiled_locally,
                         target->getBaseIdentifier(),
                         target->getPackageName(),
                         target->getModuleFilename());
    }

    // Report the public import of a private module.
    if (Ctx.LangOpts.LibraryLevel == LibraryLevel::API) {
      auto importer = ID->getModuleContext();
      if (target &&
          !ID->getAttrs().hasAttribute<ImplementationOnlyAttr>() &&
          !ID->getAttrs().hasAttribute<SPIOnlyAttr>() &&
          ID->getAccessLevel() == AccessLevel::Public &&
          target->getLibraryLevel() == LibraryLevel::SPI) {

        InFlightDiagnostic inFlight =
            Ctx.Diags.diagnose(ID, diag::error_public_import_of_private_module,
                               target->getName(), importer->getName());
        if (auto attr = ID->getAttrs().getAttribute<AccessControlAttr>()) {
          if (Ctx.LangOpts.hasFeature(Feature::InternalImportsByDefault))
            inFlight.fixItRemove(attr->getLocation());
          else
            inFlight.fixItReplace(attr->getLocation(), "internal");
        } else
          inFlight.fixItInsert(ID->getStartLoc(), "internal ");

        bool isImportOfUnderlying = importer->getName() == target->getName();
        auto *SF = ID->getDeclContext()->getParentSourceFile();
        bool treatAsError = !isImportOfUnderlying &&
                            SF->Kind != SourceFileKind::Interface;
        if (!treatAsError)
          inFlight.limitBehavior(DiagnosticBehavior::Warning);
      }
    }
  }

  void visitOperatorDecl(OperatorDecl *OD) {
    TypeChecker::checkDeclAttributes(OD);
    checkRedeclaration(OD);
    if (auto *IOD = dyn_cast<InfixOperatorDecl>(OD))
      (void)IOD->getPrecedenceGroup();
    checkAccessControl(OD);
  }

  void visitPrecedenceGroupDecl(PrecedenceGroupDecl *PGD) {
    TypeChecker::checkDeclAttributes(PGD);
    validatePrecedenceGroup(PGD);
    checkRedeclaration(PGD);
    checkAccessControl(PGD);
  }

  void visitMissingDecl(MissingDecl *missing) {  }

  void visitMissingMemberDecl(MissingMemberDecl *MMD) {
    llvm_unreachable("should always be type-checked already");
  }

  /// Determine the number of bits set.
  static unsigned numBitsSet(uint64_t value) {
    unsigned count = 0;
    for (uint64_t i : range(0, 63)) {
      if (value & (uint64_t(1) << i))
        ++count;
    }

    return count;
  }

  void visitMacroDecl(MacroDecl *MD) {
    TypeChecker::checkDeclAttributes(MD);
    checkAccessControl(MD);

    if (!MD->getDeclContext()->isModuleScopeContext())
      MD->diagnose(diag::macro_in_nested, MD->getName());
    if (!MD->getAttrs().hasAttribute<MacroRoleAttr>(/*AllowInvalid*/ true))
      MD->diagnose(diag::macro_without_role, MD->getName());

    TypeChecker::checkParameterList(MD->getParameterList(), MD);
    checkDefaultArguments(MD->getParameterList());

    // Check the macro definition.
    switch (auto macroDef = MD->getDefinition()) {
    case MacroDefinition::Kind::Undefined:
      MD->diagnose(diag::macro_must_be_defined, MD->getName());
      break;

    case MacroDefinition::Kind::Invalid:
    case MacroDefinition::Kind::Builtin:
    case MacroDefinition::Kind::Expanded:
      // Nothing else to check here.
      break;

    case MacroDefinition::Kind::External: {
        // Retrieve the external definition of the macro.
      auto external = macroDef.getExternalMacro();
      ExternalMacroDefinitionRequest request{
        &Ctx, external.moduleName, external.macroTypeName
      };
      auto externalDef =
          evaluateOrDefault(Ctx.evaluator, request,
                            ExternalMacroDefinition::error("unknown error"));
      if (externalDef.isError()) {
        MD->diagnose(diag::external_macro_not_found, external.moduleName.str(),
                     external.macroTypeName.str(), MD->getName(),
                     externalDef.getErrorMessage())
            .limitBehavior(DiagnosticBehavior::Warning);
      }

      break;
    }
    }

    // If the macro has a result type, it must have the freestanding
    // expression role. Other roles cannot have result types.
    if (auto resultTypeRepr = MD->getResultTypeRepr()) {
      if (!MD->getMacroRoles().contains(MacroRole::Expression)) {
        auto resultType = MD->getResultInterfaceType(); {
          auto diag = Ctx.Diags.diagnose(
              MD->arrowLoc, diag::macro_result_type_cannot_be_used, resultType);
          diag.highlight(resultTypeRepr->getSourceRange());

          // In a .swiftinterface file, downgrade this diagnostic to a warning.
          // This allows the compiler to process existing .swiftinterface
          // files that contain this issue.
          if (resultType->isVoid()) {
            if (auto sourceFile = MD->getParentSourceFile())
              if (sourceFile->Kind == SourceFileKind::Interface)
                diag.limitBehavior(DiagnosticBehavior::Warning);
          }
        }

        Ctx.Diags.diagnose(MD->arrowLoc, diag::macro_make_freestanding_expression)
          .fixItInsert(MD->getAttributeInsertionLoc(false),
                       "@freestanding(expression)\n");
        Ctx.Diags.diagnose(MD->arrowLoc, diag::macro_remove_result_type)
          .fixItRemove(SourceRange(MD->arrowLoc, resultTypeRepr->getEndLoc()));
      }
    }

    // A macro can only have a single freestanding macro role.
    MacroRoles freestandingRolesInhabited =
        MD->getMacroRoles() & getFreestandingMacroRoles();
    if (numBitsSet(freestandingRolesInhabited.toRaw()) > 1) {
      MD->diagnose(diag::macro_multiple_freestanding_roles);
    }
  }

  void visitMacroExpansionDecl(MacroExpansionDecl *MED) {
    // Assign a discriminator.
    (void)MED->getDiscriminator();
    MED->forEachExpandedNode([&](ASTNode node) {
      TypeChecker::typeCheckASTNode(node, MED->getDeclContext());
    });
  }

  void visitBoundVariable(VarDecl *VD) {
    // WARNING: Anything you put in this function will only be run when the
    // VarDecl is fully type-checked within its own file. It will NOT be run
    // when the VarDecl is merely used from another file.

    // Compute these requests in case they emit diagnostics.
    TypeChecker::applyAccessNote(VD);
    (void) VD->getInterfaceType();
    (void) VD->isGetterMutating();
    (void) VD->isSetterMutating();
    (void) VD->getPropertyWrapperAuxiliaryVariables();
    (void) VD->getPropertyWrapperInitializerInfo();
    (void) VD->getImplInfo();
    checkGlobalIsolation(VD);

    // Visit auxiliary decls first
    VD->visitAuxiliaryDecls([&](VarDecl *var) {
      this->visitBoundVariable(var);
    });

    // Reject cases where this is a variable that has storage but it isn't
    // allowed.
    if (VD->hasStorage()) {
      // Note: Stored properties in protocols, enums, etc are diagnosed in
      // finishStorageImplInfo().

      // We haven't implemented type-level storage in some contexts.
      if (VD->isStatic()) {
        auto PBD = VD->getParentPatternBinding();
        // Selector for unimplemented_static_var message.
        enum : unsigned {
          Misc,
          GenericTypes,
          Classes,
          ProtocolExtensions
        };
        auto unimplementedStatic = [&](unsigned diagSel) {
          auto staticLoc = PBD->getStaticLoc();
          VD->diagnose(diag::unimplemented_static_var, diagSel,
                       PBD->getStaticSpelling(), diagSel == Classes)
              .highlight(staticLoc);
        };

        auto DC = VD->getDeclContext();

        // Stored type variables in a generic context need to logically
        // occur once per instantiation, which we don't yet handle.
        if (DC->getExtendedProtocolDecl()) {
          unimplementedStatic(ProtocolExtensions);
        } else if (DC->isGenericContext()
               && !DC->getGenericSignatureOfContext()->areAllParamsConcrete()) {
          unimplementedStatic(GenericTypes);
        } else if (DC->getSelfClassDecl()) {
          auto StaticSpelling = PBD->getStaticSpelling();
          if (StaticSpelling != StaticSpellingKind::KeywordStatic)
            unimplementedStatic(Classes);
        }
      }
    }

    TypeChecker::checkDeclAttributes(VD);

    auto DC = VD->getDeclContext();

    if (!checkOverrides(VD)) {
      // If a property has an override attribute but does not override
      // anything, complain.
      auto overridden = VD->getOverriddenDecl();
      if (auto *OA = VD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!overridden) {
          auto isClassContext = DC->getSelfClassDecl() != nullptr;
          auto isStructOrEnumContext = DC->getSelfEnumDecl() != nullptr ||
                                       DC->getSelfStructDecl() != nullptr;
          if (isStructOrEnumContext) {
            VD->diagnose(diag::override_nonclass_decl)
                .highlight(OA->getLocation())
                .fixItRemove(OA->getRange());
          } else {
            VD->diagnose(diag::property_does_not_override, isClassContext)
                .highlight(OA->getLocation());
          }
          OA->setInvalid();
        }
      }
    }

    checkImplementationOnlyOverride(VD);

    if (VD->getDeclContext()->getSelfClassDecl()) {
      if (VD->getValueInterfaceType()->hasDynamicSelfType()) {
        if (VD->hasStorage())
          VD->diagnose(diag::dynamic_self_in_stored_property);
        else if (VD->isSettable(nullptr))
          VD->diagnose(diag::dynamic_self_in_mutable_property);
        else
          checkDynamicSelfType(VD, VD->getValueInterfaceType());
      }
    }
    
    checkForEmptyOptionSet(VD);

    // Now check all the accessors.
    VD->visitEmittedAccessors([&](AccessorDecl *accessor) {
      visit(accessor);
    });

    // If this var decl is a no implicit copy varDecl, error if its type is a
    // move only type. No implicit copy is redundant.
    //
    // NOTE: We do this here instead of TypeCheckAttr since types are not
    // completely type checked at that point.
    auto &DE = Ctx.Diags;
    if (auto attr = VD->getAttrs().getAttribute<NoImplicitCopyAttr>()) {
      if (auto *nom = VD->getInterfaceType()->getNominalOrBoundGenericNominal()) {
        if (!nom->canBeCopyable()) {
          DE.diagnose(attr->getLocation(),
                      diag::noimplicitcopy_attr_not_allowed_on_moveonlytype)
            .fixItRemove(attr->getRange());
        }
      }
    }

    // @_staticExclusiveOnly types cannot be put into 'var's, only 'let'.
    if (auto SD = VD->getInterfaceType()->getStructOrBoundGenericStruct()) {
      if (SD->getAttrs().hasAttribute<StaticExclusiveOnlyAttr>()) {
        auto isProtocolContext = isa<ProtocolDecl>(DC);

        if (isProtocolContext && !VD->supportsMutation()) {
          return;
        }

        if (VD->isLet()) {
          return;
        }

        auto diagMsg = isProtocolContext
                           ? diag::attr_static_exclusive_no_setters
                           : diag::attr_static_exclusive_only_let_only;

        Ctx.Diags.diagnoseWithNotes(
            VD->diagnose(diagMsg, VD->getInterfaceType()), [&]() {
              SD->diagnose(diag::attr_static_exclusive_only_type_nonmutating,
                           SD->getDeclaredInterfaceType());
            });
      }
    }
  }

  bool checkBoundInOutVarDecl(PatternBindingDecl *pbd, unsigned patternIndex,
                              const Pattern *p, VarDecl *vd) {
    // If our var decl doesn't have an initial value, error. We always want an
    // inout var decl to have an lvalue initial value that it is binding to.
    auto *expr = pbd->getInit(patternIndex);
    assert(expr && "Code assumes that we checked for validity");

    // Next make sure that our initial value was an lvalue.
    if (!isa<LoadExpr>(expr)) {
      vd->diagnose(diag::referencebindings_binding_must_be_to_lvalue, "inout");
      return false;
    }

    return true;
  }

  void visitPatternBindingDecl(PatternBindingDecl *PBD) {
    DeclContext *DC = PBD->getDeclContext();

    TypeChecker::checkDeclAttributes(PBD);

    bool isInSILMode = false;
    if (auto sourceFile = SF)
      isInSILMode = sourceFile->Kind == SourceFileKind::SIL;
    bool isTypeContext = DC->isTypeContext();

    for (auto i : range(PBD->getNumPatternEntries())) {
      const auto *entry = PBD->isFullyValidated(i)
                              ? &PBD->getPatternList()[i]
                              : PBD->getCheckedPatternBindingEntry(i);
      assert(entry && "No pattern binding entry?");

      const auto *Pat = PBD->getPattern(i);
      Pat->forEachVariable([&](VarDecl *var) {
        this->visitBoundVariable(var);

        auto markVarAndPBDInvalid = [PBD, var] {
          PBD->setInvalid();
          var->setInvalid();
        };

        if (PBD->isInitialized(i)) {
          // Add the attribute that preserves the "has an initializer" value
          // across module generation, as required for TBDGen.
          if (var->supportsInitialization() &&
              !var->getAttrs().hasAttribute<HasInitialValueAttr>()) {
            var->getAttrs().add(new (Ctx)
                                    HasInitialValueAttr(/*IsImplicit=*/true));
          }

          // If we fail to check the bound inout introducer, mark the variable
          // and pbd invalid().
          if (var->getIntroducer() == VarDecl::Introducer::InOut) {
            if (!checkBoundInOutVarDecl(PBD, i, Pat, var)) {
              markVarAndPBDInvalid();
            }
          }

          return;
        }

        // If this is a declaration without an initializer, reject code if
        // uninitialized vars are not allowed.
        if (isInSILMode) return;

        // If the variable has no storage, it never needs an initializer.
        if (!var->hasStorage())
          return;

        if (var->getAttrs().hasAttribute<SILGenNameAttr>())
          return;

        if (var->isInvalid() || PBD->isInvalid())
          return;

        // Properties with an opaque return type need an initializer to
        // determine their underlying type.
        if (var->getOpaqueResultTypeDecl()) {
          // ...but don't enforce this for SIL or module interface files.
          switch (SF->Kind) {
          case SourceFileKind::Interface:
          case SourceFileKind::SIL:
            break;
          case SourceFileKind::DefaultArgument:
          case SourceFileKind::Main:
          case SourceFileKind::Library:
          case SourceFileKind::MacroExpansion:
            var->diagnose(diag::opaque_type_var_no_init);
            break;
          }
        }

        // Non-member observing properties need an initializer.
        if (var->getWriteImpl() == WriteImplKind::StoredWithObservers &&
            !isTypeContext) {
          var->diagnose(diag::observingprop_requires_initializer);
          markVarAndPBDInvalid();
          return;
        }

        // Static/class declarations require an initializer unless in a
        // protocol.
        if (var->isStatic() && !isa<ProtocolDecl>(DC)) {
          // ...but don't enforce this for SIL or module interface files.
          switch (SF->Kind) {
          case SourceFileKind::Interface:
          case SourceFileKind::SIL:
            return;
          case SourceFileKind::DefaultArgument:
          case SourceFileKind::Main:
          case SourceFileKind::Library:
          case SourceFileKind::MacroExpansion:
            break;
          }

          var->diagnose(diag::static_requires_initializer,
                        var->getCorrectStaticSpelling(),
                        var->isLet());
          var->diagnose(diag::static_requires_initializer_add_init)
            .fixItInsert(Pat->getEndLoc(), " = <#initializer#>");
          markVarAndPBDInvalid();
          return;
        }

        // Global variables require an initializer in normal source files.
        if (DC->isModuleScopeContext()) {
          switch (SF->Kind) {
          case SourceFileKind::Main:
          case SourceFileKind::Interface:
          case SourceFileKind::SIL:
            return;
          case SourceFileKind::DefaultArgument:
          case SourceFileKind::Library:
          case SourceFileKind::MacroExpansion:
            break;
          }

          var->diagnose(diag::global_requires_initializer, var->isLet());
          var->diagnose(diag::static_requires_initializer_add_init)
            .fixItInsert(Pat->getEndLoc(), " = <#initializer#>");
          markVarAndPBDInvalid();
          return;
        }

        // Inout VarDecls need to have an initializer.
        if (var->getIntroducer() == VarDecl::Introducer::InOut) {
          var->diagnose(diag::referencebindings_binding_must_have_initial_value,
                        "inout");
          markVarAndPBDInvalid();
          return;
        }
      });
    }

    TypeChecker::checkDeclAttributes(PBD);

    checkAccessControl(PBD);

    checkExplicitAvailability(PBD);

    // If the initializers in the PBD aren't checked yet, do so now.
    for (auto i : range(PBD->getNumPatternEntries())) {
      if (!PBD->isInitialized(i))
        continue;

      if (!PBD->isInitializerChecked(i)) {
        TypeCheckExprOptions options;

        TypeChecker::typeCheckPatternBinding(PBD, i, /*patternType=*/Type(),
                                             options);
      }

      if (!PBD->isInvalid()) {
        auto *init = PBD->getInit(i);

        // If we're performing an binding to a weak or unowned variable from a
        // constructor call, emit a warning that the instance will be immediately
        // deallocated.
        diagnoseUnownedImmediateDeallocation(Ctx, PBD->getPattern(i),
                                             PBD->getEqualLoc(i),
                                             init);

        // Written placeholder types are banned in the signatures of pattern
        // bindings. If there's a valid initializer, try to offer its type
        // as a replacement.
        diagnoseWrittenPlaceholderTypes(Ctx, PBD->getPattern(i), init);

        // Trigger a request that will complete typechecking for the
        // initializer.
        (void) PBD->getCheckedAndContextualizedInit(i);
      }
    }

    if (auto *var = PBD->getSingleVar()) {

      // If this is an init accessor property with a default initializer,
      // make sure that it subsumes initializers of all of its "initializes"
      // stored properties.
      // FIXME: This should be requestified.
      auto *initAccessor = var->getAccessor(AccessorKind::Init);
      if (initAccessor && PBD->isInitialized(0)) {
        for (auto *property : initAccessor->getInitializedProperties()) {
          auto *propertyBinding = property->getParentPatternBinding();
          if (propertyBinding->isInitialized(0))
            propertyBinding->setInitializerSubsumed(0);
        }
      }

      // If we have a pure noncopyable type, we cannot have explicit read/set
      // accessors since this means that we cannot call mutating methods without
      // copying. We do not want to support types that one cannot define a
      // modify operation via a get/set or a modify.
      if (var->getTypeInContext()->isNoncopyable()) {
        if (auto *read = var->getAccessor(AccessorKind::Read)) {
          if (!read->isImplicit()) {
            if (auto *set = var->getAccessor(AccessorKind::Set)) {
              if (!set->isImplicit()) {
                var->diagnose(diag::noncopyable_cannot_have_read_set_accessor,
                              0);
                PBD->setInvalid();
                var->setInvalid();
                return;
              }
            }
          }
        }
      }
    }
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    auto *DC = SD->getDeclContext();

    // Force creation of the generic signature.
    (void) SD->getGenericSignature();
    dumpGenericSignature(Ctx, SD);

    // Force requests that can emit diagnostics.
    (void) SD->getInterfaceType();

    if (!SD->isInvalid()) {
      TypeChecker::checkReferencedGenericParams(SD);
      checkGenericParams(SD);
      TypeChecker::checkProtocolSelfRequirements(SD);
    }

    TypeChecker::checkDeclAttributes(SD);

    checkAccessControl(SD);

    checkExplicitAvailability(SD);

    if (!checkOverrides(SD)) {
      // If a subscript has an override attribute but does not override
      // anything, complain.
      if (auto *OA = SD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!SD->getOverriddenDecl()) {
          auto DC = SD->getDeclContext();
          auto isClassContext = DC->getSelfClassDecl() != nullptr;
          auto isStructOrEnumContext = DC->getSelfEnumDecl() != nullptr ||
                                       DC->getSelfStructDecl() != nullptr;
          if (isStructOrEnumContext) {
            SD->diagnose(diag::override_nonclass_decl)
                .highlight(OA->getLocation())
                .fixItRemove(OA->getRange());
          } else {
            SD->diagnose(diag::subscript_does_not_override, isClassContext)
                .highlight(OA->getLocation());
          }
          OA->setInvalid();
        }
      }
    }

    checkImplementationOnlyOverride(SD);

    // Compute these requests in case they emit diagnostics.
    (void) SD->isGetterMutating();
    (void) SD->isSetterMutating();
    (void) SD->getImplInfo();

    TypeChecker::checkParameterList(SD->getIndices(), SD);

    checkDefaultArguments(SD->getIndices());
    checkVariadicParameters(SD->getIndices(), SD);

    if (DC->getSelfClassDecl()) {
      checkDynamicSelfType(SD, SD->getValueInterfaceType());

      if (SD->getValueInterfaceType()->hasDynamicSelfType() &&
          SD->supportsMutation()) {
        SD->diagnose(diag::dynamic_self_in_mutable_subscript);
      }
    }

    // Reject "class" methods on actors.
    if (SD->getStaticSpelling() == StaticSpellingKind::KeywordClass &&
        DC->getSelfClassDecl() &&
        DC->getSelfClassDecl()->isActor()) {
      SD->diagnose(diag::class_subscript_not_in_class, false)
          .fixItReplace(SD->getStaticLoc(), "static");
    }

    // Reject noncopyable typed subscripts with read/set accessors since we
    // cannot define modify operations upon them without copying the read.
    if (SD->mapTypeIntoContext(SD->getElementInterfaceType())->isNoncopyable()) {
      if (auto *read = SD->getAccessor(AccessorKind::Read)) {
        if (!read->isImplicit()) {
          if (auto *set = SD->getAccessor(AccessorKind::Set)) {
            if (!set->isImplicit()) {
              SD->diagnose(diag::noncopyable_cannot_have_read_set_accessor,
                           1);
              SD->setInvalid();
              return;
            }
          }
        }
      }
    }

    // Now check all the accessors.
    SD->visitEmittedAccessors([&](AccessorDecl *accessor) {
      visit(accessor);
    });
  }

  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    // Force creation of the generic signature.
    (void) TAD->getGenericSignature();
    dumpGenericSignature(Ctx, TAD);

    // Force requests that can emit diagnostics.
    (void) TAD->getUnderlyingType();

    // Make sure to check the underlying type.
    
    TypeChecker::checkDeclAttributes(TAD);
    checkAccessControl(TAD);
    checkGenericParams(TAD);
  }
  
  void visitOpaqueTypeDecl(OpaqueTypeDecl *OTD) {
    // Force requests that can emit diagnostics.
    (void) OTD->getGenericSignature();

    TypeChecker::checkDeclAttributes(OTD);
    checkAccessControl(OTD);
  }
  
  void visitAssociatedTypeDecl(AssociatedTypeDecl *AT) {
    TypeChecker::checkDeclAttributes(AT);

    checkInheritanceClause(AT);
    auto *proto = AT->getProtocol();

    checkProtocolSelfRequirements(proto, AT);

    if (proto->isObjC()) {
      AT->diagnose(diag::associated_type_objc, AT->getName(), proto->getName());
    }

    checkAccessControl(AT);

    // Trigger the checking for overridden declarations.
    (void) AT->getOverriddenDecls();

    auto defaultType = AT->getDefaultDefinitionType();
    if (defaultType && !defaultType->hasError()) {
      // associatedtype X = X is invalid
      auto mentionsItself =
        defaultType.findIf([&](Type defaultType) {
          if (auto DMT = defaultType->getAs<DependentMemberType>()) {
            return (DMT->getAssocType() == AT &&
                    DMT->getBase()->isEqual(proto->getSelfInterfaceType()));
          }
          return false;
        });

      if (mentionsItself) {
        auto &DE = Ctx.Diags;
        DE.diagnose(AT->getDefaultDefinitionTypeRepr()->getLoc(),
                    diag::recursive_decl_reference, AT);
        AT->diagnose(diag::kind_declared_here, DescriptiveDeclKind::Type);
      }
    }

    // An associated type that was introduced after the protocol
    auto module = AT->getDeclContext()->getParentModule();
    if (!defaultType &&
        module->getResilienceStrategy() == ResilienceStrategy::Resilient &&
        AvailabilityInference::availableRange(proto).isSupersetOf(
            AvailabilityInference::availableRange(AT))) {
      AT->diagnose(
          diag::resilient_associated_type_less_available_requires_default, AT);
    }
  }

  void checkUnsupportedNestedType(NominalTypeDecl *NTD) {
    auto *DC = NTD->getDeclContext();

    // We don't allow nested types inside inlinable contexts.
    auto kind = DC->getFragileFunctionKind();
    if (kind.kind != FragileFunctionKind::None) {
      NTD->diagnose(diag::local_type_in_inlinable_function, NTD->getName(),
                    kind.getSelector());
    }

    if (auto *parentDecl = DC->getSelfNominalTypeDecl()) {
      // We don't allow types to be nested within a tuple extension.
      if (isa<BuiltinTupleDecl>(parentDecl)) {
        NTD->diagnose(diag::tuple_extension_nested_type, NTD);
        return;
      }

      // We don't support protocols nested in generic contexts.
      // This includes protocols nested in other protocols.
      if (isa<ProtocolDecl>(NTD) && DC->isGenericContext()) {
        if (auto *OuterPD = DC->getSelfProtocolDecl())
          NTD->diagnose(diag::unsupported_nested_protocol_in_protocol, NTD,
                        OuterPD);
        else
          NTD->diagnose(diag::unsupported_nested_protocol_in_generic, NTD);

        NTD->setInvalid();
        return;
      }

      // We don't support nested types in protocols.
      if (auto proto = dyn_cast<ProtocolDecl>(parentDecl)) {
        if (DC->getExtendedProtocolDecl()) {
          NTD->diagnose(diag::unsupported_type_nested_in_protocol_extension, NTD,
                        proto);
        } else {
          NTD->diagnose(diag::unsupported_type_nested_in_protocol, NTD, proto);
        }
        NTD->setInvalid();
      }
    }

    // We don't support nested types in generic functions yet.
    if (NTD->isGenericContext()) {
      if (DC->isLocalContext() && DC->isGenericContext()) {
        // A local generic context is a generic function.
        if (auto AFD = dyn_cast<AbstractFunctionDecl>(DC)) {
          NTD->diagnose(diag::unsupported_type_nested_in_generic_function, NTD,
                        AFD);
        } else {
          NTD->diagnose(diag::unsupported_type_nested_in_generic_closure, NTD);
        }
        NTD->setInvalid();
      }
    }
  }

  void visitEnumDecl(EnumDecl *ED) {
    checkUnsupportedNestedType(ED);

    // Force creation of the generic signature.
    (void) ED->getGenericSignature();
    dumpGenericSignature(Ctx, ED);

    // Temporary restriction until we figure out pattern matching and
    // enum case construction with packs.
    if (auto genericSig = ED->getGenericSignature()) {
      for (auto paramTy : genericSig.getGenericParams()) {
        if (paramTy->isParameterPack()) {
          ED->diagnose(diag::enum_with_pack);
          break;
        }
      }
    }

    // FIXME: Remove this once we clean up the mess involving raw values.
    (void) ED->getInterfaceType();

    checkGenericParams(ED);

    // Check for circular inheritance of the raw type.
    (void) ED->hasCircularRawValue();

    TypeChecker::checkDeclAttributes(ED);

    for (Decl *member : ED->getMembers())
      visit(member);

    checkInheritanceClause(ED);
    diagnoseMissingExplicitSendable(ED);
    checkAccessControl(ED);

    TypeChecker::checkPatternBindingCaptures(ED);

    auto &DE = Ctx.Diags;
    if (auto rawTy = ED->getRawType()) {
      // The raw type must be one of the blessed literal convertible types.
      if (!computeAutomaticEnumValueKind(ED)) {
        if (!rawTy->is<ErrorType>()) {
          DE.diagnose(ED->getInherited().getStartLoc(),
                      diag::raw_type_not_literal_convertible, rawTy);
        }
      }
      
      // We need at least one case to have a raw value.
      if (ED->getAllElements().empty()) {
        DE.diagnose(ED->getInherited().getStartLoc(),
                    diag::empty_enum_raw_type);
      }
    }

    // -----
    // NonCopyableChecks
    //

    if (ED->isObjC() && !ED->canBeCopyable()) {
      ED->diagnose(diag::noncopyable_objc_enum);
    }

    checkExplicitAvailability(ED);

    TypeChecker::checkDeclCircularity(ED);

    TypeChecker::checkConformancesInContext(ED);

    // If our enum is marked as move only, it cannot be indirect or have any
    // indirect cases.
    if (!ED->canBeCopyable()) {
      if (ED->isIndirect())
        ED->diagnose(diag::noncopyable_enums_do_not_support_indirect,
                     ED->getBaseIdentifier());
      for (auto *elt : ED->getAllElements()) {
        if (elt->isIndirect()) {
          elt->diagnose(diag::noncopyable_enums_do_not_support_indirect,
                        ED->getBaseIdentifier());
        }
      }
    }
  }

  void visitStructDecl(StructDecl *SD) {
    checkUnsupportedNestedType(SD);

    // Force creation of the generic signature.
    (void) SD->getGenericSignature();
    dumpGenericSignature(Ctx, SD);

    checkGenericParams(SD);

    // Force lowering of stored properties.
    (void) SD->getStoredProperties();

    TypeChecker::addImplicitConstructors(SD);

    installCodingKeysIfNecessary(SD);
    installDistributedActorIfNecessary(SD);

    TypeChecker::checkDeclAttributes(SD);

    for (Decl *Member : SD->getMembers()) {
      visit(Member);
    }

    TypeChecker::checkPatternBindingCaptures(SD);

    checkInheritanceClause(SD);
    diagnoseMissingExplicitSendable(SD);

    checkAccessControl(SD);

    checkExplicitAvailability(SD);

    TypeChecker::checkDeclCircularity(SD);

    TypeChecker::checkConformancesInContext(SD);
  }

  /// Check whether the given properties can be @NSManaged in this class.
  static bool propertiesCanBeNSManaged(ClassDecl *classDecl,
                                       ArrayRef<VarDecl *> vars) {
    // Check whether we have an Objective-C-defined class in our
    // inheritance chain.
    if (!classDecl->checkAncestry(AncestryFlags::ClangImported))
      return false;

    // If all of the variables are @objc, we can use @NSManaged.
    for (auto var : vars) {
      if (!var->isObjC())
        return false;
    }

    // Okay, we can use @NSManaged.
    return true;
  }

  /// Check that all stored properties have in-class initializers.
  void checkRequiredInClassInits(ClassDecl *cd) {
    // Initializers may be omitted from property declarations in module
    // interface files so don't diagnose in them.
    SourceFile *sourceFile = cd->getDeclContext()->getParentSourceFile();
    if (sourceFile && sourceFile->Kind == SourceFileKind::Interface)
      return;

    ClassDecl *source = nullptr;
    for (auto member : cd->getMembers()) {
      auto pbd = dyn_cast<PatternBindingDecl>(member);
      if (!pbd)
        continue;

      if (pbd->isStatic() || !pbd->hasStorage() || 
          pbd->isDefaultInitializable() || pbd->isInvalid())
        continue;

      // The variables in this pattern have not been
      // initialized. Diagnose the lack of initial value.
      pbd->setInvalid();
      SmallVector<VarDecl *, 4> vars;
      for (auto idx : range(pbd->getNumPatternEntries()))
        pbd->getPattern(idx)->collectVariables(vars);
      bool suggestNSManaged = propertiesCanBeNSManaged(cd, vars);
      switch (vars.size()) {
      case 0:
        llvm_unreachable("should have been marked invalid");

      case 1:
        pbd->diagnose(diag::missing_in_class_init_1, vars[0]->getName(),
                      suggestNSManaged);
        break;

      case 2:
        pbd->diagnose(diag::missing_in_class_init_2, vars[0]->getName(),
                      vars[1]->getName(), suggestNSManaged);
        break;

      case 3:
        pbd->diagnose(diag::missing_in_class_init_3plus, vars[0]->getName(),
                      vars[1]->getName(), vars[2]->getName(), false,
                      suggestNSManaged);
        break;

      default:
        pbd->diagnose(diag::missing_in_class_init_3plus, vars[0]->getName(),
                      vars[1]->getName(), vars[2]->getName(), true,
                      suggestNSManaged);
        break;
      }

      // Figure out where this requirement came from.
      if (!source) {
        source = cd;
        while (true) {
          // If this class had the 'requires_stored_property_inits'
          // attribute, diagnose here.
          if (source->getAttrs().
                hasAttribute<RequiresStoredPropertyInitsAttr>())
            break;

          // If the superclass doesn't require in-class initial
          // values, the requirement was introduced at this point, so
          // stop here.
          auto superclass = source->getSuperclassDecl();
          if (!superclass->requiresStoredPropertyInits())
            break;

          // Keep looking.
          source = superclass;
        }
      }

      // Add a note describing why we need an initializer.
      source->diagnose(diag::requires_stored_property_inits_here,
                       source->getDeclaredType(), cd == source,
                       suggestNSManaged);
    }
  }

  static void diagnoseInverseOnClass(ClassDecl *decl) {
    auto &ctx = decl->getASTContext();

    InvertibleProtocolSet inverses;
    bool anyObject = false;
    (void) getDirectlyInheritedNominalTypeDecls(decl, inverses, anyObject);

    for (auto ip : inverses) {
      // Allow ~Copyable when MoveOnlyClasses is enabled
      if (ip == InvertibleProtocolKind::Copyable
          && ctx.LangOpts.hasFeature(Feature::MoveOnlyClasses))
        continue;

      ctx.Diags.diagnose(decl->getLoc(),
                         diag::inverse_on_class,
                         getProtocolName(getKnownProtocolKind(ip)),
                         decl->isAnyActor());
    }
  }

  void visitClassDecl(ClassDecl *CD) {
    checkUnsupportedNestedType(CD);

    // Force creation of the generic signature.
    (void) CD->getGenericSignature();
    dumpGenericSignature(Ctx, CD);

    checkGenericParams(CD);

    // Check for circular inheritance.
    (void)CD->getSuperclassDecl();

    if (auto superclass = CD->getSuperclassDecl()) {
      // Actors cannot have superclasses, nor can they be superclasses.
      if (CD->isActor() && !superclass->isNSObject())
        CD->diagnose(diag::actor_inheritance,
                     /*distributed=*/CD->isDistributedActor());
      else if (superclass->isActor())
        CD->diagnose(diag::actor_inheritance,
                     /*distributed=*/CD->isDistributedActor());

      // Enforce a temporary restriction on inheriting from a superclass
      // type with a pack, until we figure out the semantics of method
      // overrides in these situations.
      if (auto genericSig = superclass->getGenericSignature()) {
        for (auto paramTy : genericSig.getGenericParams()) {
          if (paramTy->isParameterPack()) {
            CD->diagnose(diag::superclass_with_pack);
            break;
          }
        }
      }
    }

    // Check distributed actors
    TypeChecker::checkDistributedActor(SF, CD);

    // Force lowering of stored properties.
    (void) CD->getStoredProperties();

    // Force creation of an implicit destructor, if any.
    (void) CD->getDestructor();

    TypeChecker::checkDeclAttributes(CD);

    if (CD->isActor())
      TypeChecker::checkConcurrencyAvailability(CD->getLoc(), CD);

    for (Decl *Member : CD->getABIMembers())
      visit(Member);

    TypeChecker::checkPatternBindingCaptures(CD);

    // If this class requires all of its stored properties to have
    // in-class initializers, diagnose this now.
    if (CD->requiresStoredPropertyInits())
      checkRequiredInClassInits(CD);

    // Compute @objc for each superclass member, to catch selector
    // conflicts resulting from unintended overrides.
    //
    // FIXME: This should be a request so we can measure how much work
    // we're doing here.
    CD->walkSuperclasses(
      [&](ClassDecl *superclass) {
        if (!superclass->getParentSourceFile())
          return TypeWalker::Action::Stop;

        for (auto *member : superclass->getMembers()) {
          if (auto *vd = dyn_cast<ValueDecl>(member)) {
            if (vd->isSyntacticallyOverridable()) {
              (void) vd->isObjC();
            }
          }
        }

        return TypeWalker::Action::Continue;
      });

    if (auto superclassTy = CD->getSuperclass()) {
      ClassDecl *Super = superclassTy->getClassOrBoundGenericClass();
      bool isInvalidSuperclass = false;

      if (Super->isFinal()) {
        CD->diagnose(diag::inheritance_from_final_class,
                     Super->getDeclaredType());
        // FIXME: should this really be skipping the rest of decl-checking?
        return;
      }

      if (Super->hasClangNode() && Super->getGenericParams()
          && superclassTy->hasTypeParameter()) {
        CD->diagnose(diag::inheritance_from_unspecialized_objc_generic_class,
                     Super->getName());
      }

      switch (Super->getForeignClassKind()) {
      case ClassDecl::ForeignKind::Normal:
        break;
      case ClassDecl::ForeignKind::CFType:
        CD->diagnose(diag::inheritance_from_cf_class, Super->getName());
        isInvalidSuperclass = true;
        break;
      case ClassDecl::ForeignKind::RuntimeOnly:
        CD->diagnose(diag::inheritance_from_objc_runtime_visible_class,
                     Super->getName());
        isInvalidSuperclass = true;
        break;
      }

      if (!isInvalidSuperclass && Super->hasMissingVTableEntries() &&
          !Super->isResilient(CD->getParentModule(),
                              ResilienceExpansion::Minimal)) {
        auto *superFile = Super->getModuleScopeContext();
        if (auto *serialized = dyn_cast<SerializedASTFile>(superFile)) {
          const auto effVersion =
              Ctx.LangOpts.EffectiveLanguageVersion;
          if (serialized->getLanguageVersionBuiltWith() != effVersion) {
            CD->diagnose(
                diag::
                    inheritance_from_class_with_missing_vtable_entries_versioned,
                Super->getName(), serialized->getLanguageVersionBuiltWith(),
                effVersion);
            isInvalidSuperclass = true;
          }
        }
        if (!isInvalidSuperclass) {
          CD->diagnose(diag::inheritance_from_class_with_missing_vtable_entries,
                       Super->getName());
          for (const auto &member : Super->getMembers())
            if (const auto *MMD = dyn_cast<MissingMemberDecl>(member))
              CD->diagnose(diag::inheritance_from_class_with_missing_vtable_entry,
                           MMD->getName());
          isInvalidSuperclass = true;
        }
      }

      if (!Ctx.isAccessControlDisabled()) {
        // Require the superclass to be open if this is outside its
        // defining module.  But don't emit another diagnostic if we
        // already complained about the class being inherently
        // un-subclassable.
        if (!isInvalidSuperclass &&
            !Super->hasOpenAccess(CD->getDeclContext()) &&
            Super->getModuleContext() != CD->getModuleContext()) {
          CD->diagnose(diag::superclass_not_open, superclassTy);
          isInvalidSuperclass = true;
        }

        // Require superclasses to be open if the subclass is open.
        // This is a restriction we can consider lifting in the future,
        // e.g. to enable a "sealed" superclass whose subclasses are all
        // of one of several alternatives.
        if (!isInvalidSuperclass &&
            CD->getFormalAccess() == AccessLevel::Open &&
            Super->getFormalAccess() != AccessLevel::Open) {
          CD->diagnose(diag::superclass_of_open_not_open, superclassTy);
          Super->diagnose(diag::superclass_here);
        }
      }
    }

    checkInheritanceClause(CD);
    diagnoseMissingExplicitSendable(CD);

    checkAccessControl(CD);

    checkExplicitAvailability(CD);

    TypeChecker::checkDeclCircularity(CD);

    TypeChecker::checkConformancesInContext(CD);

    maybeDiagnoseClassWithoutInitializers(CD);

    diagnoseInverseOnClass(CD);
  }

  void visitProtocolDecl(ProtocolDecl *PD) {
    checkUnsupportedNestedType(PD);

    TypeChecker::checkDeclAttributes(PD);

    // Check that all named primary associated types are valid.
    if (!PD->getPrimaryAssociatedTypeNames().empty())
      (void) PD->getPrimaryAssociatedTypes();

    // Explicitly compute the requirement signature to detect errors.
    // Do this before visiting members, to avoid a request cycle if
    // a member references another declaration whose generic signature
    // has a conformance requirement to this protocol.
    auto reqSig = PD->getRequirementSignature();

    // Check the members.
    for (auto Member : PD->getMembers())
      visit(Member);

    checkAccessControl(PD);

    checkInheritanceClause(PD);

    checkProtocolRefinementRequirements(PD);

    TypeChecker::checkDeclCircularity(PD);
    if (PD->isResilient())
      if (!SF || SF->Kind != SourceFileKind::Interface)
        TypeChecker::inferDefaultWitnesses(PD);

    if (Ctx.TypeCheckerOpts.DebugGenericSignatures) {
      auto sig = PD->getRequirementSignature();
      PD->dumpRef(llvm::errs());
      llvm::errs() << "\n";
      llvm::errs() << "Requirement signature: ";
      PrintOptions Opts;
      Opts.ProtocolQualifiedDependentMemberTypes = true;
      Opts.PrintInverseRequirements =
          !Ctx.TypeCheckerOpts.DebugInverseRequirements;
      sig.print(PD, llvm::errs(), Opts);
      llvm::errs() << "\n";
    }

    if (!reqSig.getErrors()) {
      // Always verify signatures, even if building without asserts.
      //
      // An incorrect signature indicates a serious problem which can cause
      // miscompiles or inadvertent ABI dependencies on compiler bugs, so
      // we really want to avoid letting one slip by.
      PD->getGenericSignature().verify(reqSig.getRequirements());
    }

    checkExplicitAvailability(PD);
  }

  void visitVarDecl(VarDecl *VD) {
    // Delay type-checking on VarDecls until we see the corresponding
    // PatternBindingDecl.
  }

  /// FIXME: This is an egregious hack to turn off availability checking
  /// for specific functions that were missing availability in older versions
  /// of existing libraries that we must nonetheless still support.
  static bool hasHistoricallyWrongAvailability(FuncDecl *func) {
    return func->getName().isCompoundName("swift_deletedAsyncMethodError", { });
  }

  void visitFuncDecl(FuncDecl *FD) {
    // Force these requests in case they emit diagnostics.
    (void) FD->getInterfaceType();
    (void) FD->getOperatorDecl();
    (void) FD->getDynamicallyReplacedDecl();
    
    dumpGenericSignature(Ctx, FD);

    if (!isa<AccessorDecl>(FD)) {
      if (!FD->isInvalid()) {
        checkGenericParams(FD);
        TypeChecker::checkReferencedGenericParams(FD);
        TypeChecker::checkProtocolSelfRequirements(FD);
      }

      checkAccessControl(FD);

      TypeChecker::checkParameterList(FD->getParameters(), FD);
    }

    TypeChecker::checkDeclAttributes(FD);
    TypeChecker::checkDistributedFunc(FD);

    if (!checkOverrides(FD)) {
      // If a method has an 'override' keyword but does not
      // override anything, complain.
      if (auto *OA = FD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!FD->getOverriddenDecl()) {
          auto DC = FD->getDeclContext();
          auto isClassContext = DC->getSelfClassDecl() != nullptr;
          auto isStructOrEnumContext = DC->getSelfEnumDecl() != nullptr ||
                                       DC->getSelfStructDecl() != nullptr;
          if (isStructOrEnumContext) {
            FD->diagnose(diag::override_nonclass_decl)
                .highlight(OA->getLocation())
                .fixItRemove(OA->getRange());
          } else {
            FD->diagnose(diag::method_does_not_override, isClassContext)
                .highlight(OA->getLocation());
          }
          OA->setInvalid();
        }
      }
    }

    checkImplementationOnlyOverride(FD);

    if (FD->getAsyncLoc().isValid() &&
        !hasHistoricallyWrongAvailability(FD))
      TypeChecker::checkConcurrencyAvailability(FD->getAsyncLoc(), FD);
    
    if (FD->getDeclContext()->isLocalContext()) {
      // Check local function bodies right away.
      (void) FD->getTypecheckedBody();
      (void) FD->getCaptureInfo();
    } else if (!FD->isBodySkipped()) {
      addDelayedFunction(FD);
    }

    checkExplicitAvailability(FD);

    // Skip this for accessors, since we should have diagnosed the
    // storage itself.
    if (!isa<AccessorDecl>(FD))
      if (FD->getDeclContext()->getSelfClassDecl())
        checkDynamicSelfType(FD, FD->getResultInterfaceType());

    checkDefaultArguments(FD->getParameters());
    checkVariadicParameters(FD->getParameters(), FD);

    // Validate 'static'/'class' on functions in extensions.
    auto StaticSpelling = FD->getStaticSpelling();
    if (StaticSpelling != StaticSpellingKind::None &&
        isa<ExtensionDecl>(FD->getDeclContext())) {
      if (auto *NTD = FD->getDeclContext()->getSelfNominalTypeDecl()) {
        if (!isa<ClassDecl>(NTD)) {
          if (StaticSpelling == StaticSpellingKind::KeywordClass) {
            FD->diagnose(diag::class_func_not_in_class, false)
                .fixItReplace(FD->getStaticLoc(), "static");
            NTD->diagnose(diag::extended_type_declared_here);
          }
        }
      }
    }

    // Reject "class" methods on actors.
    if (StaticSpelling == StaticSpellingKind::KeywordClass &&
        FD->getDeclContext()->getSelfClassDecl() &&
        FD->getDeclContext()->getSelfClassDecl()->isActor()) {
      FD->diagnose(diag::class_func_not_in_class, false)
          .fixItReplace(FD->getStaticLoc(), "static");
    }

    // Member functions need some special validation logic.
    if (FD->getDeclContext()->isTypeContext()) {
      if (FD->isOperator() && !isMemberOperator(FD, nullptr)) {
        auto selfNominal = FD->getDeclContext()->getSelfNominalTypeDecl();
        auto isProtocol = isa_and_nonnull<ProtocolDecl>(selfNominal);
        // We did not find 'Self'. Complain.
        FD->diagnose(diag::operator_in_unrelated_type,
                     FD->getDeclContext()->getDeclaredInterfaceType(),
                     isProtocol, FD);
      }
    }

    // If the function is exported to C, it must be representable in (Obj-)C.
    // FIXME: This needs to be moved to its own request if we want to
    // productize @_cdecl.
    if (auto CDeclAttr = FD->getAttrs().getAttribute<swift::CDeclAttr>()) {
      std::optional<ForeignAsyncConvention> asyncConvention;
      std::optional<ForeignErrorConvention> errorConvention;
      ObjCReason reason(ObjCReason::ExplicitlyCDecl, CDeclAttr);
      if (isRepresentableInObjC(FD, reason, asyncConvention, errorConvention)) {
        if (FD->hasAsync()) {
          FD->setForeignAsyncConvention(*asyncConvention);
          Ctx.Diags.diagnose(
              CDeclAttr->getLocation(), diag::attr_decl_async,
              CDeclAttr->getAttrName(), FD->getDescriptiveKind());
        } else if (FD->hasThrows()) {
          FD->setForeignErrorConvention(*errorConvention);
          Ctx.Diags.diagnose(CDeclAttr->getLocation(), diag::cdecl_throws);
        }
      } else {
        reason.setAttrInvalid();
      }
    }

    TypeChecker::checkObjCImplementation(FD);
  }

  void visitModuleDecl(ModuleDecl *) { }

  void visitEnumCaseDecl(EnumCaseDecl *ECD) {
    // The type-checker doesn't care about how these are grouped.
  }

  void visitEnumElementDecl(EnumElementDecl *EED) {
    (void) EED->getInterfaceType();
    auto *ED = EED->getParentEnum();

    TypeChecker::checkDeclAttributes(EED);

    if (auto *PL = EED->getParameterList()) {
      TypeChecker::checkParameterList(PL, EED);

      checkDefaultArguments(PL);
      checkVariadicParameters(PL, EED);
    }

    auto &DE = Ctx.Diags;
    // We don't yet support raw values on payload cases.
    if (EED->hasAssociatedValues()) {
      if (auto rawTy = ED->getRawType()) {
        EED->diagnose(diag::enum_with_raw_type_case_with_argument);
        DE.diagnose(ED->getInherited().getStartLoc(),
                    diag::enum_raw_type_here, rawTy);
        EED->setInvalid();
      }
    }

    // Force the raw value expr then yell if our parent doesn't have a raw type.
    Expr *RVE = EED->getRawValueExpr();
    if (RVE && !ED->hasRawType()) {
      DE.diagnose(RVE->getLoc(), diag::enum_raw_value_without_raw_type);
      EED->setInvalid();
    }

    checkAccessControl(EED);
  }

  /// The extended type must be '(repeat each Element)' or a generic
  /// typealias with that underlying type.
  static bool isValidExtendedTypeForTupleExtension(ExtensionDecl *ED) {
    auto extType = ED->getExtendedType();
    auto selfType = ED->getSelfInterfaceType();

    // The extended type must be '(repeat each Element)'.
    if (extType->is<UnboundGenericType>()) {
      auto *extDecl = extType->getAnyGeneric();
      if (!extDecl->getDeclaredInterfaceType()->isEqual(selfType))
        return false;
    } else if (extType->is<TupleType>()) {
      if (!extType->isEqual(selfType))
        return false;
    } else {
      assert(false && "Huh?");
    }

    return true;
  }

  /// Compiler-known marker protocols cannot be extended with members.
  static void diagnoseExtensionOfMarkerProtocol(ExtensionDecl *ED) {
    auto *nominal = ED->getExtendedNominal();
    if (auto *proto = dyn_cast_or_null<ProtocolDecl>(nominal)) {
      if (proto->getKnownProtocolKind() && proto->isMarkerProtocol()) {
        ED->diagnose(diag::cannot_extend_nominal, nominal);
      }
    }
  }

  static void checkTupleExtension(ExtensionDecl *ED) {
    auto *nominal = ED->getExtendedNominal();
    if (!nominal || !isa<BuiltinTupleDecl>(nominal))
      return;

    auto &ctx = ED->getASTContext();

    if (!ctx.LangOpts.hasFeature(Feature::TupleConformances)) {
      ED->diagnose(diag::experimental_tuple_extension);
    }

    if (!isValidExtendedTypeForTupleExtension(ED)) {
      ED->diagnose(diag::tuple_extension_wrong_type,
                   ED->getSelfInterfaceType());
    }

    // Make sure we declare conformance to exactly one protocol.
    auto protocols = ED->getLocalProtocols();
    if (protocols.size() != 1) {
      ED->diagnose(diag::tuple_extension_one_conformance);
      return;
    }

    auto *protocol = protocols[0];

    // Validate the generic signature.
    auto genericSig = ED->getGenericSignature();

    // We have a single parameter pack by construction, if we
    // get this far.
    auto params = genericSig.getGenericParams();
    assert(params.size() == 1);
    assert(params[0]->isParameterPack());

    // Make sure we have a single conditional requirement,
    // 'repeat each Element: P', where 'Element' is our pack
    // and 'P' is the protocol above, and nothing else.
    bool foundRequirement = false;
    auto reqs = genericSig.getRequirements();
    for (auto req : reqs) {
      if (req.getKind() == RequirementKind::Conformance &&
          req.getFirstType()->isEqual(params[0]) &&
          req.getProtocolDecl() == protocol) {
        assert(!foundRequirement);
        foundRequirement = true;
      } else {
        if (req.getKind() == RequirementKind::Layout) {
          ED->diagnose(diag::tuple_extension_extra_requirement,
                       req.getFirstType(),
                       unsigned(RequirementKind::Conformance),
                       ctx.getAnyObjectType());
        } else {
          ED->diagnose(diag::tuple_extension_extra_requirement,
                       req.getFirstType(),
                       unsigned(req.getKind()),
                       req.getSecondType());
        }
      }
    }

    if (!foundRequirement) {
      ED->diagnose(diag::tuple_extension_missing_requirement,
                   params[0], protocol->getDeclaredInterfaceType());
    }
  }

  void visitExtensionDecl(ExtensionDecl *ED) {
    // Produce any diagnostics for the extended type.
    auto extType = ED->getExtendedType();

    auto *nominal = ED->getExtendedNominal();

    if (nominal == nullptr) {
      const bool wasAlreadyInvalid = ED->isInvalid();
      ED->setInvalid();
      if (!extType->hasError() && extType->getAnyNominal()) {
        auto canExtType = extType->getCanonicalType();
        if (auto existential = canExtType->getAs<ExistentialType>()) {
          ED->diagnose(diag::unsupported_existential_extension, extType)
              .highlight(ED->getExtendedTypeRepr()->getSourceRange());
          ED->diagnose(diag::invalid_extension_rewrite,
                       existential->getConstraintType())
              .fixItReplace(ED->getExtendedTypeRepr()->getSourceRange(),
                            existential->getConstraintType()->getString());
          return;
        }

        // If we've got here, then we have some kind of extension of a prima
        // facie non-nominal type.  This can come up when we're projecting
        // typealiases out of bound generic types.
        //
        // struct Array<T> { typealias Indices = Range<Int> }
        // extension Array.Indices.Bound {}
        //
        // Offer to rewrite it to the underlying nominal type.
        if (canExtType.getPointer() != extType.getPointer()) {
          ED->diagnose(diag::invalid_nominal_extension, extType, canExtType)
              .highlight(ED->getExtendedTypeRepr()->getSourceRange());
          ED->diagnose(diag::invalid_extension_rewrite, canExtType)
              .fixItReplace(ED->getExtendedTypeRepr()->getSourceRange(),
                            canExtType->getString());
          return;
        }
      }

      if (!wasAlreadyInvalid) {
        // If nothing else applies, fall back to a generic diagnostic.
        ED->diagnose(diag::non_nominal_extension, extType);
      }

      return;
    }

    // Record a dependency from TypeCheckSourceFileRequest to
    // ExtendedNominalRequest, since the call to getExtendedNominal()
    // above doesn't record a dependency when reading a cached value.
    ED->computeExtendedNominal();

    if (!extType->hasError()) {
      // The first condition catches syntactic forms like
      //     protocol A & B { ... } // may be protocols or typealiases
      // The second condition also looks through typealiases and catches
      //    typealias T = P1 & P2 // P2 is a refined protocol of P1
      //    extension T { ... }
      // However, it is trickier to catch cases like
      //    typealias T = P2 & P1 // P2 is a refined protocol of P1
      //    extension T { ... }
      // so we don't do that here.
      auto extTypeRepr = ED->getExtendedTypeRepr();
      auto *extTypeNominal = extType->getAnyNominal();
      bool firstNominalIsNotMostSpecific =
        extTypeNominal && extTypeNominal != nominal;
      if ((extTypeRepr && isa<CompositionTypeRepr>(extTypeRepr))
          || firstNominalIsNotMostSpecific) {
        auto diag = ED->diagnose(diag::composition_in_extended_type,
                                 nominal->getDeclaredType());
        diag.highlight(extTypeRepr->getSourceRange());
        if (firstNominalIsNotMostSpecific) {
          diag.flush();
          Type mostSpecificProtocol = extTypeNominal->getDeclaredType();
          ED->diagnose(diag::composition_in_extended_type_alternative,
                       mostSpecificProtocol)
            .fixItReplace(extTypeRepr->getSourceRange(),
                          mostSpecificProtocol->getString());
        } else {
          diag.fixItReplace(extTypeRepr->getSourceRange(),
                            nominal->getDeclaredType()->getString());
        }
      }
    }

    // Produce any diagnostics for the generic signature.
    (void) ED->getGenericSignature();
    dumpGenericSignature(Ctx, ED);

    checkInheritanceClause(ED);

    diagnoseRetroactiveConformances(ED, Ctx.Diags);

    // Only generic and protocol types are permitted to have
    // trailing where clauses.
    if (auto trailingWhereClause = ED->getTrailingWhereClause()) {
      if (!ED->getGenericParams() && !ED->isInvalid()) {
        ED->diagnose(diag::extension_nongeneric_trailing_where,
                     nominal->getDeclaredType())
          .highlight(trailingWhereClause->getSourceRange());
      }
    }

    checkGenericParams(ED);

    TypeChecker::checkDeclAttributes(ED);

    // If this is an @_objcImplementation of a class, set up some aspects of the
    // class.
    if (auto CD = dyn_cast_or_null<ClassDecl>(ED->getImplementedObjCDecl())) {
      // Force lowering of stored properties.
      (void) CD->getStoredProperties();

      // Force creation of an implicit destructor, if any.
      (void) CD->getDestructor();
      
      // FIXME: Should we duplicate any other logic from visitClassDecl()?
    }

    TypeChecker::checkObjCImplementation(ED);

    for (Decl *Member : ED->getMembers())
      visit(Member);

    TypeChecker::checkPatternBindingCaptures(ED);

    TypeChecker::checkConformancesInContext(ED);

    checkAccessControl(ED);

    checkExplicitAvailability(ED);

    TypeChecker::checkDistributedActor(SF, nominal);

    diagnoseExtensionOfMarkerProtocol(ED);

    checkTupleExtension(ED);

    checkExtensionAddsSoloInvertibleProtocol(ED);
  }

  void visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
    // See swift::performTypeChecking for TopLevelCodeDecl handling.
    llvm_unreachable("TopLevelCodeDecls are handled elsewhere");
  }
  
  void visitPoundDiagnosticDecl(PoundDiagnosticDecl *PDD) {
    if (PDD->hasBeenEmitted()) { return; }
    PDD->markEmitted();
    Ctx.Diags
        .diagnose(PDD->getMessage()->getStartLoc(),
                  PDD->isError() ? diag::pound_error : diag::pound_warning,
                  PDD->getMessage()->getValue())
        .highlight(PDD->getMessage()->getSourceRange());
  }

  void visitConstructorDecl(ConstructorDecl *CD) {
    // Force creation of the generic signature.
    (void) CD->getGenericSignature();
    dumpGenericSignature(Ctx, CD);

    // Compute these requests in case they emit diagnostics.
    (void) CD->getInterfaceType();
    (void) CD->getInitKind();

    if (!CD->isInvalid()) {
      checkGenericParams(CD);
      TypeChecker::checkReferencedGenericParams(CD);
      TypeChecker::checkProtocolSelfRequirements(CD);
    }

    TypeChecker::checkDeclAttributes(CD);
    TypeChecker::checkParameterList(CD->getParameters(), CD);

    if (CD->getAsyncLoc().isValid())
      TypeChecker::checkConcurrencyAvailability(CD->getAsyncLoc(), CD);

    // Check whether this initializer overrides an initializer in its
    // superclass.
    if (!checkOverrides(CD)) {
      auto DC = CD->getDeclContext();
      auto isClassContext = DC->getSelfClassDecl() != nullptr;
      auto isStructOrEnumContext = DC->getSelfEnumDecl() != nullptr ||
                                   DC->getSelfStructDecl() != nullptr;
      // If an initializer has an override attribute but does not override
      // anything or overrides something that doesn't need an 'override'
      // keyword (e.g., a convenience initializer), complain.
      // anything, or overrides something that complain.
      if (auto *attr = CD->getAttrs().getAttribute<OverrideAttr>()) {
        if (!CD->getOverriddenDecl()) {
          if (isStructOrEnumContext) {
            CD->diagnose(diag::override_nonclass_decl)
                .highlight(attr->getLocation())
                .fixItRemove(attr->getRange());
          } else {
            CD->diagnose(diag::initializer_does_not_override, isClassContext)
                .highlight(attr->getLocation());
          }
          attr->setInvalid();
        } else if (attr->isImplicit()) {
          // Don't diagnose implicit attributes.
        } else if (overrideRequiresKeyword(CD->getOverriddenDecl())
                     == OverrideRequiresKeyword::Never) {
          // Special case: we are overriding a 'required' initializer, so we
          // need (only) the 'required' keyword.
          if (cast<ConstructorDecl>(CD->getOverriddenDecl())->isRequired()) {
            if (CD->getAttrs().hasAttribute<RequiredAttr>()) {
              CD->diagnose(diag::required_initializer_override_keyword)
                  .fixItRemove(attr->getLocation());
            } else {
              CD->diagnose(diag::required_initializer_override_wrong_keyword)
                  .fixItReplace(attr->getLocation(), "required");
              CD->getAttrs().add(new (Ctx) RequiredAttr(/*IsImplicit=*/true));
            }

            auto *reqInit =
                findNonImplicitRequiredInit(CD->getOverriddenDecl());
            reqInit->diagnose(diag::overridden_required_initializer_here);
          } else {
            // We tried to override a convenience initializer.
            CD->diagnose(diag::initializer_does_not_override, isClassContext)
                .highlight(attr->getLocation());
            CD->getOverriddenDecl()->diagnose(
                diag::convenience_init_override_here);
          }
        }
      }

      // A failable initializer cannot override a non-failable one.
      // This would normally be diagnosed by the covariance rules;
      // however, those are disabled so that we can provide a more
      // specific diagnostic here.
      if (CD->isFailable() &&
          CD->getOverriddenDecl() &&
          !CD->getOverriddenDecl()->isFailable()) {
        CD->diagnose(diag::failable_initializer_override, CD->getName());
        auto *OD = CD->getOverriddenDecl();
        OD->diagnose(diag::nonfailable_initializer_override_here,
                     OD->getName());
      }
    }

    checkImplementationOnlyOverride(CD);

    // If this initializer overrides a 'required' initializer, it must itself
    // be marked 'required'.
    if (!CD->getAttrs().hasAttribute<RequiredAttr>()) {
      if (CD->getOverriddenDecl() && CD->getOverriddenDecl()->isRequired()) {
        CD->diagnose(diag::required_initializer_missing_keyword)
            .fixItInsert(CD->getLoc(), "required ");

        auto *reqInit = findNonImplicitRequiredInit(CD->getOverriddenDecl());
        reqInit->diagnose(diag::overridden_required_initializer_here);

        CD->getAttrs().add(new (Ctx) RequiredAttr(/*IsImplicit=*/true));
      }
    }

    if (CD->isRequired()) {
      if (auto nominal = CD->getDeclContext()->getSelfNominalTypeDecl()) {
        AccessLevel requiredAccess;
        switch (nominal->getFormalAccess()) {
        case AccessLevel::Open:
          requiredAccess = AccessLevel::Public;
          break;
        case AccessLevel::Package:
        case AccessLevel::Public:
        case AccessLevel::Internal:
          requiredAccess = AccessLevel::Internal;
          break;
        case AccessLevel::FilePrivate:
        case AccessLevel::Private:
          requiredAccess = AccessLevel::FilePrivate;
          break;
        }
        if (CD->getFormalAccess() < requiredAccess) {
          auto diag = CD->diagnose(diag::required_initializer_not_accessible,
                                   nominal->getName());
          fixItAccess(diag, CD, requiredAccess);
        }
      }
    }

    checkAccessControl(CD);

    checkExplicitAvailability(CD);

    if (CD->getDeclContext()->isLocalContext()) {
      // Check local function bodies right away.
      (void)CD->getTypecheckedBody();
    } else if (!CD->isBodySkipped()) {
      addDelayedFunction(CD);
    }

    checkDefaultArguments(CD->getParameters());
    checkVariadicParameters(CD->getParameters(), CD);

    TypeChecker::checkObjCImplementation(CD);
  }

  void visitDestructorDecl(DestructorDecl *DD) {
    // Only check again for destructor decl outside of a struct/enum/class
    // if our destructor is not marked as invalid.
    if (!DD->isInvalid()) {
      auto *nom = dyn_cast<NominalTypeDecl>(
                             DD->getDeclContext()->getImplementedObjCContext());
      if (!nom || !isa<ClassDecl, StructDecl, EnumDecl>(nom)) {
        DD->diagnose(diag::destructor_decl_outside_class_or_noncopyable);
      }

      // Temporarily ban deinit on noncopyable enums, unless the experimental
      // feature flag is set.
      if (!Ctx.LangOpts.hasFeature(Feature::MoveOnlyEnumDeinits)
          && isa<EnumDecl>(nom)
          && !nom->canBeCopyable()) {
        DD->diagnose(diag::destructor_decl_on_noncopyable_enum);
      }
    }

    TypeChecker::checkDeclAttributes(DD);

    if (DD->getDeclContext()->isLocalContext()) {
      // Check local function bodies right away.
      (void)DD->getTypecheckedBody();
    } else if (!DD->isBodySkipped()) {
      addDelayedFunction(DD);
    }
  }

  void visitBuiltinTupleDecl(BuiltinTupleDecl *BTD) {
    llvm_unreachable("BuiltinTupleDecl should not show up here");
  }
};
} // end anonymous namespace

void TypeChecker::typeCheckDecl(Decl *D) {
  auto *SF = D->getDeclContext()->getParentSourceFile();
  DeclChecker(D->getASTContext(), SF).visit(D);
}

void TypeChecker::checkParameterList(ParameterList *params,
                                     DeclContext *owner) {
  std::optional<ParamDecl *> firstIsolatedParam;
  bool diagnosedDuplicateIsolatedParam = false;
  for (auto param: *params) {
    checkDeclAttributes(param);

    // async autoclosures can only occur as parameters to async functions.
    if (param->isAutoClosure()) {
      if (auto fnType = param->getInterfaceType()->getAs<FunctionType>()) {
        if (fnType->isAsync() &&
            !(isa<AbstractFunctionDecl>(owner) &&
              cast<AbstractFunctionDecl>(owner)->isAsyncContext())) {
          param->diagnose(diag::async_autoclosure_nonasync_function);
          if (auto func = dyn_cast<FuncDecl>(owner))
            addAsyncNotes(func);
        }
      }
    }

    // check for well-formed isolated parameters.
    if (!diagnosedDuplicateIsolatedParam) {
      if (param->isIsolated()) {
        if (firstIsolatedParam) {
          // cannot have more than one isolated parameter (SE-0313)
          param->diagnose(diag::isolated_parameter_duplicate)
              .highlight(param->getSourceRange())
              .warnUntilSwiftVersion(6);
          // I'd love to describe the context in which there is an isolated parameter,
          // we had a DescriptiveDeclContextKind, but that only
          // exists for Decls.

          auto prevIso = firstIsolatedParam.value();
          prevIso
              ->diagnose(diag::isolated_parameter_previous_note,
                         prevIso->getName())
              .highlight(prevIso->getSourceRange());

          // no need to complain about any further `isolated` params
          diagnosedDuplicateIsolatedParam = true;
        } else {
          firstIsolatedParam = param; // save first one we've seen.
        }
      }
    }

    // Opaque types cannot occur in parameter position.
    if (!isa<ClosureExpr>(owner)) {
      Type interfaceType = param->getInterfaceType();
      if (interfaceType->hasTypeParameter()) {
        interfaceType.findIf([&](Type type) {
          if (auto fnType = type->getAs<FunctionType>()) {
            for (auto innerParam : fnType->getParams()) {
              auto paramType = innerParam.getPlainType();
              if (!paramType->hasTypeParameter())
                continue;

              bool hadError = paramType.findIf([&](Type innerType) {
                auto genericParam = innerType->getAs<GenericTypeParamType>();
                if (!genericParam)
                  return false;

                auto genericParamDecl = genericParam->getOpaqueDecl();
                if (!genericParamDecl)
                  return false;

                param->diagnose(
                   diag::opaque_type_in_parameter, true, interfaceType);
                return true;
              });

              if (hadError)
                return true;
            }

            return false;
          }

          return false;
        });
      }
    }

    if (param->hasAttachedPropertyWrapper())
      (void) param->getPropertyWrapperInitializerInfo();

    if (!param->isInvalid()) {
      auto *SF = owner->getParentSourceFile();
      param->visitAuxiliaryDecls([&](VarDecl *auxiliaryDecl) {
        if (!isa<ParamDecl>(auxiliaryDecl))
          DeclChecker(SF->getASTContext(), SF).visitBoundVariable(auxiliaryDecl);
      });
    }

    // If we have a noimplicitcopy parameter, make sure that the underlying type
    // is not move only. It is redundant.
    if (auto attr = param->getAttrs().getAttribute<NoImplicitCopyAttr>()) {
      if (auto *nom = param->getInterfaceType()->getNominalOrBoundGenericNominal()) {
        if (!nom->canBeCopyable()) {
          param->diagnose(diag::noimplicitcopy_attr_not_allowed_on_moveonlytype)
            .fixItRemove(attr->getRange());
        }
      }
    }

    // @_staticExclusiveOnly types cannot be passed as 'inout', only as either
    // a borrow or as consuming.
    if (auto SD = param->getInterfaceType()->getStructOrBoundGenericStruct()) {
      if (SD->getAttrs().hasAttribute<StaticExclusiveOnlyAttr>() &&
          param->isInOut()) {
        SD->getASTContext().Diags.diagnoseWithNotes(
          param->diagnose(diag::attr_static_exclusive_only_let_only_param,
                          param->getInterfaceType()),
          [&]() {
            SD->diagnose(diag::attr_static_exclusive_only_type_nonmutating,
                       SD->getDeclaredInterfaceType());
          });
      }
    }
  }

  // For source compatibility, allow duplicate internal parameter names
  // on protocol requirements.
  //
  // FIXME: Consider turning this into a warning or error if we do
  // another -swift-version.
  if (!isa<ProtocolDecl>(owner->getParent())) {
    // Check for duplicate parameter names.
    diagnoseDuplicateDecls(*params);
  }
}

std::optional<unsigned>
ExpandMacroExpansionDeclRequest::evaluate(Evaluator &evaluator,
                                          MacroExpansionDecl *MED) const {
  auto &ctx = MED->getASTContext();
  auto *dc = MED->getDeclContext();

  // Resolve macro candidates.
  auto macro = evaluateOrDefault(ctx.evaluator, ResolveMacroRequest{MED, dc},
                                 ConcreteDeclRef());
  if (!macro)
    return std::nullopt;
  MED->setMacroRef(macro);

  auto roles = cast<MacroDecl>(macro.getDecl())->getMacroRoles();
  // If it's not a declaration macro or a code item macro, it must have been
  // parsed as an expression macro, and this decl is just its substitute decl.
  // So there's no thing to be done here.
  if (!roles.contains(MacroRole::Declaration) &&
      !roles.contains(MacroRole::CodeItem))
    return std::nullopt;

  return expandFreestandingMacro(MED);
}
