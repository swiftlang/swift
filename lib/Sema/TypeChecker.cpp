//===--- TypeChecker.cpp - Type Checking ----------------------------------===//
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
// This file implements the swift::performTypeChecking entry point for
// semantic analysis.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "TypeChecker.h"
#include "MiscDiagnostics.h"
#include "GenericTypeResolver.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/Timer.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Parse/Lexer.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Strings.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"
#include <algorithm>

using namespace swift;

TypeChecker::TypeChecker(ASTContext &Ctx, DiagnosticEngine &Diags)
  : Context(Ctx), Diags(Diags)
{
  auto clangImporter =
    static_cast<ClangImporter *>(Context.getClangModuleLoader());
  clangImporter->setTypeResolver(*this);

  Context.setLazyResolver(this);
}

TypeChecker::~TypeChecker() {
  auto clangImporter =
    static_cast<ClangImporter *>(Context.getClangModuleLoader());
  clangImporter->clearTypeResolver();

  Context.setLazyResolver(nullptr);
}

ProtocolDecl *TypeChecker::getProtocol(SourceLoc loc, KnownProtocolKind kind) {
  auto protocol = Context.getProtocol(kind);
  if (!protocol && loc.isValid()) {
    diagnose(loc, diag::missing_protocol,
             Context.getIdentifier(getProtocolName(kind)));
  }

  if (protocol && !protocol->hasInterfaceType()) {
    validateDecl(protocol);
    if (protocol->isInvalid())
      return nullptr;
  }

  return protocol;
}

ProtocolDecl *TypeChecker::getLiteralProtocol(Expr *expr) {
  if (isa<ArrayExpr>(expr))
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::ExpressibleByArrayLiteral);

  if (isa<DictionaryExpr>(expr))
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::ExpressibleByDictionaryLiteral);

  if (!isa<LiteralExpr>(expr))
    return nullptr;
  
  if (isa<NilLiteralExpr>(expr))
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::ExpressibleByNilLiteral);
  
  if (isa<IntegerLiteralExpr>(expr))
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::ExpressibleByIntegerLiteral);

  if (isa<FloatLiteralExpr>(expr))
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::ExpressibleByFloatLiteral);

  if (isa<BooleanLiteralExpr>(expr))
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::ExpressibleByBooleanLiteral);

  if (const auto *SLE = dyn_cast<StringLiteralExpr>(expr)) {
    if (SLE->isSingleUnicodeScalar())
      return getProtocol(
          expr->getLoc(),
          KnownProtocolKind::ExpressibleByUnicodeScalarLiteral);

    if (SLE->isSingleExtendedGraphemeCluster())
      return getProtocol(
          expr->getLoc(),
          KnownProtocolKind::ExpressibleByExtendedGraphemeClusterLiteral);

    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::ExpressibleByStringLiteral);
  }

  if (isa<InterpolatedStringLiteralExpr>(expr))
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::ExpressibleByStringInterpolation);

  if (auto E = dyn_cast<MagicIdentifierLiteralExpr>(expr)) {
    switch (E->getKind()) {
    case MagicIdentifierLiteralExpr::File:
    case MagicIdentifierLiteralExpr::Function:
      return getProtocol(expr->getLoc(),
                         KnownProtocolKind::ExpressibleByStringLiteral);

    case MagicIdentifierLiteralExpr::Line:
    case MagicIdentifierLiteralExpr::Column:
      return getProtocol(expr->getLoc(),
                         KnownProtocolKind::ExpressibleByIntegerLiteral);

    case MagicIdentifierLiteralExpr::DSOHandle:
      return nullptr;
    }
  }

  if (auto E = dyn_cast<ObjectLiteralExpr>(expr)) {
    switch (E->getLiteralKind()) {
#define POUND_OBJECT_LITERAL(Name, Desc, Protocol)\
    case ObjectLiteralExpr::Name:\
      return getProtocol(expr->getLoc(), KnownProtocolKind::Protocol);
#include "swift/Syntax/TokenKinds.def"
    }
  }

  return nullptr;
}

DeclName TypeChecker::getObjectLiteralConstructorName(ObjectLiteralExpr *expr) {
  switch (expr->getLiteralKind()) {
  case ObjectLiteralExpr::colorLiteral: {
    return DeclName(Context, DeclBaseName::createConstructor(),
                    { Context.getIdentifier("_colorLiteralRed"),
                      Context.getIdentifier("green"),
                      Context.getIdentifier("blue"),
                      Context.getIdentifier("alpha") });
  }
  case ObjectLiteralExpr::imageLiteral: {
    return DeclName(Context, DeclBaseName::createConstructor(),
                    { Context.getIdentifier("imageLiteralResourceName") });
  }
  case ObjectLiteralExpr::fileLiteral: {
    return DeclName(Context, DeclBaseName::createConstructor(),
            { Context.getIdentifier("fileReferenceLiteralResourceName") });
  }
  }
  llvm_unreachable("unknown literal constructor");
}

/// Return an idealized form of the parameter type of the given
/// object-literal initializer.  This removes references to the protocol
/// name from the first argument label, which would be otherwise be
/// redundant when writing out the object-literal syntax:
///
///   #fileLiteral(fileReferenceLiteralResourceName: "hello.jpg")
///
/// Doing this allows us to preserve a nicer (and source-compatible)
/// literal syntax while still giving the initializer a semantically
/// unambiguous name.
Type TypeChecker::getObjectLiteralParameterType(ObjectLiteralExpr *expr,
                                                ConstructorDecl *ctor) {
  Type argType = ctor->getArgumentInterfaceType();
  auto argTuple = argType->getAs<TupleType>();
  if (!argTuple) return argType;

  auto replace = [&](StringRef replacement) -> Type {
    SmallVector<TupleTypeElt, 4> elements;
    elements.append(argTuple->getElements().begin(),
                    argTuple->getElements().end());
    elements[0] = TupleTypeElt(elements[0].getType(),
                               Context.getIdentifier(replacement));
    return TupleType::get(elements, Context);
  };

  switch (expr->getLiteralKind()) {
  case ObjectLiteralExpr::colorLiteral:
    return replace("red");
  case ObjectLiteralExpr::fileLiteral:
  case ObjectLiteralExpr::imageLiteral:
    return replace("resourceName");
  }
  llvm_unreachable("unknown literal constructor");
}

ModuleDecl *TypeChecker::getStdlibModule(const DeclContext *dc) {
  if (StdlibModule)
    return StdlibModule;

  if (!StdlibModule)
    StdlibModule = Context.getStdlibModule();
  if (!StdlibModule)
    StdlibModule = dc->getParentModule();

  assert(StdlibModule && "no main module found");
  Context.recordKnownProtocols(StdlibModule);
  return StdlibModule;
}

Type TypeChecker::lookupBoolType(const DeclContext *dc) {
  if (!boolType) {
    boolType = ([&] {
      SmallVector<ValueDecl *, 2> results;
      getStdlibModule(dc)->lookupValue({}, Context.getIdentifier("Bool"),
                                       NLKind::QualifiedLookup, results);
      if (results.size() != 1) {
        diagnose(SourceLoc(), diag::broken_bool);
        return Type();
      }

      auto tyDecl = dyn_cast<NominalTypeDecl>(results.front());
      if (!tyDecl) {
        diagnose(SourceLoc(), diag::broken_bool);
        return Type();
      }

      return tyDecl->getDeclaredType();
    })();
  }
  return *boolType;
}

/// Clone the given generic parameters in the given list. We don't need any
/// of the requirements, because they will be inferred.
static GenericParamList *cloneGenericParams(ASTContext &ctx,
                                            DeclContext *dc,
                                            GenericParamList *fromParams) {
  // Clone generic parameters.
  SmallVector<GenericTypeParamDecl *, 2> toGenericParams;
  for (auto fromGP : *fromParams) {
    // Create the new generic parameter.
    auto toGP = new (ctx) GenericTypeParamDecl(dc, fromGP->getName(),
                                               SourceLoc(),
                                               fromGP->getDepth(),
                                               fromGP->getIndex());
    toGP->setImplicit(true);

    // Record new generic parameter.
    toGenericParams.push_back(toGP);
  }

  auto toParams = GenericParamList::create(ctx, SourceLoc(), toGenericParams,
                                           SourceLoc());

  auto outerParams = fromParams->getOuterParameters();
  if (outerParams != nullptr)
    outerParams = cloneGenericParams(ctx, dc, outerParams);
  toParams->setOuterParameters(outerParams);

  return toParams;
}

static void bindExtensionDecl(ExtensionDecl *ED, TypeChecker &TC) {
  if (ED->getExtendedType())
    return;

  // If we didn't parse a type, fill in an error type and bail out.
  if (!ED->getExtendedTypeLoc().getTypeRepr()) {
    ED->setInvalid();
    ED->getExtendedTypeLoc().setInvalidType(TC.Context);
    return;
  }

  auto dc = ED->getDeclContext();

  // Validate the representation.
  // FIXME: Perform some kind of "shallow" validation here?
  TypeResolutionOptions options;
  options |= TypeResolutionFlags::AllowUnboundGenerics;
  options |= TypeResolutionFlags::ExtensionBinding;
  if (TC.validateType(ED->getExtendedTypeLoc(), dc, options)) {
    ED->setInvalid();
    return;
  }

  // Dig out the extended type.
  auto extendedType = ED->getExtendedType();

  // Hack to allow extending a generic typealias.
  if (auto *unboundGeneric = extendedType->getAs<UnboundGenericType>()) {
    if (auto *aliasDecl = dyn_cast<TypeAliasDecl>(unboundGeneric->getDecl())) {
      auto extendedNominal = aliasDecl->getDeclaredInterfaceType()->getAnyNominal();
      if (extendedNominal) {
        extendedType = extendedNominal->getDeclaredType();
        if (!isPassThroughTypealias(aliasDecl))
          ED->getExtendedTypeLoc().setType(extendedType);
      }
    }
  }

  // Handle easy cases.

  // Cannot extend a metatype.
  if (extendedType->is<AnyMetatypeType>()) {
    TC.diagnose(ED->getLoc(), diag::extension_metatype, extendedType)
      .highlight(ED->getExtendedTypeLoc().getSourceRange());
    ED->setInvalid();
    ED->getExtendedTypeLoc().setInvalidType(TC.Context);
    return;
  }

  // Cannot extend a bound generic type.
  if (extendedType->isSpecialized()) {
    TC.diagnose(ED->getLoc(), diag::extension_specialization,
                extendedType->getAnyNominal()->getName())
      .highlight(ED->getExtendedTypeLoc().getSourceRange());
    ED->setInvalid();
    ED->getExtendedTypeLoc().setInvalidType(TC.Context);
    return;
  }

  // Dig out the nominal type being extended.
  NominalTypeDecl *extendedNominal = extendedType->getAnyNominal();
  if (!extendedNominal) {
    TC.diagnose(ED->getLoc(), diag::non_nominal_extension, extendedType)
      .highlight(ED->getExtendedTypeLoc().getSourceRange());
    ED->setInvalid();
    ED->getExtendedTypeLoc().setInvalidType(TC.Context);
    return;
  }
  assert(extendedNominal && "Should have the nominal type being extended");

  // If the extended type is generic or is a protocol. Clone or create
  // the generic parameters.
  if (extendedNominal->getGenericParamsOfContext() ||
      isa<ProtocolDecl>(extendedNominal)) {
    if (auto proto = dyn_cast<ProtocolDecl>(extendedNominal)) {
      // For a protocol extension, build the generic parameter list.
      ED->setGenericParams(proto->createGenericParams(ED));
    } else {
      // Clone the existing generic parameter list.
      ED->setGenericParams(
        cloneGenericParams(TC.Context, ED,
                           extendedNominal->getGenericParamsOfContext()));
    }
  }

  // If we have a trailing where clause, deal with it now.
  // For now, trailing where clauses are only permitted on protocol extensions.
  if (auto trailingWhereClause = ED->getTrailingWhereClause()) {
    if (!(extendedNominal->getGenericParamsOfContext() ||
          isa<ProtocolDecl>(extendedNominal))) {
      // Only generic and protocol types are permitted to have
      // trailing where clauses.
      TC.diagnose(ED, diag::extension_nongeneric_trailing_where, extendedType)
        .highlight(trailingWhereClause->getSourceRange());
      ED->setTrailingWhereClause(nullptr);
    } else {
      // Merge the trailing where clause into the generic parameter list.
      // FIXME: Long-term, we'd like clients to deal with the trailing where
      // clause explicitly, but for now it's far more direct to represent
      // the trailing where clause as part of the requirements.
      ED->getGenericParams()->addTrailingWhereClause(
        TC.Context,
        trailingWhereClause->getWhereLoc(),
        trailingWhereClause->getRequirements());
    }
  }

  extendedNominal->addExtension(ED);
}

void TypeChecker::bindExtension(ExtensionDecl *ext) {
  ::bindExtensionDecl(ext, *this);
}
void TypeChecker::resolveExtensionForConformanceConstruction(
    ExtensionDecl *ext,
    SmallVectorImpl<ConformanceConstructionInfo> &protocols) {
  // To be able to know the conformances that an extension declares, we just
  // need to know which type it is connected to:
  ::bindExtensionDecl(ext, *this);

  // and the protocols which it inherits from:
  DependentGenericTypeResolver resolver;
  TypeResolutionOptions options = TypeResolutionFlags::GenericSignature;
  options |= TypeResolutionFlags::InheritanceClause;
  options |= TypeResolutionFlags::AllowUnavailableProtocol;
  options |= TypeResolutionFlags::ResolveStructure;
  for (auto &inherited : ext->getInherited()) {
    // We don't want to have know about any generic params/archetypes, because
    // that requires knowing a full generic environment for the extension (which
    // can recur with conformance construction). Furthermore, we only *need* to
    // resolve the protocol references, which won't involve any archetypes: an
    // invalid inheritance like `struct Foo<T> {} extension Foo: SomeClass<T>
    // {}` isn't relevant for conformance construction and is caught elsewhere.
    auto type = inherited.getType();
    if (!type)
      type = resolveType(inherited.getTypeRepr(), ext, options, &resolver);

    if (type && type->isExistentialType()) {
      auto layout = type->getExistentialLayout();
      for (auto proto : layout.getProtocols())
        protocols.push_back({inherited.getLoc(), proto->getDecl()});
    }
  }
}

static void typeCheckFunctionsAndExternalDecls(SourceFile &SF, TypeChecker &TC) {
  unsigned currentFunctionIdx = 0;
  unsigned currentExternalDef = TC.Context.LastCheckedExternalDefinition;
  unsigned currentSynthesizedDecl = SF.LastCheckedSynthesizedDecl;
  do {
    // Type check conformance contexts.
    for (unsigned i = 0; i != TC.ConformanceContexts.size(); ++i) {
      auto decl = TC.ConformanceContexts[i];
      if (auto *ext = dyn_cast<ExtensionDecl>(decl))
        TC.checkConformancesInContext(ext, ext);
      else {
        auto *ntd = cast<NominalTypeDecl>(decl);
        TC.checkConformancesInContext(ntd, ntd);

        // Finally, we can check classes for missing initializers.
        if (auto *classDecl = dyn_cast<ClassDecl>(ntd))
          TC.maybeDiagnoseClassWithoutInitializers(classDecl);
      }
    }
    TC.ConformanceContexts.clear();

    // Type check the body of each of the function in turn.  Note that outside
    // functions must be visited before nested functions for type-checking to
    // work correctly.
    for (unsigned n = TC.definedFunctions.size(); currentFunctionIdx != n;
         ++currentFunctionIdx) {
      auto *AFD = TC.definedFunctions[currentFunctionIdx];

      TC.typeCheckAbstractFunctionBody(AFD);
    }

    // Type check external definitions.
    for (unsigned n = TC.Context.ExternalDefinitions.size();
         currentExternalDef != n;
         ++currentExternalDef) {
      auto decl = TC.Context.ExternalDefinitions[currentExternalDef];

      if (auto *AFD = dyn_cast<AbstractFunctionDecl>(decl)) {
        TC.typeCheckAbstractFunctionBody(AFD);
        TC.checkFunctionErrorHandling(AFD);
        continue;
      }
      if (isa<NominalTypeDecl>(decl))
        continue;
      if (isa<VarDecl>(decl))
        continue;
      llvm_unreachable("Unhandled external definition kind");
    }

    // Complete any protocol requirement signatures that were delayed
    // because the protocol was validated via validateDeclForNameLookup().
    while (!TC.DelayedRequirementSignatures.empty()) {
      auto decl = TC.DelayedRequirementSignatures.pop_back_val();
      if (decl->isInvalid() || TC.Context.hadError())
        continue;

      TC.validateDecl(decl);
    }

    // Validate any referenced declarations for SIL's purposes.
    // Note: if we ever start putting extension members in vtables, we'll need
    // to validate those members too.
    // FIXME: If we're not planning to run SILGen, this is wasted effort.
    while (!TC.DeclsToFinalize.empty()) {
      auto decl = TC.DeclsToFinalize.pop_back_val();
      if (decl->isInvalid() || TC.Context.hadError())
        continue;

      TC.finalizeDecl(decl);
    }

    // Type check synthesized functions and their bodies.
    for (unsigned n = SF.SynthesizedDecls.size();
         currentSynthesizedDecl != n;
         ++currentSynthesizedDecl) {
      auto decl = SF.SynthesizedDecls[currentSynthesizedDecl];
      TC.typeCheckDecl(decl);
    }

    // Ensure that the requirements of the given conformance are
    // fully checked.
    for (unsigned i = 0; i != TC.PartiallyCheckedConformances.size(); ++i) {
      auto conformance = TC.PartiallyCheckedConformances[i];
      TC.checkConformanceRequirements(conformance);
    }
    TC.PartiallyCheckedConformances.clear();

    // Complete any conformances that we used.
    for (unsigned i = 0; i != TC.UsedConformances.size(); ++i) {
      auto conformance = TC.UsedConformances[i];
      if (conformance->isIncomplete())
        TC.checkConformance(conformance);
    }
    TC.UsedConformances.clear();

  } while (currentFunctionIdx < TC.definedFunctions.size() ||
           currentExternalDef < TC.Context.ExternalDefinitions.size() ||
           currentSynthesizedDecl < SF.SynthesizedDecls.size() ||
           !TC.DeclsToFinalize.empty() ||
           !TC.ConformanceContexts.empty() ||
           !TC.DelayedRequirementSignatures.empty() ||
           !TC.UsedConformances.empty() ||
           !TC.PartiallyCheckedConformances.empty());

  // FIXME: Horrible hack. Store this somewhere more appropriate.
  TC.Context.LastCheckedExternalDefinition = currentExternalDef;
  SF.LastCheckedSynthesizedDecl = currentSynthesizedDecl;

  // Now that all types have been finalized, run any delayed
  // circularity checks.
  // This has been written carefully to fail safe + finitely if
  // for some reason a type gets re-delayed in a non-assertions
  // build in an otherwise successful build.
  // Types can be redelayed in a failing build because we won't
  // type-check required declarations from different files.
  for (size_t i = 0, e = TC.DelayedCircularityChecks.size(); i != e; ++i) {
    TC.checkDeclCircularity(TC.DelayedCircularityChecks[i]);
    assert((e == TC.DelayedCircularityChecks.size() ||
            TC.Context.hadError()) &&
           "circularity checking for type was re-delayed!");
  }
  TC.DelayedCircularityChecks.clear();

  // Compute captures for functions and closures we visited.
  for (AnyFunctionRef closure : TC.ClosuresWithUncomputedCaptures) {
    TC.computeCaptures(closure);
  }
  for (AbstractFunctionDecl *FD : reversed(TC.definedFunctions)) {
    TC.computeCaptures(FD);
  }

  // Check error-handling correctness for all the functions defined in
  // this file.  This can depend on all of their interior function
  // bodies having been type-checked.
  for (AbstractFunctionDecl *FD : TC.definedFunctions) {
    TC.checkFunctionErrorHandling(FD);
  }
}

void swift::typeCheckExternalDefinitions(SourceFile &SF) {
  assert(SF.ASTStage == SourceFile::TypeChecked);
  auto &Ctx = SF.getASTContext();
  TypeChecker TC(Ctx);
  typeCheckFunctionsAndExternalDecls(SF, TC);
}

void swift::performTypeChecking(SourceFile &SF, TopLevelContext &TLC,
                                OptionSet<TypeCheckingFlags> Options,
                                unsigned StartElem,
                                unsigned WarnLongFunctionBodies,
                                unsigned WarnLongExpressionTypeChecking,
                                unsigned ExpressionTimeoutThreshold) {
  if (SF.ASTStage == SourceFile::TypeChecked)
    return;

  auto &Ctx = SF.getASTContext();

  // Make sure we have a type checker.
  //
  // FIXME: We should never have a type checker here, but currently do when
  // we're using immediate together with -enable-source-import.
  //
  // This possibility should be eliminated, since it results in duplicated
  // work.
  Optional<TypeChecker> MyTC;
  if (!Ctx.getLazyResolver())
    MyTC.emplace(Ctx);

  // Make sure that name binding has been completed before doing any type
  // checking.
    performNameBinding(SF, StartElem);

  {
    // NOTE: The type checker is scoped to be torn down before AST
    // verification.
    SharedTimer timer("Type checking / Semantic analysis");

    if (MyTC) {
      MyTC->setWarnLongFunctionBodies(WarnLongFunctionBodies);
      MyTC->setWarnLongExpressionTypeChecking(WarnLongExpressionTypeChecking);
      if (ExpressionTimeoutThreshold != 0)
        MyTC->setExpressionTimeoutThreshold(ExpressionTimeoutThreshold);

      if (Options.contains(TypeCheckingFlags::DebugTimeFunctionBodies))
        MyTC->enableDebugTimeFunctionBodies();

      if (Options.contains(TypeCheckingFlags::DebugTimeExpressions))
        MyTC->enableDebugTimeExpressions();

      if (Options.contains(TypeCheckingFlags::ForImmediateMode))
        MyTC->setInImmediateMode(true);
      
      // Lookup the swift module.  This ensures that we record all known
      // protocols in the AST.
      (void) MyTC->getStdlibModule(&SF);

      if (!Ctx.LangOpts.DisableAvailabilityChecking) {
        // Build the type refinement hierarchy for the primary
        // file before type checking.
        MyTC->buildTypeRefinementContextHierarchy(SF, StartElem);
      }
    }

    TypeChecker &TC =
      MyTC ? *MyTC : *static_cast<TypeChecker *>(Ctx.getLazyResolver());

    // Resolve extensions. This has to occur first during type checking,
    // because the extensions need to be wired into the AST for name lookup
    // to work.
    // FIXME: We can have interesting ordering dependencies among the various
    // extensions, so we'll need to be smarter here.
    // FIXME: The current source file needs to be handled specially, because of
    // private extensions.
    SF.forAllVisibleModules([&](ModuleDecl::ImportedModule import) {
      // FIXME: Respect the access path?
      for (auto file : import.second->getFiles()) {
        auto SF = dyn_cast<SourceFile>(file);
        if (!SF)
          continue;

        for (auto D : SF->Decls) {
          if (auto ED = dyn_cast<ExtensionDecl>(D))
            bindExtensionDecl(ED, TC);
        }
      }

      return true;
    });

    // Type check the top-level elements of the source file.
    bool hasTopLevelCode = false;
    for (auto D : llvm::makeArrayRef(SF.Decls).slice(StartElem)) {
      if (auto *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
        hasTopLevelCode = true;
        // Immediately perform global name-binding etc.
        TC.typeCheckTopLevelCodeDecl(TLCD);
      } else {
        TC.typeCheckDecl(D);
      }
    }

    if (hasTopLevelCode) {
      TC.contextualizeTopLevelCode(TLC,
                             llvm::makeArrayRef(SF.Decls).slice(StartElem));
    }

    // If we're in REPL mode, inject temporary result variables and other stuff
    // that the REPL needs to synthesize.
    if (SF.Kind == SourceFileKind::REPL && !Ctx.hadError())
      TC.processREPLTopLevel(SF, TLC, StartElem);

    typeCheckFunctionsAndExternalDecls(SF, TC);
  }

  // Checking that benefits from having the whole module available.
  if (!(Options & TypeCheckingFlags::DelayWholeModuleChecking)) {
    performWholeModuleTypeChecking(SF);
  }

  MyTC.reset();

  // Verify that we've checked types correctly.
  SF.ASTStage = SourceFile::TypeChecked;

  {
    SharedTimer timer("AST verification");
    // Verify the SourceFile.
    verify(SF);

    // Verify imported modules.
    //
    // Skip per-file verification in whole-module mode. Verifying imports
    // between files could cause the importer to cache declarations without
    // adding them to the ASTContext. This happens when the importer registers a
    // declaration without a valid TypeChecker instance, as is the case during
    // verification. A subsequent file may require that declaration to be fully
    // imported (e.g. to synthesized a function body), but since it has already
    // been cached, it will never be added to the ASTContext. The solution is to
    // skip verification and avoid caching it.
#ifndef NDEBUG
    if (!(Options & TypeCheckingFlags::DelayWholeModuleChecking) &&
        SF.Kind != SourceFileKind::REPL &&
        SF.Kind != SourceFileKind::SIL &&
        !Ctx.LangOpts.DebuggerSupport) {
      Ctx.verifyAllLoadedModules();
    }
#endif
  }
}

void swift::performWholeModuleTypeChecking(SourceFile &SF) {
  auto &Ctx = SF.getASTContext();
  FrontendStatsTracer tracer(Ctx.Stats, "perform-whole-module-type-checking");
  Ctx.diagnoseAttrsRequiringFoundation(SF);
  Ctx.diagnoseObjCMethodConflicts(SF);
  Ctx.diagnoseObjCUnsatisfiedOptReqConflicts(SF);
  Ctx.diagnoseUnintendedObjCMethodOverrides(SF);

  // In whole-module mode, import verification is deferred until all files have
  // been type checked. This avoids caching imported declarations when a valid
  // type checker is not present. The same declaration may need to be fully
  // imported by subsequent files.
  //
  // FIXME: some playgrounds tests (playground_lvalues.swift) fail with
  // verification enabled.
#if 0
  if (SF.Kind != SourceFileKind::REPL &&
      SF.Kind != SourceFileKind::SIL &&
      !Ctx.LangOpts.DebuggerSupport) {
    Ctx.verifyAllLoadedModules();
  }
#endif
}

bool swift::performTypeLocChecking(ASTContext &Ctx, TypeLoc &T,
                                   DeclContext *DC,
                                   bool ProduceDiagnostics) {
  return performTypeLocChecking(
                            Ctx, T,
                            /*isSILMode=*/false,
                            /*isSILType=*/false,
                            /*GenericEnv=*/DC->getGenericEnvironmentOfContext(),
                            DC, ProduceDiagnostics);
}

bool swift::performTypeLocChecking(ASTContext &Ctx, TypeLoc &T,
                                   bool isSILMode,
                                   bool isSILType,
                                   GenericEnvironment *GenericEnv,
                                   DeclContext *DC,
                                   bool ProduceDiagnostics) {
  TypeResolutionOptions options;

  // Fine to have unbound generic types.
  options |= TypeResolutionFlags::AllowUnboundGenerics;
  if (isSILMode) {
    options |= TypeResolutionFlags::SILMode;
    options |= TypeResolutionFlags::AllowIUO;
  }
  if (isSILType)
    options |= TypeResolutionFlags::SILType;

  GenericTypeToArchetypeResolver contextResolver(GenericEnv);

  if (ProduceDiagnostics) {
    return TypeChecker(Ctx).validateType(T, DC, options, &contextResolver);
  } else {
    // Set up a diagnostics engine that swallows diagnostics.
    DiagnosticEngine Diags(Ctx.SourceMgr);
    return TypeChecker(Ctx, Diags).validateType(T, DC, options,
                                                &contextResolver);
  }
}

/// Expose TypeChecker's handling of GenericParamList to SIL parsing.
GenericEnvironment *
swift::handleSILGenericParams(ASTContext &Ctx, GenericParamList *genericParams,
                              DeclContext *DC) {
  return TypeChecker(Ctx).handleSILGenericParams(genericParams, DC);
}

bool swift::typeCheckCompletionDecl(Decl *D) {
  auto &Ctx = D->getASTContext();

  // Set up a diagnostics engine that swallows diagnostics.
  DiagnosticEngine Diags(Ctx.SourceMgr);
  TypeChecker TC(Ctx, Diags);

  if (auto ext = dyn_cast<ExtensionDecl>(D))
    TC.validateExtension(ext);
  else
    TC.typeCheckDecl(D);
  return true;
}

static Optional<Type> getTypeOfCompletionContextExpr(
                        TypeChecker &TC,
                        DeclContext *DC,
                        CompletionTypeCheckKind kind,
                        Expr *&parsedExpr,
                        ConcreteDeclRef &referencedDecl) {
  if (TC.preCheckExpression(parsedExpr, DC))
    return None;

  switch (kind) {
  case CompletionTypeCheckKind::Normal:
    // Handle below.
    break;

  case CompletionTypeCheckKind::KeyPath:
    referencedDecl = nullptr;
    if (auto keyPath = dyn_cast<KeyPathExpr>(parsedExpr))
      return TC.checkObjCKeyPathExpr(DC, keyPath, /*requireResultType=*/true);

    return None;
  }

  Type originalType = parsedExpr->getType();
  if (auto T = TC.getTypeOfExpressionWithoutApplying(parsedExpr, DC,
                 referencedDecl, FreeTypeVariableBinding::UnresolvedType))
    return T;

  // Try to recover if we've made any progress.
  if (parsedExpr &&
      !isa<ErrorExpr>(parsedExpr) &&
      parsedExpr->getType() &&
      !parsedExpr->getType()->hasError() &&
      (originalType.isNull() ||
       !parsedExpr->getType()->isEqual(originalType))) {
    return parsedExpr->getType();
  }

  return None;
}

/// \brief Return the type of an expression parsed during code completion, or
/// a null \c Type on error.
Optional<Type> swift::getTypeOfCompletionContextExpr(
                        ASTContext &Ctx,
                        DeclContext *DC,
                        CompletionTypeCheckKind kind,
                        Expr *&parsedExpr,
                        ConcreteDeclRef &referencedDecl) {

  if (Ctx.getLazyResolver()) {
    TypeChecker *TC = static_cast<TypeChecker *>(Ctx.getLazyResolver());
    return ::getTypeOfCompletionContextExpr(*TC, DC, kind, parsedExpr,
                                            referencedDecl);
  } else {
    // Set up a diagnostics engine that swallows diagnostics.
    DiagnosticEngine diags(Ctx.SourceMgr);
    TypeChecker TC(Ctx, diags);
    // Try to solve for the actual type of the expression.
    return ::getTypeOfCompletionContextExpr(TC, DC, kind, parsedExpr,
                                            referencedDecl);
  }
}

bool swift::typeCheckCompletionSequence(DeclContext *DC, Expr *&parsedExpr) {
  auto &ctx = DC->getASTContext();
  if (ctx.getLazyResolver()) {
    TypeChecker *TC = static_cast<TypeChecker *>(ctx.getLazyResolver());
    return TC->typeCheckCompletionSequence(parsedExpr, DC);
  } else {
    // Set up a diagnostics engine that swallows diagnostics.
    DiagnosticEngine diags(ctx.SourceMgr);
    TypeChecker TC(ctx, diags);
    return TC.typeCheckCompletionSequence(parsedExpr, DC);
  }
}

bool swift::typeCheckExpression(DeclContext *DC, Expr *&parsedExpr) {
  auto &ctx = DC->getASTContext();
  if (ctx.getLazyResolver()) {
    TypeChecker *TC = static_cast<TypeChecker *>(ctx.getLazyResolver());
    auto resultTy = TC->typeCheckExpression(parsedExpr, DC);
    return !resultTy;
  } else {
    // Set up a diagnostics engine that swallows diagnostics.
    DiagnosticEngine diags(ctx.SourceMgr);
    TypeChecker TC(ctx, diags);
    auto resultTy = TC.typeCheckExpression(parsedExpr, DC);
    return !resultTy;
  }
}

bool swift::typeCheckAbstractFunctionBodyUntil(AbstractFunctionDecl *AFD,
                                               SourceLoc EndTypeCheckLoc) {
  auto &Ctx = AFD->getASTContext();

  // Set up a diagnostics engine that swallows diagnostics.
  DiagnosticEngine Diags(Ctx.SourceMgr);

  TypeChecker TC(Ctx, Diags);
  return !TC.typeCheckAbstractFunctionBodyUntil(AFD, EndTypeCheckLoc);
}

bool swift::typeCheckTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
  auto &Ctx = static_cast<Decl *>(TLCD)->getASTContext();

  // Set up a diagnostics engine that swallows diagnostics.
  DiagnosticEngine Diags(Ctx.SourceMgr);

  TypeChecker TC(Ctx, Diags);
  TC.typeCheckTopLevelCodeDecl(TLCD);
  return true;
}

static void deleteTypeCheckerAndDiags(LazyResolver *resolver) {
  DiagnosticEngine &diags = static_cast<TypeChecker*>(resolver)->Diags;
  delete resolver;
  delete &diags;
}

OwnedResolver swift::createLazyResolver(ASTContext &Ctx) {
  auto diags = new DiagnosticEngine(Ctx.SourceMgr);
  return OwnedResolver(new TypeChecker(Ctx, *diags),
                       &deleteTypeCheckerAndDiags);
}

void TypeChecker::diagnoseAmbiguousMemberType(Type baseTy,
                                              SourceRange baseRange,
                                              Identifier name,
                                              SourceLoc nameLoc,
                                              LookupTypeResult &lookup) {
  if (auto moduleTy = baseTy->getAs<ModuleType>()) {
    diagnose(nameLoc, diag::ambiguous_module_type, name,
             moduleTy->getModule()->getName())
      .highlight(baseRange);
  } else {
    diagnose(nameLoc, diag::ambiguous_member_type, name, baseTy)
      .highlight(baseRange);
  }
  for (const auto &member : lookup) {
    diagnose(member.first, diag::found_candidate_type,
             member.second);
  }
}

// checkForForbiddenPrefix is for testing purposes.

void TypeChecker::checkForForbiddenPrefix(const Decl *D) {
  if (!hasEnabledForbiddenTypecheckPrefix())
    return;
  if (auto VD = dyn_cast<ValueDecl>(D)) {
    if (!VD->getBaseName().isSpecial())
      checkForForbiddenPrefix(VD->getBaseName().getIdentifier().str());
  }
}

void TypeChecker::checkForForbiddenPrefix(const UnresolvedDeclRefExpr *E) {
  if (!hasEnabledForbiddenTypecheckPrefix())
    return;
  if (!E->getName().isSpecial())
    checkForForbiddenPrefix(E->getName().getBaseIdentifier());
}

void TypeChecker::checkForForbiddenPrefix(Identifier Ident) {
  if (!hasEnabledForbiddenTypecheckPrefix())
    return;
  checkForForbiddenPrefix(Ident.empty() ? StringRef() : Ident.str());
}

void TypeChecker::checkForForbiddenPrefix(StringRef Name) {
  if (!hasEnabledForbiddenTypecheckPrefix())
    return;
  if (Name.empty())
    return;
  if (Name.startswith(Context.LangOpts.DebugForbidTypecheckPrefix)) {
    std::string Msg = "forbidden typecheck occurred: ";
    Msg += Name;
    llvm::report_fatal_error(Msg);
  }
}

DeclTypeCheckingSemantics
TypeChecker::getDeclTypeCheckingSemantics(ValueDecl *decl) {
  // Check for a @_semantics attribute.
  if (auto semantics = decl->getAttrs().getAttribute<SemanticsAttr>()) {
    if (semantics->Value.equals("typechecker.type(of:)"))
      return DeclTypeCheckingSemantics::TypeOf;
    if (semantics->Value.equals("typechecker.withoutActuallyEscaping(_:do:)"))
      return DeclTypeCheckingSemantics::WithoutActuallyEscaping;
    if (semantics->Value.equals("typechecker._openExistential(_:do:)"))
      return DeclTypeCheckingSemantics::OpenExistential;
  }
  return DeclTypeCheckingSemantics::Normal;
}
