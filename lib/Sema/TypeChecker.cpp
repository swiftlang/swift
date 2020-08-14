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
#include "ConstraintSystem.h"
#include "TypeChecker.h"
#include "TypeCheckObjC.h"
#include "TypeCheckType.h"
#include "CodeSynthesis.h"
#include "MiscDiagnostics.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/DiagnosticSuppression.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/Timer.h"
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

ProtocolDecl *TypeChecker::getProtocol(ASTContext &Context, SourceLoc loc,
                                       KnownProtocolKind kind) {
  auto protocol = Context.getProtocol(kind);
  if (!protocol && loc.isValid()) {
    Context.Diags.diagnose(loc, diag::missing_protocol,
                           Context.getIdentifier(getProtocolName(kind)));
  }

  if (protocol && protocol->isInvalid()) {
    return nullptr;
  }

  return protocol;
}

ProtocolDecl *TypeChecker::getLiteralProtocol(ASTContext &Context, Expr *expr) {
  if (isa<ArrayExpr>(expr))
    return TypeChecker::getProtocol(
        Context, expr->getLoc(), KnownProtocolKind::ExpressibleByArrayLiteral);

  if (isa<DictionaryExpr>(expr))
    return TypeChecker::getProtocol(
        Context, expr->getLoc(),
        KnownProtocolKind::ExpressibleByDictionaryLiteral);

  if (!isa<LiteralExpr>(expr))
    return nullptr;
  
  if (isa<NilLiteralExpr>(expr))
    return TypeChecker::getProtocol(Context, expr->getLoc(),
                                    KnownProtocolKind::ExpressibleByNilLiteral);

  if (isa<IntegerLiteralExpr>(expr))
    return TypeChecker::getProtocol(
        Context, expr->getLoc(),
        KnownProtocolKind::ExpressibleByIntegerLiteral);

  if (isa<FloatLiteralExpr>(expr))
    return TypeChecker::getProtocol(
        Context, expr->getLoc(), KnownProtocolKind::ExpressibleByFloatLiteral);

  if (isa<BooleanLiteralExpr>(expr))
    return TypeChecker::getProtocol(
        Context, expr->getLoc(),
        KnownProtocolKind::ExpressibleByBooleanLiteral);

  if (const auto *SLE = dyn_cast<StringLiteralExpr>(expr)) {
    if (SLE->isSingleUnicodeScalar())
      return TypeChecker::getProtocol(
          Context, expr->getLoc(),
          KnownProtocolKind::ExpressibleByUnicodeScalarLiteral);

    if (SLE->isSingleExtendedGraphemeCluster())
      return getProtocol(
          Context, expr->getLoc(),
          KnownProtocolKind::ExpressibleByExtendedGraphemeClusterLiteral);

    return TypeChecker::getProtocol(
        Context, expr->getLoc(), KnownProtocolKind::ExpressibleByStringLiteral);
  }

  if (isa<InterpolatedStringLiteralExpr>(expr))
    return TypeChecker::getProtocol(
        Context, expr->getLoc(),
        KnownProtocolKind::ExpressibleByStringInterpolation);

  if (auto E = dyn_cast<MagicIdentifierLiteralExpr>(expr)) {
    switch (E->getKind()) {
#define MAGIC_STRING_IDENTIFIER(NAME, STRING, SYNTAX_KIND) \
    case MagicIdentifierLiteralExpr::NAME: \
      return TypeChecker::getProtocol( \
          Context, expr->getLoc(), \
          KnownProtocolKind::ExpressibleByStringLiteral);

#define MAGIC_INT_IDENTIFIER(NAME, STRING, SYNTAX_KIND) \
    case MagicIdentifierLiteralExpr::NAME: \
      return TypeChecker::getProtocol( \
          Context, expr->getLoc(), \
          KnownProtocolKind::ExpressibleByIntegerLiteral);

#define MAGIC_POINTER_IDENTIFIER(NAME, STRING, SYNTAX_KIND) \
    case MagicIdentifierLiteralExpr::NAME: \
      return nullptr;

#include "swift/AST/MagicIdentifierKinds.def"
    }
  }

  if (auto E = dyn_cast<ObjectLiteralExpr>(expr)) {
    switch (E->getLiteralKind()) {
#define POUND_OBJECT_LITERAL(Name, Desc, Protocol)                             \
  case ObjectLiteralExpr::Name:                                                \
    return TypeChecker::getProtocol(Context, expr->getLoc(),                   \
                                    KnownProtocolKind::Protocol);
#include "swift/Syntax/TokenKinds.def"
    }
  }

  return nullptr;
}

DeclName TypeChecker::getObjectLiteralConstructorName(ASTContext &Context,
                                                      ObjectLiteralExpr *expr) {
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
  auto params = ctor->getMethodInterfaceType()
                    ->castTo<FunctionType>()->getParams();
  SmallVector<AnyFunctionType::Param, 8> newParams;
  newParams.append(params.begin(), params.end());

  auto replace = [&](StringRef replacement) -> Type {
    auto &Context = ctor->getASTContext();
    newParams[0] = AnyFunctionType::Param(newParams[0].getPlainType(),
                                          Context.getIdentifier(replacement),
                                          newParams[0].getParameterFlags());
    return AnyFunctionType::composeInput(Context, newParams,
                                         /*canonicalVararg=*/false);
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
  if (auto *stdlib = dc->getASTContext().getStdlibModule()) {
    return stdlib;
  }

  return dc->getParentModule();
}

/// Bind the given extension to the given nominal type.
static void bindExtensionToNominal(ExtensionDecl *ext,
                                   NominalTypeDecl *nominal) {
  if (ext->alreadyBoundToNominal())
    return;

  nominal->addExtension(ext);
}

static void bindExtensions(SourceFile &SF) {
  // Utility function to try and resolve the extended type without diagnosing.
  // If we succeed, we go ahead and bind the extension. Otherwise, return false.
  auto tryBindExtension = [&](ExtensionDecl *ext) -> bool {
    assert(!ext->canNeverBeBound() &&
           "Only extensions that can ever be bound get here.");
    if (auto nominal = ext->computeExtendedNominal()) {
      bindExtensionToNominal(ext, nominal);
      return true;
    }

    return false;
  };

  // Phase 1 - try to bind each extension, adding those whose type cannot be
  // resolved to a worklist.
  SmallVector<ExtensionDecl *, 8> worklist;

  // FIXME: The current source file needs to be handled specially, because of
  // private extensions.
  for (auto import : namelookup::getAllImports(&SF)) {
    // FIXME: Respect the access path?
    for (auto file : import.second->getFiles()) {
      auto SF = dyn_cast<SourceFile>(file);
      if (!SF)
        continue;

      for (auto D : SF->getTopLevelDecls()) {
        if (auto ED = dyn_cast<ExtensionDecl>(D))
          if (!tryBindExtension(ED))
            worklist.push_back(ED);
      }
    }
  }

  // Phase 2 - repeatedly go through the worklist and attempt to bind each
  // extension there, removing it from the worklist if we succeed.
  bool changed;
  do {
    changed = false;

    auto last = std::remove_if(worklist.begin(), worklist.end(),
                               tryBindExtension);
    if (last != worklist.end()) {
      worklist.erase(last, worklist.end());
      changed = true;
    }
  } while(changed);

  // Any remaining extensions are invalid. They will be diagnosed later by
  // typeCheckDecl().
}

static void typeCheckDelayedFunctions(SourceFile &SF) {
  unsigned currentFunctionIdx = 0;
  unsigned currentSynthesizedDecl = SF.LastCheckedSynthesizedDecl;
  do {
    // Type check the body of each of the function in turn.  Note that outside
    // functions must be visited before nested functions for type-checking to
    // work correctly.
    for (unsigned n = SF.DelayedFunctions.size(); currentFunctionIdx != n;
         ++currentFunctionIdx) {
      auto *AFD = SF.DelayedFunctions[currentFunctionIdx];
      assert(!AFD->getDeclContext()->isLocalContext());

      TypeChecker::typeCheckAbstractFunctionBody(AFD);
    }

    // Type check synthesized functions and their bodies.
    for (unsigned n = SF.SynthesizedDecls.size();
         currentSynthesizedDecl != n;
         ++currentSynthesizedDecl) {
      auto decl = SF.SynthesizedDecls[currentSynthesizedDecl];
      TypeChecker::typeCheckDecl(decl);
    }

  } while (currentFunctionIdx < SF.DelayedFunctions.size() ||
           currentSynthesizedDecl < SF.SynthesizedDecls.size());

  SF.DelayedFunctions.clear();
}

void swift::performTypeChecking(SourceFile &SF) {
  return (void)evaluateOrDefault(SF.getASTContext().evaluator,
                                 TypeCheckSourceFileRequest{&SF}, {});
}

evaluator::SideEffect
TypeCheckSourceFileRequest::evaluate(Evaluator &eval, SourceFile *SF) const {
  assert(SF && "Source file cannot be null!");
  assert(SF->ASTStage != SourceFile::TypeChecked &&
         "Should not be re-typechecking this file!");

  // Eagerly build the top-level scopes tree before type checking
  // because type-checking expressions mutates the AST and that throws off the
  // scope-based lookups. Only the top-level scopes because extensions have not
  // been bound yet.
  auto &Ctx = SF->getASTContext();
  if (Ctx.LangOpts.EnableASTScopeLookup && SF->isSuitableForASTScopes())
    SF->getScope()
        .buildEnoughOfTreeForTopLevelExpressionsButDontRequestGenericsOrExtendedNominals();

  BufferIndirectlyCausingDiagnosticRAII cpr(*SF);

  // Make sure that import resolution has been completed before doing any type
  // checking.
  performImportResolution(*SF);

  // Could build scope maps here because the AST is stable now.

  {
    FrontendStatsTracer tracer(Ctx.Stats,
                               "Type checking and Semantic analysis");

    if (Ctx.TypeCheckerOpts.SkipNonInlinableFunctionBodies)
      // Disable this optimization if we're compiling SwiftOnoneSupport, because
      // we _definitely_ need to look inside every declaration to figure out
      // what gets prespecialized.
      if (SF->getParentModule()->isOnoneSupportModule())
        Ctx.TypeCheckerOpts.SkipNonInlinableFunctionBodies = false;

    if (!Ctx.LangOpts.DisableAvailabilityChecking) {
      // Build the type refinement hierarchy for the primary
      // file before type checking.
      TypeChecker::buildTypeRefinementContextHierarchy(*SF);
    }

    // Resolve extensions. This has to occur first during type checking,
    // because the extensions need to be wired into the AST for name lookup
    // to work.
    ::bindExtensions(*SF);

    // Type check the top-level elements of the source file.
    for (auto D : SF->getTopLevelDecls()) {
      if (auto *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
        TypeChecker::typeCheckTopLevelCodeDecl(TLCD);
        TypeChecker::contextualizeTopLevelCode(TLCD);
      } else {
        TypeChecker::typeCheckDecl(D);
      }
    }

    // If we're in REPL mode, inject temporary result variables and other stuff
    // that the REPL needs to synthesize.
    if (SF->Kind == SourceFileKind::REPL && !Ctx.hadError())
      TypeChecker::processREPLTopLevel(*SF);

    typeCheckDelayedFunctions(*SF);
  }

  // Checking that benefits from having the whole module available.
  if (!Ctx.TypeCheckerOpts.DelayWholeModuleChecking) {
    performWholeModuleTypeChecking(*SF);
  }

  return std::make_tuple<>();
}

void swift::performWholeModuleTypeChecking(SourceFile &SF) {
  auto &Ctx = SF.getASTContext();
  FrontendStatsTracer tracer(Ctx.Stats,
                             "perform-whole-module-type-checking");
  diagnoseAttrsRequiringFoundation(SF);
  diagnoseObjCMethodConflicts(SF);
  diagnoseObjCUnsatisfiedOptReqConflicts(SF);
  diagnoseUnintendedObjCMethodOverrides(SF);

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

bool swift::isAdditiveArithmeticConformanceDerivationEnabled(SourceFile &SF) {
  auto &ctx = SF.getASTContext();
  // Return true if `AdditiveArithmetic` derived conformances are explicitly
  // enabled.
  if (ctx.LangOpts.EnableExperimentalAdditiveArithmeticDerivedConformances)
    return true;
  // Otherwise, return true iff differentiable programming is enabled.
  // Differentiable programming depends on `AdditiveArithmetic` derived
  // conformances.
  return isDifferentiableProgrammingEnabled(SF);
}

void swift::checkInconsistentImplementationOnlyImports(ModuleDecl *MainModule) {
  bool hasAnyImplementationOnlyImports =
      llvm::any_of(MainModule->getFiles(), [](const FileUnit *F) -> bool {
    auto *SF = dyn_cast<SourceFile>(F);
    return SF && SF->hasImplementationOnlyImports();
  });
  if (!hasAnyImplementationOnlyImports)
    return;

  auto diagnose = [MainModule](const ImportDecl *normalImport,
                               const ImportDecl *implementationOnlyImport) {
    auto &diags = MainModule->getDiags();
    {
      InFlightDiagnostic warning =
          diags.diagnose(normalImport, diag::warn_implementation_only_conflict,
                         normalImport->getModule()->getName());
      if (normalImport->getAttrs().isEmpty()) {
        // Only try to add a fix-it if there's no other annotations on the
        // import to avoid creating things like
        // `@_implementationOnly @_exported import Foo`. The developer can
        // resolve those manually.
        warning.fixItInsert(normalImport->getStartLoc(),
                            "@_implementationOnly ");
      }
    }
    diags.diagnose(implementationOnlyImport,
                   diag::implementation_only_conflict_here);
  };

  llvm::DenseMap<ModuleDecl *, std::vector<const ImportDecl *>> normalImports;
  llvm::DenseMap<ModuleDecl *, const ImportDecl *> implementationOnlyImports;

  for (const FileUnit *file : MainModule->getFiles()) {
    auto *SF = dyn_cast<SourceFile>(file);
    if (!SF)
      continue;

    for (auto *topLevelDecl : SF->getTopLevelDecls()) {
      auto *nextImport = dyn_cast<ImportDecl>(topLevelDecl);
      if (!nextImport)
        continue;

      ModuleDecl *module = nextImport->getModule();
      if (!module)
        continue;

      if (nextImport->getAttrs().hasAttribute<ImplementationOnlyAttr>()) {
        // We saw an implementation-only import.
        bool isNew =
            implementationOnlyImports.insert({module, nextImport}).second;
        if (!isNew)
          continue;

        auto seenNormalImportPosition = normalImports.find(module);
        if (seenNormalImportPosition != normalImports.end()) {
          for (auto *seenNormalImport : seenNormalImportPosition->getSecond())
            diagnose(seenNormalImport, nextImport);

          // We're done with these; keep the map small if possible.
          normalImports.erase(seenNormalImportPosition);
        }
        continue;
      }

      // We saw a non-implementation-only import. Is that in conflict with what
      // we've seen?
      if (auto *seenImplementationOnlyImport =
            implementationOnlyImports.lookup(module)) {
        diagnose(nextImport, seenImplementationOnlyImport);
        continue;
      }

      // Otherwise, record it for later.
      normalImports[module].push_back(nextImport);
    }
  }
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
  TypeResolutionOptions options = None;

  // Fine to have unbound generic types.
  options |= TypeResolutionFlags::AllowUnboundGenerics;
  if (isSILMode) {
    options |= TypeResolutionFlags::SILMode;
  }
  if (isSILType)
    options |= TypeResolutionFlags::SILType;

  auto resolution = TypeResolution::forContextual(DC, GenericEnv);
  Optional<DiagnosticSuppression> suppression;
  if (!ProduceDiagnostics)
    suppression.emplace(Ctx.Diags);
  return TypeChecker::validateType(Ctx, T, resolution, options);
}

/// Expose TypeChecker's handling of GenericParamList to SIL parsing.
GenericEnvironment *
swift::handleSILGenericParams(GenericParamList *genericParams,
                              DeclContext *DC) {
  if (genericParams == nullptr)
    return nullptr;

  SmallVector<GenericParamList *, 2> nestedList;
  for (; genericParams; genericParams = genericParams->getOuterParameters()) {
    nestedList.push_back(genericParams);
  }

  std::reverse(nestedList.begin(), nestedList.end());

  for (unsigned i = 0, e = nestedList.size(); i < e; ++i) {
    auto genericParams = nestedList[i];
    genericParams->setDepth(i);
  }

  auto sig =
      TypeChecker::checkGenericSignature(nestedList.back(), DC,
                                         /*parentSig=*/nullptr,
                                         /*allowConcreteGenericParams=*/true);
  return (sig ? sig->getGenericEnvironment() : nullptr);
}

void swift::typeCheckPatternBinding(PatternBindingDecl *PBD,
                                    unsigned bindingIndex) {
  assert(!PBD->isInitializerChecked(bindingIndex) &&
         PBD->getInit(bindingIndex));

  auto &Ctx = PBD->getASTContext();
  DiagnosticSuppression suppression(Ctx.Diags);
  (void)evaluateOrDefault(
      Ctx.evaluator, PatternBindingEntryRequest{PBD, bindingIndex}, nullptr);
  TypeChecker::typeCheckPatternBinding(PBD, bindingIndex);
}

static Optional<Type> getTypeOfCompletionContextExpr(
                        DeclContext *DC,
                        CompletionTypeCheckKind kind,
                        Expr *&parsedExpr,
                        ConcreteDeclRef &referencedDecl) {
  if (constraints::ConstraintSystem::preCheckExpression(
          parsedExpr, DC, /*replaceInvalidRefsWithErrors=*/true))
    return None;

  switch (kind) {
  case CompletionTypeCheckKind::Normal:
    // Handle below.
    break;

  case CompletionTypeCheckKind::KeyPath:
    referencedDecl = nullptr;
    if (auto keyPath = dyn_cast<KeyPathExpr>(parsedExpr))
      return TypeChecker::checkObjCKeyPathExpr(DC, keyPath,
                                               /*requireResultType=*/true);

    return None;
  }

  Type originalType = parsedExpr->getType();
  if (auto T = TypeChecker::getTypeOfExpressionWithoutApplying(parsedExpr, DC,
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

/// Return the type of an expression parsed during code completion, or
/// a null \c Type on error.
Optional<Type> swift::getTypeOfCompletionContextExpr(
                        ASTContext &Ctx,
                        DeclContext *DC,
                        CompletionTypeCheckKind kind,
                        Expr *&parsedExpr,
                        ConcreteDeclRef &referencedDecl) {
  DiagnosticSuppression suppression(Ctx.Diags);

  // Try to solve for the actual type of the expression.
  return ::getTypeOfCompletionContextExpr(DC, kind, parsedExpr,
                                          referencedDecl);
}

/// Return the type of operator function for specified LHS, or a null
/// \c Type on error.
FunctionType *
swift::getTypeOfCompletionOperator(DeclContext *DC, Expr *LHS,
                                   Identifier opName, DeclRefKind refKind,
                                   ConcreteDeclRef &referencedDecl) {
  auto &ctx = DC->getASTContext();
  DiagnosticSuppression suppression(ctx.Diags);
  return TypeChecker::getTypeOfCompletionOperator(DC, LHS, opName, refKind,
                                                  referencedDecl);
}

bool swift::typeCheckExpression(DeclContext *DC, Expr *&parsedExpr) {
  auto &ctx = DC->getASTContext();
  DiagnosticSuppression suppression(ctx.Diags);
  auto resultTy = TypeChecker::typeCheckExpression(parsedExpr, DC, TypeLoc(),
                                                   CTP_Unused);
  return !resultTy;
}

bool swift::typeCheckAbstractFunctionBodyUntil(AbstractFunctionDecl *AFD,
                                               SourceLoc EndTypeCheckLoc) {
  auto &Ctx = AFD->getASTContext();
  DiagnosticSuppression suppression(Ctx.Diags);
  return !TypeChecker::typeCheckAbstractFunctionBodyUntil(AFD, EndTypeCheckLoc);
}

bool swift::typeCheckTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
  auto &Ctx = static_cast<Decl *>(TLCD)->getASTContext();
  DiagnosticSuppression suppression(Ctx.Diags);
  TypeChecker::typeCheckTopLevelCodeDecl(TLCD);
  return true;
}

void TypeChecker::checkForForbiddenPrefix(ASTContext &C, DeclBaseName Name) {
  if (C.TypeCheckerOpts.DebugForbidTypecheckPrefix.empty())
    return;

  // Don't touch special names or empty names.
  if (Name.isSpecial() || Name.empty())
    return;

  StringRef Str = Name.getIdentifier().str();
  if (Str.startswith(C.TypeCheckerOpts.DebugForbidTypecheckPrefix)) {
    std::string Msg = "forbidden typecheck occurred: ";
    Msg += Str;
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

void swift::bindExtensions(SourceFile &SF) {
  ::bindExtensions(SF);
}

LookupResult
swift::lookupSemanticMember(DeclContext *DC, Type ty, DeclName name) {
  return TypeChecker::lookupMember(DC, ty, DeclNameRef(name), None);
}
