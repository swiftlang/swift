//===--- TypeChecker.cpp - Type Checking ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the swift::performTypeChecking entry point for
// semantic analysis.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/TypeRefinementContext.h"
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

void TypeChecker::handleExternalDecl(Decl *decl) {
  if (auto SD = dyn_cast<StructDecl>(decl)) {
    addImplicitConstructors(SD);
    addImplicitStructConformances(SD);
  }
  if (auto CD = dyn_cast<ClassDecl>(decl)) {
    addImplicitDestructor(CD);
  }
  if (auto ED = dyn_cast<EnumDecl>(decl)) {
    addImplicitEnumConformances(ED);
  }
}

ProtocolDecl *TypeChecker::getProtocol(SourceLoc loc, KnownProtocolKind kind) {
  auto protocol = Context.getProtocol(kind);
  if (!protocol && loc.isValid()) {
    diagnose(loc, diag::missing_protocol,
             Context.getIdentifier(getProtocolName(kind)));
  }

  if (protocol && !protocol->hasType()) {
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
#include "swift/Parse/Tokens.def"
    }
  }

  return nullptr;
}

DeclName TypeChecker::getObjectLiteralConstructorName(ObjectLiteralExpr *expr) {
  switch (expr->getLiteralKind()) {
  case ObjectLiteralExpr::colorLiteral: {
    return DeclName(Context, Context.Id_init,
                    { Context.getIdentifier("colorLiteralRed"),
                      Context.getIdentifier("green"),
                      Context.getIdentifier("blue"),
                      Context.getIdentifier("alpha") });
  }
  case ObjectLiteralExpr::imageLiteral: {
    return DeclName(Context, Context.Id_init,
                    { Context.getIdentifier("imageLiteralResourceName") });
  }
  case ObjectLiteralExpr::fileLiteral: {
    return DeclName(Context, Context.Id_init,
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
  Type argType = ctor->getArgumentType();
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

Module *TypeChecker::getStdlibModule(const DeclContext *dc) {
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

      auto tyDecl = dyn_cast<TypeDecl>(results.front());
      if (!tyDecl) {
        diagnose(SourceLoc(), diag::broken_bool);
        return Type();
      }

      return tyDecl->getDeclaredType();
    })();
  }
  return *boolType;
}

namespace swift {

/// Clone the given generic parameters in the given list. We don't need any
/// of the requirements, because they will be inferred.
GenericParamList *cloneGenericParams(ASTContext &ctx,
                                     DeclContext *dc,
                                     GenericParamList *fromParams,
                                     GenericParamList *outerParams) {
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
  toParams->setOuterParameters(outerParams);
  return toParams;
}
}

// FIXME: total hack
GenericParamList *createProtocolGenericParams(ASTContext &ctx,
                                              ProtocolDecl *proto,
                                              DeclContext *dc);

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
  options |= TR_AllowUnboundGenerics;
  options |= TR_ExtensionBinding;
  if (TC.validateType(ED->getExtendedTypeLoc(), dc, options)) {
    ED->setInvalid();
    return;
  }

  // Dig out the extended type.
  auto extendedType = ED->getExtendedType();

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
  if (extendedType->isSpecialized() && extendedType->getAnyNominal()) {
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
  if (extendedNominal->getGenericParams()) {
    if (auto proto = dyn_cast<ProtocolDecl>(extendedNominal)) {
      // For a protocol extension, build the generic parameter list.
      ED->setGenericParams(proto->createGenericParams(ED));
    } else {
      // Clone the existing generic parameter list.
      ED->setGenericParams(
        cloneGenericParams(TC.Context, ED,
                           extendedNominal->getGenericParams(),
                           nullptr));
    }
  }

  // If we have a trailing where clause, deal with it now.
  // For now, trailing where clauses are only permitted on protocol extensions.
  if (auto trailingWhereClause = ED->getTrailingWhereClause()) {
    if (!extendedNominal->getGenericParams()) {
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

static void typeCheckFunctionsAndExternalDecls(TypeChecker &TC) {
  unsigned currentFunctionIdx = 0;
  unsigned currentExternalDef = TC.Context.LastCheckedExternalDefinition;
  do {
    // Type check the body of each of the function in turn.  Note that outside
    // functions must be visited before nested functions for type-checking to
    // work correctly.
    for (unsigned n = TC.definedFunctions.size(); currentFunctionIdx != n;
         ++currentFunctionIdx) {
      auto *AFD = TC.definedFunctions[currentFunctionIdx];

      // HACK: don't type-check the same function body twice.  This is
      // supposed to be handled by just not enqueuing things twice,
      // but that gets tricky with synthesized function bodies.
      if (AFD->isBodyTypeChecked()) continue;

      PrettyStackTraceDecl StackEntry("type-checking", AFD);
      TC.typeCheckAbstractFunctionBody(AFD);

      AFD->setBodyTypeCheckedIfPresent();
    }

    for (unsigned n = TC.Context.ExternalDefinitions.size();
         currentExternalDef != n;
         ++currentExternalDef) {
      auto decl = TC.Context.ExternalDefinitions[currentExternalDef];
      
      if (auto *AFD = dyn_cast<AbstractFunctionDecl>(decl)) {
        // HACK: don't type-check the same function body twice.  This is
        // supposed to be handled by just not enqueuing things twice,
        // but that gets tricky with synthesized function bodies.
        if (AFD->isBodyTypeChecked()) continue;

        PrettyStackTraceDecl StackEntry("type-checking", AFD);
        TC.typeCheckAbstractFunctionBody(AFD);
        continue;
      }
      if (isa<NominalTypeDecl>(decl)) {
        TC.handleExternalDecl(decl);
        continue;
      }
      if (isa<VarDecl>(decl))
        continue;
      llvm_unreachable("Unhandled external definition kind");
    }

    // Validate the contents of any referenced nominal types for SIL's purposes.
    // Note: if we ever start putting extension members in vtables, we'll need
    // to validate those members too.
    // FIXME: If we're not planning to run SILGen, this is wasted effort.
    while (!TC.ValidatedTypes.empty()) {
      auto nominal = TC.ValidatedTypes.pop_back_val();
      if (nominal->isInvalid() || TC.Context.hadError())
        continue;

      Optional<bool> lazyVarsAlreadyHaveImplementation;

      for (auto *D : nominal->getMembers()) {
        auto VD = dyn_cast<ValueDecl>(D);
        if (!VD)
          continue;
        TC.validateDecl(VD);

        // The only thing left to do is synthesize storage for lazy variables.
        // We only have to do that if it's a type from another file, though.
        // In NDEBUG builds, bail out as soon as we can.
#ifdef NDEBUG
        if (lazyVarsAlreadyHaveImplementation.hasValue() &&
            lazyVarsAlreadyHaveImplementation.getValue())
          continue;
#endif
        auto *prop = dyn_cast<VarDecl>(D);
        if (!prop)
          continue;

        if (prop->getAttrs().hasAttribute<LazyAttr>() && !prop->isStatic()
                                                      && prop->getGetter()) {
          bool hasImplementation = prop->getGetter()->hasBody();

          if (lazyVarsAlreadyHaveImplementation.hasValue()) {
            assert(lazyVarsAlreadyHaveImplementation.getValue() ==
                     hasImplementation &&
                   "only some lazy vars already have implementations");
          } else {
            lazyVarsAlreadyHaveImplementation = hasImplementation;
          }

          if (!hasImplementation)
            TC.completeLazyVarImplementation(prop);
        }
      }

      // FIXME: We need to add implicit initializers and dtors when a decl is
      // touched, because it affects vtable layout.  If you're not defining the
      // class, you shouldn't have to know what the vtable layout is.
      if (auto *CD = dyn_cast<ClassDecl>(nominal)) {
        TC.addImplicitConstructors(CD);
        TC.addImplicitDestructor(CD);
      }
    }

    // Complete any conformances that we used.
    for (unsigned i = 0; i != TC.UsedConformances.size(); ++i) {
      auto conformance = TC.UsedConformances[i];
      if (conformance->isIncomplete())
        TC.checkConformance(conformance);
    }
    TC.UsedConformances.clear();

  } while (currentFunctionIdx < TC.definedFunctions.size() ||
           currentExternalDef < TC.Context.ExternalDefinitions.size() ||
           !TC.UsedConformances.empty());

  // FIXME: Horrible hack. Store this somewhere more appropriate.
  TC.Context.LastCheckedExternalDefinition = currentExternalDef;

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
  for (auto decl : TC.Context.ExternalDefinitions) {
    if (auto fn = dyn_cast<AbstractFunctionDecl>(decl)) {
      TC.checkFunctionErrorHandling(fn);
    }
  }
}

void swift::typeCheckExternalDefinitions(SourceFile &SF) {
  assert(SF.ASTStage == SourceFile::TypeChecked);
  auto &Ctx = SF.getASTContext();
  TypeChecker TC(Ctx);
  typeCheckFunctionsAndExternalDecls(TC);
}

void swift::performTypeChecking(SourceFile &SF, TopLevelContext &TLC,
                                OptionSet<TypeCheckingFlags> Options,
                                unsigned StartElem,
                                unsigned WarnLongFunctionBodies) {
  if (SF.ASTStage == SourceFile::TypeChecked)
    return;

  // Make sure that name binding has been completed before doing any type
  // checking.
  {
    SharedTimer timer("Name binding");
    performNameBinding(SF, StartElem);
  }

  auto &Ctx = SF.getASTContext();
  {
    // NOTE: The type checker is scoped to be torn down before AST
    // verification.
    TypeChecker TC(Ctx);
    SharedTimer timer("Type checking / Semantic analysis");

    TC.setWarnLongFunctionBodies(WarnLongFunctionBodies);
    if (Options.contains(TypeCheckingFlags::DebugTimeFunctionBodies))
      TC.enableDebugTimeFunctionBodies();

    if (Options.contains(TypeCheckingFlags::ForImmediateMode))
      TC.setInImmediateMode(true);
    
    // Lookup the swift module.  This ensures that we record all known
    // protocols in the AST.
    (void) TC.getStdlibModule(&SF);

    if (!Ctx.LangOpts.DisableAvailabilityChecking) {
      // Build the type refinement hierarchy for the primary
      // file before type checking.
      TC.buildTypeRefinementContextHierarchy(SF, StartElem);
    }

    // Resolve extensions. This has to occur first during type checking,
    // because the extensions need to be wired into the AST for name lookup
    // to work.
    // FIXME: We can have interesting ordering dependencies among the various
    // extensions, so we'll need to be smarter here.
    // FIXME: The current source file needs to be handled specially, because of
    // private extensions.
    SF.forAllVisibleModules([&](Module::ImportedModule import) {
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
    });

    // FIXME: Check for cycles in class inheritance here?
    
    // Type check the top-level elements of the source file.
    for (auto D : llvm::makeArrayRef(SF.Decls).slice(StartElem)) {
      if (isa<TopLevelCodeDecl>(D))
        continue;

      TC.typeCheckDecl(D, /*isFirstPass*/true);
    }

    // At this point, we can perform general name lookup into any type.

    // We don't know the types of all the global declarations in the first
    // pass, which means we can't completely analyze everything. Perform the
    // second pass now.

    bool hasTopLevelCode = false;
    for (auto D : llvm::makeArrayRef(SF.Decls).slice(StartElem)) {
      if (TopLevelCodeDecl *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
        hasTopLevelCode = true;
        // Immediately perform global name-binding etc.
        TC.typeCheckTopLevelCodeDecl(TLCD);
      } else {
        TC.typeCheckDecl(D, /*isFirstPass*/false);
      }
    }

    if (hasTopLevelCode) {
      TC.contextualizeTopLevelCode(TLC,
                             llvm::makeArrayRef(SF.Decls).slice(StartElem));
    }

    // If we're in REPL mode, inject temporary result variables and other stuff
    // that the REPL needs to synthesize.
    if (SF.Kind == SourceFileKind::REPL && !TC.Context.hadError())
      TC.processREPLTopLevel(SF, TLC, StartElem);

    typeCheckFunctionsAndExternalDecls(TC);
  }

  // Checking that benefits from having the whole module available.
  if (!(Options & TypeCheckingFlags::DelayWholeModuleChecking)) {
    performWholeModuleTypeChecking(SF);
  }

  // Verify that we've checked types correctly.
  SF.ASTStage = SourceFile::TypeChecked;

  {
    SharedTimer timer("AST verification");
    // Verify the SourceFile.
    verify(SF);

    // Verify imported modules.
#ifndef NDEBUG
    if (SF.Kind != SourceFileKind::REPL &&
        !Ctx.LangOpts.DebuggerSupport) {
      Ctx.verifyAllLoadedModules();
    }
#endif
  }
}

void swift::finishTypeChecking(SourceFile &SF) {
  auto &Ctx = SF.getASTContext();
  TypeChecker TC(Ctx);

  for (auto D : SF.Decls)
    if (auto PD = dyn_cast<ProtocolDecl>(D))
      TC.inferDefaultWitnesses(PD);
}

void swift::performWholeModuleTypeChecking(SourceFile &SF) {
  auto &Ctx = SF.getASTContext();
  Ctx.diagnoseAttrsRequiringFoundation(SF);
  Ctx.diagnoseObjCMethodConflicts(SF);
  Ctx.diagnoseObjCUnsatisfiedOptReqConflicts(SF);
  Ctx.diagnoseUnintendedObjCMethodOverrides(SF);
}

bool swift::performTypeLocChecking(ASTContext &Ctx, TypeLoc &T,
                                   DeclContext *DC,
                                   bool ProduceDiagnostics) {
  return performTypeLocChecking(Ctx, T,
                                /*isSILMode=*/false,
                                /*isSILType=*/false,
                                DC, ProduceDiagnostics);
}

bool swift::performTypeLocChecking(ASTContext &Ctx, TypeLoc &T,
                                   bool isSILMode,
                                   bool isSILType,
                                   DeclContext *DC,
                                   bool ProduceDiagnostics) {
  TypeResolutionOptions options;

  // Fine to have unbound generic types.
  options |= TR_AllowUnboundGenerics;
  if (isSILMode)
    options |= TR_SILMode;
  if (isSILType)
    options |= TR_SILType;

  if (ProduceDiagnostics) {
    return TypeChecker(Ctx).validateType(T, DC, options);
  } else {
    // Set up a diagnostics engine that swallows diagnostics.
    DiagnosticEngine Diags(Ctx.SourceMgr);
    return TypeChecker(Ctx, Diags).validateType(T, DC, options);
  }
}

/// Expose TypeChecker's handling of GenericParamList to SIL parsing.
GenericSignature *swift::handleSILGenericParams(ASTContext &Ctx,
                                                GenericParamList *genericParams,
                                                DeclContext *DC) {
  return TypeChecker(Ctx).handleSILGenericParams(genericParams, DC);
}

bool swift::typeCheckCompletionDecl(Decl *D) {
  auto &Ctx = D->getASTContext();

  // Set up a diagnostics engine that swallows diagnostics.
  DiagnosticEngine Diags(Ctx.SourceMgr);
  TypeChecker TC(Ctx, Diags);

  TC.typeCheckDecl(D, true);
  return true;
}

bool swift::isConvertibleTo(Type Ty1, Type Ty2, DeclContext &DC) {
  auto &Ctx = DC.getASTContext();

  // We try to reuse the type checker associated with the ast context first.
  if (Ctx.getLazyResolver()) {
    TypeChecker *TC = static_cast<TypeChecker*>(Ctx.getLazyResolver());
    return TC->isConvertibleTo(Ty1, Ty2, &DC);
  } else {
    DiagnosticEngine Diags(Ctx.SourceMgr);
    return (new TypeChecker(Ctx, Diags))->isConvertibleTo(Ty1, Ty2, &DC);
  }
}

Type swift::lookUpTypeInContext(DeclContext *DC, StringRef Name) {
  auto &Ctx = DC->getASTContext();
  auto ReturnResult = [](UnqualifiedLookup &Lookup) {
    if (auto Result = Lookup.getSingleTypeResult())
      return Result->getDeclaredType();
    return Type();
  };
  if (Ctx.getLazyResolver()) {
    UnqualifiedLookup Lookup(DeclName(Ctx.getIdentifier(Name)), DC,
                             Ctx.getLazyResolver(), false, SourceLoc(), true);
    return ReturnResult(Lookup);
  } else {
    DiagnosticEngine Diags(Ctx.SourceMgr);
    LazyResolver *Resolver = new TypeChecker(Ctx, Diags);
    UnqualifiedLookup Lookup(DeclName(Ctx.getIdentifier(Name)), DC,
                             Resolver, false, SourceLoc(), true);
    return ReturnResult(Lookup);
  }
}

static Optional<Type> getTypeOfCompletionContextExpr(
                        TypeChecker &TC,
                        DeclContext *DC,
                        CompletionTypeCheckKind kind,
                        Expr *&parsedExpr,
                        ConcreteDeclRef &referencedDecl) {
  switch (kind) {
  case CompletionTypeCheckKind::Normal:
    // Handle below.
    break;

  case CompletionTypeCheckKind::ObjCKeyPath:
    referencedDecl = nullptr;
    if (auto keyPath = dyn_cast<ObjCKeyPathExpr>(parsedExpr))
      return TC.checkObjCKeyPathExpr(DC, keyPath, /*requireResulType=*/true);

    return None;
  }

  CanType originalType = parsedExpr->getType().getCanonicalTypeOrNull();
  if (auto T = TC.getTypeOfExpressionWithoutApplying(parsedExpr, DC,
                 referencedDecl, FreeTypeVariableBinding::GenericParameters))
    return T;

  // Try to recover if we've made any progress.
  if (parsedExpr && !isa<ErrorExpr>(parsedExpr) && parsedExpr->getType() &&
      !parsedExpr->getType()->is<ErrorType>() &&
      parsedExpr->getType().getCanonicalTypeOrNull() != originalType) {
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
    return TC->typeCheckExpression(parsedExpr, DC);
  } else {
    // Set up a diagnostics engine that swallows diagnostics.
    DiagnosticEngine diags(ctx.SourceMgr);
    TypeChecker TC(ctx, diags);
    return TC.typeCheckExpression(parsedExpr, DC);
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

/// Returns the first availability attribute on the declaration that is active
/// on the target platform.
static const AvailableAttr *getActiveAvailableAttribute(const Decl *D,
                                                              ASTContext &AC) {
  for (auto Attr : D->getAttrs())
    if (auto AvAttr = dyn_cast<AvailableAttr>(Attr)) {
      if (!AvAttr->isInvalid() && AvAttr->isActivePlatform(AC)) {
        return AvAttr;
      }
    }
  return nullptr;
}

/// Returns true if there is any availability attribute on the declaration
/// that is active on the target platform.
static bool hasActiveAvailableAttribute(Decl *D,
                                           ASTContext &AC) {
  return getActiveAvailableAttribute(D, AC);
}

namespace {

/// A class to walk the AST to build the type refinement context hierarchy.
class TypeRefinementContextBuilder : private ASTWalker {

  struct ContextInfo {
    TypeRefinementContext *TRC;

    /// The node whose end marks the end of the refinement context.
    /// If the builder sees this node in a post-visitor, it will pop
    /// the context from the stack. This node can be null (ParentTy()),
    /// indicating that custom logic elsewhere will handle removing
    /// the context when needed.
    ParentTy ScopeNode;
  };

  std::vector<ContextInfo> ContextStack;
  TypeChecker &TC;

  /// A mapping from abstract storage declarations with accessors to
  /// to the type refinement contexts for those declarations. We refer to
  /// this map to determine the appropriate parent TRC to use when
  /// walking the accessor function.
  llvm::DenseMap<AbstractStorageDecl *, TypeRefinementContext *>
      StorageContexts;

  TypeRefinementContext *getCurrentTRC() {
    return ContextStack.back().TRC;
  }

  void pushContext(TypeRefinementContext *TRC, ParentTy PopAfterNode) {
    ContextInfo Info;
    Info.TRC = TRC;
    Info.ScopeNode = PopAfterNode;
    ContextStack.push_back(Info);
  }

public:
  TypeRefinementContextBuilder(TypeRefinementContext *TRC, TypeChecker &TC)
      : TC(TC) {
    assert(TRC);
    pushContext(TRC, ParentTy());
  }

  void build(Decl *D) {
    unsigned StackHeight = ContextStack.size();
    D->walk(*this);
    assert(ContextStack.size() == StackHeight);
    (void)StackHeight;
  }

  void build(Stmt *S) {
    unsigned StackHeight = ContextStack.size();
    S->walk(*this);
    assert(ContextStack.size() == StackHeight);
    (void)StackHeight;
  }

  void build(Expr *E) {
    unsigned StackHeight = ContextStack.size();
    E->walk(*this);
    assert(ContextStack.size() == StackHeight);
    (void)StackHeight;
  }

private:
  virtual bool walkToDeclPre(Decl *D) override {
    TypeRefinementContext *DeclTRC = getNewContextForWalkOfDecl(D);

    if (DeclTRC) {
      pushContext(DeclTRC, D);
    }

    return true;
  }

  virtual bool walkToDeclPost(Decl *D) override {
    if (ContextStack.back().ScopeNode.getAsDecl() == D) {
      ContextStack.pop_back();
    }
    return true;
  }

  /// Returns a new context to be introduced for the declaration, or nullptr
  /// if no new context should be introduced.
  TypeRefinementContext *getNewContextForWalkOfDecl(Decl *D) {
    if (auto FD = dyn_cast<FuncDecl>(D)) {
      if (FD->isAccessor()) {
        // Use TRC of the storage rather the current TRC when walking this
        // function.
        auto it = StorageContexts.find(FD->getAccessorStorageDecl());
        if (it != StorageContexts.end()) {
          return it->second;
        }
      }
    }
    
    if (declarationIntroducesNewContext(D)) {
      return buildDeclarationRefinementContext(D);
    }
    
    return nullptr;
  }

  /// Builds the type refinement hierarchy for the body of the function.
  TypeRefinementContext *buildDeclarationRefinementContext(Decl *D) {
    // We require a valid range in order to be able to query for the TRC
    // corresponding to a given SourceLoc.
    // If this assert fires, it means we have probably synthesized an implicit
    // declaration without location information. The appropriate fix is
    // probably to gin up a source range for the declaration when synthesizing
    // it.
    assert(D->getSourceRange().isValid());
    
    // The potential versions in the declaration are constrained by both
    // the declared availability of the declaration and the potential versions
    // of its lexical context.
    AvailabilityContext DeclInfo =
        swift::AvailabilityInference::availableRange(D, TC.Context);
    DeclInfo.intersectWith(getCurrentTRC()->getAvailabilityInfo());

    TypeRefinementContext *NewTRC =
        TypeRefinementContext::createForDecl(TC.Context, D, getCurrentTRC(),
                                             DeclInfo,
                                             refinementSourceRangeForDecl(D));
    
    // Record the TRC for this storage declaration so that
    // when we process the accessor, we can use this TRC as the
    // parent.
    if (auto *StorageDecl = dyn_cast<AbstractStorageDecl>(D)) {
      if (StorageDecl->hasAccessorFunctions()) {
        StorageContexts[StorageDecl] = NewTRC;
      }
    }
    
    return NewTRC;
  }
  
  /// Returns true if the declaration should introduce a new refinement context.
  bool declarationIntroducesNewContext(Decl *D) {
    if (!isa<ValueDecl>(D) && !isa<ExtensionDecl>(D)) {
      return false;
    }
    
    // No need to introduce a context if the declaration does not have an
    // availability attribute.
    if (!hasActiveAvailableAttribute(D, TC.Context)) {
      return false;
    }
    
    // Only introduce for an AbstractStorageDecl if it is not local.
    // We introduce for the non-local case because these may
    // have getters and setters (and these may be synthesized, so they might
    // not even exist yet).
    if (auto *storageDecl = dyn_cast<AbstractStorageDecl>(D)) {
      if (storageDecl->getDeclContext()->isLocalContext()) {
        // No need to
        return false;
      }
    }
    
    return true;
  }

  /// Returns the source range which should be refined by declaration. This
  /// provides a convenient place to specify the refined range when it is
  /// different than the declaration's source range.
  SourceRange refinementSourceRangeForDecl(Decl *D) {
    if (auto *storageDecl = dyn_cast<AbstractStorageDecl>(D)) {
      // Use the declaration's availability for the context when checking
      // the bodies of its accessors.

      // HACK: For synthesized trivial accessors we may have not a valid
      // location for the end of the braces, so in that case we will fall back
      // to using the range for the storage declaration. The right fix here is
      // to update AbstractStorageDecl::addTrivialAccessors() to take brace
      // locations and have callers of that method provide appropriate source
      // locations.
      SourceLoc BracesEnd = storageDecl->getBracesRange().End;
      if (storageDecl->hasAccessorFunctions() && BracesEnd.isValid()) {
        return SourceRange(storageDecl->getStartLoc(),
                           BracesEnd);
      }
      
      // For a variable declaration (without accessors) we use the range of the
      // containing pattern binding declaration to make sure that we include
      // any type annotation in the type refinement context range.
      if (auto varDecl = dyn_cast<VarDecl>(storageDecl)) {
        auto *PBD = varDecl->getParentPatternBinding();
        if (PBD)
          return PBD->getSourceRange();
      }
    }
    
    return D->getSourceRange();
  }

  virtual std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    if (auto *IS = dyn_cast<IfStmt>(S)) {
      buildIfStmtRefinementContext(IS);
      return std::make_pair(false, S);
    }

    if (auto *RS = dyn_cast<GuardStmt>(S)) {
      buildGuardStmtRefinementContext(RS);
      return std::make_pair(false, S);
    }

    if (auto *WS = dyn_cast<WhileStmt>(S)) {
      buildWhileStmtRefinementContext(WS);
      return std::make_pair(false, S);
    }

    return std::make_pair(true, S);
  }

  virtual Stmt *walkToStmtPost(Stmt *S) override {
    // If we have multiple guard statements in the same block
    // then we may have multiple refinement contexts to pop
    // after walking that block.
    while (!ContextStack.empty() &&
           ContextStack.back().ScopeNode.getAsStmt() == S) {
      ContextStack.pop_back();
    }

    return S;
  }

  /// Builds the type refinement hierarchy for the IfStmt if the guard
  /// introduces a new refinement context for the Then branch.
  /// There is no need for the caller to explicitly traverse the children
  /// of this node.
  void buildIfStmtRefinementContext(IfStmt *IS) {
    Optional<AvailabilityContext> ThenRange;
    Optional<AvailabilityContext> ElseRange;
    std::tie(ThenRange, ElseRange) =
        buildStmtConditionRefinementContext(IS->getCond());

    if (ThenRange.hasValue()) {
      // Create a new context for the Then branch and traverse it in that new
      // context.
      auto *ThenTRC =
          TypeRefinementContext::createForIfStmtThen(TC.Context, IS,
                                                     getCurrentTRC(),
                                                     ThenRange.getValue());
      TypeRefinementContextBuilder(ThenTRC, TC).build(IS->getThenStmt());
    } else {
      build(IS->getThenStmt());
    }

    Stmt *ElseStmt = IS->getElseStmt();
    if (!ElseStmt)
      return;

    // Refine the else branch if we're given a version range for that branch.
    // For now, if present, this will only be the empty range, indicating
    // that the branch is dead. We use it to suppress potential unavailability
    // and deprecation diagnostics on code that definitely will not run with
    // the current platform and minimum deployment target.
    // If we add a more precise version range lattice (i.e., one that can
    // support "<") we should create non-empty contexts for the Else branch.
    if (ElseRange.hasValue()) {
      // Create a new context for the Then branch and traverse it in that new
      // context.
      auto *ElseTRC =
          TypeRefinementContext::createForIfStmtElse(TC.Context, IS,
                                                     getCurrentTRC(),
                                                     ElseRange.getValue());
      TypeRefinementContextBuilder(ElseTRC, TC).build(ElseStmt);
    } else {
      build(IS->getElseStmt());
    }
  }

  /// Builds the type refinement hierarchy for the WhileStmt if the guard
  /// introduces a new refinement context for the body branch.
  /// There is no need for the caller to explicitly traverse the children
  /// of this node.
  void buildWhileStmtRefinementContext(WhileStmt *WS) {
    Optional<AvailabilityContext> BodyRange =
        buildStmtConditionRefinementContext(WS->getCond()).first;

    if (BodyRange.hasValue()) {
      // Create a new context for the body and traverse it in the new
      // context.
      auto *BodyTRC = TypeRefinementContext::createForWhileStmtBody(
          TC.Context, WS, getCurrentTRC(), BodyRange.getValue());
      TypeRefinementContextBuilder(BodyTRC, TC).build(WS->getBody());
    } else {
      build(WS->getBody());
    }
  }

  /// Builds the type refinement hierarchy for the GuardStmt and pushes
  /// the fallthrough context onto the context stack so that subsequent
  /// AST elements in the same scope are analyzed in the context of the
  /// fallthrough TRC.
  void buildGuardStmtRefinementContext(GuardStmt *GS) {
    // 'guard' statements fall through if all of the
    // guard conditions are true, so we refine the range after the require
    // until the end of the enclosing block.
    // if ... {
    //   guard available(...) else { return } <-- Refined range starts here
    //   ...
    // } <-- Refined range ends here
    //
    // This is slightly tricky because, unlike our other control constructs,
    // the refined region is not lexically contained inside the construct
    // introducing the refinement context.
    Optional<AvailabilityContext> FallthroughRange;
    Optional<AvailabilityContext> ElseRange;
    std::tie(FallthroughRange, ElseRange) =
        buildStmtConditionRefinementContext(GS->getCond());

    if (Stmt *ElseBody = GS->getBody()) {
      if (ElseRange.hasValue()) {
        auto *TrueTRC = TypeRefinementContext::createForGuardStmtElse(
            TC.Context, GS, getCurrentTRC(), ElseRange.getValue());

        TypeRefinementContextBuilder(TrueTRC, TC).build(ElseBody);
      } else {
        build(ElseBody);
      }
    }

    BraceStmt *ParentBrace = dyn_cast<BraceStmt>(Parent.getAsStmt());
    assert(ParentBrace && "Expected parent of GuardStmt to be BraceStmt");
    if (!FallthroughRange.hasValue())
      return;

    // Create a new context for the fallthrough.

    auto *FallthroughTRC =
          TypeRefinementContext::createForGuardStmtFallthrough(TC.Context, GS,
              ParentBrace, getCurrentTRC(), FallthroughRange.getValue());

    pushContext(FallthroughTRC, ParentBrace);
  }

  /// Build the type refinement context for a StmtCondition and return a pair
  /// of optional version ranges, the first for the true branch and the second
  /// for the false branch. A value of None for a given branch indicates that
  /// the branch does not introduce a new refinement.
  std::pair<Optional<AvailabilityContext>, Optional<AvailabilityContext>>
  buildStmtConditionRefinementContext(StmtCondition Cond) {

    // Any refinement contexts introduced in the statement condition
    // will end at the end of the last condition element.
    StmtConditionElement LastElement = Cond.back();
    
    // Keep track of how many nested refinement contexts we have pushed on
    // the context stack so we can pop them when we're done building the
    // context for the StmtCondition.
    unsigned NestedCount = 0;

    // Tracks the potential version range when the condition is false.
    auto FalseFlow = AvailabilityContext::neverAvailable();

    TypeRefinementContext *StartingTRC = getCurrentTRC();

    for (StmtConditionElement Element : Cond) {
      TypeRefinementContext *CurrentTRC = getCurrentTRC();
      AvailabilityContext CurrentInfo = CurrentTRC->getAvailabilityInfo();

      // If the element is not a condition, walk it in the current TRC.
      if (Element.getKind() != StmtConditionElement::CK_Availability) {

        // Assume any condition element that is not a #available() can
        // potentially be false, so conservatively combine the version
        // range of the current context with the accumulated false flow
        // of all other conjuncts.
        FalseFlow.unionWith(CurrentInfo);

        Element.walk(*this);
        continue;
      }

      // #available query: introduce a new refinement context for the statement
      // condition elements following it.
      auto *Query = Element.getAvailability();

      // If this query expression has no queries, we will not introduce a new
      // refinement context. We do not diagnose here: a diagnostic will already
      // have been emitted by the parser.
      if (Query->getQueries().size() == 0)
        continue;

      AvailabilitySpec *Spec = bestActiveSpecForQuery(Query);
      if (!Spec) {
        // We couldn't find an appropriate spec for the current platform,
        // so rather than refining, emit a diagnostic and just use the current
        // TRC.
        TC.Diags.diagnose(Query->getLoc(),
                          diag::availability_query_required_for_platform,
                          platformString(targetPlatform(TC.getLangOpts())));
        
        continue;
      }

      AvailabilityContext NewConstraint = contextForSpec(Spec);
      Query->setAvailableRange(NewConstraint.getOSVersion());

      if (Spec->getKind() == AvailabilitySpecKind::OtherPlatform) {
        // The wildcard spec '*' represents the minimum deployment target, so
        // there is no need to create a refinement context for this query.
        // Further, we won't diagnose for useless #available() conditions
        // where * matched on this platform -- presumably those conditions are
        // needed for some other platform.
        continue;
      }


      // If the version range for the current TRC is completely contained in
      // the range for the spec, then a version query can never be false, so the
      // spec is useless. If so, report this.
      if (CurrentInfo.isContainedIn(NewConstraint)) {
        DiagnosticEngine &Diags = TC.Diags;
        // Some availability checks will always pass because the minimum
        // deployment target guarantees they will never be false. We don't
        // diagnose these checks as useless because the source file may
        // be shared with other projects/targets having older deployment
        // targets. We don't currently have a mechanism for the user to
        // suppress these warnings (for example, by indicating when the
        // required compatibility version is different than the deployment
        // target).
        if (CurrentTRC->getReason() != TypeRefinementContext::Reason::Root) {
          Diags.diagnose(Query->getLoc(),
                         diag::availability_query_useless_enclosing_scope,
                         platformString(targetPlatform(TC.getLangOpts())));
          Diags.diagnose(CurrentTRC->getIntroductionLoc(),
                         diag::availability_query_useless_enclosing_scope_here);
        }

        // No need to actually create the refinement context if we know it is
        // useless.
        continue;
      }

      // If the #available() is not useless then there is potential false flow,
      // so join the false flow with the potential versions of the current
      // context.
      // We could be more precise here if we enriched the lattice to include
      // ranges of the form [x, y).
      FalseFlow.unionWith(CurrentInfo);

      auto *TRC = TypeRefinementContext::createForConditionFollowingQuery(
          TC.Context, Query, LastElement, CurrentTRC, NewConstraint);

      pushContext(TRC, ParentTy());
      NestedCount++;
    }


    Optional<AvailabilityContext> FalseRefinement = None;
    // The version range for the false branch should never have any versions
    // that weren't possible when the condition started evaluating.
    assert(FalseFlow.isContainedIn(StartingTRC->getAvailabilityInfo()));

    // If the starting version range is not completely contained in the
    // false flow version range then it must be the case that false flow range
    // is strictly smaller than the starting range (because the false flow
    // range *is* contained in the starting range), so we should introduce a
    // new refinement for the false flow.
    if (!StartingTRC->getAvailabilityInfo().isContainedIn(FalseFlow)) {
      FalseRefinement = FalseFlow;
    }

    if (NestedCount == 0)
      return std::make_pair(None, FalseRefinement);

    TypeRefinementContext *NestedTRC = getCurrentTRC();
    while (NestedCount-- > 0)
      ContextStack.pop_back();

    assert(getCurrentTRC() == StartingTRC);

    return std::make_pair(NestedTRC->getAvailabilityInfo(), FalseRefinement);
  }

  /// Return the best active spec for the target platform or nullptr if no
  /// such spec exists.
  AvailabilitySpec *bestActiveSpecForQuery(PoundAvailableInfo *available) {
    OtherPlatformAvailabilitySpec *FoundOtherSpec = nullptr;
    for (auto *Spec : available->getQueries()) {
      if (auto *OtherSpec = dyn_cast<OtherPlatformAvailabilitySpec>(Spec)) {
        FoundOtherSpec = OtherSpec;
        continue;
      }

      auto *VersionSpec = dyn_cast<VersionConstraintAvailabilitySpec>(Spec);
      if (!VersionSpec)
        continue;

      // FIXME: This is not quite right: we want to handle AppExtensions
      // properly. For example, on the OSXApplicationExtension platform
      // we want to chose the OS X spec unless there is an explicit
      // OSXApplicationExtension spec.
      if (isPlatformActive(VersionSpec->getPlatform(), TC.getLangOpts())) {
        return VersionSpec;
      }
    }

    // If we have reached this point, we found no spec for our target, so
    // we return the other spec ('*'), if we found it, or nullptr, if not.
    return FoundOtherSpec;
  }

  /// Return the availability context for the given spec.
  AvailabilityContext contextForSpec(AvailabilitySpec *Spec) {
    if (isa<OtherPlatformAvailabilitySpec>(Spec)) {
      return AvailabilityContext::alwaysAvailable();
    }

    auto *VersionSpec = cast<VersionConstraintAvailabilitySpec>(Spec);
    return AvailabilityContext(VersionRange::allGTE(VersionSpec->getVersion()));
  }

  virtual Expr *walkToExprPost(Expr *E) override {
    if (ContextStack.back().ScopeNode.getAsExpr() == E) {
      ContextStack.pop_back();
    }

    return E;
  }
};
  
}

void TypeChecker::buildTypeRefinementContextHierarchy(SourceFile &SF,
                                                      unsigned StartElem) {
  TypeRefinementContext *RootTRC = SF.getTypeRefinementContext();

  // If we are not starting at the beginning of the source file, we had better
  // already have a root type refinement context.
  assert(StartElem == 0 || RootTRC);

  ASTContext &AC = SF.getASTContext();

  if (!RootTRC) {
    // The root type refinement context reflects the fact that all parts of
    // the source file are guaranteed to be executing on at least the minimum
    // platform version.
    AvailabilityContext MinPlatformReq{
        VersionRange::allGTE(AC.LangOpts.getMinPlatformVersion())};
    RootTRC = TypeRefinementContext::createRoot(&SF, MinPlatformReq);
    SF.setTypeRefinementContext(RootTRC);
  }

  // Build refinement contexts, if necessary, for all declarations starting
  // with StartElem.
  TypeRefinementContextBuilder Builder(RootTRC, *this);
  for (auto D : llvm::makeArrayRef(SF.Decls).slice(StartElem)) {
    Builder.build(D);
  }
}

TypeRefinementContext *
TypeChecker::getOrBuildTypeRefinementContext(SourceFile *SF) {
  TypeRefinementContext *TRC = SF->getTypeRefinementContext();
  if (!TRC) {
    buildTypeRefinementContextHierarchy(*SF, 0);
    TRC = SF->getTypeRefinementContext();
  }

  return TRC;
}

AvailabilityContext
TypeChecker::overApproximateAvailabilityAtLocation(SourceLoc loc,
                                                   const DeclContext *DC) {
  SourceFile *SF = DC->getParentSourceFile();

  // If our source location is invalid (this may be synthesized code), climb
  // the decl context hierarchy until we find a location that is valid,
  // collecting availability ranges on the way up.
  // We will combine the version ranges from these annotations
  // with the TRC for the valid location to overapproximate the running
  // OS versions at the original source location.
  // Because we are climbing DeclContexts we will miss refinement contexts in
  // synthesized code that are introduced by AST elements that are themselves
  // not DeclContexts, such as  #available(..) and property declarations.
  // That is, a reference with an invalid location that is contained
  // inside a #available() and with no intermediate DeclContext will not be
  // refined. For now, this is fine -- but if we ever synthesize #available(),
  // this will be a real problem.

  // We can assume we are running on at least the minimum deployment target.
  AvailabilityContext OverApproximateContext{
    VersionRange::allGTE(getLangOpts().getMinPlatformVersion())};

  while (DC && loc.isInvalid()) {
    const Decl *D = DC->getInnermostDeclarationDeclContext();
    if (!D)
      break;

    loc = D->getLoc();

    Optional<AvailabilityContext> Info =
        AvailabilityInference::annotatedAvailableRange(D, Context);

    if (Info.hasValue()) {
      OverApproximateContext.constrainWith(Info.getValue());
    }

    DC = D->getDeclContext();
  }

  if (SF && loc.isValid()) {
    TypeRefinementContext *rootTRC = getOrBuildTypeRefinementContext(SF);
    TypeRefinementContext *TRC =
        rootTRC->findMostRefinedSubContext(loc, Context.SourceMgr);
    OverApproximateContext.constrainWith(TRC->getAvailabilityInfo());
  }

  return OverApproximateContext;
}

bool TypeChecker::isDeclAvailable(const Decl *D, SourceLoc referenceLoc,
                                  const DeclContext *referenceDC,
                                  AvailabilityContext &OutAvailableInfo) {

  AvailabilityContext safeRangeUnderApprox{
      AvailabilityInference::availableRange(D, Context)};
  AvailabilityContext runningOSOverApprox =
      overApproximateAvailabilityAtLocation(referenceLoc, referenceDC);

  // The reference is safe if an over-approximation of the running OS
  // versions is fully contained within an under-approximation
  // of the versions on which the declaration is available. If this
  // containment cannot be guaranteed, we say the reference is
  // not available.
  if (!(runningOSOverApprox.isContainedIn(safeRangeUnderApprox))) {
    OutAvailableInfo = safeRangeUnderApprox;
    return false;
  }
  
  return true;
}

Optional<UnavailabilityReason>
TypeChecker::checkDeclarationAvailability(const Decl *D, SourceLoc referenceLoc,
                                          const DeclContext *referenceDC) {
  if (Context.LangOpts.DisableAvailabilityChecking) {
    return None;
  }

  if (!referenceDC->getParentSourceFile()) {
    // We only check availability if this reference is in a source file; we do
    // not check in other kinds of FileUnits.
    return None;
  }

  auto safeRangeUnderApprox = AvailabilityContext::neverAvailable();
  if (isDeclAvailable(D, referenceLoc, referenceDC, safeRangeUnderApprox)) {
    return None;
  }

  // safeRangeUnderApprox now holds the safe range.
  VersionRange version = safeRangeUnderApprox.getOSVersion();
  return UnavailabilityReason::requiresVersionRange(version);
}

void TypeChecker::diagnosePotentialUnavailability(
    const ValueDecl *D, SourceRange ReferenceRange,
    const DeclContext *ReferenceDC,
    const UnavailabilityReason &Reason) {
  diagnosePotentialUnavailability(D, D->getFullName(), ReferenceRange,
                                  ReferenceDC, Reason);
}

/// A class that walks the AST to find the innermost (i.e., deepest) node that
/// contains a target SourceRange and matches a particular criterion.
/// This class finds the innermost nodes of interest by walking
/// down the root until it has found the target range (in a Pre-visitor)
/// and then recording the innermost node on the way back up in the
/// Post-visitors. It does its best to not search unnecessary subtrees,
/// although this is complicated by the fact that not all nodes have
/// source range information.
class InnermostAncestorFinder : private ASTWalker {
public:

  /// The type of a match predicate, which takes as input a node and its
  /// parent and returns a bool indicating whether the node matches.
  typedef std::function<bool(ASTNode, ASTWalker::ParentTy)> MatchPredicate;

private:
  const SourceRange TargetRange;
  const SourceManager &SM;
  const MatchPredicate Predicate;

  bool FoundTarget = false;
  Optional<ASTNode> InnermostMatchingNode;

public:
  InnermostAncestorFinder(SourceRange TargetRange, const SourceManager &SM,
                          ASTNode SearchNode, const MatchPredicate &Predicate)
      : TargetRange(TargetRange), SM(SM), Predicate(Predicate) {
    assert(TargetRange.isValid());

    SearchNode.walk(*this);
  }

  /// Returns the innermost node containing the target range that matches
  /// the predicate.
  Optional<ASTNode> getInnermostMatchingNode() { return InnermostMatchingNode; }

  virtual std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    return std::make_pair(walkToRangePre(E->getSourceRange()), E);
  }

  virtual std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    return std::make_pair(walkToRangePre(S->getSourceRange()), S);
  }

  virtual bool walkToDeclPre(Decl *D) override {
    return walkToRangePre(D->getSourceRange());
  }

  virtual std::pair<bool, Pattern *> walkToPatternPre(Pattern *P) override {
    return std::make_pair(walkToRangePre(P->getSourceRange()), P);
  }

  virtual bool walkToTypeReprPre(TypeRepr *T) override {
    return walkToRangePre(T->getSourceRange());
  }

  /// Returns true if the walker should traverse an AST node with
  /// source range Range.
  bool walkToRangePre(SourceRange Range) {
    // When walking down the tree, we traverse until we have found a node
    // inside the target range. Once we have found such a node, there is no
    // need to traverse any deeper.
    if (FoundTarget)
      return false;

    // If we haven't found our target yet and the node we are pre-visiting
    // doesn't have a valid range, we still have to traverse it because its
    // subtrees may have valid ranges.
    if (Range.isInvalid())
      return true;

    // We have found our target if the range of the node we are visiting
    // is contained in the range we are looking for.
    FoundTarget = SM.rangeContains(TargetRange, Range);

    if (FoundTarget)
      return false;

    // Search the subtree if the target range is inside its range.
    return SM.rangeContains(Range, TargetRange);
  }

  virtual Expr *walkToExprPost(Expr *E) override {
    if (walkToNodePost(E)) {
      return E;
    }

    return nullptr;
  }

  virtual Stmt *walkToStmtPost(Stmt *S) override {
    if (walkToNodePost(S)) {
      return S;
    }

    return nullptr;
  }

  virtual bool walkToDeclPost(Decl *D) override {
    return walkToNodePost(D);
  }

  /// Once we have found the target node, look for the innermost ancestor
  /// matching our criteria on the way back up the spine of the tree.
  bool walkToNodePost(ASTNode Node) {
    if (!InnermostMatchingNode.hasValue() && Predicate(Node, Parent)) {
      assert(Node.getSourceRange().isInvalid() ||
             SM.rangeContains(Node.getSourceRange(), TargetRange));

      InnermostMatchingNode = Node;
      return false;
    }

    return true;
  }
};

/// Starting from SearchRoot, finds the innermost node containing ChildRange
/// for which Predicate returns true. Returns None if no such root is found.
static Optional<ASTNode> findInnermostAncestor(
    SourceRange ChildRange, const SourceManager &SM, ASTNode SearchRoot,
    const InnermostAncestorFinder::MatchPredicate &Predicate) {
  InnermostAncestorFinder Finder(ChildRange, SM, SearchRoot, Predicate);
  return Finder.getInnermostMatchingNode();
}

/// Given a reference range and a declaration context containing the range,
/// attempt to find a declaration containing the reference. This may not
/// be the innermost declaration containing the range.
/// Returns null if no such declaration can be found.
static const Decl *findContainingDeclaration(SourceRange ReferenceRange,
                                             const DeclContext *ReferenceDC,
                                             const SourceManager &SM) {
  if (const Decl *D = ReferenceDC->getInnermostDeclarationDeclContext())
    return D;

  // We couldn't find a suitable node by climbing the DeclContext
  // hierarchy, so fall back to looking for a top-level declaration
  // that contains the reference range. We will hit this case for
  // top-level elements that do not themselves introduce DeclContexts,
  // such as extensions and global variables. If we don't have a reference
  // range, there is nothing we can do, so return null.
  if (ReferenceRange.isInvalid())
    return nullptr;

  SourceFile *SF = ReferenceDC->getParentSourceFile();
  if (!SF)
    return nullptr;

  for (Decl *D : SF->Decls) {
    if (SM.rangeContains(D->getSourceRange(), ReferenceRange)) {
      return D;
    }
  }

  return nullptr;
}

/// Given a declaration that allows availability attributes in the abstract
/// syntax tree, return the declaration upon which the declaration would
/// appear in concrete syntax. This function is necessary because for semantic
/// analysis, the parser attaches attributes to declarations other
/// than those on which they, concretely, appear. For these declarations (enum
/// cases and variable declarations) a Fix-It for an added availability
/// attribute should be suggested for the appropriate concrete location.
static const Decl *
concreteSyntaxDeclForAvailableAttribute(const Decl *AbstractSyntaxDecl) {
  // This function needs to be kept in sync with its counterpart,
  // abstractSyntaxDeclForAvailableAttribute().

  // The source range for VarDecls does not include 'var ' (and, in any
  // event, multiple variables can be introduced with a single 'var'),
  // so suggest adding an attribute to the PatterningBindingDecl instead.
  if (auto *VD = dyn_cast<VarDecl>(AbstractSyntaxDecl)) {
    return VD->getParentPatternBinding();
  }

  // Similarly suggest applying the Fix-It to the parent enum case rather than
  // the enum element.
  if (auto *EE = dyn_cast<EnumElementDecl>(AbstractSyntaxDecl)) {
    return EE->getParentCase();
  }

  return AbstractSyntaxDecl;
}

/// Given a declaration upon which an availability attribute would appear in
/// concrete syntax, return a declaration to which the parser
/// actually attaches the attribute in the abstract syntax tree. We use this
/// function to determine whether the concrete syntax already has an
/// availability attribute.
static const Decl *
abstractSyntaxDeclForAvailableAttribute(const Decl *ConcreteSyntaxDecl) {
  // This function needs to be kept in sync with its counterpart,
  // concreteSyntaxDeclForAvailableAttribute().

  if (auto *PBD = dyn_cast<PatternBindingDecl>(ConcreteSyntaxDecl)) {
    // Existing @available attributes in the AST are attached to VarDecls
    // rather than PatternBindingDecls, so we return the first VarDecl for
    // the pattern binding declaration.
    // This is safe, even though there may be multiple VarDecls, because
    // all parsed attribute that appear in the concrete syntax upon on the
    // PatternBindingDecl are added to all of the VarDecls for the pattern
    // binding.
    ArrayRef<PatternBindingEntry> Entries = PBD->getPatternList();
    if (Entries.size() > 0) {
      VarDecl *VD = Entries.front().getPattern()->getSingleVar();
      if (VD)
        return VD;
    }
  } else if (auto *ECD = dyn_cast<EnumCaseDecl>(ConcreteSyntaxDecl)) {
    // Similar to the PatternBindingDecl case above, we return the
    // first EnumElementDecl.
    ArrayRef<EnumElementDecl *> Elems = ECD->getElements();
    if (Elems.size() > 0) {
      return Elems.front();
    }
  }

  return ConcreteSyntaxDecl;
}

/// Given a declaration, return a better related declaration for which
/// to suggest an @available fixit, or the original declaration
/// if no such related declaration exists.
static const Decl *relatedDeclForAvailabilityFixit(const Decl *D) {
  if (auto *FD = dyn_cast<FuncDecl>(D)) {
    // Suggest @available Fix-Its on property rather than individual
    // accessors.
    if (FD->isAccessor()) {
      D = FD->getAccessorStorageDecl();
    }
  }

  return abstractSyntaxDeclForAvailableAttribute(D);
}

/// Walk the DeclContext hierarchy starting from D to find a declaration
/// at the member level (i.e., declared in a type context) on which to provide
/// an @available() Fix-It.
static const Decl *ancestorMemberLevelDeclForAvailabilityFixit(const Decl *D) {
  while (D) {
    D = relatedDeclForAvailabilityFixit(D);

    if (D->getDeclContext()->isTypeContext() &&
        DeclAttribute::canAttributeAppearOnDecl(DeclAttrKind::DAK_Available,
                                                D)) {
      break;
    }

    D = cast_or_null<AbstractFunctionDecl>(
        D->getDeclContext()->getInnermostMethodContext());
  }

  return D;
}

/// Returns true if the declaration is at the type level (either a nominal
/// type, an extension, or a global function) and can support an @available
/// attribute.
static bool isTypeLevelDeclForAvailabilityFixit(const Decl *D) {
  if (!DeclAttribute::canAttributeAppearOnDecl(DeclAttrKind::DAK_Available,
                                               D)) {
    return false;
  }

  if (isa<ExtensionDecl>(D) || isa<NominalTypeDecl>(D)) {
    return true;
  }

  bool IsModuleScopeContext = D->getDeclContext()->isModuleScopeContext();

  // We consider global functions to be "type level"
  if (isa<FuncDecl>(D)) {
    return IsModuleScopeContext;
  }

  if (auto *VD = dyn_cast<VarDecl>(D)) {
    if (!IsModuleScopeContext)
      return false;

    if (PatternBindingDecl *PBD = VD->getParentPatternBinding()) {
      return PBD->getDeclContext()->isModuleScopeContext();
    }
  }

  return false;
}

/// Walk the DeclContext hierarchy starting from D to find a declaration
/// at a member level (i.e., declared in a type context) on which to provide an
/// @available() Fix-It.
static const Decl *ancestorTypeLevelDeclForAvailabilityFixit(const Decl *D) {
  assert(D);

  D = relatedDeclForAvailabilityFixit(D);

  while (D && !isTypeLevelDeclForAvailabilityFixit(D)) {
    D = D->getDeclContext()->getInnermostDeclarationDeclContext();
  }

  return D;
}

/// Given the range of a reference to an unavailable symbol and the
/// declaration context containing the reference, make a best effort find up to
/// three locations for potential fixits.
///
/// \param FoundVersionCheckNode Returns a node that can be wrapped in a
/// if #available(...) { ... } version check to fix the unavailable reference,
/// or None if such a node cannot be found.
///
/// \param FoundMemberLevelDecl Returns member-level declaration (i.e., the
///  child of a type DeclContext) for which an @available attribute would
/// fix the unavailable reference.
///
/// \param FoundTypeLevelDecl returns a type-level declaration (a
/// a nominal type, an extension, or a global function) for which an
/// @available attribute would fix the unavailable reference.
static void findAvailabilityFixItNodes(SourceRange ReferenceRange,
                                       const DeclContext *ReferenceDC,
                                       const SourceManager &SM,
                                       Optional<ASTNode> &FoundVersionCheckNode,
                                       const Decl *&FoundMemberLevelDecl,
                                       const Decl *&FoundTypeLevelDecl) {
  FoundVersionCheckNode = None;
  FoundMemberLevelDecl = nullptr;
  FoundTypeLevelDecl = nullptr;

  // Limit tree to search based on the DeclContext of the reference.
  const Decl *DeclarationToSearch =
      findContainingDeclaration(ReferenceRange, ReferenceDC, SM);
  if (!DeclarationToSearch)
    return;

  // Const-cast to inject into ASTNode. This search will not modify
  // the declaration.
  ASTNode SearchRoot = const_cast<Decl *>(DeclarationToSearch);

  // The node to wrap in if #available(...) { ... } is the innermost node in
  // SearchRoot that (1) can be guarded with an if statement and (2)
  // contains the ReferenceRange.
  // We make no guarantee that the Fix-It, when applied, will result in
  // semantically valid code -- but, at a minimum, it should parse. So,
  // for example, we may suggest wrapping a variable declaration in a guard,
  // which would not be valid if the variable is later used. The goal
  // is discoverability of #os() (via the diagnostic and Fix-It) rather than
  // magically fixing the code in all cases.

  InnermostAncestorFinder::MatchPredicate IsGuardable =
      [](ASTNode Node, ASTWalker::ParentTy Parent) {
        if (Expr *ParentExpr = Parent.getAsExpr()) {
          auto *ParentClosure = dyn_cast<ClosureExpr>(ParentExpr);
          if (!ParentClosure || !ParentClosure->hasSingleExpressionBody()) {
            return false;
          }
        } else if (auto *ParentStmt = Parent.getAsStmt()) {
          if (!isa<BraceStmt>(ParentStmt)) {
            return false;
          }
        } else {
          return false;
        }

        return true;
      };

  FoundVersionCheckNode =
      findInnermostAncestor(ReferenceRange, SM, SearchRoot, IsGuardable);

  // Find some Decl that contains the reference range. We use this declaration
  // as a starting place to climb the DeclContext hierarchy to find
  // places to suggest adding @available() annotations.
  InnermostAncestorFinder::MatchPredicate IsDeclaration = [](
      ASTNode Node, ASTWalker::ParentTy Parent) { return Node.is<Decl *>(); };

  Optional<ASTNode> FoundDeclarationNode =
      findInnermostAncestor(ReferenceRange, SM, SearchRoot, IsDeclaration);

  const Decl *ContainingDecl = nullptr;
  if (FoundDeclarationNode.hasValue()) {
    ContainingDecl = FoundDeclarationNode.getValue().get<Decl *>();
  }

  if (!ContainingDecl) {
    ContainingDecl = ReferenceDC->getInnermostMethodContext();
  }

  // Try to find declarations on which @available attributes can be added.
  // The heuristics for finding these declarations are biased towards deeper
  // nodes in the AST to limit the scope of suggested availability regions
  // and provide a better IDE experience (it can get jumpy if Fix-It locations
  // are far away from the error needing the Fix-It).
  if (ContainingDecl) {
    FoundMemberLevelDecl =
        ancestorMemberLevelDeclForAvailabilityFixit(ContainingDecl);

    FoundTypeLevelDecl =
        ancestorTypeLevelDeclForAvailabilityFixit(ContainingDecl);
  }
}

/// Emit a diagnostic note and Fix-It to add an @available attribute
/// on the given declaration for the given version range.
static void fixAvailabilityForDecl(SourceRange ReferenceRange, const Decl *D,
                                   const VersionRange &RequiredRange,
                                   TypeChecker &TC) {
  assert(D);

  // Don't suggest adding an @available() to a declaration where we would
  // emit a diagnostic saying it is not allowed.
  if (TC.diagnosticIfDeclCannotBePotentiallyUnavailable(D).hasValue())
    return;

  if (getActiveAvailableAttribute(D, TC.Context)) {
    // For QoI, in future should emit a fixit to update the existing attribute.
    return;
  }

  // For some declarations (variables, enum elements), the location in concrete
  // syntax to suggest the Fix-It may differ from the declaration to which
  // we attach availability attributes in the abstract syntax tree during
  // parsing.
  const Decl *ConcDecl = concreteSyntaxDeclForAvailableAttribute(D);

  DescriptiveDeclKind KindForDiagnostic = ConcDecl->getDescriptiveKind();
  SourceLoc InsertLoc;

  // To avoid exposing the pattern binding declaration to the user, get the
  // descriptive kind from one of the VarDecls. We get the Fix-It location
  // from the PatternBindingDecl unless the VarDecl has attributes,
  // in which case we get the start location of the VarDecl attributes.
  DeclAttributes AttrsForLoc;
  if (KindForDiagnostic == DescriptiveDeclKind::PatternBinding) {
    KindForDiagnostic = D->getDescriptiveKind();
    AttrsForLoc = D->getAttrs();
  } else {
    InsertLoc = ConcDecl->getAttrs().getStartLoc(/*forModifiers=*/false);
  }

  InsertLoc = D->getAttrs().getStartLoc(/*forModifiers=*/false);
  if (InsertLoc.isInvalid()) {
    InsertLoc = ConcDecl->getStartLoc();
  }

  if (InsertLoc.isInvalid())
    return;

  StringRef OriginalIndent =
      Lexer::getIndentationForLine(TC.Context.SourceMgr, InsertLoc);

  std::string AttrText;
  {
    llvm::raw_string_ostream Out(AttrText);

    PlatformKind Target = targetPlatform(TC.getLangOpts());
    Out << "@available(" << platformString(Target) << " "
        << RequiredRange.getLowerEndpoint().getAsString() << ", *)\n"
        << OriginalIndent;
  }

  TC.diagnose(ReferenceRange.Start, diag::availability_add_attribute,
              KindForDiagnostic)
      .fixItInsert(InsertLoc, AttrText);
}

/// Emit a diagnostic note and Fix-It to add an if #available(...) { } guard
/// that checks for the given version range around the given node.
static void fixAvailabilityByAddingVersionCheck(
    ASTNode NodeToWrap, const VersionRange &RequiredRange,
    SourceRange ReferenceRange, TypeChecker &TC) {
  SourceRange RangeToWrap = NodeToWrap.getSourceRange();
  if (RangeToWrap.isInvalid())
    return;

  SourceLoc ReplaceLocStart = RangeToWrap.Start;
  StringRef OriginalIndent =
      Lexer::getIndentationForLine(TC.Context.SourceMgr, ReplaceLocStart);

  std::string IfText;
  {
    llvm::raw_string_ostream Out(IfText);

    SourceLoc ReplaceLocEnd =
        Lexer::getLocForEndOfToken(TC.Context.SourceMgr, RangeToWrap.End);

    std::string GuardedText =
        TC.Context.SourceMgr.extractText(CharSourceRange(TC.Context.SourceMgr,
                                                         ReplaceLocStart,
                                                         ReplaceLocEnd)).str();

    // We'll indent with 4 spaces
    std::string ExtraIndent = "    ";
    std::string NewLine = "\n";

    // Indent the body of the Fix-It if. Because the body may be a compound
    // statement, we may have to indent multiple lines.
    size_t StartAt = 0;
    while ((StartAt = GuardedText.find(NewLine, StartAt)) !=
           std::string::npos) {
      GuardedText.replace(StartAt, NewLine.length(), NewLine + ExtraIndent);
      StartAt += NewLine.length();
    }

    PlatformKind Target = targetPlatform(TC.getLangOpts());

    Out << "if #available(" << platformString(Target)
        << " " << RequiredRange.getLowerEndpoint().getAsString()
        << ", *) {\n";

    Out << OriginalIndent << ExtraIndent << GuardedText << "\n";

    // We emit an empty fallback case with a comment to encourage the developer
    // to think explicitly about whether fallback on earlier versions is needed.
    Out << OriginalIndent << "} else {\n";
    Out << OriginalIndent << ExtraIndent << "// Fallback on earlier versions\n";
    Out << OriginalIndent << "}";
  }

  TC.diagnose(ReferenceRange.Start, diag::availability_guard_with_version_check)
      .fixItReplace(RangeToWrap, IfText);
}

/// Emit suggested Fix-Its for a reference with to an unavailable symbol
/// requiting the given OS version range.
static void fixAvailability(SourceRange ReferenceRange,
                            const DeclContext *ReferenceDC,
                            const VersionRange &RequiredRange,
                            TypeChecker &TC) {
  if (ReferenceRange.isInvalid())
    return;

  Optional<ASTNode> NodeToWrapInVersionCheck;
  const Decl *FoundMemberDecl = nullptr;
  const Decl *FoundTypeLevelDecl = nullptr;

  findAvailabilityFixItNodes(ReferenceRange, ReferenceDC, TC.Context.SourceMgr,
                             NodeToWrapInVersionCheck, FoundMemberDecl,
                             FoundTypeLevelDecl);

  // Suggest wrapping in if #available(...) { ... } if possible.
  if (NodeToWrapInVersionCheck.hasValue()) {
    fixAvailabilityByAddingVersionCheck(NodeToWrapInVersionCheck.getValue(),
                                        RequiredRange, ReferenceRange, TC);
  }

  // Suggest adding availability attributes.
  if (FoundMemberDecl) {
    fixAvailabilityForDecl(ReferenceRange, FoundMemberDecl, RequiredRange, TC);
  }

  if (FoundTypeLevelDecl) {
    fixAvailabilityForDecl(ReferenceRange, FoundTypeLevelDecl, RequiredRange,
                           TC);
  }
}

void TypeChecker::diagnosePotentialUnavailability(
    const Decl *D, DeclName Name, SourceRange ReferenceRange,
    const DeclContext *ReferenceDC, const UnavailabilityReason &Reason) {

  // We only emit diagnostics for API unavailability, not for explicitly
  // weak-linked symbols.
  if (Reason.getReasonKind() !=
      UnavailabilityReason::Kind::RequiresOSVersionRange) {
    return;
  }

  diagnose(ReferenceRange.Start, diag::availability_decl_only_version_newer,
           Name, prettyPlatformString(targetPlatform(Context.LangOpts)),
           Reason.getRequiredOSVersionRange().getLowerEndpoint());

  fixAvailability(ReferenceRange, ReferenceDC,
                  Reason.getRequiredOSVersionRange(), *this);
}

void TypeChecker::diagnosePotentialAccessorUnavailability(
    FuncDecl *Accessor, SourceRange ReferenceRange,
    const DeclContext *ReferenceDC, const UnavailabilityReason &Reason,
    bool ForInout) {
  assert(Accessor->isGetterOrSetter());

  AbstractStorageDecl *ASD = Accessor->getAccessorStorageDecl();
  DeclName Name = ASD->getFullName();

  auto &diag = ForInout ? diag::availability_inout_accessor_only_version_newer
                        : diag::availability_accessor_only_version_newer;

  diagnose(ReferenceRange.Start, diag,
           static_cast<unsigned>(Accessor->getAccessorKind()), Name,
           prettyPlatformString(targetPlatform(Context.LangOpts)),
           Reason.getRequiredOSVersionRange().getLowerEndpoint());

  fixAvailability(ReferenceRange, ReferenceDC,
                  Reason.getRequiredOSVersionRange(), *this);
}

const AvailableAttr *TypeChecker::getDeprecated(const Decl *D) {
  if (auto *Attr = D->getAttrs().getDeprecated(D->getASTContext()))
    return Attr;

  // Treat extensions methods as deprecated if their extension
  // is deprecated.
  DeclContext *DC = D->getDeclContext();
  if (auto *ED = dyn_cast<ExtensionDecl>(DC)) {
    return getDeprecated(ED);
  }

  return nullptr;
}

/// Returns true if some declaration lexically enclosing the reference
/// matches the passed in predicate and false otherwise.
static bool someEnclosingDeclMatches(SourceRange ReferenceRange,
                                     const DeclContext *ReferenceDC,
                                     TypeChecker &TC,
                                     std::function<bool(const Decl *)> Pred) {
  ASTContext &Ctx = TC.Context;

  // Climb the DeclContext hierarchy to see if any of the containing
  // declarations matches the predicate.
  const DeclContext *DC = ReferenceDC;
  do {
    auto *D = DC->getInnermostDeclarationDeclContext();
    if (!D)
      break;

    if (Pred(D)) {
      return true;
    }

    // If we are in an accessor, check to see if the associated
    // property is matches the predicate.
    auto *FD = dyn_cast<FuncDecl>(D);
    if (FD && FD->isAccessor() && Pred(FD->getAccessorStorageDecl())) {
      return true;
    }

    DC = DC->getParent();
  } while (DC);

  // Search the AST starting from our innermost declaration context to see if
  // if the reference is inside a property declaration but not inside an
  // accessor (this can happen for the TypeRepr for the declared type of a
  // property, for example).
  // We can't rely on the DeclContext hierarchy climb above because properties
  // do not introduce a new DeclContext. This search is potentially slow, so we
  // do it last and only if the reference declaration context is a
  // type or global context.

  if (!ReferenceDC->isTypeContext() && !ReferenceDC->isModuleScopeContext())
    return false;

  // Don't search for a containing declaration if we don't have a source range.
  if (ReferenceRange.isInvalid())
    return false;

  const Decl *DeclToSearch =
      findContainingDeclaration(ReferenceRange, ReferenceDC, Ctx.SourceMgr);

  // We may not be able to find a declaration to search if the ReferenceRange
  // is invalid (i.e., we are in synthesized code).
  if (!DeclToSearch)
    return false;

  InnermostAncestorFinder::MatchPredicate IsDeclaration =
      [](ASTNode Node, ASTWalker::ParentTy Parent) {
        return Node.is<Decl *>();
  };

  Optional<ASTNode> FoundDeclarationNode =
      findInnermostAncestor(ReferenceRange, Ctx.SourceMgr,
                            const_cast<Decl *>(DeclToSearch), IsDeclaration);

  if (FoundDeclarationNode.hasValue()) {
    const Decl *D = FoundDeclarationNode.getValue().get<Decl *>();
    D = abstractSyntaxDeclForAvailableAttribute(D);
    if (Pred(D)) {
      return true;
    }
  }

  return false;
}

bool TypeChecker::isInsideImplicitFunction(SourceRange ReferenceRange,
                                           const DeclContext *DC) {
  auto IsInsideImplicitFunc = [](const Decl *D) {
    auto *AFD = dyn_cast<AbstractFunctionDecl>(D);
    return AFD && AFD->isImplicit();
  };

  return someEnclosingDeclMatches(ReferenceRange, DC, *this,
                                  IsInsideImplicitFunc);
}


bool TypeChecker::isInsideUnavailableDeclaration(
    SourceRange ReferenceRange, const DeclContext *ReferenceDC) {
  auto IsUnavailable = [](const Decl *D) {
    return D->getAttrs().getUnavailable(D->getASTContext());
  };

  return someEnclosingDeclMatches(ReferenceRange, ReferenceDC, *this,
                                  IsUnavailable);
}

bool TypeChecker::isInsideDeprecatedDeclaration(SourceRange ReferenceRange,
                                                const DeclContext *ReferenceDC){
  std::function<bool(const Decl *)> IsDeprecated = [](const Decl *D) {
    return D->getAttrs().getDeprecated(D->getASTContext());
  };

  return someEnclosingDeclMatches(ReferenceRange, ReferenceDC, *this,
                                  IsDeprecated);
}

// checkForForbiddenPrefix is for testing purposes.

void TypeChecker::checkForForbiddenPrefix(const Decl *D) {
  if (!hasEnabledForbiddenTypecheckPrefix())
    return;
  if (auto VD = dyn_cast<ValueDecl>(D)) {
    checkForForbiddenPrefix(VD->getNameStr());
  }
}

void TypeChecker::checkForForbiddenPrefix(const UnresolvedDeclRefExpr *E) {
  if (!hasEnabledForbiddenTypecheckPrefix())
    return;
  checkForForbiddenPrefix(E->getName().getBaseName());
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
