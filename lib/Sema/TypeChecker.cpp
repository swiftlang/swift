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
#include "swift/AST/Identifier.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
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

namespace swift {

/// Clone the given generic parameters in the given list. We don't need any
/// of the requirements, because they will be inferred.
GenericParamList *cloneGenericParams(ASTContext &ctx,
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
} // namespace swift

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

  // Hack to allow extending a generic typealias.
  if (auto *unboundGeneric = extendedType->getAs<UnboundGenericType>()) {
    if (auto *aliasDecl = dyn_cast<TypeAliasDecl>(unboundGeneric->getDecl())) {
      auto extendedNominal = aliasDecl->getDeclaredInterfaceType()->getAnyNominal();
      if (extendedNominal) {
        extendedType = extendedNominal->getDeclaredType();
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
  if (extendedNominal->isGenericContext()) {
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
    if (!extendedNominal->isGenericContext()) {
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

static bool shouldValidateMemberDuringFinalization(NominalTypeDecl *nominal,
                                                   ValueDecl *VD) {
  // For enums, we only need to validate enum elements to know
  // the layout.
  if (isa<EnumDecl>(nominal) &&
      isa<EnumElementDecl>(VD))
    return true;

  // For structs, we only need to validate stored properties to
  // know the layout.
  if (isa<StructDecl>(nominal) &&
      (isa<VarDecl>(VD) &&
       !cast<VarDecl>(VD)->isStatic() &&
       (cast<VarDecl>(VD)->hasStorage() ||
        VD->getAttrs().hasAttribute<LazyAttr>())))
    return true;

  // For classes, we need to validate properties and functions,
  // but skipping nested types is OK.
  if (isa<ClassDecl>(nominal) &&
      !isa<TypeDecl>(VD))
    return true;

  // For protocols, skip nested typealiases and nominal types.
  if (isa<ProtocolDecl>(nominal) &&
      !isa<GenericTypeDecl>(VD))
    return true;

  return false;
}

static void finalizeType(TypeChecker &TC, NominalTypeDecl *nominal) {
  Optional<bool> lazyVarsAlreadyHaveImplementation;

  for (auto *D : nominal->getMembers()) {
    auto VD = dyn_cast<ValueDecl>(D);
    if (!VD)
      continue;

    if (!shouldValidateMemberDuringFinalization(nominal, VD))
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

  // validateDeclForNameLookup will not trigger an immediate full
  // validation of protocols, but clients will assume that things
  // like the requirement signature have been set.
  if (auto PD = dyn_cast<ProtocolDecl>(nominal)) {
    if (!PD->isRequirementSignatureComputed()) {
      TC.validateDecl(PD);
    }
  }
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
    while (!TC.TypesToFinalize.empty()) {
      auto nominal = TC.TypesToFinalize.pop_back_val();
      if (nominal->isInvalid() || TC.Context.hadError())
        continue;

      finalizeType(TC, nominal);
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
           !TC.TypesToFinalize.empty() ||
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

  auto &Ctx = SF.getASTContext();

  // Make sure we have a type checker.
  Optional<TypeChecker> MyTC;
  if (!Ctx.getLazyResolver())
    MyTC.emplace(Ctx);

  // Make sure that name binding has been completed before doing any type
  // checking.
  {
    SharedTimer timer("Name binding");
    performNameBinding(SF, StartElem);
  }

  {
    // NOTE: The type checker is scoped to be torn down before AST
    // verification.
    SharedTimer timer("Type checking / Semantic analysis");

    if (MyTC) {
      MyTC->setWarnLongFunctionBodies(WarnLongFunctionBodies);
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
    if (SF.Kind == SourceFileKind::REPL && !Ctx.hadError())
      TC.processREPLTopLevel(SF, TLC, StartElem);

    typeCheckFunctionsAndExternalDecls(TC);
  }
  MyTC.reset();

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
        SF.Kind != SourceFileKind::SIL &&
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
  options |= TR_AllowUnboundGenerics;
  if (isSILMode)
    options |= TR_SILMode;
  if (isSILType)
    options |= TR_SILType;

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
    TC.typeCheckDecl(D, true);
  return true;
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
      return TC.checkObjCKeyPathExpr(DC, keyPath, /*requireResultType=*/true);

    return None;
  }

  Type originalType = parsedExpr->getType();
  if (auto T = TC.getTypeOfExpressionWithoutApplying(parsedExpr, DC,
                 referencedDecl, FreeTypeVariableBinding::GenericParameters))
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

void swift::
collectDefaultImplementationForProtocolMembers(ProtocolDecl *PD,
                    llvm::SmallDenseMap<ValueDecl*, ValueDecl*> &DefaultMap) {
  Type BaseTy = PD->getDeclaredInterfaceType();
  DeclContext *DC = PD->getInnermostDeclContext();
  std::unique_ptr<TypeChecker> CreatedTC;
  auto *TC = static_cast<TypeChecker*>(DC->getASTContext().getLazyResolver());
  if (!TC) {
    CreatedTC.reset(new TypeChecker(DC->getASTContext()));
    TC = CreatedTC.get();
  }
  auto HandleMembers = [&](DeclRange Members) {
    for (Decl *D : Members) {
      ValueDecl *VD = dyn_cast<ValueDecl>(D);

      // Skip non-value decl.
      if (!VD)
        continue;

      // Skip decls with empty names, e.g. setter/getters for properties.
      if (VD->getName().empty())
        continue;

      ResolvedMemberResult Result = resolveValueMember(*DC, BaseTy,
                                                       VD->getFullName());
      assert(Result);
      for (auto *Default : Result.getMemberDecls(InterestedMemberKind::All)) {
        if (PD == Default->getDeclContext()->getAsProtocolExtensionContext()) {
          DefaultMap.insert({Default, VD});
        }
      }
    }
  };

  // Collect the default implementations for the members in this given protocol.
  HandleMembers(PD->getMembers());

  // Collect the default implementations for the members in the inherited
  // protocols.
  for (auto* IP : PD->getInheritedProtocols())
    HandleMembers(IP->getMembers());
}
