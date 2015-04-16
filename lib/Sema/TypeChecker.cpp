//===--- TypeChecker.cpp - Type Checking ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
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
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Parse/Lexer.h"
#include "swift/Sema/CodeCompletionTypeChecking.h"
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
                       KnownProtocolKind::ArrayLiteralConvertible);

  if (isa<DictionaryExpr>(expr))
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::DictionaryLiteralConvertible);

  if (!isa<LiteralExpr>(expr))
    return nullptr;
  
  if (isa<NilLiteralExpr>(expr))
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::NilLiteralConvertible);
  
  if (isa<IntegerLiteralExpr>(expr))
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::IntegerLiteralConvertible);

  if (isa<FloatLiteralExpr>(expr))
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::FloatLiteralConvertible);

  if (isa<BooleanLiteralExpr>(expr))
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::BooleanLiteralConvertible);

  if (isa<CharacterLiteralExpr>(expr))
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::CharacterLiteralConvertible);

  if (const auto *SLE = dyn_cast<StringLiteralExpr>(expr)) {
    if (SLE->isSingleUnicodeScalar())
      return getProtocol(
          expr->getLoc(),
          KnownProtocolKind::UnicodeScalarLiteralConvertible);

    if (SLE->isSingleExtendedGraphemeCluster())
      return getProtocol(
          expr->getLoc(),
          KnownProtocolKind::ExtendedGraphemeClusterLiteralConvertible);

    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::StringLiteralConvertible);
  }

  if (isa<InterpolatedStringLiteralExpr>(expr))
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::StringInterpolationConvertible);

  if (auto E = dyn_cast<MagicIdentifierLiteralExpr>(expr)) {
    switch (E->getKind()) {
    case MagicIdentifierLiteralExpr::File:
    case MagicIdentifierLiteralExpr::Function:
      return getProtocol(expr->getLoc(),
                         KnownProtocolKind::StringLiteralConvertible);

    case MagicIdentifierLiteralExpr::Line:
    case MagicIdentifierLiteralExpr::Column:
      return getProtocol(expr->getLoc(),
                         KnownProtocolKind::IntegerLiteralConvertible);

    case MagicIdentifierLiteralExpr::DSOHandle:
      return nullptr;
    }
  }
  
  return nullptr;
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
        diagnose(SourceLoc(), diag::bool_type_broken);
        return Type();
      }

      auto tyDecl = dyn_cast<TypeDecl>(results.front());
      if (!tyDecl) {
        diagnose(SourceLoc(), diag::bool_type_broken);
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

  auto dc = ED->getDeclContext();

  // Local function that invalidates all components of the extension.
  auto invalidateAllComponents = [&] {
    for (auto &ref : ED->getRefComponents()) {
      ref.IdentType.setInvalidType(TC.Context);
    }
  };

  // Synthesize a type representation for the extended type.
  SmallVector<ComponentIdentTypeRepr *, 2> components;
  for (auto &ref : ED->getRefComponents()) {
    auto *TyR = cast<SimpleIdentTypeRepr>(ref.IdentType.getTypeRepr());
    // A reference to ".Type" is an attempt to extend the metatype.
    if (TyR->getIdentifier() == TC.Context.Id_Type && !components.empty()) {
      TC.diagnose(TyR->getIdLoc(), diag::extension_metatype);
      ED->setInvalid();
      ED->setExtendedType(ErrorType::get(TC.Context));
      invalidateAllComponents();
      return;
    }

    components.push_back(TyR);
  }

  // Validate the representation.
  TypeLoc typeLoc(IdentTypeRepr::create(TC.Context, components));
  if (TC.validateType(typeLoc, dc, TR_AllowUnboundGenerics)) {
    ED->setInvalid();
    ED->setExtendedType(ErrorType::get(TC.Context));
    invalidateAllComponents();
    return;
  }

  // Check the generic parameter lists for each of the components.
  GenericParamList *outerGenericParams = nullptr;
  for (unsigned i = 0, n = components.size(); i != n; ++i) {
    // Find the type declaration to which the identifier type actually referred.
    auto ident = components[i];
    NominalTypeDecl *typeDecl = nullptr;
    if (auto type = ident->getBoundType()) {
      if (auto unbound = dyn_cast<UnboundGenericType>(type.getPointer()))
        typeDecl = unbound->getDecl();
      else if (auto nominal = dyn_cast<NominalType>(type.getPointer()))
        typeDecl = nominal->getDecl();
    } else if (auto decl = ident->getBoundDecl()) {
      typeDecl = dyn_cast<NominalTypeDecl>(decl);
    }

    // FIXME: There are more restrictions on what we can refer to, e.g.,
    // we can't look through a typealias to a bound generic type of any form.

    // We aren't referring to a type declaration, so make sure we don't have
    // generic arguments.
    auto &ref = ED->getRefComponents()[i];
    auto *tyR = cast<SimpleIdentTypeRepr>(ref.IdentType.getTypeRepr());
    ref.IdentType.setType(ident->getBoundType());

    if (!typeDecl) {
      // FIXME: This diagnostic is awful. It should point at what we did find,
      // e.g., a type, module, etc.
      if (ref.GenericParams) {
        TC.diagnose(tyR->getIdLoc(),
                    diag::extension_generic_params_for_non_generic,
                    tyR->getIdentifier());
        ref.GenericParams = nullptr;
      }
      continue;
    }

    // The extended type is generic but the extension does not have generic
    // parameters.
    if (typeDecl->getGenericParams() && !ref.GenericParams) {
      if (auto proto = dyn_cast<ProtocolDecl>(typeDecl)) {
        // For a protocol extension, build the generic parameter list.
        ref.GenericParams = proto->createGenericParams(ED);
      } else {
        // Clone the existing generic parameter list.
        ref.GenericParams = cloneGenericParams(TC.Context, ED,
                                               typeDecl->getGenericParams(),
                                               outerGenericParams);
      }

      outerGenericParams = ref.GenericParams;
      continue;
    }

    // The extended type is non-generic but the extension has generic
    // parameters. Complain and drop them.
    if (!typeDecl->getGenericParams() && ref.GenericParams) {
      TC.diagnose(tyR->getIdLoc(),
                  diag::extension_generic_params_for_non_generic_type,
                  typeDecl->getDeclaredType())
        .highlight(ref.GenericParams->getSourceRange());
      TC.diagnose(typeDecl, diag::extended_type_here,
                  typeDecl->getDeclaredType());
      ref.GenericParams = nullptr;
      continue;
    }

    // If neither has generic parameters, we're done.
    if (!ref.GenericParams)
      continue;

    // Both have generic parameters: check that we have the right number of
    // parameters. Semantic checks will wait for extension validation.
    if (ref.GenericParams->size() != typeDecl->getGenericParams()->size()) {
      unsigned numHave = ref.GenericParams->size();
      unsigned numExpected = typeDecl->getGenericParams()->size();
      TC.diagnose(tyR->getIdLoc(),
                  diag::extension_generic_wrong_number_of_parameters,
                  typeDecl->getDeclaredType(), numHave > numExpected,
                  numHave, numExpected)
        .highlight(ref.GenericParams->getSourceRange());
      ED->setInvalid();
      ED->setExtendedType(ErrorType::get(TC.Context));
      return;
    }

    // Chain the generic parameters together.
    ref.GenericParams->setOuterParameters(outerGenericParams);
    outerGenericParams = ref.GenericParams;
  }

  // Check whether we extended something that is not a nominal type.
  Type extendedTy = typeLoc.getType();
  if (!extendedTy->is<NominalType>() && !extendedTy->is<UnboundGenericType>()) {
    TC.diagnose(ED, diag::non_nominal_extension, extendedTy);
    ED->setInvalid();
    ED->setExtendedType(ErrorType::get(TC.Context));
    invalidateAllComponents();
    return;
  }

  // If we have a trailing where clause, deal with it now.
  // For now, trailing where clauses are only permitted on protocol extensions.
  if (auto trailingWhereClause = ED->getTrailingWhereClause()) {
    if (!extendedTy->is<ProtocolType>()) {
      // Only protocol types are permitted to have trailing where clauses.
      TC.diagnose(ED, diag::extension_nonprotocol_trailing_where, extendedTy)
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

  ED->setExtendedType(extendedTy);
  if (auto nominal = extendedTy->getAnyNominal())
    nominal->addExtension(ED);
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
        PrettyStackTraceDecl StackEntry("type-checking", AFD);
        TC.typeCheckAbstractFunctionBody(AFD);
        continue;
      }
      if (isa<NominalTypeDecl>(decl)) {
        TC.handleExternalDecl(decl);
        continue;
      }
      llvm_unreachable("Unhandled external definition kind");
    }

    // Validate the contents of any referenced nominal types for SIL's purposes.
    // Note: if we ever start putting extension members in vtables, we'll need
    // to validate those members too.
    // FIXME: If we're not planning to run SILGen, this is wasted effort.
    while (!TC.ValidatedTypes.empty()) {
      auto nominal = TC.ValidatedTypes.pop_back_val();

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

    TC.definedFunctions.insert(TC.definedFunctions.end(),
                               TC.implicitlyDefinedFunctions.begin(),
                               TC.implicitlyDefinedFunctions.end());
    TC.implicitlyDefinedFunctions.clear();

  } while (currentFunctionIdx < TC.definedFunctions.size() ||
           currentExternalDef < TC.Context.ExternalDefinitions.size());

  // FIXME: Horrible hack. Store this somewhere more sane.
  TC.Context.LastCheckedExternalDefinition = currentExternalDef;

  // Compute captures for functions and closures we visited.
  for (AnyFunctionRef closure : TC.ClosuresWithUncomputedCaptures) {
    TC.computeCaptures(closure);
  }
  for (AbstractFunctionDecl *FD : reversed(TC.definedFunctions)) {
    TC.computeCaptures(FD);
  }

  // Check all of the local function captures. One can only capture a local
  // function that itself has no captures.
  for (const auto &localFunctionCapture : TC.LocalFunctionCaptures) {
    SmallVector<CapturedValue, 2> localCaptures;
    localFunctionCapture.LocalFunction->getLocalCaptures(localCaptures);
    for (const auto capture : localCaptures) {
      // The presence of any variable indicates a capture; we're (intentionally)
      // skipping over functions because any local functions that cannot be
      // captured will be diagnosed by the outer loop, and we don't need to
      // let the diagnostic cascade.
      if (isa<VarDecl>(capture.getDecl())) {
        TC.diagnose(localFunctionCapture.CaptureLoc,
                    diag::unsupported_local_function_reference);
        break;
      }
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
                                unsigned StartElem) {
  if (SF.ASTStage == SourceFile::TypeChecked)
    return;

  // Make sure that name binding has been completed before doing any type
  // checking.
  performNameBinding(SF, StartElem);

  auto &Ctx = SF.getASTContext();
  TypeChecker TC(Ctx);
  auto &DefinedFunctions = TC.definedFunctions;
  if (Options.contains(TypeCheckingFlags::DebugTimeFunctionBodies))
    TC.enableDebugTimeFunctionBodies();
  
  // Lookup the swift module.  This ensures that we record all known protocols
  // in the AST.
  (void) TC.getStdlibModule(&SF);

  if (!Ctx.LangOpts.DisableAvailabilityChecking ) {
    // Build the type refinement hierarchy for the primary
    // file before type checking.
    TypeChecker::buildTypeRefinementContextHierarchy(SF, StartElem);
  }

  // Resolve extensions. This has to occur first during type checking,
  // because the extensions need to be wired into the AST for name lookup
  // to work.
  // FIXME: We can have interesting ordering dependencies among the various
  // extensions, so we'll need to be smarter here.
  // FIXME: The current source file needs to be handled specially, because of
  // private extensions.
  bool ImportsFoundationModule = false;
  auto FoundationModuleName = Ctx.getIdentifier("Foundation");
  SF.forAllVisibleModules([&](Module::ImportedModule import) {
    if (import.second->getName() == FoundationModuleName)
      ImportsFoundationModule = true;

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
  
  DefinedFunctions.insert(DefinedFunctions.end(),
                          TC.implicitlyDefinedFunctions.begin(),
                          TC.implicitlyDefinedFunctions.end());
  TC.implicitlyDefinedFunctions.clear();

  // If we're in REPL mode, inject temporary result variables and other stuff
  // that the REPL needs to synthesize.
  if (SF.Kind == SourceFileKind::REPL && !TC.Context.hadError())
    TC.processREPLTopLevel(SF, TLC, StartElem);

  typeCheckFunctionsAndExternalDecls(TC);

  // Checking that benefits from having the whole module available.
  if (!(Options & TypeCheckingFlags::DelayWholeModuleChecking)) {
    // Diagnose conflicts and unintended overrides between
    // Objective-C methods.
    Ctx.diagnoseObjCMethodConflicts(SF);
    Ctx.diagnoseObjCUnsatisfiedOptReqConflicts(SF);
    Ctx.diagnoseUnintendedObjCMethodOverrides(SF);
  }

  // Verify that we've checked types correctly.
  SF.ASTStage = SourceFile::TypeChecked;

  // Emit an error if there is a declaration with the @objc attribute
  // but we have not imported the Foundation module.
  if (!ImportsFoundationModule && StartElem == 0 &&
      Ctx.LangOpts.EnableObjCAttrRequiresFoundation &&
      SF.FirstObjCAttr && SF.Kind != SourceFileKind::SIL) {
    SourceLoc L = SF.FirstObjCAttr->getLocation();
    Ctx.Diags.diagnose(L, diag::attr_used_without_required_module,
                       SF.FirstObjCAttr, FoundationModuleName)
      .highlight(SF.FirstObjCAttr->getRangeWithAt());
  }

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

void swift::performWholeModuleTypeChecking(SourceFile &SF) {
  auto &Ctx = SF.getASTContext();
  Ctx.diagnoseObjCMethodConflicts(SF);
  Ctx.diagnoseObjCUnsatisfiedOptReqConflicts(SF);
  Ctx.diagnoseUnintendedObjCMethodOverrides(SF);
}

bool swift::performTypeLocChecking(ASTContext &Ctx, TypeLoc &T,
                                   bool isSILType, DeclContext *DC,
                                   bool ProduceDiagnostics) {
  TypeResolutionOptions options;
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
/// We pass in a vector of nested GenericParamLists and a vector of
/// ArchetypeBuilders with the innermost GenericParamList in the beginning
/// of the vector.
bool swift::handleSILGenericParams(ASTContext &Ctx,
              SmallVectorImpl<GenericParamList *> &gps,
              DeclContext *DC,
              SmallVectorImpl<ArchetypeBuilder *> &builders) {
  return TypeChecker(Ctx).handleSILGenericParams(builders, gps, DC);
}

bool swift::typeCheckCompletionDecl(Decl *D) {
  auto &Ctx = D->getASTContext();

  // Set up a diagnostics engine that swallows diagnostics.
  DiagnosticEngine Diags(Ctx.SourceMgr);
  TypeChecker TC(Ctx, Diags);

  TC.typeCheckDecl(D, true);
  return true;
}

bool swift::typeCheckCompletionContextExpr(ASTContext &Ctx, DeclContext *DC,
                                           Expr *&parsedExpr) {
  // Set up a diagnostics engine that swallows diagnostics.
  DiagnosticEngine diags(Ctx.SourceMgr);

  TypeChecker TC(Ctx, diags);
  TC.typeCheckExpression(parsedExpr, DC, Type(), Type(), /*discardedExpr=*/true,
                         FreeTypeVariableBinding::GenericParameters);
  
  return parsedExpr && !isa<ErrorExpr>(parsedExpr)
                    && parsedExpr->getType()
                    && !parsedExpr->getType()->is<ErrorType>();
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
  diagnose(nameLoc, diag::ambiguous_member_type, name, baseTy)
    .highlight(baseRange);
  for (const auto &member : lookup) {
    diagnose(member.first, diag::found_candidate_type,
             member.second);
  }
}

Optional<VersionRange> TypeChecker::annotatedAvailableRange(const Decl *D,
                                                            ASTContext &Ctx) {
  Optional<VersionRange> AnnotatedRange;

  for (auto Attr : D->getAttrs()) {
    auto *AvailAttr = dyn_cast<AvailabilityAttr>(Attr);
    if (AvailAttr == NULL || !AvailAttr->Introduced.hasValue() ||
        !AvailAttr->isActivePlatform(Ctx)) {
      continue;
    }

    VersionRange AttrRange =
        VersionRange::allGTE(AvailAttr->Introduced.getValue());

    // If we have multiple introduction versions, we will conservatively
    // assume the worst case scenario. We may want to be more precise here
    // in the future or emit a diagnostic.

    if (AnnotatedRange.hasValue()) {
      AnnotatedRange.getValue().meetWith(AttrRange);
    } else {
      AnnotatedRange = AttrRange;
    }
  }

  return AnnotatedRange;
}

VersionRange TypeChecker::availableRange(const Decl *D, ASTContext &Ctx) {
  Optional<VersionRange> AnnotatedRange = annotatedAvailableRange(D, Ctx);
  if (AnnotatedRange.hasValue()) {
    return AnnotatedRange.getValue();
  }

  // Unlike other declarations, extensions can be used without referring to them
  // by name (they don't have one) in the source. For this reason, when checking
  // the available range of a declaration we also need to check to see if it is
  // immediately contained in an extension and use the extension's availability
  // if the declaration does not have an explicit @availability attribute
  // itself. This check relies on the fact that we cannot have nested
  // extensions.

  DeclContext *DC = D->getDeclContext();
  if (auto *ED = dyn_cast<ExtensionDecl>(DC)) {
    AnnotatedRange = annotatedAvailableRange(ED, Ctx);
    if (AnnotatedRange.hasValue()) {
      return AnnotatedRange.getValue();
    }
  }

  // Treat unannotated declarations as always available.
  return VersionRange::all();
}

/// Returns the first availability attribute on the declaration that is active
/// on the target platform.
static const AvailabilityAttr *getActiveAvailabilityAttribute(const Decl *D,
                                                              ASTContext &AC) {
  for (auto Attr : D->getAttrs())
    if (auto AvAttr = dyn_cast<AvailabilityAttr>(Attr)) {
      if (!AvAttr->isInvalid() && AvAttr->isActivePlatform(AC)) {
        return AvAttr;
      }
    }
  return nullptr;
}

/// Returns true if there is any availability attribute on the declaration
/// that is active on the target platform.
static bool hasActiveAvailabilityAttribute(Decl *D,
                                           ASTContext &AC) {
  return getActiveAvailabilityAttribute(D, AC);
}

namespace {

/// A class to walk the AST to build the type refinement context hierarchy.
class TypeRefinementContextBuilder : private ASTWalker {
  std::vector<TypeRefinementContext *> ContextStack;
  ASTContext &AC;
  
  /// A mapping from abstract storage declarations with accessors to
  /// to the type refinement contexts for those declarations. We refer to
  /// this map to determine the appopriate parent TRC to use when
  /// walking the accessor function.
  llvm::DenseMap<AbstractStorageDecl *, TypeRefinementContext *>
      StorageContexts;

  TypeRefinementContext *getCurrentTRC() {
    assert(ContextStack.size() > 0);
    return ContextStack[ContextStack.size() - 1];
  }
  
public:
  TypeRefinementContextBuilder(TypeRefinementContext *TRC, ASTContext &AC)
      : AC(AC) {
    assert(TRC);
    ContextStack.push_back(TRC);
  }

  void build(Decl *D) { D->walk(*this); }
  void build(Stmt *S) { S->walk(*this); }
  void build(Expr *E) { E->walk(*this); }

private:
  virtual bool walkToDeclPre(Decl *D) override {
    TypeRefinementContext *DeclTRC = getContextForWalkOfDecl(D);
    ContextStack.push_back(DeclTRC);
    return true;
  }
  
  virtual bool walkToDeclPost(Decl *D) override {
    assert(ContextStack.size() > 0);
    ContextStack.pop_back();
    return true;
  }
  
  TypeRefinementContext *getContextForWalkOfDecl(Decl *D) {
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
    
    TypeRefinementContext *NewTRC = nullptr;
    if (declarationIntroducesNewContext(D)) {
      NewTRC = buildDeclarationRefinementContext(D);
    } else {
      NewTRC = getCurrentTRC();
    }
    
    return NewTRC;
  }

  /// Builds the type refinement hierarchy for the body of the function.
  TypeRefinementContext *buildDeclarationRefinementContext(Decl *D) {
    // We require a valid range in order to be able to query for the TRC
    // corresponding to a given SourceLoc.
    assert(D->getSourceRange().isValid());
    
    // The potential versions in the declaration are constrained by both
    // the declared availability of the declaration and the potential versions
    // of its lexical context.
    VersionRange DeclVersionRange = TypeChecker::availableRange(D, AC);
    DeclVersionRange.meetWith(getCurrentTRC()->getPotentialVersions());
    
    TypeRefinementContext *NewTRC =
        TypeRefinementContext::createForDecl(AC, D, getCurrentTRC(),
                                             DeclVersionRange,
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
    if (!hasActiveAvailabilityAttribute(D, AC)) {
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
    
    if (auto *funcDecl = dyn_cast<AbstractFunctionDecl>(D)) {
      return funcDecl->getBodyKind() != AbstractFunctionDecl::BodyKind::None;
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
      if (storageDecl->hasAccessorFunctions()) {
        return SourceRange(storageDecl->getStartLoc(),
                           storageDecl->getBracesRange().End);
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
    auto IS = dyn_cast<IfStmt>(S);
    if (!IS)
      return std::make_pair(true, S);

    bool BuiltTRC = buildIfStmtRefinementContext(IS);
    return std::make_pair(!BuiltTRC, S);
  }

  /// Builds the type refinement hierarchy for the IfStmt if the guard
  /// introduces a new refinement context for either the Then or the Else
  /// branch. Returns true if the statement introduced a new hierarchy. In this
  /// case, there is no need for the caller to explicitly traverse the children
  /// of this node.
  bool buildIfStmtRefinementContext(IfStmt *IS) {
    // We don't refine for if let.
    // FIXME: Should this refine for 'where' clauses?
    if (IS->getCond().size() != 1 ||
        !IS->getCond()[0].isCondition())
      return false;
    
    auto CondExpr = IS->getCond()[0].getCondition();
    if (!CondExpr)
      return false;

    // For now, we only refine if the guard is an availability query expression.
    auto QueryExpr =
        dyn_cast<AvailabilityQueryExpr>(CondExpr->getSemanticsProvidingExpr());
    if (!QueryExpr)
      return false;

    // If this query expression has no queries, we will not introduce a new
    // refinement context. We do not diagnose here: a diagnostic will already
    // have been emitted by the parser.
    if (QueryExpr->getQueries().size() == 0)
      return false;
    
    validateAvailabilityQuery(QueryExpr);

    // There is no need to traverse the guard condition explicitly
    // in the current context because AvailabilityQueryExprs do not have
    // sub expressions.

    // Create a new context for the Then branch and traverse it in that new
    // context.
    auto *ThenTRC = refinedThenContextForQuery(QueryExpr, IS);
    TypeRefinementContextBuilder(ThenTRC, AC).build(IS->getThenStmt());

    if (IS->getElseStmt()) {
      // For now, we imprecisely do not refine the context for the Else branch
      // and instead traverse it in the current context.
      // Once we add a more precise version range lattice (i.e., one that can
      // support "<") we should create a TRC for the Else branch.
      build(IS->getElseStmt());
    }

    return true;
  }

  /// Validate the availability query, emitting diagnostics if necessary.
  void validateAvailabilityQuery(AvailabilityQueryExpr *E) {
    llvm::SmallSet<PlatformKind, 2> Platforms;
    bool HasOtherPlatformSpec = false;
    for (auto *Spec : E->getQueries()) {
      if (isa<OtherPlatformAvailabilitySpec>(Spec)) {
        HasOtherPlatformSpec = true;
        continue;
      }

      auto *VersionSpec = cast<VersionConstraintAvailabilitySpec>(Spec);
      bool Inserted = Platforms.insert(VersionSpec->getPlatform()).second;
      if (!Inserted) {
        // Rule out multiple version specs referring to the same platform.
        // For example, we emit an error for
        /// #available(OSX >= 10.10, OSX >= 10.11, *)
        PlatformKind Platform = VersionSpec->getPlatform();
        AC.Diags.diagnose(VersionSpec->getPlatformLoc(),
                          diag::availability_query_repeated_platform,
                          platformString(Platform));
      }
    }

    if (!HasOtherPlatformSpec) {
      ArrayRef<AvailabilitySpec *> Queries = E->getQueries();
      assert(Queries.size() > 0);
      AvailabilitySpec *LastQuery = Queries[Queries.size() - 1];
      SourceLoc InsertWildcardLoc = LastQuery->getSourceRange().End;
      AC.Diags.diagnose(E->getLoc(), diag::availability_query_wildcard_required)
          .fixItInsertAfter(InsertWildcardLoc, ", *");
    }
  }

  /// Return the type refinement context for the Then branch of an
  /// availability query.
  TypeRefinementContext *refinedThenContextForQuery(AvailabilityQueryExpr *E,
                                                    IfStmt *IS) {
    TypeRefinementContext *CurTRC = getCurrentTRC();
    
    AvailabilitySpec *Spec = bestActiveSpecForQuery(E);
    if (!Spec) {
      // We couldn't find an appropriate spec for the current platform,
      // so rather than refining, emit a diagnostic and just use the current
      // TRC.
      AC.Diags.diagnose(E->getLoc(),
                        diag::availability_query_required_for_platform,
                        platformString(targetPlatform(AC.LangOpts)));
      return CurTRC;
    }

    VersionRange range = rangeForSpec(Spec);
    E->setAvailableRange(range);
    
    // If the version range for the current TRC is completely contained in
    // the range for the spec, then a version query can never be false, so the
    // spec is useless. If so, report this.
    if (Spec->getKind() == AvailabilitySpecKind::VersionConstraint &&
        CurTRC->getPotentialVersions().isContainedIn(range)) {
      DiagnosticEngine &Diags = AC.Diags;
      if (CurTRC->getReason() == TypeRefinementContext::Reason::Root) {
        Diags.diagnose(E->getLoc(),
                       diag::availability_query_useless_min_deployment,
                       platformString(targetPlatform(AC.LangOpts)));
      } else {
        Diags.diagnose(E->getLoc(),
                       diag::availability_query_useless_enclosing_scope,
                       platformString(targetPlatform(AC.LangOpts)));
        Diags.diagnose(CurTRC->getIntroductionLoc(),
                       diag::availability_query_useless_enclosing_scope_here);
        
      }
    }
    
    return TypeRefinementContext::createForIfStmtThen(AC, IS, getCurrentTRC(),
                                                      range);
  }

  /// Return the best active spec for the target platform or nullptr if no
  /// such spec exists.
  AvailabilitySpec *bestActiveSpecForQuery(AvailabilityQueryExpr *E) {
    OtherPlatformAvailabilitySpec *FoundOtherSpec = nullptr;
    for (auto *Spec : E->getQueries()) {
      if (auto *OtherSpec = dyn_cast<OtherPlatformAvailabilitySpec>(Spec)) {
        FoundOtherSpec = OtherSpec;
        continue;
      }

      auto *VersionSpec = dyn_cast<VersionConstraintAvailabilitySpec>(Spec);
      if (!VersionSpec)
        continue;

      // FIXME: This is not quite right: we want to handle AppExtensions
      // properly. For example, on the OSXApplicationExtension platform
      // we want to chose the OSX spec unless there is an explicit
      // OSXApplicationExtension spec.
      if (isPlatformActive(VersionSpec->getPlatform(), AC.LangOpts)) {
        return VersionSpec;
      }
    }

    // If we have reached this point, we found no spec for our target, so
    // we return the other spec ('*'), if we found it, or nullptr, if not.
    return FoundOtherSpec;
  }

  /// Return the version range for the given availability spec.
  VersionRange rangeForSpec(AvailabilitySpec *Spec) {
    if (isa<OtherPlatformAvailabilitySpec>(Spec)) {
      return VersionRange::allGTE(AC.LangOpts.getMinPlatformVersion());
    }

    auto *VersionSpec = cast<VersionConstraintAvailabilitySpec>(Spec);
    switch (VersionSpec->getComparison()) {
    case VersionComparison::GreaterThanEqual:
      return VersionRange::allGTE(VersionSpec->getVersion());
    }
  }
  
  virtual std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    auto QueryExpr = dyn_cast<AvailabilityQueryExpr>(E);
    if (!QueryExpr)
      return std::make_pair(true, E);
    
    // If we have gotten here, it means we encountered #available(...)
    // in a context other than if #available(...) { }, so we emit an error.
    // We may want to loosen this restriction in the future (to, e.g., IfExprs)
    // -- but, in general, we don't want #available() to appear where static
    // analysis cannot easily determine its effect.
    AC.Diags.diagnose(E->getLoc(),
                      diag::availability_query_outside_if_stmt_guard);
    
    return std::make_pair(false, E);
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
    auto VersionRange =
        VersionRange::allGTE(AC.LangOpts.getMinPlatformVersion());
    RootTRC = TypeRefinementContext::createRoot(&SF, VersionRange);
    SF.setTypeRefinementContext(RootTRC);
  }

  // Build refinement contexts, if necessary, for all declarations starting
  // with StartElem.
  TypeRefinementContextBuilder Builder(RootTRC, AC);
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

/// Returns an over-approximation of the range of operating system versions
/// that could  the passed-in location location could be executing upon for
/// the target platform.
static VersionRange overApproximateOSVersionsAtLocation(SourceLoc loc,
                                                        const DeclContext *DC,
                                                        TypeChecker &TC) {
  SourceFile *SF = DC->getParentSourceFile();
  assert(SF);

  // If our source location is invalid (this may be synthesized code), climb
  // the decl context hierarchy until until we find a location that is valid,
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

  VersionRange OverApproximateVersionRange = VersionRange::all();

  while (DC && loc.isInvalid()) {
    const Decl *D = DC->getInnermostDeclarationDeclContext();
    if (!D)
      break;

    loc = D->getLoc();

    Optional<VersionRange> Range =
        TypeChecker::annotatedAvailableRange(D, TC.Context);

    if (Range.hasValue()) {
      // We're relying on a precise meet here to be over-approximate.
      // This should really be a constrain operation.
      OverApproximateVersionRange.meetWith(Range.getValue());
    }

    DC = D->getDeclContext();
  }

  TypeRefinementContext *rootTRC = TC.getOrBuildTypeRefinementContext(SF);
  TypeRefinementContext *TRC;
  if (loc.isValid()) {
    TRC = rootTRC->findMostRefinedSubContext(loc, TC.Context.SourceMgr);
  } else {
    // If we could not find a valid location, conservatively use the root
    // refinement context.
    TRC = rootTRC;
  }

  // Again, we're relying on a precise meet.
  OverApproximateVersionRange.meetWith(TRC->getPotentialVersions());
  return OverApproximateVersionRange;
}

bool TypeChecker::isDeclAvailable(const Decl *D, SourceLoc referenceLoc,
                                  const DeclContext *referenceDC,
                                  VersionRange &OutAvailableRange) {

  VersionRange safeRangeUnderApprox = TypeChecker::availableRange(D, Context);
  VersionRange runningOSOverApprox = overApproximateOSVersionsAtLocation(
      referenceLoc, referenceDC, *this);
  
  // The reference is safe if an over-approximation of the running OS
  // versions is fully contained within an under-approximation
  // of the versions on which the declaration is available. If this
  // containment cannot be guaranteed, we say the reference is
  // not available.
  if (!(runningOSOverApprox.isContainedIn(safeRangeUnderApprox))) {
    OutAvailableRange = safeRangeUnderApprox;
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

  VersionRange safeRangeUnderApprox = VersionRange::empty();
  if (isDeclAvailable(D, referenceLoc, referenceDC, safeRangeUnderApprox)) {
    return None;
  }

  // safeRangeUnderApprox now holds the safe range.
  return UnavailabilityReason::requiresVersionRange(safeRangeUnderApprox);
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
/// the Post-visitors. It does its best to not search unnecessary subtrees,
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
  /// matching our criteria on the way back up the spine of of the tree.
  bool walkToNodePost(ASTNode Node) {
    if (!InnermostMatchingNode.hasValue() && Predicate(Node, Parent)) {
      assert((Node.getSourceRange().isInvalid() && FoundTarget) ||
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
concreteSyntaxDeclForAvailabilityAttribute(const Decl *AbstractSyntaxDecl) {
  // This function needs to be kept in sync with its counterpart,
  // abstractSyntaxDeclForAvailabilityAttribute().

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
abstractSyntaxDeclForAvailabilityAttribute(const Decl *ConcreteSyntaxDecl) {
  // This function needs to be kept in sync with its counterpart,
  // concreteSyntaxDeclForAvailabilityAttribute().

  if (auto *PBD = dyn_cast<PatternBindingDecl>(ConcreteSyntaxDecl)) {
    // Existing @availability attributes in the AST are attached to VarDecls
    // rather than PatternBindingDecls, so we return the first VarDecl for
    // the pattern binding declaration.
    // This is safe, even though there may be multiple VarDecls, because
    // all parsed attribute that appear in the concrete syntax upon on the
    // PatternBindingDecl are added to all of the VarDecls for the pattern
    // binding.
    ArrayRef<PatternBindingEntry> Entries = PBD->getPatternList();
    if (Entries.size() > 0) {
      VarDecl *VD = Entries.front().ThePattern->getSingleVar();
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
/// to suggest an @availability fixit, or the original declaration
/// if no such related declaration exists.
static const Decl *relatedDeclForAvailabilityFixit(const Decl *D) {
  if (auto *FD = dyn_cast<FuncDecl>(D)) {
    // Suggest @availability Fix-Its on property rather than individual
    // accessors.
    if (FD->isAccessor()) {
      D = FD->getAccessorStorageDecl();
    }
  }

  return abstractSyntaxDeclForAvailabilityAttribute(D);
}

/// Walk the DeclContext hierarchy starting from D to find a declaration
/// at the member level (i.e., declared in a type context) on which to provide
/// an @availability() Fix-It.
static const Decl *ancestorMemberLevelDeclForAvailabilityFixit(const Decl *D) {
  while (D) {
    D = relatedDeclForAvailabilityFixit(D);

    if (D->getDeclContext()->isTypeContext() &&
        DeclAttribute::canAttributeAppearOnDecl(DeclAttrKind::DAK_Availability,
                                                D)) {
      break;
    }

    D = cast_or_null<AbstractFunctionDecl>(
        D->getDeclContext()->getInnermostMethodContext());
  }

  return D;
}

/// Returns true if the declaration is at the type level (either a nominal
/// type, an extension, or a global function) and can support an @availability
/// attribute.
static bool isTypeLevelDeclForAvailabilityFixit(const Decl *D) {
  if (!DeclAttribute::canAttributeAppearOnDecl(DeclAttrKind::DAK_Availability,
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
/// @availability() Fix-It.
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
/// \param  FoundVersionCheckNode Returns a node that can be wrapped in a
/// if #available(...) { ... } version check to fix the unavailable reference,
/// or None if such such a node cannot be found.
///
/// \param FoundMemberLevelDecl Returns memember-level declaration (i.e., the
///  child of a type DeclContext) for which an @availability attribute would
/// fix the unavailable reference.
///
/// \param FoundTypeLevelDecl returns a type-level declaration (a
/// a nominal type, an extension, or a global function) for which an
/// @availability attribute would fix the unavailable reference.
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
  // places to suggest adding @availability() annotations.
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

  // Try to find declarations on which @availability attributes can be added.
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

/// Emit a diagnostic note and Fix-It to add an @availability attribute
/// on the given declaration for the given version range.
static void fixAvailabilityForDecl(SourceRange ReferenceRange, const Decl *D,
                                   const VersionRange &RequiredRange,
                                   TypeChecker &TC) {
  assert(D);

  if (getActiveAvailabilityAttribute(D, TC.Context)) {
    // For QoI, in future should emit a fixit to update the existing attribute.
    return;
  }

  // For some declarations (variables, enum elements), the location in concrete
  // syntax to suggest the Fix-It may differ from the declaration to which
  // we attach availability attributes in the abstract syntax tree during
  // parsing.
  D = concreteSyntaxDeclForAvailabilityAttribute(D);

  SourceLoc InsertLoc = D->getAttrs().getStartLoc(/*forModifiers=*/false);
  if (InsertLoc.isInvalid()) {
    InsertLoc = D->getStartLoc();
  }

  if (InsertLoc.isInvalid())
    return;

  StringRef OriginalIndent =
      Lexer::getIndentationForLine(TC.Context.SourceMgr, InsertLoc);

  std::string AttrText;
  {
    llvm::raw_string_ostream Out(AttrText);

    PlatformKind Target = targetPlatform(TC.getLangOpts());
    Out << "@availability(" << platformString(Target)
        << ", introduced=" << RequiredRange.getLowerEndpoint().getAsString()
        << ")\n" << OriginalIndent;
  }

  DescriptiveDeclKind KindForDiagnostic = D->getDescriptiveKind();

  // To avoid exposing the pattern binding declaration to the user, get the
  // descriptive kind from one of the VarDecls while still using the Fix-It
  // location from the PatternBindingDecl.
  if (KindForDiagnostic == DescriptiveDeclKind::PatternBinding) {
    KindForDiagnostic =
        abstractSyntaxDeclForAvailabilityAttribute(D)->getDescriptiveKind();
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
        << " >= " << RequiredRange.getLowerEndpoint().getAsString() << ") {\n";

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

const AvailabilityAttr *TypeChecker::getDeprecated(const Decl *D) {
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
    D = abstractSyntaxDeclForAvailabilityAttribute(D);
    if (Pred(D)) {
      return true;
    }
  }

  return false;
}

bool TypeChecker::isInsideImplicitFunction(SourceRange ReferenceRange,
                                           const DeclContext *DC) {
  std::function<bool(const Decl *)> IsInsideImplicitFunc = [](const Decl *D) {
    auto *AFD = dyn_cast<AbstractFunctionDecl>(D);
    return AFD && AFD->isImplicit();
  };

  return someEnclosingDeclMatches(ReferenceRange, DC, *this,
                                  IsInsideImplicitFunc);
}


bool TypeChecker::isInsideUnavailableDeclaration(
    SourceRange ReferenceRange, const DeclContext *ReferenceDC) {
  std::function<bool(const Decl *)> IsUnavailable = [](const Decl *D) {
    return D->getAttrs().getUnavailable(D->getASTContext());
  };

  return someEnclosingDeclMatches(ReferenceRange, ReferenceDC, *this,
                                  IsUnavailable);
}

/// Returns true if the reference is lexically contained in a declaration
/// that is deprecated on all deployment targets.
static bool isInsideDeprecatedDeclaration(SourceRange ReferenceRange,
                                          const DeclContext *ReferenceDC,
                                          TypeChecker &TC) {
  std::function<bool(const Decl *)> IsDeprecated = [](const Decl *D) {
    return D->getAttrs().getDeprecated(D->getASTContext());
  };

  return someEnclosingDeclMatches(ReferenceRange, ReferenceDC, TC,
                                  IsDeprecated);
}

void TypeChecker::diagnoseDeprecated(SourceRange ReferenceRange,
                                     const DeclContext *ReferenceDC,
                                     const AvailabilityAttr *Attr,
                                     DeclName Name) {
  // We match the behavior of clang to not report deprecation warnigs
  // inside declarations that are themselves deprecated on all deployment
  // targets.
  if (isInsideDeprecatedDeclaration(ReferenceRange, ReferenceDC, *this)) {
    return;
  }

  StringRef Platform = Attr->prettyPlatformString();
  clang::VersionTuple DeprecatedVersion = Attr->Deprecated.getValue();

  if (Attr->Message.empty()) {
    diagnose(ReferenceRange.Start, diag::availability_deprecated, Name,
             Platform, DeprecatedVersion).highlight(Attr->getRange());
    return;
  }

  diagnose(ReferenceRange.Start, diag::availability_deprecated_msg, Name,
           Platform, DeprecatedVersion,
           Attr->Message).highlight(Attr->getRange());
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
  checkForForbiddenPrefix(E->getName());
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
