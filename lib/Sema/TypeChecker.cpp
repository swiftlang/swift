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
#include "swift/Sema/CodeCompletionTypeChecking.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"

using namespace swift;

TypeChecker::TypeChecker(ASTContext &Ctx, DiagnosticEngine &Diags)
  : Context(Ctx), Diags(Diags)
{
  auto clangImporter =
    static_cast<ClangImporter *>(Context.getClangModuleLoader());
  clangImporter->setTypeResolver(*this);

}

TypeChecker::~TypeChecker() {
  auto clangImporter =
    static_cast<ClangImporter *>(Context.getClangModuleLoader());
  clangImporter->clearTypeResolver();
}

void TypeChecker::handleExternalDecl(Decl *decl) {
  if (auto SD = dyn_cast<StructDecl>(decl)) {\
    SmallVector<Decl*, 2> NewInits;
    addImplicitConstructors(SD, NewInits);
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
  return boolType.cache([&]{
    UnqualifiedLookup boolLookup(Context.getIdentifier("Bool"),
                                 getStdlibModule(dc), nullptr,
                                 SourceLoc(),
                                 /*IsTypeLookup=*/true);
    if (!boolLookup.isSuccess()) {
      diagnose(SourceLoc(), diag::bool_type_broken);
      return Type();
    }
    TypeDecl *tyDecl = boolLookup.getSingleTypeResult();
    
    if (!tyDecl) {
      diagnose(SourceLoc(), diag::bool_type_broken);
      return Type();
    }
    
    return tyDecl->getDeclaredType();
  });
}

static void bindExtensionDecl(ExtensionDecl *ED, TypeChecker &TC) {
  if (ED->getExtendedType())
    return;

  auto dc = ED->getDeclContext();

  // Synthesize a type representation for the extended type.
  SmallVector<ComponentIdentTypeRepr *, 2> components;
  for (auto &ref : ED->getRefComponents()) {
    // A reference to ".Type" is an attempt to extend the metatype.
    if (ref.Name == TC.Context.Id_Type && !components.empty()) {
      TC.diagnose(ref.NameLoc, diag::extension_metatype);
      ED->setInvalid();
      ED->setExtendedType(ErrorType::get(TC.Context));
      return;
    }

    components.push_back(
      new (TC.Context) SimpleIdentTypeRepr(ref.NameLoc, ref.Name));
  }

  // Validate the representation.
  TypeLoc typeLoc(IdentTypeRepr::create(TC.Context, components));
  if (TC.validateType(typeLoc, dc, TR_AllowUnboundGenerics)) {
    ED->setInvalid();
    ED->setExtendedType(ErrorType::get(TC.Context));
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
    // generic arguments.
    auto &ref = ED->getRefComponents()[i];
    if (!typeDecl) {
      // FIXME: This diagnostic is awful. It should point at what we did find,
      // e.g., a type, module, etc.
      if (ref.GenericParams) {
        TC.diagnose(ref.NameLoc, diag::extension_generic_params_for_non_generic,
                    ref.Name);
        ref.GenericParams = nullptr;
      }

      continue;
    }

    // The extended type is generic but the extension does not have generic
    // parameters.
    // FIXME: This will eventually become a Fix-It.
    if (typeDecl->getGenericParams() && !ref.GenericParams) {
      continue;
    }

    // The extended type is non-generic but the extension has generic
    // parameters. Complain and drop them.
    if (!typeDecl->getGenericParams() && ref.GenericParams) {
      TC.diagnose(ref.NameLoc,
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
      TC.diagnose(ref.NameLoc,
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
    TC.diagnose(ED, diag::non_nominal_extension, false, extendedTy);
    ED->setInvalid();
    ED->setExtendedType(ErrorType::get(TC.Context));
    return;
  }

  ED->setExtendedType(extendedTy);
  if (auto nominal = extendedTy->getAnyNominal())
    nominal->addExtension(ED);
}

/// Returns true if the given decl or extension conforms to a protocol whose
/// name matches a compiler-known protocol. This is a syntactic check; no type
/// resolution is performed.
template <typename DeclTy>
static bool mayConformToKnownProtocol(const DeclTy *D) {
  for (TypeLoc inherited : D->getInherited()) {
    auto identRepr = dyn_cast_or_null<IdentTypeRepr>(inherited.getTypeRepr());
    if (!identRepr)
      continue;

    auto lastSimpleID =
        dyn_cast<SimpleIdentTypeRepr>(identRepr->getComponentRange().back());
    if (!lastSimpleID)
      continue;

    bool matchesKnownProtocol =
      llvm::StringSwitch<bool>(lastSimpleID->getIdentifier().str())
#define PROTOCOL(Name) \
        .Case(#Name, true)
#include "swift/AST/KnownProtocols.def"
        .Default(false);
    if (matchesKnownProtocol)
      return true;
  }

  return false;
}

static void typeCheckFunctionsAndExternalDecls(TypeChecker &TC) {
  unsigned currentFunctionIdx = 0;
  unsigned currentExternalDef = TC.Context.LastCheckedExternalDefinition;
  do {
    // Type check the body of each of the function in turn.  Note that outside
    // functions must be visited before nested functions for type-checking to
    // work correctly.
    unsigned previousFunctionIdx = currentFunctionIdx;
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

    // Compute captures for functions we visited, in the opposite order of type
    // checking. i.e., the nested DefinedFunctions will be visited before the
    // outer DefinedFunctions.
    for (unsigned i = currentFunctionIdx; i > previousFunctionIdx; --i) {
      if (auto *FD = dyn_cast<AbstractFunctionDecl>(TC.definedFunctions[i-1]))
        TC.computeCaptures(FD);
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

      for (auto *D : nominal->getMembers()) {
        if (auto VD = dyn_cast<ValueDecl>(D))
          TC.validateDecl(VD);
      }

      if (auto *CD = dyn_cast<ClassDecl>(nominal)) {
        SmallVector<Decl *, 4> ignoredCtors;
        TC.addImplicitConstructors(CD, ignoredCtors);
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

  // Check all of the local function captures. One can only capture a local
  // function that itself has no captures.
  for (const auto &localFunctionCapture : TC.LocalFunctionCaptures) {
    SmallVector<CaptureInfo::LocalCaptureTy, 2> localCaptures;
    localFunctionCapture.LocalFunction->getLocalCaptures(localCaptures);
    for (const auto capture : localCaptures) {
      // The presence of any variable indicates a capture; we're (intentionally)
      // skipping over functions because any local functions that cannot be
      // captured will be diagnosed by the outer loop, and we don't need to
      // let the diagnostic cascade.
      if (isa<VarDecl>(capture.getPointer())) {
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
                                unsigned StartElem) {
  if (SF.ASTStage == SourceFile::TypeChecked)
    return;

  // Make sure that name binding has been completed before doing any type
  // checking.
  performNameBinding(SF, StartElem);

  auto &Ctx = SF.getASTContext();
  TypeChecker TC(Ctx);
  auto &DefinedFunctions = TC.definedFunctions;
  
  // Lookup the swift module.  This ensures that we record all known protocols
  // in the AST.
  (void) TC.getStdlibModule(&SF);

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
        if (auto ED = dyn_cast<ExtensionDecl>(D)) {
          bindExtensionDecl(ED, TC);
          if (mayConformToKnownProtocol(ED) &&
              ED->getExtendedType()->getAnyNominal())
            TC.validateDecl(ED->getExtendedType()->getAnyNominal());
        } else if (auto nominal = dyn_cast<NominalTypeDecl>(D)) {
          if (mayConformToKnownProtocol(nominal))
            TC.validateDecl(nominal);
        }
      }
    }
  });

  // FIXME: Check for cycles in class inheritance here?

  if (Ctx.LangOpts.EnableExperimentalAvailabilityChecking ) {
    // Build the type refinement hierarchy for the file before type checking.
    TypeChecker::buildTypeRefinementContextHierarchy(SF, StartElem);
  }
  
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

  // Verify that we've checked types correctly.
  SF.ASTStage = SourceFile::TypeChecked;

  // Emit an error if there is a declaration with the @objc attribute
  // but we have not imported the Foundation module.
  if (Ctx.LangOpts.EnableObjCAttrRequiresFoundation &&
      SF.Kind == SourceFileKind::Main &&
      StartElem == 0 &&
      SF.FirstObjCAttrLoc && !ImportsFoundationModule) {
    auto L = SF.FirstObjCAttrLoc.getValue();
    Ctx.Diags.diagnose(L, diag::objc_decl_used_without_required_module,
                       "objc", FoundationModuleName)
    .highlight(SourceRange(L));
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

VersionRange TypeChecker::availableRange(Decl *D, ASTContext &Ctx) {
  VersionRange Avail = VersionRange::all();

  for (auto Attr : D->getAttrs()) {
    AvailabilityAttr *AvailAttr = dyn_cast<AvailabilityAttr>(Attr);
    if (AvailAttr == NULL || !AvailAttr->Introduced.hasValue() ||
        !AvailAttr->isActivePlatform(Ctx)) {
      continue;
    }

    VersionRange AttrRange =
        VersionRange::allGTE(AvailAttr->Introduced.getValue());

    // If we have multiple introduction versions, we will conservatively
    // assume the worst case scenario. We may want to be more precise here
    // in the future or emit a diagnostic.
    Avail.meetWith(AttrRange);
  }

  return Avail;
}

namespace {

/// A class to walk the AST to build the type refinement context hierarchy.
class TypeRefinementContextBuilder : private ASTWalker {
  TypeRefinementContext *CurTRC;
  ASTContext &AC;

public:
  TypeRefinementContextBuilder(TypeRefinementContext *TRC, ASTContext &AC)
      : CurTRC(TRC), AC(AC) {
    assert(TRC);
  }

  void build(Decl *D) { D->walk(*this); }
  void build(Stmt *S) { S->walk(*this); }
  void build(Expr *E) { E->walk(*this); }

private:
  virtual bool walkToDeclPre(Decl *D) override {
    auto FD = dyn_cast<AbstractFunctionDecl>(D);
    if (!FD) return true;

    buildFunctionRefinementContext(FD);

    return false;
  }

  /// Builds the type refinement hierarchy for the body of the function.
  void buildFunctionRefinementContext(AbstractFunctionDecl *D) {
    if (D->getBodyKind() == AbstractFunctionDecl::BodyKind::None) {
      return;
    }

    // We only introduce a nested refinement context for the body if the
    // function has an availability attribute.
    if (!hasActiveAvailabilityAttribute(D)) {
      return;
    }

    // We require a valid range in order to be able to query for the TRC
    // corresponding to a given SourceLoc.
    assert(D->getSourceRange().isValid());
    
    // The potential versions in the body are constrained by both
    // the declared availability of the function and potential versions
    // of its lexical context.
    VersionRange BodyVersionRange = TypeChecker::availableRange(D, AC);
    BodyVersionRange.meetWith(CurTRC->getPotentialVersions());
    
    TypeRefinementContext *newTRC =
        TypeRefinementContext::createForDecl(AC, D, CurTRC, BodyVersionRange);
    
    Stmt *Body = D->getBody();
    // If there is no body it may be because we will synthesize it and
    // type check it later.
    if (Body) {
      Body->walk(TypeRefinementContextBuilder(newTRC, AC));
    }
  }

  bool hasActiveAvailabilityAttribute(Decl *D) {
    for (auto Attr : D->getAttrs())
      if (auto AvAttr = dyn_cast<AvailabilityAttr>(Attr)) {
        if (!AvAttr->isInvalid() && AvAttr->isActivePlatform(AC)) {
          return true;
        }
      }
    return false;
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
    auto CondExpr = IS->getCond().dyn_cast<Expr *>();
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

    // Traverse the guard in the current context.
    build(CondExpr);

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
    // Rule out multiple version specs referring to the same platform.
    // For example, we emit an error for #os(OSX >= 10.10, OSX >= 10.11)
    llvm::SmallSet<PlatformKind, 2> Platforms;
    for (auto *Spec : E->getQueries()) {
      bool Inserted = Platforms.insert(Spec->getPlatform());
      if (!Inserted) {
        PlatformKind Platform = Spec->getPlatform();
        AC.Diags.diagnose(Spec->getPlatformLoc(),
                          diag::availability_query_repeated_platform,
                          platformString(Platform));
      }
    }
  }

  /// Return the type refinement context for the Then branch of an
  /// availability query.
  TypeRefinementContext *refinedThenContextForQuery(AvailabilityQueryExpr *E,
                                                    IfStmt *IS) {
    VersionConstraintAvailabilitySpec *Spec = bestActiveSpecForQuery(E);
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
    
    return TypeRefinementContext::createForIfStmtThen(AC, IS, CurTRC, range);
  }

  /// Return the best active spec for the target platform or nullptr if no
  /// such spec exists.
  VersionConstraintAvailabilitySpec *
  bestActiveSpecForQuery(AvailabilityQueryExpr *E) {
    for (auto *Spec : E->getQueries()) {
      // FIXME: This is not quite right: we want to handle AppExtensions
      // properly. For example, on the OSXApplicationExtension platform
      // we want to chose the OSX spec unless there is an explicit
      // OSXApplicationExtension spec.
      if (isPlatformActive(Spec->getPlatform(), AC.LangOpts)) {
        return Spec;
      }
    }

    return nullptr;
  }

  /// Return the version range for the given availability spec.
  VersionRange rangeForSpec(VersionConstraintAvailabilitySpec *Spec) {
    switch (Spec->getComparison()) {
    case VersionComparison::GreaterThanEqual:
      return VersionRange::allGTE(Spec->getVersion());
    }
  }
};
  
}

void TypeChecker::buildTypeRefinementContextHierarchy(SourceFile &SF,
                                                      unsigned StartElem) {
  ASTContext &AC = SF.getASTContext();

  TypeRefinementContext *RootTRC = SF.getTypeRefinementContext();
  // Build refinement contexts, if necessary, for all declarations starting
  // with StartElem.
  TypeRefinementContextBuilder Builder(RootTRC, AC);
  for (auto D : llvm::makeArrayRef(SF.Decls).slice(StartElem)) {
    Builder.build(D);
  }
}

bool TypeChecker::isDeclAvailable(Decl *D, SourceLoc referenceLoc,
                                  DeclContext *referenceDC,
                                  VersionRange &OutAvailableRange) {
  SourceFile *SF = referenceDC->getParentSourceFile();
  assert(SF);
  
  TypeRefinementContext *rootTRC = SF->getTypeRefinementContext();
  
  TypeRefinementContext *TRC;
  if (referenceLoc.isValid()) {
    TRC = rootTRC->findMostRefinedSubContext(referenceLoc,
                                             Context.SourceMgr);
  } else {
    // For expressions without a valid location (these may be synthesized
    // code) we conservatively use the root refinement context. In
    // the future, we can be more precise here by climbing DeclContexts
    // until we find a DeclContext with a valid location and looking up
    // the TRC for that.
    TRC = rootTRC;
  }
  
  VersionRange safeRangeUnderApprox = TypeChecker::availableRange(D, Context);
  VersionRange runningOSOverApprox = TRC->getPotentialVersions();
  
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
