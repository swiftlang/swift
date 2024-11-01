//===--- Decl.cpp - Swift Language Decl ASTs ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Decl class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/AccessRequests.h"
#include "swift/AST/AccessScope.h"
#include "swift/AST/Attr.h"
#include "swift/AST/CaptureInfo.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/DeclExportabilityVisitor.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/MacroDefinition.h"
#include "swift/AST/MacroDiscriminatorContext.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ParseRequests.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/SwiftNameTranslation.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Basic/TypeID.h"
#include "swift/ClangImporter/ClangImporterRequests.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Demangling/ManglingMacros.h"
#include "swift/Parse/Lexer.h" // FIXME: Bad dependency
#include "clang/Lex/MacroInfo.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Basic/CharInfo.h"
#include "clang/Basic/Module.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"

#include "InlinableText.h"
#include <algorithm>

using namespace swift;

#define DEBUG_TYPE "Serialization"

STATISTIC(NumLazyRequirementSignatures,
          "# of lazily-deserialized requirement signatures known");

#undef DEBUG_TYPE

#define DECL(Id, _) \
  static_assert((DeclKind::Id == DeclKind::Module) ^ \
                IsTriviallyDestructible<Id##Decl>::value, \
                "Decls are BumpPtrAllocated; the destructor is never called");
#include "swift/AST/DeclNodes.def"
static_assert(IsTriviallyDestructible<ParameterList>::value,
              "ParameterLists are BumpPtrAllocated; the d'tor is never called");
static_assert(IsTriviallyDestructible<GenericParamList>::value,
              "GenericParamLists are BumpPtrAllocated; the d'tor isn't called");

const clang::MacroInfo *ClangNode::getAsMacro() const {
  if (auto MM = getAsModuleMacro())
    return MM->getMacroInfo();
  return getAsMacroInfo();
}

clang::SourceLocation ClangNode::getLocation() const {
  if (auto D = getAsDecl())
    return D->getLocation();
  if (auto M = getAsMacro())
    return M->getDefinitionLoc();

  return clang::SourceLocation();
}

clang::SourceRange ClangNode::getSourceRange() const {
  if (auto D = getAsDecl())
    return D->getSourceRange();
  if (auto M = getAsMacro())
    return clang::SourceRange(M->getDefinitionLoc(), M->getDefinitionEndLoc());

  return clang::SourceLocation();
}

const clang::Module *ClangNode::getClangModule() const {
  if (auto *M = getAsModule())
    return M;
  if (auto *ID = dyn_cast_or_null<clang::ImportDecl>(getAsDecl()))
    return ID->getImportedModule();
  return nullptr;
}

void ClangNode::dump() const {
  if (auto D = getAsDecl())
    D->dump();
  else if (auto M = getAsMacro())
    M->dump();
  else if (auto M = getAsModule())
    M->dump();
  else
    llvm::errs() << "ClangNode contains nullptr\n";
}

StringRef Decl::getKindName(DeclKind K) {
  switch (K) {
#define DECL(Id, Parent) case DeclKind::Id: return #Id;
#include "swift/AST/DeclNodes.def"
  }
  llvm_unreachable("bad DeclKind");
}

DescriptiveDeclKind Decl::getDescriptiveKind() const {
#define TRIVIAL_KIND(Kind)                      \
  case DeclKind::Kind:                          \
    return DescriptiveDeclKind::Kind

  switch (getKind()) {
  TRIVIAL_KIND(Import);
  TRIVIAL_KIND(Extension);
  TRIVIAL_KIND(EnumCase);
  TRIVIAL_KIND(TopLevelCode);
  TRIVIAL_KIND(PoundDiagnostic);
  TRIVIAL_KIND(PatternBinding);
  TRIVIAL_KIND(PrecedenceGroup);
  TRIVIAL_KIND(InfixOperator);
  TRIVIAL_KIND(PrefixOperator);
  TRIVIAL_KIND(PostfixOperator);
  TRIVIAL_KIND(TypeAlias);
  TRIVIAL_KIND(GenericTypeParam);
  TRIVIAL_KIND(AssociatedType);
  TRIVIAL_KIND(Protocol);
  TRIVIAL_KIND(Constructor);
  TRIVIAL_KIND(Destructor);
  TRIVIAL_KIND(EnumElement);
  TRIVIAL_KIND(Param);
  TRIVIAL_KIND(Module);
  TRIVIAL_KIND(Missing);
  TRIVIAL_KIND(MissingMember);
  TRIVIAL_KIND(Macro);
  TRIVIAL_KIND(MacroExpansion);

   case DeclKind::Enum:
     return cast<EnumDecl>(this)->getGenericParams()
              ? DescriptiveDeclKind::GenericEnum
              : DescriptiveDeclKind::Enum;

   case DeclKind::Struct:
     return cast<StructDecl>(this)->getGenericParams()
              ? DescriptiveDeclKind::GenericStruct
              : DescriptiveDeclKind::Struct;

   case DeclKind::Class: {
     auto clazz = cast<ClassDecl>(this);
     bool isAnyActor = clazz->isAnyActor();
     bool isGeneric = clazz->getGenericParams();

     auto kind = isGeneric ? DescriptiveDeclKind::GenericClass
                           : DescriptiveDeclKind::Class;

     if (isAnyActor) {
       if (clazz->isDistributedActor()) {
         kind = isGeneric ? DescriptiveDeclKind::GenericDistributedActor
                          : DescriptiveDeclKind::DistributedActor;
       } else {
         kind = isGeneric ? DescriptiveDeclKind::GenericActor
                          : DescriptiveDeclKind::Actor;
       }
     }
     return kind;
   }

   case DeclKind::Var: {
     auto var = cast<VarDecl>(this);
     switch (var->getCorrectStaticSpelling()) {
     case StaticSpellingKind::None:
       if (var->getDeclContext()->isTypeContext()) {
         if (var->isDistributed() && !var->isLet()) {
           return DescriptiveDeclKind::DistributedProperty;
         }

         return DescriptiveDeclKind::Property;
       }
       return var->isLet() ? DescriptiveDeclKind::Let
                           : DescriptiveDeclKind::Var;
     case StaticSpellingKind::KeywordStatic:
       return DescriptiveDeclKind::StaticProperty;
     case StaticSpellingKind::KeywordClass:
       return DescriptiveDeclKind::ClassProperty;
     }
   }

   case DeclKind::Subscript: {
     auto subscript = cast<SubscriptDecl>(this);
     switch (subscript->getCorrectStaticSpelling()) {
     case StaticSpellingKind::None:
       return DescriptiveDeclKind::Subscript;
     case StaticSpellingKind::KeywordStatic:
       return DescriptiveDeclKind::StaticSubscript;
     case StaticSpellingKind::KeywordClass:
       return DescriptiveDeclKind::ClassSubscript;
     }
   }

   case DeclKind::Accessor: {
     auto accessor = cast<AccessorDecl>(this);

     switch (accessor->getAccessorKind()) {
     case AccessorKind::Get:
     case AccessorKind::DistributedGet:
       return DescriptiveDeclKind::Getter;

     case AccessorKind::Set:
       return DescriptiveDeclKind::Setter;

     case AccessorKind::WillSet:
       return DescriptiveDeclKind::WillSet;

     case AccessorKind::DidSet:
       return DescriptiveDeclKind::DidSet;

     case AccessorKind::Address:
       return DescriptiveDeclKind::Addressor;

     case AccessorKind::MutableAddress:
       return DescriptiveDeclKind::MutableAddressor;

     case AccessorKind::Read:
     case AccessorKind::Read2:
       return DescriptiveDeclKind::ReadAccessor;

     case AccessorKind::Modify:
     case AccessorKind::Modify2:
       return DescriptiveDeclKind::ModifyAccessor;

     case AccessorKind::Init:
       return DescriptiveDeclKind::InitAccessor;
     }
     llvm_unreachable("bad accessor kind");
   }

   case DeclKind::Func: {
     auto func = cast<FuncDecl>(this);

     if (func->isOperator())
       return DescriptiveDeclKind::OperatorFunction;

     if (func->getDeclContext()->isLocalContext())
       return DescriptiveDeclKind::LocalFunction;

     if (func->getDeclContext()->isModuleScopeContext())
       return DescriptiveDeclKind::GlobalFunction;

     // We have a method.
     switch (func->getCorrectStaticSpelling()) {
     case StaticSpellingKind::None:
       if (func->isDistributed()) {
         return DescriptiveDeclKind::DistributedMethod;
       } else {
         return DescriptiveDeclKind::Method;
       }
     case StaticSpellingKind::KeywordStatic:
       return DescriptiveDeclKind::StaticMethod;
     case StaticSpellingKind::KeywordClass:
       return DescriptiveDeclKind::ClassMethod;
     }
   }

   case DeclKind::OpaqueType: {
     auto *opaqueTypeDecl = cast<OpaqueTypeDecl>(this);
     if (dyn_cast_or_null<VarDecl>(opaqueTypeDecl->getNamingDecl()))
       return DescriptiveDeclKind::OpaqueVarType;
     return DescriptiveDeclKind::OpaqueResultType;
   }

   case DeclKind::BuiltinTuple:
     llvm_unreachable("BuiltinTupleDecl should not end up here");
  }
#undef TRIVIAL_KIND
  llvm_unreachable("bad DescriptiveDeclKind");
}

StringRef Decl::getDescriptiveKindName(DescriptiveDeclKind K) {
#define ENTRY(Kind, String) case DescriptiveDeclKind::Kind: return String
  switch (K) {
  ENTRY(Import, "import");
  ENTRY(Extension, "extension");
  ENTRY(EnumCase, "case");
  ENTRY(TopLevelCode, "top-level code");
  ENTRY(PoundDiagnostic, "diagnostic");
  ENTRY(PatternBinding, "pattern binding");
  ENTRY(Var, "var");
  ENTRY(Param, "parameter");
  ENTRY(Let, "let");
  ENTRY(Property, "property");
  ENTRY(StaticProperty, "static property");
  ENTRY(ClassProperty, "class property");
  ENTRY(DistributedProperty, "distributed property");
  ENTRY(PrecedenceGroup, "precedence group");
  ENTRY(InfixOperator, "infix operator");
  ENTRY(PrefixOperator, "prefix operator");
  ENTRY(PostfixOperator, "postfix operator");
  ENTRY(TypeAlias, "type alias");
  ENTRY(GenericTypeParam, "generic parameter");
  ENTRY(AssociatedType, "associated type");
  ENTRY(Type, "type");
  ENTRY(Enum, "enum");
  ENTRY(Struct, "struct");
  ENTRY(Class, "class");
  ENTRY(Actor, "actor");
  ENTRY(DistributedActor, "distributed actor");
  ENTRY(Protocol, "protocol");
  ENTRY(GenericEnum, "generic enum");
  ENTRY(GenericStruct, "generic struct");
  ENTRY(GenericClass, "generic class");
  ENTRY(GenericActor, "generic actor");
  ENTRY(GenericDistributedActor, "generic distributed actor");
  ENTRY(GenericType, "generic type");
  ENTRY(Subscript, "subscript");
  ENTRY(StaticSubscript, "static subscript");
  ENTRY(ClassSubscript, "class subscript");
  ENTRY(Constructor, "initializer");
  ENTRY(Destructor, "deinitializer");
  ENTRY(LocalFunction, "local function");
  ENTRY(GlobalFunction, "global function");
  ENTRY(OperatorFunction, "operator function");
  ENTRY(Method, "instance method");
  ENTRY(StaticMethod, "static method");
  ENTRY(ClassMethod, "class method");
  ENTRY(DistributedMethod, "distributed instance method");
  ENTRY(Getter, "getter");
  ENTRY(Setter, "setter");
  ENTRY(WillSet, "willSet observer");
  ENTRY(DidSet, "didSet observer");
  ENTRY(Addressor, "address accessor");
  ENTRY(MutableAddressor, "mutableAddress accessor");
  ENTRY(ReadAccessor, "_read accessor");
  ENTRY(ModifyAccessor, "_modify accessor");
  ENTRY(InitAccessor, "init acecssor");
  ENTRY(EnumElement, "enum case");
  ENTRY(Module, "module");
  ENTRY(Missing, "missing decl");
  ENTRY(MissingMember, "missing member placeholder");
  ENTRY(Requirement, "requirement");
  ENTRY(OpaqueResultType, "result");
  ENTRY(OpaqueVarType, "type");
  ENTRY(Macro, "macro");
  ENTRY(MacroExpansion, "pound literal");
  }
#undef ENTRY
  llvm_unreachable("bad DescriptiveDeclKind");
}

ParsedDeclAttributes Decl::getParsedAttrs() const {
  return ParsedDeclAttributes(getAttrs(), this);
}

DeclAttributes Decl::getExpandedAttrs() const {
  auto mutableThis = const_cast<Decl *>(this);
  (void)evaluateOrDefault(getASTContext().evaluator,
                          ExpandMemberAttributeMacros{mutableThis}, {});

  return getAttrs();
}

DeclAttributes Decl::getSemanticAttrs() const {
  (void)evaluateOrDefault(getASTContext().evaluator,
                          SemanticDeclAttrsRequest{this}, {});
  return getAttrs();
}

void Decl::visitAuxiliaryDecls(
    AuxiliaryDeclCallback callback,
    bool visitFreestandingExpanded
) const {
  auto &ctx = getASTContext();
  auto *mutableThis = const_cast<Decl *>(this);
  SourceManager &sourceMgr = ctx.SourceMgr;
  auto *moduleDecl = getModuleContext();

  auto peerBuffers =
      evaluateOrDefault(ctx.evaluator,
                        ExpandPeerMacroRequest{mutableThis},
                        {});

  for (auto bufferID : peerBuffers) {
    auto startLoc = sourceMgr.getLocForBufferStart(bufferID);
    auto *sourceFile = moduleDecl->getSourceFileContainingLocation(startLoc);
    for (auto *peer : sourceFile->getTopLevelDecls()) {
      callback(peer);
    }
  }

  if (visitFreestandingExpanded) {
    Decl *thisDecl = mutableThis;

    // If this is a top-level code decl consisting of a macro expansion
    // expression that substituted with a macro expansion declaration, use
    // that instead.
    if (auto *tlcd = dyn_cast<TopLevelCodeDecl>(thisDecl)) {
      if (auto body = tlcd->getBody()) {
        if (body->getNumElements() == 1) {
          if (auto expr = body->getFirstElement().dyn_cast<Expr *>()) {
            if (auto expansion = dyn_cast<MacroExpansionExpr>(expr)) {
              if (auto substitute = expansion->getSubstituteDecl())
                thisDecl = substitute;
            }
          }
        }
      }
    }

    if (auto *med = dyn_cast<MacroExpansionDecl>(thisDecl)) {
      if (auto bufferID = evaluateOrDefault(
              ctx.evaluator, ExpandMacroExpansionDeclRequest{med}, {})) {
        auto startLoc = sourceMgr.getLocForBufferStart(*bufferID);
        auto *sourceFile = moduleDecl->getSourceFileContainingLocation(startLoc);
        for (auto *decl : sourceFile->getTopLevelDecls())
          callback(decl);
      }
    }
  }

  // FIXME: fold VarDecl::visitAuxiliaryDecls into this.
}

void Decl::forEachAttachedMacro(MacroRole role,
                                MacroCallback macroCallback) const {
  for (auto customAttrConst : getExpandedAttrs().getAttributes<CustomAttr>()) {
    auto customAttr = const_cast<CustomAttr *>(customAttrConst);
    auto *macroDecl = getResolvedMacro(customAttr);

    if (!macroDecl)
      continue;

    if (!macroDecl->getMacroRoles().contains(role))
      continue;

    macroCallback(customAttr, macroDecl);
  }
}

MacroDecl *Decl::getResolvedMacro(CustomAttr *customAttr) const {
  auto declRef = evaluateOrDefault(
      getASTContext().evaluator,
      ResolveMacroRequest{customAttr, getDeclContext()}, ConcreteDeclRef());

  return dyn_cast_or_null<MacroDecl>(declRef.getDecl());
}

unsigned Decl::getAttachedMacroDiscriminator(DeclBaseName macroName,
                                             MacroRole role,
                                             const CustomAttr *attr) const {
  assert(isAttachedMacro(role) && "Not an attached macro role");

  if (role != MacroRole::MemberAttribute) {
    llvm::SmallDenseMap<Identifier, unsigned> nextDiscriminator;
    std::optional<unsigned> foundDiscriminator;

    forEachAttachedMacro(
        role, [&](CustomAttr *foundAttr, MacroDecl *foundMacro) {
          unsigned discriminator =
              nextDiscriminator[foundMacro->getBaseIdentifier()]++;
          if (attr == foundAttr)
            foundDiscriminator = discriminator;
        });

    if (foundDiscriminator)
      return *foundDiscriminator;
  }

  // If that failed, conjure up a discriminator.
  // FIXME: Better discriminator for member attributes - add the member name?
  ASTContext &ctx = getASTContext();
  assert(role == MacroRole::MemberAttribute || ctx.Diags.hadAnyError());
  return ctx.getNextMacroDiscriminator(
      MacroDiscriminatorContext::getParentOf(getLoc(), getDeclContext()),
      macroName);
}

Type Decl::getResolvedCustomAttrType(CustomAttr *attr) const {
  if (auto ty = attr->getType())
    return ty;

  auto dc = getDeclContext();
  auto *nominal = evaluateOrDefault(
      getASTContext().evaluator, CustomAttrNominalRequest{attr, dc}, nullptr);
  if (!nominal)
    return Type();

  CustomAttrTypeKind kind;
  if (nominal->isGlobalActor()) {
    kind = CustomAttrTypeKind::GlobalActor;
  } else if (nominal->getAttrs().hasAttribute<PropertyWrapperAttr>()) {
    kind = CustomAttrTypeKind::PropertyWrapper;
  } else {
    kind = CustomAttrTypeKind::NonGeneric;
  }

  return evaluateOrDefault(getASTContext().evaluator,
                           CustomAttrTypeRequest{attr, dc, kind}, Type());
}

bool Decl::isExposedToClients() const {
  return DeclExportabilityVisitor().visit(this);
}

const Decl *Decl::getInnermostDeclWithAvailability() const {
  if (getAttrs().hasAttribute<AvailableAttr>())
    return this;

  if (auto parent =
          AvailabilityInference::parentDeclForInferredAvailability(this))
    return parent->getInnermostDeclWithAvailability();

  return nullptr;
}

std::optional<llvm::VersionTuple>
Decl::getIntroducedOSVersion(PlatformKind Kind) const {
  for (auto *attr: getAttrs()) {
    if (auto *ava = dyn_cast<AvailableAttr>(attr)) {
      if (ava->Platform == Kind && ava->Introduced) {
        return ava->Introduced;
      }
    }
  }
  return std::nullopt;
}

std::optional<llvm::VersionTuple>
Decl::getBackDeployedBeforeOSVersion(ASTContext &Ctx,
                                     bool forTargetVariant) const {
  if (auto *attr = getAttrs().getBackDeployed(Ctx, forTargetVariant)) {
    auto version = attr->Version;
    StringRef ignoredPlatformString;
    AvailabilityInference::updateBeforePlatformForFallback(
        attr, getASTContext(), ignoredPlatformString, version);

    // If the remap for fallback resulted in 1.0, then the
    // backdeployment prior to that is not meaningful.
    if (version == clang::VersionTuple(1, 0, 0, 0))
      return std::nullopt;

    return version;
  }

  // Accessors may inherit `@backDeployed`.
  if (auto *AD = dyn_cast<AccessorDecl>(this))
    return AD->getStorage()->getBackDeployedBeforeOSVersion(Ctx,
                                                            forTargetVariant);

  return std::nullopt;
}

bool Decl::isBackDeployed(ASTContext &Ctx) const {
  if (getBackDeployedBeforeOSVersion(Ctx))
    return true;

  if (Ctx.LangOpts.TargetVariant) {
    if (getBackDeployedBeforeOSVersion(Ctx, /*forTargetVariant=*/true))
      return true;
  }

  return false;
}

bool Decl::hasBackDeployedAttr() const {
  if (getAttrs().hasAttribute<BackDeployedAttr>())
    return true;

  // Accessors may inherit `@backDeployed`.
  if (auto *AD = dyn_cast<AccessorDecl>(this))
    return AD->getStorage()->hasBackDeployedAttr();

  return false;
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &OS,
                                     StaticSpellingKind SSK) {
  switch (SSK) {
  case StaticSpellingKind::None:
    return OS << "<none>";
  case StaticSpellingKind::KeywordStatic:
    return OS << "'static'";
  case StaticSpellingKind::KeywordClass:
    return OS << "'class'";
  }
  llvm_unreachable("bad StaticSpellingKind");
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &OS,
                                     ReferenceOwnership RO) {
  if (RO == ReferenceOwnership::Strong)
    return OS << "'strong'";
  return OS << "'" << keywordOf(RO) << "'";
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &OS,
                                     SelfAccessKind SAK) {
  switch (SAK) {
  case SelfAccessKind::NonMutating: return OS << "'nonmutating'";
  case SelfAccessKind::Mutating: return OS << "'mutating'";
  case SelfAccessKind::LegacyConsuming: return OS << "'__consuming'";
  case SelfAccessKind::Consuming: return OS << "'consuming'";
  case SelfAccessKind::Borrowing:
    return OS << "'borrowing'";
  }
  llvm_unreachable("Unknown SelfAccessKind");
}

DeclContext *Decl::getInnermostDeclContext() const {
  if (auto func = dyn_cast<AbstractFunctionDecl>(this))
    return const_cast<AbstractFunctionDecl*>(func);
  if (auto subscript = dyn_cast<SubscriptDecl>(this))
    return const_cast<SubscriptDecl*>(subscript);
  if (auto type = dyn_cast<GenericTypeDecl>(this))
    return const_cast<GenericTypeDecl*>(type);
  if (auto ext = dyn_cast<ExtensionDecl>(this))
    return const_cast<ExtensionDecl*>(ext);
  if (auto topLevel = dyn_cast<TopLevelCodeDecl>(this))
    return const_cast<TopLevelCodeDecl*>(topLevel);
  if (auto macro = dyn_cast<MacroDecl>(this))
    return const_cast<MacroDecl*>(macro);

  return getDeclContext();
}

bool Decl::isInvalid() const {
  switch (getKind()) {
#define VALUE_DECL(ID, PARENT)
#define DECL(ID, PARENT) \
  case DeclKind::ID:
#include "swift/AST/DeclNodes.def"
    return Bits.Decl.Invalid;
  case DeclKind::Param: {
    // Parameters are special because closure parameters may not have type
    // annotations. In which case, the interface type request returns
    // ErrorType. Therefore, consider parameters with implicit types to always
    // be valid.
    auto *PD = cast<ParamDecl>(this);
    if (!PD->getTypeRepr() && !PD->hasInterfaceType())
      return false;
  }
    LLVM_FALLTHROUGH;
  case DeclKind::Enum:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Protocol:
  case DeclKind::OpaqueType:
  case DeclKind::TypeAlias:
  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType:
  case DeclKind::Module:
  case DeclKind::Var:
  case DeclKind::Subscript:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
  case DeclKind::Func:
  case DeclKind::EnumElement:
  case DeclKind::Macro:
    return cast<ValueDecl>(this)->getInterfaceType()->hasError();

  case DeclKind::Accessor: {
    auto *AD = cast<AccessorDecl>(this);
    if (AD->hasInterfaceType() && AD->getInterfaceType()->hasError())
      return true;
    return AD->getStorage()->isInvalid();
  }

  case DeclKind::BuiltinTuple:
    return false;
  }

  llvm_unreachable("Unknown decl kind");
}

void Decl::setInvalidBit() { Bits.Decl.Invalid = true; }

void Decl::setInvalid() {
  switch (getKind()) {
#define VALUE_DECL(ID, PARENT)
#define DECL(ID, PARENT) \
  case DeclKind::ID:
#include "swift/AST/DeclNodes.def"
    Bits.Decl.Invalid = true;
    return;
  case DeclKind::Enum:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Protocol:
  case DeclKind::OpaqueType:
  case DeclKind::TypeAlias:
  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType:
  case DeclKind::Module:
  case DeclKind::Var:
  case DeclKind::Param:
  case DeclKind::Subscript:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
  case DeclKind::Func:
  case DeclKind::Accessor:
  case DeclKind::EnumElement:
  case DeclKind::Macro:
    cast<ValueDecl>(this)->setInterfaceType(ErrorType::get(getASTContext()));
    return;

  case DeclKind::BuiltinTuple:
    llvm_unreachable("BuiltinTupleDecl should not end up here");
  }

  llvm_unreachable("Unknown decl kind");
}

void Decl::setDeclContext(DeclContext *DC) { 
  Context = DC;
}

bool Decl::isUserAccessible() const {
  if (auto VD = dyn_cast<ValueDecl>(this)) {
    return VD->isUserAccessible();
  }
  return true;
}

bool Decl::canHaveComment() const {
  return !this->hasClangNode() &&
         (isa<ValueDecl>(this) || isa<ExtensionDecl>(this)) &&
         !isa<ParamDecl>(this) &&
         !isa<GenericTypeParamDecl>(this);
}

ModuleDecl *Decl::getModuleContext() const {
  return getDeclContext()->getParentModule();
}

/// If `decl` is an imported Cxx decl, returns the actual module that the decl
/// was imported from. This is necessary to compensate for the way that
/// Cxx `namespace` declarations are imported. Since namespaces can span
/// multiple Clang modules, the corresponding decls and all of their members get
/// associated with the Clang imported header module (named `__ObjC`). This
/// utility reaches through the Clang AST to find the actual module that members
/// of namespaces originated from.
static ModuleDecl *getModuleContextForNameLookupForCxxDecl(const Decl *decl) {
  auto &ctx = decl->getASTContext();
  if (!ctx.LangOpts.EnableCXXInterop)
    return nullptr;

  if (!decl->hasClangNode())
    return nullptr;

  auto parentModule = decl->getModuleContext();

  // We only need to look for the real parent module when the existing parent
  // is the imported header module.
  if (!parentModule->isClangHeaderImportModule())
    return nullptr;

  auto clangModule = decl->getClangDecl()->getOwningModule();
  if (!clangModule)
    return nullptr;

  return ctx.getClangModuleLoader()->getWrapperForModule(clangModule);
}

ModuleDecl *Decl::getModuleContextForNameLookup() const {
  if (auto parentModule = getModuleContextForNameLookupForCxxDecl(this))
    return parentModule;

  return getModuleContext();
}

/// Retrieve the diagnostic engine for diagnostics emission.
DiagnosticEngine &Decl::getDiags() const {
  return getASTContext().Diags;
}

// Helper functions to verify statically whether source-location
// functions have been overridden.
typedef const char (&TwoChars)[2];
template<typename Class> 
inline char checkSourceLocType(SourceLoc (Class::*)() const);
inline TwoChars checkSourceLocType(SourceLoc (Decl::*)() const);

template<typename Class>
inline char checkSourceLocType(SourceLoc (Class::*)(bool) const);
inline TwoChars checkSourceLocType(SourceLoc (Decl::*)(bool) const);

template<typename Class> 
inline char checkSourceRangeType(SourceRange (Class::*)() const);
inline TwoChars checkSourceRangeType(SourceRange (Decl::*)() const);

SourceRange Decl::getSourceRange() const {
  switch (getKind()) {
#define DECL(ID, PARENT) \
static_assert(sizeof(checkSourceRangeType(&ID##Decl::getSourceRange)) == 1, \
              #ID "Decl is missing getSourceRange()"); \
case DeclKind::ID: return cast<ID##Decl>(this)->getSourceRange();
#include "swift/AST/DeclNodes.def"
  }

  llvm_unreachable("Unknown decl kind");
}

SourceRange Decl::getSourceRangeIncludingAttrs() const {
  auto Range = getSourceRange();

  // Attributes on AccessorDecl may syntactically belong to PatternBindingDecl.
  // e.g. 'override'.
  if (auto *AD = dyn_cast<AccessorDecl>(this)) {
    // If this is implicit getter, accessor range should not include attributes.
    if (AD->isImplicitGetter())
      return Range;

    // Otherwise, include attributes directly attached to the accessor.
    SourceLoc VarLoc = AD->getStorage()->getStartLoc();
    for (auto *Attr : getParsedAttrs()) {
      if (!Attr->getRange().isValid())
        continue;

      SourceLoc AttrStartLoc = Attr->getRangeWithAt().Start;
      if (getASTContext().SourceMgr.isBeforeInBuffer(VarLoc, AttrStartLoc))
        Range.widen(AttrStartLoc);
    }
    return Range;
  }

  // Attributes on VarDecl syntactically belong to PatternBindingDecl.
  if (isa<VarDecl>(this) && !isa<ParamDecl>(this))
    return Range;

  // Attributes on PatternBindingDecls are attached to VarDecls in AST.
  if (auto *PBD = dyn_cast<PatternBindingDecl>(this)) {
    for (auto i : range(PBD->getNumPatternEntries()))
      PBD->getPattern(i)->forEachVariable([&](VarDecl *VD) {
        for (auto *Attr : VD->getParsedAttrs())
          if (Attr->getRange().isValid())
            Range.widen(Attr->getRangeWithAt());
      });
  }

  for (auto *Attr : getParsedAttrs()) {
    if (Attr->getRange().isValid())
      Range.widen(Attr->getRangeWithAt());
  }
  return Range;
}

bool Decl::isInMacroExpansionInContext() const {
  auto *dc = getDeclContext();
  auto parentFile = dc->getParentSourceFile();
  auto *mod = getModuleContext();
  auto *file = mod->getSourceFileContainingLocation(getStartLoc());

  // Decls in macro expansions always have a source file. The source
  // file can be null if the decl is implicit or has an invalid
  // source location.
  if (!parentFile || !file)
    return false;

  if (file->getBufferID() == parentFile->getBufferID())
    return false;

  return file->getFulfilledMacroRole() != std::nullopt;
}

SourceLoc Decl::getLocFromSource() const {
  switch (getKind()) {
#define DECL(ID, X) \
static_assert(sizeof(checkSourceLocType(&ID##Decl::getLocFromSource)) == 1, \
              #ID "Decl is missing getLocFromSource()"); \
case DeclKind::ID: return cast<ID##Decl>(this)->getLocFromSource();
#include "swift/AST/DeclNodes.def"
  }

  llvm_unreachable("Unknown decl kind");
}

const ExternalSourceLocs *Decl::getSerializedLocs() const {
  auto &Context = getASTContext();
  if (auto EL = Context.getExternalSourceLocs(this).value_or(nullptr))
    return EL;

  static ExternalSourceLocs NullLocs{};

  auto *File = cast<FileUnit>(getDeclContext()->getModuleScopeContext());
  if (File->getKind() != FileUnitKind::SerializedAST)
    return &NullLocs;

  auto RawLocs = File->getExternalRawLocsForDecl(this);
  if (!RawLocs.has_value()) {
    // Don't read .swiftsourceinfo again on failure
    Context.setExternalSourceLocs(this, &NullLocs);
    return &NullLocs;
  }

  auto &SM = getASTContext().SourceMgr;
  unsigned BufferID = SM.getExternalSourceBufferID(RawLocs->SourceFilePath);
  if (!BufferID) {
    // Don't try open the file again on failure
    Context.setExternalSourceLocs(this, &NullLocs);
    return &NullLocs;
  }

  CharSourceRange BufferRange = SM.getRangeForBuffer(BufferID);
  auto ResolveLoc = [&](const ExternalSourceLocs::RawLoc &Raw) -> SourceLoc {
    // If the underlying source has been updated and the swiftsourceinfo hasn't,
    // make sure we don't produce invalid source locations. Ideally would check
    // the file hasn't been modified.
    if (Raw.Offset > BufferRange.getByteLength())
      return SourceLoc();

    // If the decl had a presumed loc, create its virtual file so that
    // getPresumedLineAndColForLoc works from serialized locations as well. No
    // need to check the buffer range, the directive must be before the location
    // itself.
    if (Raw.Directive.isValid()) {
      auto &LD = Raw.Directive;
      SourceLoc Loc = SM.getLocForOffset(BufferID, LD.Offset);
      SM.createVirtualFile(Loc, LD.Name, LD.LineOffset, LD.Length);
    }
    return SM.getLocForOffset(BufferID, Raw.Offset);
  };

  auto *Result = getASTContext().Allocate<ExternalSourceLocs>();
  Result->BufferID = BufferID;
  Result->Loc = ResolveLoc(RawLocs->Loc);

  auto DocRanges = getASTContext().AllocateUninitialized<CharSourceRange>(RawLocs->DocRanges.size());
  for (auto I : indices(RawLocs->DocRanges)) {
    auto &Range = RawLocs->DocRanges[I];
    DocRanges[I] = CharSourceRange(ResolveLoc(Range.first), Range.second);
  }
  Result->DocRanges = DocRanges;

  Context.setExternalSourceLocs(this, Result);
  return Result;
}

StringRef Decl::getAlternateModuleName() const {
  for (auto *Att: Attrs) {
    if (auto *OD = dyn_cast<OriginallyDefinedInAttr>(Att)) {
      if (OD->isActivePlatform(getASTContext())) {
        return OD->OriginalModuleName;
      }
    }
  }
  
  for (auto *DC = getDeclContext(); DC; DC = DC->getParent()) {
    if (auto decl = DC->getAsDecl()) {
      if (decl == this)
        continue;
      auto AM = decl->getAlternateModuleName();
      if (!AM.empty())
        return AM;
    }
  }
  return StringRef();
}

SourceLoc Decl::getLoc(bool SerializedOK) const {
#define DECL(ID, X) \
static_assert(sizeof(checkSourceLocType(&ID##Decl::getLoc)) == 2, \
              #ID "Decl is re-defining getLoc()");
#include "swift/AST/DeclNodes.def"
  if (isa<ModuleDecl>(this))
    return SourceLoc();
  // When the decl is context-free, we should get loc from source buffer.
  if (!getDeclContext())
    return getLocFromSource();
  FileUnit *File = dyn_cast<FileUnit>(getDeclContext()->getModuleScopeContext());
  if (!File)
    return getLocFromSource();
  switch(File->getKind()) {
  case FileUnitKind::Source:
    return getLocFromSource();
  case FileUnitKind::SerializedAST: {
    if (!SerializedOK)
      return SourceLoc();
    return getSerializedLocs()->Loc;
  }
  case FileUnitKind::Builtin:
  case FileUnitKind::Synthesized:
  case FileUnitKind::ClangModule:
  case FileUnitKind::DWARFModule:
    return SourceLoc();
  }
  llvm_unreachable("invalid file kind");
}

std::optional<CustomAttrNominalPair> Decl::getGlobalActorAttr() const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<Decl *>(this);
  return evaluateOrDefault(
      ctx.evaluator, GlobalActorAttributeRequest{mutableThis}, std::nullopt);
}

bool Decl::hasExplicitIsolationAttribute() const {
  if (auto nonisolatedAttr = getAttrs().getAttribute<NonisolatedAttr>()) {
    if (!nonisolatedAttr->isImplicit())
      return true;
  }

  if (auto isolatedAttr = getAttrs().getAttribute<IsolatedAttr>()) {
    if (!isolatedAttr->isImplicit()) {
      return true;
    }
  }

  if (auto globalActorAttr = getGlobalActorAttr()) {
    if (!globalActorAttr->first->isImplicit())
      return true;
  }

  return false;
}

bool Decl::preconcurrency() const {
  if (getAttrs().hasAttribute<PreconcurrencyAttr>())
    return true;

  // Imported C declarations always predate concurrency.
  if (isa<ClangModuleUnit>(getDeclContext()->getModuleScopeContext()))
    return true;

  // Variables declared in top-level code are @_predatesConcurrency
  if (const VarDecl *var = dyn_cast<VarDecl>(this)) {
    const LangOptions &langOpts = getASTContext().LangOpts;
    return !langOpts.isSwiftVersionAtLeast(6) && var->isTopLevelGlobal() &&
           var->getDeclContext()->isAsyncContext();
  }

  return false;
}

bool Decl::isUnsafe() const {
  return evaluateOrDefault(
      getASTContext().evaluator,
      IsUnsafeRequest{const_cast<Decl *>(this)},
      false);
}

Type AbstractFunctionDecl::getThrownInterfaceType() const {
  if (!getThrownTypeRepr())
    return ThrownType.getType();

  auto mutableThis = const_cast<AbstractFunctionDecl *>(this);
  return CatchNode(mutableThis).getExplicitCaughtType(getASTContext());
}

std::optional<Type> AbstractFunctionDecl::getEffectiveThrownErrorType() const {
  // FIXME: Only getters can have thrown error types right now, and DidSet
  // has a cyclic reference if we try to get its interface type here. Find a
  // better way to express this.
  if (auto accessor = dyn_cast<AccessorDecl>(this)) {
    if (accessor->getAccessorKind() != AccessorKind::Get &&
        accessor->getAccessorKind() != AccessorKind::DistributedGet) {
      return std::nullopt;
    }
  }

  Type interfaceType = getInterfaceType();
  if (hasImplicitSelfDecl()) {
    if (auto fnType = interfaceType->getAs<AnyFunctionType>())
      interfaceType = fnType->getResult();
  }

  if (auto fnType = interfaceType->getAs<AnyFunctionType>())
    return fnType->getEffectiveThrownErrorType();

  return std::nullopt;
}

bool AbstractStorageDecl::isCompileTimeConst() const {
  return getAttrs().hasAttribute<CompileTimeConstAttr>();
}

bool AbstractStorageDecl::isTransparent() const {
  return getAttrs().hasAttribute<TransparentAttr>();
}

bool AbstractFunctionDecl::isTransparent() const {
  // Check if the declaration had the attribute.
  if (getAttrs().hasAttribute<TransparentAttr>())
    return true;

  // If this is an accessor, the computation is a bit more involved, so we
  // kick off a request.
  if (const auto *AD = dyn_cast<AccessorDecl>(this)) {
    ASTContext &ctx = getASTContext();
    return evaluateOrDefault(ctx.evaluator,
      IsAccessorTransparentRequest{const_cast<AccessorDecl *>(AD)},
      false);
  }

  return false;
}

bool ParameterList::hasInternalParameter(StringRef Prefix) const {
  for (auto param : *this) {
    if (param->hasName() && param->getNameStr().starts_with(Prefix))
      return true;
    auto argName = param->getArgumentName();
    if (!argName.empty() && argName.str().starts_with(Prefix))
      return true;
  }
  return false;
}

bool Decl::hasUnderscoredNaming() const {
  const Decl *D = this;

  // If it's a function or subscript with a parameter with leading
  // underscore, it's a private function or subscript.
  if (isa<AbstractFunctionDecl>(D) || isa<SubscriptDecl>(D)) {
    const auto VD = cast<ValueDecl>(D);
    if (getParameterList(const_cast<ValueDecl *>(VD))
            ->hasInternalParameter("_")) {
      return true;
    }
  }

  if (const auto PD = dyn_cast<ProtocolDecl>(D)) {
    StringRef NameStr = PD->getNameStr();
    if (NameStr.starts_with("_Builtin")) {
      return true;
    }
    if (NameStr.starts_with("_ExpressibleBy")) {
      return true;
    }
  }

  if (const auto ImportD = dyn_cast<ImportDecl>(D)) {
    if (const auto *Mod = ImportD->getModule()) {
      if (Mod->isSwiftShimsModule()) {
        return true;
      }
    }
  }

  const auto VD = dyn_cast<ValueDecl>(D);
  if (!VD || !VD->hasName()) {
    return false;
  }

  if (!VD->getBaseName().isSpecial() &&
      VD->getBaseIdentifier().hasUnderscoredNaming()) {
    return true;
  }

  return false;
}

bool Decl::isPrivateSystemDecl(bool treatNonBuiltinProtocolsAsPublic) const {
  const Decl *D = this;
  if (auto ExtD = dyn_cast<ExtensionDecl>(D)) {
    Type extTy = ExtD->getExtendedType();
    return extTy.isPrivateSystemType(treatNonBuiltinProtocolsAsPublic);
  }

  DeclContext *DC = D->getDeclContext()->getModuleScopeContext();
  auto *M = DC->getParentModule();
  if (M->isBuiltinModule() || M->isSwiftShimsModule())
    return true;
  if (!M->isSystemModule() && !M->isNonUserModule())
    return false;
  auto FU = dyn_cast<FileUnit>(DC);
  if (!FU)
    return false;
  // Check for stdlib and imported Swift modules.
  if (!M->isStdlibModule() && FU->getKind() != FileUnitKind::SerializedAST)
    return false;

  if (isa<ProtocolDecl>(D)) {
    if (treatNonBuiltinProtocolsAsPublic)
      return false;
  }

  if (D->getAttrs().hasAttribute<ShowInInterfaceAttr>()) {
    return false;
  }

  return hasUnderscoredNaming();
}

bool Decl::isStdlibDecl() const {
  DeclContext *DC = getDeclContext();
  return DC->isModuleScopeContext() &&
         DC->getParentModule()->isStdlibModule();
}

LifetimeAnnotation Decl::getLifetimeAnnotationFromAttributes() const {
  auto &attrs = getAttrs();
  if (attrs.hasAttribute<EagerMoveAttr>())
    return LifetimeAnnotation::EagerMove;
  if (attrs.hasAttribute<NoEagerMoveAttr>())
    return LifetimeAnnotation::Lexical;
  return LifetimeAnnotation::None;
}

LifetimeAnnotation Decl::getLifetimeAnnotation() const {
  if (auto *pd = dyn_cast<ParamDecl>(this)) {
    return pd->getLifetimeAnnotation();
  }
  if (auto *fd = dyn_cast<FuncDecl>(this)) {
    return fd->getLifetimeAnnotation();
  }
  return getLifetimeAnnotationFromAttributes();
}

AvailabilityRange Decl::getAvailabilityForLinkage() const {
  ASTContext &ctx = getASTContext();

  // When computing availability for linkage, use the "before" version from
  // the @backDeployed attribute, if present.
  if (auto backDeployVersion = getBackDeployedBeforeOSVersion(ctx))
    return AvailabilityRange{VersionRange::allGTE(*backDeployVersion)};

  auto containingContext = AvailabilityInference::annotatedAvailableRange(this);
  if (containingContext.has_value()) {
    // If this entity comes from the concurrency module, adjust its
    // availability for linkage purposes up to Swift 5.5, so that we use
    // weak references any time we reference those symbols when back-deploying
    // concurrency.
    if (getModuleContext()->getName() == ctx.Id_Concurrency) {
      containingContext->intersectWith(ctx.getConcurrencyAvailability());
    }

    return *containingContext;
  }

  // FIXME: Adopt AvailabilityInference::parentDeclForInferredAvailability()
  // here instead of duplicating the logic.
  if (auto *accessor = dyn_cast<AccessorDecl>(this))
    return accessor->getStorage()->getAvailabilityForLinkage();

  if (auto *opaqueTypeDecl = dyn_cast<OpaqueTypeDecl>(this))
    return opaqueTypeDecl->getNamingDecl()->getAvailabilityForLinkage();

  if (auto *ext = dyn_cast<ExtensionDecl>(this))
    if (auto *nominal = ext->getExtendedNominal())
      return nominal->getAvailabilityForLinkage();

  auto *dc = getDeclContext();
  if (auto *ext = dyn_cast<ExtensionDecl>(dc))
    return ext->getAvailabilityForLinkage();
  else if (auto *nominal = dyn_cast<NominalTypeDecl>(dc))
    return nominal->getAvailabilityForLinkage();

  return AvailabilityRange::alwaysAvailable();
}

bool Decl::isAlwaysWeakImported() const {
  // For a Clang declaration, trust Clang.
  if (auto clangDecl = getClangDecl()) {
    return clangDecl->isWeakImported();
  }

  if (getAttrs().hasAttribute<WeakLinkedAttr>())
    return true;

  // FIXME: Weak linking on Windows is not yet supported
  // https://github.com/apple/swift/issues/53303
  if (getSemanticUnavailableAttr() &&
      !getASTContext().LangOpts.Target.isOSWindows())
    return true;

  if (auto *accessor = dyn_cast<AccessorDecl>(this))
    return accessor->getStorage()->isAlwaysWeakImported();

  if (auto *opaqueTypeDecl = dyn_cast<OpaqueTypeDecl>(this))
    return opaqueTypeDecl->getNamingDecl()->isAlwaysWeakImported();

  if (auto *ext = dyn_cast<ExtensionDecl>(this))
    if (auto *nominal = ext->getExtendedNominal())
      return nominal->isAlwaysWeakImported();

  auto *dc = getDeclContext();
  if (auto *ext = dyn_cast<ExtensionDecl>(dc))
    return ext->isAlwaysWeakImported();
  if (auto *nominal = dyn_cast<NominalTypeDecl>(dc))
    return nominal->isAlwaysWeakImported();

  return false;
}

bool Decl::isWeakImported(ModuleDecl *fromModule) const {
  if (fromModule == nullptr) {
    return (isAlwaysWeakImported() ||
            !getAvailabilityForLinkage().isAlwaysAvailable());
  }

  if (getModuleContext() == fromModule)
    return false;

  if (isAlwaysWeakImported())
    return true;

  if (fromModule->isImportedAsWeakLinked(this->getModuleContext()))
    return true;

  auto availability = getAvailabilityForLinkage();
  if (availability.isAlwaysAvailable())
    return false;

  auto &ctx = fromModule->getASTContext();
  auto deploymentTarget = AvailabilityRange::forDeploymentTarget(ctx);

  if (ctx.LangOpts.WeakLinkAtTarget)
    return !availability.isSupersetOf(deploymentTarget);

  return !deploymentTarget.isContainedIn(availability);
}

GenericContext::GenericContext(DeclContextKind Kind, DeclContext *Parent,
                               GenericParamList *Params)
    : _GenericContext(), DeclContext(Kind, Parent) {
  if (Params) {
    Params->setDeclContext(this);
    GenericParamsAndState.setPointerAndInt(Params, GenericParamsState::Parsed);
  }
}

ArrayRef<GenericTypeParamType *>
GenericContext::getInnermostGenericParamTypes() const {
  return getGenericSignature().getInnermostGenericParams();
}

/// Retrieve the generic requirements.
ArrayRef<Requirement> GenericContext::getGenericRequirements() const {
  return getGenericSignature().getRequirements();
}

GenericParamList *GenericContext::getGenericParams() const {
  return evaluateOrDefault(getASTContext().evaluator,
                           GenericParamListRequest{
                               const_cast<GenericContext *>(this)}, nullptr);
}

GenericParamList *GenericContext::getParsedGenericParams() const {
  switch (GenericParamsAndState.getInt()) {
  case GenericParamsState::Parsed:
  case GenericParamsState::ParsedAndTypeChecked:
    return GenericParamsAndState.getPointer();

  case GenericParamsState::TypeChecked:
    return nullptr;
  }
}

bool GenericContext::hasComputedGenericSignature() const {
  return GenericSigAndBit.getInt();
}
      
bool GenericContext::isComputingGenericSignature() const {
  return getASTContext().evaluator.hasActiveRequest(
                 GenericSignatureRequest{const_cast<GenericContext*>(this)});
}

/// If we hit a cycle while building the generic signature, we can't return
/// nullptr, since this breaks invariants elsewhere. Instead, build a dummy
/// signature where everything is Copyable and Escapable, to avoid spurious
/// downstream diagnostics concerning move-only types.
static GenericSignature getPlaceholderGenericSignature(
    ASTContext &ctx, const DeclContext *DC) {
  SmallVector<GenericParamList *, 2> gpLists;
  DC->forEachGenericContext([&](GenericParamList *genericParams) {
    gpLists.push_back(genericParams);
  });

  if (gpLists.empty())
    return nullptr;

  std::reverse(gpLists.begin(), gpLists.end());
  for (unsigned i : indices(gpLists))
    gpLists[i]->setDepth(i);

  SmallVector<GenericTypeParamType *, 2> genericParams;
  SmallVector<Requirement, 2> requirements;

  for (auto *gpList : gpLists) {
    for (auto *genericParam : *gpList) {
      auto type = genericParam->getDeclaredInterfaceType();
      genericParams.push_back(type->castTo<GenericTypeParamType>());

      for (auto ip : InvertibleProtocolSet::allKnown()) {
        auto proto = ctx.getProtocol(getKnownProtocolKind(ip));
        requirements.emplace_back(RequirementKind::Conformance, type,
                                  proto->getDeclaredInterfaceType());
      }
    }
  }

  return GenericSignature::get(genericParams, requirements);
}

GenericSignature GenericContext::getGenericSignature() const {
  auto &ctx = getASTContext();
  return ctx.evaluator(
      GenericSignatureRequest{const_cast<GenericContext *>(this)},
      [&ctx, this]() { return getPlaceholderGenericSignature(ctx, this); });
}

GenericEnvironment *GenericContext::getGenericEnvironment() const {
  return getGenericSignature().getGenericEnvironment();
}

void GenericContext::setGenericSignature(GenericSignature genericSig) {
  assert(!GenericSigAndBit.getPointer() && "Generic signature cannot be changed");
  getASTContext().evaluator.cacheOutput(GenericSignatureRequest{this},
                                        std::move(genericSig));
}

SourceRange GenericContext::getGenericTrailingWhereClauseSourceRange() const {
  if (const auto *where = getTrailingWhereClause())
    return where->getSourceRange();

  return SourceRange();
}

ImportDecl *ImportDecl::create(ASTContext &Ctx, DeclContext *DC,
                               SourceLoc ImportLoc, ImportKind Kind,
                               SourceLoc KindLoc,
                               ImportPath Path,
                               ClangNode ClangN) {
  assert(!Path.empty());
  assert(Kind == ImportKind::Module || Path.size() > 1);
  assert(ClangN.isNull() || ClangN.getAsModule() ||
         isa<clang::ImportDecl>(ClangN.getAsDecl()));
  size_t Size = totalSizeToAlloc<ImportPath::Element>(Path.size());
  void *ptr = allocateMemoryForDecl<ImportDecl>(Ctx, Size, !ClangN.isNull());
  auto D = new (ptr) ImportDecl(DC, ImportLoc, Kind, KindLoc, Path);
  if (ClangN)
    D->setClangNode(ClangN);
  auto realNameIfExists = Ctx.getRealModuleName(Path.front().Item,
                                                ASTContext::ModuleAliasLookupOption::realNameFromAlias);
  if (!realNameIfExists.empty()) {
    D->RealModuleName = realNameIfExists;
  }
  return D;
}

ImportDecl::ImportDecl(DeclContext *DC, SourceLoc ImportLoc, ImportKind K,
                       SourceLoc KindLoc, ImportPath Path)
  : Decl(DeclKind::Import, DC), ImportLoc(ImportLoc), KindLoc(KindLoc) {
  Bits.ImportDecl.NumPathElements = Path.size();
  assert(Bits.ImportDecl.NumPathElements == Path.size() && "Truncation error");
  Bits.ImportDecl.ImportKind = static_cast<unsigned>(K);
  assert(getImportKind() == K && "not enough bits for ImportKind");
  std::uninitialized_copy(Path.begin(), Path.end(),
                          getTrailingObjects<ImportPath::Element>());
}

ImportKind ImportDecl::getBestImportKind(const ValueDecl *VD) {
  switch (VD->getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::EnumCase:
  case DeclKind::PoundDiagnostic:
  case DeclKind::PrecedenceGroup:
  case DeclKind::Missing:
  case DeclKind::MissingMember:
  case DeclKind::MacroExpansion:
    llvm_unreachable("not a ValueDecl");

  case DeclKind::AssociatedType:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
  case DeclKind::GenericTypeParam:
  case DeclKind::Subscript:
  case DeclKind::EnumElement:
  case DeclKind::Param:
    llvm_unreachable("not a top-level ValueDecl");

  case DeclKind::BuiltinTuple:
    llvm_unreachable("BuiltinTupleDecl should not end up here");

  case DeclKind::Protocol:
    return ImportKind::Protocol;

  case DeclKind::Class:
    return ImportKind::Class;
  case DeclKind::Enum:
    return ImportKind::Enum;
  case DeclKind::Struct:
    return ImportKind::Struct;
      
  case DeclKind::OpaqueType:
    return ImportKind::Type;

  case DeclKind::TypeAlias: {
    Type type = cast<TypeAliasDecl>(VD)->getDeclaredInterfaceType();
    // FIXME: It's necessary to check for existentials because `getAnyNominal`
    // looks through them.
    auto canonical = type->getCanonicalType();
    if (isa<ExistentialType>(canonical))
      return ImportKind::Type;
    auto *nominal = canonical->getAnyNominal();
    if (!nominal)
      return ImportKind::Type;
    return getBestImportKind(nominal);
  }

  case DeclKind::Accessor:
  case DeclKind::Func:
    return ImportKind::Func;

  case DeclKind::Var:
    return ImportKind::Var;

  case DeclKind::Module:
  case DeclKind::Macro:
    return ImportKind::Module;
  }
  llvm_unreachable("bad DeclKind");
}

std::optional<ImportKind>
ImportDecl::findBestImportKind(ArrayRef<ValueDecl *> Decls) {
  assert(!Decls.empty());
  ImportKind FirstKind = ImportDecl::getBestImportKind(Decls.front());

  // FIXME: Only functions can be overloaded.
  if (Decls.size() == 1)
    return FirstKind;
  if (FirstKind != ImportKind::Func)
    return std::nullopt;

  for (auto NextDecl : Decls.slice(1)) {
    if (ImportDecl::getBestImportKind(NextDecl) != FirstKind)
      return std::nullopt;
  }

  return FirstKind;
}

ImportPath ImportDecl::getRealImportPath(ImportPath::Builder &scratch) const {
  assert(scratch.empty() && "scratch ImportPath::Builder must be initially empty");
  auto path = getImportPath();
  if (RealModuleName.empty())
    return path;

  for (auto elem : path) {
    if (scratch.empty()) {
      // Add the real module name instead of its alias
      scratch.push_back(RealModuleName);
    } else {
      // Add the rest if any (access path elements)
      scratch.push_back(elem.Item);
    }
  }
  return scratch.get();
}

ArrayRef<ValueDecl *> ImportDecl::getDecls() const {
  // If this isn't a scoped import, there's nothing to do.
  if (getImportKind() == ImportKind::Module)
    return {};

  auto &ctx = getASTContext();
  auto *mutableThis = const_cast<ImportDecl *>(this);
  return evaluateOrDefault(ctx.evaluator,
                           ScopedImportLookupRequest{mutableThis}, {});
}

AccessLevel ImportDecl::getAccessLevel() const {
  if (auto attr = getAttrs().getAttribute<AccessControlAttr>()) {
    if (attr->getAccess() == AccessLevel::Open) {
      // Use a conservative import if the level given is invalid.
      return AccessLevel::Internal;
    } else
      return attr->getAccess();
  }

  auto &LangOpts = getASTContext().LangOpts;
  if (LangOpts.hasFeature(Feature::InternalImportsByDefault)) {
    // Swift 6 mode where the default import is internal.
    return AccessLevel::Internal;
  } else {
    return AccessLevel::Public;
  }
}

bool ImportDecl::isAccessLevelImplicit() const {
  if (auto attr = getAttrs().getAttribute<AccessControlAttr>()) {
    return false;
  }
  return true;
}

void NominalTypeDecl::setConformanceLoader(LazyMemberLoader *lazyLoader,
                                           uint64_t contextData) {
  assert(!Bits.NominalTypeDecl.HasLazyConformances &&
         "Already have lazy conformances");
  Bits.NominalTypeDecl.HasLazyConformances = true;

  ASTContext &ctx = getASTContext();
  auto contextInfo = ctx.getOrCreateLazyIterableContextData(this, lazyLoader);
  contextInfo->allConformancesData = contextData;
}

std::pair<LazyMemberLoader *, uint64_t>
NominalTypeDecl::takeConformanceLoaderSlow() {
  assert(Bits.NominalTypeDecl.HasLazyConformances && "not lazy conformances");
  Bits.NominalTypeDecl.HasLazyConformances = false;
  auto contextInfo =
    getASTContext().getOrCreateLazyIterableContextData(this, nullptr);
  return { contextInfo->loader, contextInfo->allConformancesData };
}

InheritedEntry::InheritedEntry(const TypeLoc &typeLoc)
    : InheritedEntry(typeLoc, /*isUnchecked=*/false, /*isRetroactive=*/false,
                     /*isPreconcurrency=*/false) {
  if (auto typeRepr = typeLoc.getTypeRepr()) {
    IsUnchecked = typeRepr->findAttrLoc(TypeAttrKind::Unchecked).isValid();
    IsRetroactive = typeRepr->findAttrLoc(TypeAttrKind::Retroactive).isValid();
    IsPreconcurrency =
        typeRepr->findAttrLoc(TypeAttrKind::Preconcurrency).isValid();
  }
}

InheritedTypes::InheritedTypes(const TypeDecl *typeDecl) : Decl(typeDecl) {
  Entries = typeDecl->Inherited;
}

InheritedTypes::InheritedTypes(const ExtensionDecl *extensionDecl)
    : Decl(extensionDecl) {
  Entries = extensionDecl->Inherited;
}

InheritedTypes::InheritedTypes(
    llvm::PointerUnion<const TypeDecl *, const ExtensionDecl *> decl)
    : Decl(decl) {
  if (auto *typeDecl = decl.dyn_cast<const TypeDecl *>()) {
    Entries = typeDecl->Inherited;
  } else {
    Entries = decl.get<const ExtensionDecl *>()->Inherited;
  }
}

InheritedTypes::InheritedTypes(const class Decl *decl) {
  if (auto typeDecl = dyn_cast<TypeDecl>(decl)) {
    Decl = typeDecl;
    Entries = typeDecl->Inherited;
  } else if (auto extensionDecl = dyn_cast<ExtensionDecl>(decl)) {
    Decl = extensionDecl;
    Entries = extensionDecl->Inherited;
  } else {
    Decl = nullptr;
    Entries = ArrayRef<InheritedEntry>();
  }
}

ASTContext &InheritedTypes::getASTContext() const {
  if (auto typeDecl = Decl.dyn_cast<const TypeDecl *>()) {
    return typeDecl->getASTContext();
  } else {
    return Decl.get<const ExtensionDecl *>()->getASTContext();
  }
}

SourceLoc InheritedTypes::getColonLoc() const {
  if (Entries.size() == 0)
    return SourceLoc();

  SourceLoc precedingTok;
  if (auto typeDecl = Decl.dyn_cast<const TypeDecl *>()) {
    precedingTok = typeDecl->getNameLoc();
  } else {
    auto *ext = Decl.get<const ExtensionDecl *>();
    precedingTok = ext->getSourceRange().End;
  }

  auto &ctx = getASTContext();
  return Lexer::getLocForEndOfToken(ctx.SourceMgr, precedingTok);
}

SourceRange InheritedTypes::getRemovalRange(unsigned i) const {
  auto inheritedClause = getEntries();
  auto &ctx = getASTContext();

  // If there is just one entry, remove the entire inheritance clause.
  if (inheritedClause.size() == 1) {
    SourceLoc end = inheritedClause[i].getSourceRange().End;
    return SourceRange(getColonLoc(),
                       Lexer::getLocForEndOfToken(ctx.SourceMgr, end));
  }

  // If we're at the first entry, remove from the start of this entry to the
  // start of the next entry.
  if (i == 0) {
    return SourceRange(inheritedClause[i].getSourceRange().Start,
                       inheritedClause[i+1].getSourceRange().Start);
  }

  // Otherwise, remove from the end of the previous entry to the end of this
  // entry.
  SourceLoc afterPriorLoc =
      Lexer::getLocForEndOfToken(ctx.SourceMgr,
                                 inheritedClause[i-1].getSourceRange().End);

  SourceLoc afterMyEndLoc =
      Lexer::getLocForEndOfToken(ctx.SourceMgr,
                                 inheritedClause[i].getSourceRange().End);

  return SourceRange(afterPriorLoc, afterMyEndLoc);
}

Type InheritedTypes::getResolvedType(unsigned i,
                                     TypeResolutionStage stage) const {
  ASTContext &ctx = Decl.is<const ExtensionDecl *>()
                        ? Decl.get<const ExtensionDecl *>()->getASTContext()
                        : Decl.get<const TypeDecl *>()->getASTContext();
  return evaluateOrDefault(ctx.evaluator, InheritedTypeRequest{Decl, i, stage},
                           InheritedTypeResult::forDefault())
      .getInheritedTypeOrNull(getASTContext());
}

ExtensionDecl::ExtensionDecl(SourceLoc extensionLoc,
                             TypeRepr *extendedType,
                             ArrayRef<InheritedEntry> inherited,
                             DeclContext *parent,
                             TrailingWhereClause *trailingWhereClause)
  : GenericContext(DeclContextKind::ExtensionDecl, parent, nullptr),
    Decl(DeclKind::Extension, parent),
    IterableDeclContext(IterableDeclContextKind::ExtensionDecl),
    ExtensionLoc(extensionLoc),
    ExtendedTypeRepr(extendedType),
    Inherited(inherited)
{
  Bits.ExtensionDecl.DefaultAndMaxAccessLevel = 0;
  Bits.ExtensionDecl.HasLazyConformances = false;
  setTrailingWhereClause(trailingWhereClause);
}

ExtensionDecl *ExtensionDecl::create(ASTContext &ctx, SourceLoc extensionLoc,
                                     TypeRepr *extendedType,
                                     ArrayRef<InheritedEntry> inherited,
                                     DeclContext *parent,
                                     TrailingWhereClause *trailingWhereClause,
                                     ClangNode clangNode) {
  unsigned size = sizeof(ExtensionDecl);

  void *declPtr = allocateMemoryForDecl<ExtensionDecl>(ctx, size,
                                                       !clangNode.isNull());

  // Construct the extension.
  auto result = ::new (declPtr) ExtensionDecl(extensionLoc, extendedType,
                                              inherited, parent,
                                              trailingWhereClause);
  if (clangNode)
    result->setClangNode(clangNode);

  return result;
}

void ExtensionDecl::setConformanceLoader(LazyMemberLoader *lazyLoader,
                                         uint64_t contextData) {
  assert(!Bits.ExtensionDecl.HasLazyConformances && 
         "Already have lazy conformances");
  Bits.ExtensionDecl.HasLazyConformances = true;

  ASTContext &ctx = getASTContext();
  auto contextInfo = ctx.getOrCreateLazyIterableContextData(this, lazyLoader);
  contextInfo->allConformancesData = contextData;
}

std::pair<LazyMemberLoader *, uint64_t>
ExtensionDecl::takeConformanceLoaderSlow() {
  assert(Bits.ExtensionDecl.HasLazyConformances && "no conformance loader?");
  Bits.ExtensionDecl.HasLazyConformances = false;

  auto contextInfo =
    getASTContext().getOrCreateLazyIterableContextData(this, nullptr);
  return { contextInfo->loader, contextInfo->allConformancesData };
}

NominalTypeDecl *ExtensionDecl::getExtendedNominal() const {
  if (hasBeenBound()) {
    return ExtendedNominal.getPointer();
  } else if (canNeverBeBound()) {
    return computeExtendedNominal();
  }

  llvm_unreachable(
      "Extension must have already been bound (by bindExtensions)");
}

NominalTypeDecl *ExtensionDecl::computeExtendedNominal() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(
      ctx.evaluator, ExtendedNominalRequest{const_cast<ExtensionDecl *>(this)},
      nullptr);
}

bool ExtensionDecl::canNeverBeBound() const {
  // \c bindExtensions() only looks at valid parents for extensions.
  return !hasValidParent();
}

bool ExtensionDecl::hasValidParent() const {
  return getDeclContext()->canBeParentOfExtension();
}

/// Does the extension's generic signature impose additional generic requirements
/// not stated on the extended nominal type itself?
bool ExtensionDecl::isConstrainedExtension() const {
  auto nominal = getExtendedNominal();
  if (!nominal)
    return false;

  auto typeSig = nominal->getGenericSignature();
  if (!typeSig)
    return false;

  auto extSig = getGenericSignature();
  if (!extSig)
    return false;

  // If the generic signature differs from that of the nominal type, it's a
  // constrained extension.
  return !typeSig->isEqual(extSig);
}

/// Is the extension written as an unconstrained extension? This is not the same
/// as isConstrainedExtension() in the case where the extended nominal type has
/// inverse requirements, because an extension of such a type introduces default
/// conformance requirements unless they're suppressed on the extension.
///
/// enum Optional<Wrapped> where Wrapped: ~Copyable {}
///
/// extension Optional {}
///   --> isConstrainedExtension(): true
///   --> isWrittenWithConstraints(): false
///
/// extension Optional where Wrapped: ~Copyable {}
///   --> isConstrainedExtension(): false
///   --> isWrittenWithConstraints(): true
bool ExtensionDecl::isWrittenWithConstraints() const {
  auto nominal = getExtendedNominal();
  if (!nominal)
    return false;

  // If there's no generic signature, then it's unconstrained.
  CanGenericSignature extSig = getGenericSignature().getCanonicalSignature();
  if (!extSig)
    return false;

  CanGenericSignature typeSig =
      nominal->getGenericSignature().getCanonicalSignature();

  // Get the requirements and inverses for both the extension and type.
  SmallVector<Requirement, 2> extReqs;
  SmallVector<InverseRequirement, 2> extInverseReqs;
  extSig->getRequirementsWithInverses(extReqs, extInverseReqs);

  SmallVector<Requirement, 2> typeReqs;
  SmallVector<InverseRequirement, 2> typeInverseReqs;
  typeSig->getRequirementsWithInverses(typeReqs, typeInverseReqs);

  // If the non-inverse requirements are different between the extension and
  // the original type, it's written with constraints.
  if (extReqs != typeReqs)
    return true;

  // If the extension has inverse requirements, then it's written with
  // constraints.
  if (!extInverseReqs.empty())
    return true;

  // Otherwise, the extension is written as an unconstrained extension.
  return false;
}

bool ExtensionDecl::isInSameDefiningModule() const {
  auto decl = getExtendedNominal();
  auto extensionAlterName = getAlternateModuleName();
  auto typeAlterName = decl->getAlternateModuleName();

  if (!extensionAlterName.empty()) {
    if (!typeAlterName.empty()) {
      // Case I: type and extension are both moved from somewhere else
      return typeAlterName == extensionAlterName;
    } else {
      // Case II: extension alone was moved from somewhere else
      return extensionAlterName == decl->getParentModule()->getNameStr();
    }
  } else {
    if (!typeAlterName.empty()) {
      // Case III: extended type alone was moved from somewhere else
      return typeAlterName == getParentModule()->getNameStr();
    } else {
      // Case IV: neither of type and extension was moved from somewhere else
      return getParentModule() == decl->getParentModule();
    }
  }
}

bool ExtensionDecl::isEquivalentToExtendedContext() const {
  return isInSameDefiningModule()
    && !isConstrainedExtension()
    && !getDeclaredInterfaceType()->isExistentialType();
}

AccessLevel ExtensionDecl::getDefaultAccessLevel() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    DefaultAndMaxAccessLevelRequest{const_cast<ExtensionDecl *>(this)},
    {AccessLevel::Private, AccessLevel::Private}).first;
}

AccessLevel ExtensionDecl::getMaxAccessLevel() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    DefaultAndMaxAccessLevelRequest{const_cast<ExtensionDecl *>(this)},
    {AccessLevel::Private, AccessLevel::Private}).second;
}
      
Type ExtensionDecl::getExtendedType() const {
  ASTContext &ctx = getASTContext();
  if (auto type = evaluateOrDefault(ctx.evaluator,
          ExtendedTypeRequest{const_cast<ExtensionDecl *>(this)},
          Type()))
    return type;
  return ErrorType::get(ctx);
}

bool ExtensionDecl::isAddingConformanceToInvertible() const {
  const unsigned numEntries = getInherited().size();
  for (unsigned i = 0; i < numEntries; ++i) {
    InheritedTypeRequest request{this, i, TypeResolutionStage::Structural};
    auto result = evaluateOrDefault(getASTContext().evaluator, request,
                                    InheritedTypeResult::forDefault());
    Type inheritedTy;
    switch (result) {
    case InheritedTypeResult::Inherited:
      inheritedTy = result.getInheritedType();
      break;
    case InheritedTypeResult::Suppressed:
    case InheritedTypeResult::Default:
      continue;
    }

    if (inheritedTy)
      if (auto kp = inheritedTy->getKnownProtocol())
        if (getInvertibleProtocolKind(*kp))
          return true;
  }
  return false;
}

bool Decl::isObjCImplementation() const {
  return getAttrs().hasAttribute<ObjCImplementationAttr>(/*AllowInvalid=*/true);
}

PatternBindingDecl::PatternBindingDecl(SourceLoc StaticLoc,
                                       StaticSpellingKind StaticSpelling,
                                       SourceLoc VarLoc,
                                       unsigned NumPatternEntries,
                                       DeclContext *Parent)
  : Decl(DeclKind::PatternBinding, Parent),
    StaticLoc(StaticLoc), VarLoc(VarLoc) {
  Bits.PatternBindingDecl.IsStatic = StaticLoc.isValid();
  Bits.PatternBindingDecl.StaticSpelling =
       static_cast<unsigned>(StaticSpelling);
  Bits.PatternBindingDecl.NumPatternEntries = NumPatternEntries;
}

PatternBindingDecl *
PatternBindingDecl::create(ASTContext &Ctx, SourceLoc StaticLoc,
                           StaticSpellingKind StaticSpelling, SourceLoc VarLoc,
                           Pattern *Pat, SourceLoc EqualLoc, Expr *E,
                           DeclContext *Parent) {
  // We can provide a null context, 'create' will fill it in for us.
  // FIXME: This seems dubious, see the comment in 'create'.
  auto PBE = PatternBindingEntry(Pat, EqualLoc, E, /*InitContext*/ nullptr);
  return create(Ctx, StaticLoc, StaticSpelling, VarLoc, PBE, Parent);
}

PatternBindingDecl *PatternBindingDecl::createImplicit(
    ASTContext &Ctx, StaticSpellingKind StaticSpelling, Pattern *Pat, Expr *E,
    DeclContext *Parent, SourceLoc VarLoc) {
  auto *Result = create(Ctx, /*StaticLoc*/ SourceLoc(), StaticSpelling, VarLoc,
                        Pat, /*EqualLoc*/ SourceLoc(), E, Parent);
  Result->setImplicit();
  return Result;
}

PatternBindingDecl *PatternBindingDecl::createForDebugger(
    ASTContext &Ctx, StaticSpellingKind StaticSpelling, Pattern *Pat, Expr *E,
    DeclContext *Parent) {
  auto *Result = createImplicit(Ctx, StaticSpelling, Pat, E, Parent);
  Result->Bits.PatternBindingDecl.IsDebugger = true;
  for (auto &entry : Result->getMutablePatternList()) {
    entry.setFromDebugger();
  }
  return Result;
}

PatternBindingDecl *
PatternBindingDecl::create(ASTContext &Ctx, SourceLoc StaticLoc,
                           StaticSpellingKind StaticSpelling,
                           SourceLoc VarLoc,
                           ArrayRef<PatternBindingEntry> PatternList,
                           DeclContext *Parent) {
  size_t Size = totalSizeToAlloc<PatternBindingEntry>(PatternList.size());
  void *D = allocateMemoryForDecl<PatternBindingDecl>(Ctx, Size,
                                                      /*ClangNode*/false);
  auto PBD = ::new (D) PatternBindingDecl(StaticLoc, StaticSpelling, VarLoc,
                                          PatternList.size(), Parent);
  // Set up the patterns.
  std::uninitialized_copy(PatternList.begin(), PatternList.end(),
                          PBD->getTrailingObjects<PatternBindingEntry>());

  for (auto idx : range(PBD->getNumPatternEntries())) {
    auto *initContext = PBD->getInitContext(idx);

    // FIXME: We ought to reconsider this since it won't recontextualize any
    // closures/decls present in the initialization expr. This currently should
    // only affect implicit code though.
    if (!initContext && !Parent->isLocalContext())
      initContext = PatternBindingInitializer::create(Parent);

    // We need to call setPattern to ensure the VarDecls in the pattern have
    // the PatternBindingDecl set as their parent. We also need to call
    // setInitContext to setup the context.
    PBD->setPattern(idx, PBD->getPattern(idx));
    PBD->setInitContext(idx, initContext);
  }
  return PBD;
}

PatternBindingDecl *PatternBindingDecl::createDeserialized(
                      ASTContext &Ctx, SourceLoc StaticLoc,
                      StaticSpellingKind StaticSpelling,
                      SourceLoc VarLoc,
                      unsigned NumPatternEntries,
                      DeclContext *Parent) {
  size_t Size = totalSizeToAlloc<PatternBindingEntry>(NumPatternEntries);
  void *D = allocateMemoryForDecl<PatternBindingDecl>(Ctx, Size,
                                                      /*ClangNode*/false);
  auto PBD = ::new (D) PatternBindingDecl(StaticLoc, StaticSpelling, VarLoc,
                                          NumPatternEntries, Parent);
  for (auto &entry : PBD->getMutablePatternList()) {
    entry = PatternBindingEntry(/*Pattern*/ nullptr, /*EqualLoc*/ SourceLoc(),
                                /*Init*/ nullptr, /*InitContext*/ nullptr);
  }
  return PBD;
}

PatternBindingInitializer *
PatternBindingInitializer::createDeserialized(PatternBindingDecl *PBD,
                                              unsigned index) {
  auto *init = PatternBindingInitializer::create(PBD->getDeclContext());
  init->setBinding(PBD, index);
  return init;
}

ParamDecl *PatternBindingInitializer::getImplicitSelfDecl() const {
  if (SelfParam)
    return SelfParam;

  if (auto singleVar = getInitializedLazyVar()) {
    auto DC = singleVar->getDeclContext();
    if (DC->isTypeContext()) {
      auto specifier = (DC->getDeclaredInterfaceType()->hasReferenceSemantics()
                        ? ParamSpecifier::Default
                        : ParamSpecifier::InOut);

      ASTContext &C = DC->getASTContext();
      auto *mutableThis = const_cast<PatternBindingInitializer *>(this);
      auto *LazySelfParam = new (C) ParamDecl(SourceLoc(), SourceLoc(),
                                    Identifier(), singleVar->getLoc(),
                                    C.Id_self, mutableThis);
      LazySelfParam->setImplicit();
      LazySelfParam->setSpecifier(specifier);
      LazySelfParam->setInterfaceType(DC->getSelfInterfaceType());

      // Lazy members of actors have an isolated 'self', assuming there is
      // no "nonisolated" attribute.
      if (auto nominal = DC->getSelfNominalTypeDecl()) {
        if (nominal->isActor() &&
            !singleVar->getAttrs().hasAttribute<NonisolatedAttr>())
          LazySelfParam->setIsolated();
      }

      mutableThis->SelfParam = LazySelfParam;
    }
  }

  return SelfParam;
}

void PatternBindingInitializer::setBinding(PatternBindingDecl *binding,
                                           unsigned bindingIndex) {
  assert(binding);
  assert(!Binding || Binding == binding &&
         "Cannot change the binding after the fact");
  assert(!Binding || SpareBits == bindingIndex &&
         "Cannot change the binding index after the fact");
  setParent(binding->getDeclContext());
  Binding = binding;
  SpareBits = bindingIndex;
}

VarDecl *PatternBindingInitializer::getInitializedLazyVar() const {
  if (auto binding = getBinding()) {
    if (auto var = binding->getSingleVar()) {
      if (var->getAttrs().hasAttribute<LazyAttr>())
        return var;
    }
  }
  return nullptr;
}

unsigned PatternBindingDecl::getPatternEntryIndexForVarDecl(const VarDecl *VD) const {
  assert(VD && "Cannot find a null VarDecl");
  
  auto List = getPatternList();
  if (List.size() == 1) {
    assert(List[0].getPattern()->containsVarDecl(VD) &&
           "Single entry PatternBindingDecl is set up wrong");
    return 0;
  }
  
  unsigned Result = 0;
  for (auto entry : List) {
    if (entry.getPattern()->containsVarDecl(VD))
      return Result;
    ++Result;
  }
  
  assert(0 && "PatternBindingDecl doesn't bind the specified VarDecl!");
  return ~0U;
}

Expr *PatternBindingEntry::getOriginalInit() const {
  return InitContextAndFlags.getInt().contains(PatternFlags::IsText)
             ? nullptr
             : InitExpr.originalInit;
}

SourceRange PatternBindingEntry::getOriginalInitRange() const {
  if (auto *i = getOriginalInit())
    return i->getSourceRange();
  return SourceRange();
}

void PatternBindingEntry::setOriginalInit(Expr *E) {
  InitExpr.originalInit = E;
  InitContextAndFlags.setInt(InitContextAndFlags.getInt() -
                             PatternFlags::IsText);
}

bool PatternBindingEntry::isInitialized(bool onlyExplicit) const {
  // Directly initialized.
  if (getInit() && (!onlyExplicit || getEqualLoc().isValid()))
    return true;

  // Initialized via a property wrapper.
  if (auto var = getPattern()->getSingleVar()) {
    auto customAttrs = var->getAttachedPropertyWrappers();
    if (customAttrs.size() > 0 && customAttrs[0]->hasArgs())
      return true;
  }

  return false;
}

void PatternBindingEntry::setInit(Expr *E) {
  auto F = PatternAndFlags.getInt();
  if (E) {
    PatternAndFlags.setInt(F - Flags::Removed);
  } else {
    PatternAndFlags.setInt(F | Flags::Removed);
  }
  InitExpr.initAfterSynthesis.setPointer(E);
  InitContextAndFlags.setInt(InitContextAndFlags.getInt() -
                             PatternFlags::IsText);
}

VarDecl *PatternBindingEntry::getAnchoringVarDecl() const {
  SmallVector<VarDecl *, 8> variables;
  getPattern()->collectVariables(variables);
  if (variables.empty())
    return nullptr;
  return variables[0];
}

SourceLoc PatternBindingEntry::getLastAccessorEndLoc() const {
  SourceLoc lastAccessorEnd;
  getPattern()->forEachVariable([&](VarDecl *var) {
    auto accessorsEndLoc = var->getBracesRange().End;
    if (accessorsEndLoc.isValid())
      lastAccessorEnd = accessorsEndLoc;
  });
  return lastAccessorEnd;
}

SourceLoc PatternBindingEntry::getStartLoc() const {
  return getPattern()->getStartLoc();
}

SourceLoc PatternBindingEntry::getEndLoc(bool omitAccessors) const {
  // Accessors are last
  if (!omitAccessors) {
    const auto lastAccessorEnd = getLastAccessorEndLoc();
    if (lastAccessorEnd.isValid())
      return lastAccessorEnd;
  }
  const auto initEnd = getOriginalInitRange().End;
  if (initEnd.isValid())
    return initEnd;

  return getPattern()->getEndLoc();
}

SourceRange PatternBindingEntry::getSourceRange(bool omitAccessors) const {
  const SourceLoc startLoc = getStartLoc();
  if (startLoc.isInvalid())
    return SourceRange();
  const SourceLoc endLoc = getEndLoc(omitAccessors);
  if (endLoc.isInvalid())
    return SourceRange();
  return SourceRange(startLoc, endLoc);
}

bool PatternBindingEntry::hasInitStringRepresentation() const {
  if (InitContextAndFlags.getInt().contains(PatternFlags::IsText))
    return !InitStringRepresentation.empty();
  return getOriginalInit() && getOriginalInit()->getSourceRange().isValid();
}

StringRef PatternBindingEntry::getInitStringRepresentation(
  SmallVectorImpl<char> &scratch) const {

  assert(hasInitStringRepresentation() &&
         "must check if pattern has string representation");

  if (InitContextAndFlags.getInt().contains(PatternFlags::IsText) &&
      !InitStringRepresentation.empty())
    return InitStringRepresentation;
  auto &ctx = getAnchoringVarDecl()->getASTContext();
  auto init = getOriginalInit();
  return extractInlinableText(ctx, init, scratch);
}

SourceRange PatternBindingDecl::getSourceRange() const {
  SourceLoc startLoc = getStartLoc();
  SourceLoc endLoc = getPatternList().empty()
      ? SourceLoc()
      : getPatternList().back().getSourceRange().End;
  if (startLoc.isValid() != endLoc.isValid()) return SourceRange();
  return { startLoc, endLoc };
}

static StaticSpellingKind getCorrectStaticSpellingForDecl(const Decl *D) {
  if (auto classDecl = D->getDeclContext()->getSelfClassDecl()) {
    if (!classDecl->isActor())
      return StaticSpellingKind::KeywordClass;
  }

  return StaticSpellingKind::KeywordStatic;
}

StaticSpellingKind PatternBindingDecl::getCorrectStaticSpelling() const {
  if (!isStatic())
    return StaticSpellingKind::None;
  if (getStaticSpelling() != StaticSpellingKind::None)
    return getStaticSpelling();

  return getCorrectStaticSpellingForDecl(this);
}

bool PatternBindingDecl::isAsyncLet() const {
  if (auto var = getAnchoringVarDecl(0))
    return var->isAsyncLet();

  // Check for "async let _: <Type> = <expression>" pattern.
  auto *pattern = getPatternList()[0].getPattern();
  if (auto *typedPattern = dyn_cast<TypedPattern>(pattern)) {
    auto *anyPattern = dyn_cast<AnyPattern>(typedPattern->getSubPattern());
    return anyPattern && anyPattern->isAsyncLet();
  }

  // Check for "async let _ = <expression>" pattern.
  if (auto *anyPattern = dyn_cast<AnyPattern>(pattern)) {
    return anyPattern->isAsyncLet();
  }

  return false;
}

ActorIsolation
PatternBindingDecl::getInitializerIsolation(unsigned i) const {
  auto *var = getPatternList()[i].getAnchoringVarDecl();
  if (!var)
    return ActorIsolation::forUnspecified();

  return var->getInitializerIsolation();
}

Expr *PatternBindingDecl::getContextualizedInit(unsigned i) const {
  auto *mutableThis = const_cast<PatternBindingDecl *>(this);
  return evaluateOrDefault(
      getASTContext().evaluator,
      PatternBindingCheckedAndContextualizedInitRequest{mutableThis, i},
      nullptr);
}

Expr *PatternBindingDecl::getCheckedAndContextualizedInit(unsigned i) const {
  auto *expr = getContextualizedInit(i);

  if (auto *initContext = getInitContext(i)) {
    // Property wrapper isolation is checked separately while
    // synthesizing the backing property wrapper initializer.
    auto *var = getSingleVar();
    if (!(var && var->hasAttachedPropertyWrapper())) {
      (void)getInitializerIsolation(i);
    }

    // Effects checking for 'async' needs actor isolation to be
    // computed. Always run effects checking after the actor
    // isolation checker.
    evaluateOrDefault(
        getASTContext().evaluator,
        CheckInitEffectsRequest{initContext, expr},
        {});
  }

  return expr;
}

Expr *PatternBindingDecl::getCheckedAndContextualizedExecutableInit(
    unsigned i) const {
  (void)getCheckedAndContextualizedInit(i);
  return getExecutableInit(i);
}

bool PatternBindingDecl::hasStorage() const {
  // Walk the pattern, to check to see if any of the VarDecls included in it
  // have storage.
  for (auto entry : getPatternList())
    if (entry.getPattern()->hasStorage())
      return true;
  return false;
}

const PatternBindingEntry *
PatternBindingDecl::getCheckedPatternBindingEntry(unsigned i) const {
  return evaluateOrDefault(
      getASTContext().evaluator,
      PatternBindingEntryRequest{const_cast<PatternBindingDecl *>(this), i},
      nullptr);
}

void PatternBindingDecl::setPattern(unsigned i, Pattern *P,
                                    bool isFullyValidated) {
  auto PatternList = getMutablePatternList();
  PatternList[i].setPattern(P);

  // Make sure that any VarDecl's contained within the pattern know about this
  // PatternBindingDecl as their parent.
  if (P) {
    P->forEachVariable([&](VarDecl *VD) {
      if (!VD->isCaptureList())
        VD->setParentPatternBinding(this);
    });

    if (isFullyValidated)
      PatternList[i].setFullyValidated();
  }
}

VarDecl *PatternBindingDecl::getSingleVar() const {
  if (getNumPatternEntries() == 1)
    return getPatternList()[0].getPattern()->getSingleVar();
  return nullptr;
}

VarDecl *PatternBindingDecl::getAnchoringVarDecl(unsigned i) const {
  return getPatternList()[i].getAnchoringVarDecl();
}

bool VarDecl::isInitExposedToClients() const {
  // 'lazy' initializers are emitted inside the getter, which is never
  // @inlinable.
  if (getAttrs().hasAttribute<LazyAttr>())
    return false;

  return hasInitialValue() && isLayoutExposedToClients();
}

bool VarDecl::isLayoutExposedToClients() const {
  auto parent = dyn_cast<NominalTypeDecl>(getDeclContext());
  if (!parent) return false;
  if (isStatic()) return false;


  auto nominalAccess =
    parent->getFormalAccessScope(/*useDC=*/nullptr,
                                 /*treatUsableFromInlineAsPublic=*/true);
  if (!nominalAccess.isPublic()) return false;

  if (!parent->getAttrs().hasAttribute<FrozenAttr>() &&
      !parent->getAttrs().hasAttribute<FixedLayoutAttr>())
    return false;

  if (!hasStorage() &&
      !getAttrs().hasAttribute<LazyAttr>() &&
      !hasAttachedPropertyWrapper()) {
    return false;
  }

  return true;
}

/// Check whether the given type representation will be
/// default-initializable.
static bool isDefaultInitializable(const TypeRepr *typeRepr, ASTContext &ctx) {
  // Look through most attributes.
  if (const auto attributed = dyn_cast<AttributedTypeRepr>(typeRepr)) {
    // Ownership kinds have optionalness requirements.
    // FIXME: this is checking for *SIL* ownership; normal weak/unowned/etc.
    // are decl attributes.  Is this actually an important check to do?
    if (optionalityOf(attributed->getSILOwnership()) ==
        ReferenceOwnershipOptionality::Required)
      return true;

    return isDefaultInitializable(attributed->getTypeRepr(), ctx);
  }

  // Optional types are default-initializable.
  if (isa<OptionalTypeRepr>(typeRepr) ||
      isa<ImplicitlyUnwrappedOptionalTypeRepr>(typeRepr))
    return true;

  // Also support the desugared 'Optional<T>' spelling.
  if (!ctx.isSwiftVersionAtLeast(5)) {
    if (typeRepr->isSimpleUnqualifiedIdentifier(ctx.Id_Void)) {
      return true;
    }

    if (auto *unqualIdentRepr = dyn_cast<UnqualifiedIdentTypeRepr>(typeRepr)) {
      if (unqualIdentRepr->getNumGenericArgs() == 1 &&
          unqualIdentRepr->getNameRef().getBaseIdentifier() == ctx.Id_Optional)
        return true;
    }
  }

  // Tuple types are default-initializable if all of their element
  // types are.
  if (const auto tuple = dyn_cast<TupleTypeRepr>(typeRepr)) {
    for (const auto &elt : tuple->getElements()) {
      if (!isDefaultInitializable(elt.Type, ctx))
        return false;
    }

    return true;
  }

  // Not default initializable.
  return false;
}

// @NSManaged properties never get default initialized, nor do debugger
// variables and immutable properties.
bool Pattern::isNeverDefaultInitializable() const {
  bool result = false;

  forEachVariable([&](const VarDecl *var) {
    if (var->getAttrs().hasAttribute<NSManagedAttr>())
      return;

    if (var->isDebuggerVar() ||
        var->isLet())
      result = true;
  });

  return result;
}

bool PatternBindingDecl::isDefaultInitializableViaPropertyWrapper(unsigned i) const {
  if (auto singleVar = getSingleVar()) {
    if (auto wrapperInfo = singleVar->getAttachedPropertyWrapperTypeInfo(0)) {
      if (wrapperInfo.defaultInit)
        return true;
    }
  }

  return false;
}

bool PatternBindingDecl::isDefaultInitializable(unsigned i) const {
  const auto entry = getPatternList()[i];

  // If it has an initializer expression, this is trivially true.
  if (entry.isInitialized())
    return true;

  // If the outermost attached property wrapper vends an `init()`, use that
  // for default initialization.
  if (isDefaultInitializableViaPropertyWrapper(i))
    return true;

  // If one of the attached wrappers is missing a wrappedValue
  // initializer, cannot default-initialize.
  if (auto singleVar = getSingleVar()) {
    if (auto wrapperInfo = singleVar->getAttachedPropertyWrapperTypeInfo(0)) {
      if (!singleVar->allAttachedPropertyWrappersHaveWrappedValueInit())
        return false;
    }
  }

  if (entry.getPattern()->isNeverDefaultInitializable())
    return false;

  auto &ctx = getASTContext();

  // If the pattern is typed as optional (or tuples thereof), it is
  // default initializable.
  if (const auto typedPattern = dyn_cast<TypedPattern>(entry.getPattern())) {
    if (const auto typeRepr = typedPattern->getTypeRepr()) {
      if (::isDefaultInitializable(typeRepr, ctx))
        return true;
    } else if (typedPattern->isImplicit()) {
      // Lazy vars have implicit storage assigned to back them. Because the
      // storage is implicit, the pattern is typed and has a TypeLoc, but not a
      // TypeRepr.
      //
      // All lazy storage is implicitly default initializable, though, because
      // lazy backing storage is optional.
      if (const auto *varDecl = typedPattern->getSingleVar()) {
        // Lazy storage is never user accessible.
        if (!varDecl->isUserAccessible()) {
          if (typedPattern->hasType() &&
              typedPattern->getType()->getOptionalObjectType()) {
            return true;
          }
        }
      }
    }
  }

  // Otherwise, we can't default initialize this binding.
  return false;
}


bool PatternBindingDecl::isComputingPatternBindingEntry(
    const VarDecl *vd) const {
  unsigned i = getPatternEntryIndexForVarDecl(vd);
  return getASTContext().evaluator.hasActiveRequest(
      PatternBindingEntryRequest{const_cast<PatternBindingDecl *>(this), i});
}

bool PatternBindingDecl::isExplicitlyInitialized(unsigned i) const {
  const auto &entry = getPatternList()[i];
  return entry.isInitialized(/*onlyExplicit=*/true);
}

SourceLoc PatternBindingDecl::getEqualLoc(unsigned i) const {
  const auto &entry = getPatternList()[i];
  return entry.getEqualLoc();
}

SourceLoc TopLevelCodeDecl::getStartLoc() const {
  return Body ? Body->getStartLoc() : SourceLoc();
}

SourceRange TopLevelCodeDecl::getSourceRange() const {
  return Body? Body->getSourceRange() : SourceRange();
}

static bool isPolymorphic(const AbstractStorageDecl *storage) {
  if (storage->shouldUseObjCDispatch())
    return true;


  // Imported declarations behave like they are dynamic, even if they're
  // not marked as such explicitly.
  if (storage->isObjC() && storage->hasClangNode())
    return true;

  if (auto *classDecl = dyn_cast<ClassDecl>(storage->getDeclContext())) {
    // Accesses to members of foreign reference types should be made directly
    // to storage as these are references to clang records which are not allowed
    // to have dynamic dispatch.
    if (storage->isFinal() || classDecl->isFinal() ||
        classDecl->isForeignReferenceType())
      return false;

    return true;
  }

  if (isa<ProtocolDecl>(storage->getDeclContext()))
    return true;

  return false;
}

/// Returns true iff a defer's storage access kind should always
/// match the access kind of its immediately enclosing function.
///
/// In Swift 5 and earlier, this was not true, meaning that property observers,
/// etc, would be invoked in initializers or deinitializers if a property access
/// happens within a defer, but not when outside the defer.
static bool deferMatchesEnclosingAccess(const FuncDecl *defer) {
  assert(defer->isDeferBody());

  // In Swift 6+, then yes.
  if (defer->getASTContext().isSwiftVersionAtLeast(6))
    return true;

  // If the defer is part of a function that is a member of an actor or
  // concurrency-aware type, then yes.
  if (auto *deferContext = defer->getParent()) {
    if (auto *funcContext = deferContext->getParent()) {
      if (auto *type = funcContext->getSelfNominalTypeDecl()) {
        if (type->isAnyActor())
          return true;

        auto isolation = getActorIsolation(type);
        switch (isolation) {
          case ActorIsolation::Unspecified:
          case ActorIsolation::NonisolatedUnsafe:
            break;

          case ActorIsolation::GlobalActor:
            if (isolation.preconcurrency())
              break;

            return true;

          case ActorIsolation::ActorInstance:
          case ActorIsolation::Nonisolated:
          case ActorIsolation::Erased: // really can't happen
            return true;
        }
      }
    }
  }

  return false;
}

static bool isDirectToStorageAccess(const DeclContext *UseDC,
                                    const VarDecl *var, bool isAccessOnSelf) {
  if (!var->hasStorage())
    return false;

  auto *AFD = dyn_cast_or_null<AbstractFunctionDecl>(UseDC);
  if (AFD == nullptr)
    return false;

  // Check if this is a function representing a defer.
  if (auto *func = dyn_cast<FuncDecl>(AFD))
    if (func->isDeferBody() && deferMatchesEnclosingAccess(func))
      return isDirectToStorageAccess(func->getParent(), var, isAccessOnSelf);

  // The property reference is for immediate class, not a derived class.
  if (AFD->getParent()->getSelfNominalTypeDecl() !=
      var->getDeclContext()->getSelfNominalTypeDecl())
    return false;

  // If the storage is resilient, we cannot access it directly at all.
  if (var->isResilient(UseDC->getParentModule(),
                       UseDC->getResilienceExpansion()))
    return var->getModuleContext()->getBypassResilience();

  if (isa<ConstructorDecl>(AFD) || isa<DestructorDecl>(AFD)) {
    // The access must also be a member access on 'self' in all language modes.
    if (!isAccessOnSelf)
      return false;

    return true;
  } else if (auto *accessor = dyn_cast<AccessorDecl>(AFD)) {
    // The accessor must be for the variable itself.
    if (accessor->getStorage() != var)
      return false;

    // In Swift 5 and later, the access must also be a member access on 'self'.
    if (!isAccessOnSelf &&
        var->getDeclContext()->isTypeContext() &&
        var->getASTContext().isSwiftVersionAtLeast(5))
      return false;

    // As a special case, 'read' and 'modify' coroutines with forced static
    // dispatch must use ordinary semantics, so that the 'modify' coroutine for a
    // 'dynamic' property uses Objective-C message sends and not direct access to
    // storage.
    if (accessor->hasForcedStaticDispatch())
      return false;

    return true;
  }

  return false;
}

/// Determines the access semantics to use in a DeclRefExpr or
/// MemberRefExpr use of this value in the specified context.
AccessSemantics
ValueDecl::getAccessSemanticsFromContext(const DeclContext *UseDC,
                                         bool isAccessOnSelf) const {
  if (auto *var = dyn_cast<VarDecl>(this))
    if (isDirectToStorageAccess(UseDC, var, isAccessOnSelf))
      return AccessSemantics::DirectToStorage;

  // Otherwise, it's a semantically normal access.  The client should be
  // able to figure out the most efficient way to do this access.
  return AccessSemantics::Ordinary;
}

static AccessStrategy
getDirectReadAccessStrategy(const AbstractStorageDecl *storage) {
  switch (storage->getReadImpl()) {
  case ReadImplKind::Stored:
    return AccessStrategy::getStorage();
  case ReadImplKind::Inherited:
    // TODO: maybe add a specific strategy for this?
    return AccessStrategy::getAccessor(AccessorKind::Get,
                                       /*dispatch*/ false);
  case ReadImplKind::Get:
    return AccessStrategy::getAccessor(AccessorKind::Get,
                                       /*dispatch*/ false);
  case ReadImplKind::Address:
    return AccessStrategy::getAccessor(AccessorKind::Address,
                                       /*dispatch*/ false);
  case ReadImplKind::Read:
    return AccessStrategy::getAccessor(AccessorKind::Read,
                                       /*dispatch*/ false);
  case ReadImplKind::Read2:
    return AccessStrategy::getAccessor(AccessorKind::Read2,
                                       /*dispatch*/ false);
  }
  llvm_unreachable("bad impl kind");
}

static AccessStrategy
getDirectWriteAccessStrategy(const AbstractStorageDecl *storage) {
  switch (storage->getWriteImpl()) {
  case WriteImplKind::Immutable: {
    if (storage->hasInitAccessor())
      return AccessStrategy::getAccessor(AccessorKind::Init,
                                         /*dispatch=*/false);

    assert(isa<VarDecl>(storage) && cast<VarDecl>(storage)->isLet() &&
           "mutation of a immutable variable that isn't a let");
    return AccessStrategy::getStorage();
  }
  case WriteImplKind::Stored:
    return AccessStrategy::getStorage();
  case WriteImplKind::StoredWithObservers:
    // TODO: maybe add a specific strategy for this?
    return AccessStrategy::getAccessor(AccessorKind::Set,
                                       /*dispatch*/ false);
  case WriteImplKind::InheritedWithObservers:
    // TODO: maybe add a specific strategy for this?
    return AccessStrategy::getAccessor(AccessorKind::Set,
                                       /*dispatch*/ false);
  case WriteImplKind::Set:
    return AccessStrategy::getAccessor(AccessorKind::Set,
                                       /*dispatch*/ false);
  case WriteImplKind::MutableAddress:
    return AccessStrategy::getAccessor(AccessorKind::MutableAddress,
                                       /*dispatch*/ false);
  case WriteImplKind::Modify:
    return AccessStrategy::getAccessor(AccessorKind::Modify,
                                       /*dispatch*/ false);
  case WriteImplKind::Modify2:
    return AccessStrategy::getAccessor(AccessorKind::Modify2,
                                       /*dispatch*/ false);
  }
  llvm_unreachable("bad impl kind");
}

static AccessStrategy
getOpaqueReadAccessStrategy(const AbstractStorageDecl *storage, bool dispatch);
static AccessStrategy
getOpaqueWriteAccessStrategy(const AbstractStorageDecl *storage, bool dispatch);

static AccessStrategy
getDirectReadWriteAccessStrategy(const AbstractStorageDecl *storage) {
  switch (storage->getReadWriteImpl()) {
  case ReadWriteImplKind::Immutable:
    assert(isa<VarDecl>(storage) && cast<VarDecl>(storage)->isLet() &&
           "mutation of a immutable variable that isn't a let");
    return AccessStrategy::getStorage();
  case ReadWriteImplKind::Stored: {
    // If the storage isDynamic (and not @objc) use the accessors.
    if (storage->shouldUseNativeDynamicDispatch())
      return AccessStrategy::getMaterializeToTemporary(
          getOpaqueReadAccessStrategy(storage, false),
          getOpaqueWriteAccessStrategy(storage, false));
    return AccessStrategy::getStorage();
  }
  case ReadWriteImplKind::MutableAddress:
    return AccessStrategy::getAccessor(AccessorKind::MutableAddress,
                                       /*dispatch*/ false);
  case ReadWriteImplKind::Modify:
    return AccessStrategy::getAccessor(AccessorKind::Modify,
                                       /*dispatch*/ false);
  case ReadWriteImplKind::Modify2:
    return AccessStrategy::getAccessor(AccessorKind::Modify2,
                                       /*dispatch*/ false);
  case ReadWriteImplKind::StoredWithDidSet:
  case ReadWriteImplKind::InheritedWithDidSet:
    if (storage->requiresOpaqueModify2Coroutine() &&
        storage->getParsedAccessor(AccessorKind::DidSet)->isSimpleDidSet()) {
      return AccessStrategy::getAccessor(AccessorKind::Modify2,
                                         /*dispatch*/ false);
    } else if (storage->requiresOpaqueModifyCoroutine() &&
               storage->getParsedAccessor(AccessorKind::DidSet)
                   ->isSimpleDidSet()) {
      return AccessStrategy::getAccessor(AccessorKind::Modify,
                                         /*dispatch*/ false);
    } else {
      return AccessStrategy::getMaterializeToTemporary(
          getDirectReadAccessStrategy(storage),
          getDirectWriteAccessStrategy(storage));
    }
  case ReadWriteImplKind::MaterializeToTemporary:
    return AccessStrategy::getMaterializeToTemporary(
                                       getDirectReadAccessStrategy(storage),
                                       getDirectWriteAccessStrategy(storage));
  }
  llvm_unreachable("bad impl kind");
}

static AccessStrategy
getOpaqueReadAccessStrategy(const AbstractStorageDecl *storage, bool dispatch) {
  if (storage->requiresOpaqueRead2Coroutine())
    return AccessStrategy::getAccessor(AccessorKind::Read2, dispatch);
  if (storage->requiresOpaqueReadCoroutine())
    return AccessStrategy::getAccessor(AccessorKind::Read, dispatch);
  return AccessStrategy::getAccessor(AccessorKind::Get, dispatch);
}

static AccessStrategy
getOpaqueWriteAccessStrategy(const AbstractStorageDecl *storage, bool dispatch) {
  if (storage->hasInitAccessor() && !storage->getAccessor(AccessorKind::Set))
    return AccessStrategy::getAccessor(AccessorKind::Init, dispatch);
  return AccessStrategy::getAccessor(AccessorKind::Set, dispatch);
}

static AccessStrategy
getOpaqueReadWriteAccessStrategy(const AbstractStorageDecl *storage,
                                 bool dispatch) {
  if (storage->requiresOpaqueModify2Coroutine())
    return AccessStrategy::getAccessor(AccessorKind::Modify2, dispatch);
  if (storage->requiresOpaqueModifyCoroutine())
    return AccessStrategy::getAccessor(AccessorKind::Modify, dispatch);
  return AccessStrategy::getMaterializeToTemporary(
           getOpaqueReadAccessStrategy(storage, dispatch),
           getOpaqueWriteAccessStrategy(storage, dispatch));
}

static AccessStrategy
getOpaqueAccessStrategy(const AbstractStorageDecl *storage,
                        AccessKind accessKind, bool dispatch) {
  switch (accessKind) {
  case AccessKind::Read:
    return getOpaqueReadAccessStrategy(storage, dispatch);
  case AccessKind::Write:
    return getOpaqueWriteAccessStrategy(storage, dispatch);
  case AccessKind::ReadWrite:
    return getOpaqueReadWriteAccessStrategy(storage, dispatch);
  }
  llvm_unreachable("bad access kind");
}

AccessStrategy
AbstractStorageDecl::getAccessStrategy(AccessSemantics semantics,
                                       AccessKind accessKind,
                                       ModuleDecl *module,
                                       ResilienceExpansion expansion) const {
  switch (semantics) {
  case AccessSemantics::DirectToStorage:
    assert(hasStorage() || getASTContext().Diags.hadAnyError());
    return AccessStrategy::getStorage();

  case AccessSemantics::DistributedThunk:
    return AccessStrategy::getDistributedThunkDispatchStrategy();

  case AccessSemantics::Ordinary:
    // Skip these checks for local variables, both because they're unnecessary
    // and because we won't necessarily have computed access.
    if (!getDeclContext()->isLocalContext()) {
      // If the property is defined in a non-final class or a protocol, the
      // accessors are dynamically dispatched, and we cannot do direct access.
      if (isPolymorphic(this))
        return getOpaqueAccessStrategy(this, accessKind, /*dispatch*/ true);

      if (shouldUseNativeDynamicDispatch())
        return getOpaqueAccessStrategy(this, accessKind, /*dispatch*/ false);

      // If the storage is resilient from the given module and resilience
      // expansion, we cannot use direct access.
      //
      // If we end up here with a stored property of a type that's resilient
      // from some resilience domain, we cannot do direct access.
      //
      // As an optimization, we do want to perform direct accesses of stored
      // properties declared inside the same resilience domain as the access
      // context.
      //
      // This is done by using DirectToStorage semantics above, with the
      // understanding that the access semantics are with respect to the
      // resilience domain of the accessor's caller.
      bool resilient;
      if (module)
        resilient = isResilient(module, expansion);
      else
        resilient = isResilient();

      if (resilient)
        return getOpaqueAccessStrategy(this, accessKind, /*dispatch*/ false);
    }

    LLVM_FALLTHROUGH;

  case AccessSemantics::DirectToImplementation:
    switch (accessKind) {
    case AccessKind::Read:
      return getDirectReadAccessStrategy(this);
    case AccessKind::Write:
      return getDirectWriteAccessStrategy(this);
    case AccessKind::ReadWrite:
      return getDirectReadWriteAccessStrategy(this);
    }
    llvm_unreachable("bad access kind");

  }
  llvm_unreachable("bad access semantics");
}

bool AbstractStorageDecl::requiresOpaqueAccessors() const {
  // Subscripts always require opaque accessors, so don't even kick off
  // a request.
  auto *var = dyn_cast<VarDecl>(this);
  if (var == nullptr)
    return true;

  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    RequiresOpaqueAccessorsRequest{const_cast<VarDecl *>(var)},
    false);
}

bool AbstractStorageDecl::requiresOpaqueAccessor(AccessorKind kind) const {
  switch (kind) {
  case AccessorKind::Get:
  case AccessorKind::DistributedGet:
    return requiresOpaqueGetter();
  case AccessorKind::Set:
    return requiresOpaqueSetter();
  case AccessorKind::Read:
    return requiresOpaqueReadCoroutine();
  case AccessorKind::Read2:
    return requiresOpaqueRead2Coroutine();
  case AccessorKind::Modify:
    return requiresOpaqueModifyCoroutine();
  case AccessorKind::Modify2:
    return requiresOpaqueModify2Coroutine();

  // Other accessors are never part of the opaque-accessors set.
#define OPAQUE_ACCESSOR(ID, KEYWORD)
#define ACCESSOR(ID) \
  case AccessorKind::ID:
#include "swift/AST/AccessorKinds.def"
    return false;
  }
  llvm_unreachable("bad accessor kind");
}

bool AbstractStorageDecl::requiresOpaqueReadCoroutine() const {
  ASTContext &ctx = getASTContext();
  if (ctx.LangOpts.hasFeature(Feature::CoroutineAccessors))
    return requiresCorrespondingUnderscoredCoroutineAccessor(
        AccessorKind::Read2);
  return getOpaqueReadOwnership() != OpaqueReadOwnership::Owned;
}

bool AbstractStorageDecl::requiresOpaqueRead2Coroutine() const {
  ASTContext &ctx = getASTContext();
  if (!ctx.LangOpts.hasFeature(Feature::CoroutineAccessors))
    return false;
  return getOpaqueReadOwnership() != OpaqueReadOwnership::Owned;
}

bool AbstractStorageDecl::requiresOpaqueModifyCoroutine() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(
      ctx.evaluator,
      RequiresOpaqueModifyCoroutineRequest{
          const_cast<AbstractStorageDecl *>(this), /*isUnderscored=*/true},
      false);
}

bool AbstractStorageDecl::requiresOpaqueModify2Coroutine() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(
      ctx.evaluator,
      RequiresOpaqueModifyCoroutineRequest{
          const_cast<AbstractStorageDecl *>(this), /*isUnderscored=*/false},
      false);
}

AccessorDecl *AbstractStorageDecl::getSynthesizedAccessor(AccessorKind kind) const {
  if (auto *accessor = getAccessor(kind))
    return accessor;

  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    SynthesizeAccessorRequest{const_cast<AbstractStorageDecl *>(this), kind},
    nullptr);
}

AccessorDecl *AbstractStorageDecl::getEffectfulGetAccessor() const {
    if (getAllAccessors().size() != 1)
      return nullptr;

    if (auto accessor = getAccessor(AccessorKind::Get))
      if (accessor->hasAsync() || accessor->hasThrows())
        return accessor;

  return nullptr;
}

bool AbstractStorageDecl::isLessEffectfulThan(AbstractStorageDecl const* other,
                                              EffectKind kind) const {
  bool allowedByOther = false;
  if (auto otherGetter = other->getEffectfulGetAccessor())
    allowedByOther = otherGetter->hasEffect(kind);

  if (auto getter = getEffectfulGetAccessor())
    if (getter->hasEffect(kind) && !allowedByOther)
      return false; // has the effect when other does not; it's more effectful!

  return true; // OK
}

AccessorDecl *AbstractStorageDecl::getOpaqueAccessor(AccessorKind kind) const {
  auto *accessor = getAccessor(kind);
  if (accessor && !accessor->isImplicit())
    return accessor;

  if (!requiresOpaqueAccessors())
    return nullptr;

  if (!requiresOpaqueAccessor(kind))
    return nullptr;

  return getSynthesizedAccessor(kind);
}

ArrayRef<AccessorDecl*> AbstractStorageDecl::getOpaqueAccessors(
    llvm::SmallVectorImpl<AccessorDecl*> &scratch) const {
  visitOpaqueAccessors([&](AccessorDecl *D) { scratch.push_back(D); });
  return scratch;
}

bool AbstractStorageDecl::hasParsedAccessors() const {
  for (auto *accessor : getAllAccessors())
    if (!accessor->isImplicit())
      return true;
  return false;
}

AccessorDecl *AbstractStorageDecl::getParsedAccessor(AccessorKind kind) const {
  auto *accessor = getAccessor(kind);
  if (accessor && !accessor->isImplicit())
    return accessor;

  return nullptr;
}

void AbstractStorageDecl::visitParsedAccessors(
                        llvm::function_ref<void (AccessorDecl*)> visit) const {
  for (auto *accessor : getAllAccessors())
    if (!accessor->isImplicit())
      visit(accessor);
}

void AbstractStorageDecl::visitEmittedAccessors(
                        llvm::function_ref<void (AccessorDecl*)> visit) const {
  visitParsedAccessors(visit);
  visitOpaqueAccessors([&](AccessorDecl *accessor) {
    if (accessor->isImplicit())
      visit(accessor);
  });
}

void AbstractStorageDecl::visitExpectedOpaqueAccessors(
                        llvm::function_ref<void (AccessorKind)> visit) const {
  if (!requiresOpaqueAccessors())
    return;

  if (requiresOpaqueGetter())
    visit(AccessorKind::Get);

  if (requiresOpaqueReadCoroutine())
    visit(AccessorKind::Read);

  if (requiresOpaqueRead2Coroutine())
    visit(AccessorKind::Read2);

  // All mutable storage should have a setter.
  if (requiresOpaqueSetter())
    visit(AccessorKind::Set);

  // Include the modify coroutine if it's required.
  if (requiresOpaqueModifyCoroutine())
    visit(AccessorKind::Modify);

  if (requiresOpaqueModify2Coroutine())
    visit(AccessorKind::Modify2);
}

void AbstractStorageDecl::visitOpaqueAccessors(
                        llvm::function_ref<void (AccessorDecl*)> visit) const {
  visitExpectedOpaqueAccessors([&](AccessorKind kind) {
    auto accessor = getSynthesizedAccessor(kind);
    assert(!accessor->hasForcedStaticDispatch() &&
            "opaque accessor with forced static dispatch?");
    visit(accessor);
  });
}

static bool hasPrivateOrFilePrivateFormalAccess(const Decl *D) {
  if (auto *VD = dyn_cast<ValueDecl>(D))
    return VD->getFormalAccess() <= AccessLevel::FilePrivate;
  return isa<MacroExpansionDecl>(D);
}

/// Returns true if one of the ancestor DeclContexts of this ValueDecl is either
/// marked private or fileprivate or is a local context.
static bool isInPrivateOrLocalContext(const Decl *D) {
  const DeclContext *DC = D->getDeclContext();
  if (!DC->isTypeContext()) {
    assert((DC->isModuleScopeContext() || DC->isLocalContext()) &&
           "unexpected context kind");
    return DC->isLocalContext();
  }

  auto *nominal = DC->getSelfNominalTypeDecl();
  if (nominal == nullptr)
    return false;

  if (hasPrivateOrFilePrivateFormalAccess(nominal))
    return true;
  return isInPrivateOrLocalContext(nominal);
}

bool Decl::isOutermostPrivateOrFilePrivateScope() const {
  return hasPrivateOrFilePrivateFormalAccess(this) &&
         !isInPrivateOrLocalContext(this);
}

bool AbstractStorageDecl::isStrictlyResilient() const {
  return isResilient() && !getModuleContext()->allowNonResilientAccess();
}

bool AbstractStorageDecl::isResilient() const {
  // Check for an explicit @_fixed_layout attribute.
  if (getAttrs().hasAttribute<FixedLayoutAttr>())
    return false;

  // If we're an instance property of a nominal type, query the type.
  if (!isStatic())
    if (auto *nominalDecl = getDeclContext()->getSelfNominalTypeDecl())
      return nominalDecl->isResilient();

  // Non-public global and static variables always have a
  // fixed layout.
  auto accessScope = getFormalAccessScope(/*useDC=*/nullptr,
                                          /*treatUsableFromInlineAsPublic=*/true);
  if (!accessScope.isPublicOrPackage())
    return false;

  return getModuleContext()->isResilient();
}

bool AbstractStorageDecl::isResilient(ModuleDecl *M,
                                      ResilienceExpansion expansion) const {
  switch (expansion) {
  case ResilienceExpansion::Minimal:
    return isResilient();
  case ResilienceExpansion::Maximal:
    if (M == getModuleContext())
      return false;
    return isResilient();
  }
  llvm_unreachable("bad resilience expansion");
}

bool AbstractStorageDecl::isValidKeyPathComponent() const {
  // Check whether we're an ABI compatible override of another property. If we
  // are, then the key path should refer to the base decl instead.
  auto &ctx = getASTContext();
  auto isABICompatibleOverride = evaluateOrDefault(
      ctx.evaluator,
      IsABICompatibleOverrideRequest{const_cast<AbstractStorageDecl *>(this)},
      false);
  return !isABICompatibleOverride;
}

bool AbstractStorageDecl::isGetterMutating() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    IsGetterMutatingRequest{const_cast<AbstractStorageDecl *>(this)}, {});
}

bool AbstractStorageDecl::isSetterMutating() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    IsSetterMutatingRequest{const_cast<AbstractStorageDecl *>(this)}, {});
}

StorageMutability 
AbstractStorageDecl::mutability(const DeclContext *useDC,
                                std::optional<const DeclRefExpr *> base ) const {
  if (auto vd = dyn_cast<VarDecl>(this))
    return vd->mutability(useDC, base);

  auto sd = cast<SubscriptDecl>(this);
  return sd->supportsMutation() ? StorageMutability::Mutable
                                : StorageMutability::Immutable;
}

/// Determine the mutability of this storage declaration when
/// accessed from a given declaration context in Swift.
///
/// This method differs only from 'mutability()' in its handling of
/// 'optional' storage requirements, which lack support for direct
/// writes in Swift.
StorageMutability
AbstractStorageDecl::mutabilityInSwift(
    const DeclContext *useDC,
    std::optional<const DeclRefExpr *> base
) const {
  // TODO: Writing to an optional storage requirement is not supported in Swift.
  if (getAttrs().hasAttribute<OptionalAttr>()) {
    return StorageMutability::Immutable;
  }

  return mutability(useDC, base);
}

OpaqueReadOwnership AbstractStorageDecl::getOpaqueReadOwnership() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    OpaqueReadOwnershipRequest{const_cast<AbstractStorageDecl *>(this)}, {});
}

bool ValueDecl::isInstanceMember() const {
  DeclContext *DC = getDeclContext();
  if (!DC->isTypeContext())
    return false;

  switch (getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::EnumCase:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::PoundDiagnostic:
  case DeclKind::PrecedenceGroup:
  case DeclKind::Missing:
  case DeclKind::MissingMember:
  case DeclKind::MacroExpansion:
    llvm_unreachable("Not a ValueDecl");

  case DeclKind::Class:
  case DeclKind::Enum:
  case DeclKind::Protocol:
  case DeclKind::Struct:
  case DeclKind::TypeAlias:
  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType:
  case DeclKind::OpaqueType:
    // Types are not instance members.
    return false;

  case DeclKind::Constructor:
    // Constructors are not instance members.
    return false;

  case DeclKind::Destructor:
    // Destructors are technically instance members, although they
    // can't actually be referenced as such.
    return true;

  case DeclKind::Func:
  case DeclKind::Accessor:
    // Non-static methods are instance members.
    return !cast<FuncDecl>(this)->isStatic();

  case DeclKind::EnumElement:
  case DeclKind::Param:
    // enum elements and function parameters are not instance members.
    return false;

  case DeclKind::Subscript:
  case DeclKind::Var:
    // Non-static variables and subscripts are instance members.
    return !cast<AbstractStorageDecl>(this)->isStatic();

  case DeclKind::Module:
    // Modules are never instance members.
    return false;

  case DeclKind::Macro:
    // Macros are never instance members.
    return false;

  case DeclKind::BuiltinTuple:
    llvm_unreachable("BuiltinTupleDecl should not end up here");
  }
  llvm_unreachable("bad DeclKind");
}

bool ValueDecl::hasLocalDiscriminator() const {
  // Generic parameters and unnamed parameters never have local discriminators.
  if (isa<GenericTypeParamDecl>(this) ||
      (isa<ParamDecl>(this) && !hasName()))
    return false;

  // Opaque types never have local discriminators.
  if (isa<OpaqueTypeDecl>(this))
    return false;

  // Accessors never have local discriminators.
  if (isa<AccessorDecl>(this))
    return false;

  // Implicit and unnamed declarations never have local discriminators.
  if (getBaseName().isSpecial())
    return false;

  // If we are not in a local context, there's nothing to do.
  if (!getDeclContext()->isLocalContext())
    return false;

  return true;
}

unsigned ValueDecl::getLocalDiscriminator() const {
  // If we have already assigned a local discriminator, we're done.
  if (LocalDiscriminator != InvalidDiscriminator)
    return LocalDiscriminator;

  // If this declaration does not have a local discriminator, use 0 as a
  // stand-in.
  if (!hasLocalDiscriminator())
    return 0;

  // Assign local discriminators in this context.
  ASTContext &ctx = getASTContext();
  evaluateOrDefault(
      ctx.evaluator,
      LocalDiscriminatorsRequest{getDeclContext()}, InvalidDiscriminator);

  // If we don't have a discriminator, and either
  //   1. We have ill-formed code and we're able to assign a discriminator, or
  //   2. We are in a macro expansion buffer
  //
  // then assign the next discriminator now.
  if (LocalDiscriminator == InvalidDiscriminator &&
      (ctx.Diags.hadAnyError() ||
       (getLoc().isValid() &&
        getModuleContext()
                ->getSourceFileContainingLocation(getLoc())
                ->getFulfilledMacroRole() != std::nullopt))) {
    auto discriminator = ctx.getNextDiscriminator(getDeclContext());
    ctx.setMaxAssignedDiscriminator(getDeclContext(), discriminator + 1);
    const_cast<ValueDecl *>(this)->LocalDiscriminator = discriminator;
  }

  assert(LocalDiscriminator != InvalidDiscriminator);

  return LocalDiscriminator;
}

void ValueDecl::setLocalDiscriminator(unsigned index) {
  assert(hasLocalDiscriminator());
  assert(LocalDiscriminator == InvalidDiscriminator &&
         "LocalDiscriminator is set multiple times");
  LocalDiscriminator = index;
}

ValueDecl *ValueDecl::getOverriddenDecl() const {
  auto overridden = getOverriddenDecls();
  if (overridden.empty()) return nullptr;

  // FIXME: Arbitrarily pick the first overridden declaration.
  return overridden.front();
}

ValueDecl *ValueDecl::getOverriddenDeclOrSuperDeinit() const {
  if (auto overridden = getOverriddenDecl()) {
    return overridden;
  }
  if (auto dtor = dyn_cast<DestructorDecl>(this)) {
    return dtor->getSuperDeinit();
  }
  return nullptr;
}

bool ValueDecl::overriddenDeclsComputed() const {
  return LazySemanticInfo.hasOverriddenComputed;
}

bool swift::conflicting(const OverloadSignature& sig1,
                        const OverloadSignature& sig2,
                        bool skipProtocolExtensionCheck) {
  // A member of a protocol extension never conflicts with a member of a
  // protocol.
  if (!skipProtocolExtensionCheck &&
      sig1.InProtocolExtension != sig2.InProtocolExtension)
    return false;

  // If the base names are different, they can't conflict.
  if (sig1.Name.getBaseName() != sig2.Name.getBaseName())
    return false;

  // If one is an operator and the other is not, they can't conflict.
  if (sig1.UnaryOperator != sig2.UnaryOperator)
    return false;

  // If one is an instance and the other is not, they can't conflict.
  if (sig1.IsInstanceMember != sig2.IsInstanceMember)
    return false;

  // For distributed decls, check there's no async/no-async overloads,
  // since those are more fragile in distribution than we'd want distributed calls to be.
  //
  // A remote call is always 'async throws', and we can always record
  // an async throws "accessor" (see AccessibleFunction.cpp) as such.
  // This means, if we allowed async/no-async overloads of functions,
  // we'd have to store the precise "it was not throwing" information,
  // but we'll _never_ make use of such because all remote calls are
  // necessarily going to async to the actor in the recipient process,
  // and for the remote caller, they are always as-if-async.
  //
  // By banning such overloads, which may be useful in local APIs,
  // but too fragile in distributed APIs, we allow a remote 'v2' version
  // of an implementation to add or remove `async` to their implementation
  // without breaking calls which were made on previous 'v1' versions of
  // the same interface; Callers are never broken this way, and rollouts
  // are simpler.
  //
  // The restriction on overloads is not a problem for distributed calls,
  // as we don't have a vast swab of APIs which must compatibly get async
  // versions, as that is what the async overloading aimed to address.
  //
  // Note also, that overloading on throws is already illegal anyway.
  if (!sig1.IsDistributed && !sig2.IsDistributed) {
    // For non-distributed functions,
    // if one is an async function and the other is not, they don't conflict.
    if (sig1.IsAsyncFunction != sig2.IsAsyncFunction)
      return false;
  } // else, if any of the methods was distributed, continue checking

  // If one is a macro and the other is not, they can't conflict.
  if (sig1.IsMacro != sig2.IsMacro)
    return false;

  // If one is a compound name and the other is not, they do not conflict
  // if one is a property and the other is a non-nullary function.
  if (sig1.Name.isCompoundName() != sig2.Name.isCompoundName()) {
    return !((sig1.IsVariable && !sig2.Name.getArgumentNames().empty()) ||
             (sig2.IsVariable && !sig1.Name.getArgumentNames().empty()));
  }

  // Note that we intentionally ignore the HasOpaqueReturnType bit here.
  // For declarations that can't be overloaded by type, we want them to be
  // considered conflicting independent of their type.
  
  return sig1.Name == sig2.Name;
}

bool swift::conflicting(ASTContext &ctx,
                        const OverloadSignature& sig1, CanType sig1Type,
                        const OverloadSignature& sig2, CanType sig2Type,
                        bool *wouldConflictInSwift5,
                        bool skipProtocolExtensionCheck) {
  // If the signatures don't conflict to begin with, we're done.
  if (!conflicting(sig1, sig2, skipProtocolExtensionCheck))
    return false;

  // Functions and enum elements do not conflict with each other if their types
  // are different.
  if (((sig1.IsFunction && sig2.IsEnumElement) ||
       (sig1.IsEnumElement && sig2.IsFunction)) &&
      sig1Type != sig2Type) {
    return false;
  }

  // Nominal types and enum elements always conflict with each other.
  if ((sig1.IsNominal && sig2.IsEnumElement) ||
      (sig1.IsEnumElement && sig2.IsNominal)) {
    return true;
  }

  // Typealiases and enum elements always conflict with each other.
  if ((sig1.IsTypeAlias && sig2.IsEnumElement) ||
      (sig1.IsEnumElement && sig2.IsTypeAlias)) {
    return true;
  }

  // Enum elements always conflict with each other. At this point, they
  // have the same base name but different types.
  if (sig1.IsEnumElement && sig2.IsEnumElement) {
    return true;
  }

  // Functions always conflict with non-functions with the same signature.
  // In practice, this only applies for zero argument functions.
  if (sig1.IsFunction != sig2.IsFunction)
    return true;

  // Variables always conflict with non-variables with the same signature.
  // (e.g variables with zero argument functions, variables with type
  //  declarations)
  if (sig1.IsVariable != sig2.IsVariable) {
    // Prior to Swift 5, we permitted redeclarations of variables as different
    // declarations if the variable was declared in an extension of a generic
    // type. Make sure we maintain this behaviour in versions < 5.
    if (!ctx.isSwiftVersionAtLeast(5)) {
      if ((sig1.IsVariable && sig1.InExtensionOfGenericType) ||
          (sig2.IsVariable && sig2.InExtensionOfGenericType)) {
        if (wouldConflictInSwift5)
          *wouldConflictInSwift5 = true;

        return false;
      }
    }

    return true;
  }

  // Otherwise, the declarations conflict if the overload types are the same.
  if (sig1.HasOpaqueReturnType != sig2.HasOpaqueReturnType)
    return false;
  
  if (sig1Type != sig2Type)
    return false;

  // The Swift 5 overload types are the same, but similar to the above, prior to
  // Swift 5, a variable not in an extension of a generic type got a null
  // overload type instead of a function type as it does now, so we really
  // follow that behaviour and warn if there's going to be a conflict in future.
  if (!ctx.isSwiftVersionAtLeast(5)) {
    auto swift4Sig1Type = sig1.IsVariable && !sig1.InExtensionOfGenericType
                              ? CanType()
                              : sig1Type;
    auto swift4Sig2Type = sig1.IsVariable && !sig2.InExtensionOfGenericType
                              ? CanType()
                              : sig1Type;
    if (swift4Sig1Type != swift4Sig2Type) {
      // Old was different to the new behaviour!
      if (wouldConflictInSwift5)
        *wouldConflictInSwift5 = true;

      return false;
    }
  }

  return true;
}

static Type mapSignatureFunctionType(ASTContext &ctx, Type type,
                                     bool topLevelFunction,
                                     bool isMethod,
                                     bool isInitializer,
                                     unsigned curryLevels);

/// Map a type within the signature of a declaration.
static Type mapSignatureType(ASTContext &ctx, Type type) {
  return type.transformRec([&](Type type) -> std::optional<Type> {
      if (type->is<FunctionType>()) {
        return mapSignatureFunctionType(ctx, type, false, false, false, 1);
      }

      return std::nullopt;
    });
}

/// Map a signature type for a parameter.
static Type mapSignatureParamType(ASTContext &ctx, Type type) {
  return mapSignatureType(ctx, type);
}

/// Map an ExtInfo for a function type.
///
/// When checking if two signatures should be equivalent for overloading,
/// we may need to compare the extended information.
///
/// In the type of the function declaration, none of the extended information
/// is relevant. We cannot overload purely on 'throws' or the calling
/// convention of the declaration itself.
///
/// For function parameter types, we do want to be able to overload on
/// 'throws', since that is part of the mangled symbol name, but not
/// @noescape.
static AnyFunctionType::ExtInfo
mapSignatureExtInfo(AnyFunctionType::ExtInfo info,
                    bool topLevelFunction) {
  if (topLevelFunction)
    return AnyFunctionType::ExtInfo();
  return AnyFunctionType::ExtInfoBuilder()
      .withRepresentation(info.getRepresentation())
      .withSendable(info.isSendable())
      .withAsync(info.isAsync())
      .withThrows(info.isThrowing(), info.getThrownError())
      .withClangFunctionType(info.getClangTypeInfo().getType())
      .build();
}

/// Map a function's type to the type used for computing signatures,
/// which involves stripping some attributes, stripping default arguments,
/// transforming implicitly unwrapped optionals into strict optionals,
/// stripping 'inout' on the 'self' parameter etc.
static Type mapSignatureFunctionType(ASTContext &ctx, Type type,
                                     bool topLevelFunction,
                                     bool isMethod,
                                     bool isInitializer,
                                     unsigned curryLevels) {
  if (type->hasError()) {
    return type;
  }

  if (curryLevels == 0) {
    // In an initializer, ignore optionality.
    if (isInitializer) {
      if (auto objectType = type->getOptionalObjectType()) {
        type = objectType;
      }
    }
    
    // Functions and subscripts cannot overload differing only in opaque return
    // types. Replace the opaque type with `Any`.
    if (type->is<OpaqueTypeArchetypeType>()) {
      type = ctx.getAnyExistentialType();
    }

    return mapSignatureParamType(ctx, type);
  }

  auto funcTy = type->castTo<AnyFunctionType>();
  SmallVector<AnyFunctionType::Param, 4> newParams;
  for (const auto &param : funcTy->getParams()) {
    auto newParamType = mapSignatureParamType(ctx, param.getPlainType());

    // Don't allow overloading by @_nonEphemeral or isolated.
    auto newFlags = param.getParameterFlags()
        .withNonEphemeral(false)
        .withIsolated(false);

    // For the 'self' of a method, strip off 'inout'.
    if (isMethod) {
      newFlags = newFlags.withInOut(false);
    }

    AnyFunctionType::Param newParam(newParamType, param.getLabel(), newFlags,
                                    param.getInternalLabel());
    newParams.push_back(newParam);
  }

  // Map the result type.
  auto resultTy = mapSignatureFunctionType(
    ctx, funcTy->getResult(), topLevelFunction, false, isInitializer,
    curryLevels - 1);

  // Map various attributes differently depending on if we're looking at
  // the declaration, or a function parameter type.
  AnyFunctionType::ExtInfo info = mapSignatureExtInfo(
      funcTy->getExtInfo(), topLevelFunction);

  // Rebuild the resulting function type.
  if (auto genericFuncTy = dyn_cast<GenericFunctionType>(funcTy))
    return GenericFunctionType::get(genericFuncTy->getGenericSignature(),
                                    newParams, resultTy, info);

  return FunctionType::get(newParams, resultTy, info);
}

OverloadSignature ValueDecl::getOverloadSignature() const {
  OverloadSignature signature;

  signature.Name = getName();
  signature.InProtocolExtension
    = static_cast<bool>(getDeclContext()->getExtendedProtocolDecl());
  signature.IsInstanceMember = isInstanceMember();
  signature.IsVariable = isa<VarDecl>(this);
  signature.IsEnumElement = isa<EnumElementDecl>(this);
  signature.IsNominal = isa<NominalTypeDecl>(this);
  signature.IsTypeAlias = isa<TypeAliasDecl>(this);
  signature.IsMacro = isa<MacroDecl>(this);
  signature.HasOpaqueReturnType =
                       !signature.IsVariable && (bool)getOpaqueResultTypeDecl();

  // Unary operators also include prefix/postfix.
  if (auto func = dyn_cast<FuncDecl>(this)) {
    if (func->isUnaryOperator()) {
      signature.UnaryOperator = func->getAttrs().getUnaryOperatorKind();
    }
  }

  // Functions include async/not-async.
  if (auto func = dyn_cast<AbstractFunctionDecl>(this)) {
    signature.IsFunction = true;
    if (func->hasAsync())
      signature.IsAsyncFunction = true;
    if (func->isDistributed())
      signature.IsDistributed = true;
  }

  if (auto *extension = dyn_cast<ExtensionDecl>(getDeclContext()))
    if (extension->isGeneric())
      signature.InExtensionOfGenericType = true;

  return signature;
}

CanType ValueDecl::getOverloadSignatureType() const {
  if (auto *afd = dyn_cast<AbstractFunctionDecl>(this)) {
    bool isMethod = afd->hasImplicitSelfDecl();
    return mapSignatureFunctionType(getASTContext(), getInterfaceType(),
                                    /*topLevelFunction=*/true, isMethod,
                                    /*isInitializer=*/isa<ConstructorDecl>(afd),
                                    getNumCurryLevels())
        ->getMinimalCanonicalType();
  }

  if (isa<AbstractStorageDecl>(this)) {
    // First, get the default overload signature type for the decl. For vars,
    // this is the empty tuple type, as variables cannot be overloaded directly
    // by type. For subscripts, it's their interface type.
    CanType defaultSignatureType;
    if (isa<VarDecl>(this)) {
      defaultSignatureType = TupleType::getEmpty(getASTContext());
    } else {
      defaultSignatureType =
          mapSignatureFunctionType(getASTContext(), getInterfaceType(),
                                   /*topLevelFunction=*/true,
                                   /*isMethod=*/false,
                                   /*isInitializer=*/false, getNumCurryLevels())
              ->getMinimalCanonicalType();
    }

    // We want to curry the default signature type with the 'self' type of the
    // given context (if any) in order to ensure the overload signature type
    // is unique across different contexts, such as between a protocol extension
    // and struct decl.
    return defaultSignatureType->addCurriedSelfType(getDeclContext())
        ->getCanonicalType();
  }

  if (isa<EnumElementDecl>(this) || isa<MacroDecl>(this)) {
    auto mappedType = mapSignatureFunctionType(
        getASTContext(), getInterfaceType(), /*topLevelFunction=*/false,
        /*isMethod=*/false, /*isInitializer=*/false, getNumCurryLevels());
    return mappedType->getMinimalCanonicalType();
  }

  // Note: If you add more cases to this function, you should update the
  // implementation of the swift::conflicting overload that deals with
  // overload types, in order to account for cases where the overload types
  // don't match, but the decls differ and therefore always conflict.
  assert(isa<TypeDecl>(this));
  return CanType();
}

llvm::TinyPtrVector<ValueDecl *> ValueDecl::getOverriddenDecls() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    OverriddenDeclsRequest{const_cast<ValueDecl *>(this)}, {});
}

void ValueDecl::setOverriddenDecls(ArrayRef<ValueDecl *> overridden) {
  llvm::TinyPtrVector<ValueDecl *> overriddenVec(overridden);
  OverriddenDeclsRequest request{const_cast<ValueDecl *>(this)};
  request.cacheResult(overriddenVec);
}

// To-Do: Replce calls to getOpaqueResultTypeRepr with getResultTypeRepr()
TypeRepr *ValueDecl::getResultTypeRepr() const {
  TypeRepr *returnRepr = nullptr;
  if (auto *VD = dyn_cast<VarDecl>(this)) {
    if (auto *P = VD->getParentPattern()) {
      while (auto *PP = dyn_cast<ParenPattern>(P))
        P = PP->getSubPattern();

      if (auto *TP = dyn_cast<TypedPattern>(P)) {
        P = P->getSemanticsProvidingPattern();
        if (auto *NP = dyn_cast<NamedPattern>(P)) {
          assert(NP->getDecl() == VD);
          (void)NP;

          returnRepr = TP->getTypeRepr();
        }
      }
    } else {
      returnRepr = VD->getTypeReprOrParentPatternTypeRepr();
    }
  } else if (auto *FD = dyn_cast<FuncDecl>(this)) {
    returnRepr = FD->getResultTypeRepr();
  } else if (auto *SD = dyn_cast<SubscriptDecl>(this)) {
    returnRepr = SD->getElementTypeRepr();
  } else if (auto *MD = dyn_cast<MacroDecl>(this)) {
    returnRepr = MD->resultType.getTypeRepr();
  } else if (auto *CD = dyn_cast<ConstructorDecl>(this)) {
    returnRepr = CD->getResultTypeRepr();
  }

  return returnRepr;
}

TypeRepr *ValueDecl::getOpaqueResultTypeRepr() const {
  // FIXME: Macros don't allow opaque result types yet.
  if (isa<MacroDecl>(this))
    return nullptr;

  auto *returnRepr = this->getResultTypeRepr();

  auto *dc = getDeclContext();
  auto &ctx = dc->getASTContext();
  
  if (returnRepr && returnRepr->hasOpaque()) {
    return returnRepr;
  } else if (returnRepr && ctx.LangOpts.hasFeature(Feature::ImplicitSome)) {
    auto opaqueReprs =
        collectOpaqueTypeReprs(returnRepr, getASTContext(), getDeclContext());
    return opaqueReprs.empty() ? nullptr : returnRepr;
  } else {
    return nullptr;
  }
}

OpaqueTypeDecl *ValueDecl::getOpaqueResultTypeDecl() const {
  if (getOpaqueResultTypeRepr() == nullptr) {
    if (!isa<VarDecl>(this) &&
        !isa<FuncDecl>(this) &&
        !isa<SubscriptDecl>(this))
      return nullptr;
    auto file = cast<FileUnit>(getDeclContext()->getModuleScopeContext());
    // Don't look up when the decl is from source, otherwise a cycle will happen.
    if (file->getKind() == FileUnitKind::SerializedAST) {
      Mangle::ASTMangler mangler;
      auto name = mangler.mangleOpaqueTypeDecl(this);
      return file->lookupOpaqueResultType(name);
    }
    return nullptr;
  }

  return evaluateOrDefault(getASTContext().evaluator,
    OpaqueResultTypeRequest{const_cast<ValueDecl *>(this)},
    nullptr);
}

bool ValueDecl::isObjC() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    IsObjCRequest{const_cast<ValueDecl *>(this)},
    getAttrs().hasAttribute<ObjCAttr>());
}

void ValueDecl::setIsObjC(bool value) {
  assert(!LazySemanticInfo.isObjCComputed || LazySemanticInfo.isObjC == value);

  if (LazySemanticInfo.isObjCComputed) {
    assert(LazySemanticInfo.isObjC == value);
    return;
  }

  LazySemanticInfo.isObjCComputed = true;
  LazySemanticInfo.isObjC = value;
}

Identifier ExtensionDecl::getObjCCategoryName() const {
  // If there's an @objc attribute, it's authoritative. (ClangImporter
  // attaches one automatically.)
  if (auto objcAttr = getAttrs().getAttribute<ObjCAttr>(/*AllowInvalid*/true)) {
    if (objcAttr->hasName() && objcAttr->getName()->getNumArgs() == 0)
      return objcAttr->getName()->getSimpleName();

    return Identifier();
  }

  // Not a category, evidently.
  return Identifier();
}

bool ValueDecl::isSemanticallyFinal() const {
  // Actor types are semantically final.
  if (auto classDecl = dyn_cast<ClassDecl>(this)) {
    if (classDecl->isAnyActor())
      return true;
  }

  // As are methods/accessors of actor types.
  if (!isa<TypeDecl>(this)) {
    if (auto classDecl = getDeclContext()->getSelfClassDecl()) {
      if (classDecl->isAnyActor())
        return true;
    }
  }

  // For everything else, the same as 'final'.
  return isFinal();
}

bool ValueDecl::isFinal() const {
  return evaluateOrDefault(getASTContext().evaluator,
                           IsFinalRequest { const_cast<ValueDecl *>(this) },
                           getAttrs().hasAttribute<FinalAttr>());
}

bool ValueDecl::isDynamic() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    IsDynamicRequest{const_cast<ValueDecl *>(this)},
    getAttrs().hasAttribute<DynamicAttr>());
}

bool ValueDecl::isObjCDynamicInGenericClass() const {
  if (!isObjCDynamic())
    return false;

  auto *DC = this->getDeclContext();
  auto *classDecl = DC->getSelfClassDecl();
  if (!classDecl)
    return false;

  return classDecl->isGenericContext()
             && !classDecl->isTypeErasedGenericClass();
}

bool ValueDecl::shouldUseObjCMethodReplacement() const {
  if (isNativeDynamic())
    return false;

  if (getModuleContext()->isImplicitDynamicEnabled() &&
      isObjCDynamicInGenericClass())
    return false;

  return isObjCDynamic();
}

bool ValueDecl::shouldUseNativeMethodReplacement() const {
  if (isNativeDynamic())
    return true;

  if (!isObjCDynamicInGenericClass())
    return false;

  auto *replacedDecl = getDynamicallyReplacedDecl();
  if (replacedDecl)
    return false;

  return getModuleContext()->isImplicitDynamicEnabled();
}

bool ValueDecl::isNativeMethodReplacement() const {
  // Is this a @_dynamicReplacement(for:) that use the native dynamic function
  // replacement mechanism.
  auto *replacedDecl = getDynamicallyReplacedDecl();
  if (!replacedDecl)
    return false;

  if (isNativeDynamic())
    return true;

  if (replacedDecl->isObjCDynamicInGenericClass())
    return replacedDecl->getModuleContext()->isImplicitDynamicEnabled();

  return false;
}

void ValueDecl::setIsDynamic(bool value) {
  assert(!LazySemanticInfo.isDynamicComputed ||
         LazySemanticInfo.isDynamic == value);

  if (LazySemanticInfo.isDynamicComputed) {
    assert(LazySemanticInfo.isDynamic == value);
    return;
  }

  LazySemanticInfo.isDynamicComputed = true;
  LazySemanticInfo.isDynamic = value;
}

ValueDecl *ValueDecl::getDynamicallyReplacedDecl() const {
  return evaluateOrDefault(getASTContext().evaluator,
                           DynamicallyReplacedDeclRequest{
                               const_cast<ValueDecl *>(this)},
                           nullptr);
}

bool ValueDecl::canBeAccessedByDynamicLookup() const {
  if (!hasName())
    return false;

  auto *dc = getDeclContext();
  if (!dc->mayContainMembersAccessedByDynamicLookup())
    return false;

  // Dynamic lookup can find functions, variables, and subscripts.
  if (!isa<FuncDecl>(this) && !isa<VarDecl>(this) && !isa<SubscriptDecl>(this))
    return false;

  return true;
}

bool ValueDecl::isImplicitlyUnwrappedOptional() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    IsImplicitlyUnwrappedOptionalRequest{const_cast<ValueDecl *>(this)},
    false);
}

bool ValueDecl::isLocalCapture() const {
  auto *dc = getDeclContext();

  if (auto *fd = dyn_cast<FuncDecl>(this))
    if (isa<SourceFile>(dc))
      return fd->hasTopLevelLocalContextCaptures();

  return dc->isLocalContext();
}

ArrayRef<ValueDecl *>
ValueDecl::getSatisfiedProtocolRequirements(bool Sorted) const {
  // Dig out the nominal type.
  NominalTypeDecl *NTD = getDeclContext()->getSelfNominalTypeDecl();
  if (!NTD || isa<ProtocolDecl>(NTD))
    return {};

  return NTD->getSatisfiedProtocolRequirementsForMember(this, Sorted);
}

std::optional<AttributedImport<ImportedModule>>
ValueDecl::findImport(const DeclContext *fromDC) const {
  // If the type is from the current module, there's no import.
  auto module = getModuleContextForNameLookup();
  if (module == fromDC->getParentModule())
    return std::nullopt;

  auto fromSourceFile = fromDC->getParentSourceFile();
  if (!fromSourceFile)
    return std::nullopt;

  return fromSourceFile->findImport(module);
}

bool ValueDecl::isProtocolRequirement() const {
  assert(isa<ProtocolDecl>(getDeclContext()));

  if (isa<AccessorDecl>(this) ||
      isa<TypeAliasDecl>(this) ||
      isa<NominalTypeDecl>(this))
    return false;
  return true;
}

bool ValueDecl::hasInterfaceType() const {
  return !TypeAndAccess.getPointer().isNull();
}

static bool isComputingInterfaceType(const ValueDecl *VD) {
  return VD->getASTContext().evaluator.hasActiveRequest(
            InterfaceTypeRequest{const_cast<ValueDecl *>(VD)});
}

bool ValueDecl::isRecursiveValidation() const {
  if (isComputingInterfaceType(this) && !hasInterfaceType())
    return true;

  if (auto *vd = dyn_cast<VarDecl>(this))
    if (auto *pbd = vd->getParentPatternBinding())
      if (pbd->isComputingPatternBindingEntry(vd))
        return true;

  auto *dc = getDeclContext();
  while (isa<NominalTypeDecl>(dc))
    dc = dc->getParent();

  if (auto *ext = dyn_cast<ExtensionDecl>(dc)) {
    if (ext->isComputingGenericSignature())
      return true;
  }

  return false;
}

Type ValueDecl::getInterfaceType() const {
  auto &ctx = getASTContext();
  return ctx.evaluator(InterfaceTypeRequest{const_cast<ValueDecl *>(this)},
                       [&ctx]() { return ErrorType::get(ctx); });
}

void ValueDecl::setInterfaceType(Type type) {
  assert(!type.isNull() && "Resetting the interface type to null is forbidden");
  getASTContext().evaluator.cacheOutput(InterfaceTypeRequest{this},
                                        std::move(type));
}

StringRef ValueDecl::getCDeclName() const {
  // Treat imported C functions as implicitly @_cdecl.
  if (auto clangDecl = dyn_cast_or_null<clang::FunctionDecl>(getClangDecl())) {
    if (clangDecl->getLanguageLinkage() == clang::CLanguageLinkage
          && clangDecl->getIdentifier())
      return clangDecl->getName();
  }

  // Handle explicit cdecl attributes.
  if (auto cdeclAttr = getAttrs().getAttribute<CDeclAttr>()) {
    return cdeclAttr->Name;
  }

  return "";
}

std::optional<ObjCSelector>
ValueDecl::getObjCRuntimeName(bool skipIsObjCResolution) const {
  if (auto func = dyn_cast<AbstractFunctionDecl>(this))
    return func->getObjCSelector(DeclName(), skipIsObjCResolution);

  ASTContext &ctx = getASTContext();
  auto makeSelector = [&](Identifier name) -> ObjCSelector {
    return ObjCSelector(ctx, 0, { name });
  };

  if (auto classDecl = dyn_cast<ClassDecl>(this)) {
    SmallString<32> scratch;
    return makeSelector(
             ctx.getIdentifier(classDecl->getObjCRuntimeName(scratch)));
  }

  if (auto protocol = dyn_cast<ProtocolDecl>(this)) {
    SmallString<32> scratch;
    return makeSelector(
             ctx.getIdentifier(protocol->getObjCRuntimeName(scratch)));
  }

  if (auto var = dyn_cast<VarDecl>(this))
    return makeSelector(var->getObjCPropertyName());

  return std::nullopt;
}

bool ValueDecl::canInferObjCFromRequirement(ValueDecl *requirement) {
  // Only makes sense for a requirement of an @objc protocol.
  auto proto = cast<ProtocolDecl>(requirement->getDeclContext());
  if (!proto->isObjC()) return false;

  // Only makes sense when this declaration is within a nominal type
  // or extension thereof.
  auto nominal = getDeclContext()->getSelfNominalTypeDecl();
  if (!nominal) return false;

  // If there is already an @objc attribute with an explicit name, we
  // can't infer a name (it's already there).
  if (auto objcAttr = getAttrs().getAttribute<ObjCAttr>()) {
    if (objcAttr->hasName() && !objcAttr->isNameImplicit())
      return false;
  }

  // If the nominal type doesn't conform to the protocol at all, we
  // cannot infer @objc no matter what we do.
  SmallVector<ProtocolConformance *, 1> conformances;
  if (!nominal->lookupConformance(proto, conformances))
    return false;

  // If any of the conformances is attributed to the context in which
  // this declaration resides, we can infer @objc or the Objective-C
  // name.
  auto dc = getDeclContext();
  for (auto conformance : conformances) {
    if (conformance->getDeclContext() == dc)
      return true;
  }

  // Nothing to infer from.
  return false;
}

SourceLoc Decl::getAttributeInsertionLoc(bool forModifier) const {
  // Some decls have a parent/child split where the introducer keyword is on the
  // parent, but the attributes are on the children. If this is a child in such
  // a pair, `introDecl` will be changed to point to the parent. (The parent
  // decl should delegate to one of its children.)
  const Decl *introDecl = this;

  switch (getKind()) {
  case DeclKind::Module:
  case DeclKind::TopLevelCode:
  case DeclKind::PoundDiagnostic:
  case DeclKind::Missing:
  case DeclKind::MissingMember:
  case DeclKind::MacroExpansion:
  case DeclKind::BuiltinTuple:
    // These don't take attributes.
    return SourceLoc();

  case DeclKind::EnumCase:
    // An ECD's attributes are attached to its elements.
    if (auto elem = cast<EnumCaseDecl>(this)->getFirstElement())
      return elem->getAttributeInsertionLoc(forModifier);
    break;

  case DeclKind::EnumElement:
    // An EED's introducer keyword is on its parent case.
    if (auto parent = cast<EnumElementDecl>(this)->getParentCase())
      introDecl = parent;
    break;

  case DeclKind::PatternBinding: {
    // A PBD's attributes are attached to the vars in its patterns.
    auto pbd = cast<PatternBindingDecl>(this);

    for (unsigned i = 0; i < pbd->getNumPatternEntries(); i++) {
      if (auto var = pbd->getAnchoringVarDecl(i)) {
        return var->getAttributeInsertionLoc(forModifier);
      }
    }

    break;
  }

  case DeclKind::Var:
  case DeclKind::Param:
    // A VarDecl's introducer keyword, if it has one, is on its pattern binding.
    if (auto pbd = cast<VarDecl>(this)->getParentPatternBinding())
      introDecl = pbd;
    break;

  case DeclKind::Enum:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Protocol:
  case DeclKind::OpaqueType:
  case DeclKind::TypeAlias:
  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType:
  case DeclKind::Subscript:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
  case DeclKind::Func:
  case DeclKind::Accessor:
  case DeclKind::Macro:
  case DeclKind::Extension:
  case DeclKind::Import:
  case DeclKind::PrecedenceGroup:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    // Both the introducer keyword and the attributes are on `this`.
    break;
  }

  if (isImplicit())
    return SourceLoc();

  SourceLoc resultLoc = getAttrs().getStartLoc(forModifier);
  return resultLoc.isValid() ? resultLoc : introDecl->getStartLoc();
}

/// Returns true if \p VD needs to be treated as publicly-accessible
/// at the SIL, LLVM, and machine levels due to being @usableFromInline.
bool ValueDecl::isUsableFromInline() const {
  assert(getFormalAccess() < AccessLevel::Public);

  if (getAttrs().hasAttribute<UsableFromInlineAttr>() ||
      getAttrs().hasAttribute<AlwaysEmitIntoClientAttr>() ||
      getAttrs().hasAttribute<InlinableAttr>())
    return true;

  if (auto *accessor = dyn_cast<AccessorDecl>(this)) {
    auto *storage = accessor->getStorage();
    if (storage->getAttrs().hasAttribute<UsableFromInlineAttr>() ||
        storage->getAttrs().hasAttribute<AlwaysEmitIntoClientAttr>() ||
        storage->getAttrs().hasAttribute<InlinableAttr>())
      return true;
  }

  if (auto *opaqueType = dyn_cast<OpaqueTypeDecl>(this)) {
    if (auto *namingDecl = opaqueType->getNamingDecl()) {
      if (namingDecl->getAttrs().hasAttribute<UsableFromInlineAttr>() ||
          namingDecl->getAttrs().hasAttribute<AlwaysEmitIntoClientAttr>() ||
          namingDecl->getAttrs().hasAttribute<InlinableAttr>())
        return true;
    }
  }

  if (auto *EED = dyn_cast<EnumElementDecl>(this))
    if (EED->getParentEnum()->getAttrs().hasAttribute<UsableFromInlineAttr>())
      return true;

  if (auto *containingProto = dyn_cast<ProtocolDecl>(getDeclContext())) {
    if (containingProto->getAttrs().hasAttribute<UsableFromInlineAttr>())
      return true;
  }

  if (auto *DD = dyn_cast<DestructorDecl>(this))
    if (auto *CD = dyn_cast<ClassDecl>(DD->getDeclContext()))
      if (CD->getAttrs().hasAttribute<UsableFromInlineAttr>())
        return true;

  return false;
}

bool ValueDecl::isInterfacePackageEffectivelyPublic() const {
  // A package decl with @usableFromInline (or other inlinable
  // attributes) is essentially public, and can be printed in
  // public (or private) interface file without package-name;
  // it can be referenced by another module (without package-name)
  // importing such interface module.
  auto isCandidate = getFormalAccess() == AccessLevel::Package &&
                     isUsableFromInline() &&
                     getModuleContext()->getPackageName().empty();
  if (!isCandidate)
    return false;

  // Treat the decl as public (1) if it's contained in an interface
  // file, e.g. when running -typecheck-module-from-interface or
  // -compile-module-from-interface.
  isCandidate = false;
  if (auto srcFile = getDeclContext()->getParentSourceFile()) {
    isCandidate = srcFile->Kind == SourceFileKind::Interface;
  }
  // Or (2) if the decl being referenced in a client file is defined
  // in an interface module.
  return isCandidate || getModuleContext()->isBuiltFromInterface();
}

bool ValueDecl::shouldHideFromEditor() const {
  // Hide private stdlib declarations.
  if (isPrivateSystemDecl(/*treatNonBuiltinProtocolsAsPublic*/ false) ||
      // ShowInInterfaceAttr is for decls to show in interface as exception but
      // they are not intended to be used directly.
      getAttrs().hasAttribute<ShowInInterfaceAttr>())
    return true;

  if (AvailableAttr::isUnavailable(this))
    return true;

  // Hide 'swift_private' clang decls. They are imported with '__' prefix.
  if (auto *ClangD = getClangDecl()) {
    bool bypassSwiftPrivate = false;
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(this)) {
      if (AFD->getForeignAsyncConvention().has_value()) {
        // For imported 'async' declarations, visibility can be controlled by
        // 'swift_async(...)' attribute.
        if (auto *asyncAttr = ClangD->getAttr<clang::SwiftAsyncAttr>()) {
          bypassSwiftPrivate = true;
          switch (asyncAttr->getKind()) {
          case clang::SwiftAsyncAttr::None:
            // Should be unreachable.
            return true;
          case clang::SwiftAsyncAttr::SwiftPrivate:
            // Hide 'swift_async(swift_private, ...)'.
            return true;
          case clang::SwiftAsyncAttr::NotSwiftPrivate:
            break;
          }
        } else if (ClangD->getAttr<clang::SwiftAsyncNameAttr>()) {
          // Manually specifying the name bypasses 'swift_private' attr.
          bypassSwiftPrivate = true;
        }
      }
    }
    if (!bypassSwiftPrivate && ClangD->hasAttr<clang::SwiftPrivateAttr>())
      return true;
  }

  if (!isUserAccessible())
    return true;

  // Hide editor placeholders.
  if (getBaseName().isEditorPlaceholder())
    return true;

  // '$__' names are reserved by compiler internal.
  if (!getBaseName().isSpecial() &&
      getBaseIdentifier().str().starts_with("$__"))
    return true;

  // Macro unique names are only intended to be used inside the expanded code.
  if (MacroDecl::isUniqueMacroName(getBaseName()))
    return true;

  return false;
}

/// Return maximally open access level which could be associated with the
/// given declaration accounting for @testable importers.
static AccessLevel getMaximallyOpenAccessFor(const ValueDecl *decl) {
  // Non-final classes are considered open to @testable importers.
  if (auto cls = dyn_cast<ClassDecl>(decl)) {
    if (!cls->isSemanticallyFinal())
      return AccessLevel::Open;

  // Non-final overridable class members are considered open to
  // @testable importers.
  } else if (decl->isSyntacticallyOverridable()) {
    if (!cast<ValueDecl>(decl)->isSemanticallyFinal())
      return AccessLevel::Open;
  }

  // Everything else is considered public.
  return AccessLevel::Public;
}

/// Adjust \p access based on whether \p VD is \@usableFromInline, has been
/// testably imported from \p useDC or \p VD is an imported SPI.
///
/// \p access isn't always just `VD->getFormalAccess()` because this adjustment
/// may be for a write, in which case the setter's access might be used instead.
static AccessLevel getAdjustedFormalAccess(const ValueDecl *VD,
                                           AccessLevel access,
                                           const DeclContext *useDC,
                                           bool treatUsableFromInlineAsPublic) {
  // If access control is disabled in the current context, adjust
  // access level of the current declaration to be as open as possible.
  if (useDC && VD->getASTContext().isAccessControlDisabled())
    return getMaximallyOpenAccessFor(VD);

  if (VD->isInterfacePackageEffectivelyPublic())
    return AccessLevel::Public;

  if (treatUsableFromInlineAsPublic &&
      access < AccessLevel::Public &&
      VD->isUsableFromInline()) {
    return AccessLevel::Public;
  }

  if (useDC) {
    // If the use site decl context is PackageUnit, just return
    // the access level that's passed in
    if (auto usePkg = useDC->getPackageContext())
      return access;
    // Check whether we need to modify the access level based on
    // @testable/@_private import attributes.
    auto *useSF = dyn_cast<SourceFile>(useDC->getModuleScopeContext());
    if (!useSF) return access;
    if (useSF->hasTestableOrPrivateImport(access, VD))
      return getMaximallyOpenAccessFor(VD);
  }

  return access;
}

/// Convenience overload that uses `VD->getFormalAccess()` as the access to
/// adjust.
static AccessLevel
getAdjustedFormalAccess(const ValueDecl *VD, const DeclContext *useDC,
                        bool treatUsableFromInlineAsPublic) {
  return getAdjustedFormalAccess(VD, VD->getFormalAccess(), useDC,
                                 treatUsableFromInlineAsPublic);
}

AccessLevel ValueDecl::getEffectiveAccess() const {
  auto effectiveAccess =
    getAdjustedFormalAccess(this, /*useDC=*/nullptr,
                            /*treatUsableFromInlineAsPublic=*/true);

  // Handle @testable/@_private(sourceFile:)
  switch (effectiveAccess) {
  case AccessLevel::Open:
  case AccessLevel::Package:
  case AccessLevel::Public:
  case AccessLevel::Internal:
    if (getModuleContext()->isTestingEnabled() ||
        getModuleContext()->arePrivateImportsEnabled())
      effectiveAccess = getMaximallyOpenAccessFor(this);
    break;
  case AccessLevel::FilePrivate:
    if (getModuleContext()->arePrivateImportsEnabled())
      effectiveAccess = getMaximallyOpenAccessFor(this);
    break;
  case AccessLevel::Private:
    effectiveAccess = AccessLevel::FilePrivate;
    if (getModuleContext()->arePrivateImportsEnabled())
      effectiveAccess = getMaximallyOpenAccessFor(this);
    break;
  }

  auto restrictToEnclosing = [this](AccessLevel effectiveAccess,
                                    AccessLevel enclosingAccess) -> AccessLevel{
    if (effectiveAccess == AccessLevel::Open &&
        enclosingAccess == AccessLevel::Public &&
        isa<NominalTypeDecl>(this)) {
      // Special case: an open class may be contained in a public
      // class/struct/enum. Leave effectiveAccess as is.
      return effectiveAccess;
    }
    return std::min(effectiveAccess, enclosingAccess);
  };

  if (auto enclosingNominal = dyn_cast<NominalTypeDecl>(getDeclContext())) {
    effectiveAccess =
        restrictToEnclosing(effectiveAccess,
                            enclosingNominal->getEffectiveAccess());

  } else if (auto enclosingExt = dyn_cast<ExtensionDecl>(getDeclContext())) {
    // Just check the base type. If it's a constrained extension, Sema should
    // have already enforced access more strictly.
    if (auto nominal = enclosingExt->getExtendedNominal()) {
      effectiveAccess =
          restrictToEnclosing(effectiveAccess, nominal->getEffectiveAccess());
    }

  } else if (getDeclContext()->isLocalContext()) {
    effectiveAccess = AccessLevel::FilePrivate;
  }

  return effectiveAccess;
}

AccessLevel ValueDecl::getFormalAccess() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    AccessLevelRequest{const_cast<ValueDecl *>(this)}, AccessLevel::Private);
}

bool ValueDecl::hasOpenAccess(const DeclContext *useDC) const {
  AccessLevel access =
      getAdjustedFormalAccess(this, useDC,
                              /*treatUsableFromInlineAsPublic*/false);
  return access == AccessLevel::Open;
}

bool ValueDecl::bypassResilienceInPackage(ModuleDecl *accessingModule) const {
  // If the defining module is built with package-cmo, bypass
  // resilient access from the use site that belongs to a module
  // in the same package.
  auto declModule = getModuleContext();
  return declModule->inSamePackage(accessingModule) &&
         declModule->isResilient() &&
         declModule->allowNonResilientAccess() &&
         declModule->serializePackageEnabled();
}

/// Given the formal access level for using \p VD, compute the scope where
/// \p VD may be accessed, taking \@usableFromInline, \@testable imports,
/// \@_spi imports, and enclosing access levels into account.
///
/// \p access isn't always just `VD->getFormalAccess()` because this adjustment
/// may be for a write, in which case the setter's access might be used instead.
static AccessScope
getAccessScopeForFormalAccess(const ValueDecl *VD,
                              AccessLevel formalAccess,
                              const DeclContext *useDC,
                              bool treatUsableFromInlineAsPublic) {
  AccessLevel access = getAdjustedFormalAccess(VD, formalAccess, useDC,
                                               treatUsableFromInlineAsPublic);
  const DeclContext *resultDC = VD->getDeclContext();

  while (!resultDC->isModuleScopeContext()) {
    if (isa<TopLevelCodeDecl>(resultDC)) {
      return AccessScope(resultDC->getModuleScopeContext(),
                         access == AccessLevel::Private);
    }

    if (resultDC->isLocalContext() || access == AccessLevel::Private)
      return AccessScope(resultDC, /*private*/ true);

    if (auto enclosingNominal = dyn_cast<GenericTypeDecl>(resultDC)) {
      auto enclosingAccess =
          getAdjustedFormalAccess(enclosingNominal, useDC,
                                  treatUsableFromInlineAsPublic);
      access = std::min(access, enclosingAccess);

    } else if (auto enclosingExt = dyn_cast<ExtensionDecl>(resultDC)) {
      // Just check the base type. If it's a constrained extension, Sema should
      // have already enforced access more strictly.
      if (auto nominal = enclosingExt->getExtendedNominal()) {
        if (nominal->getParentModule() == enclosingExt->getParentModule()) {
          auto nominalAccess =
              getAdjustedFormalAccess(nominal, useDC,
                                      treatUsableFromInlineAsPublic);
          access = std::min(access, nominalAccess);
        }
      }

    } else {
      llvm_unreachable("unknown DeclContext kind");
    }

    resultDC = resultDC->getParent();
  }

  auto localImportRestriction = VD->getImportAccessFrom(useDC);
  if (localImportRestriction.has_value()) {
    AccessLevel importAccessLevel =
      localImportRestriction.value().accessLevel;
    if (access > importAccessLevel) {
      access = std::min(access, importAccessLevel);
      resultDC = useDC->getParentSourceFile();
    }
  }

  switch (access) {
  case AccessLevel::Private:
  case AccessLevel::FilePrivate:
    assert(resultDC->isModuleScopeContext());
    return AccessScope(resultDC, access == AccessLevel::Private);
  case AccessLevel::Internal:
    return AccessScope(resultDC->getParentModule());
  case AccessLevel::Package: {
    auto pkg = resultDC->getPackageContext(/*lookupIfNotCurrent*/ true);
    if (!pkg) {
      if (VD->isInterfacePackageEffectivelyPublic())
        return AccessScope::getPublic();

      // If reached here, should be treated as internal.
      return AccessScope(resultDC->getParentModule());
    } else {
      return AccessScope(pkg);
    }
  }
  case AccessLevel::Public:
  case AccessLevel::Open:
    return AccessScope::getPublic();
  }

  llvm_unreachable("unknown access level");
}

AccessScope
ValueDecl::getFormalAccessScope(const DeclContext *useDC,
                                bool treatUsableFromInlineAsPublic) const {
  return getAccessScopeForFormalAccess(this, getFormalAccess(), useDC,
                                       treatUsableFromInlineAsPublic);
}

/// Checks if \p VD may be used from \p useDC, taking \@testable imports into
/// account.
///
/// Whenever the enclosing context of \p VD is usable from \p useDC, this
/// should compute the same result as checkAccess, below, but more slowly.
///
/// Here's an example while typechecking a file with the following code.
///
/// ```
/// import OtherModule
///
/// // `Foo` is a `public` struct defined in `OtherModule`
/// public func myFunc(_ arg: OtherModule.Foo) {}
/// ```
///
/// The use site of `Foo`is a function `myFunc`, and its DeclContext (useDC)
/// is FileUnit. The call \c getAccessScopeForFormalAccess inside this function
/// to get the access scope of`Foo` returns a public scope based on its `public`
/// access level, which is a wrapper around a nullptr DeclContext. Note that the
/// useDC is still non-null (FileUnit) even though the use site itself also has
/// a `public` acess level.
///
/// The `isChildOf` call compares the DeclContext hierarchy of the use site
/// (useDC) and the decl (VD) site, and returns true in this case, since
/// FileUnit is a child of nullptr based on the DeclContext hierarchy. The
/// hierarchy is created when subclasses of DeclContext such as FileUnit or
/// ModuleDecl are constructed. For example, a top ClassDecl ctor takes FileUnit
/// as its parent DeclContext and FileUnit ctor takes ModuleDecl as its parent
/// DeclContext. There's an exception, however, for the case of PackageUnit.
/// \see PackageUnit for details on how the hierachy between that and ModuleDecl
///      is created.
/// \see DeclContext::ASTHierarchy
/// \see AccessScope::getAccessScopeForFormalAccess
/// \see ValueDecl::isAccessibleFrom for a description of \p forConformance.
static bool checkAccessUsingAccessScopes(const DeclContext *useDC,
                                         const ValueDecl *VD,
                                         AccessLevel access,
                                         bool includeInlineable) {
  if (VD->getASTContext().isAccessControlDisabled())
    return true;

  AccessScope accessScope = getAccessScopeForFormalAccess(
      VD, access, useDC,
      /*treatUsableFromInlineAsPublic*/ includeInlineable);
  if (accessScope.getDeclContext() == useDC)
    return true;
  if (!AccessScope(useDC).isChildOf(accessScope))
    return false;
  // useDC is null only when caller wants to skip non-public type checks.
  if (!useDC)
    return true;
  // Check SPI access
  if (!VD->isSPI())
    return true;
  auto useSF = dyn_cast<SourceFile>(useDC->getModuleScopeContext());
  return !useSF || useSF->isImportedAsSPI(VD) ||
         VD->getDeclContext()->getParentModule() == useDC->getParentModule();
}

/// Checks if \p VD is an ObjC member implementation:
///
/// \li It's in an \c \@_objcImplementation extension
/// \li It's not explicitly \c final
/// \li Its access level is not \c private or \c fileprivate
static bool
isObjCMemberImplementation(const ValueDecl *VD,
                           llvm::function_ref<AccessLevel()> getAccessLevel) {
  if (auto ED = dyn_cast<ExtensionDecl>(VD->getDeclContext()))
    if (ED->isObjCImplementation() && !isa<TypeDecl>(VD)) {
      auto attrDecl = isa<AccessorDecl>(VD)
                    ? cast<AccessorDecl>(VD)->getStorage()
                    : VD;
      return !attrDecl->isFinal()
                  && !attrDecl->getAttrs().hasAttribute<NonObjCAttr>()
                  && !attrDecl->getAttrs().hasAttribute<OverrideAttr>()
                  && getAccessLevel() >= AccessLevel::Internal;
    }

  return false;
}

bool ValueDecl::isObjCMemberImplementation() const {
  return ::isObjCMemberImplementation(
              this, [&]() { return this->getFormalAccess(); });
}

/// Checks if \p VD may be used from \p useDC, taking \@testable and \@_spi
/// imports into account.
///
/// When \p access is the same as `VD->getFormalAccess()` and the enclosing
/// context of \p VD is usable from \p useDC, this ought to be the same as
/// getting the AccessScope for `VD` and checking if \p useDC is within it.
/// However, there's a source compatibility hack around protocol extensions
/// that makes it not quite the same.
///
/// See ValueDecl::isAccessibleFrom for a description of \p forConformance.
static bool checkAccess(const DeclContext *useDC, const ValueDecl *VD,
                        bool forConformance,
                        bool includeInlineable,
                        llvm::function_ref<AccessLevel()> getAccessLevel) {
  // If this is an @_objcImplementation member implementation, and we aren't in
  // a context where we would access its storage directly, forbid access. Name
  // lookups will instead find and use the matching interface decl.
  // FIXME: Passing `true` for `isAccessOnSelf` may cause false positives.
  if ((VD->isObjCImplementation() ||
         isObjCMemberImplementation(VD, getAccessLevel)) &&
      VD->getAccessSemanticsFromContext(useDC, /*isAccessOnSelf=*/true)
          != AccessSemantics::DirectToStorage)
    return false;

  if (VD->getASTContext().isAccessControlDisabled())
    return true;

  if (VD->isInterfacePackageEffectivelyPublic())
    return true;

  auto access = getAccessLevel();
  auto *sourceDC = VD->getDeclContext();

  // Preserve "fast path" behavior for everything inside
  // protocol extensions and operators, otherwise allow access
  // check declarations inside inaccessible members via slower
  // access scope based check, which is helpful for diagnostics.
  if (!(sourceDC->getSelfProtocolDecl() || VD->isOperator()))
    return checkAccessUsingAccessScopes(useDC, VD, access, includeInlineable);

  if (!forConformance) {
    if (auto *proto = sourceDC->getSelfProtocolDecl()) {
      // FIXME: Swift 4.1 allowed accessing protocol extension methods that were
      // marked 'public' if the protocol was '@_versioned' (now
      // '@usableFromInline'). Which works at the ABI level, so let's keep
      // supporting that here by explicitly checking for it.
      auto protoAccess = proto->getFormalAccess();
      if (access == AccessLevel::Public &&
          (protoAccess == AccessLevel::Internal || protoAccess == AccessLevel::Package) &&
          proto->isUsableFromInline()) {
        return true;
      }

      // Skip the fast path below and just compare access scopes.
      return checkAccessUsingAccessScopes(useDC, VD, access, includeInlineable);
    }
  }

  // Fast path: assume that the client context already has access to our parent
  // DeclContext, and only check what might be different about this declaration.
  if (!useDC)
    return access >= AccessLevel::Public;

  switch (access) {
  case AccessLevel::Private:
    if (useDC != sourceDC) {
      auto *useSF = dyn_cast<SourceFile>(useDC->getModuleScopeContext());
      if (useSF && useSF->hasTestableOrPrivateImport(access, VD))
        return true;
    }
    return (useDC == sourceDC ||
      AccessScope::allowsPrivateAccess(useDC, sourceDC));
  case AccessLevel::FilePrivate:
    if (useDC->getModuleScopeContext() != sourceDC->getModuleScopeContext()) {
      auto *useSF = dyn_cast<SourceFile>(useDC->getModuleScopeContext());
      return useSF && useSF->hasTestableOrPrivateImport(access, VD);
    }
    return true;
  case AccessLevel::Internal: {
    // Invalid if the use site is > Internal.
    // E.g. extension containing a member of a protocol it conforms to has
    // `package` access level but the member is `internal`
    if (useDC->getContextKind() == DeclContextKind::Package)
      return false;
    const ModuleDecl *sourceModule = sourceDC->getParentModule();
    const DeclContext *useFile = useDC->getModuleScopeContext();
    if (useFile->getParentModule() == sourceModule)
      return true;
    auto *useSF = dyn_cast<SourceFile>(useFile);
    return useSF && useSF->hasTestableOrPrivateImport(access, sourceModule);
  }
  case AccessLevel::Package: {
    auto srcPkg = sourceDC->getPackageContext(/*lookupIfNotCurrent*/ true);
    auto usePkg = useDC->getPackageContext(/*lookupIfNotCurrent*/ true);
    return srcPkg && usePkg && usePkg->isSamePackageAs(srcPkg);
  }
  case AccessLevel::Public:
  case AccessLevel::Open:
    return true;
  }
  llvm_unreachable("bad access level");
}

bool ValueDecl::isMoreVisibleThan(ValueDecl *other) const {
  auto scope = getFormalAccessScope(/*UseDC=*/nullptr,
                                    /*treatUsableFromInlineAsPublic=*/true);

  // 'other' may have come from a @testable import, so we need to upgrade it's
  // visibility to public here. That is not the same as whether 'other' is
  // being built with -enable-testing though -- we don't want to treat it
  // differently in that case.
  auto otherScope =
      other->getFormalAccessScope(getDeclContext(),
                                  /*treatUsableFromInlineAsPublic=*/true);

  if (scope.isPublic())
    return !otherScope.isPublic();
  else if (scope.isPackage())
    return !otherScope.isPublicOrPackage();
  else if (scope.isInternal())
    return !otherScope.isPublic() && !otherScope.isInternal();
  else
    return false;
}

bool ValueDecl::isAccessibleFrom(const DeclContext *useDC,
                                 bool forConformance,
                                 bool allowUsableFromInline) const {
  return checkAccess(useDC, this, forConformance, allowUsableFromInline,
                     [&]() { return getFormalAccess(); });
}

ImportAccessLevel Decl::getImportAccessFrom(const DeclContext *useDC) const {
  ModuleDecl *Mod = getModuleContext();
  if (useDC && useDC->getParentModule() != Mod) {
    if (auto useSF = useDC->getParentSourceFile()) {
      return useSF->getImportAccessLevel(Mod);
    }
  }
  return std::nullopt;
}

bool AbstractStorageDecl::isSetterAccessibleFrom(const DeclContext *DC,
                                                 bool forConformance) const {
  assert(isSettable(DC));

  // If a stored property does not have a setter, it is still settable from the
  // designated initializer constructor. In this case, don't check setter
  // access; it is not set.
  if (hasStorage() && !isSettable(nullptr))
    return true;

  if (isa<ParamDecl>(this))
    return true;

  return checkAccess(DC, this, forConformance, /*includeInlineable*/ false,
                     [&]() { return getSetterFormalAccess(); });
}

void ValueDecl::copyFormalAccessFrom(const ValueDecl *source,
                                     bool sourceIsParentContext) {
  assert(!hasAccess());

  AccessLevel access = source->getFormalAccess();

  // To make something have the same access as a 'private' parent, it has to
  // be 'fileprivate' or greater.
  if (sourceIsParentContext && access == AccessLevel::Private)
    access = AccessLevel::FilePrivate;

  // Only certain declarations can be 'open'.
  if (access == AccessLevel::Open && !isSyntacticallyOverridable()) {
    assert(!isa<ClassDecl>(this) &&
           "copying 'open' onto a class has complications");
    access = AccessLevel::Public;
  }

  setAccess(access);

  // Inherit the @usableFromInline attribute.
  if (source->getAttrs().hasAttribute<UsableFromInlineAttr>() &&
      !getAttrs().hasAttribute<UsableFromInlineAttr>() &&
      !getAttrs().hasAttribute<InlinableAttr>() &&
      DeclAttribute::canAttributeAppearOnDecl(DeclAttrKind::UsableFromInline,
                                              this)) {
    auto &ctx = getASTContext();
    auto *clonedAttr = new (ctx) UsableFromInlineAttr(/*implicit=*/true);
    getAttrs().add(clonedAttr);
  }
}

TypeDecl::CanBeInvertible::Result
NominalTypeDecl::canConformTo(InvertibleProtocolKind ip) const {
  auto *proto = getASTContext().getProtocol(getKnownProtocolKind(ip));
  assert(proto && "missing Copyable/Escapable from stdlib!");

  // Handle protocols specially, without building a GenericSignature.
  if (auto *protoDecl = dyn_cast<ProtocolDecl>(this)) {
    return protoDecl->inheritsFrom(proto)
         ? TypeDecl::CanBeInvertible::Always
         : TypeDecl::CanBeInvertible::Never;
  }

  Type selfTy = getDeclaredInterfaceType();
  assert(selfTy);

  auto conformance = swift::lookupConformance(selfTy, proto,
                                              /*allowMissing=*/false);

  if (conformance.isInvalid())
    return TypeDecl::CanBeInvertible::Never;

  if (!conformance.getConditionalRequirements().empty())
    return TypeDecl::CanBeInvertible::Conditionally;

  return TypeDecl::CanBeInvertible::Always;
}

TypeDecl::CanBeInvertible::Result NominalTypeDecl::canBeCopyable() const {
  return canConformTo(InvertibleProtocolKind::Copyable);
}

TypeDecl::CanBeInvertible::Result NominalTypeDecl::canBeEscapable() const {
  return canConformTo(InvertibleProtocolKind::Escapable);
}

Type TypeDecl::getDeclaredInterfaceType() const {
  if (auto *NTD = dyn_cast<NominalTypeDecl>(this))
    return NTD->getDeclaredInterfaceType();

  if (auto *ATD = dyn_cast<AssociatedTypeDecl>(this)) {
    auto &ctx = getASTContext();
    auto selfTy = getDeclContext()->getSelfInterfaceType();
    if (!selfTy)
      return ErrorType::get(ctx);
    return DependentMemberType::get(
        selfTy, const_cast<AssociatedTypeDecl *>(ATD));

  }

  return getInterfaceType()->getMetatypeInstanceType();
}

int TypeDecl::compare(const TypeDecl *type1, const TypeDecl *type2) {
  // Order based on the enclosing declaration.
  auto dc1 = type1->getDeclContext();
  auto dc2 = type2->getDeclContext();

  // Prefer lower depths.
  auto depth1 = dc1->getSemanticDepth();
  auto depth2 = dc2->getSemanticDepth();
  if (depth1 != depth2)
    return depth1 < depth2 ? -1 : +1;

  // Prefer module names earlier in the alphabet.
  if (dc1->isModuleScopeContext() && dc2->isModuleScopeContext()) {
    auto module1 = dc1->getParentModule();
    auto module2 = dc2->getParentModule();
    if (int result = module1->getName().str().compare(module2->getName().str()))
      return result;
  }

  auto nominal1 = dc1->getSelfNominalTypeDecl();
  auto nominal2 = dc2->getSelfNominalTypeDecl();
  if (static_cast<bool>(nominal1) != static_cast<bool>(nominal2)) {
    return static_cast<bool>(nominal1) ? -1 : +1;
  }
  if (nominal1 && nominal2) {
    if (int result = compare(nominal1, nominal2))
      return result;
  }

  if (int result = type1->getBaseIdentifier().str().compare(
                                  type2->getBaseIdentifier().str()))
    return result;

  // Error case: two type declarations that cannot be distinguished.
  if (type1 < type2)
    return -1;
  if (type1 > type2)
    return +1;
  return 0;
}

bool NominalTypeDecl::isFormallyResilient() const {
  // Private and (unversioned) internal types always have a
  // fixed layout.
  if (!getFormalAccessScope(/*useDC=*/nullptr,
                            /*treatUsableFromInlineAsPublic=*/true).isPublicOrPackage())
    return false;

  // Check for an explicit @_fixed_layout or @frozen attribute.
  if (getAttrs().hasAttribute<FixedLayoutAttr>() ||
      getAttrs().hasAttribute<FrozenAttr>()) {
    return false;
  }

  // Structs and enums imported from C *always* have a fixed layout.
  // We know their size, and pass them as values in SIL and IRGen.
  if (hasClangNode())
    return false;

  // @objc enums and protocols always have a fixed layout.
  if ((isa<EnumDecl>(this) || isa<ProtocolDecl>(this)) && isObjC())
    return false;

  // Otherwise, the declaration behaves as if it was accessed via indirect
  // "resilient" interfaces, even if the module is not built with resilience.
  return true;
}

bool NominalTypeDecl::isResilient() const {
  if (!isFormallyResilient())
    return false;
  return getModuleContext()->isResilient();
}

bool NominalTypeDecl::isStrictlyResilient() const {
  return isResilient() && !getModuleContext()->allowNonResilientAccess();
}

DestructorDecl *NominalTypeDecl::getValueTypeDestructor() {
  if (!isa<StructDecl>(this) && !isa<EnumDecl>(this)) {
    return nullptr;
  }
  
  auto found = lookupDirect(DeclBaseName::createDestructor());
  if (found.size() != 1) {
    return nullptr;
  }
  return cast<DestructorDecl>(found[0]);
}

static bool isOriginallyDefinedIn(const Decl *D, const ModuleDecl* MD) {
  if (!MD)
    return false;
  if (D->getAlternateModuleName().empty())
    return false;
  return D->getAlternateModuleName() == MD->getName().str();
}

bool NominalTypeDecl::isResilient(ModuleDecl *M,
                                  ResilienceExpansion expansion) const {
  switch (expansion) {
  case ResilienceExpansion::Minimal:
    return isResilient();
  case ResilienceExpansion::Maximal:
    // We can access declarations from the same module
    // non-resiliently in a maximal context.
    if (M == getModuleContext())
      return false;
    // Access non-resiliently if package optimization is enabled
    if (bypassResilienceInPackage(M))
      return false;

    // If a protocol is originally declared in the current module, then we
    // directly expose protocol witness tables and their contents for any
    // conformances in the same module as symbols. If the protocol later
    // moves, then we need to preserve those extra symbols from the home
    // module by treating the protocol as if it was still defined in the same
    // module.
    //
    // This logic does not and should not generally extend to other kinds of
    // declaration. If a declaration moves to a new module with library
    // evolution enabled, then even the original module has to access it
    // according to the library evolution ABI. This is an ABI compatibility
    // hack only for protocols. If you see other variations of `isResilient`
    // that don't check `isOriginallyDefinedIn`, they are probably correct.
    if (isa<ProtocolDecl>(this)
        && isOriginallyDefinedIn(this, M)) {
      return false;
    }
    // Otherwise, we have to access the declaration resiliently if it's
    // resilient anywhere.
    return isResilient();
  }
  llvm_unreachable("bad resilience expansion");
}

enum class DeclTypeKind : unsigned {
  DeclaredType,
  DeclaredInterfaceType
};

static Type computeNominalType(NominalTypeDecl *decl, DeclTypeKind kind) {
  ASTContext &ctx = decl->getASTContext();

  // Special case the Builtin.TheTupleType singleton.
  if (isa<BuiltinTupleDecl>(decl))
    return ctx.getBuiltinTupleType();

  // If `decl` is a nested type, find the parent type.
  Type ParentTy;
  DeclContext *dc = decl->getDeclContext();
  bool isUnsupportedNestedProtocol =
    isa<ProtocolDecl>(decl) && decl->getParent()->isGenericContext();

  if (!isUnsupportedNestedProtocol && dc->isTypeContext()) {
    switch (kind) {
    case DeclTypeKind::DeclaredType: {
      if (auto *nominal = dc->getSelfNominalTypeDecl())
        ParentTy = nominal->getDeclaredType();
      break;
    }
    case DeclTypeKind::DeclaredInterfaceType:
      ParentTy = dc->getDeclaredInterfaceType();
      if (ParentTy->is<ErrorType>())
        ParentTy = Type();
      break;
    }
  }

  if (!isa<ProtocolDecl>(decl) && decl->getGenericParams()) {
    switch (kind) {
    case DeclTypeKind::DeclaredType:
      return UnboundGenericType::get(decl, ParentTy, ctx);
    case DeclTypeKind::DeclaredInterfaceType: {
      // Note that here, we need to be able to produce a type
      // before the decl has been validated, so we rely on
      // the generic parameter list directly instead of looking
      // at the signature.
      SmallVector<Type, 4> args;
      for (auto param : decl->getGenericParams()->getParams()) {
        auto argTy = param->getDeclaredInterfaceType();
        if (param->isParameterPack())
          argTy = PackType::getSingletonPackExpansion(argTy);
        args.push_back(argTy);
      }

      return BoundGenericType::get(decl, ParentTy, args);
    }
    }

    llvm_unreachable("Unhandled DeclTypeKind in switch.");
  }

  return NominalType::get(decl, ParentTy, ctx);
}

Type NominalTypeDecl::getDeclaredType() const {
  if (DeclaredTy)
    return DeclaredTy;

  auto *decl = const_cast<NominalTypeDecl *>(this);
  decl->DeclaredTy = computeNominalType(decl, DeclTypeKind::DeclaredType);
  return DeclaredTy;
}

Type NominalTypeDecl::getDeclaredInterfaceType() const {
  if (DeclaredInterfaceTy)
    return DeclaredInterfaceTy;

  auto *decl = const_cast<NominalTypeDecl *>(this);
  decl->DeclaredInterfaceTy = computeNominalType(decl,
                                                 DeclTypeKind::DeclaredInterfaceType);
  return DeclaredInterfaceTy;
}

void NominalTypeDecl::prepareExtensions() {
  // Types in local contexts can't have extensions
  if (getLocalContext() != nullptr) {
    return;
  }
  
  auto &context = Decl::getASTContext();

  // If our list of extensions is out of date, update it now.
  if (context.getCurrentGeneration() > ExtensionGeneration) {
    unsigned previousGeneration = ExtensionGeneration;
    ExtensionGeneration = context.getCurrentGeneration();
    context.loadExtensions(this, previousGeneration);
  }
}

ExtensionRange NominalTypeDecl::getExtensions() {
  prepareExtensions();
  return ExtensionRange(ExtensionIterator(FirstExtension), ExtensionIterator());
}

void NominalTypeDecl::addExtension(ExtensionDecl *extension) {
  assert(!extension->NextExtension.getInt() && "Already added extension");
  extension->NextExtension.setInt(true);
  
  // First extension; set both first and last.
  if (!FirstExtension) {
    FirstExtension = extension;
    LastExtension = extension;

    addedExtension(extension);
    return;
  }

  // Add to the end of the list.
  LastExtension->NextExtension.setPointer(extension);
  LastExtension = extension;

  addedExtension(extension);
}

ArrayRef<VarDecl *> NominalTypeDecl::getStoredProperties() const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator,
      StoredPropertiesRequest{mutableThis},
      {});
}

ArrayRef<VarDecl *>
NominalTypeDecl::getInitAccessorProperties() const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator,
      InitAccessorPropertiesRequest{mutableThis},
      {});
}

ArrayRef<VarDecl *>
NominalTypeDecl::getMemberwiseInitProperties() const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator,
      MemberwiseInitPropertiesRequest{mutableThis},
      {});
}

void NominalTypeDecl::collectPropertiesInitializableByInitAccessors(
    std::multimap<VarDecl *, VarDecl *> &result) const {
  for (auto *property : getInitAccessorProperties()) {
    auto *initAccessor = property->getAccessor(AccessorKind::Init);
    for (auto *subsumed : initAccessor->getInitializedProperties())
      result.insert({subsumed, property});
  }
}

ArrayRef<Decl *>
NominalTypeDecl::getStoredPropertiesAndMissingMemberPlaceholders() const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator,
      StoredPropertiesAndMissingMembersRequest{mutableThis},
      {});
}

bool NominalTypeDecl::isOptionalDecl() const {
  return this == getASTContext().getOptionalDecl();
}

std::optional<KeyPathTypeKind> NominalTypeDecl::getKeyPathTypeKind() const {
  auto &ctx = getASTContext();
#define CASE(NAME) if (this == ctx.get##NAME##Decl()) return KPTK_##NAME;
  CASE(KeyPath)
  CASE(WritableKeyPath)
  CASE(ReferenceWritableKeyPath)
  CASE(AnyKeyPath)
  CASE(PartialKeyPath)
#undef CASE
  return std::nullopt;
}

PropertyWrapperTypeInfo NominalTypeDecl::getPropertyWrapperTypeInfo() const {
  ASTContext &ctx = getASTContext();
  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(ctx.evaluator,
                           PropertyWrapperTypeInfoRequest{mutableThis},
                           PropertyWrapperTypeInfo());
}

bool NominalTypeDecl::isActor() const {
  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           IsActorRequest{mutableThis},
                           false);
}

bool NominalTypeDecl::isDistributedActor() const {
  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           IsDistributedActorRequest{mutableThis},
                           false);
}

bool NominalTypeDecl::isAnyActor() const {
  return isActor() || isDistributedActor();
}

bool NominalTypeDecl::isMainActor() const {
  return getName().is("MainActor") &&
         getParentModule()->getName() == getASTContext().Id_Concurrency;
}

bool NominalTypeDecl::suppressesConformance(KnownProtocolKind kp) const {
  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           SuppressesConformanceRequest{mutableThis, kp},
                           false);
}

GenericTypeDecl::GenericTypeDecl(DeclKind K, DeclContext *DC,
                                 Identifier name, SourceLoc nameLoc,
                                 ArrayRef<InheritedEntry> inherited,
                                 GenericParamList *GenericParams) :
    GenericContext(DeclContextKind::GenericTypeDecl, DC, GenericParams),
    TypeDecl(K, DC, name, nameLoc, inherited) {}

TypeAliasDecl::TypeAliasDecl(SourceLoc TypeAliasLoc, SourceLoc EqualLoc,
                             Identifier Name, SourceLoc NameLoc,
                             GenericParamList *GenericParams, DeclContext *DC)
  : GenericTypeDecl(DeclKind::TypeAlias, DC, Name, NameLoc, {}, GenericParams),
    TypeAliasLoc(TypeAliasLoc), EqualLoc(EqualLoc) {
  Bits.TypeAliasDecl.IsCompatibilityAlias = false;
  Bits.TypeAliasDecl.IsDebuggerAlias = false;
}

SourceRange TypeAliasDecl::getSourceRange() const {
  auto TrailingWhereClauseSourceRange = getGenericTrailingWhereClauseSourceRange();
  if (TrailingWhereClauseSourceRange.isValid())
    return { TypeAliasLoc, TrailingWhereClauseSourceRange.End };
  if (UnderlyingTy.hasLocation())
    return { TypeAliasLoc, UnderlyingTy.getSourceRange().End };
  if (TypeEndLoc.isValid())
    return { TypeAliasLoc, TypeEndLoc };
  return { TypeAliasLoc, getNameLoc() };
}

Type TypeAliasDecl::getUnderlyingType() const {
  auto &ctx = getASTContext();
  if (auto type = evaluateOrDefault(ctx.evaluator,
           UnderlyingTypeRequest{const_cast<TypeAliasDecl *>(this)},
           Type()))
    return type;
  return ErrorType::get(ctx);
}
      
void TypeAliasDecl::setUnderlyingType(Type underlying) {
  // lldb creates global typealiases containing archetypes
  // sometimes...
  assert(!underlying->hasArchetype() || !isGenericContext());

  getASTContext().evaluator.cacheOutput(
          StructuralTypeRequest{const_cast<TypeAliasDecl *>(this)},
          std::move(underlying));
  getASTContext().evaluator.cacheOutput(
          UnderlyingTypeRequest{const_cast<TypeAliasDecl *>(this)},
          std::move(underlying));
}

UnboundGenericType *TypeAliasDecl::getUnboundGenericType() const {
  assert(getGenericParams());

  Type parentTy;
  auto parentDC = getDeclContext();
  if (auto nominal = parentDC->getSelfNominalTypeDecl())
    parentTy = nominal->getDeclaredType();

  return UnboundGenericType::get(
      const_cast<TypeAliasDecl *>(this),
      parentTy, getASTContext());
}

Type TypeAliasDecl::getStructuralType() const {
  auto &ctx = getASTContext();
  if (auto type = evaluateOrDefault(
      ctx.evaluator,
      StructuralTypeRequest{const_cast<TypeAliasDecl *>(this)},
      Type()))
    return type;
  return ErrorType::get(ctx);
}

GenericTypeParamDecl::GenericTypeParamDecl(DeclContext *dc, Identifier name,
                                           SourceLoc nameLoc,
                                           SourceLoc specifierLoc,
                                           unsigned depth, unsigned index,
                                           GenericTypeParamKind paramKind,
                                           bool isOpaqueType,
                                           TypeRepr *opaqueTypeRepr)
    : TypeDecl(DeclKind::GenericTypeParam, dc, name, nameLoc, {}) {
  ASSERT(!(specifierLoc &&
         !(paramKind == GenericTypeParamKind::Pack || paramKind == GenericTypeParamKind::Value)) &&
    "'each' or 'let' keyword imply a parameter pack or value generic parameter");
  ASSERT(isOpaqueType || !opaqueTypeRepr);

  Bits.GenericTypeParamDecl.Depth = depth;
  assert(Bits.GenericTypeParamDecl.Depth == depth && "Truncation");
  Bits.GenericTypeParamDecl.Index = index;
  assert(Bits.GenericTypeParamDecl.Index == index && "Truncation");
  Bits.GenericTypeParamDecl.ParamKind = (uint8_t) paramKind;
  Bits.GenericTypeParamDecl.IsOpaqueType = isOpaqueType;

  if (this->isOpaqueType())
    *getTrailingObjects<TypeRepr *>() = opaqueTypeRepr;

  if (this->isParameterPack())
    *getTrailingObjects<SourceLoc>() = specifierLoc;

  if (this->isValue())
    *getTrailingObjects<SourceLoc>() = specifierLoc;
}

GenericTypeParamDecl *GenericTypeParamDecl::create(
    DeclContext *dc, Identifier name, SourceLoc nameLoc, SourceLoc specifierLoc,
    unsigned depth, unsigned index, GenericTypeParamKind paramKind,
    bool isOpaqueType, TypeRepr *opaqueTypeRepr) {
  auto &ctx = dc->getASTContext();

  auto numTypeReprs = 0;

  if (isOpaqueType)
    numTypeReprs = 1;

  auto numSourceLocs = 0;

  if (paramKind == GenericTypeParamKind::Pack ||
      paramKind == GenericTypeParamKind::Value)
    numSourceLocs = 1;

  auto allocSize = totalSizeToAlloc<TypeRepr *, SourceLoc>(numTypeReprs,
                                                           numSourceLocs);
  auto mem = ctx.Allocate(allocSize, alignof(GenericTypeParamDecl));
  return new (mem)
      GenericTypeParamDecl(dc, name, nameLoc, specifierLoc, depth, index,
                           paramKind, isOpaqueType, opaqueTypeRepr);
}

GenericTypeParamDecl *GenericTypeParamDecl::createDeserialized(
    DeclContext *dc, Identifier name, unsigned depth, unsigned index,
    GenericTypeParamKind paramKind, bool isOpaqueType) {
  return GenericTypeParamDecl::create(dc, name, SourceLoc(), SourceLoc(),
                                      depth, index, paramKind,
                                      isOpaqueType,
                                      /*opaqueRepr*/ nullptr);
}

GenericTypeParamDecl *
GenericTypeParamDecl::createParsed(DeclContext *dc, Identifier name,
                                   SourceLoc nameLoc, SourceLoc specifierLoc,
                                   unsigned index,
                                   GenericTypeParamKind paramKind) {
  // We always create generic type parameters with an invalid depth.
  // Semantic analysis fills in the depth when it processes the generic
  // parameter list.
  return GenericTypeParamDecl::create(
      dc, name, nameLoc, specifierLoc, GenericTypeParamDecl::InvalidDepth,
      index, paramKind, /*isOpaqueType*/ false, /*opaqueTypeRepr*/ nullptr);
}

GenericTypeParamDecl *GenericTypeParamDecl::createImplicit(
    DeclContext *dc, Identifier name, unsigned depth, unsigned index,
    GenericTypeParamKind paramKind, TypeRepr *opaqueTypeRepr, SourceLoc nameLoc,
    SourceLoc specifierLoc) {
  auto *param = GenericTypeParamDecl::create(dc, name, nameLoc, specifierLoc,
                                             depth, index, paramKind,
                                             (bool)opaqueTypeRepr,
                                             opaqueTypeRepr);
  param->setImplicit();
  return param;
}

Type GenericTypeParamDecl::getValueType() const {
  return evaluateOrDefault(getASTContext().evaluator,
    GenericTypeParamDeclGetValueTypeRequest{const_cast<GenericTypeParamDecl *>(this)},
    Type());
}

SourceRange GenericTypeParamDecl::getSourceRange() const {
  auto startLoc = getNameLoc();
  auto endLoc = getNameLoc();

  if (const auto specifierLoc = getSpecifierLoc())
    startLoc = specifierLoc;

  if (!getInherited().empty()) {
    if (getInherited().getEndLoc().isValid())
      endLoc = getInherited().getEndLoc();
    else
      assert(startLoc.isInvalid() || this->hasClangNode());
  }

  return {startLoc, endLoc};
}

AssociatedTypeDecl::AssociatedTypeDecl(DeclContext *dc, SourceLoc keywordLoc,
                                       Identifier name, SourceLoc nameLoc,
                                       TypeRepr *defaultDefinition,
                                       TrailingWhereClause *trailingWhere)
    : TypeDecl(DeclKind::AssociatedType, dc, name, nameLoc, { }),
      KeywordLoc(keywordLoc), DefaultDefinition(defaultDefinition),
      TrailingWhere(trailingWhere) {
  Bits.AssociatedTypeDecl.IsDefaultDefinitionTypeComputed = false;
}

AssociatedTypeDecl *
AssociatedTypeDecl::createParsed(ASTContext &ctx, DeclContext *dc,
                                 SourceLoc keywordLoc, Identifier name,
                                 SourceLoc nameLoc, TypeRepr *defaultDefinition,
                                 TrailingWhereClause *trailingWhere) {
  auto *decl = new (ctx) AssociatedTypeDecl(dc, keywordLoc, name, nameLoc,
                                            defaultDefinition, trailingWhere);

  // Sort out this trivial case now to enable the AST dumper to differentiate
  // between a nonexistent and null default type without having to trigger a
  // request.
  if (!defaultDefinition)
    decl->setDefaultDefinitionType(nullptr);

  return decl;
}

AssociatedTypeDecl *AssociatedTypeDecl::createDeserialized(
    ASTContext &ctx, DeclContext *dc, SourceLoc keywordLoc, Identifier name,
    SourceLoc nameLoc, TrailingWhereClause *trailingWhere,
    LazyMemberLoader *lazyLoader, uint64_t defaultDefinitionTypeData) {
  assert(lazyLoader && "missing lazy member loader");
  auto *decl = new (ctx)
      AssociatedTypeDecl(dc, keywordLoc, name, nameLoc,
                         /*defaultDefinition*/ nullptr, trailingWhere);

  // Sort out this trivial case now to enable the AST dumper to differentiate
  // between a nonexistent and null default type without having to trigger a
  // request. '0' is the sentinel ID for no data.
  if (defaultDefinitionTypeData == 0) {
    decl->setDefaultDefinitionType(nullptr);
  } else {
    auto *data = static_cast<LazyAssociatedTypeData *>(
        ctx.getOrCreateLazyContextData(decl, lazyLoader));
    data->defaultDefinitionTypeData = defaultDefinitionTypeData;
  }

  return decl;
}

Type AssociatedTypeDecl::getDefaultDefinitionType() const {
  return evaluateOrDefault(getASTContext().evaluator,
           DefaultDefinitionTypeRequest{const_cast<AssociatedTypeDecl *>(this)},
           Type());
}

std::optional<Type> AssociatedTypeDecl::getCachedDefaultDefinitionType() const {
  if (Bits.AssociatedTypeDecl.IsDefaultDefinitionTypeComputed)
    return DefaultDefinition.getType();

  return std::nullopt;
}

void AssociatedTypeDecl::setDefaultDefinitionType(Type ty) {
  DefaultDefinition.setType(ty);
  Bits.AssociatedTypeDecl.IsDefaultDefinitionTypeComputed = true;
}

SourceRange AssociatedTypeDecl::getSourceRange() const {
  SourceLoc endLoc;
  if (auto TWC = getTrailingWhereClause()) {
    endLoc = TWC->getSourceRange().End;
  } else if (auto defaultDefinition = getDefaultDefinitionTypeRepr()) {
    endLoc = defaultDefinition->getEndLoc();
  } else if (!getInherited().empty()) {
    endLoc = getInherited().getEndLoc();
  } else {
    endLoc = getNameLoc();
  }
  return SourceRange(KeywordLoc, endLoc);
}

llvm::TinyPtrVector<AssociatedTypeDecl *>
AssociatedTypeDecl::getOverriddenDecls() const {
  // FIXME: Performance hack because we end up looking at the overridden
  // declarations of an associated type a *lot*.
  OverriddenDeclsRequest request{const_cast<AssociatedTypeDecl *>(this)};
  llvm::TinyPtrVector<ValueDecl *> overridden;
  if (auto cached = request.getCachedResult())
    overridden = std::move(*cached);
  else
    overridden = TypeDecl::getOverriddenDecls();

  llvm::TinyPtrVector<AssociatedTypeDecl *> assocTypes;
  for (auto decl : overridden) {
    assocTypes.push_back(cast<AssociatedTypeDecl>(decl));
  }
  return assocTypes;
}

namespace {
static AssociatedTypeDecl *getAssociatedTypeAnchor(
                      const AssociatedTypeDecl *ATD,
                      llvm::SmallSet<const AssociatedTypeDecl *, 8> &searched) {
  auto overridden = ATD->getOverriddenDecls();

  // If this declaration does not override any other declarations, it's
  // the anchor.
  if (overridden.empty()) return const_cast<AssociatedTypeDecl *>(ATD);

  // Find the best anchor among the anchors of the overridden decls and avoid
  // reentrancy when erroneous cyclic protocols exist.
  AssociatedTypeDecl *bestAnchor = nullptr;
  for (auto assocType : overridden) {
    if (!searched.insert(assocType).second)
      continue;
    auto anchor = getAssociatedTypeAnchor(assocType, searched);
    if (!anchor)
      continue;
    if (!bestAnchor || TypeDecl::compare(anchor, bestAnchor) < 0)
      bestAnchor = anchor;
  }

  return bestAnchor;
}
}

AssociatedTypeDecl *AssociatedTypeDecl::getAssociatedTypeAnchor() const {
  llvm::SmallSet<const AssociatedTypeDecl *, 8> searched;
  return ::getAssociatedTypeAnchor(this, searched);
}

EnumDecl::EnumDecl(SourceLoc EnumLoc,
                     Identifier Name, SourceLoc NameLoc,
                     ArrayRef<InheritedEntry> Inherited,
                     GenericParamList *GenericParams, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::Enum, Parent, Name, NameLoc, Inherited,
                    GenericParams),
    EnumLoc(EnumLoc)
{
  Bits.EnumDecl.HasAssociatedValues
    = static_cast<unsigned>(AssociatedValueCheck::Unchecked);
  Bits.EnumDecl.HasAnyUnavailableValues
    = false;
}

Type EnumDecl::getRawType() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(
      ctx.evaluator, EnumRawTypeRequest{const_cast<EnumDecl *>(this)},
      ErrorType::get(ctx));
}

void EnumDecl::setRawType(Type rawType) {
  getASTContext().evaluator.cacheOutput(EnumRawTypeRequest{this},
                                        std::move(rawType));
}

StructDecl::StructDecl(SourceLoc StructLoc, Identifier Name, SourceLoc NameLoc,
                       ArrayRef<InheritedEntry> Inherited,
                       GenericParamList *GenericParams, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::Struct, Parent, Name, NameLoc, Inherited,
                    GenericParams),
    StructLoc(StructLoc)
{
  Bits.StructDecl.HasUnreferenceableStorage = false;
  Bits.StructDecl.IsCxxNonTrivial = false;
}

bool NominalTypeDecl::hasMemberwiseInitializer() const {
  // Currently only structs can have memberwise initializers.
  auto *sd = dyn_cast<StructDecl>(this);
  if (!sd)
    return false;

  auto &ctx = getASTContext();
  auto *mutableThis = const_cast<StructDecl *>(sd);
  return evaluateOrDefault(ctx.evaluator, HasMemberwiseInitRequest{mutableThis},
                           false);
}

ConstructorDecl *NominalTypeDecl::getMemberwiseInitializer() const {
  if (!hasMemberwiseInitializer())
    return nullptr;

  auto &ctx = getASTContext();
  auto *mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator, SynthesizeMemberwiseInitRequest{mutableThis}, nullptr);
}

ConstructorDecl *NominalTypeDecl::getEffectiveMemberwiseInitializer() {
  auto &ctx = getASTContext();
  auto *mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(ctx.evaluator,
                           ResolveEffectiveMemberwiseInitRequest{mutableThis},
                           nullptr);
}

bool NominalTypeDecl::hasDefaultInitializer() const {
  // Currently only structs and classes can have default initializers.
  if (!isa<StructDecl>(this) && !isa<ClassDecl>(this))
    return false;

  auto &ctx = getASTContext();
  auto *mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(ctx.evaluator, HasDefaultInitRequest{mutableThis},
                           false);
}

bool NominalTypeDecl::isTypeErasedGenericClass() const {
  // ObjC classes are type erased.
  // TODO: Unless they have magic methods...
  if (auto clazz = dyn_cast<ClassDecl>(this))
    return clazz->isTypeErasedGenericClass();
  return false;
}

ConstructorDecl *NominalTypeDecl::getDefaultInitializer() const {
  if (!hasDefaultInitializer())
    return nullptr;

  auto &ctx = getASTContext();
  auto *mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(ctx.evaluator,
                           SynthesizeDefaultInitRequest{mutableThis}, nullptr);
}

void NominalTypeDecl::synthesizeSemanticMembersIfNeeded(DeclName member) {
  // Silently break cycles here because we can't be sure when and where a
  // request to synthesize will come from yet.
  // FIXME: rdar://56844567
  if (Bits.NominalTypeDecl.IsComputingSemanticMembers)
    return;
    
  Bits.NominalTypeDecl.IsComputingSemanticMembers = true;
  SWIFT_DEFER { Bits.NominalTypeDecl.IsComputingSemanticMembers = false; };

  auto baseName = member.getBaseName();
  auto &Context = getASTContext();
  std::optional<ImplicitMemberAction> action = std::nullopt;
  if (baseName.isConstructor())
    action.emplace(ImplicitMemberAction::ResolveImplicitInit);

  if (member.isSimpleName() && !baseName.isSpecial()) {
    if (baseName.getIdentifier() == getASTContext().Id_CodingKeys) {
      action.emplace(ImplicitMemberAction::ResolveCodingKeys);
    }
  } else {
    auto argumentNames = member.getArgumentNames();
    if (member.isSimpleName() || argumentNames.size() == 1) {
      if (baseName.isConstructor()) {
        if ((member.isSimpleName() || argumentNames.front() == Context.Id_from)) {
          action.emplace(ImplicitMemberAction::ResolveDecodable);
        } else if (argumentNames.front() == Context.Id_system) {
          action.emplace(ImplicitMemberAction::ResolveDistributedActorSystem);
        }
      } else if (!baseName.isSpecial() &&
                 baseName.getIdentifier() == Context.Id_encode &&
                 (member.isSimpleName() ||
                  argumentNames.front() == Context.Id_to)) {
        action.emplace(ImplicitMemberAction::ResolveEncodable);
      }
    } else if (member.isSimpleName() || argumentNames.size() == 2) {
      if (baseName.isConstructor()) {
        if (argumentNames[0] == Context.Id_resolve &&
            argumentNames[1] == Context.Id_using) {
          action.emplace(ImplicitMemberAction::ResolveDistributedActor);
        }
      }
    }
  }

  if (auto actionToTake = action) {
    (void)evaluateOrDefault(Context.evaluator,
        ResolveImplicitMemberRequest{this, actionToTake.value()}, {});
  }
}

VarDecl *NominalTypeDecl::getGlobalActorInstance() const {
  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           GlobalActorInstanceRequest{mutableThis},
                           nullptr);
}

AbstractFunctionDecl *
NominalTypeDecl::getExecutorOwnedEnqueueFunction() const {
  auto &C = getASTContext();
  StructDecl *executorJobDecl = C.getExecutorJobDecl();
  if (!executorJobDecl)
    return nullptr;

  auto proto = dyn_cast<ProtocolDecl>(this);
  if (!proto)
    return nullptr;

  llvm::SmallVector<ValueDecl *, 2> results;
  lookupQualified(getSelfNominalTypeDecl(),
                  DeclNameRef(C.Id_enqueue),
                  getLoc(), NL_ProtocolMembers,
                  results);

  for (auto candidate: results) {
    // we're specifically looking for the Executor protocol requirement
    if (!isa<ProtocolDecl>(candidate->getDeclContext()))
      continue;

    if (auto *funcDecl = dyn_cast<AbstractFunctionDecl>(candidate)) {
      auto params = funcDecl->getParameters();

      if (params->size() != 1)
        continue;

      if ((params->get(0)->getSpecifier() == ParamSpecifier::LegacyOwned ||
           params->get(0)->getSpecifier() == ParamSpecifier::Consuming) &&
          params->get(0)->getInterfaceType()->isEqual(executorJobDecl->getDeclaredInterfaceType())) {
        return funcDecl;
      }
    }
  }

  return nullptr;
}

AbstractFunctionDecl *
NominalTypeDecl::getExecutorLegacyOwnedEnqueueFunction() const {
  auto &C = getASTContext();
  StructDecl *legacyJobDecl = C.getJobDecl();
  if (!legacyJobDecl)
    return nullptr;

  auto proto = dyn_cast<ProtocolDecl>(this);
  if (!proto)
    return nullptr;

  llvm::SmallVector<ValueDecl *, 2> results;
  lookupQualified(getSelfNominalTypeDecl(),
                  DeclNameRef(C.Id_enqueue),
                  getLoc(), NL_ProtocolMembers,
                  results);

  for (auto candidate: results) {
    // we're specifically looking for the Executor protocol requirement
    if (!isa<ProtocolDecl>(candidate->getDeclContext()))
      continue;

    if (auto *funcDecl = dyn_cast<AbstractFunctionDecl>(candidate)) {
      auto params = funcDecl->getParameters();

      if (params->size() != 1)
        continue;

      if ((params->get(0)->getSpecifier() == ParamSpecifier::LegacyOwned ||
          params->get(0)->getSpecifier() == ParamSpecifier::Consuming) &&
          params->get(0)->getInterfaceType()->isEqual(legacyJobDecl->getDeclaredInterfaceType())) {
        return funcDecl;
      }
    }
  }

  return nullptr;
}

AbstractFunctionDecl *
NominalTypeDecl::getExecutorLegacyUnownedEnqueueFunction() const {
  auto &C = getASTContext();
  StructDecl *unownedJobDecl = C.getUnownedJobDecl();
  if (!unownedJobDecl)
    return nullptr;

  auto proto = dyn_cast<ProtocolDecl>(this);
  if (!proto)
    return nullptr;

  llvm::SmallVector<ValueDecl *, 2> results;
  lookupQualified(getSelfNominalTypeDecl(),
                  DeclNameRef(C.Id_enqueue),
                  getLoc(), NL_ProtocolMembers,
                  results);

  for (auto candidate: results) {
    // we're specifically looking for the Executor protocol requirement
    if (!isa<ProtocolDecl>(candidate->getDeclContext()))
      continue;

    if (auto *funcDecl = dyn_cast<AbstractFunctionDecl>(candidate)) {
      auto params = funcDecl->getParameters();
      if (params->size() != 1)
        continue;

      auto param = params->get(0);
      if (param->getSpecifier() == ParamSpecifier::LegacyOwned ||
          param->getSpecifier() == ParamSpecifier::Consuming) {
        return funcDecl;
      }
    }
  }

  return nullptr;
}

ClassDecl::ClassDecl(SourceLoc ClassLoc, Identifier Name, SourceLoc NameLoc,
                     ArrayRef<InheritedEntry> Inherited,
                     GenericParamList *GenericParams, DeclContext *Parent,
                     bool isActor)
  : NominalTypeDecl(DeclKind::Class, Parent, Name, NameLoc, Inherited,
                    GenericParams),
    ClassLoc(ClassLoc) {
  Bits.ClassDecl.InheritsSuperclassInits = 0;
  Bits.ClassDecl.ComputedInheritsSuperclassInits = 0;
  Bits.ClassDecl.RawForeignKind = 0;
  Bits.ClassDecl.HasMissingDesignatedInitializers = 0;
  Bits.ClassDecl.ComputedHasMissingDesignatedInitializers = 0;
  Bits.ClassDecl.HasMissingVTableEntries = 0;
  Bits.ClassDecl.ComputedHasMissingVTableEntries = 0;
  Bits.ClassDecl.IsIncompatibleWithWeakReferences = 0;
  Bits.ClassDecl.IsActor = isActor;
}

bool ClassDecl::hasResilientMetadata() const {
  // Imported classes don't have a vtable, etc, at all.
  if (hasClangNode())
    return false;

  // If the module is not resilient, neither is the class metadata.
  if (!getModuleContext()->isResilient())
    return false;

  // If the class is not public or package, we can't use it outside the module at all.
  // Take enable testing into account.
  if (getEffectiveAccess() < AccessLevel::Package)
    return false;

  // Otherwise we access metadata members, such as vtable entries, resiliently.
  return true;
}

bool ClassDecl::hasResilientMetadata(ModuleDecl *M,
                                     ResilienceExpansion expansion) const {
  switch (expansion) {
  case ResilienceExpansion::Minimal:
    return hasResilientMetadata();
  case ResilienceExpansion::Maximal:
    return M != getModuleContext() && hasResilientMetadata();
  }
  llvm_unreachable("bad resilience expansion");
}

DestructorDecl *ClassDecl::getDestructor() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
                           GetDestructorRequest{const_cast<ClassDecl *>(this)},
                           nullptr);
}

/// Synthesizer callback for an empty implicit function body.
static std::pair<BraceStmt *, bool>
synthesizeEmptyFunctionBody(AbstractFunctionDecl *afd, void *context) {
  ASTContext &ctx = afd->getASTContext();
  return { BraceStmt::create(ctx, afd->getLoc(), { }, afd->getLoc(), true),
           /*isTypeChecked=*/true };
}

DestructorDecl *
GetDestructorRequest::evaluate(Evaluator &evaluator, ClassDecl *CD) const {
  auto dc = CD->getImplementationContext();

  auto &ctx = CD->getASTContext();
  auto *DD = new (ctx) DestructorDecl(CD->getLoc(), dc->getAsGenericContext());

  DD->setImplicit();

  // Synthesize an empty body for the destructor as needed.
  DD->setBodySynthesizer(synthesizeEmptyFunctionBody);

  // Propagate access control and versioned-ness.
  DD->copyFormalAccessFrom(CD, /*sourceIsParentContext*/true);

  // Mark DD as ObjC, as all dtors are.
  DD->setIsObjC(ctx.LangOpts.EnableObjCInterop);
  if (ctx.LangOpts.EnableObjCInterop)
    CD->recordObjCMethod(DD, DD->getObjCSelector());

  // Mark it as synthesized to make its location in getEmittedMembers()
  // deterministic.
  DD->setSynthesized(true);

  return DD;
}

bool ClassDecl::isDefaultActor() const {
  return isDefaultActor(getModuleContext(), ResilienceExpansion::Minimal);
}

bool ClassDecl::isDefaultActor(ModuleDecl *M,
                               ResilienceExpansion expansion) const {
  auto mutableThis = const_cast<ClassDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           IsDefaultActorRequest{mutableThis, M,
                                                 expansion},
                           false);
}

const ClassDecl *ClassDecl::getRootActorClass() const {
  if (!isActor()) return nullptr;
  auto cur = this;
  while (true) {
    auto super = cur->getSuperclassDecl();
    if (!super || !super->isActor())
      return cur;
    cur = super;
  }
}

bool ClassDecl::hasMissingDesignatedInitializers() const {
  return evaluateOrDefault(
      getASTContext().evaluator,
      HasMissingDesignatedInitializersRequest{const_cast<ClassDecl *>(this)},
      false);
}

bool ClassDecl::hasMissingVTableEntries() const {
  if (!Bits.ClassDecl.ComputedHasMissingVTableEntries) {
    auto *mutableThis = const_cast<ClassDecl *>(this);
    mutableThis->Bits.ClassDecl.ComputedHasMissingVTableEntries = 1;
    mutableThis->loadAllMembers();
  }

  return Bits.ClassDecl.HasMissingVTableEntries;
}

bool ClassDecl::isIncompatibleWithWeakReferences() const {
  if (Bits.ClassDecl.IsIncompatibleWithWeakReferences) {
    return true;
  }
  if (auto superclass = getSuperclassDecl()) {
    return superclass->isIncompatibleWithWeakReferences();
  }
  return false;
}

bool ClassDecl::inheritsSuperclassInitializers() const {
  // If there's no superclass, there's nothing to inherit.
  if (!getSuperclassDecl())
    return false;

  auto &ctx = getASTContext();
  auto *mutableThis = const_cast<ClassDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator, InheritsSuperclassInitializersRequest{mutableThis}, false);
}

AncestryOptions ClassDecl::checkAncestry() const {
  return AncestryOptions(evaluateOrDefault(getASTContext().evaluator,
                           ClassAncestryFlagsRequest{const_cast<ClassDecl *>(this)},
                           AncestryFlags()));
}
        
AncestryFlags
ClassAncestryFlagsRequest::evaluate(Evaluator &evaluator,
                                    ClassDecl *value) const {
  AncestryOptions result;
  const ClassDecl *CD = value;
  const ClassDecl *PreviousCD = nullptr;
  auto *M = value->getParentModule();

  do {
    if (CD->isGenericContext())
      result |= AncestryFlags::Generic;

    // Note: it's OK to check for @objc explicitly instead of calling isObjC()
    // to infer it since we're going to visit every superclass.
    if (CD->getAttrs().hasAttribute<ObjCAttr>())
      result |= AncestryFlags::ObjC;

    if (CD->getAttrs().hasAttribute<ObjCMembersAttr>())
      result |= AncestryFlags::ObjCMembers;

    if (CD->hasClangNode()) {
      result |= AncestryFlags::ClangImported;

      // Inheriting from an ObjC-defined class generally forces the use
      // of the ObjC object model, but certain classes that directly
      // inherit from NSObject can change that.
      if (!PreviousCD ||
          !(CD->isNSObject() && PreviousCD->isNativeNSObjectSubclass()))
        result |= AncestryFlags::ObjCObjectModel;
    }

    if (CD->hasResilientMetadata())
      result |= AncestryFlags::Resilient;

    if (CD->hasResilientMetadata(M, ResilienceExpansion::Maximal))
      result |= AncestryFlags::ResilientOther;

    if (CD->getAttrs().hasAttribute<RequiresStoredPropertyInitsAttr>())
      result |= AncestryFlags::RequiresStoredPropertyInits;

    PreviousCD = CD;
    CD = CD->getSuperclassDecl();
  } while (CD != nullptr);

  return AncestryFlags(result.toRaw());
}
      
void swift::simple_display(llvm::raw_ostream &out, AncestryFlags value) {
  AncestryOptions opts(value);
  out << "{ ";
  // If we have more than one bit set, we need to print the separator.
  bool wantsSeparator = false;
  auto printBit = [&wantsSeparator, &out](bool val, StringRef name) {
    if (wantsSeparator) {
      out << ", ";
    }
      
    if (!wantsSeparator) {
      wantsSeparator = true;
    }
      
    out << name;
    if (val) {
      out << " = true";
    } else {
      out << " = false";
    }
  };
  printBit(opts.contains(AncestryFlags::ObjC), "ObjC");
  printBit(opts.contains(AncestryFlags::ObjCMembers), "ObjCMembers");
  printBit(opts.contains(AncestryFlags::Generic), "Generic");
  printBit(opts.contains(AncestryFlags::Resilient), "Resilient");
  printBit(opts.contains(AncestryFlags::ResilientOther), "ResilientOther");
  printBit(opts.contains(AncestryFlags::ClangImported), "ClangImported");
  printBit(opts.contains(AncestryFlags::RequiresStoredPropertyInits),
           "RequiresStoredPropertyInits");
  out << " }";
}

bool ClassDecl::isSuperclassOf(const ClassDecl *other) const {
  llvm::SmallPtrSet<const ClassDecl *, 8> visited;

  do {
    if (!visited.insert(other).second)
      break;

    if (this == other)
      return true;

    other = other->getSuperclassDecl();
  } while (other != nullptr);

  return false;
}

ClassDecl::MetaclassKind ClassDecl::getMetaclassKind() const {
  assert(getASTContext().LangOpts.EnableObjCInterop &&
         "querying metaclass kind without objc interop");
  auto objc = checkAncestry(AncestryFlags::ObjC);
  return objc ? MetaclassKind::ObjC : MetaclassKind::SwiftStub;
}

/// Mangle the name of a protocol or class for use in the Objective-C
/// runtime.
static StringRef mangleObjCRuntimeName(const NominalTypeDecl *nominal,
                                       llvm::SmallVectorImpl<char> &buffer) {
  {
    Mangle::ASTMangler Mangler;
    std::string MangledName = Mangler.mangleObjCRuntimeName(nominal);

    buffer.clear();
    llvm::raw_svector_ostream os(buffer);
    os << MangledName;
  }

  assert(buffer.size() && "Invalid buffer size");
  return StringRef(buffer.data(), buffer.size());
}

StringRef ClassDecl::getObjCRuntimeName(
                       llvm::SmallVectorImpl<char> &buffer) const {
  // If there is a Clang declaration, use it's runtime name.
  if (auto objcClass
        = dyn_cast_or_null<clang::ObjCInterfaceDecl>(getClangDecl()))
    return objcClass->getObjCRuntimeNameAsString();

  // If there is an 'objc' attribute with a name, use that name.
  if (auto attr = getAttrs().getAttribute<ObjCRuntimeNameAttr>())
    return attr->Name;
  if (auto objc = getAttrs().getAttribute<ObjCAttr>()) {
    if (auto name = objc->getName())
      return name->getString(buffer);
  }

  // Produce the mangled name for this class.
  return mangleObjCRuntimeName(this, buffer);
}

ArtificialMainKind Decl::getArtificialMainKind() const {
  if (getAttrs().hasAttribute<UIApplicationMainAttr>())
    return ArtificialMainKind::UIApplicationMain;
  if (getAttrs().hasAttribute<NSApplicationMainAttr>())
    return ArtificialMainKind::NSApplicationMain;
  if (isa<FuncDecl>(this))
    return ArtificialMainKind::TypeMain;
  llvm_unreachable("type has no @Main attr?!");
}

static bool isOverridingDecl(const ValueDecl *Derived,
                             const ValueDecl *Base) {
  while (Derived) {
    if (Derived == Base)
      return true;
    Derived = Derived->getOverriddenDecl();
  }
  return false;
}

static ValueDecl *findOverridingDecl(const ClassDecl *C,
                                     const ValueDecl *Base) {
  // FIXME: This is extremely inefficient. The SILOptimizer should build a
  // reverse lookup table to answer these types of queries.
  for (auto M : C->getMembers()) {
    if (auto *Derived = dyn_cast<ValueDecl>(M))
      if (::isOverridingDecl(Derived, Base))
        return Derived;
  }

  return nullptr;
}

AbstractFunctionDecl *
ClassDecl::findOverridingDecl(const AbstractFunctionDecl *Method) const {
  if (auto *Accessor = dyn_cast<AccessorDecl>(Method)) {
    auto *Storage = Accessor->getStorage();
    if (auto *Derived = ::findOverridingDecl(this, Storage)) {
      auto *DerivedStorage = cast<AbstractStorageDecl>(Derived);
      return DerivedStorage->getOpaqueAccessor(Accessor->getAccessorKind());
    }

    return nullptr;
  }

  return cast_or_null<AbstractFunctionDecl>(::findOverridingDecl(this, Method));
}

AbstractFunctionDecl *
ClassDecl::findImplementingMethod(const AbstractFunctionDecl *Method) const {
  // FIXME: This is extremely inefficient. The SILOptimizer should build a
  // reverse lookup table to answer these types of queries.
  const ClassDecl *C = this;
  while (C) {
    if (C == Method->getDeclContext())
      return const_cast<AbstractFunctionDecl *>(Method);

    if (auto *Derived = C->findOverridingDecl(Method))
      return Derived;

    // Check the superclass
    C = C->getSuperclassDecl();
  }
  return nullptr;
}

bool ClassDecl::walkSuperclasses(
    llvm::function_ref<TypeWalker::Action(ClassDecl *)> fn) const {

  auto *cls = const_cast<ClassDecl *>(this);

  while (cls) {
    switch (fn(cls)) {
    case TypeWalker::Action::Stop:
      return true;
    case TypeWalker::Action::SkipNode:
      return false;
    case TypeWalker::Action::Continue:
      cls = cls->getSuperclassDecl();
      continue;
    }
  }

  return false;
}

bool ClassDecl::isForeignReferenceType() const {
  auto clangRecordDecl = dyn_cast_or_null<clang::RecordDecl>(getClangDecl());
  if (!clangRecordDecl)
    return false;

  CxxRecordSemanticsKind kind = evaluateOrDefault(
      getASTContext().evaluator,
      CxxRecordSemantics({clangRecordDecl, getASTContext()}), {});
  return kind == CxxRecordSemanticsKind::Reference;
}

bool ClassDecl::hasRefCountingAnnotations() const {
  return evaluateOrDefault(getASTContext().evaluator,
                           CustomRefCountingOperation(
                               {this, CustomRefCountingOperationKind::release}),
                           {})
             .kind != CustomRefCountingOperationResult::immortal;
}

ReferenceCounting ClassDecl::getObjectModel() const {
  if (isForeignReferenceType())
    return hasRefCountingAnnotations() ? ReferenceCounting::Custom
                                       : ReferenceCounting::None;

  if (checkAncestry(AncestryFlags::ObjCObjectModel))
    return ReferenceCounting::ObjC;

  return ReferenceCounting::Native;
}

EnumCaseDecl *EnumCaseDecl::create(SourceLoc CaseLoc,
                                   ArrayRef<EnumElementDecl *> Elements,
                                   DeclContext *DC) {
  size_t bytes = totalSizeToAlloc<EnumElementDecl *>(Elements.size());
  void *buf = DC->getASTContext().Allocate(bytes, alignof(EnumCaseDecl));
  return ::new (buf) EnumCaseDecl(CaseLoc, Elements, DC);
}

bool EnumDecl::hasPotentiallyUnavailableCaseValue() const {
  (void)this->hasOnlyCasesWithoutAssociatedValues(); // Prime the cache
  return static_cast<bool>(Bits.EnumDecl.HasAnyUnavailableValues);
}

bool EnumDecl::hasCasesUnavailableDuringLowering() const {
  (void)this->hasOnlyCasesWithoutAssociatedValues(); // Prime the cache
  return static_cast<bool>(Bits.EnumDecl.HasAnyUnavailableDuringLoweringValues);
}

bool EnumDecl::hasOnlyCasesWithoutAssociatedValues() const {
  // Check whether we already have a cached answer.
  switch (static_cast<AssociatedValueCheck>(
            Bits.EnumDecl.HasAssociatedValues)) {
    case AssociatedValueCheck::Unchecked:
      // Compute below.
      break;

    case AssociatedValueCheck::NoAssociatedValues:
      return true;

    case AssociatedValueCheck::HasAssociatedValues:
      return false;
  }

  bool hasAnyUnavailableValues = false;
  bool hasAnyUnavailableDuringLoweringValues = false;
  bool hasAssociatedValues = false;

  for (auto elt : getAllElements()) {
    for (auto Attr : elt->getAttrs()) {
      if (auto AvAttr = dyn_cast<AvailableAttr>(Attr)) {
        if (!AvAttr->isInvalid())
          hasAnyUnavailableValues = true;
      }
    }

    if (!elt->isAvailableDuringLowering())
      hasAnyUnavailableDuringLoweringValues = true;

    if (elt->hasAssociatedValues())
      hasAssociatedValues = true;
  }

  EnumDecl *enumDecl = const_cast<EnumDecl *>(this);

  enumDecl->Bits.EnumDecl.HasAnyUnavailableValues = hasAnyUnavailableValues;
  enumDecl->Bits.EnumDecl.HasAnyUnavailableDuringLoweringValues =
      hasAnyUnavailableDuringLoweringValues;
  enumDecl->Bits.EnumDecl.HasAssociatedValues = static_cast<unsigned>(
      hasAssociatedValues ? AssociatedValueCheck::HasAssociatedValues
                          : AssociatedValueCheck::NoAssociatedValues);
  return !hasAssociatedValues;
}

bool EnumDecl::treatAsExhaustiveForDiags(const DeclContext *useDC) const {
  return isFormallyExhaustive(useDC) ||
         (useDC && getModuleContext()->inSamePackage(useDC->getParentModule()));
}

bool EnumDecl::isFormallyExhaustive(const DeclContext *useDC) const {
  // Enums explicitly marked frozen are exhaustive.
  if (getAttrs().hasAttribute<FrozenAttr>())
    return true;

  // Objective-C enums /not/ marked frozen are /not/ exhaustive.
  // Note: This implicitly holds @objc enums defined in Swift to a higher
  // standard!
  if (hasClangNode())
    return false;

  // Non-imported enums in non-resilient modules are exhaustive.
  const ModuleDecl *containingModule = getModuleContext();
  if (!containingModule->isResilient())
    return true;

  // Non-public, non-versioned enums are always exhaustive.
  AccessScope accessScope = getFormalAccessScope(/*useDC*/ nullptr,
                                                 /*respectVersioned*/ true);
  if (!accessScope.isPublicOrPackage())
    return true;

  // All other checks are use-site specific; with no further information, the
  // enum must be treated non-exhaustively.
  if (!useDC)
    return false;

  // Enums in the same module as the use site are exhaustive /unless/ the use
  // site is inlinable.
  if (useDC->getParentModule() == containingModule)
    if (useDC->getResilienceExpansion() == ResilienceExpansion::Maximal)
      return true;

  // Testably imported enums are exhaustive, on the grounds that only the author
  // of the original library can import it testably.
  if (auto *useSF = dyn_cast<SourceFile>(useDC->getModuleScopeContext()))
    if (useSF->hasTestableOrPrivateImport(AccessLevel::Internal,
                                          containingModule))
      return true;

  // Otherwise, the enum is non-exhaustive.
  return false;
}

bool EnumDecl::isEffectivelyExhaustive(ModuleDecl *M,
                                       ResilienceExpansion expansion) const {
  // Generated Swift code commits to handling garbage values of @objc enums,
  // whether imported or not, to deal with C's loose rules around enums.
  // This covers both frozen and non-frozen @objc enums.
  if (isObjC())
    return false;

  // Otherwise, the only non-exhaustive enums are those that don't have
  // a fixed layout; however, they are treated as exhaustive if package
  // optimization is enabled.
  assert((isFormallyExhaustive(M) || bypassResilienceInPackage(M)) ==
             !isResilient(M, ResilienceExpansion::Maximal) &&
         "ignoring the effects of @inlinable, @testable, and @objc, "
         "these should match up");
  return !isResilient(M, expansion);
}
      
void EnumDecl::setHasFixedRawValues() {
  SemanticFlags |= OptionSet<EnumDecl::SemanticInfoFlags>{EnumDecl::HasFixedRawValues};
}

bool EnumDecl::hasCircularRawValue() const {
  auto &ctx = getASTContext();
  auto *mutableThis = const_cast<EnumDecl *>(this);
  return evaluateOrDefault(ctx.evaluator,
                           HasCircularRawValueRequest{mutableThis}, true);
}

ProtocolDecl::ProtocolDecl(DeclContext *DC, SourceLoc ProtocolLoc,
                           SourceLoc NameLoc, Identifier Name,
                           ArrayRef<PrimaryAssociatedTypeName> PrimaryAssociatedTypeNames,
                           ArrayRef<InheritedEntry> Inherited,
                           TrailingWhereClause *TrailingWhere)
    : NominalTypeDecl(DeclKind::Protocol, DC, Name, NameLoc, Inherited,
                      nullptr),
      ProtocolLoc(ProtocolLoc),
      PrimaryAssociatedTypeNames(PrimaryAssociatedTypeNames) {
  Bits.ProtocolDecl.RequiresClassValid = false;
  Bits.ProtocolDecl.RequiresClass = false;
  Bits.ProtocolDecl.ExistentialConformsToSelfValid = false;
  Bits.ProtocolDecl.ExistentialConformsToSelf = false;
  Bits.ProtocolDecl.InheritedProtocolsValid = false;
  Bits.ProtocolDecl.AllInheritedProtocolsValid = false;
  Bits.ProtocolDecl.HasMissingRequirements = false;
  Bits.ProtocolDecl.KnownProtocol = 0;
  Bits.ProtocolDecl.HasAssociatedTypes = false;
  Bits.ProtocolDecl.HasLazyAssociatedTypes = false;
  Bits.ProtocolDecl.HasRequirementSignature = false;
  Bits.ProtocolDecl.HasLazyRequirementSignature = false;
  Bits.ProtocolDecl.ProtocolRequirementsValid = false;
  setTrailingWhereClause(TrailingWhere);
}

bool ProtocolDecl::isMarkerProtocol() const {
  return getAttrs().hasAttribute<MarkerAttr>();
}

std::optional<InvertibleProtocolKind>
ProtocolDecl::getInvertibleProtocolKind() const {
  if (auto kp = getKnownProtocolKind())
    return ::getInvertibleProtocolKind(*kp);

  return std::nullopt;
}

ObjCRequirementMap ProtocolDecl::getObjCRequiremenMap() const {
  ObjCRequirementMap defaultMap;
  if (!isObjC())
    return defaultMap;

  return evaluateOrDefault(getASTContext().evaluator,
                           ObjCRequirementMapRequest{this}, defaultMap);
}

ArrayRef<ProtocolDecl *> ProtocolDecl::getInheritedProtocols() const {
  auto *mutThis = const_cast<ProtocolDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           InheritedProtocolsRequest{mutThis},
                           {});
}

ArrayRef<ProtocolDecl *> ProtocolDecl::getAllInheritedProtocols() const {
  // Avoid evaluator overhead because we call this from Symbol::compare()
  // in the Requirement Machine.
  if (Bits.ProtocolDecl.AllInheritedProtocolsValid)
    return AllInheritedProtocols;

  auto *mutThis = const_cast<ProtocolDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           AllInheritedProtocolsRequest{mutThis},
                           {});
}

ArrayRef<AssociatedTypeDecl *>
ProtocolDecl::getAssociatedTypeMembers() const {
  if (Bits.ProtocolDecl.HasAssociatedTypes)
    return AssociatedTypes;

  auto *self = const_cast<ProtocolDecl *>(this);
  self->Bits.ProtocolDecl.HasAssociatedTypes = 1;

  // Clang-imported protocols never have associated types.
  if (hasClangNode())
    return ArrayRef<AssociatedTypeDecl *>();

  // Deserialized @objc protocols never have associated types.
  if (getParentSourceFile() == nullptr && isObjC())
    return ArrayRef<AssociatedTypeDecl *>();

  SmallVector<AssociatedTypeDecl *, 2> result;
  if (Bits.ProtocolDecl.HasLazyAssociatedTypes) {
    auto &ctx = getASTContext();
    auto contextData = static_cast<LazyProtocolData *>(
        ctx.getOrCreateLazyContextData(this, nullptr));

    contextData->loader->loadAssociatedTypes(
        this, contextData->associatedTypesData, result);
  } else {
    for (auto member : getProtocolRequirements()) {
      if (auto ATD = dyn_cast<AssociatedTypeDecl>(member)) {
        result.push_back(ATD);
      }
    }
  }

  self->AssociatedTypes = getASTContext().AllocateCopy(result);
  return AssociatedTypes;
}

ArrayRef<ValueDecl *> ProtocolDecl::getProtocolRequirements() const {
  auto *mutableSelf = const_cast<ProtocolDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           ProtocolRequirementsRequest{mutableSelf}, {});
}

ValueDecl *ProtocolDecl::getSingleRequirement(DeclName name) const {
  auto results = const_cast<ProtocolDecl *>(this)->lookupDirect(name);
  ValueDecl *result = nullptr;
  for (auto candidate : results) {
    if (candidate->getDeclContext() != this ||
        !candidate->isProtocolRequirement())
      continue;
    if (result) {
      // Multiple results.
      return nullptr;
    }
    result = candidate;
  }

  return result;
}

AssociatedTypeDecl *ProtocolDecl::getAssociatedType(Identifier name) const {
  auto results = const_cast<ProtocolDecl *>(this)->lookupDirect(name);
  for (auto candidate : results) {
    if (candidate->getDeclContext() == this &&
        isa<AssociatedTypeDecl>(candidate)) {
      return cast<AssociatedTypeDecl>(candidate);
    }
  }
  return nullptr;
}

ClassDecl *ProtocolDecl::getSuperclassDecl() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    SuperclassDeclRequest{const_cast<ProtocolDecl *>(this)}, nullptr);
}

bool ProtocolDecl::walkInheritedProtocols(
              llvm::function_ref<TypeWalker::Action(ProtocolDecl *)> fn) const {
  auto self = const_cast<ProtocolDecl *>(this);

  // Visit all of the inherited protocols.
  SmallPtrSet<ProtocolDecl *, 8> visited;
  SmallVector<ProtocolDecl *, 4> stack;
  stack.push_back(self);
  visited.insert(self);
  while (!stack.empty()) {
    // Pull the next protocol off the stack.
    auto proto = stack.back();
    stack.pop_back();

    switch (fn(proto)) {
    case TypeWalker::Action::Stop:
      return true;

    case TypeWalker::Action::Continue:
      // Add inherited protocols to the stack.
      for (auto inherited : proto->getInheritedProtocols()) {
        if (visited.insert(inherited).second)
          stack.push_back(inherited);
      }
      break;

    case TypeWalker::Action::SkipNode:
      break;
    }
  }

  return false;

}

bool ProtocolDecl::inheritsFrom(const ProtocolDecl *super) const {
  assert(super);

  // Fast path.
  if (this == super)
    return false;

  auto allInherited = getAllInheritedProtocols();
  return (llvm::find(allInherited, super) != allInherited.end());
}

bool ProtocolDecl::requiresClass() const {
  return evaluateOrDefault(getASTContext().evaluator,
    ProtocolRequiresClassRequest{const_cast<ProtocolDecl *>(this)}, false);
}

bool ProtocolDecl::requiresSelfConformanceWitnessTable() const {
  return isSpecificProtocol(KnownProtocolKind::Error);
}

bool ProtocolDecl::existentialConformsToSelf() const {
  return evaluateOrDefault(getASTContext().evaluator,
    ExistentialConformsToSelfRequest{const_cast<ProtocolDecl *>(this)}, true);
}

bool ProtocolDecl::hasSelfOrAssociatedTypeRequirements() const {
  return evaluateOrDefault(getASTContext().evaluator,
                           HasSelfOrAssociatedTypeRequirementsRequest{
                               const_cast<ProtocolDecl *>(this)},
                           true);
}

bool ProtocolDecl::existentialRequiresAny() const {
  if (getASTContext().LangOpts.hasFeature(Feature::ExistentialAny))
    return true;

  return hasSelfOrAssociatedTypeRequirements();
}

ArrayRef<AssociatedTypeDecl *>
ProtocolDecl::getPrimaryAssociatedTypes() const {
  return evaluateOrDefault(getASTContext().evaluator,
    PrimaryAssociatedTypesRequest{const_cast<ProtocolDecl *>(this)},
    nullptr);
}

StringRef ProtocolDecl::getObjCRuntimeName(
                          llvm::SmallVectorImpl<char> &buffer) const {
  // If there is an 'objc' attribute with a name, use that name.
  if (auto objc = getAttrs().getAttribute<ObjCAttr>()) {
    if (auto name = objc->getName())
      return name->getString(buffer);
  }

  // Produce the mangled name for this protocol.
  return mangleObjCRuntimeName(this, buffer);
}

ArrayRef<StructuralRequirement>
ProtocolDecl::getStructuralRequirements() const {
  return evaluateOrDefault(
      getASTContext().evaluator,
      StructuralRequirementsRequest{const_cast<ProtocolDecl *>(this)}, {});
}

ArrayRef<Requirement>
ProtocolDecl::getTypeAliasRequirements() const {
  return evaluateOrDefault(
      getASTContext().evaluator,
      TypeAliasRequirementsRequest{const_cast<ProtocolDecl *>(this)}, {});
}

ArrayRef<ProtocolDecl *>
ProtocolDecl::getProtocolDependencies() const {
  return evaluateOrDefault(
      getASTContext().evaluator,
      ProtocolDependenciesRequest{const_cast<ProtocolDecl *>(this)},
      std::nullopt);
}

RequirementSignature ProtocolDecl::getRequirementSignature() const {
  return getASTContext().evaluator(
               RequirementSignatureRequest{const_cast<ProtocolDecl *>(this)},
               [this]() {
                 return RequirementSignature::getPlaceholderRequirementSignature(
                     this, GenericSignatureErrors());
               });
}

bool ProtocolDecl::isComputingRequirementSignature() const {
  return getASTContext().evaluator.hasActiveRequest(
                 RequirementSignatureRequest{const_cast<ProtocolDecl*>(this)});
}

void ProtocolDecl::setRequirementSignature(RequirementSignature requirementSig) {
  RequirementSig = requirementSig;
  Bits.ProtocolDecl.HasRequirementSignature = 1;
}

void
ProtocolDecl::setLazyRequirementSignature(LazyMemberLoader *lazyLoader,
                                          uint64_t requirementSignatureData) {
  assert(!isRequirementSignatureComputed() && "requirement signature already set");

  auto contextData = static_cast<LazyProtocolData *>(
      getASTContext().getOrCreateLazyContextData(this, lazyLoader));
  contextData->requirementSignatureData = requirementSignatureData;
  Bits.ProtocolDecl.HasLazyRequirementSignature = true;

  ++NumLazyRequirementSignatures;
  // FIXME: (transitional) increment the redundant "always-on" counter.
  if (auto *Stats = getASTContext().Stats)
    ++Stats->getFrontendCounters().NumLazyRequirementSignatures;
}

void
ProtocolDecl::setLazyAssociatedTypeMembers(
    LazyMemberLoader *lazyLoader, uint64_t associatedTypesData) {
  assert(!Bits.ProtocolDecl.HasAssociatedTypes);
  assert(!Bits.ProtocolDecl.HasLazyAssociatedTypes);

  auto contextData = static_cast<LazyProtocolData *>(
      getASTContext().getOrCreateLazyContextData(this, lazyLoader));
  contextData->associatedTypesData = associatedTypesData;
  Bits.ProtocolDecl.HasLazyAssociatedTypes = true;
}

void
ProtocolDecl::setLazyPrimaryAssociatedTypeMembers(
    LazyMemberLoader *lazyLoader, uint64_t associatedTypesData) {
  assert(!Bits.ProtocolDecl.HasLazyPrimaryAssociatedTypes);

  auto contextData = static_cast<LazyProtocolData *>(
      getASTContext().getOrCreateLazyContextData(this, lazyLoader));
  contextData->primaryAssociatedTypesData = associatedTypesData;
  Bits.ProtocolDecl.HasLazyPrimaryAssociatedTypes = true;
}

void ProtocolDecl::computeKnownProtocolKind() const {
  auto module = getModuleContext();
  if (module != module->getASTContext().getStdlibModule() &&
      module != module->getASTContext().TheBuiltinModule &&
      !module->getName().is("Foundation") &&
      !module->getName().is("_Differentiation") &&
      !module->getName().is("_Concurrency") &&
      !module->getName().is("Distributed")) {
    const_cast<ProtocolDecl *>(this)->Bits.ProtocolDecl.KnownProtocol = 1;
    return;
  }

  unsigned value =
    llvm::StringSwitch<unsigned>(getBaseName().userFacingName())
#define PROTOCOL_WITH_NAME(Id, Name) \
      .Case(Name, static_cast<unsigned>(KnownProtocolKind::Id) + 2)
#include "swift/AST/KnownProtocols.def"
      .Default(1);

  const_cast<ProtocolDecl *>(this)->Bits.ProtocolDecl.KnownProtocol = value;
}

std::optional<KnownDerivableProtocolKind>
ProtocolDecl::getKnownDerivableProtocolKind() const {
  const auto knownKind = getKnownProtocolKind();
  if (!knownKind)
    return std::nullopt;

  switch (*knownKind) {
  case KnownProtocolKind::RawRepresentable:
    return KnownDerivableProtocolKind::RawRepresentable;
  case KnownProtocolKind::OptionSet:
    return KnownDerivableProtocolKind::OptionSet;
  case KnownProtocolKind::CaseIterable:
    return KnownDerivableProtocolKind::CaseIterable;
  case KnownProtocolKind::Comparable:
    return KnownDerivableProtocolKind::Comparable;
  case KnownProtocolKind::Equatable:
    return KnownDerivableProtocolKind::Equatable;
  case KnownProtocolKind::Hashable:
    return KnownDerivableProtocolKind::Hashable;
  case KnownProtocolKind::BridgedNSError:
    return KnownDerivableProtocolKind::BridgedNSError;
  case KnownProtocolKind::CodingKey:
    return KnownDerivableProtocolKind::CodingKey;
  case KnownProtocolKind::Encodable:
    return KnownDerivableProtocolKind::Encodable;
  case KnownProtocolKind::Decodable:
    return KnownDerivableProtocolKind::Decodable;
  case KnownProtocolKind::AdditiveArithmetic:
    return KnownDerivableProtocolKind::AdditiveArithmetic;
  case KnownProtocolKind::Differentiable:
    return KnownDerivableProtocolKind::Differentiable;
  case KnownProtocolKind::Identifiable:
    return KnownDerivableProtocolKind::Identifiable;
  case KnownProtocolKind::Actor:
    return KnownDerivableProtocolKind::Actor;
  case KnownProtocolKind::DistributedActor:
    return KnownDerivableProtocolKind::DistributedActor;
  case KnownProtocolKind::DistributedActorSystem:
    return KnownDerivableProtocolKind::DistributedActorSystem;
  default:
    return std::nullopt;
  }
}

bool ProtocolDecl::hasCircularInheritedProtocols() const {
  auto &ctx = getASTContext();
  auto *mutableThis = const_cast<ProtocolDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator, HasCircularInheritedProtocolsRequest{mutableThis}, true);
}

/// Returns a descriptive name for the given accessor/addressor kind.
StringRef swift::getAccessorNameForDiagnostic(AccessorKind accessorKind,
                                              bool article) {
  switch (accessorKind) {
  case AccessorKind::Get:
    return article ? "a getter" : "getter";
  case AccessorKind::DistributedGet:
    return article ? "a distributed getter" : "distributed getter";
  case AccessorKind::Set:
    return article ? "a setter" : "setter";
  case AccessorKind::Address:
    return article ? "an addressor" : "addressor";
  case AccessorKind::MutableAddress:
    return article ? "a mutable addressor" : "mutable addressor";
  case AccessorKind::Read:
  case AccessorKind::Read2:
    return article ? "a 'read' accessor" : "'read' accessor";
  case AccessorKind::Modify:
  case AccessorKind::Modify2:
    return article ? "a 'modify' accessor" : "'modify' accessor";
  case AccessorKind::WillSet:
    return "'willSet'";
  case AccessorKind::DidSet:
    return "'didSet'";
  case AccessorKind::Init:
    return article ? "an init accessor" : "init accessor";
  }
  llvm_unreachable("bad accessor kind");
}

StringRef swift::getAccessorNameForDiagnostic(AccessorDecl *accessor,
                                              bool article) {
  return getAccessorNameForDiagnostic(accessor->getAccessorKind(),
                                      article);
}

bool AbstractStorageDecl::hasStorage() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    HasStorageRequest{const_cast<AbstractStorageDecl *>(this)},
    false);
}

StorageImplInfo AbstractStorageDecl::getImplInfo() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    StorageImplInfoRequest{const_cast<AbstractStorageDecl *>(this)},
    StorageImplInfo::getSimpleStored(StorageIsMutable));
}

void AbstractStorageDecl::cacheImplInfo(StorageImplInfo implInfo) {
  LazySemanticInfo.ImplInfoComputed = 1;
  ImplInfo = implInfo;
}

void AbstractStorageDecl::setImplInfo(StorageImplInfo implInfo) {
  cacheImplInfo(implInfo);

  if (isImplicit()) {
    if (!LazySemanticInfo.HasStorageComputed) {
      LazySemanticInfo.HasStorageComputed = true;
      LazySemanticInfo.HasStorage = implInfo.hasStorage();
    } else {
      assert(LazySemanticInfo.HasStorage == implInfo.hasStorage());
    }
  }
}

bool AbstractStorageDecl::hasPrivateAccessor() const {
  for (auto accessor : getAllAccessors()) {
    if (hasPrivateOrFilePrivateFormalAccess(accessor))
      return true;
  }
  return false;
}

bool AbstractStorageDecl::hasDidSetOrWillSetDynamicReplacement() const {
  if (auto *func = getParsedAccessor(AccessorKind::DidSet))
    return (bool)func->getDynamicallyReplacedDecl();
  if (auto *func = getParsedAccessor(AccessorKind::WillSet))
    return (bool)func->getDynamicallyReplacedDecl();
  return false;
}

bool AbstractStorageDecl::hasAnyNativeDynamicAccessors() const {
  for (auto accessor : getAllAccessors()) {
    if (accessor->shouldUseNativeDynamicDispatch())
      return true;
  }
  return false;
}

void AbstractStorageDecl::setAccessors(SourceLoc lbraceLoc,
                                       ArrayRef<AccessorDecl *> accessors,
                                       SourceLoc rbraceLoc) {
  // This method is called after we've already recorded an accessors clause
  // only on recovery paths and only when that clause was empty, or when a
  // macro expands to accessors (in which case, we may already have accessors).
  auto record = Accessors.getPointer();
  if (record) {
    for (auto accessor : accessors) {
      if (!record->getAccessor(accessor->getAccessorKind()))
        (void) record->addOpaqueAccessor(accessor);
    }
  } else {
    record = AccessorRecord::create(getASTContext(),
                                    SourceRange(lbraceLoc, rbraceLoc),
                                    accessors);
    Accessors.setPointer(record);
  }
}

// Compute the number of opaque accessors.
const size_t NumOpaqueAccessors =
  0
#define ACCESSOR(ID)
#define OPAQUE_ACCESSOR(ID, KEYWORD) \
  + 1
#include "swift/AST/AccessorKinds.def"
;

AbstractStorageDecl::AccessorRecord *
AbstractStorageDecl::AccessorRecord::create(ASTContext &ctx,
                                            SourceRange braces,
                                            ArrayRef<AccessorDecl*> accessors) {
  // Silently cap the number of accessors we store at a number that should
  // be easily sufficient for all the valid cases, including space for adding
  // implicit opaque accessors later.
  //
  // We should have already emitted a diagnostic in the parser if we have
  // this many accessors, because most of them will necessarily be redundant.
  if (accessors.size() + NumOpaqueAccessors > MaxNumAccessors) {
    accessors = accessors.slice(0, MaxNumAccessors - NumOpaqueAccessors);
  }

  // Make sure that we have enough space to add implicit opaque accessors later.
  size_t numMissingOpaque = NumOpaqueAccessors;
  {
#define ACCESSOR(ID)
#define OPAQUE_ACCESSOR(ID, KEYWORD)          \
    bool has##ID = false;
#include "swift/AST/AccessorKinds.def"
    for (auto accessor : accessors) {
      switch (accessor->getAccessorKind()) {
#define ACCESSOR(ID)                          \
      case AccessorKind::ID:                  \
        continue;
#define OPAQUE_ACCESSOR(ID, KEYWORD)          \
      case AccessorKind::ID:                  \
        if (!has##ID) {                       \
          has##ID = true;                     \
          --numMissingOpaque;                 \
        }                                     \
        continue;
#include "swift/AST/AccessorKinds.def"
      }
      llvm_unreachable("bad accessor kind");
    }
  }

  auto accessorsCapacity = AccessorIndex(accessors.size() + numMissingOpaque);
  void *mem = ctx.Allocate(totalSizeToAlloc<AccessorDecl*>(accessorsCapacity),
                           alignof(AccessorRecord));
  return new (mem) AccessorRecord(braces, accessors, accessorsCapacity);
}

AbstractStorageDecl::AccessorRecord::AccessorRecord(SourceRange braces,
                                            ArrayRef<AccessorDecl *> accessors,
                                            AccessorIndex accessorsCapacity)
    : Braces(braces), NumAccessors(accessors.size()),
      AccessorsCapacity(accessorsCapacity), AccessorIndices{} {

  // Copy the complete accessors list into place.
  memcpy(getAccessorsBuffer().data(), accessors.data(),
         accessors.size() * sizeof(AccessorDecl*));

  // Register all the accessors.
  for (auto index : indices(accessors)) {
    (void) registerAccessor(accessors[index], index);
  }
}

void AbstractStorageDecl::AccessorRecord::addOpaqueAccessor(AccessorDecl *decl){
  assert(decl);

  // Add the accessor to the array.
  assert(NumAccessors < AccessorsCapacity);
  AccessorIndex index = NumAccessors++;
  getAccessorsBuffer()[index] = decl;

  // Register it.
  bool isUnique = registerAccessor(decl, index);
  assert(isUnique && "adding opaque accessor that's already present");
  (void) isUnique;
}

void AbstractStorageDecl::AccessorRecord::removeAccessor(
    AccessorDecl *accessor
) {
  // Remove this accessor from the list of accessors.
  assert(getAccessor(accessor->getAccessorKind()) == accessor);
  (void)std::remove(getAccessorsBuffer().begin(), getAccessorsBuffer().end(),
                    accessor);

  // Clear out the accessor kind -> index mapping.
  std::memset(AccessorIndices, 0, sizeof(AccessorIndices));

  // Re-add all of the remaining accessors to build up the index mapping.
  unsigned numAccessorsLeft = NumAccessors - 1;
  NumAccessors = numAccessorsLeft;
  auto buffer = getAccessorsBuffer();
  NumAccessors = 0;
  for (auto index : range(0, numAccessorsLeft)) {
    addOpaqueAccessor(buffer[index]);
  }
}

/// Register that we have an accessor of the given kind.
bool AbstractStorageDecl::AccessorRecord::registerAccessor(AccessorDecl *decl,
                                                           AccessorIndex index){
  // Remember that we have at least one accessor of this kind.
  auto &indexSlot = AccessorIndices[unsigned(decl->getAccessorKind())];
  if (indexSlot) {
    return false;
  } else {
    indexSlot = index + 1;

    assert(getAccessor(decl->getAccessorKind()) == decl);
    return true;
  }
}

AccessLevel
AbstractStorageDecl::getSetterFormalAccess() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
        SetterAccessLevelRequest{const_cast<AbstractStorageDecl *>(this)},
        AccessLevel::Private);
}

AccessScope
AbstractStorageDecl::getSetterFormalAccessScope(const DeclContext *useDC,
                                    bool treatUsableFromInlineAsPublic) const {
  return getAccessScopeForFormalAccess(this, getSetterFormalAccess(), useDC,
                                       treatUsableFromInlineAsPublic);
}

void AbstractStorageDecl::setComputedSetter(AccessorDecl *setter) {
  assert(getImplInfo().getReadImpl() == ReadImplKind::Get);
  assert(!getImplInfo().supportsMutation());
  assert(getAccessor(AccessorKind::Get) && "invariant check: missing getter");
  assert(!getAccessor(AccessorKind::Set) && "already has a setter");
  assert(hasClangNode() && "should only be used for ObjC properties");
  assert(setter && "should not be called for readonly properties");
  assert(setter->getAccessorKind() == AccessorKind::Set);

  setImplInfo(StorageImplInfo::getMutableComputed());
  Accessors.getPointer()->addOpaqueAccessor(setter);
}

void
AbstractStorageDecl::setSynthesizedAccessor(AccessorKind kind,
                                            AccessorDecl *accessor) {
  assert(!getAccessor(kind) && "accessor already exists");
  assert(accessor->getAccessorKind() == kind);

  auto accessors = Accessors.getPointer();
  if (!accessors) {
    accessors = AccessorRecord::create(getASTContext(), SourceRange(), {});
    Accessors.setPointer(accessors);
  }

  accessors->addOpaqueAccessor(accessor);
}

static std::optional<ObjCSelector>
getNameFromObjcAttribute(const ObjCAttr *attr, DeclName preferredName) {
  if (!attr)
    return std::nullopt;
  if (auto name = attr->getName()) {
    if (attr->isNameImplicit()) {
      // preferredName > implicit name, because implicit name is just cached
      // actual name.
      if (!preferredName)
        return *name;
    } else {
      // explicit name > preferred name.
      return *name;
    }
  }
  return std::nullopt;
}

ObjCSelector
AbstractStorageDecl::getObjCGetterSelector(Identifier preferredName) const {
  // If the getter has an @objc attribute with a name, use that.
  if (auto getter = getAccessor(AccessorKind::Get)) {
      if (auto name = getNameFromObjcAttribute(getter->getAttrs().
          getAttribute<ObjCAttr>(), preferredName))
        return *name;
  }

  // Subscripts use a specific selector.
  auto &ctx = getASTContext();
  if (auto *SD = dyn_cast<SubscriptDecl>(this)) {
    switch (SD->getObjCSubscriptKind()) {
    case ObjCSubscriptKind::Indexed:
      return ObjCSelector(ctx, 1, ctx.Id_objectAtIndexedSubscript);
    case ObjCSubscriptKind::Keyed:
      return ObjCSelector(ctx, 1, ctx.Id_objectForKeyedSubscript);
    }
  }

  // The getter selector is the property name itself.
  auto var = cast<VarDecl>(this);
  auto name = var->getObjCPropertyName();

  // Use preferred name is specified.
  if (!preferredName.empty())
    name = preferredName;
  return VarDecl::getDefaultObjCGetterSelector(ctx, name);
}

ObjCSelector
AbstractStorageDecl::getObjCSetterSelector(Identifier preferredName) const {
  // If the setter has an @objc attribute with a name, use that.
  auto setter = getAccessor(AccessorKind::Set);
  auto objcAttr = setter ? setter->getAttrs().getAttribute<ObjCAttr>()
                         : nullptr;
  if (auto name = getNameFromObjcAttribute(objcAttr, DeclName(preferredName))) {
    return *name;
  }

  // Subscripts use a specific selector.
  auto &ctx = getASTContext();
  if (auto *SD = dyn_cast<SubscriptDecl>(this)) {
    switch (SD->getObjCSubscriptKind()) {
    case ObjCSubscriptKind::Indexed:
      return ObjCSelector(ctx, 2,
                          { ctx.Id_setObject, ctx.Id_atIndexedSubscript });
    case ObjCSubscriptKind::Keyed:
      return ObjCSelector(ctx, 2,
                          { ctx.Id_setObject, ctx.Id_forKeyedSubscript });
    }
  }
  

  // The setter selector for, e.g., 'fooBar' is 'setFooBar:', with the
  // property name capitalized and preceded by 'set'.
  auto var = cast<VarDecl>(this);
  Identifier Name = var->getObjCPropertyName();
  if (!preferredName.empty())
    Name = preferredName;
  auto result = VarDecl::getDefaultObjCSetterSelector(ctx, Name);

  // Cache the result, so we don't perform string manipulation again.
  if (objcAttr && preferredName.empty())
    const_cast<ObjCAttr *>(objcAttr)->setName(result, /*implicit=*/true);

  return result;
}

SourceLoc AbstractStorageDecl::getOverrideLoc() const {
  if (auto *Override = getAttrs().getAttribute<OverrideAttr>())
    return Override->getLocation();
  return SourceLoc();
}

Type AbstractStorageDecl::getValueInterfaceType() const {
  if (auto var = dyn_cast<VarDecl>(this))
    return var->getInterfaceType()->getReferenceStorageReferent();
  return cast<SubscriptDecl>(this)->getElementInterfaceType();
}

bool AbstractStorageDecl::hasInitAccessor() const {
  return evaluateOrDefault(
      getASTContext().evaluator,
      HasInitAccessorRequest{const_cast<AbstractStorageDecl *>(this)}, false);
}

VarDecl::VarDecl(DeclKind kind, bool isStatic, VarDecl::Introducer introducer,
                 SourceLoc nameLoc, Identifier name,
                 DeclContext *dc, StorageIsMutable_t supportsMutation)
  : AbstractStorageDecl(kind, isStatic, dc, name, nameLoc, supportsMutation)
{
  Bits.VarDecl.Introducer = unsigned(introducer);
  Bits.VarDecl.IsSelfParamCapture = false;
  Bits.VarDecl.IsDebuggerVar = false;
  Bits.VarDecl.IsLazyStorageProperty = false;
  Bits.VarDecl.IsPropertyWrapperBackingProperty = false;
  Bits.VarDecl.IsTopLevelGlobal = false;
  Bits.VarDecl.NoAttachedPropertyWrappers = false;
  Bits.VarDecl.NoPropertyWrapperAuxiliaryVariables = false;
}

Type VarDecl::getTypeInContext() const {
  return getDeclContext()->mapTypeIntoContext(getInterfaceType());
}

/// Translate an "is mutable" bit into a StorageMutability value.
static StorageMutability storageIsMutable(bool isMutable) {
  return isMutable ? StorageMutability::Mutable
                   : StorageMutability::Immutable;
}

/// Returns whether the var is settable in the specified context: this
/// is either because it is a stored var, because it has a custom setter, or
/// is a let member in an initializer.
StorageMutability
VarDecl::mutability(const DeclContext *UseDC,
                    std::optional<const DeclRefExpr *> base) const {
  // Parameters are settable or not depending on their ownership convention.
  if (auto *PD = dyn_cast<ParamDecl>(this))
    return storageIsMutable(!PD->isImmutableInFunctionBody());

  // If this is a 'var' decl, then we're settable if we have storage or a
  // setter.
  if (!isLet()) {
    if (hasInitAccessor()) {
      if (auto *ctor = dyn_cast_or_null<ConstructorDecl>(UseDC)) {
        // If we're referencing 'self.', it's mutable.
        if (!base ||
            (*base && ctor->getImplicitSelfDecl() == (*base)->getDecl()))
          return StorageMutability::Mutable;

        return storageIsMutable(supportsMutation());
      }
    }

    return storageIsMutable(supportsMutation());
  }

  // Static 'let's are always immutable.
  if (isStatic()) {
    return StorageMutability::Immutable;
  }

  //
  // All the remaining logic handles the special cases where you can
  // assign a 'let'.
  //

  // Debugger expression 'let's are initialized through a side-channel.
  if (isDebuggerVar())
    return StorageMutability::Immutable;

  // 'let's are only ever settable from a specific DeclContext.
  if (UseDC == nullptr)
    return StorageMutability::Immutable;

  // 'let' properties in structs/classes are only ever settable in their
  // designated initializer(s) or by init accessors.
  if (isInstanceMember()) {
    // Init accessors allow assignments to `let` properties if a
    // property is part of `initializes` list.
    if (auto *accessor =
            dyn_cast<AccessorDecl>(const_cast<DeclContext *>(UseDC))) {
      // Check whether this property is part of `initializes` list,
      // and allow assignment/mutation if so. DI would be responsible
      // for checking for re-assignment.
      if (accessor->isInitAccessor() &&
             llvm::is_contained(accessor->getInitializedProperties(),
                                const_cast<VarDecl *>(this)))
        return StorageMutability::Initializable;

      return StorageMutability::Immutable;
    }

    auto *CD = dyn_cast<ConstructorDecl>(UseDC);
    if (!CD) return StorageMutability::Immutable;

    auto *CDC = CD->getDeclContext();

    // 'let' properties are not valid inside protocols.
    if (CDC->getExtendedProtocolDecl())
      return StorageMutability::Immutable;

    // If this init is defined inside of the same type (or in an extension
    // thereof) as the let property, then it is mutable.
    if (CDC->getSelfNominalTypeDecl() !=
        getDeclContext()->getSelfNominalTypeDecl())
      return StorageMutability::Immutable;

    // If this is a convenience initializer (i.e. one that calls
    // self.init), then let properties are never mutable in it.  They are
    // only mutable in designated initializers.
    auto initKindAndExpr = CD->getDelegatingOrChainedInitKind();
    if (initKindAndExpr.initKind == BodyInitKind::Delegating)
      return StorageMutability::Immutable;

    // If we were given a base and it is 'self', it's initializable.
    if (!base || (*base && CD->getImplicitSelfDecl() == (*base)->getDecl())) {
      // Treat values of tuple type as mutable in these contexts, because
      // SILGen wants to see them as lvalues.
      if (getInterfaceType()->is<TupleType>())
        return StorageMutability::Mutable;

      return StorageMutability::Initializable;
    }

    return StorageMutability::Immutable;
  }

  // If the 'let' has a value bound to it but has no PBD, then it is
  // already initializedand not settable.
  if (getParentPatternBinding() == nullptr)
    return StorageMutability::Immutable;

  // If the 'let' has an explicitly written initializer with a pattern binding,
  // then it isn't settable.
  if (isParentInitialized())
    return StorageMutability::Immutable;

  // Normal lets (e.g. globals) are only mutable in the context of the
  // declaration.  To handle top-level code properly, we look through
  // the TopLevelCode decl on the use (if present) since the vardecl may be
  // one level up.
  if (getDeclContext() == UseDC) {
    // Treat values of tuple type as mutable in these contexts, because
    // SILGen wants to see them as lvalues.
    if (getInterfaceType()->is<TupleType>())
      return StorageMutability::Mutable;

    return StorageMutability::Initializable;
  }

  if (isa<TopLevelCodeDecl>(UseDC) &&
      getDeclContext() == UseDC->getParent())
    return StorageMutability::Initializable;

  return StorageMutability::Immutable;
}

bool VarDecl::isLazilyInitializedGlobal() const {
  assert(!getDeclContext()->isLocalContext() &&
         "not a global variable!");
  assert(hasStorage() && "not a stored global variable!");

  // Imports from C are never lazily initialized.
  if (hasClangNode())
    return false;

  if (isDebuggerVar())
    return false;

  if (getAttrs().hasAttribute<SILGenNameAttr>())
    return false;

  // Top-level global variables in the main source file and in the REPL are not
  // lazily initialized.
  return !isTopLevelGlobal();
}

Expr *VarDecl::getParentExecutableInitializer() const {
  if (auto *PBD = getParentPatternBinding()) {
    const auto i = PBD->getPatternEntryIndexForVarDecl(this);
    return PBD->getExecutableInit(i);
  }

  return nullptr;
}

ActorIsolation VarDecl::getInitializerIsolation() const {
  auto *mutableThis = const_cast<VarDecl *>(this);
  return evaluateOrDefault(
      getASTContext().evaluator,
      DefaultInitializerIsolation{mutableThis},
      ActorIsolation::forUnspecified());
}

SourceRange VarDecl::getSourceRange() const {
  if (auto Param = dyn_cast<ParamDecl>(this))
    return Param->getSourceRange();
  return getNameLoc();
}

SourceRange VarDecl::getTypeSourceRangeForDiagnostics() const {
  // For a parameter, map back to its parameter to get the TypeLoc.
  if (auto *PD = dyn_cast<ParamDecl>(this)) {
    if (auto typeRepr = PD->getTypeRepr())
      return typeRepr->getSourceRange();
  }
  
  Pattern *Pat = getParentPattern();
  if (!Pat || Pat->isImplicit())
    return SourceRange();

  if (auto *VP = dyn_cast<BindingPattern>(Pat))
    Pat = VP->getSubPattern();
  if (auto *TP = dyn_cast<TypedPattern>(Pat))
    if (auto typeRepr = TP->getTypeRepr())
      return typeRepr->getSourceRange();

  return SourceRange();
}

static std::optional<std::pair<CaseStmt *, Pattern *>>
findParentPatternCaseStmtAndPattern(const VarDecl *inputVD) {
  auto getMatchingPattern = [&](CaseStmt *cs) -> Pattern * {
    // Check if inputVD is in our case body var decls if we have any. If we do,
    // treat its pattern as our first case label item pattern.
    for (auto *vd : cs->getCaseBodyVariablesOrEmptyArray()) {
      if (vd == inputVD) {
        return cs->getMutableCaseLabelItems().front().getPattern();
      }
    }

    // Then check the rest of our case label items.
    for (auto &item : cs->getMutableCaseLabelItems()) {
      if (item.getPattern()->containsVarDecl(inputVD)) {
        return item.getPattern();
      }
    }

    // Otherwise return false if we do not find anything.
    return nullptr;
  };

  // First find our canonical var decl. This is the VarDecl corresponding to the
  // first case label item of the first case block in the fallthrough chain that
  // our case block is within. Grab the case stmt associated with that var decl
  // and start traveling down the fallthrough chain looking for the case
  // statement that the input VD belongs to by using getMatchingPattern().
  auto *canonicalVD = inputVD->getCanonicalVarDecl();
  auto *caseStmt =
      dyn_cast_or_null<CaseStmt>(canonicalVD->getParentPatternStmt());
  if (!caseStmt)
    return std::nullopt;

  if (auto *p = getMatchingPattern(caseStmt))
    return std::make_pair(caseStmt, p);

  while ((caseStmt = caseStmt->getFallthroughDest().getPtrOrNull())) {
    if (auto *p = getMatchingPattern(caseStmt))
      return std::make_pair(caseStmt, p);
  }

  return std::nullopt;
}

VarDecl *VarDecl::getCanonicalVarDecl() const {
  // Any var decl without a parent var decl is canonical. This means that before
  // type checking, all var decls are canonical.
  auto *cur = const_cast<VarDecl *>(this);
  auto *vd = cur->getParentVarDecl();
  if (!vd)
    return cur;

#ifndef NDEBUG
  // Make sure that we don't get into an infinite loop.
  SmallPtrSet<VarDecl *, 8> visitedDecls;
  visitedDecls.insert(vd);
  visitedDecls.insert(cur);
#endif
  while (vd) {
    cur = vd;
    vd = vd->getParentVarDecl();
    assert((!vd || visitedDecls.insert(vd).second) && "Infinite loop ?!");
  }

  return cur;
}

Stmt *VarDecl::getRecursiveParentPatternStmt() const {
  // If our parent is already a pattern stmt, just return that.
  if (auto *stmt = getParentPatternStmt())
    return stmt;

  // Otherwise, see if we have a parent var decl. If we do not, then return
  // nullptr. Otherwise, return the case stmt that we found.
  auto result = findParentPatternCaseStmtAndPattern(this);
  if (!result.has_value())
    return nullptr;
  return result->first;
}

/// Return the Pattern involved in initializing this VarDecl.  Recall that the
/// Pattern may be involved in initializing more than just this one vardecl
/// though.  For example, if this is a VarDecl for "x", the pattern may be
/// "(x, y)" and the initializer on the PatternBindingDecl may be "(1,2)" or
/// "foo()".
///
/// If this has no parent pattern binding decl or statement associated, it
/// returns null.
///
Pattern *VarDecl::getParentPattern() const {
  // If this has a PatternBindingDecl parent, use its pattern.
  if (auto *PBD = getParentPatternBinding()) {
    const auto i = PBD->getPatternEntryIndexForVarDecl(this);
    return PBD->getPattern(i);
  }

  // If this is a statement parent, dig the pattern out of it.
  if (auto *stmt = getParentPatternStmt()) {
    if (auto *FES = dyn_cast<ForEachStmt>(stmt))
      return FES->getPattern();

    if (auto *cs = dyn_cast<CaseStmt>(stmt)) {
      // In a case statement, search for the pattern that contains it.  This is
      // a bit silly, because you can't have something like "case x, y:" anyway.
      for (auto items : cs->getCaseLabelItems()) {
        if (items.getPattern()->containsVarDecl(this))
          return items.getPattern();
      }
    }

    if (auto *LCS = dyn_cast<LabeledConditionalStmt>(stmt)) {
      for (auto &elt : LCS->getCond())
        if (auto pat = elt.getPatternOrNull())
          if (pat->containsVarDecl(this))
            return pat;
    }
  }

  // Otherwise, check if we have to walk our case stmt's var decl list to find
  // the pattern.
  if (auto caseStmtPatternPair = findParentPatternCaseStmtAndPattern(this)) {
    return caseStmtPatternPair->second;
  }

  // Otherwise, this is a case we do not know or understand. Return nullptr to
  // signal we do not have any information.
  return nullptr;
}

NamedPattern *VarDecl::getNamingPattern() const {
  return evaluateOrDefault(getASTContext().evaluator,
                           NamingPatternRequest{const_cast<VarDecl *>(this)},
                           nullptr);
}

void VarDecl::setNamingPattern(NamedPattern *Pat) {
  getASTContext().evaluator.cacheOutput(NamingPatternRequest{this},
                                        std::move(Pat));
}

TypeRepr *VarDecl::getTypeReprOrParentPatternTypeRepr() const {
  if (auto *Param = dyn_cast<ParamDecl>(this))
    return Param->getTypeRepr();

  auto *ParentPattern = getParentPattern();

  if (auto *TyPattern = dyn_cast_or_null<TypedPattern>(ParentPattern))
    return TyPattern->getTypeRepr();

  // Handle typed if/guard/while-let as a special case (e.g. `if let x: Int
  // = Optional.some(4)`), since the `TypedPattern` is not the top-level
  // pattern here - instead it is an implicit `OptionalSomePattern`
  if (auto *SomePattern =
          dyn_cast_or_null<OptionalSomePattern>(ParentPattern)) {
    if (auto *TyPattern =
            dyn_cast<TypedPattern>(SomePattern->getSubPattern())) {
      return TyPattern->getTypeRepr();
    }
  }

  return nullptr;
}

NullablePtr<VarDecl>
VarDecl::getCorrespondingFirstCaseLabelItemVarDecl() const {
  if (!hasName())
    return nullptr;

  auto *caseStmt = dyn_cast_or_null<CaseStmt>(getRecursiveParentPatternStmt());
  if (!caseStmt)
    return nullptr;

  auto *pattern = caseStmt->getCaseLabelItems().front().getPattern();
  SmallVector<VarDecl *, 8> vars;
  pattern->collectVariables(vars);
  for (auto *vd : vars) {
    if (vd->hasName() && vd->getName() == getName())
      return vd;
  }
  return nullptr;
}

bool VarDecl::isCaseBodyVariable() const {
  auto *caseStmt = dyn_cast_or_null<CaseStmt>(getRecursiveParentPatternStmt());
  if (!caseStmt)
    return false;
  return llvm::any_of(caseStmt->getCaseBodyVariablesOrEmptyArray(),
                      [&](VarDecl *vd) { return vd == this; });
}

NullablePtr<VarDecl> VarDecl::getCorrespondingCaseBodyVariable() const {
  // Only var decls associated with case statements can have child var decls.
  auto *caseStmt = dyn_cast_or_null<CaseStmt>(getRecursiveParentPatternStmt());
  if (!caseStmt)
    return nullptr;

  // If this var decl doesn't have a name, it can not have a corresponding case
  // body variable.
  if (!hasName())
    return nullptr;

  auto name = getName();

  // A var decl associated with a case stmt implies that the case stmt has body
  // var decls. So we can access the optional value here without worry.
  auto caseBodyVars = caseStmt->getCaseBodyVariables();
  auto result = llvm::find_if(caseBodyVars, [&](VarDecl *caseBodyVar) {
    return caseBodyVar->getName() == name;
  });
  return (result != caseBodyVars.end()) ? *result : nullptr;
}

bool VarDecl::isSelfParameter() const {
  if (isa<ParamDecl>(this)) {
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(getDeclContext()))
      return AFD->getImplicitSelfDecl(/*createIfNeeded=*/false) == this;
    if (auto *PBI = dyn_cast<PatternBindingInitializer>(getDeclContext()))
      return PBI->getImplicitSelfDecl() == this;
  }

  return false;
}

bool VarDecl::isActorSelf() const {
  if (!isSelfParameter() && !isSelfParamCapture())
    return false;

  auto *dc = getDeclContext();
  while (!dc->isTypeContext() && !dc->isModuleScopeContext())
    dc = dc->getParent();

  // Check if this `self` parameter belongs to an actor declaration or
  // extension.
  auto nominal = dc->getSelfNominalTypeDecl();
  return nominal && nominal->isActor();
}

/// Whether the given variable is the backing storage property for
/// a declared property that is either `lazy` or has an attached
/// property wrapper.
static bool isBackingStorageForDeclaredProperty(const VarDecl *var) {
  if (var->isLazyStorageProperty())
    return true;

  if (var->getOriginalWrappedProperty())
    return true;

  return false;
}

/// Whether the given variable is a declared property that has separate backing storage.
static bool isDeclaredPropertyWithBackingStorage(const VarDecl *var) {
  if (var->getAttrs().hasAttribute<LazyAttr>())
    return true;

  if (var->hasAttachedPropertyWrapper())
    return true;

  return false;
}

bool VarDecl::isMemberwiseInitialized(bool preferDeclaredProperties) const {
  // Only non-static properties in type context can be part of a memberwise
  // initializer.
  if (!getDeclContext()->isTypeContext() || isStatic())
    return false;

  // If this is a stored property, and not a backing property in a case where
  // we only want to see the declared properties, it can be memberwise
  // initialized.
  if (hasStorage() && preferDeclaredProperties &&
      isBackingStorageForDeclaredProperty(this))
    return false;

  // If this is a computed property with `init` accessor, it's
  // memberwise initializable when it could be used to initialize
  // other stored properties.
  if (hasInitAccessor()) {
    if (auto *init = getAccessor(AccessorKind::Init))
      return true;
  }

  // If this is a computed property, it's not memberwise initialized unless
  // the caller has asked for the declared properties and it is either a
  // `lazy` property or a property with an attached wrapper.
  if (!hasStorage() &&
      !(preferDeclaredProperties &&
        isDeclaredPropertyWithBackingStorage(this)))
    return false;

  // Initialized 'let' properties have storage, but don't get an argument
  // to the memberwise initializer since they already have an initial
  // value that cannot be overridden.
  if (isLet() && isParentInitialized())
    return false;

  // Properties with attached wrappers that have an access level < internal
  // but do have an initializer don't participate in the memberwise
  // initializer, because they would arbitrarily lower the access of the
  // memberwise initializer.
  auto origVar = this;
  if (auto origWrapped = getOriginalWrappedProperty())
    origVar = origWrapped;
  if (origVar->getFormalAccess() < AccessLevel::Internal &&
      origVar->hasAttachedPropertyWrapper() &&
      (origVar->isParentInitialized() ||
       (origVar->getParentPatternBinding() &&
        origVar->getParentPatternBinding()->isDefaultInitializable())))
    return false;

  return true;
}

bool VarDecl::isLet() const {
  // An awful hack that stabilizes the value of 'isLet' for ParamDecl instances.
  //
  // All of the callers in SIL are actually looking for the semantic
  // "is immutable" predicate (present on ParamDecl) and should be migrated to
  // a high-level request. Once this is done, all callers of the introducer and
  // specifier setters can be removed.
  if (auto *PD = dyn_cast<ParamDecl>(this)) {
    return PD->isImmutableInFunctionBody();
  }
  return getIntroducer() == Introducer::Let
    || getIntroducer() == Introducer::Borrowing;
}

bool VarDecl::isAsyncLet() const {
  return getAttrs().hasAttribute<AsyncAttr>();
}

bool VarDecl::isKnownToBeLocal() const {
  return getAttrs().hasAttribute<KnownToBeLocalAttr>();
}

bool VarDecl::isOrdinaryStoredProperty() const {
  // we assume if it hasAttachedPropertyWrapper, it has no storage.
  //
  // also, we don't expect someone to call this on a local property, so for
  // efficiency we don't check if it's not async-let. feel free to promote
  // the assert into a full-fledged part of the condition if needed.
  assert(!isAsyncLet());
  return hasStorage() && !hasObservers();
}

VarDecl *VarDecl::createImplicitStringInterpolationVar(DeclContext *DC) {
  // Make the variable which will contain our temporary value.
  ASTContext &C = DC->getASTContext();
  auto var =
      new (C) VarDecl(/*IsStatic=*/false, VarDecl::Introducer::Var,
                      /*NameLoc=*/SourceLoc(), C.Id_dollarInterpolation, DC);
  var->setImplicit(true);
  var->setUserAccessible(false);
  return var;
}

void ParamDecl::setSpecifier(Specifier specifier) {
  VarDecl::Introducer introducer;
  switch (specifier) {
  // Unannotated or `borrowing` parameters are locally immutable.
  // So are parameters using the legacy `__shared` or  `__owned` modifiers.
  case ParamSpecifier::Default:
  case ParamSpecifier::Borrowing:
  case ParamSpecifier::LegacyShared:
  case ParamSpecifier::LegacyOwned:
    introducer = VarDecl::Introducer::Let;
    break;
  
  // `inout` and `consuming` parameters are locally mutable.
  case ParamSpecifier::InOut:
  case ParamSpecifier::Consuming:
  case ParamSpecifier::ImplicitlyCopyableConsuming:
    introducer = VarDecl::Introducer::Var;
    break;
  }
  
  setIntroducer(introducer);
  Bits.ParamDecl.OwnershipSpecifier = static_cast<unsigned>(specifier) + 1;
  assert(getCachedSpecifier() == specifier
         && "not enough bits in ParamDecl flags for specifier anymore!");
}

bool ParamDecl::isAnonClosureParam() const {
  auto name = getName();
  if (name.empty())
    return false;

  auto nameStr = name.str();
  if (nameStr.empty())
    return false;

  return nameStr[0] == '$';
}

bool ParamDecl::isVariadic() const {
  (void) getInterfaceType();
  return getOptions().contains(Flag::IsVariadic);
}

ParamDecl::Specifier ParamDecl::getSpecifier() const {
  auto &ctx = getASTContext();

  auto mutableThis = const_cast<ParamDecl *>(this);
  return evaluateOrDefault(ctx.evaluator,
                           ParamSpecifierRequest{mutableThis},
                           ParamDecl::Specifier::Default);
}

LifetimeAnnotation ParamDecl::getLifetimeAnnotation() const {
  auto specifier = getSpecifier();
  // Copyable parameters which are consumed have eager-move semantics.
  if (specifier == ParamDecl::Specifier::Consuming &&
      !getTypeInContext()->isNoncopyable()) {
    if (getAttrs().hasAttribute<NoEagerMoveAttr>())
      return LifetimeAnnotation::Lexical;
    return LifetimeAnnotation::EagerMove;
  }
  return getLifetimeAnnotationFromAttributes();
}

StringRef ParamDecl::getSpecifierSpelling(ParamSpecifier specifier) {
  switch (specifier) {
  case ParamSpecifier::Default:
    return "";
  case ParamSpecifier::Borrowing:
    return "borrowing";
  case ParamSpecifier::Consuming:
    return "consuming";
  case ParamSpecifier::InOut:
    return "inout";
  case ParamSpecifier::LegacyShared:
    return "__shared";
  case ParamSpecifier::LegacyOwned:
    return "__owned";
  case ParamSpecifier::ImplicitlyCopyableConsuming:
    return "implicitly_copyable_consuming";
  }
  llvm_unreachable("invalid ParamSpecifier");
}

StaticSpellingKind AbstractStorageDecl::getCorrectStaticSpelling() const {
  if (!isStatic())
    return StaticSpellingKind::None;
  if (auto *VD = dyn_cast<VarDecl>(this)) {
    if (auto *PBD = VD->getParentPatternBinding()) {
      if (PBD->getStaticSpelling() != StaticSpellingKind::None)
        return PBD->getStaticSpelling();
    }
  } else if (auto *SD = dyn_cast<SubscriptDecl>(this)) {
    return SD->getStaticSpelling();
  }

  return getCorrectStaticSpellingForDecl(this);
}

llvm::TinyPtrVector<CustomAttr *> VarDecl::getAttachedPropertyWrappers() const {
  auto mutableThis = const_cast<VarDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           AttachedPropertyWrappersRequest{mutableThis},
                           { });
}

/// Whether this property has any attached property wrappers.
bool VarDecl::hasAttachedPropertyWrapper() const {
  if (!getAttachedPropertyWrappers().empty())
    return true;

  if (hasImplicitPropertyWrapper())
    return true;

  return false;
}

bool VarDecl::hasImplicitPropertyWrapper() const {
  if (getAttrs().hasAttribute<CustomAttr>()) {
    if (!getAttachedPropertyWrappers().empty())
      return false;
  }

  if (isImplicit())
    return false;

  if (!isa<ParamDecl>(this))
    return false;

  if (!isa<AbstractClosureExpr>(getDeclContext()))
    return false;

  return getName().hasDollarPrefix();
}

bool VarDecl::hasExternalPropertyWrapper() const {
  if (!hasAttachedPropertyWrapper() || !isa<ParamDecl>(this))
    return false;

  // This decision needs to be made before closures are type checked (and
  // the wrapper types are potentially inferred) so closure parameters with
  // property wrappers are always "external". This is fine, because the
  // type checker will always inject a thunk with the wrapped or projected type
  // around the closure, so the wrapper will never affect the caller's
  // arguments directly anyway.
  if (isa<AbstractClosureExpr>(getDeclContext()))
    return true;

  // Wrappers with attribute arguments are always implementation-detail.
  if (getOutermostAttachedPropertyWrapper()->hasArgs())
    return false;

  auto wrapperInfo = getAttachedPropertyWrapperTypeInfo(0);
  return wrapperInfo.projectedValueVar && wrapperInfo.hasProjectedValueInit;
}

/// Whether all of the attached property wrappers have an init(wrappedValue:)
/// initializer.
bool VarDecl::allAttachedPropertyWrappersHaveWrappedValueInit() const {
  for (unsigned i : indices(getAttachedPropertyWrappers())) {
    if (!getAttachedPropertyWrapperTypeInfo(i).wrappedValueInit)
      return false;
  }
  
  return true;
}

PropertyWrapperTypeInfo
VarDecl::getAttachedPropertyWrapperTypeInfo(unsigned i) const {
  NominalTypeDecl *nominal;
  if (hasImplicitPropertyWrapper()) {
    assert(i == 0);
    nominal = getInterfaceType()->getAnyNominal();
  } else {
    auto attrs = getAttachedPropertyWrappers();
    if (i >= attrs.size())
      return PropertyWrapperTypeInfo();

    auto attr = attrs[i];
    auto dc = getDeclContext();
    ASTContext &ctx = getASTContext();
    nominal = evaluateOrDefault(
        ctx.evaluator, CustomAttrNominalRequest{attr, dc}, nullptr);
  }

  if (!nominal)
    return PropertyWrapperTypeInfo();

  return nominal->getPropertyWrapperTypeInfo();
}

Type VarDecl::getAttachedPropertyWrapperType(unsigned index) const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<VarDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator,
      AttachedPropertyWrapperTypeRequest{mutableThis, index},
      Type());
}

Type VarDecl::getPropertyWrapperBackingPropertyType() const {
  ASTContext &ctx = getASTContext();
  auto mutableThis = const_cast<VarDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator, PropertyWrapperBackingPropertyTypeRequest{mutableThis},
      Type());
}

PropertyWrapperAuxiliaryVariables
VarDecl::getPropertyWrapperAuxiliaryVariables() const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<VarDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator,
      PropertyWrapperAuxiliaryVariablesRequest{mutableThis},
      PropertyWrapperAuxiliaryVariables());
}

PropertyWrapperInitializerInfo
VarDecl::getPropertyWrapperInitializerInfo() const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<VarDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator,
      PropertyWrapperInitializerInfoRequest{mutableThis},
      PropertyWrapperInitializerInfo());
}

std::optional<PropertyWrapperMutability>
VarDecl::getPropertyWrapperMutability() const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<VarDecl *>(this);
  return evaluateOrDefault(ctx.evaluator,
                           PropertyWrapperMutabilityRequest{mutableThis},
                           std::nullopt);
}

std::optional<PropertyWrapperSynthesizedPropertyKind>
VarDecl::getPropertyWrapperSynthesizedPropertyKind() const {
  if (getOriginalWrappedProperty(
          PropertyWrapperSynthesizedPropertyKind::Backing))
    return PropertyWrapperSynthesizedPropertyKind::Backing;
  if (getOriginalWrappedProperty(
          PropertyWrapperSynthesizedPropertyKind::Projection))
    return PropertyWrapperSynthesizedPropertyKind::Projection;
  return std::nullopt;
}

VarDecl *VarDecl::getPropertyWrapperBackingProperty() const {
  return getPropertyWrapperAuxiliaryVariables().backingVar;
}

VarDecl *VarDecl::getPropertyWrapperProjectionVar() const {
  return getPropertyWrapperAuxiliaryVariables().projectionVar;
}

VarDecl *VarDecl::getPropertyWrapperWrappedValueVar() const {
  return getPropertyWrapperAuxiliaryVariables().localWrappedValueVar;
}

bool VarDecl::hasStorageOrWrapsStorage() const {
  if (hasStorage())
    return true;
  
  if (getAttrs().hasAttribute<LazyAttr>())
    return true;
  
  auto *backing = getPropertyWrapperBackingProperty();
  if (backing && backing->hasStorage())
    return true;
  
  return false;
}

void VarDecl::visitAuxiliaryDecls(llvm::function_ref<void(VarDecl *)> visit) const {
  if (getDeclContext()->isTypeContext() ||
      (isImplicit() && !isa<ParamDecl>(this)))
    return;

  if (getAttrs().hasAttribute<LazyAttr>()) {
    if (auto *backingVar = getLazyStorageProperty())
      visit(backingVar);
  }

  if (getAttrs().hasAttribute<CustomAttr>() || hasImplicitPropertyWrapper()) {
    if (auto *backingVar = getPropertyWrapperBackingProperty())
      visit(backingVar);

    if (auto *projectionVar = getPropertyWrapperProjectionVar())
      visit(projectionVar);

    if (auto *wrappedValueVar = getPropertyWrapperWrappedValueVar())
      visit(wrappedValueVar);
  }
}

VarDecl *VarDecl::getLazyStorageProperty() const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<VarDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator,
      LazyStoragePropertyRequest{mutableThis},
      {});
}

bool VarDecl::isGlobalStorage() const {
  if (!hasStorage()) {
    return false;
  }
  const auto *dc = getDeclContext();
  return isStatic() || dc->isModuleScopeContext() ||
         (dc->isTypeContext() && !isInstanceMember());
}

bool VarDecl::isPropertyMemberwiseInitializedWithWrappedType() const {
  auto customAttrs = getAttachedPropertyWrappers();
  if (customAttrs.empty())
    return false;

  auto *PBD = getParentPatternBinding();
  if (!PBD)
    return false;

  // If there was an initializer on the original property, initialize
  // via the initial value.
  if (PBD->getEqualLoc(0).isValid())
    return true;

  // If there was an initializer on the outermost wrapper, initialize
  // via the full wrapper.
  if (customAttrs[0]->hasArgs())
    return false;

  // Default initialization does not use a value.
  if (getAttachedPropertyWrapperTypeInfo(0).defaultInit)
    return false;

  // If all property wrappers have a wrappedValue initializer, the property
  // wrapper will be initialized that way.
  return allAttachedPropertyWrappersHaveWrappedValueInit();
}

Type VarDecl::getPropertyWrapperInitValueInterfaceType() const {
  auto initInfo = getPropertyWrapperInitializerInfo();
  if (!initInfo.getWrappedValuePlaceholder())
    return Type();

  Type valueInterfaceTy = initInfo.getWrappedValuePlaceholder()->getType();
  if (valueInterfaceTy->hasPrimaryArchetype())
    valueInterfaceTy = valueInterfaceTy->mapTypeOutOfContext();

  return valueInterfaceTy;
}

Identifier VarDecl::getObjCPropertyName() const {
  if (auto attr = getAttrs().getAttribute<ObjCAttr>()) {
    if (auto name = attr->getName())
      return name->getSelectorPieces()[0];
  }

  return getName();
}

ObjCSelector VarDecl::getDefaultObjCGetterSelector(ASTContext &ctx,
                                                   Identifier propertyName) {
  return ObjCSelector(ctx, 0, propertyName);
}


ObjCSelector VarDecl::getDefaultObjCSetterSelector(ASTContext &ctx,
                                                   Identifier propertyName) {
  llvm::SmallString<16> scratch;
  scratch += "set";
  camel_case::appendSentenceCase(scratch, propertyName.str());

  return ObjCSelector(ctx, 1, ctx.getIdentifier(scratch));
}

/// If this is a simple 'let' constant, emit a note with a fixit indicating
/// that it can be rewritten to a 'var'.  This is used in situations where the
/// compiler detects obvious attempts to mutate a constant.
void VarDecl::emitLetToVarNoteIfSimple(DeclContext *UseDC) const {
  // If it isn't a 'let', don't touch it.
  if (!isLet()) return;

  // If this is the 'self' argument of a non-mutating method in a value type,
  // suggest adding 'mutating' to the method.
  if (isSelfParameter() && UseDC) {
    // If the problematic decl is 'self', then we might be trying to mutate
    // a property in a non-mutating method.
    auto FD = dyn_cast_or_null<FuncDecl>(UseDC->getInnermostMethodContext());

    if (FD && !FD->isMutating() && !FD->isImplicit() && FD->isInstanceMember()&&
        !FD->getDeclContext()->getDeclaredInterfaceType()
                 ->hasReferenceSemantics()) {
      // Do not suggest the fix-it in implicit getters
      if (auto AD = dyn_cast<AccessorDecl>(FD)) {
        if (AD->isImplicitGetter())
          return;
      }

      auto &d = getASTContext().Diags;
      auto descriptiveKindName = Decl::getDescriptiveKindName(FD->getDescriptiveKind());
      auto diags = d.diagnose(FD->getFuncLoc(), diag::change_to_mutating,
                              isa<AccessorDecl>(FD), descriptiveKindName);
      if (auto nonmutatingAttr =
              FD->getAttrs().getAttribute<NonMutatingAttr>()) {
        diags.fixItReplace(nonmutatingAttr->getLocation(), "mutating");
      } else {
        diags.fixItInsert(FD->getFuncLoc(), "mutating ");
      }
      return;
    }
  }

  // Besides self, don't suggest mutability for explicit function parameters.
  if (isa<ParamDecl>(this)) return;

  // Don't suggest any fixes for capture list elements.
  if (isCaptureList()) return;

  // If this is a normal variable definition, then we can change 'let' to 'var'.
  // We even are willing to suggest this for multi-variable binding, like
  //   "let (a,b) = "
  // since the user has to choose to apply this anyway.
  if (auto *PBD = getParentPatternBinding()) {
    // Don't touch generated or invalid code.
    if (PBD->getLoc().isInvalid() || PBD->isImplicit())
      return;

    auto &d = getASTContext().Diags;
    d.diagnose(PBD->getLoc(), diag::convert_let_to_var)
     .fixItReplace(PBD->getLoc(), "var");
    return;
  }
}

clang::PointerAuthQualifier VarDecl::getPointerAuthQualifier() const {
  if (auto *clangDecl = getClangDecl()) {
    if (auto *valueDecl = dyn_cast<clang::ValueDecl>(clangDecl)) {
      return valueDecl->getType().getPointerAuth();
    }
  }
  return clang::PointerAuthQualifier();
}

ParamDecl::ParamDecl(SourceLoc specifierLoc, SourceLoc argumentNameLoc,
                     Identifier argumentName, SourceLoc parameterNameLoc,
                     Identifier parameterName, DeclContext *dc)
    : VarDecl(DeclKind::Param,
              /*IsStatic*/ false, VarDecl::Introducer::Let, parameterNameLoc,
              parameterName, dc, StorageIsNotMutable),
      ArgumentNameAndFlags(argumentName, std::nullopt),
      ParameterNameLoc(parameterNameLoc), ArgumentNameLoc(argumentNameLoc),
      SpecifierLoc(specifierLoc) {
  Bits.ParamDecl.OwnershipSpecifier = 0;
  Bits.ParamDecl.defaultArgumentKind =
    static_cast<unsigned>(DefaultArgumentKind::None);
}

ParamDecl *
ParamDecl::cloneWithoutType(const ASTContext &Ctx, ParamDecl *PD,
                            std::optional<DefaultArgumentKind> defaultArgKind) {
  auto *Clone = new (Ctx) ParamDecl(
      SourceLoc(), SourceLoc(), PD->getArgumentName(),
      SourceLoc(), PD->getParameterName(), PD->getDeclContext());
  Clone->setOptionsAndPointers(nullptr, nullptr, PD->getOptions());

  if (defaultArgKind) {
    Clone->setDefaultArgumentKind(*defaultArgKind);
  } else {
    Clone->setDefaultArgumentKind(PD->getDefaultArgumentKind());
  }

  Clone->setSpecifier(PD->getSpecifier());
  Clone->setImplicitlyUnwrappedOptional(PD->isImplicitlyUnwrappedOptional());
  if (PD->isImplicit()) {
    Clone->setImplicit();
  }
  return Clone;
}

ParamDecl *ParamDecl::clone(const ASTContext &Ctx, ParamDecl *PD) {
  auto *Clone = ParamDecl::cloneWithoutType(Ctx, PD);
  Clone->setInterfaceType(PD->getInterfaceType());
  return Clone;
}

ParamDecl *
ParamDecl::createImplicit(ASTContext &Context, SourceLoc specifierLoc,
                          SourceLoc argumentNameLoc, Identifier argumentName,
                          SourceLoc parameterNameLoc, Identifier parameterName,
                          Type interfaceType, DeclContext *Parent,
                          ParamSpecifier specifier) {
  auto decl =
      new (Context) ParamDecl(specifierLoc, argumentNameLoc, argumentName,
                              parameterNameLoc, parameterName, Parent);
  decl->setImplicit();
  // implicit ParamDecls must have a specifier set
  decl->setSpecifier(specifier);
  decl->setInterfaceType(interfaceType);
  return decl;
}

ParamDecl *ParamDecl::createImplicit(ASTContext &Context,
                                     Identifier argumentName,
                                     Identifier parameterName,
                                     Type interfaceType, DeclContext *Parent,
                                     ParamSpecifier specifier) {
  return ParamDecl::createImplicit(Context, SourceLoc(), SourceLoc(),
                                   argumentName, SourceLoc(), parameterName,
                                   interfaceType, Parent, specifier);
}

/// Determine the kind of a default argument for the given expression.
static DefaultArgumentKind computeDefaultArgumentKind(DeclContext *dc,
                                                      Expr *init) {
  if (!init)
    return DefaultArgumentKind::None;

  // Parse an as-written 'nil' expression as the special NilLiteral kind,
  // which is emitted by the caller and can participate in rethrows
  // checking.
  if (isa<NilLiteralExpr>(init))
    return DefaultArgumentKind::NilLiteral;

  if (isa<MacroExpansionExpr>(init))
    return DefaultArgumentKind::ExpressionMacro;

  if (isa<SuperRefExpr>(init)) {
    // The compiler does not synthesize inherited initializers when
    // type-checking Swift module interfaces. Instead, module interfaces are
    // expected to include them explicitly in subclasses. A default argument of
    // '= super' in a parameter of such initializer indicates that the default
    // argument is inherited.
    if (dc->getParentSourceFile()->Kind == SourceFileKind::Interface) {
      return DefaultArgumentKind::Inherited;
    } else {
      return DefaultArgumentKind::Normal;
    }
  }

  auto magic = dyn_cast<MagicIdentifierLiteralExpr>(init);
  if (!magic)
    return DefaultArgumentKind::Normal;

  switch (magic->getKind()) {
#define MAGIC_IDENTIFIER(NAME, STRING, SYNTAX_KIND)                            \
  case MagicIdentifierLiteralExpr::NAME:                                       \
    return DefaultArgumentKind::NAME;
#include "swift/AST/MagicIdentifierKinds.def"
  }

  llvm_unreachable("Unhandled MagicIdentifierLiteralExpr in switch.");
}

ParamDecl *ParamDecl::createParsed(ASTContext &Context, SourceLoc specifierLoc,
                                   SourceLoc argumentNameLoc,
                                   Identifier argumentName,
                                   SourceLoc parameterNameLoc,
                                   Identifier parameterName, Expr *defaultValue,
                                   DeclContext *dc) {
  auto *decl =
      new (Context) ParamDecl(specifierLoc, argumentNameLoc, argumentName,
                              parameterNameLoc, parameterName, dc);

  const auto kind = computeDefaultArgumentKind(dc, defaultValue);
  if (kind == DefaultArgumentKind::Inherited) {
    // The 'super' in inherited default arguments is a specifier rather than an
    // expression.
    // TODO: However, we may want to retain its location for diagnostics.
    defaultValue = nullptr;
  }

  decl->setDefaultExpr(defaultValue);
  decl->setDefaultArgumentKind(kind);

  return decl;
}

void ParamDecl::setDefaultArgumentKind(DefaultArgumentKind K) {
  assert(getDefaultArgumentKind() == DefaultArgumentKind::None &&
         "Overwrite of default argument kind");
  Bits.ParamDecl.defaultArgumentKind = static_cast<unsigned>(K);
}

/// Retrieve the type of 'self' for the given context.
Type DeclContext::getSelfTypeInContext() const {
  return mapTypeIntoContext(getSelfInterfaceType());
}

TupleType *BuiltinTupleDecl::getTupleSelfType(const ExtensionDecl *owner) const {
  auto &ctx = getASTContext();

  // Get the generic parameter type 'each T'.
  GenericParamList *genericParams;
  if (owner != nullptr) {
    genericParams = owner->getGenericParams();
  } else {
    genericParams = getGenericParams();
  }

  assert(genericParams != nullptr);
  assert(genericParams->getParams().size() == 1);
  assert(genericParams->getOuterParameters() == nullptr);
  auto paramType = genericParams->getParams()[0]->getDeclaredInterfaceType();

  // Build the pack expansion type 'repeat each T'.
  Type packExpansionType = PackExpansionType::get(paramType, paramType);

  // Build the one-element tuple type '(repeat each T)'.
  SmallVector<TupleTypeElt, 1> elts;
  elts.push_back(packExpansionType);

  return TupleType::get(elts, ctx);
}

/// Retrieve the interface type of 'self' for the given context.
Type DeclContext::getSelfInterfaceType() const {
  assert(isTypeContext());

  if (auto *nominalDecl = getSelfNominalTypeDecl()) {
    if (auto *builtinTupleDecl = dyn_cast<BuiltinTupleDecl>(nominalDecl))
      return builtinTupleDecl->getTupleSelfType(dyn_cast<ExtensionDecl>(this));

    if (isa<ProtocolDecl>(nominalDecl)) {
      auto *genericParams = nominalDecl->getGenericParams();
      return genericParams->getParams().front()
          ->getDeclaredInterfaceType();
    }

    return getDeclaredInterfaceType();
  }

  return ErrorType::get(getASTContext());
}

/// Return the full source range of this parameter.
SourceRange ParamDecl::getSourceRange() const {
  SourceLoc APINameLoc = getArgumentNameLoc();
  SourceLoc nameLoc = getNameLoc();

  SourceLoc startLoc;
  if (APINameLoc.isValid())
    startLoc = APINameLoc;
  else if (nameLoc.isValid())
    startLoc = nameLoc;
  else if (auto *repr = getTypeRepr())
    startLoc = repr->getStartLoc();

  if (startLoc.isInvalid())
    return SourceRange();

  // It would be nice to extend the front of the range to show where inout is,
  // but we don't have that location info.  Extend the back of the range to the
  // location of the default argument, or the typeloc if they are valid.
  if (auto expr = getStructuralDefaultExpr()) {
    auto endLoc = expr->getEndLoc();
    if (endLoc.isValid())
      return SourceRange(startLoc, endLoc);
  }
  
  // If the typeloc has a valid location, use it to end the range.
  if (auto typeRepr = getTypeRepr()) {
    auto endLoc = typeRepr->getEndLoc();
    if (endLoc.isValid())
      return SourceRange(startLoc, endLoc);
  }

  // The name has a location we can use.
  if (nameLoc.isValid())
    return SourceRange(startLoc, nameLoc);

  return startLoc;
}

bool ParamDecl::isNonEphemeral() const {
  if (getAttrs().hasAttribute<NonEphemeralAttr>())
    return true;

  // Only enum element parameters are non-ephemeral without '@_nonEphemeral'.
  auto *parentDecl = getDeclContext()->getAsDecl();
  if (!parentDecl || !isa<EnumElementDecl>(parentDecl))
    return false;

  // Only pointer parameters can be non-ephemeral.
  auto ty = getInterfaceType();
  if (!ty->lookThroughSingleOptionalType()->getAnyPointerElementType())
    return false;

  return true;
}

void ParamDecl::setNonEphemeralIfPossible() {
  assert(hasInterfaceType() && "Must be pre-typechecked.");
  // Don't apply the attribute if this isn't a pointer param.
  auto type = getInterfaceType();
  if (!type->lookThroughSingleOptionalType()->getAnyPointerElementType())
    return;

  if (!getAttrs().hasAttribute<NonEphemeralAttr>()) {
    auto &ctx = getASTContext();
    getAttrs().add(new (ctx) NonEphemeralAttr(/*IsImplicit*/ true));
  }
}

Type ParamDecl::getVarargBaseTy(Type VarArgT) {
  TypeBase *T = VarArgT.getPointer();
  if (auto *AT = dyn_cast<VariadicSequenceType>(T))
    return AT->getBaseType();
  if (auto *BGT = dyn_cast<BoundGenericType>(T)) {
    // It's the stdlib Array<T>.
    return BGT->getGenericArgs()[0];
  }
  return T;
}

AnyFunctionType::Param ParamDecl::toFunctionParam(Type type) const {
  if (!type) {
    type = getInterfaceType();

    if (hasExternalPropertyWrapper()) {
      if (auto wrapper = getPropertyWrapperBackingPropertyType()) {
        type = wrapper;
      }
    }
  }

  if (isVariadic())
    type = ParamDecl::getVarargBaseTy(type);

  auto label = getArgumentName();
  auto internalLabel = getParameterName();
  auto flags = ParameterTypeFlags::fromParameterType(
      type, isVariadic(), isAutoClosure(), isNonEphemeral(), getSpecifier(),
      isIsolated(), /*isNoDerivative*/ false, isCompileTimeConst(),
      isSending());
  return AnyFunctionType::Param(type, label, flags, internalLabel);
}

std::optional<Initializer *>
ParamDecl::getCachedDefaultArgumentInitContext() const {
  if (auto *defaultInfo = DefaultValueAndFlags.getPointer())
    if (auto *init = defaultInfo->InitContextAndIsTypeChecked.getPointer())
      return init;

  return std::nullopt;
}

Initializer *ParamDecl::getDefaultArgumentInitContext() const {
  // If this param doesn't need a context, don't bother kicking off a request.
  if (!hasDefaultExpr() && !getStoredProperty())
    return nullptr;

  auto &ctx = getASTContext();
  auto *mutableThis = const_cast<ParamDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator, DefaultArgumentInitContextRequest{mutableThis}, nullptr);
}

bool ParamDecl::hasDefaultExpr() const {
  switch (getDefaultArgumentKind()) {
  case DefaultArgumentKind::None:
  case DefaultArgumentKind::Inherited:
  case DefaultArgumentKind::StoredProperty:
    return false;
  case DefaultArgumentKind::Normal:
#define MAGIC_IDENTIFIER(NAME, STRING, SYNTAX_KIND) \
  case DefaultArgumentKind::NAME:
#include "swift/AST/MagicIdentifierKinds.def"
  case DefaultArgumentKind::ExpressionMacro:
  case DefaultArgumentKind::NilLiteral:
  case DefaultArgumentKind::EmptyArray:
  case DefaultArgumentKind::EmptyDictionary:
    // Check if we have a structural default expr. This ensures we return false
    // for deserialized decls.
    return getStructuralDefaultExpr();
  }
  llvm_unreachable("Unhandled case in switch");
}

bool ParamDecl::hasCallerSideDefaultExpr() const {
  switch (getDefaultArgumentKind()) {
  case DefaultArgumentKind::None:
  case DefaultArgumentKind::Inherited:
  case DefaultArgumentKind::StoredProperty:
  case DefaultArgumentKind::Normal:
    return false;
#define MAGIC_IDENTIFIER(NAME, STRING, SYNTAX_KIND) \
  case DefaultArgumentKind::NAME:
#include "swift/AST/MagicIdentifierKinds.def"
  case DefaultArgumentKind::NilLiteral:
  case DefaultArgumentKind::EmptyArray:
  case DefaultArgumentKind::EmptyDictionary:
  case DefaultArgumentKind::ExpressionMacro:
    return true;
  }
  llvm_unreachable("invalid default argument kind");
}

Expr *ParamDecl::getTypeCheckedDefaultExpr() const {
  // Don't kick off a request if we know there's no default expr. The only
  // exception is for inherited default args which we need to perform a couple
  // of semantic checks for.
  if (!hasDefaultExpr() &&
      getDefaultArgumentKind() != DefaultArgumentKind::Inherited) {
    return nullptr;
  }

  auto &ctx = getASTContext();
  if (Expr *E = evaluateOrDefault(
          ctx.evaluator,
          DefaultArgumentExprRequest{const_cast<ParamDecl *>(this)}, nullptr)) {
    return E;
  }
  return new (ctx) ErrorExpr(getSourceRange(), ErrorType::get(ctx));
}

Type ParamDecl::getTypeOfDefaultExpr() const {
  auto &ctx = getASTContext();

  // If this is a caller-side default, the type is determined based on
  // a particular call site.
  assert(!hasCallerSideDefaultExpr());

  if (Type type = evaluateOrDefault(
          ctx.evaluator,
          DefaultArgumentTypeRequest{const_cast<ParamDecl *>(this)}, nullptr)) {
    return type;
  }

  return Type();
}

void ParamDecl::setDefaultExpr(Expr *E) {
  auto *defaultInfo = DefaultValueAndFlags.getPointer();
  if (defaultInfo) {
    assert(defaultInfo->DefaultArg.isNull() ||
           defaultInfo->DefaultArg.is<Expr *>());

    auto *const oldE = defaultInfo->DefaultArg.dyn_cast<Expr *>();
    assert((bool)E == (bool)oldE && "Overwrite of non-null default with null");
    assert((!oldE || !oldE->getType() || (bool)E->getType()) &&
           "Overwrite of type-checked default with non-type-checked default");
  } else {
    if (!E) return;

    defaultInfo = getASTContext().Allocate<StoredDefaultArgument>();
    DefaultValueAndFlags.setPointer(defaultInfo);

    defaultInfo->InitContextAndIsTypeChecked.setInt(false);
  }

  defaultInfo->DefaultArg = E;
}

void ParamDecl::setTypeCheckedDefaultExpr(Expr *E) {
  assert(E || getDefaultArgumentKind() == DefaultArgumentKind::Inherited);
  setDefaultExpr(E);

  auto *defaultInfo = DefaultValueAndFlags.getPointer();
  if (!defaultInfo) {
    defaultInfo = getASTContext().Allocate<StoredDefaultArgument>();
    DefaultValueAndFlags.setPointer(defaultInfo);
  }

  defaultInfo->InitContextAndIsTypeChecked.setInt(true);
}

void ParamDecl::setDefaultExprType(Type type) {
  if (!DefaultValueAndFlags.getPointer()) {
    // If there is no type, let's not allocate storage.
    if (!type)
      return;

    DefaultValueAndFlags.setPointer(
        getASTContext().Allocate<StoredDefaultArgument>());
  }

  auto *defaultInfo = DefaultValueAndFlags.getPointer();
  defaultInfo->ExprType = type;
}

void ParamDecl::setStoredProperty(VarDecl *var) {
  if (!DefaultValueAndFlags.getPointer()) {
    if (!var) return;

    DefaultValueAndFlags.setPointer(
      getASTContext().Allocate<StoredDefaultArgument>());
  }

  auto *defaultInfo = DefaultValueAndFlags.getPointer();
  assert(defaultInfo->DefaultArg.isNull() ||
         defaultInfo->DefaultArg.is<VarDecl *>());
  defaultInfo->DefaultArg = var;
}

Type ValueDecl::getResultBuilderType() const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<ValueDecl *>(this);
  return evaluateOrDefault(ctx.evaluator,
                           ResultBuilderTypeRequest{mutableThis},
                           Type());
}

CustomAttr *ValueDecl::getAttachedResultBuilder() const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<ValueDecl *>(this);
  return evaluateOrDefault(ctx.evaluator,
                           AttachedResultBuilderRequest{mutableThis},
                           nullptr);
}

void ParamDecl::setDefaultArgumentInitContext(Initializer *initContext) {
  auto oldContext = getCachedDefaultArgumentInitContext();
  assert((!oldContext || oldContext == initContext) &&
         "Cannot change init context after setting");

  auto *defaultInfo = DefaultValueAndFlags.getPointer();
  assert(defaultInfo);
  defaultInfo->InitContextAndIsTypeChecked.setPointer(initContext);
}

CaptureInfo ParamDecl::getDefaultArgumentCaptureInfo() const {
  if (!DefaultValueAndFlags.getPointer())
    return CaptureInfo::empty();

  auto &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
                           ParamCaptureInfoRequest{const_cast<ParamDecl *>(this)},
                           CaptureInfo::empty());
}

void ParamDecl::setDefaultArgumentCaptureInfo(CaptureInfo captures) {
  assert(DefaultValueAndFlags.getPointer());
  assert(captures.hasBeenComputed());
  DefaultValueAndFlags.getPointer()->Captures = captures;
}

PropertyWrapperValuePlaceholderExpr *
swift::findWrappedValuePlaceholder(Expr *init) {
  class Walker : public ASTWalker {
  public:
    PropertyWrapperValuePlaceholderExpr *placeholder = nullptr;

    /// Only walk the arguments of a macro, to represent the source as written.
    MacroWalking getMacroWalkingBehavior() const override {
      return MacroWalking::Arguments;
    }

    virtual PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
      if (placeholder)
        return Action::SkipNode(E);

      if (auto *value = dyn_cast<PropertyWrapperValuePlaceholderExpr>(E)) {
        placeholder = value;
        return Action::SkipNode(value);
      }

      return Action::Continue(E);
    }
  } walker;
  init->walk(walker);

  return walker.placeholder;
}

/// Writes a tuple expression where each element is either `nil` or another such
/// tuple of nils.
/// This comes up when printing default arguments for memberwise initializers
/// that were created implicitly.
/// For example, this var:
/// ```
/// var x: (Int?, (Int?, Int?, ()))
/// ```
/// will produce `(nil, (nil, nil, ()))`
static void writeTupleOfNils(TupleType *type, llvm::raw_ostream &os) {
  os << '(';
  for (unsigned i = 0; i < type->getNumElements(); ++i) {
    auto &elt = type->getElement(i);
    if (elt.hasName()) {
      os << elt.getName().str() << ": ";
    }

    if (elt.getType()->getOptionalObjectType()) {
      os << "nil";
    } else {
      writeTupleOfNils(elt.getType()->castTo<TupleType>(), os);
    }
    if (i < type->getNumElements() - 1) {
      os << ", ";
    }
  }
  os << ')';
}

/// Determines if the given type is a potentially nested tuple of optional
/// types.
static bool isTupleOfOptionals(Type type) {
  auto tuple = type->getAs<TupleType>();
  if (!tuple) return false;
  for (auto elt : tuple->getElementTypes())
    if (!elt->getOptionalObjectType() && !isTupleOfOptionals(elt))
      return false;
  return true;
}

StringRef
ParamDecl::getDefaultValueStringRepresentation(
  SmallVectorImpl<char> &scratch) const {
  switch (getDefaultArgumentKind()) {
  case DefaultArgumentKind::None:
    llvm_unreachable("called on a ParamDecl with no default value");
  case DefaultArgumentKind::ExpressionMacro:
  case DefaultArgumentKind::Normal: {
    assert(DefaultValueAndFlags.getPointer() &&
           "default value not provided yet");
    auto existing = DefaultValueAndFlags.getPointer()->StringRepresentation;
    if (!existing.empty())
      return existing;

    assert(hasDefaultExpr()
           && "Normal default argument with no default expression?!");
    return extractInlinableText(getASTContext(),
                                getStructuralDefaultExpr(), scratch);
  }
  case DefaultArgumentKind::StoredProperty: {
    assert(DefaultValueAndFlags.getPointer() &&
           "default value not provided yet");
    auto existing = DefaultValueAndFlags.getPointer()->StringRepresentation;
    if (!existing.empty())
      return existing;
    auto var = getStoredProperty();

    if (auto original = var->getOriginalWrappedProperty()) {
      auto wrapperAttrs = original->getAttachedPropertyWrappers();
      if (wrapperAttrs.size() > 0) {
        auto attr = wrapperAttrs.front();
        if (auto *args = attr->getArgs()) {
          SourceRange fullRange(attr->getTypeRepr()->getSourceRange().Start,
                                args->getEndLoc());
          auto charRange = Lexer::getCharSourceRangeFromSourceRange(
              getASTContext().SourceMgr, fullRange);
          return getASTContext().SourceMgr.extractText(charRange);
        }

        // If there is no initial wrapped value, we used the default initializer.
        Expr *wrappedValue = nullptr;
        if (auto *parentInit = original->getParentInitializer())
          if (auto *placeholder = findWrappedValuePlaceholder(parentInit))
            wrappedValue = placeholder->getOriginalWrappedValue();

        if (!wrappedValue) {
          if (auto type = original->getPropertyWrapperBackingPropertyType()) {
            if (auto nominal = type->getAnyNominal()) {
              scratch.clear();
              auto typeName = nominal->getName().str();
              scratch.append(typeName.begin(), typeName.end());
              scratch.push_back('(');
              scratch.push_back(')');
              return {scratch.data(), scratch.size()};
            }
          }

          return ".init()";
        }

        return extractInlinableText(getASTContext(), wrappedValue, scratch);
      }
    }

    auto init = var->getParentInitializer();
    if (!init || !init->getSourceRange().isValid()) {
      // Special case: There are two possible times where we will synthesize a
      //               default initial value for a stored property: if the type
      //               is Optional, or if it's a (potentially nested) tuple of
      //               all Optional elements. If it's Optional, we'll set
      //               the DefaultArgumentKind to NilLiteral, but if we're still
      //               handling a StoredProperty, then we know it's a tuple.
      if (isTupleOfOptionals(getInterfaceType())) {
        llvm::raw_svector_ostream os(scratch);
        writeTupleOfNils(getInterfaceType()->castTo<TupleType>(), os);
        return os.str();
      }
      return "<<empty>>";
    }

    return extractInlinableText(getASTContext(), init, scratch);
  }
  case DefaultArgumentKind::Inherited: return "super";
#define MAGIC_IDENTIFIER(NAME, STRING, SYNTAX_KIND) \
  case DefaultArgumentKind::NAME: return STRING;
#include "swift/AST/MagicIdentifierKinds.def"
  case DefaultArgumentKind::NilLiteral: return "nil";
  case DefaultArgumentKind::EmptyArray: return "[]";
  case DefaultArgumentKind::EmptyDictionary: return "[:]";
  }
  llvm_unreachable("unhandled kind");
}

void
ParamDecl::setDefaultValueStringRepresentation(StringRef stringRepresentation) {
  assert(getDefaultArgumentKind() == DefaultArgumentKind::Normal ||
         getDefaultArgumentKind() == DefaultArgumentKind::StoredProperty ||
         getDefaultArgumentKind() == DefaultArgumentKind::ExpressionMacro);
  assert(!stringRepresentation.empty());

  if (!DefaultValueAndFlags.getPointer()) {
    DefaultValueAndFlags.setPointer(
        getASTContext().Allocate<StoredDefaultArgument>());
  }

  DefaultValueAndFlags.getPointer()->StringRepresentation =
      stringRepresentation;
}

void DefaultArgumentInitializer::changeFunction(
    DeclContext *parent, ParameterList *paramList) {
  if (parent->isLocalContext()) {
    setParent(parent);
  }

  auto param = paramList->get(getIndex());
  if (param->hasDefaultExpr() || param->getStoredProperty())
    param->setDefaultArgumentInitContext(this);
}

/// Determine whether the given Swift type is an integral type, i.e.,
/// a type that wraps a builtin integer.
static bool isIntegralType(Type type) {
  // Consider structs in the standard library module that wrap a builtin
  // integer type to be integral types.
  if (auto structTy = type->getAs<StructType>()) {
    auto structDecl = structTy->getDecl();
    const DeclContext *DC = structDecl->getDeclContext();
    if (!DC->isModuleScopeContext() || !DC->getParentModule()->isStdlibModule())
      return false;

    // Find the single ivar.
    VarDecl *singleVar = nullptr;
    for (auto member : structDecl->getStoredProperties()) {
      if (singleVar)
        return false;
      singleVar = member;
    }

    if (!singleVar)
      return false;

    // Check whether it has integer type.
    return singleVar->getInterfaceType()->is<BuiltinIntegerType>();
  }

  return false;
}

void SubscriptDecl::setIndices(ParameterList *p) {
  Indices = p;
  
  if (Indices)
    Indices->setDeclContextOfParamDecls(this);
}

Type SubscriptDecl::getElementInterfaceType() const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<SubscriptDecl *>(this);
  if (auto type = evaluateOrDefault(ctx.evaluator,
                           ResultTypeRequest{mutableThis},
                           Type()))
    return type;
  return ErrorType::get(ctx);
}

ObjCSubscriptKind SubscriptDecl::getObjCSubscriptKind() const {
  // If the index type is an integral type, we have an indexed
  // subscript.
  if (auto funcTy = getInterfaceType()->getAs<AnyFunctionType>()) {
    auto params = funcTy->getParams();
    if (params.size() == 1)
      if (isIntegralType(params[0].getPlainType()))
        return ObjCSubscriptKind::Indexed;
  }

  // If the index type is an object type in Objective-C, we have a
  // keyed subscript.
  return ObjCSubscriptKind::Keyed;
}

void SubscriptDecl::setElementInterfaceType(Type type) {
  getASTContext().evaluator.cacheOutput(ResultTypeRequest{this},
                                        std::move(type));
}

SubscriptDecl *
SubscriptDecl::createDeserialized(ASTContext &Context, DeclName Name,
                                  StaticSpellingKind StaticSpelling,
                                  Type ElementTy, DeclContext *Parent,
                                  GenericParamList *GenericParams) {
  assert(ElementTy && "Deserialized element type must not be null");
  auto *const SD = new (Context)
      SubscriptDecl(Name, SourceLoc(), StaticSpelling, SourceLoc(), nullptr,
                    SourceLoc(), /*ElementTyR=*/nullptr, Parent, GenericParams);
  SD->setElementInterfaceType(ElementTy);
  return SD;
}

SubscriptDecl *SubscriptDecl::createParsed(
    ASTContext &Context, SourceLoc StaticLoc, StaticSpellingKind StaticSpelling,
    SourceLoc SubscriptLoc, ParameterList *Indices, SourceLoc ArrowLoc,
    TypeRepr *ElementTyR, DeclContext *Parent,
    GenericParamList *GenericParams) {
  assert(ElementTyR);
  auto Name = DeclName(Context, DeclBaseName::createSubscript(), Indices);
  auto *const SD = new (Context)
      SubscriptDecl(Name, StaticLoc, StaticSpelling, SubscriptLoc, Indices,
                    ArrowLoc, ElementTyR, Parent, GenericParams);
  return SD;
}

SubscriptDecl *SubscriptDecl::create(ASTContext &Context, DeclName Name,
                                     SourceLoc StaticLoc,
                                     StaticSpellingKind StaticSpelling,
                                     SourceLoc SubscriptLoc,
                                     ParameterList *Indices, SourceLoc ArrowLoc,
                                     Type ElementTy, DeclContext *Parent,
                                     GenericParamList *GenericParams) {
  auto *const SD = new (Context)
      SubscriptDecl(Name, StaticLoc, StaticSpelling, SubscriptLoc, Indices,
                    ArrowLoc, nullptr, Parent, GenericParams);
  SD->setElementInterfaceType(ElementTy);
  return SD;
}

SubscriptDecl *SubscriptDecl::createImported(ASTContext &Context, DeclName Name,
                                             SourceLoc SubscriptLoc,
                                             ParameterList *Indices,
                                             SourceLoc ArrowLoc, Type ElementTy,
                                             DeclContext *Parent,
                                             GenericParamList *GenericParams,
                                             ClangNode ClangN) {
  assert(ClangN && ElementTy);
  auto *DeclPtr = allocateMemoryForDecl<SubscriptDecl>(
      Context, sizeof(SubscriptDecl), /*includeSpaceForClangNode=*/true);

  auto *const SD = ::new (DeclPtr)
      SubscriptDecl(Name, SourceLoc(), StaticSpellingKind::None, SubscriptLoc,
                    Indices, ArrowLoc, /*ElementTyR=*/nullptr, Parent,
                    GenericParams);
  SD->setElementInterfaceType(ElementTy);
  SD->setClangNode(ClangN);
  return SD;
}

SourceRange SubscriptDecl::getSourceRange() const {
  auto Start = getStaticLoc().isValid() ? getStaticLoc() : getSubscriptLoc();
  if (Start.isInvalid())
    return SourceRange();

  if (auto End = getBracesRange().End)
    return SourceRange(Start, End);

  if (auto *Where = getTrailingWhereClause()) {
    if (auto End = Where->getSourceRange().End)
      return SourceRange(Start, End);
  }
  if (auto *ElementTy = getElementTypeRepr()) {
    if (auto End = ElementTy->getEndLoc())
      return SourceRange(Start, End);
  }
  if (ArrowLoc)
    return SourceRange(Start, ArrowLoc);

  if (auto *Indices = getIndices()) {
    if (auto End = Indices->getEndLoc())
      return SourceRange(Start, End);
  }
  return SourceRange(Start);
}

SourceRange SubscriptDecl::getSignatureSourceRange() const {
  if (isImplicit())
    return SourceRange();
  if (auto Indices = getIndices()) {
    auto End = Indices->getEndLoc();
    if (End.isValid()) {
      return SourceRange(getSubscriptLoc(), End);
    }
  }
  return getSubscriptLoc();
}

DeclName AbstractFunctionDecl::getEffectiveFullName() const {
  if (getName())
    return getName();

  if (auto accessor = dyn_cast<AccessorDecl>(this)) {
    auto &ctx = getASTContext();
    auto storage = accessor->getStorage();
    auto subscript = dyn_cast<SubscriptDecl>(storage);
    switch (accessor->getAccessorKind()) {
    // These don't have any extra implicit parameters.
    case AccessorKind::Address:
    case AccessorKind::MutableAddress:
    case AccessorKind::Get:
    case AccessorKind::DistributedGet:
    case AccessorKind::Read:
    case AccessorKind::Read2:
    case AccessorKind::Modify:
    case AccessorKind::Modify2:
      return subscript ? subscript->getName()
                       : DeclName(ctx, storage->getBaseName(),
                                  ArrayRef<Identifier>());

    case AccessorKind::Set:
    case AccessorKind::DidSet:
    case AccessorKind::WillSet:
    case AccessorKind::Init: {
      SmallVector<Identifier, 4> argNames;
      // The implicit value/buffer parameter.
      argNames.push_back(Identifier());
      // The subscript index parameters.
      if (subscript) {
        argNames.append(subscript->getName().getArgumentNames().begin(),
                        subscript->getName().getArgumentNames().end());
      }
      return DeclName(ctx, storage->getBaseName(), argNames);
    }
    }
    llvm_unreachable("bad accessor kind");
  }

  return DeclName();
}

ParameterList *swift::getParameterList(ValueDecl *source) {
  if (auto *AFD = dyn_cast<AbstractFunctionDecl>(source)) {
    return AFD->getParameters();
  } else if (auto *EED = dyn_cast<EnumElementDecl>(source)) {
    return EED->getParameterList();
  } else if (auto *SD = dyn_cast<SubscriptDecl>(source)) {
    return SD->getIndices();
  } else if (auto *MD = dyn_cast<MacroDecl>(source)) {
    return MD->parameterList;
  }

  return nullptr;
}

ParameterList *swift::getParameterList(DeclContext *source) {
  if (auto *D = source->getAsDecl()) {
    if (auto *VD = dyn_cast<ValueDecl>(D)) {
      return getParameterList(VD);
    }
  } else if (auto *CE = dyn_cast<AbstractClosureExpr>(source)) {
    return CE->getParameters();
  }

  return nullptr;
}

const ParamDecl *swift::getParameterAt(ConcreteDeclRef declRef,
                                       unsigned index) {
  auto *source = declRef.getDecl();
  if (auto *params = getParameterList(const_cast<ValueDecl *>(source))) {
    unsigned origIndex = params->getOrigParamIndex(declRef.getSubstitutions(),
                                                   index);
    return params->get(origIndex);
  }
  return nullptr;
}

const ParamDecl *swift::getParameterAt(const ValueDecl *source,
                                       unsigned index) {
  if (auto *params = getParameterList(const_cast<ValueDecl *>(source))) {
    return index < params->size() ? params->get(index) : nullptr;
  }
  return nullptr;
}

const ParamDecl *swift::getParameterAt(const DeclContext *source,
                                       unsigned index) {
  if (auto *params = getParameterList(const_cast<DeclContext *>(source))) {
    return index < params->size() ? params->get(index) : nullptr;
  }
  return nullptr;
}

CaptureInfo AbstractFunctionDecl::getCaptureInfo() const {
  auto &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
                           CaptureInfoRequest{const_cast<AbstractFunctionDecl *>(this)},
                           CaptureInfo::empty());
}

Type AbstractFunctionDecl::getMethodInterfaceType() const {
  assert(getDeclContext()->isTypeContext());
  auto Ty = getInterfaceType();
  if (Ty->is<ErrorType>())
    return Ty;
  return Ty->castTo<AnyFunctionType>()->getResult();
}

bool AbstractFunctionDecl::hasDynamicSelfResult() const {
  if (auto *funcDecl = dyn_cast<FuncDecl>(this))
    return funcDecl->getResultInterfaceType()->hasDynamicSelfType();
  return isa<ConstructorDecl>(this);
}

AbstractFunctionDecl *AbstractFunctionDecl::getAsyncAlternative() const {
  // Async functions can't have async alternatives
  if (hasAsync())
    return nullptr;

  const AvailableAttr *avAttr = nullptr;
  for (const auto *attr : getAttrs().getAttributes<AvailableAttr>()) {
    // If there's an attribute with an already-resolved rename decl, use it
    if (attr->RenameDecl) {
      avAttr = attr;
      break;
    }

    // Otherwise prefer the first availability attribute with no platform and
    // rename parameter, falling back to the first with a rename. Note that
    // `getAttrs` is in reverse source order, so the last attribute is the
    // first in source
    if (!attr->Rename.empty() &&
        (attr->Platform == PlatformKind::none || !avAttr) && !attr->isNoAsync()) {
      avAttr = attr;
    }
  }

  auto *renamedDecl = evaluateOrDefault(
      getASTContext().evaluator, RenamedDeclRequest{this, avAttr}, nullptr);
  auto *alternative = dyn_cast_or_null<AbstractFunctionDecl>(renamedDecl);
  if (!alternative || !alternative->hasAsync())
    return nullptr;
  return alternative;
}

static bool isPotentialCompletionHandler(const ParamDecl *param) {
  if (!param->getInterfaceType())
    return false;

  auto *paramType = param->getInterfaceType()->getAs<AnyFunctionType>();
  return paramType && paramType->getResult()->isVoid() &&
         !paramType->isNoEscape() && !param->isAutoClosure();
}

std::optional<unsigned>
AbstractFunctionDecl::findPotentialCompletionHandlerParam(
    const AbstractFunctionDecl *asyncAlternative) const {
  const ParameterList *params = getParameters();
  if (params->size() == 0)
    return std::nullopt;

  // If no async alternative given, just find the last parameter that matches
  // a completion handler signature
  if (!asyncAlternative) {
    for (int i = params->size() - 1; i >= 0; --i) {
      if (isPotentialCompletionHandler(params->get(i)))
        return i;
    }
    return std::nullopt;
  }

  // If this is an imported function with an async convention then we already
  // have the index, grab it from there
  auto asyncConvention = asyncAlternative->getForeignAsyncConvention();
  if (asyncConvention) {
    auto errorConvention = asyncAlternative->getForeignErrorConvention();
    unsigned handlerIndex = asyncConvention->completionHandlerParamIndex();
    if (errorConvention &&
        !errorConvention->isErrorParameterReplacedWithVoid() &&
        handlerIndex >= errorConvention->getErrorParameterIndex()) {
      handlerIndex--;
    }
    return handlerIndex;
  }

  // Otherwise, match up the parameters of each function and return the single
  // missing parameter that must also match a completion handler signature.
  // Ignore any defaulted params in the alternative if their label is different
  // to the corresponding param in the original function.

  const ParameterList *asyncParams = asyncAlternative->getParameters();
  unsigned paramIndex = 0;
  unsigned asyncParamIndex = 0;
  std::optional<unsigned> potentialParam;
  while (paramIndex < params->size() || asyncParamIndex < asyncParams->size()) {
    if (paramIndex >= params->size()) {
      // Have more async params than original params, if we haven't found a
      // completion handler then there isn't going to be any. If we have then
      // ensure the rest of the async params are defaulted
      if (!potentialParam ||
          !asyncParams->get(asyncParamIndex)->isDefaultArgument())
        return std::nullopt;
      asyncParamIndex++;
      continue;
    }

    auto *param = params->get(paramIndex);
    bool paramMatches = false;
    if (asyncParamIndex < asyncParams->size()) {
      const ParamDecl *asyncParam = asyncParams->get(asyncParamIndex);

      // Skip if the labels are different and it's defaulted
      if (param->getArgumentName() != asyncParam->getArgumentName() &&
          asyncParam->isDefaultArgument()) {
        asyncParamIndex++;
        continue;
      }

      // Don't have types for some reason, just return no match
      if (!param->getInterfaceType() || !asyncParam->getInterfaceType())
        return std::nullopt;

      paramMatches = param->getInterfaceType()->matchesParameter(
          asyncParam->getInterfaceType(), TypeMatchOptions());
    }

    if (paramMatches) {
      paramIndex++;
      asyncParamIndex++;
      continue;
    }

    // Param doesn't match, either it's the first completion handler or these
    // functions don't match
    if (potentialParam || !isPotentialCompletionHandler(param))
      return std::nullopt;

    // The next original param should match the current async, so don't
    // increment the async index
    potentialParam = paramIndex;
    paramIndex++;
  }
  return potentialParam;
}

bool AbstractFunctionDecl::argumentNameIsAPIByDefault() const {
  // Initializers have argument labels.
  if (isa<ConstructorDecl>(this))
    return true;

  if (auto func = dyn_cast<FuncDecl>(this)) {
    // Operators do not have argument labels.
    if (func->isOperator())
      return false;

    // Other functions have argument labels for all arguments
    return true;
  }

  assert(isa<DestructorDecl>(this));
  return false;
}

bool AbstractFunctionDecl::isSendable() const {
  return getAttrs().hasAttribute<SendableAttr>();
}

bool AbstractFunctionDecl::hasBody() const {
  switch (getBodyKind()) {
  case BodyKind::Deserialized:
  case BodyKind::Parsed:
  case BodyKind::SILSynthesize:
  case BodyKind::Synthesize:
  case BodyKind::Unparsed:
    return true;

  case BodyKind::None:
    return false;

  case BodyKind::TypeChecked:
    return BodyAndFP.getBody() != nullptr;
  }
}

bool AbstractFunctionDecl::bodyHasExplicitReturnStmt() const {
  return AnyFunctionRef(const_cast<AbstractFunctionDecl *>(this))
      .bodyHasExplicitReturnStmt();
}

void AbstractFunctionDecl::getExplicitReturnStmts(
    SmallVectorImpl<ReturnStmt *> &results) const {
  AnyFunctionRef(const_cast<AbstractFunctionDecl *>(this))
      .getExplicitReturnStmts(results);
}

/// Expand all preamble macros attached to the given function declaration.
static std::vector<ASTNode> expandPreamble(AbstractFunctionDecl *func) {
  std::vector<ASTNode> preamble;

  ASTContext &ctx = func->getASTContext();
  ExpandPreambleMacroRequest request{func};
  auto module = func->getParentModule();
  for (auto bufferID : evaluateOrDefault(ctx.evaluator, request, { })) {
    auto bufferStart = ctx.SourceMgr.getLocForBufferStart(bufferID);
    auto preambleSF = module->getSourceFileContainingLocation(bufferStart);
    preamble.insert(preamble.end(),
                    preambleSF->getTopLevelItems().begin(),
                    preambleSF->getTopLevelItems().end());
  }

  return preamble;
}

/// Expand body macros and produce the resulting body.
static BraceStmt *expandBodyMacro(AbstractFunctionDecl *fn) {
  ASTContext &ctx = fn->getASTContext();

  // Expand a body macro, if there is one.
  BraceStmt *macroExpandedBody = nullptr;
  if (auto bufferID = evaluateOrDefault(
          ctx.evaluator, ExpandBodyMacroRequest{fn}, std::nullopt)) {
    CharSourceRange bufferRange = ctx.SourceMgr.getRangeForBuffer(*bufferID);
    auto bufferStart = bufferRange.getStart();
    auto module = fn->getParentModule();
    auto macroSourceFile = module->getSourceFileContainingLocation(bufferStart);

    if (macroSourceFile->getTopLevelItems().size() == 1) {
      auto stmt = macroSourceFile->getTopLevelItems()[0].dyn_cast<Stmt *>();
      macroExpandedBody = dyn_cast<BraceStmt>(stmt);
    }
  }

  // Expand the preamble.
  auto preamble = expandPreamble(fn);

  // If there is no preamble, we're done one way or the other: return the
  // macro-expanded body.
  if (preamble.empty())
    return macroExpandedBody;

  // We have a preamble. The body is either the one produced by macro expansion,
  // or if not that, the one that was written.
  auto body = macroExpandedBody ? macroExpandedBody : fn->getBody();

  // If there is no body at this point, the preamble has nowhere to go.
  if (!body) {
    // FIXME: diagnose this
    return nullptr;
  }

  // Merge the preamble into the body.
  auto contents = std::move(preamble);
  contents.insert(
      contents.end(),
      body->getElements().begin(),
      body->getElements().end());
  return BraceStmt::create(
      ctx, body->getStartLoc(), contents, body->getEndLoc());
}

BraceStmt *AbstractFunctionDecl::getMacroExpandedBody() const {
  auto mutableThis = const_cast<AbstractFunctionDecl *>(this);
  switch (getBodyKind()) {
  case BodyKind::None:
  case BodyKind::Unparsed:
  case BodyKind::Parsed:
    switch (getBodyExpandedStatus()) {
    case BodyExpandedStatus::NotExpanded:
      if (auto expandedBody = expandBodyMacro(mutableThis)) {
        // Save the original body's source range.
        mutableThis->keepOriginalBodySourceRange();

        // Cache the expanded macro body as the parsed body of the function.
        mutableThis->setBodyExpandedStatus(BodyExpandedStatus::Expanded);
        mutableThis->setBodyParsed(expandedBody);

        return expandedBody;
      }

      mutableThis->setBodyExpandedStatus(BodyExpandedStatus::NoMacros);
      break;

    case BodyExpandedStatus::NoMacros:
    case BodyExpandedStatus::Expanded:
      break;
    }

    // Fall through to get the body.
    LLVM_FALLTHROUGH;

  case BodyKind::Synthesize:
  case BodyKind::TypeChecked:
  case BodyKind::SILSynthesize:
  case BodyKind::Deserialized:
    return getBody(/*canSynthesize=*/true);
  }
}

BraceStmt *AbstractFunctionDecl::getBody(bool canSynthesize) const {
  if ((getBodyKind() == BodyKind::Synthesize ||
       getBodyKind() == BodyKind::Unparsed) &&
      !canSynthesize)
    return nullptr;

  ASTContext &ctx = getASTContext();

  // Don't allow getBody() to trigger parsing of an unparsed body containing the
  // IDE inspection location.
  // FIXME: We should be properly constructing the range of the body as a
  // CharSourceRange but we can't because we don't have access to the lexer
  // here. Using the end location of the SourceRange works good enough here
  // because the last token is a '}' and the IDE inspection point is not inside
  // the closing brace.
  if (getBodyKind() == BodyKind::Unparsed &&
      ctx.SourceMgr.rangeContainsIDEInspectionTarget(
          CharSourceRange(ctx.SourceMgr, getBodySourceRange().Start,
                          getBodySourceRange().End))) {
    return nullptr;
  }

  auto mutableThis = const_cast<AbstractFunctionDecl *>(this);
  return evaluateOrDefault(ctx.evaluator,
                           ParseAbstractFunctionBodyRequest{mutableThis}, {})
      .getBody();
}

BraceStmt *AbstractFunctionDecl::getTypecheckedBody() const {
  auto &ctx = getASTContext();
  auto *mutableThis = const_cast<AbstractFunctionDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator, TypeCheckFunctionBodyRequest{mutableThis}, nullptr);
}

void AbstractFunctionDecl::setBody(BraceStmt *S, BodyKind NewBodyKind) {
  std::optional<Fingerprint> fp = std::nullopt;
  if (getBodyKind() == BodyKind::TypeChecked ||
      getBodyKind() == BodyKind::Parsed) {
    fp = BodyAndFP.getFingerprint();
  }
  BodyAndFP = BodyAndFingerprint(S, fp);
  setBodyKind(NewBodyKind);

  // Need to recompute init body kind.
  if (NewBodyKind < BodyKind::TypeChecked) {
    if (auto *ctor = dyn_cast<ConstructorDecl>(this))
      ctor->clearCachedDelegatingOrChainedInitKind();
  }
}

bool AbstractFunctionDecl::isBodySkipped() const {
  return evaluateOrDefault(getASTContext().evaluator,
                           IsFunctionBodySkippedRequest{this}, false);
}

/// Determines whether typechecking can be skipped for a function body. Bodies
/// are skipped as a performance optimization when an
/// `-experimental-skip-*-function-bodies` flag is specified and the body meets
/// the criteria for skipping. If a body is skipped during typechecking, it is
/// also skipped during SILGen. Some bodies cannot be skipped, even when they
/// otherwise meet the criteria, because typechecking them has essential
/// side-effects that are required for correctness of the AST.
bool IsFunctionBodySkippedRequest::evaluate(
    Evaluator &evaluator, const AbstractFunctionDecl *afd) const {
  auto &Ctx = afd->getASTContext();
  auto skippingMode = Ctx.TypeCheckerOpts.SkipFunctionBodies;
  if (skippingMode == FunctionBodySkipping::None)
    return false;

  // Functions that have been synthesized for clang modules will be serialized
  // because they have shared linkage.
  if (isa<ClangModuleUnit>(afd->getDeclContext()->getModuleScopeContext()))
    return false;

  if (auto *accessor = dyn_cast<AccessorDecl>(afd)) {
    // didSet accessors needs to be checked to determine whether to keep their
    // parameters.
    if (accessor->getAccessorKind() == AccessorKind::DidSet)
      return false;

    // Synthesized accessors with forced static dispatch are emitted on-demand
    // and are serialized. Since they are serialized we must be willing to
    // typecheck them.
    if (accessor->hasForcedStaticDispatch())
      return false;
  }

  // Actor initializers need to be checked to determine delegation status.
  if (auto *ctor = dyn_cast<ConstructorDecl>(afd))
    if (auto *nom = ctor->getParent()->getSelfNominalTypeDecl())
      if (nom->isAnyActor())
        return false;

  // Skipping all bodies won't serialize anything, so we can skip everything
  // else.
  if (skippingMode == FunctionBodySkipping::All)
    return true;

  // If we want all types (for LLDB) then we can't skip functions with nested
  // types. We could probably improve upon this and type-check only the nested
  // types instead for better performances.
  if (afd->hasNestedTypeDeclarations() &&
      skippingMode == FunctionBodySkipping::NonInlinableWithoutTypes)
    return false;

  // Skip functions that don't need to be serialized.
  return afd->getResilienceExpansion() != ResilienceExpansion::Minimal;
}

void AbstractFunctionDecl::setBodyToBeReparsed(SourceRange bodyRange) {
  assert(bodyRange.isValid());
  assert(getBodyKind() == BodyKind::Unparsed ||
         getBodyKind() == BodyKind::Parsed ||
         getBodyKind() == BodyKind::TypeChecked);

  keepOriginalBodySourceRange();
  BodyRange = bodyRange;
  setBodyKind(BodyKind::Unparsed);

  if (auto SF = getParentSourceFile()) {
    SF->getASTContext().evaluator.clearCachedOutput(LocalTypeDeclsRequest{SF});
  }
}

SourceRange AbstractFunctionDecl::getBodySourceRange() const {
  switch (getBodyKind()) {
  case BodyKind::None:
  case BodyKind::SILSynthesize:
  case BodyKind::Deserialized:
  case BodyKind::Synthesize:
    return SourceRange();

  case BodyKind::Parsed:
  case BodyKind::TypeChecked:
    if (auto body = getBody(/*canSynthesize=*/false))
      return body->getSourceRange();

    return SourceRange();

  case BodyKind::Unparsed:
    return BodyRange;
  }
  llvm_unreachable("bad BodyKind");
}

SourceRange AbstractFunctionDecl::getSignatureSourceRange() const {
  if (isImplicit())
    return SourceRange();

  auto paramList = getParameters();

  auto endLoc = paramList->getSourceRange().End;
  if (endLoc.isValid())
    return SourceRange(getNameLoc(), endLoc);

  return getNameLoc();
}

std::optional<Fingerprint> AbstractFunctionDecl::getBodyFingerprint() const {
  ASTContext &ctx = getASTContext();
  auto mutableThis = const_cast<AbstractFunctionDecl *>(this);
  return evaluateOrDefault(ctx.evaluator,
                           ParseAbstractFunctionBodyRequest{mutableThis}, {})
      .getFingerprint();
}

std::optional<Fingerprint>
AbstractFunctionDecl::getBodyFingerprintIncludingLocalTypeMembers() const {

  class HashCombiner : public ASTWalker {
    StableHasher &hasher;

  public:
    HashCombiner(StableHasher &hasher) : hasher(hasher) {}

    /// Only walk the arguments of a macro, to represent the source as written.
    MacroWalking getMacroWalkingBehavior() const override {
      return MacroWalking::Arguments;
    }

    PreWalkAction walkToDeclPre(Decl *D) override {
      if (D->isImplicit())
        return Action::SkipNode();

      if (auto *idc = dyn_cast<IterableDeclContext>(D)) {
        if (auto fp = idc->getBodyFingerprint())
          hasher.combine(*fp);

        // Since ASTWalker calls 'getMembers()' which might tries to synthesize
        // members etc., manually recurse into `getParsedMembers()`.
        for (auto *d : idc->getParsedMembers())
          const_cast<Decl *>(d)->walk(*this);

        return Action::SkipNode();
      }

      if (auto *afd = dyn_cast<AbstractFunctionDecl>(D)) {
        if (auto fp = afd->getBodyFingerprint())
          hasher.combine(*fp);
      }

      return Action::Continue();
    }
  };

  StableHasher hasher = StableHasher::defaultHasher();
  HashCombiner combiner(hasher);
  const_cast<AbstractFunctionDecl *>(this)->walk(combiner);
  return Fingerprint(std::move(hasher));
}

ObjCSelector
AbstractFunctionDecl::getObjCSelector(DeclName preferredName,
                                      bool skipIsObjCResolution) const {
  // FIXME: Forces computation of the Objective-C selector.
  if (!skipIsObjCResolution)
    (void)isObjC();

  // If there is an @objc attribute with a name, use that name.
  auto *objc = getAttrs().getAttribute<ObjCAttr>();
  if (auto name = getNameFromObjcAttribute(objc, preferredName)) {
    return *name;
  }

  auto &ctx = getASTContext();

  StringRef baseNameStr;
  if (auto destructor = dyn_cast<DestructorDecl>(this)) {
    return destructor->getObjCSelector();
  } else if (auto func = dyn_cast<FuncDecl>(this)) {
    // Otherwise cast this to be able to access getName()
    baseNameStr = func->getBaseIdentifier().str();
  } else if (isa<ConstructorDecl>(this)) {
    baseNameStr = "init";
  } else {
    llvm_unreachable("Unknown subclass of AbstractFunctionDecl");
  }

  auto argNames = getName().getArgumentNames();

  // Use the preferred name if specified
  if (preferredName) {
    // Return invalid selector if argument count doesn't match.
    if (argNames.size() != preferredName.getArgumentNames().size()) {
      return ObjCSelector();
    }
    baseNameStr = preferredName.getBaseName().userFacingName();
    argNames = preferredName.getArgumentNames();
  }

  auto baseName = ctx.getIdentifier(baseNameStr);

  if (auto accessor = dyn_cast<AccessorDecl>(this)) {
    // For a getter or setter, go through the variable or subscript decl.
    auto asd = accessor->getStorage();
    if (accessor->isGetter())
      return asd->getObjCGetterSelector(baseName);
    if (accessor->isSetter())
      return asd->getObjCSetterSelector(baseName);
  }

  // If this is a zero-parameter initializer with a long selector
  // name, form that selector.
  auto ctor = dyn_cast<ConstructorDecl>(this);
  if (ctor && ctor->isObjCZeroParameterWithLongSelector()) {
    Identifier firstName = argNames[0];
    llvm::SmallString<16> scratch;
    scratch += "init";

    // If the first argument name doesn't start with a preposition, add "with".
    if (!isPreposition(camel_case::getFirstWord(firstName.str()))) {
      camel_case::appendSentenceCase(scratch, "With");
    }

    camel_case::appendSentenceCase(scratch, firstName.str());
    return ObjCSelector(ctx, 0, ctx.getIdentifier(scratch));
  }

  // The number of selector pieces we'll have.
  std::optional<ForeignAsyncConvention> asyncConvention =
      getForeignAsyncConvention();
  std::optional<ForeignErrorConvention> errorConvention =
      getForeignErrorConvention();
  unsigned numSelectorPieces
    = argNames.size() + (asyncConvention.has_value() ? 1 : 0)
    + (errorConvention.has_value() ? 1 : 0);

  // If we have no arguments, it's a nullary selector.
  if (numSelectorPieces == 0) {
    return ObjCSelector(ctx, 0, baseName);
  }

 // If it's a unary selector with no name for the first argument, we're done.
  if (numSelectorPieces == 1 && argNames.size() == 1 && argNames[0].empty()) {
    return ObjCSelector(ctx, 1, baseName);
  }

  /// Collect the selector pieces.
  SmallVector<Identifier, 4> selectorPieces;
  selectorPieces.reserve(numSelectorPieces);
  bool didStringManipulation = false;
  unsigned argIndex = 0;
  for (unsigned piece = 0; piece != numSelectorPieces; ++piece) {
    if (piece > 0) {
      // If we have an async convention that inserts a completion handler
      // parameter here, add "completionHandler".
      if (asyncConvention &&
          piece == asyncConvention->completionHandlerParamIndex()) {
        selectorPieces.push_back(ctx.getIdentifier("completionHandler"));
        continue;
      }

      // If we have an error convention that inserts an error parameter
      // here, add "error".
      if (errorConvention &&
          piece == errorConvention->getErrorParameterIndex()) {
        selectorPieces.push_back(ctx.Id_error);
        continue;
      }

      // Selector pieces beyond the first are simple.
      selectorPieces.push_back(argNames[argIndex++]);
      continue;
    }

    // For the first selector piece, attach either the first parameter,
    // "withCompletionHandler", or "AndReturnError" to the base name,
    // if appropriate.
    auto firstPiece = baseName;
    llvm::SmallString<32> scratch;
    scratch += firstPiece.str();
    if (asyncConvention &&
        piece == asyncConvention->completionHandlerParamIndex()) {
      // The completion handler is first; append "WithCompletionHandler".
      camel_case::appendSentenceCase(scratch, "WithCompletionHandler");

      firstPiece = ctx.getIdentifier(scratch);
      didStringManipulation = true;
    } else if (errorConvention &&
               piece == errorConvention->getErrorParameterIndex()) {
      // The error is first; append "AndReturnError".
      camel_case::appendSentenceCase(scratch, "AndReturnError");

      firstPiece = ctx.getIdentifier(scratch);
      didStringManipulation = true;
    } else if (!argNames[argIndex].empty()) {
      // If the first argument name doesn't start with a preposition, and the
      // method name doesn't end with a preposition, add "with".
      auto firstName = argNames[argIndex++];
      if (!isPreposition(camel_case::getFirstWord(firstName.str())) &&
          !isPreposition(camel_case::getLastWord(firstPiece.str()))) {
        camel_case::appendSentenceCase(scratch, "With");
      }

      camel_case::appendSentenceCase(scratch, firstName.str());
      firstPiece = ctx.getIdentifier(scratch);
      didStringManipulation = true;
    } else {
      ++argIndex;
    }

    selectorPieces.push_back(firstPiece);
  }
  assert(argIndex == argNames.size());

  // Form the result.
  auto result = ObjCSelector(ctx, selectorPieces.size(), selectorPieces);

  // If we did any string manipulation, cache the result. We don't want to
  // do that again.
  if (didStringManipulation && objc && !preferredName)
    const_cast<ObjCAttr *>(objc)->setName(result, /*implicit=*/true);

  return result;
}

bool AbstractFunctionDecl::isObjCInstanceMethod() const {
  return isInstanceMember() || isa<ConstructorDecl>(this);
}

bool AbstractFunctionDecl::needsNewVTableEntry() const {
  auto &ctx = getASTContext();
  return evaluateOrDefault(
      ctx.evaluator,
      NeedsNewVTableEntryRequest{const_cast<AbstractFunctionDecl *>(this)},
      false);
}

ParamDecl *AbstractFunctionDecl::getImplicitSelfDecl(bool createIfNeeded) {
  auto **selfDecl = getImplicitSelfDeclStorage();

  // If this is not a method, return nullptr.
  if (selfDecl == nullptr)
    return nullptr;

  // If we've already created a 'self' parameter, just return it.
  if (*selfDecl != nullptr)
    return *selfDecl;

  // If we're not allowed to create one, return nullptr.
  if (!createIfNeeded)
    return nullptr;

  // Create and save our 'self' parameter.
  auto &ctx = getASTContext();
  *selfDecl = new (ctx) ParamDecl(SourceLoc(), SourceLoc(), Identifier(),
                                  getLoc(), ctx.Id_self, this);
  (*selfDecl)->setImplicit();
  return *selfDecl;
}

void AbstractFunctionDecl::setParameters(ParameterList *BodyParams) {
#ifndef NDEBUG
  const auto Name = getName();
  if (!isa<DestructorDecl>(this))
    assert((!Name || !Name.isSimpleName()) && "Must have a compound name");
  assert(!Name || (Name.getArgumentNames().size() == BodyParams->size()));
#endif

  Params = BodyParams;
  BodyParams->setDeclContextOfParamDecls(this);
}

OpaqueTypeDecl::OpaqueTypeDecl(ValueDecl *NamingDecl,
                               GenericParamList *GenericParams, DeclContext *DC,
                               GenericSignature OpaqueInterfaceGenericSignature,
                               ArrayRef<TypeRepr *>
                                   OpaqueReturnTypeReprs)
    : GenericTypeDecl(DeclKind::OpaqueType, DC, Identifier(), SourceLoc(), {},
                      GenericParams),
      NamingDeclAndHasOpaqueReturnTypeRepr(
        NamingDecl, !OpaqueReturnTypeReprs.empty()),
      OpaqueInterfaceGenericSignature(OpaqueInterfaceGenericSignature) {
  // Always implicit.
  setImplicit();

  /// We either have no opaque return type representations ('some P'), or we
  /// have one for each opaque generic parameter.
  assert(OpaqueReturnTypeReprs.empty() ||
         OpaqueReturnTypeReprs.size() ==
            OpaqueInterfaceGenericSignature.getInnermostGenericParams().size());
  std::uninitialized_copy(
      OpaqueReturnTypeReprs.begin(), OpaqueReturnTypeReprs.end(),
      getTrailingObjects<TypeRepr *>());
}

OpaqueTypeDecl *OpaqueTypeDecl::get(
      ValueDecl *NamingDecl, GenericParamList *GenericParams,
      DeclContext *DC,
      GenericSignature OpaqueInterfaceGenericSignature,
      ArrayRef<TypeRepr *> OpaqueReturnTypeReprs) {
  ASTContext &ctx = DC->getASTContext();
  auto size = totalSizeToAlloc<TypeRepr *>(
      OpaqueReturnTypeReprs.size());
  auto mem = ctx.Allocate(size, alignof(OpaqueTypeDecl));
  return new (mem) OpaqueTypeDecl(
      NamingDecl, GenericParams, DC, OpaqueInterfaceGenericSignature,
      OpaqueReturnTypeReprs);
}

bool OpaqueTypeDecl::isOpaqueReturnTypeOfFunction(
                                       const AbstractFunctionDecl *func) const {
  // Either the function is declared with its own opaque return type...
  if (getNamingDecl() == func)
    return true;

  // ...or the function is a getter for a property or subscript with an
  // opaque return type.
  if (auto accessor = dyn_cast<AccessorDecl>(func)) {
    return accessor->isGetter() && getNamingDecl() == accessor->getStorage();
  }

  return false;
}

bool OpaqueTypeDecl::hasExplicitGenericParams() const {
  return getExplicitGenericParam(0) != nullptr;
}

GenericTypeParamDecl *OpaqueTypeDecl::getExplicitGenericParam(
    unsigned ordinal) const {
  if (ordinal >= getOpaqueGenericParams().size())
    return nullptr;

  auto genericParamType = getOpaqueGenericParams()[ordinal];
  return genericParamType->getDecl();
}

bool OpaqueTypeDecl::exportUnderlyingType() const {
  auto mod = getDeclContext()->getParentModule();
  if (mod->getResilienceStrategy() != ResilienceStrategy::Resilient)
    return true;

  // If we perform package CMO, in-package clients must have access to the
  // underlying type.
  if (mod->serializePackageEnabled())
    return true;

  ValueDecl *namingDecl = getNamingDecl();
  if (auto *AFD = dyn_cast<AbstractFunctionDecl>(namingDecl))
    return AFD->getResilienceExpansion() == ResilienceExpansion::Minimal;

  if (auto *ASD = dyn_cast<AbstractStorageDecl>(namingDecl)) {
    for (auto *accessor : ASD->getAllAccessors())
      if (accessor->getResilienceExpansion() == ResilienceExpansion::Minimal)
        return true;
    return false;
  }

  llvm_unreachable("The naming decl is expected to be either an AFD or ASD");
}

std::optional<SubstitutionMap>
OpaqueTypeDecl::getUniqueUnderlyingTypeSubstitutions() const {
  return evaluateOrDefault(getASTContext().evaluator,
                           UniqueUnderlyingTypeSubstitutionsRequest{this}, {});
}

std::optional<unsigned>
OpaqueTypeDecl::getAnonymousOpaqueParamOrdinal(TypeRepr *repr) const {
  assert(NamingDeclAndHasOpaqueReturnTypeRepr.getInt() &&
         "can't do opaque param lookup without underlying interface repr");
  auto opaqueReprs = getOpaqueReturnTypeReprs();
  auto found = std::find(opaqueReprs.begin(), opaqueReprs.end(), repr);
  if (found != opaqueReprs.end())
    return found - opaqueReprs.begin();
  return std::nullopt;
}

Identifier OpaqueTypeDecl::getOpaqueReturnTypeIdentifier() const {
  assert(getNamingDecl() && "not an opaque return type");
  if (!OpaqueReturnTypeIdentifier.empty())
    return OpaqueReturnTypeIdentifier;
  
  SmallString<64> mangleBuf;
  {
    llvm::raw_svector_ostream os(mangleBuf);
    Mangle::ASTMangler mangler;
    os << mangler.mangleOpaqueTypeDecl(this);
  }

  OpaqueReturnTypeIdentifier = getASTContext().getIdentifier(mangleBuf);
  return OpaqueReturnTypeIdentifier;
}

void OpaqueTypeDecl::setConditionallyAvailableSubstitutions(
    ArrayRef<ConditionallyAvailableSubstitutions *> substitutions) {
  assert(!ConditionallyAvailableTypes &&
         "resetting conditionally available substitutions?!");
  ConditionallyAvailableTypes = getASTContext().AllocateCopy(substitutions);
}

OpaqueTypeDecl::ConditionallyAvailableSubstitutions *
OpaqueTypeDecl::ConditionallyAvailableSubstitutions::get(
    ASTContext &ctx,
    ArrayRef<AvailabilityCondition> availabilityContext,
    SubstitutionMap substitutions) {
  auto size =
      totalSizeToAlloc<AvailabilityCondition>(availabilityContext.size());
  auto mem = ctx.Allocate(size, alignof(ConditionallyAvailableSubstitutions));
  return new (mem)
      ConditionallyAvailableSubstitutions(availabilityContext, substitutions);
}

bool AbstractFunctionDecl::hasInlinableBodyText() const {
  switch (getBodyKind()) {
  case BodyKind::Deserialized:
    return true;

  case BodyKind::Unparsed:
  case BodyKind::Parsed:
  case BodyKind::TypeChecked:
    if (auto body = getBody())
      return !body->isImplicit();
    return false;

  case BodyKind::None:
  case BodyKind::Synthesize:
  case BodyKind::SILSynthesize:
    return false;
  }
  llvm_unreachable("covered switch");
}

StringRef AbstractFunctionDecl::getInlinableBodyText(
  SmallVectorImpl<char> &scratch) const {
  assert(hasInlinableBodyText() &&
         "can't get string representation of function with no text");

  if (getBodyKind() == BodyKind::Deserialized)
    return BodyStringRepresentation;

  auto body = getBody();
  return extractInlinableText(getASTContext(), body, scratch);
}

/// A uniqued list of derivative function configurations.
struct AbstractFunctionDecl::DerivativeFunctionConfigurationList
    : public ASTAllocated<DerivativeFunctionConfigurationList>,
      public llvm::SetVector<AutoDiffConfig> {};

void AbstractFunctionDecl::prepareDerivativeFunctionConfigurations() {
  if (DerivativeFunctionConfigs)
    return;
  auto &ctx = getASTContext();
  DerivativeFunctionConfigs = new (ctx) DerivativeFunctionConfigurationList();
  // Register an `ASTContext` cleanup calling the list destructor.
  ctx.addCleanup([this]() {
    this->DerivativeFunctionConfigs->~DerivativeFunctionConfigurationList();
  });
}

ArrayRef<AutoDiffConfig>
AbstractFunctionDecl::getDerivativeFunctionConfigurations() {
  prepareDerivativeFunctionConfigurations();

  // Resolve derivative function configurations from `@differentiable`
  // attributes by type-checking them.
  for (auto *diffAttr : getAttrs().getAttributes<DifferentiableAttr>())
    (void)diffAttr->getParameterIndices();
  // For accessors: resolve derivative function configurations from storage
  // `@differentiable` attributes by type-checking them.
  if (auto *accessor = dyn_cast<AccessorDecl>(this)) {
    auto *storage = accessor->getStorage();
    for (auto *diffAttr : storage->getAttrs().getAttributes<DifferentiableAttr>())
      (void)diffAttr->getParameterIndices();
  }

  // Load derivative configurations from imported modules.
  auto &ctx = getASTContext();
  if (ctx.getCurrentGeneration() > DerivativeFunctionConfigGeneration) {
    unsigned previousGeneration = DerivativeFunctionConfigGeneration;
    DerivativeFunctionConfigGeneration = ctx.getCurrentGeneration();
    ctx.loadDerivativeFunctionConfigurations(this, previousGeneration,
                                             *DerivativeFunctionConfigs);
  }

  return DerivativeFunctionConfigs->getArrayRef();
}

void AbstractFunctionDecl::addDerivativeFunctionConfiguration(
    const AutoDiffConfig &config) {
  prepareDerivativeFunctionConfigurations();
  DerivativeFunctionConfigs->insert(config);
}

std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
AbstractFunctionDecl::getLifetimeDependencies() const {
  if (!isa<FuncDecl>(this) && !isa<ConstructorDecl>(this)) {
    return std::nullopt;
  }

  return evaluateOrDefault(
      getASTContext().evaluator,
      LifetimeDependenceInfoRequest{const_cast<AbstractFunctionDecl *>(this)},
      std::nullopt);
}

void FuncDecl::setResultInterfaceType(Type type) {
  getASTContext().evaluator.cacheOutput(ResultTypeRequest{this},
                                        std::move(type));
}

void FuncDecl::setDeserializedResultTypeLoc(TypeLoc ResultTyR) {
  FnRetType = ResultTyR;
}

FuncDecl *FuncDecl::createImpl(ASTContext &Context,
                               SourceLoc StaticLoc,
                               StaticSpellingKind StaticSpelling,
                               SourceLoc FuncLoc,
                               DeclName Name, SourceLoc NameLoc,
                               bool Async, SourceLoc AsyncLoc,
                               bool Throws, SourceLoc ThrowsLoc,
                               TypeLoc ThrownTy,
                               GenericParamList *GenericParams,
                               DeclContext *Parent,
                               ClangNode ClangN) {
  bool HasImplicitSelfDecl = Parent->isTypeContext();
  size_t Size = sizeof(FuncDecl) + (HasImplicitSelfDecl
                                    ? sizeof(ParamDecl *)
                                    : 0);
  void *DeclPtr = allocateMemoryForDecl<FuncDecl>(Context, Size,
                                                  !ClangN.isNull());
  auto D = ::new (DeclPtr)
      FuncDecl(DeclKind::Func, StaticLoc, StaticSpelling, FuncLoc,
               Name, NameLoc, Async, AsyncLoc, Throws, ThrowsLoc, ThrownTy,
               HasImplicitSelfDecl, GenericParams, Parent);
  if (ClangN)
    D->setClangNode(ClangN);
  if (HasImplicitSelfDecl)
    *D->getImplicitSelfDeclStorage() = nullptr;

  return D;
}

FuncDecl *FuncDecl::createDeserialized(ASTContext &Context,
                                       StaticSpellingKind StaticSpelling,
                                       DeclName Name, bool Async, bool Throws,
                                       Type ThrownType,
                                       GenericParamList *GenericParams,
                                       Type FnRetType, DeclContext *Parent) {
  assert(FnRetType && "Deserialized result type must not be null");
  auto *const FD =
      FuncDecl::createImpl(Context, SourceLoc(), StaticSpelling, SourceLoc(),
                           Name, SourceLoc(), Async, SourceLoc(), Throws,
                           SourceLoc(), TypeLoc::withoutLoc(ThrownType),
                           GenericParams, Parent,
                           ClangNode());
  FD->setResultInterfaceType(FnRetType);
  return FD;
}

FuncDecl *FuncDecl::create(ASTContext &Context, SourceLoc StaticLoc,
                           StaticSpellingKind StaticSpelling, SourceLoc FuncLoc,
                           DeclName Name, SourceLoc NameLoc, bool Async,
                           SourceLoc AsyncLoc, bool Throws, SourceLoc ThrowsLoc,
                           TypeRepr *ThrownTyR,
                           GenericParamList *GenericParams,
                           ParameterList *BodyParams, TypeRepr *ResultTyR,
                           DeclContext *Parent) {
  auto *const FD = FuncDecl::createImpl(
      Context, StaticLoc, StaticSpelling, FuncLoc, Name, NameLoc, Async,
      AsyncLoc, Throws, ThrowsLoc, ThrownTyR, GenericParams, Parent,
      ClangNode());
  FD->setParameters(BodyParams);
  FD->FnRetType = TypeLoc(ResultTyR);
  if (llvm::isa_and_nonnull<SendingTypeRepr>(ResultTyR))
    FD->setSendingResult();
  return FD;
}

FuncDecl *FuncDecl::createImplicit(ASTContext &Context,
                                   StaticSpellingKind StaticSpelling,
                                   DeclName Name, SourceLoc NameLoc, bool Async,
                                   bool Throws, Type ThrownType,
                                   GenericParamList *GenericParams,
                                   ParameterList *BodyParams, Type FnRetType,
                                   DeclContext *Parent) {
  assert(FnRetType);
  auto *const FD = FuncDecl::createImpl(
      Context, SourceLoc(), StaticSpelling, SourceLoc(), Name, NameLoc, Async,
      SourceLoc(), Throws, SourceLoc(), TypeLoc::withoutLoc(ThrownType),
      GenericParams, Parent, ClangNode());
  FD->setImplicit();
  FD->setParameters(BodyParams);
  FD->setResultInterfaceType(FnRetType);
  return FD;
}

FuncDecl *FuncDecl::createImported(ASTContext &Context, SourceLoc FuncLoc,
                                   DeclName Name, SourceLoc NameLoc, bool Async,
                                   bool Throws, Type ThrownType,
                                   ParameterList *BodyParams,
                                   Type FnRetType,
                                   GenericParamList *GenericParams,
                                   DeclContext *Parent, ClangNode ClangN) {
  assert(ClangN);
  auto *const FD = FuncDecl::createImpl(
      Context, SourceLoc(), StaticSpellingKind::None, FuncLoc, Name, NameLoc,
      Async, SourceLoc(), Throws, SourceLoc(), TypeLoc::withoutLoc(ThrownType),
      GenericParams, Parent, ClangN);
  FD->setParameters(BodyParams);
  FD->setResultInterfaceType(FnRetType);
  return FD;
}

OperatorDecl *FuncDecl::getOperatorDecl() const {
  // Fast-path: Most functions are not operators.
  if (!isOperator()) {
    return nullptr;
  }
  return evaluateOrDefault(getASTContext().evaluator,
                           FunctionOperatorRequest{
                             const_cast<FuncDecl *>(this)
                           },
                           nullptr);
}

bool FuncDecl::isStatic() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    IsStaticRequest{const_cast<FuncDecl *>(this)},
    false);
}

AccessorDecl *AccessorDecl::createImpl(
    ASTContext &ctx, SourceLoc declLoc, SourceLoc accessorKeywordLoc,
    AccessorKind accessorKind, AbstractStorageDecl *storage, bool async,
    SourceLoc asyncLoc, bool throws, SourceLoc throwsLoc, TypeLoc thrownType,
    DeclContext *parent, ClangNode clangNode) {
  bool hasImplicitSelfDecl = parent->isTypeContext();
  size_t size = sizeof(AccessorDecl) + (hasImplicitSelfDecl
                                        ? sizeof(ParamDecl *)
                                        : 0);
  void *buffer = allocateMemoryForDecl<AccessorDecl>(ctx, size,
                                                     !clangNode.isNull());
  auto D = ::new (buffer) AccessorDecl(
      declLoc, accessorKeywordLoc, accessorKind, storage, async, asyncLoc,
      throws, throwsLoc, thrownType, hasImplicitSelfDecl, parent);
  if (clangNode)
    D->setClangNode(clangNode);
  if (hasImplicitSelfDecl)
    *D->getImplicitSelfDeclStorage() = nullptr;

  return D;
}

AccessorDecl *AccessorDecl::createDeserialized(ASTContext &ctx,
                                               AccessorKind accessorKind,
                                               AbstractStorageDecl *storage,
                                               bool async, bool throws,
                                               Type thrownType, Type fnRetType,
                                               DeclContext *parent) {
  assert(fnRetType && "Deserialized result type must not be null");
  auto *const D = AccessorDecl::createImpl(
      ctx, SourceLoc(), SourceLoc(), accessorKind, storage, async, SourceLoc(),
      throws, SourceLoc(), TypeLoc::withoutLoc(thrownType), parent,
      ClangNode());
  D->setResultInterfaceType(fnRetType);
  return D;
}

AccessorDecl *AccessorDecl::create(ASTContext &ctx, SourceLoc declLoc,
                                   SourceLoc accessorKeywordLoc,
                                   AccessorKind accessorKind,
                                   AbstractStorageDecl *storage, bool async,
                                   SourceLoc asyncLoc, bool throws,
                                   SourceLoc throwsLoc, TypeLoc thrownType,
                                   ParameterList *bodyParams, Type fnRetType,
                                   DeclContext *parent, ClangNode clangNode) {
  auto *D = AccessorDecl::createImpl(
      ctx, declLoc, accessorKeywordLoc, accessorKind, storage, async, asyncLoc,
      throws, throwsLoc, thrownType, parent, clangNode);
  D->setParameters(bodyParams);
  D->setResultInterfaceType(fnRetType);
  return D;
}

AccessorDecl *AccessorDecl::createImplicit(ASTContext &ctx,
                                           AccessorKind accessorKind,
                                           AbstractStorageDecl *storage,
                                           bool async, bool throws,
                                           TypeLoc thrownType,
                                           Type fnRetType,
                                           DeclContext *parent) {
  AccessorDecl *D = AccessorDecl::createImpl(
      ctx, /*declLoc=*/SourceLoc(),
      /*accessorKeywordLoc=*/SourceLoc(), accessorKind,
      storage, async, /*asyncLoc=*/SourceLoc(),
      /*throws=*/true, /*throwsLoc=*/SourceLoc(),
      thrownType, parent,
      /*clangNode=*/ClangNode());
  D->setImplicit();
  D->setResultInterfaceType(fnRetType);
  return D;
}

AccessorDecl *AccessorDecl::createParsed(
    ASTContext &ctx, AccessorKind accessorKind, AbstractStorageDecl *storage,
    SourceLoc declLoc, SourceLoc accessorKeywordLoc, ParameterList *paramList,
    SourceLoc asyncLoc, SourceLoc throwsLoc, TypeRepr *thrownType,
    DeclContext *dc) {
  auto *accessor = AccessorDecl::createImpl(
      ctx, declLoc, accessorKeywordLoc, accessorKind, storage,
      /*async*/ asyncLoc.isValid(), asyncLoc,
      /*throws*/ throwsLoc.isValid(), throwsLoc, thrownType, dc,
      /*clangNode*/ ClangNode());

  // Set up the parameter list. This is the "newValue" name (for setters),
  // followed by the index list (for subscripts).  For non-subscript getters,
  // this degenerates down to "()".
  //
  // We put the 'newValue' argument before the subscript index list as a
  // micro-optimization for Objective-C thunk generation.
  SmallVector<ParamDecl *, 2> newParams;
  SourceLoc paramsStart, paramsEnd;
  if (paramList) {
    assert(paramList->size() == 1 &&
           "Should only have a single parameter in the list");
    newParams.push_back(paramList->get(0));
    paramsStart = paramList->getStartLoc();
    paramsEnd = paramList->getEndLoc();
  } else {
    // No parameter list, if we have an implicit parameter name, fill it in.
    auto implicitName = AccessorDecl::implicitParameterNameFor(accessorKind);
    if (!implicitName.empty()) {
      auto *implicitParam = new (ctx)
          ParamDecl(SourceLoc(), SourceLoc(), Identifier(), declLoc,
                    ctx.getIdentifier(implicitName), /*declContext*/ accessor);
      implicitParam->setImplicit();
      newParams.push_back(implicitParam);
    }
  }

  // If this is a subscript accessor, we need to splice in the subscript
  // parameters into the accessor's parameter list.
  if (auto *SD = dyn_cast<SubscriptDecl>(storage)) {
    auto *indices = SD->getIndices();
    if (paramsStart.isInvalid()) {
      paramsStart = indices->getStartLoc();
      paramsEnd = indices->getEndLoc();
    }
    for (auto *subscriptParam : *indices) {
      // Clone the parameter.
      auto *param = new (ctx) ParamDecl(
          subscriptParam->getSpecifierLoc(),
          subscriptParam->getArgumentNameLoc(),
          subscriptParam->getArgumentName(), subscriptParam->getNameLoc(),
          subscriptParam->getName(), /*declContext*/ accessor);
      param->setAutoClosure(subscriptParam->isAutoClosure());

      // The cloned parameter is implicit.
      param->setImplicit();

      newParams.push_back(param);
    }
  }
  accessor->setParameters(
      ParameterList::create(ctx, paramsStart, newParams, paramsEnd));
  return accessor;
}

StringRef AccessorDecl::implicitParameterNameFor(AccessorKind kind) {
  switch (kind) {
  case AccessorKind::Set:
  case AccessorKind::WillSet:
  case AccessorKind::Init:
    return "newValue";
  case AccessorKind::DidSet:
    return "oldValue";
  case AccessorKind::Get:
  case AccessorKind::DistributedGet:
  case AccessorKind::Read:
  case AccessorKind::Read2:
  case AccessorKind::Modify:
  case AccessorKind::Modify2:
  case AccessorKind::Address:
  case AccessorKind::MutableAddress:
    return StringRef();
  }
}

bool AccessorDecl::isAssumedNonMutating() const {
  switch (getAccessorKind()) {
  case AccessorKind::Get:
  case AccessorKind::DistributedGet:
  case AccessorKind::Address:
  case AccessorKind::Read:
  case AccessorKind::Read2:
    return true;

  case AccessorKind::Set:
  case AccessorKind::WillSet:
  case AccessorKind::DidSet:
  case AccessorKind::MutableAddress:
  case AccessorKind::Modify:
  case AccessorKind::Modify2:
  case AccessorKind::Init:
    return false;
  }
  llvm_unreachable("bad accessor kind");
}

bool AccessorDecl::isExplicitNonMutating() const {
  return !isMutating() &&
    !isAssumedNonMutating() &&
    isInstanceMember() &&
    !getDeclContext()->getDeclaredInterfaceType()->hasReferenceSemantics();
}

bool AccessorDecl::isSimpleDidSet() const {
  auto mutableThis = const_cast<AccessorDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           SimpleDidSetRequest{mutableThis}, false);
}

void AccessorDecl::printUserFacingName(raw_ostream &out) const {
  switch (getAccessorKind()) {
  case AccessorKind::Get:
    out << "getter:";
    break;
  case AccessorKind::DistributedGet:
    out << "_distributed_getter:";
    break;
  case AccessorKind::Set:
    out << "setter:";
    break;
  default:
    out << getName();
    return;
  }

  out << getStorage()->getName() << "(";
  if (this->isSetter()) {
    for (const auto *param : *getParameters()) {
      out << param->getName() << ":";
    }
  }
  out << ")";
}

ArrayRef<VarDecl *> AccessorDecl::getInitializedProperties() const {
  assert(isInitAccessor());

  if (auto *SR = getAttrs().getAttribute<StorageRestrictionsAttr>())
    return SR->getInitializesProperties(const_cast<AccessorDecl *>(this));

  return {};
}

ArrayRef<VarDecl *> AccessorDecl::getAccessedProperties() const {
  assert(isInitAccessor());

  if (auto *SR = getAttrs().getAttribute<StorageRestrictionsAttr>())
    return SR->getAccessesProperties(const_cast<AccessorDecl *>(this));

  return {};
}

StaticSpellingKind FuncDecl::getCorrectStaticSpelling() const {
  assert(getDeclContext()->isTypeContext());
  if (!isStatic())
    return StaticSpellingKind::None;
  if (getStaticSpelling() != StaticSpellingKind::None)
    return getStaticSpelling();

  return getCorrectStaticSpellingForDecl(this);
}

Type FuncDecl::getResultInterfaceType() const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<FuncDecl *>(this);
  if (auto type = evaluateOrDefault(ctx.evaluator,
                           ResultTypeRequest{mutableThis},
                           Type()))
    return type;
  return ErrorType::get(ctx);
}

bool FuncDecl::isUnaryOperator() const {
  if (!isOperator())
    return false;
  
  auto *params = getParameters();
  return params->size() == 1 && !params->get(0)->isVariadic();
}

bool FuncDecl::isBinaryOperator() const {
  if (!isOperator())
    return false;
  
  auto *params = getParameters();
  return params->size() == 2 &&
    !params->get(0)->isVariadic() &&
    !params->get(1)->isVariadic();
}

SelfAccessKind FuncDecl::getSelfAccessKind() const {
  auto &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
                           SelfAccessKindRequest{const_cast<FuncDecl *>(this)},
                           SelfAccessKind::NonMutating);
}

LifetimeAnnotation FuncDecl::getLifetimeAnnotation() const {
  // Copyable parameters which are consumed have eager-move semantics.
  if (getSelfAccessKind() == SelfAccessKind::Consuming) {
    auto *selfDecl = getImplicitSelfDecl();
    if (selfDecl && !selfDecl->getTypeInContext()->isNoncopyable()) {
      if (getAttrs().hasAttribute<NoEagerMoveAttr>())
        return LifetimeAnnotation::Lexical;
      return LifetimeAnnotation::EagerMove;
    }
  }
  return getLifetimeAnnotationFromAttributes();
}

bool FuncDecl::isCallAsFunctionMethod() const {
  return getBaseIdentifier() == getASTContext().Id_callAsFunction &&
         isInstanceMember();
}

bool FuncDecl::isMainTypeMainMethod() const {
  return (getBaseIdentifier() == getASTContext().Id_main) &&
         !isInstanceMember() && getResultInterfaceType()->isVoid() &&
         getParameters()->size() == 0;
}

ConstructorDecl::ConstructorDecl(DeclName Name, SourceLoc ConstructorLoc,
                                 bool Failable, SourceLoc FailabilityLoc,
                                 bool Async, SourceLoc AsyncLoc,
                                 bool Throws, SourceLoc ThrowsLoc,
                                 TypeLoc ThrownType,
                                 ParameterList *BodyParams,
                                 GenericParamList *GenericParams,
                                 DeclContext *Parent, TypeRepr *ResultTyR)
  : AbstractFunctionDecl(DeclKind::Constructor, Parent, Name, ConstructorLoc,
                         Async, AsyncLoc, Throws, ThrowsLoc, ThrownType,
                         /*HasImplicitSelfDecl=*/true,
                         GenericParams),
    FailabilityLoc(FailabilityLoc),
    SelfDecl(nullptr)
{
  if (BodyParams)
    setParameters(BodyParams);

  InitRetType = TypeLoc(ResultTyR);
  Bits.ConstructorDecl.HasStubImplementation = 0;
  Bits.ConstructorDecl.Failable = Failable;

  assert(Name.getBaseName().isConstructor());
}

ConstructorDecl *ConstructorDecl::createImported(
    ASTContext &ctx, ClangNode clangNode, DeclName name,
    SourceLoc constructorLoc, bool failable, SourceLoc failabilityLoc,
    bool async, SourceLoc asyncLoc,
    bool throws, SourceLoc throwsLoc, Type thrownType,
    ParameterList *bodyParams,
    GenericParamList *genericParams, DeclContext *parent) {
  void *declPtr = allocateMemoryForDecl<ConstructorDecl>(
      ctx, sizeof(ConstructorDecl), true);
  auto ctor = ::new (declPtr)
      ConstructorDecl(name, constructorLoc,
                      failable, failabilityLoc, 
                      async, asyncLoc,
                      throws, throwsLoc, TypeLoc::withoutLoc(thrownType),
                      bodyParams, genericParams, parent,
                      /*LifetimeDependenceTypeRepr*/ nullptr);
  ctor->setClangNode(clangNode);
  return ctor;
}

void ConstructorDecl::setDeserializedResultTypeLoc(TypeLoc ResultTyR) {
  InitRetType = ResultTyR;
}

bool ConstructorDecl::isObjCZeroParameterWithLongSelector() const {
  // The initializer must have a single, non-empty argument name.
  if (getName().getArgumentNames().size() != 1 ||
      getName().getArgumentNames()[0].empty())
    return false;

  auto *params = getParameters();
  if (params->size() != 1)
    return false;

  return params->get(0)->getInterfaceType()->isVoid();
}

DestructorDecl::DestructorDecl(SourceLoc DestructorLoc, DeclContext *Parent)
  : AbstractFunctionDecl(DeclKind::Destructor, Parent,
                         DeclBaseName::createDestructor(), DestructorLoc,
                         /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
                         /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                         /*ThrownType=*/TypeLoc(),
                         /*HasImplicitSelfDecl=*/true,
                         /*GenericParams=*/nullptr),
    SelfDecl(nullptr) {
  setParameters(ParameterList::createEmpty(Parent->getASTContext()));
}

ObjCSelector DestructorDecl::getObjCSelector() const {
  // Deinitializers are always called "dealloc".
  auto &ctx = getASTContext();
  return ObjCSelector(ctx, 0, ctx.Id_dealloc);
}

DestructorDecl *DestructorDecl::getSuperDeinit() const {
  auto declContext = getDeclContext()->getImplementedObjCContext();
  if (auto classDecl = dyn_cast<ClassDecl>(declContext)) {
    if (auto superclass = classDecl->getSuperclassDecl()) {
      return superclass->getDestructor();
    }
  }
  return nullptr;
}

SourceRange FuncDecl::getSourceRange() const {
  SourceLoc StartLoc = getStartLoc();

  if (StartLoc.isInvalid())
    return SourceRange();

  if (getBodyKind() == BodyKind::Unparsed)
    return { StartLoc, BodyRange.End };

  SourceLoc RBraceLoc = getOriginalBodySourceRange().End;
  if (RBraceLoc.isValid()) {
    return { StartLoc, RBraceLoc };
  }

  if (isa<AccessorDecl>(this))
    return StartLoc;

  if (getBodyKind() == BodyKind::Synthesize)
    return SourceRange();

  auto TrailingWhereClauseSourceRange = getGenericTrailingWhereClauseSourceRange();
  if (TrailingWhereClauseSourceRange.isValid())
    return { StartLoc, TrailingWhereClauseSourceRange.End };

  const auto ResultTyEndLoc = getResultTypeSourceRange().End;
  if (ResultTyEndLoc.isValid())
    return { StartLoc, ResultTyEndLoc };

  if (hasThrows())
    return { StartLoc, getThrowsLoc() };

  if (hasAsync())
    return { StartLoc, getAsyncLoc() };

  auto LastParamListEndLoc = getParameters()->getSourceRange().End;
  if (LastParamListEndLoc.isValid())
    return { StartLoc, LastParamListEndLoc };
  return StartLoc;
}

EnumElementDecl::EnumElementDecl(SourceLoc IdentifierLoc, DeclName Name,
                                 ParameterList *Params,
                                 SourceLoc EqualsLoc,
                                 LiteralExpr *RawValueExpr,
                                 DeclContext *DC)
  : DeclContext(DeclContextKind::EnumElementDecl, DC),
    ValueDecl(DeclKind::EnumElement, DC, Name, IdentifierLoc),
    EqualsLoc(EqualsLoc),
    RawValueExpr(RawValueExpr) {
  setParameterList(Params);
}

SourceRange EnumElementDecl::getSourceRange() const {
  if (RawValueExpr && !RawValueExpr->isImplicit())
    return {getStartLoc(), RawValueExpr->getEndLoc()};
  if (auto *PL = getParameterList())
    return {getStartLoc(), PL->getSourceRange().End};
  return {getStartLoc(), getNameLoc()};
}

ArrayRef<AnyFunctionType::Param>
EnumElementDecl::getCaseConstructorParams() const {
  if (!hasAssociatedValues())
    return {};

  auto interfaceType = getInterfaceType();
  if (interfaceType->is<ErrorType>())
    return {};

  auto funcTy = interfaceType->castTo<AnyFunctionType>();
  return funcTy->getResult()->castTo<FunctionType>()->getParams();
}

Type EnumElementDecl::getPayloadInterfaceType() const {
  if (!hasAssociatedValues())
    return Type();

  auto interfaceType = getInterfaceType();
  if (interfaceType->is<ErrorType>())
    return interfaceType;

  // The payload type of an enum is an imploded tuple of the internal arguments
  // of the case constructor. As such, compose a tuple type with the parameter
  // flags dropped.
  return AnyFunctionType::composeTuple(getASTContext(),
                                       getCaseConstructorParams(),
                                       ParameterFlagHandling::IgnoreNonEmpty);
}

void EnumElementDecl::setParameterList(ParameterList *params) {
  Params = params;

  if (params)
    params->setDeclContextOfParamDecls(this);
}

EnumCaseDecl *EnumElementDecl::getParentCase() const {
  for (EnumCaseDecl *EC : getParentEnum()->getAllCases()) {
    ArrayRef<EnumElementDecl *> CaseElements = EC->getElements();
    if (std::find(CaseElements.begin(), CaseElements.end(), this) !=
        CaseElements.end()) {
      return EC;
    }
  }

  llvm_unreachable("enum element not in case of parent enum");
}
      
LiteralExpr *EnumElementDecl::getRawValueExpr() const {
  // The return value of this request is irrelevant - it exists as
  // a cache-warmer.
  (void)evaluateOrDefault(
      getASTContext().evaluator,
      EnumRawValuesRequest{getParentEnum(), TypeResolutionStage::Interface},
      {});
  return RawValueExpr;
}

LiteralExpr *EnumElementDecl::getStructuralRawValueExpr() const {
  // The return value of this request is irrelevant - it exists as
  // a cache-warmer.
  (void)evaluateOrDefault(
      getASTContext().evaluator,
      EnumRawValuesRequest{getParentEnum(), TypeResolutionStage::Structural},
      {});
  return RawValueExpr;
}

void EnumElementDecl::setRawValueExpr(LiteralExpr *e) {
  assert((!RawValueExpr || e == RawValueExpr || e->getType()) &&
         "Illegal mutation of raw value expr");
  RawValueExpr = e;
}

SourceRange ConstructorDecl::getSourceRange() const {
  if (isImplicit())
    return getConstructorLoc();

  SourceLoc End = getOriginalBodySourceRange().End;
  if (End.isInvalid())
    End = getGenericTrailingWhereClauseSourceRange().End;
  if (End.isInvalid())
    End = getThrowsLoc();
  if (End.isInvalid())
    End = getSignatureSourceRange().End;

  return { getConstructorLoc(), End };
}

Type ConstructorDecl::getResultInterfaceType() const {
  Type resultTy;

  auto *dc = getDeclContext();
  if (!dc->isTypeContext())
    resultTy = ErrorType::get(getASTContext());
  else
    resultTy = dc->getSelfInterfaceType();

  // Adjust result type for failability.
  if (isFailable())
    return OptionalType::get(resultTy);

  return resultTy;
}

Type ConstructorDecl::getInitializerInterfaceType() {
  if (InitializerInterfaceType)
    return InitializerInterfaceType;

  // Lazily calculate initializer type.
  auto allocatorTy = getInterfaceType();
  if (!allocatorTy->is<AnyFunctionType>()) {
    InitializerInterfaceType = ErrorType::get(getASTContext());
    return InitializerInterfaceType;
  }

  auto funcTy = allocatorTy->castTo<AnyFunctionType>()->getResult();
  assert(funcTy->is<FunctionType>());

  // Constructors have an initializer type that takes an instance
  // instead of a metatype.
  auto initSelfParam = computeSelfParam(this, /*isInitializingCtor=*/true);

  // FIXME: Verify ExtInfo state is correct, not working by accident.
  AnyFunctionType::ExtInfo info;
  if (initSelfParam.isIsolated())
    info = info.withIsolation(FunctionTypeIsolation::forParameter());

  Type initFuncTy;
  if (auto sig = getGenericSignature()) {
    initFuncTy = GenericFunctionType::get(sig, {initSelfParam}, funcTy, info);
  } else {
    initFuncTy = FunctionType::get({initSelfParam}, funcTy, info);
  }
  InitializerInterfaceType = initFuncTy;

  return InitializerInterfaceType;
}

CtorInitializerKind ConstructorDecl::getInitKind() const {
  const auto *ED =
      dyn_cast_or_null<ExtensionDecl>(getDeclContext()->getAsDecl());
  if (ED && !ED->hasBeenBound()) {
    // When the declaration context is an extension and this is called when the
    // extended nominal hasn't be bound yet, e.g. dumping pre-typechecked AST,
    // there is not enough information about extended nominal to use for
    // computing init kind on InitKindRequest as bindExtensions is done at
    // typechecking, so in that case just look to parsed attribute in init
    // declaration.
    return getAttrs().hasAttribute<ConvenienceAttr>()
               ? CtorInitializerKind::Convenience
               : CtorInitializerKind::Designated;
  }

  return evaluateOrDefault(getASTContext().evaluator,
    InitKindRequest{const_cast<ConstructorDecl *>(this)},
    CtorInitializerKind::Designated);
}

BodyInitKindAndExpr
ConstructorDecl::getDelegatingOrChainedInitKind() const {
  return evaluateOrDefault(getASTContext().evaluator,
    BodyInitKindRequest{const_cast<ConstructorDecl *>(this)},
    BodyInitKindAndExpr());
  assert(hasBody() && "Constructor does not have a definition");
}

void ConstructorDecl::clearCachedDelegatingOrChainedInitKind() {
  getASTContext().evaluator.clearCachedOutput(
    BodyInitKindRequest{const_cast<ConstructorDecl *>(this)});
}

SourceRange DestructorDecl::getSourceRange() const {
  SourceLoc End = getOriginalBodySourceRange().End;
  if (End.isInvalid()) {
    End = getDestructorLoc();
  }

  return { getDestructorLoc(), End };
}

StringRef swift::getAssociativitySpelling(Associativity value) {
  switch (value) {
  case Associativity::None: return "none";
  case Associativity::Left: return "left";
  case Associativity::Right: return "right";
  }
  llvm_unreachable("Unhandled Associativity in switch.");
}

PrecedenceGroupDecl *
PrecedenceGroupDecl::create(DeclContext *dc,
                            SourceLoc precedenceGroupLoc,
                            SourceLoc nameLoc,
                            Identifier name,
                            SourceLoc lbraceLoc,
                            SourceLoc associativityKeywordLoc,
                            SourceLoc associativityValueLoc,
                            Associativity associativity,
                            SourceLoc assignmentKeywordLoc,
                            SourceLoc assignmentValueLoc,
                            bool isAssignment,
                            SourceLoc higherThanLoc,
                            ArrayRef<Relation> higherThan,
                            SourceLoc lowerThanLoc,
                            ArrayRef<Relation> lowerThan,
                            SourceLoc rbraceLoc) {
  void *memory = dc->getASTContext().Allocate(sizeof(PrecedenceGroupDecl) +
                    (higherThan.size() + lowerThan.size()) * sizeof(Relation),
                                              alignof(PrecedenceGroupDecl));
  return new (memory) PrecedenceGroupDecl(dc, precedenceGroupLoc, nameLoc, name,
                                          lbraceLoc, associativityKeywordLoc,
                                          associativityValueLoc, associativity,
                                          assignmentKeywordLoc,
                                          assignmentValueLoc, isAssignment,
                                          higherThanLoc, higherThan,
                                          lowerThanLoc, lowerThan, rbraceLoc);
}

PrecedenceGroupDecl::PrecedenceGroupDecl(DeclContext *dc,
                                         SourceLoc precedenceGroupLoc,
                                         SourceLoc nameLoc,
                                         Identifier name,
                                         SourceLoc lbraceLoc,
                                         SourceLoc associativityKeywordLoc,
                                         SourceLoc associativityValueLoc,
                                         Associativity associativity,
                                         SourceLoc assignmentKeywordLoc,
                                         SourceLoc assignmentValueLoc,
                                         bool isAssignment,
                                         SourceLoc higherThanLoc,
                                         ArrayRef<Relation> higherThan,
                                         SourceLoc lowerThanLoc,
                                         ArrayRef<Relation> lowerThan,
                                         SourceLoc rbraceLoc)
  : Decl(DeclKind::PrecedenceGroup, dc),
    PrecedenceGroupLoc(precedenceGroupLoc), NameLoc(nameLoc),
    LBraceLoc(lbraceLoc), RBraceLoc(rbraceLoc),
    AssociativityKeywordLoc(associativityKeywordLoc),
    AssociativityValueLoc(associativityValueLoc),
    AssignmentKeywordLoc(assignmentKeywordLoc),
    AssignmentValueLoc(assignmentValueLoc),
    HigherThanLoc(higherThanLoc), LowerThanLoc(lowerThanLoc), Name(name),
    NumHigherThan(higherThan.size()), NumLowerThan(lowerThan.size()) {
  Bits.PrecedenceGroupDecl.Associativity = unsigned(associativity);
  Bits.PrecedenceGroupDecl.IsAssignment = isAssignment;
  memcpy(getHigherThanBuffer(), higherThan.data(),
         higherThan.size() * sizeof(Relation));
  memcpy(getLowerThanBuffer(), lowerThan.data(),
         lowerThan.size() * sizeof(Relation));
}

PrecedenceGroupDecl *InfixOperatorDecl::getPrecedenceGroup() const {
  return evaluateOrDefault(
      getASTContext().evaluator,
      OperatorPrecedenceGroupRequest{const_cast<InfixOperatorDecl *>(this)},
      nullptr);
}

bool FuncDecl::isDeferBody() const {
  return getBaseIdentifier() == getASTContext().getIdentifier("$defer");
}

bool FuncDecl::isPotentialIBActionTarget() const {
  return isInstanceMember() &&
    getDeclContext()->getSelfClassDecl() &&
    !isa<AccessorDecl>(this);
}

void FuncDecl::setHasTopLevelLocalContextCaptures(bool hasCaptures) {
  assert(!hasCaptures || isa<SourceFile>(getDeclContext()));
  
  Bits.FuncDecl.HasTopLevelLocalContextCaptures = hasCaptures;
}

Type TypeBase::getSwiftNewtypeUnderlyingType() {
  auto structDecl = getStructOrBoundGenericStruct();
  if (!structDecl)
    return {};

  // Make sure the clang node has swift_newtype attribute
  auto clangNode = structDecl->getClangDecl();
  if (!clangNode || !clangNode->hasAttr<clang::SwiftNewTypeAttr>())
    return {};

  // Underlying type is the type of rawValue
  for (auto member : structDecl->getMembers())
    if (auto varDecl = dyn_cast<VarDecl>(member))
      if (varDecl->getName() == getASTContext().Id_rawValue)
        return varDecl->getInterfaceType();

  return {};
}

const VarDecl *ClassDecl::getUnownedExecutorProperty() const {
  auto &C = getASTContext();

  if (!isAnyActor())
    return nullptr;

  llvm::SmallVector<ValueDecl *, 2> results;
  this->lookupQualified(getSelfNominalTypeDecl(),
                        DeclNameRef(C.Id_unownedExecutor),
                        getLoc(), NL_ProtocolMembers,
                        results);

  for (auto candidate: results) {
    if (isa<ProtocolDecl>(candidate->getDeclContext()))
      continue;

    if (VarDecl *var = dyn_cast<VarDecl>(candidate))
      return var;
  }

  return nullptr;
}

bool ClassDecl::isRootDefaultActor() const {
  return isRootDefaultActor(getModuleContext(), ResilienceExpansion::Maximal);
}

bool ClassDecl::isRootDefaultActor(ModuleDecl *M,
                                   ResilienceExpansion expansion) const {
  if (!isDefaultActor(M, expansion)) return false;
  auto superclass = getSuperclassDecl();
  return (!superclass || superclass->isNSObject());
}

bool ClassDecl::isNonDefaultExplicitDistributedActor() const {
  return isNonDefaultExplicitDistributedActor(getModuleContext(), ResilienceExpansion::Maximal);
}
bool ClassDecl::isNonDefaultExplicitDistributedActor(ModuleDecl *M,
                                                     ResilienceExpansion expansion) const {
  return !isDefaultActor(M, expansion) && isExplicitDistributedActor();
}


bool ClassDecl::isNativeNSObjectSubclass() const {
  // @objc actors implicitly inherit from NSObject.
  if (isActor()) {
    if (getAttrs().hasAttribute<ObjCAttr>()) {
      return true;
    }
    ClassDecl *superclass = getSuperclassDecl();
    return superclass && superclass->isNSObject();
  }

  // For now, non-actor classes cannot use the native NSObject subclass.
  // Eventually we should roll this out to more classes that directly
  // inherit NSObject, but we have to do it with ABI compatibility.
  return false;
}

bool ClassDecl::isNSObject() const {
  if (!getName().is("NSObject")) return false;
  ASTContext &ctx = getASTContext();
  return (getModuleContext()->getName() == ctx.Id_Foundation ||
          getModuleContext()->getName() == ctx.Id_ObjectiveC ||
          getModuleContext()->getName().is("SwiftFoundation"));
}

Type ClassDecl::getSuperclass() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    SuperclassTypeRequest{const_cast<ClassDecl *>(this),
                          TypeResolutionStage::Interface},
    Type());
}

ClassDecl *ClassDecl::getSuperclassDecl() const {
  ASTContext &ctx = getASTContext();
  auto result = evaluateOrDefault(ctx.evaluator,
    SuperclassDeclRequest{const_cast<ClassDecl *>(this)},
    const_cast<ClassDecl *>(this));

  if (result == this)
    return nullptr;

  return result;
}

void ClassDecl::setSuperclass(Type superclass) {
  assert((!superclass || !superclass->hasArchetype())
         && "superclass must be interface type");
  LazySemanticInfo.SuperclassType.setPointerAndInt(superclass, true);
  LazySemanticInfo.SuperclassDecl.setPointerAndInt(
    superclass ? superclass->getClassOrBoundGenericClass() : nullptr,
    true);
}

bool VarDecl::isSelfParamCaptureIsolated() const {
  assert(isSelfParamCapture());

  // Find the "self" parameter that we captured and determine whether
  // it is potentially isolated.
  for (auto dc = getDeclContext(); dc; dc = dc->getParent()) {
    if (auto func = dyn_cast<AbstractFunctionDecl>(dc)) {
      if (auto selfDecl = func->getImplicitSelfDecl()) {
        return selfDecl->isIsolated();
      }

      if (auto capture = func->getCaptureInfo().getIsolatedParamCapture())
        return capture->isSelfParameter() || capture->isSelfParamCapture();
    }

    if (auto closure = dyn_cast<AbstractClosureExpr>(dc)) {
      switch (auto isolation = closure->getActorIsolation()) {
      case ActorIsolation::Unspecified:
      case ActorIsolation::Nonisolated:
      case ActorIsolation::NonisolatedUnsafe:
      case ActorIsolation::GlobalActor:
      case ActorIsolation::Erased:
        return false;

      case ActorIsolation::ActorInstance:
        auto isolatedVar = isolation.getActorInstance();
        return isolatedVar->isSelfParameter() ||
            isolatedVar-isSelfParamCapture();
      }
    }

    if (dc->isModuleScopeContext() || dc->isTypeContext())
      break;
  }

  return false;
}

ActorIsolation swift::getActorIsolation(ValueDecl *value) {
  return getInferredActorIsolation(value).isolation;
}

InferredActorIsolation
swift::getInferredActorIsolation(ValueDecl *value) {
  auto &ctx = value->getASTContext();
  return evaluateOrDefault(
      ctx.evaluator, ActorIsolationRequest{value},
      InferredActorIsolation::forUnspecified());
}

ActorIsolation swift::getActorIsolationOfContext(
    DeclContext *dc,
    llvm::function_ref<ActorIsolation(AbstractClosureExpr *)>
        getClosureActorIsolation) {
  auto &ctx = dc->getASTContext();
  auto dcToUse = dc;
  // Defer bodies share actor isolation of their enclosing context.
  if (auto FD = dyn_cast<FuncDecl>(dcToUse)) {
    if (FD->isDeferBody()) {
      dcToUse = FD->getDeclContext();
    }
  }
  if (auto *vd = dyn_cast_or_null<ValueDecl>(dcToUse->getAsDecl()))
    return getActorIsolation(vd);

  // In the context of the initializing or default-value expression of a
  // stored property:
  //   - For a static stored property, the isolation matches the VarDecl.
  //     Static properties are initialized upon first use, so the isolation
  //     of the initializer must match the isolation required to access the
  //     property.
  //   - For a field of a nominal type, the expression can require the same
  //     actor isolation as the field itself. That default expression may only
  //     be used from inits that meet the required isolation.
  if (auto *var = dcToUse->getNonLocalVarDecl()) {
    // If IsolatedDefaultValues are enabled, treat this context as having
    // unspecified isolation. We'll compute the required isolation for
    // the initializer and validate that it matches the isolation of the
    // var itself in the DefaultInitializerIsolation request.
    if (ctx.LangOpts.hasFeature(Feature::IsolatedDefaultValues))
      return ActorIsolation::forUnspecified();

    return getActorIsolation(var);
  }

  if (auto *closure = dyn_cast<AbstractClosureExpr>(dcToUse)) {
    return getClosureActorIsolation(closure);
  }

  if (auto *tld = dyn_cast<TopLevelCodeDecl>(dcToUse)) {
    if (dcToUse->isAsyncContext() ||
        dcToUse->getASTContext().LangOpts.StrictConcurrencyLevel >=
            StrictConcurrency::Complete) {
      if (Type mainActor = dcToUse->getASTContext().getMainActorType())
        return ActorIsolation::forGlobalActor(mainActor)
            .withPreconcurrency(
                !dcToUse->getASTContext().isSwiftVersionAtLeast(6));
    }
  }

  return ActorIsolation::forUnspecified();
}

bool swift::isSameActorIsolated(ValueDecl *value, DeclContext *dc) {
    auto valueIsolation = getActorIsolation(value);
    auto dcIsolation = getActorIsolationOfContext(dc);
    return valueIsolation.isActorIsolated() && dcIsolation.isActorIsolated() &&
           valueIsolation.getActor() == dcIsolation.getActor();
}

ClangNode Decl::getClangNodeImpl() const {
  assert(Bits.Decl.FromClang);
  void * const *ptr = nullptr;
  switch (getKind()) {
#define DECL(Id, Parent) \
  case DeclKind::Id: \
    ptr = reinterpret_cast<void * const*>(static_cast<const Id##Decl*>(this)); \
    break;
#include "swift/AST/DeclNodes.def"
  }
  return ClangNode::getFromOpaqueValue(*(ptr - 1));
}

void Decl::setClangNode(ClangNode Node) {
  Bits.Decl.FromClang = true;
  // The extra/preface memory is allocated by the importer.
  void **ptr = nullptr;
  switch (getKind()) {
#define DECL(Id, Parent) \
  case DeclKind::Id: \
    ptr = reinterpret_cast<void **>(static_cast<Id##Decl*>(this)); \
    break;
#include "swift/AST/DeclNodes.def"
  }
  *(ptr - 1) = Node.getOpaqueValue();
}

// See swift/Basic/Statistic.h for declaration: this enables tracing Decls, is
// defined here to avoid too much layering violation / circular linkage
// dependency.

struct DeclTraceFormatter : public UnifiedStatsReporter::TraceFormatter {
  void traceName(const void *Entity, raw_ostream &OS) const override {
    if (!Entity)
      return;
    const Decl *D = static_cast<const Decl *>(Entity);
    if (auto const *VD = dyn_cast<const ValueDecl>(D)) {
      VD->getName().print(OS, false);
    } else {
      OS << "<"
         << Decl::getDescriptiveKindName(D->getDescriptiveKind())
         << ">";
    }
  }
  void traceLoc(const void *Entity, SourceManager *SM,
                clang::SourceManager *CSM, raw_ostream &OS) const override {
    if (!Entity)
      return;
    const Decl *D = static_cast<const Decl *>(Entity);
    D->getSourceRange().print(OS, *SM, false);
  }
};

static DeclTraceFormatter TF;

template<>
const UnifiedStatsReporter::TraceFormatter*
FrontendStatsTracer::getTraceFormatter<const Decl *>() {
  return &TF;
}

TypeOrExtensionDecl::TypeOrExtensionDecl(NominalTypeDecl *D) : Decl(D) {}
TypeOrExtensionDecl::TypeOrExtensionDecl(ExtensionDecl *D) : Decl(D) {}

Decl *TypeOrExtensionDecl::getAsDecl() const {
  if (auto NTD = Decl.dyn_cast<NominalTypeDecl *>())
    return NTD;

  return Decl.get<ExtensionDecl *>();
}
DeclContext *TypeOrExtensionDecl::getAsDeclContext() const {
  return getAsDecl()->getInnermostDeclContext();
}

IterableDeclContext *TypeOrExtensionDecl::getAsIterableDeclContext() const {
  if (auto nominal = Decl.dyn_cast<NominalTypeDecl *>())
    return nominal;

  return Decl.get<ExtensionDecl *>();
}

NominalTypeDecl *TypeOrExtensionDecl::getBaseNominal() const {
  return getAsDeclContext()->getSelfNominalTypeDecl();
}
bool TypeOrExtensionDecl::isNull() const { return Decl.isNull(); }

void swift::simple_display(llvm::raw_ostream &out, const Decl *decl) {
  if (!decl) {
    out << "(null)";
    return;
  }

  if (auto value = dyn_cast<ValueDecl>(decl)) {
    return simple_display(out, value);
  } else if (auto ext = dyn_cast<ExtensionDecl>(decl)) {
    out << "extension of ";
    if (auto typeRepr = ext->getExtendedTypeRepr())
      typeRepr->print(out);
    else
      ext->getSelfNominalTypeDecl()->dumpRef(out);
  } else if (auto med = dyn_cast<MacroExpansionDecl>(decl)) {
    out << '#' << med->getMacroName() << " in ";
    printContext(out, med->getDeclContext());
  } else {
    out << "(unknown decl)";
  }
  if (decl->getLoc().isValid()) {
    out << '@';
    decl->getLoc().print(out, decl->getASTContext().SourceMgr);
  }
}

void swift::simple_display(llvm::raw_ostream &out,
                           OptionSet<NominalTypeDecl::LookupDirectFlags> opts) {
  out << "{ ";
  using LookupFlags = NominalTypeDecl::LookupDirectFlags;
  if (opts.contains(LookupFlags::IncludeAttrImplements))
    out << "IncludeAttrImplements";
  out << " }";
}

void swift::simple_display(llvm::raw_ostream &out,
                           TypeOrExtensionDecl container) {
  simple_display(out, container.getAsDecl());
}

void swift::simple_display(llvm::raw_ostream &out, const ValueDecl *decl) {
  if (decl) decl->dumpRef(out);
  else out << "(null)";
}

void swift::simple_display(llvm::raw_ostream &out, const GenericParamList *GPL) {
  if (GPL) GPL->print(out);
  else out << "(null)";
}

StringRef swift::getAccessorLabel(AccessorKind kind) {
  switch (kind) {
  #define SINGLETON_ACCESSOR(ID, KEYWORD) \
    case AccessorKind::ID: return #KEYWORD;
  #define ACCESSOR(ID)
  #include "swift/AST/AccessorKinds.def"
    }
    llvm_unreachable("bad accessor kind");
}

void swift::simple_display(llvm::raw_ostream &out, AccessorKind kind) {
  out << getAccessorLabel(kind);
}

SourceLoc swift::extractNearestSourceLoc(const Decl *decl) {
  auto loc = decl->getLoc(/*SerializedOK=*/false);
  if (loc.isValid())
    return loc;

  return extractNearestSourceLoc(decl->getDeclContext());
}

SourceLoc swift::extractNearestSourceLoc(TypeOrExtensionDecl container) {
  return extractNearestSourceLoc(container.Decl);
}

std::optional<BodyAndFingerprint>
ParseAbstractFunctionBodyRequest::getCachedResult() const {
  using BodyKind = AbstractFunctionDecl::BodyKind;
  auto afd = std::get<0>(getStorage());
  switch (afd->getBodyKind()) {
  case BodyKind::Deserialized:
  case BodyKind::SILSynthesize:
  case BodyKind::None:
    return BodyAndFingerprint{};

  case BodyKind::TypeChecked:
  case BodyKind::Parsed:
    return afd->BodyAndFP;

  case BodyKind::Synthesize:
  case BodyKind::Unparsed:
    return std::nullopt;
  }
  llvm_unreachable("Unhandled BodyKing in switch");
}

void ParseAbstractFunctionBodyRequest::cacheResult(
    BodyAndFingerprint value) const {
  using BodyKind = AbstractFunctionDecl::BodyKind;
  auto afd = std::get<0>(getStorage());
  switch (afd->getBodyKind()) {
  case BodyKind::Deserialized:
  case BodyKind::SILSynthesize:
    // The body is always empty, so don't cache anything.
    assert(!value.getFingerprint().has_value() && value.getBody() == nullptr);
    return;

  case BodyKind::Parsed:
  case BodyKind::TypeChecked:
  case BodyKind::None:
    afd->BodyAndFP = value;
    return;

  case BodyKind::Synthesize:
  case BodyKind::Unparsed:
    llvm_unreachable("evaluate() did not set the body kind");
    return;
  }
}

std::optional<bool> IsFunctionBodySkippedRequest::getCachedResult() const {
  using BodySkippedStatus = AbstractFunctionDecl::BodySkippedStatus;
  auto afd = std::get<0>(getStorage());
  switch (afd->getBodySkippedStatus()) {
  case BodySkippedStatus::Unknown:
    return std::nullopt;
  case BodySkippedStatus::Skipped:
    return true;
  case BodySkippedStatus::NotSkipped:
    return false;
  }
  llvm_unreachable("bad BodySkippedStatus");
}

void IsFunctionBodySkippedRequest::cacheResult(bool isSkipped) const {
  using BodySkippedStatus = AbstractFunctionDecl::BodySkippedStatus;
  auto afd = std::get<0>(getStorage());
  const_cast<AbstractFunctionDecl *>(afd)->setBodySkippedStatus(
      isSkipped ? BodySkippedStatus::Skipped : BodySkippedStatus::NotSkipped);
}

void swift::simple_display(llvm::raw_ostream &out, BodyAndFingerprint value) {
  out << "(";
  simple_display(out, value.getBody());
  out << ", ";
  simple_display(out, value.getFingerprint());
  out << ")";
}

void swift::simple_display(llvm::raw_ostream &out, AnyFunctionRef fn) {
  if (auto func = fn.getAbstractFunctionDecl())
    simple_display(out, func);
  else
    out << "closure";
}

BuiltinTupleDecl::BuiltinTupleDecl(Identifier Name, DeclContext *Parent)
    : NominalTypeDecl(DeclKind::BuiltinTuple, Parent, Name, SourceLoc(),
                      ArrayRef<InheritedEntry>(), nullptr) {}

std::vector<MacroRole> swift::getAllMacroRoles() {
  return {
#define MACRO_ROLE(Name, Description) MacroRole::Name,
#include "swift/Basic/MacroRoles.def"
  };
}

StringRef swift::getMacroRoleString(MacroRole role) {
  switch (role) {
#define MACRO_ROLE(Name, Description) \
  case MacroRole::Name:               \
    return Description;
#include "swift/Basic/MacroRoles.def"
  }
}

std::vector<MacroIntroducedDeclNameKind>
swift::getAllMacroIntroducedDeclNameKinds() {
  return {
      MacroIntroducedDeclNameKind::Named,
      MacroIntroducedDeclNameKind::Overloaded,
      MacroIntroducedDeclNameKind::Prefixed,
      MacroIntroducedDeclNameKind::Suffixed,
      MacroIntroducedDeclNameKind::Arbitrary,
  };
}

bool swift::macroIntroducedNameRequiresArgument(
  MacroIntroducedDeclNameKind kind
) {
  switch (kind) {
  case MacroIntroducedDeclNameKind::Named:
  case MacroIntroducedDeclNameKind::Prefixed:
  case MacroIntroducedDeclNameKind::Suffixed:
    return true;

  case MacroIntroducedDeclNameKind::Overloaded:
  case MacroIntroducedDeclNameKind::Arbitrary:
    return false;
  }
}

StringRef swift::getMacroIntroducedDeclNameString(
    MacroIntroducedDeclNameKind kind) {
  switch (kind) {
  case MacroIntroducedDeclNameKind::Named:
    return "named";

  case MacroIntroducedDeclNameKind::Overloaded:
    return "overloaded";

  case MacroIntroducedDeclNameKind::Prefixed:
    return "prefixed";

  case MacroIntroducedDeclNameKind::Suffixed:
    return "suffixed";

  case MacroIntroducedDeclNameKind::Arbitrary:
    return "arbitrary";
  }
}

static MacroRoles freestandingMacroRoles =
  (MacroRoles()
#define FREESTANDING_MACRO_ROLE(Name, Description) | MacroRole::Name
#define ATTACHED_MACRO_ROLE(Name, Description, MangledChar)
#include "swift/Basic/MacroRoles.def"
   );
static MacroRoles attachedMacroRoles = 
  (MacroRoles()
#define ATTACHED_MACRO_ROLE(Name, Description, MangledChar) | MacroRole::Name
#define FREESTANDING_MACRO_ROLE(Name, Description)
#include "swift/Basic/MacroRoles.def"
   );

bool swift::isFreestandingMacro(MacroRoles contexts) {
  return bool(contexts & freestandingMacroRoles);
}

MacroRoles swift::getFreestandingMacroRoles() {
  return freestandingMacroRoles;
}

bool swift::isAttachedMacro(MacroRoles contexts) {
  return bool(contexts & attachedMacroRoles);
}

MacroRoles swift::getAttachedMacroRoles() {
  return attachedMacroRoles;
}

bool swift::isMacroSupported(MacroRole role, ASTContext &ctx) {
  switch (role) {
#define EXPERIMENTAL_ATTACHED_MACRO_ROLE(Name, Description, MangledChar, FeatureName) \
  case MacroRole::Name: \
    return ctx.LangOpts.hasFeature(Feature::FeatureName);

#define EXPERIMENTAL_FREESTANDING_MACRO_ROLE(Name, Description, FeatureName) \
  case MacroRole::Name: return ctx.LangOpts.hasFeature(Feature::FeatureName);

#define MACRO_ROLE(Name, Description)

#include "swift/Basic/MacroRoles.def"

#define EXPERIMENTAL_ATTACHED_MACRO_ROLE(Name, Description, MangledChar, FeatureName)
#define EXPERIMENTAL_FREESTANDING_MACRO_ROLE(Name, Description, FeatureName)
#define MACRO_ROLE(Name, Description) case MacroRole::Name:
#include "swift/Basic/MacroRoles.def"
    return true;
  }
}

void MissingDecl::forEachMacroExpandedDecl(MacroExpandedDeclCallback callback) {
  auto macroRef = unexpandedMacro.macroRef;
  auto *baseDecl = unexpandedMacro.baseDecl;

  // If the macro itself is a macro expansion expression, it should come with
  // a top-level code declaration that we can use for resolution. For such
  // cases, resolve the macro to determine whether it is a declaration or
  // code-item macro, meaning that it can produce declarations. In such cases,
  // expand the macro and use its substituted declaration (a MacroExpansionDecl)
  // instead.
  if (auto freestanding = macroRef.dyn_cast<FreestandingMacroExpansion *>()) {
    if (auto expr = dyn_cast<MacroExpansionExpr>(freestanding)) {
      bool replacedWithDecl = false;
      if (auto tlcd = dyn_cast_or_null<TopLevelCodeDecl>(baseDecl)) {
        ASTContext &ctx = tlcd->getASTContext();
        if (auto macro = evaluateOrDefault(
                ctx.evaluator,
                ResolveMacroRequest{macroRef, tlcd->getDeclContext()},
                nullptr)) {
          auto macroDecl = cast<MacroDecl>(macro.getDecl());
          auto roles = macroDecl->getMacroRoles();
          if (roles.contains(MacroRole::Declaration) ||
              roles.contains(MacroRole::CodeItem)) {
            (void)evaluateOrDefault(ctx.evaluator,
                                    ExpandMacroExpansionExprRequest{expr},
                                    std::nullopt);
            if (auto substituted = expr->getSubstituteDecl()) {
              macroRef = substituted;
              baseDecl = substituted;
              replacedWithDecl = true;
            }
          }
        }
      }

      // If we didn't end up replacing the macro expansion expression with
      // a declaration, we're done.
      if (!replacedWithDecl)
        return;
    }
  }

  if (!macroRef || !baseDecl)
    return;
  auto *module = getModuleContext();

  baseDecl->visitAuxiliaryDecls([&](Decl *auxiliaryDecl) {
    SourceFile *sf = auxiliaryDecl->getLoc()
        ? module->getSourceFileContainingLocation(auxiliaryDecl->getLoc())
        : auxiliaryDecl->getInnermostDeclContext()->getParentSourceFile();
    // We only visit auxiliary decls that are macro expansions associated with
    // this macro reference.
    if (auto *med = macroRef.dyn_cast<FreestandingMacroExpansion *>()) {
      auto medAsDecl = dyn_cast<MacroExpansionDecl>(med);
      auto medAsExpr = dyn_cast<MacroExpansionExpr>(med);
      if ((!medAsDecl || medAsDecl != sf->getMacroExpansion().dyn_cast<Decl *>()) &&
          (!medAsExpr || medAsExpr != sf->getMacroExpansion().dyn_cast<Expr *>()))
        return;
    } else if (auto *attr = macroRef.dyn_cast<CustomAttr *>()) {
     if (attr != sf->getAttachedMacroAttribute())
       return;
    } else {
      return;
    }
    if (auto *vd = dyn_cast<ValueDecl>(auxiliaryDecl))
      callback(vd);
  });
}

MacroDecl::MacroDecl(
    SourceLoc macroLoc, DeclName name, SourceLoc nameLoc,
    GenericParamList *genericParams,
    ParameterList *parameterList,
    SourceLoc arrowLoc,
    TypeRepr *resultType,
    Expr *definition,
    DeclContext *parent
) : GenericContext(DeclContextKind::MacroDecl, parent, genericParams),
    ValueDecl(DeclKind::Macro, parent, name, nameLoc),
    macroLoc(macroLoc), parameterList(parameterList),
    arrowLoc(arrowLoc),
    resultType(resultType),
    definition(definition) {

  if (parameterList)
    parameterList->setDeclContextOfParamDecls(this);
}

Type MacroDecl::getResultInterfaceType() const {
  auto &ctx = getASTContext();
  auto mutableThis = const_cast<MacroDecl *>(this);
  if (auto type = evaluateOrDefault(ctx.evaluator,
                           ResultTypeRequest{mutableThis},
                           Type()))
    return type;
  return ErrorType::get(ctx);
}

SourceRange MacroDecl::getSourceRange() const {
  SourceLoc endLoc = getNameLoc();
  if (parameterList)
    endLoc = parameterList->getEndLoc();
  if (resultType.getSourceRange().isValid())
    endLoc = resultType.getSourceRange().End;
  if (definition)
    endLoc = definition->getEndLoc();
  if (auto trailing = getTrailingWhereClause())
    endLoc = trailing->getSourceRange().End;
  return SourceRange(macroLoc, endLoc);
}

MacroRoles MacroDecl::getMacroRoles() const {
  MacroRoles contexts = std::nullopt;
  for (auto attr : getAttrs().getAttributes<MacroRoleAttr>())
    contexts |= attr->getMacroRole();
  return contexts;
}

const MacroRoleAttr *MacroDecl::getMacroRoleAttr(MacroRole role) const {
  for (auto attr : getAttrs().getAttributes<MacroRoleAttr>())
    if (attr->getMacroRole() == role)
      return attr;

  return nullptr;
}

DeclName MacroDecl::getUniqueNamePlaceholder(ASTContext &ctx) {
  return ctx.getIdentifier("$");
}

bool MacroDecl::isUniqueNamePlaceholder(DeclName name) {
  return name.getBaseName().userFacingName() == "$";
}

bool MacroDecl::isUniqueMacroName(StringRef name) {
  // Unique macro names are mangled names, which always start with "$s".
  if (!name.starts_with("$s"))
    return false;

  // Unique macro names end with fMu<digits>_. Match that.

  // Strip off the trailing _.
  if (name.back() != '_')
    return false;
  name = name.drop_back();

  // Strip off trailing digits. This is the discriminator.
  while (isdigit(name.back()))
    name = name.drop_back();

  // Check for fMu.
  return name.ends_with("fMu");
}

bool MacroDecl::isUniqueMacroName(DeclBaseName name) {
  return isUniqueMacroName(name.userFacingName());
}


void MacroDecl::getIntroducedNames(MacroRole role, ValueDecl *attachedTo,
                                   SmallVectorImpl<DeclName> &names) const {
  ASTContext &ctx = getASTContext();
  auto *attr = getMacroRoleAttr(role);
  if (!attr)
    return;

  for (auto expandedName : attr->getNames()) {
    switch (expandedName.getKind()) {
    case MacroIntroducedDeclNameKind::Named: {
      names.push_back(DeclName(expandedName.getName()));
      break;
    }

    case MacroIntroducedDeclNameKind::Overloaded: {
      if (!attachedTo)
        break;

      names.push_back(attachedTo->getBaseName());
      break;
    }

    case MacroIntroducedDeclNameKind::Prefixed: {
      if (!attachedTo)
        break;

      auto baseName = attachedTo->getBaseName();
      std::string prefixedName;
      {
        llvm::raw_string_ostream out(prefixedName);
        out << expandedName.getName();
        out << baseName.getIdentifier();
      }

      Identifier nameId = ctx.getIdentifier(prefixedName);
      names.push_back(DeclName(nameId));
      break;
    }

    case MacroIntroducedDeclNameKind::Suffixed: {
      if (!attachedTo)
        break;

      auto baseName = attachedTo->getBaseName();
      std::string suffixedName;
      {
        llvm::raw_string_ostream out(suffixedName);
        out << baseName.getIdentifier();
        out << expandedName.getName();
      }

      Identifier nameId = ctx.getIdentifier(suffixedName);
      names.push_back(DeclName(nameId));
      break;
    }

    case MacroIntroducedDeclNameKind::Arbitrary:
      names.push_back(MacroDecl::getArbitraryName());
      break;
    }
  }

  // Add the unique name, if the macro can introduce declarations anywhere.
  switch (role) {
  case MacroRole::Expression:
  case MacroRole::Declaration:
  case MacroRole::Member:
  case MacroRole::Peer:
  case MacroRole::CodeItem:
  case MacroRole::Extension:
    names.push_back(MacroDecl::getUniqueNamePlaceholder(getASTContext()));
    break;

  case MacroRole::Accessor:
  case MacroRole::Conformance:
  case MacroRole::MemberAttribute:
  case MacroRole::Preamble:
  case MacroRole::Body:
    break;
  }
}

void MacroDecl::getIntroducedConformances(
    NominalTypeDecl *attachedTo,
    MacroRole role,
    SmallVectorImpl<ProtocolDecl *> &conformances) const {
  auto *attr = getMacroRoleAttr(role);
  if (!attr)
    return;

  auto &ctx = getASTContext();
  auto constraintTypes = evaluateOrDefault(
      ctx.evaluator,
      ResolveMacroConformances{attr, this},
      {});

  for (auto constraint : constraintTypes) {
    assert(constraint->isConstraintType());

    std::function<void(Type)> addConstraint =
        [&](Type constraint) -> void {
          if (auto *proto = constraint->getAs<ParameterizedProtocolType>()) {
            conformances.push_back(proto->getProtocol());
            return;
          } else if (auto *proto = constraint->getAs<ProtocolType>()) {
            conformances.push_back(proto->getDecl());
            return;
          }

          auto *composition =
              constraint->castTo<ProtocolCompositionType>();
          for (auto constraint : composition->getMembers()) {
            addConstraint(constraint);
          }
        };

    addConstraint(constraint);
  }
}

MacroDefinition MacroDecl::getDefinition() const {
  return evaluateOrDefault(
      getASTContext().evaluator,
      MacroDefinitionRequest{const_cast<MacroDecl *>(this)},
      MacroDefinition::forUndefined());
}

void MacroDecl::setDefinition(MacroDefinition definition) {
  getASTContext().evaluator.cacheOutput(MacroDefinitionRequest{this},
                                        std::move(definition));
}

std::optional<BuiltinMacroKind> MacroDecl::getBuiltinKind() const {
  auto def = getDefinition();
  if (def.kind != MacroDefinition::Kind::Builtin)
    return std::nullopt;
  return def.getBuiltinKind();
}

MacroDefinition MacroDefinition::forExpanded(
    ASTContext &ctx,
    StringRef expansionText,
    ArrayRef<ExpandedMacroReplacement> replacements,
    ArrayRef<ExpandedMacroReplacement> genericReplacements
) {
  return ExpandedMacroDefinition{ctx.AllocateCopy(expansionText),
                                 ctx.AllocateCopy(replacements),
                                 ctx.AllocateCopy(genericReplacements)};
}

MacroExpansionDecl::MacroExpansionDecl(DeclContext *dc,
                                       MacroExpansionInfo *info)
    : Decl(DeclKind::MacroExpansion, dc),
      FreestandingMacroExpansion(FreestandingMacroKind::Decl, info) {
}

MacroExpansionDecl *
MacroExpansionDecl::create(
    DeclContext *dc, SourceLoc poundLoc, DeclNameRef macro,
    DeclNameLoc macroLoc, SourceLoc leftAngleLoc,
    ArrayRef<TypeRepr *> genericArgs, SourceLoc rightAngleLoc,
    ArgumentList *args
) {
  ASTContext &ctx = dc->getASTContext();
  MacroExpansionInfo *info = new (ctx)
      MacroExpansionInfo{poundLoc,
                         /*moduleName*/ DeclNameRef(),
                         /*moduleNameLoc*/ DeclNameLoc(),
                         macro,
                         macroLoc,
                         leftAngleLoc,
                         rightAngleLoc,
                         genericArgs,
                         args ? args : ArgumentList::createImplicit(ctx, {})};
  return new (ctx) MacroExpansionDecl(dc, info);
}

void MacroExpansionDecl::forEachExpandedNode(
    llvm::function_ref<void(ASTNode)> callback
) const {
  auto mutableThis = const_cast<MacroExpansionDecl *>(this);
  auto bufferID = evaluateOrDefault(
      getASTContext().evaluator,
      ExpandMacroExpansionDeclRequest{mutableThis}, {});
  auto &sourceMgr = getASTContext().SourceMgr;
  auto *moduleDecl = getModuleContext();
  if (!bufferID)
    return;
  auto startLoc = sourceMgr.getLocForBufferStart(*bufferID);
  auto *sourceFile = moduleDecl->getSourceFileContainingLocation(startLoc);

  auto *macro = dyn_cast<MacroDecl>(getMacroRef().getDecl());
  auto roles = macro->getMacroRoles();

  for (auto node : sourceFile->getTopLevelItems()) {
    // The assumption here is that macros can only have a single
    // freestanding macro role. Expression macros can only produce
    // expressions, declaration macros can only produce declarations,
    // and code item macros can produce expressions, declarations, and
    // statements.
    if (roles.contains(MacroRole::Expression) && !node.is<Expr *>())
      continue;

    if (roles.contains(MacroRole::Declaration) && !node.is<Decl *>())
      continue;

    callback(node);
  }
}

/// Adjust the declaration context to find a point in the context hierarchy
/// that the macro can be anchored on.
DeclContext *
MacroDiscriminatorContext::getInnermostMacroContext(DeclContext *dc) {
  switch (dc->getContextKind()) {
  case DeclContextKind::SubscriptDecl:
    // For a subscript, return its parent context.
    return getInnermostMacroContext(dc->getParent());

  case DeclContextKind::EnumElementDecl:
  case DeclContextKind::AbstractFunctionDecl:
  case DeclContextKind::SerializedAbstractClosure:
  case DeclContextKind::SerializedTopLevelCodeDecl:
  case DeclContextKind::Package:
  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
  case DeclContextKind::GenericTypeDecl:
  case DeclContextKind::ExtensionDecl:
  case DeclContextKind::MacroDecl:
    // These contexts are always fine
    return dc;

  case DeclContextKind::TopLevelCodeDecl:
    // For top-level code, use the enclosing source file as the context.
    return getInnermostMacroContext(dc->getParent());

  case DeclContextKind::AbstractClosureExpr: {
    // For closures, we can mangle the closure if we're in a context we can
    // mangle. Check that context.
    auto adjustedParentDC = getInnermostMacroContext(dc->getParent());
    if (adjustedParentDC == dc->getParent())
      return dc;

    return adjustedParentDC;
  }

  case DeclContextKind::Initializer:
    // Initializers can be part of inferring types for variables, so we need
    // their context.
    return getInnermostMacroContext(dc->getParent());
  }
}

/// Retrieve the parent discriminator context for the given macro.
MacroDiscriminatorContext MacroDiscriminatorContext::getParentOf(
    SourceLoc loc, DeclContext *origDC) {
  origDC = getInnermostMacroContext(origDC);

  if (loc.isInvalid())
    return origDC;

  ASTContext &ctx = origDC->getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;

  auto bufferID = sourceMgr.findBufferContainingLoc(loc);
  auto generatedSourceInfo = sourceMgr.getGeneratedSourceInfo(bufferID);
  if (!generatedSourceInfo)
    return origDC;

  switch (generatedSourceInfo->kind) {
  // Attached macros
#define FREESTANDING_MACRO_ROLE(Name, Description)  \
  case GeneratedSourceInfo::Name##MacroExpansion:
#define ATTACHED_MACRO_ROLE(Name, Description, MangledChar)
#include "swift/Basic/MacroRoles.def"
  {
    auto node = ASTNode::getFromOpaqueValue(generatedSourceInfo->astNode);
    if (auto expansion = cast_or_null<MacroExpansionExpr>(
                             node.dyn_cast<Expr *>())) {
      if (!origDC->isChildContextOf(expansion->getDeclContext()))
        return MacroDiscriminatorContext(expansion);
    } else {
      auto expansionDecl = cast<MacroExpansionDecl>(node.get<Decl *>());
      if (!origDC->isChildContextOf(expansionDecl->getDeclContext()))
        return MacroDiscriminatorContext(expansionDecl);
    }

    return origDC;
  }

  // Attached macros
#define FREESTANDING_MACRO_ROLE(Name, Description)
#define ATTACHED_MACRO_ROLE(Name, Description, MangledChar)      \
  case GeneratedSourceInfo::Name##MacroExpansion:
#include "swift/Basic/MacroRoles.def"
  case GeneratedSourceInfo::PrettyPrinted:
  case GeneratedSourceInfo::ReplacedFunctionBody:
  case GeneratedSourceInfo::DefaultArgument:
    return origDC;
  }
}

MacroDiscriminatorContext
MacroDiscriminatorContext::getParentOf(FreestandingMacroExpansion *expansion) {
  return getParentOf(
      expansion->getPoundLoc(), expansion->getDeclContext());
}

std::optional<Type>
CatchNode::getThrownErrorTypeInContext(ASTContext &ctx) const {
  if (auto func = dyn_cast<AbstractFunctionDecl *>()) {
    if (auto thrownError = func->getEffectiveThrownErrorType())
      return func->mapTypeIntoContext(*thrownError);

    return std::nullopt;
  }

  if (auto closure = dyn_cast<ClosureExpr *>()) {
    if (closure->getType())
      return closure->getEffectiveThrownType();

    if (Type thrownType = closure->getExplicitThrownType()) {
      if (thrownType->isNever())
        return std::nullopt;

      return thrownType;
    }

    return std::nullopt;
  }

  if (auto doCatch = dyn_cast<DoCatchStmt *>()) {
    if (auto thrownError = doCatch->getCaughtErrorType()) {
      if (thrownError->isNever())
        return std::nullopt;

      return thrownError;
    }

    // If we haven't computed the error type yet, return 'any Error'.
    return ctx.getErrorExistentialType();
  }

  auto tryExpr = get<AnyTryExpr *>();
  if (auto forceTry = llvm::dyn_cast<ForceTryExpr>(tryExpr)) {
    if (auto thrownError = forceTry->getThrownError())
      return thrownError;

    // If we haven't computed the error type yet, return 'any Error'.
    return ctx.getErrorExistentialType();
  }

  if (auto optTry = llvm::dyn_cast<OptionalTryExpr>(tryExpr)) {
    if (auto thrownError = optTry->getThrownError())
      return thrownError;

    // If we haven't computed the error type yet, return 'any Error'.
    return ctx.getErrorExistentialType();
  }

  llvm_unreachable("Unhandled catch node kind");
}

Type CatchNode::getExplicitCaughtType(ASTContext &ctx) const {
  return evaluateOrDefault(
      ctx.evaluator, ExplicitCaughtTypeRequest{&ctx, *this}, Type());
}

void swift::simple_display(llvm::raw_ostream &out, CatchNode catchNode) {
  out << "catch node";
}

SourceLoc swift::extractNearestSourceLoc(CatchNode catchNode) {
  if (auto func = catchNode.dyn_cast<AbstractFunctionDecl *>())
    return func->getLoc();
  if (auto closure = catchNode.dyn_cast<ClosureExpr *>())
    return closure->getLoc();
  if (auto doCatch = catchNode.dyn_cast<DoCatchStmt *>())
    return doCatch->getDoLoc();
  if (auto tryExpr = catchNode.dyn_cast<AnyTryExpr *>())
    return tryExpr->getTryLoc();
  llvm_unreachable("Unhandled catch node");
}

//----------------------------------------------------------------------------//
// ExplicitCaughtTypeRequest computation.
//----------------------------------------------------------------------------//
bool ExplicitCaughtTypeRequest::isCached() const {
  auto catchNode = std::get<1>(getStorage());

  // try? and try! never need to be cached.
  if (catchNode.is<AnyTryExpr *>())
    return false;

  // Functions with explicitly-written thrown types need the result cached.
  if (auto func = catchNode.dyn_cast<AbstractFunctionDecl *>()) {
    return func->ThrownType.getTypeRepr() != nullptr;
  }

  // Closures with explicitly-written thrown types need the result cached.
  if (auto closure = catchNode.dyn_cast<ClosureExpr *>()) {
    return closure->ThrownType != nullptr;
  }

  // Do..catch with explicitly-written thrown types need the result cached.
  if (auto doCatch = catchNode.dyn_cast<DoCatchStmt *>()) {
    return doCatch->getThrowsLoc().isValid();
  }

  llvm_unreachable("Unhandled catch node");
}

std::optional<Type> ExplicitCaughtTypeRequest::getCachedResult() const {
  // Map a possibly-null Type to std::optional<Type>.
  auto nonnullTypeOrNone = [](Type type) -> std::optional<Type> {
    if (type.isNull())
      return std::nullopt;

    return type;
  };

  auto catchNode = std::get<1>(getStorage());

  if (auto func = catchNode.dyn_cast<AbstractFunctionDecl *>()) {
    return nonnullTypeOrNone(func->ThrownType.getType());
  }

  if (auto closure = catchNode.dyn_cast<ClosureExpr *>()) {
    if (closure->ThrownType) {
      return nonnullTypeOrNone(closure->ThrownType->getInstanceType());
    }

    return std::nullopt;
  }

  if (auto doCatch = catchNode.dyn_cast<DoCatchStmt *>()) {
    return nonnullTypeOrNone(doCatch->ThrownType.getType());
  }

  llvm_unreachable("Unhandled catch node");
}

void ExplicitCaughtTypeRequest::cacheResult(Type type) const {
  auto catchNode = std::get<1>(getStorage());

  if (auto func = catchNode.dyn_cast<AbstractFunctionDecl *>()) {
    func->ThrownType.setType(type);
    return;
  }

  if (auto closure = catchNode.dyn_cast<ClosureExpr *>()) {
    if (closure->ThrownType)
      closure->ThrownType->setType(MetatypeType::get(type));
    else
      closure->ThrownType =
          TypeExpr::createImplicit(type, type->getASTContext());
    return;
  }

  if (auto doCatch = catchNode.dyn_cast<DoCatchStmt *>()) {
    doCatch->ThrownType.setType(type);
    return;
  }

  llvm_unreachable("Unhandled catch node");
}
