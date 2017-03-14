//===--- Decl.cpp - Swift Language Decl ASTs ------------------------------===//
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
//  This file implements the Decl class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/AccessScope.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeLoc.h"
#include "swift/AST/SwiftNameTranslation.h"
#include "clang/Lex/MacroInfo.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/StringExtras.h"

#include "clang/Basic/CharInfo.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"

#include <algorithm>

using namespace swift;

#define DEBUG_TYPE "Serialization"

STATISTIC(NumLazyGenericEnvironments,
          "# of lazily-deserialized generic environments known");
STATISTIC(NumLazyGenericEnvironmentsLoaded,
          "# of lazily-deserialized generic environments loaded");

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

// Only allow allocation of Decls using the allocator in ASTContext.
void *Decl::operator new(size_t Bytes, const ASTContext &C,
                         unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

// Only allow allocation of Modules using the allocator in ASTContext.
void *ModuleDecl::operator new(size_t Bytes, const ASTContext &C,
                           unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
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
  TRIVIAL_KIND(IfConfig);
  TRIVIAL_KIND(PatternBinding);
  TRIVIAL_KIND(PrecedenceGroup);
  TRIVIAL_KIND(InfixOperator);
  TRIVIAL_KIND(PrefixOperator);
  TRIVIAL_KIND(PostfixOperator);
  TRIVIAL_KIND(TypeAlias);
  TRIVIAL_KIND(GenericTypeParam);
  TRIVIAL_KIND(AssociatedType);
  TRIVIAL_KIND(Protocol);
  TRIVIAL_KIND(Subscript);
  TRIVIAL_KIND(Constructor);
  TRIVIAL_KIND(Destructor);
  TRIVIAL_KIND(EnumElement);
  TRIVIAL_KIND(Param);
  TRIVIAL_KIND(Module);

   case DeclKind::Enum:
     return cast<EnumDecl>(this)->getGenericParams()
              ? DescriptiveDeclKind::GenericEnum
              : DescriptiveDeclKind::Enum;

   case DeclKind::Struct:
     return cast<StructDecl>(this)->getGenericParams()
              ? DescriptiveDeclKind::GenericStruct
              : DescriptiveDeclKind::Struct;

   case DeclKind::Class:
     return cast<ClassDecl>(this)->getGenericParams()
              ? DescriptiveDeclKind::GenericClass
              : DescriptiveDeclKind::Class;

   case DeclKind::Var: {
     auto var = cast<VarDecl>(this);
     switch (var->getCorrectStaticSpelling()) {
     case StaticSpellingKind::None:
       return var->isLet()? DescriptiveDeclKind::Let
                          : DescriptiveDeclKind::Var;
     case StaticSpellingKind::KeywordStatic:
       return var->isLet()? DescriptiveDeclKind::StaticLet
                          : DescriptiveDeclKind::StaticVar;
     case StaticSpellingKind::KeywordClass:
       return var->isLet()? DescriptiveDeclKind::ClassLet
                          : DescriptiveDeclKind::ClassVar;
     }
   }

   case DeclKind::Func: {
     auto func = cast<FuncDecl>(this);

     // First, check for an accessor.
     switch (func->getAccessorKind()) {
     case AccessorKind::NotAccessor:
       // Other classifications below.
       break;

     case AccessorKind::IsGetter:
       return DescriptiveDeclKind::Getter;

     case AccessorKind::IsSetter:
       return DescriptiveDeclKind::Setter;

     case AccessorKind::IsWillSet:
       return DescriptiveDeclKind::WillSet;

     case AccessorKind::IsDidSet:
       return DescriptiveDeclKind::DidSet;

     case AccessorKind::IsAddressor:
       return DescriptiveDeclKind::Addressor;

     case AccessorKind::IsMutableAddressor:
       return DescriptiveDeclKind::MutableAddressor;

     case AccessorKind::IsMaterializeForSet:
       return DescriptiveDeclKind::MaterializeForSet;
     }

     if (func->isOperator())
       return DescriptiveDeclKind::OperatorFunction;

     if (func->getDeclContext()->isLocalContext())
       return DescriptiveDeclKind::LocalFunction;

     if (func->getDeclContext()->isModuleScopeContext())
       return DescriptiveDeclKind::GlobalFunction;

     // We have a method.
     switch (func->getCorrectStaticSpelling()) {
     case StaticSpellingKind::None:
       return DescriptiveDeclKind::Method;
     case StaticSpellingKind::KeywordStatic:
       return DescriptiveDeclKind::StaticMethod;
     case StaticSpellingKind::KeywordClass:
       return DescriptiveDeclKind::ClassMethod;
     }
   }
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
  ENTRY(IfConfig, "conditional block");
  ENTRY(PatternBinding, "pattern binding");
  ENTRY(Var, "var");
  ENTRY(Param, "parameter");
  ENTRY(Let, "let");
  ENTRY(StaticVar, "static var");
  ENTRY(StaticLet, "static let");
  ENTRY(ClassVar, "class var");
  ENTRY(ClassLet, "class let");
  ENTRY(PrecedenceGroup, "precedence group");
  ENTRY(InfixOperator, "infix operator");
  ENTRY(PrefixOperator, "prefix operator");
  ENTRY(PostfixOperator, "postfix operator");
  ENTRY(TypeAlias, "type alias");
  ENTRY(GenericTypeParam, "generic parameter");
  ENTRY(AssociatedType, "associated type");
  ENTRY(Enum, "enum");
  ENTRY(Struct, "struct");
  ENTRY(Class, "class");
  ENTRY(Protocol, "protocol");
  ENTRY(GenericEnum, "generic enum");
  ENTRY(GenericStruct, "generic struct");
  ENTRY(GenericClass, "generic class");
  ENTRY(Subscript, "subscript");
  ENTRY(Constructor, "initializer");
  ENTRY(Destructor, "deinitializer");
  ENTRY(LocalFunction, "local function");
  ENTRY(GlobalFunction, "global function");
  ENTRY(OperatorFunction, "operator function");
  ENTRY(Method, "instance method");
  ENTRY(StaticMethod, "static method");
  ENTRY(ClassMethod, "class method");
  ENTRY(Getter, "getter");
  ENTRY(Setter, "setter");
  ENTRY(WillSet, "willSet observer");
  ENTRY(DidSet, "didSet observer");
  ENTRY(MaterializeForSet, "materializeForSet accessor");
  ENTRY(Addressor, "address accessor");
  ENTRY(MutableAddressor, "mutableAddress accessor");
  ENTRY(EnumElement, "enum element");
  ENTRY(Module, "module");
  }
#undef ENTRY
  llvm_unreachable("bad DescriptiveDeclKind");
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

  return getDeclContext();
}

DeclContext *Decl::getDeclContextForModule() const {
  if (auto module = dyn_cast<ModuleDecl>(this))
    return const_cast<ModuleDecl *>(module);

  return nullptr;
}

void Decl::setDeclContext(DeclContext *DC) { 
  Context = DC;
}

bool Decl::isUserAccessible() const {
  if (auto VD = dyn_cast<VarDecl>(this)) {
    return VD->isUserAccessible();
  }
  return true;
}

bool Decl::canHaveComment() const {
  return !this->hasClangNode() &&
         (isa<ValueDecl>(this) || isa<ExtensionDecl>(this)) &&
         !isa<ParamDecl>(this) &&
         (!isa<AbstractTypeParamDecl>(this) || isa<AssociatedTypeDecl>(this));
}

ModuleDecl *Decl::getModuleContext() const {
  return getDeclContext()->getParentModule();
}

// Helper functions to verify statically whether source-location
// functions have been overridden.
typedef const char (&TwoChars)[2];
template<typename Class> 
inline char checkSourceLocType(SourceLoc (Class::*)() const);
inline TwoChars checkSourceLocType(SourceLoc (Decl::*)() const);

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

SourceLoc Decl::getLoc() const {
  switch (getKind()) {
#define DECL(ID, X) \
static_assert(sizeof(checkSourceLocType(&ID##Decl::getLoc)) == 1, \
              #ID "Decl is missing getLoc()"); \
case DeclKind::ID: return cast<ID##Decl>(this)->getLoc();
#include "swift/AST/DeclNodes.def"
  }

  llvm_unreachable("Unknown decl kind");
}

bool AbstractStorageDecl::isTransparent() const {
  return getAttrs().hasAttribute<TransparentAttr>();
}

bool AbstractFunctionDecl::isTransparent() const {
  // Check if the declaration had the attribute.
  if (getAttrs().hasAttribute<TransparentAttr>())
    return true;

  // If this is an accessor, check if the transparent attribute was set
  // on the value decl.
  if (const FuncDecl *FD = dyn_cast<FuncDecl>(this)) {
    if (auto *ASD = FD->getAccessorStorageDecl())
      return ASD->isTransparent();
  }

  return false;
}

bool Decl::isPrivateStdlibDecl(bool whitelistProtocols) const {
  const Decl *D = this;
  if (auto ExtD = dyn_cast<ExtensionDecl>(D))
    return ExtD->getExtendedType().isPrivateStdlibType(whitelistProtocols);

  DeclContext *DC = D->getDeclContext()->getModuleScopeContext();
  if (DC->getParentModule()->isBuiltinModule() ||
      DC->getParentModule()->isSwiftShimsModule())
    return true;
  if (!DC->getParentModule()->isSystemModule())
    return false;
  auto FU = dyn_cast<FileUnit>(DC);
  if (!FU)
    return false;
  // Check for Swift module and overlays.
  if (!DC->getParentModule()->isStdlibModule() &&
      FU->getKind() != FileUnitKind::SerializedAST)
    return false;

  auto hasInternalParameter = [](const ParameterList *params) -> bool {
    for (auto param : *params) {
      if (param->hasName() && param->getNameStr().startswith("_"))
        return true;
      auto argName = param->getArgumentName();
      if (!argName.empty() && argName.str().startswith("_"))
        return true;
    }
    return false;
  };

  if (auto AFD = dyn_cast<AbstractFunctionDecl>(D)) {
    // Hide '~>' functions (but show the operator, because it defines
    // precedence).
    if (AFD->getNameStr() == "~>")
      return true;

    // If it's a function with a parameter with leading underscore, it's a
    // private function.
    for (auto *PL : AFD->getParameterLists())
      if (hasInternalParameter(PL))
        return true;
  }

  if (auto SubscriptD = dyn_cast<SubscriptDecl>(D)) {
    if (hasInternalParameter(SubscriptD->getIndices()))
      return true;
  }

  if (auto PD = dyn_cast<ProtocolDecl>(D)) {
    if (PD->getAttrs().hasAttribute<ShowInInterfaceAttr>())
      return false;
    StringRef NameStr = PD->getNameStr();
    if (NameStr.startswith("_Builtin"))
      return true;
    if (NameStr.startswith("_ExpressibleBy"))
      return true;
    if (whitelistProtocols)
      return false;
  }

  if (auto ImportD = dyn_cast<ImportDecl>(D)) {
    if (ImportD->getModule()->isSwiftShimsModule())
      return true;
  }

  auto VD = dyn_cast<ValueDecl>(D);
  if (!VD || !VD->hasName())
    return false;

  // If the name has leading underscore then it's a private symbol.
  if (VD->getNameStr().startswith("_"))
    return true;

  return false;
}

bool Decl::isWeakImported(ModuleDecl *fromModule) const {
  // For a Clang declaration, trust Clang.
  if (auto clangDecl = getClangDecl()) {
    return clangDecl->isWeakImported();
  }

  // FIXME: Implement using AvailableAttr::getVersionAvailability().
  return false;
}

GenericParamList::GenericParamList(SourceLoc LAngleLoc,
                                   ArrayRef<GenericTypeParamDecl *> Params,
                                   SourceLoc WhereLoc,
                                   MutableArrayRef<RequirementRepr> Requirements,
                                   SourceLoc RAngleLoc)
  : Brackets(LAngleLoc, RAngleLoc), NumParams(Params.size()),
    WhereLoc(WhereLoc), Requirements(Requirements),
    OuterParameters(nullptr),
    FirstTrailingWhereArg(Requirements.size())
{
  std::uninitialized_copy(Params.begin(), Params.end(),
                          getTrailingObjects<GenericTypeParamDecl *>());
}

GenericParamList *
GenericParamList::create(ASTContext &Context,
                         SourceLoc LAngleLoc,
                         ArrayRef<GenericTypeParamDecl *> Params,
                         SourceLoc RAngleLoc) {
  unsigned Size = totalSizeToAlloc<GenericTypeParamDecl *>(Params.size());
  void *Mem = Context.Allocate(Size, alignof(GenericParamList));
  return new (Mem) GenericParamList(LAngleLoc, Params, SourceLoc(),
                                    MutableArrayRef<RequirementRepr>(),
                                    RAngleLoc);
}

GenericParamList *
GenericParamList::create(const ASTContext &Context,
                         SourceLoc LAngleLoc,
                         ArrayRef<GenericTypeParamDecl *> Params,
                         SourceLoc WhereLoc,
                         ArrayRef<RequirementRepr> Requirements,
                         SourceLoc RAngleLoc) {
  unsigned Size = totalSizeToAlloc<GenericTypeParamDecl *>(Params.size());
  void *Mem = Context.Allocate(Size, alignof(GenericParamList));
  return new (Mem) GenericParamList(LAngleLoc, Params,
                                    WhereLoc,
                                    Context.AllocateCopy(Requirements),
                                    RAngleLoc);
}

GenericParamList *
GenericParamList::clone(DeclContext *dc) const {
  auto &ctx = dc->getASTContext();
  SmallVector<GenericTypeParamDecl *, 2> params;
  for (auto param : getParams()) {
    auto *newParam = new (ctx) GenericTypeParamDecl(
      dc, param->getName(), param->getNameLoc(),
      GenericTypeParamDecl::InvalidDepth,
      param->getIndex());
    params.push_back(newParam);

    SmallVector<TypeLoc, 2> inherited;
    for (auto loc : param->getInherited())
      inherited.push_back(loc.clone(ctx));
    newParam->setInherited(ctx.AllocateCopy(inherited));
  }

  SmallVector<RequirementRepr, 2> requirements;
  for (auto reqt : getRequirements()) {
    switch (reqt.getKind()) {
    case RequirementReprKind::TypeConstraint: {
      auto first = reqt.getSubjectLoc();
      auto second = reqt.getConstraintLoc();
      reqt = RequirementRepr::getTypeConstraint(
          first.clone(ctx),
          reqt.getColonLoc(),
          second.clone(ctx));
      break;
    }
    case RequirementReprKind::SameType: {
      auto first = reqt.getFirstTypeLoc();
      auto second = reqt.getSecondTypeLoc();
      reqt = RequirementRepr::getSameType(
          first.clone(ctx),
          reqt.getEqualLoc(),
          second.clone(ctx));
      break;
    }
    case RequirementReprKind::LayoutConstraint: {
      auto first = reqt.getSubjectLoc();
      auto layout = reqt.getLayoutConstraintLoc();
      reqt = RequirementRepr::getLayoutConstraint(
          first.clone(ctx),
          reqt.getColonLoc(),
          layout);
      break;
    }
    }

    requirements.push_back(reqt);
  }

  return GenericParamList::create(ctx,
                                  getLAngleLoc(),
                                  params,
                                  getWhereLoc(),
                                  requirements,
                                  getRAngleLoc());
}

void GenericParamList::addTrailingWhereClause(
       ASTContext &ctx,
       SourceLoc trailingWhereLoc,
       ArrayRef<RequirementRepr> trailingRequirements) {
  assert(TrailingWhereLoc.isInvalid() &&
         "Already have a trailing where clause?");
  TrailingWhereLoc = trailingWhereLoc;
  FirstTrailingWhereArg = Requirements.size();

  // Create a unified set of requirements.
  auto newRequirements = ctx.AllocateUninitialized<RequirementRepr>(
                           Requirements.size() + trailingRequirements.size());
  std::memcpy(newRequirements.data(), Requirements.data(),
              Requirements.size() * sizeof(RequirementRepr));
  std::memcpy(newRequirements.data() + Requirements.size(),
              trailingRequirements.data(),
              trailingRequirements.size() * sizeof(RequirementRepr));

  Requirements = newRequirements;
}

TrailingWhereClause::TrailingWhereClause(
                       SourceLoc whereLoc,
                       ArrayRef<RequirementRepr> requirements)
  : WhereLoc(whereLoc),
    NumRequirements(requirements.size())
{
  std::uninitialized_copy(requirements.begin(), requirements.end(),
                          getTrailingObjects<RequirementRepr>());
}

TrailingWhereClause *TrailingWhereClause::create(
                       ASTContext &ctx,
                       SourceLoc whereLoc,
                       ArrayRef<RequirementRepr> requirements) {
  unsigned size = totalSizeToAlloc<RequirementRepr>(requirements.size());
  void *mem = ctx.Allocate(size, alignof(TrailingWhereClause));
  return new (mem) TrailingWhereClause(whereLoc, requirements);
}

void GenericContext::setGenericParams(GenericParamList *params) {
  GenericParams = params;

  if (GenericParams) {
    for (auto param : *GenericParams)
      param->setDeclContext(this);
  }
}

GenericSignature *GenericContext::getGenericSignature() const {
  if (auto genericEnv = GenericSigOrEnv.dyn_cast<GenericEnvironment *>())
    return genericEnv->getGenericSignature();

  if (auto genericSig = GenericSigOrEnv.dyn_cast<GenericSignature *>())
    return genericSig;

  // The signature of a Protocol is trivial (Self: TheProtocol) so let's compute
  // it.
  if (auto PD = dyn_cast<ProtocolDecl>(this)) {
    auto self = PD->getSelfInterfaceType()->castTo<GenericTypeParamType>();
    auto req =
        Requirement(RequirementKind::Conformance, self, PD->getDeclaredType());
    return GenericSignature::get({self}, {req});
  }

  return nullptr;
}

GenericEnvironment *GenericContext::getGenericEnvironment() const {
  // Fast case: we already have a generic environment.
  if (auto genericEnv = GenericSigOrEnv.dyn_cast<GenericEnvironment *>())
    return genericEnv;

  // If we only have a generic signature, build the generic environment.
  if (GenericSigOrEnv.dyn_cast<GenericSignature *>())
    return getLazyGenericEnvironmentSlow();

  return nullptr;
}

void GenericContext::setGenericEnvironment(GenericEnvironment *genericEnv) {
  assert((GenericSigOrEnv.isNull() ||
          getGenericSignature()->getCanonicalSignature() ==
            genericEnv->getGenericSignature()->getCanonicalSignature()) &&
         "set a generic environment with a different generic signature");
  this->GenericSigOrEnv = genericEnv;
  if (genericEnv)
    genericEnv->setOwningDeclContext(this);
}

GenericEnvironment *
GenericContext::getLazyGenericEnvironmentSlow() const {
  assert(GenericSigOrEnv.is<GenericSignature *>() &&
         "not a lazily deserialized generic environment");

  auto contextData = getASTContext().getOrCreateLazyGenericContextData(
    this, nullptr);
  auto *genericEnv = contextData->loader->loadGenericEnvironment(
    this, contextData->genericEnvData);

  const_cast<GenericContext *>(this)->setGenericEnvironment(genericEnv);
  ++NumLazyGenericEnvironmentsLoaded;
  return genericEnv;
}

void GenericContext::setLazyGenericEnvironment(LazyMemberLoader *lazyLoader,
                                               GenericSignature *genericSig,
                                               uint64_t genericEnvData) {
  assert(GenericSigOrEnv.isNull() && "already have a generic signature");
  GenericSigOrEnv = genericSig;

  auto contextData =
    getASTContext().getOrCreateLazyGenericContextData(this, lazyLoader);
  contextData->genericEnvData = genericEnvData;

  ++NumLazyGenericEnvironments;
}

ImportDecl *ImportDecl::create(ASTContext &Ctx, DeclContext *DC,
                               SourceLoc ImportLoc, ImportKind Kind,
                               SourceLoc KindLoc,
                               ArrayRef<AccessPathElement> Path,
                               ClangNode ClangN) {
  assert(!Path.empty());
  assert(Kind == ImportKind::Module || Path.size() > 1);
  assert(ClangN.isNull() || ClangN.getAsModule() ||
         isa<clang::ImportDecl>(ClangN.getAsDecl()));
  size_t Size = totalSizeToAlloc<AccessPathElement>(Path.size());
  void *ptr = allocateMemoryForDecl<ImportDecl>(Ctx, Size, !ClangN.isNull());
  auto D = new (ptr) ImportDecl(DC, ImportLoc, Kind, KindLoc, Path);
  if (ClangN)
    D->setClangNode(ClangN);
  return D;
}

ImportDecl::ImportDecl(DeclContext *DC, SourceLoc ImportLoc, ImportKind K,
                       SourceLoc KindLoc, ArrayRef<AccessPathElement> Path)
  : Decl(DeclKind::Import, DC), ImportLoc(ImportLoc), KindLoc(KindLoc),
    NumPathElements(Path.size()) {
  ImportDeclBits.ImportKind = static_cast<unsigned>(K);
  assert(getImportKind() == K && "not enough bits for ImportKind");
  std::uninitialized_copy(Path.begin(), Path.end(),
                          getTrailingObjects<AccessPathElement>());
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
  case DeclKind::IfConfig:
  case DeclKind::PrecedenceGroup:
    llvm_unreachable("not a ValueDecl");

  case DeclKind::AssociatedType:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
  case DeclKind::GenericTypeParam:
  case DeclKind::Subscript:
  case DeclKind::EnumElement:
  case DeclKind::Param:
    llvm_unreachable("not a top-level ValueDecl");

  case DeclKind::Protocol:
    return ImportKind::Protocol;

  case DeclKind::Class:
    return ImportKind::Class;
  case DeclKind::Enum:
    return ImportKind::Enum;
  case DeclKind::Struct:
    return ImportKind::Struct;

  case DeclKind::TypeAlias: {
    Type type = cast<TypeAliasDecl>(VD)->getDeclaredInterfaceType();
    return getBestImportKind(type->getAnyNominal());
  }

  case DeclKind::Func:
    return ImportKind::Func;

  case DeclKind::Var:
    return ImportKind::Var;

  case DeclKind::Module:
    return ImportKind::Module;
  }
  llvm_unreachable("bad DeclKind");
}

Optional<ImportKind>
ImportDecl::findBestImportKind(ArrayRef<ValueDecl *> Decls) {
  assert(!Decls.empty());
  ImportKind FirstKind = ImportDecl::getBestImportKind(Decls.front());

  // FIXME: Only functions can be overloaded.
  if (Decls.size() == 1)
    return FirstKind;
  if (FirstKind != ImportKind::Func)
    return None;

  for (auto NextDecl : Decls.slice(1)) {
    if (ImportDecl::getBestImportKind(NextDecl) != FirstKind)
      return None;
  }

  return FirstKind;
}

DeclRange NominalTypeDecl::getMembers() const {
  loadAllMembers();
  return IterableDeclContext::getMembers();
}

void NominalTypeDecl::setConformanceLoader(LazyMemberLoader *lazyLoader,
                                           uint64_t contextData) {
  assert(!NominalTypeDeclBits.HasLazyConformances &&
         "Already have lazy conformances");
  NominalTypeDeclBits.HasLazyConformances = true;

  ASTContext &ctx = getASTContext();
  auto contextInfo = ctx.getOrCreateLazyIterableContextData(this, lazyLoader);
  contextInfo->allConformancesData = contextData;
}

std::pair<LazyMemberLoader *, uint64_t>
NominalTypeDecl::takeConformanceLoaderSlow() {
  assert(NominalTypeDeclBits.HasLazyConformances && "not lazy conformances");
  NominalTypeDeclBits.HasLazyConformances = false;
  auto contextInfo =
    getASTContext().getOrCreateLazyIterableContextData(this, nullptr);
  return { contextInfo->loader, contextInfo->allConformancesData };
}

ExtensionDecl::ExtensionDecl(SourceLoc extensionLoc,
                             TypeLoc extendedType,
                             MutableArrayRef<TypeLoc> inherited,
                             DeclContext *parent,
                             TrailingWhereClause *trailingWhereClause)
  : Decl(DeclKind::Extension, parent),
    GenericContext(DeclContextKind::ExtensionDecl, parent),
    IterableDeclContext(IterableDeclContextKind::ExtensionDecl),
    ExtensionLoc(extensionLoc),
    ExtendedType(extendedType),
    Inherited(inherited)
{
  ExtensionDeclBits.CheckedInheritanceClause = false;
  ExtensionDeclBits.DefaultAndMaxAccessLevel = 0;
  ExtensionDeclBits.HasLazyConformances = false;
  setTrailingWhereClause(trailingWhereClause);
}

ExtensionDecl *ExtensionDecl::create(ASTContext &ctx, SourceLoc extensionLoc,
                                     TypeLoc extendedType,
                                     MutableArrayRef<TypeLoc> inherited,
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

DeclRange ExtensionDecl::getMembers() const {
  loadAllMembers();
  return IterableDeclContext::getMembers();
}

void ExtensionDecl::setConformanceLoader(LazyMemberLoader *lazyLoader,
                                         uint64_t contextData) {
  assert(!ExtensionDeclBits.HasLazyConformances && 
         "Already have lazy conformances");
  ExtensionDeclBits.HasLazyConformances = true;

  ASTContext &ctx = getASTContext();
  auto contextInfo = ctx.getOrCreateLazyIterableContextData(this, lazyLoader);
  contextInfo->allConformancesData = contextData;
}

std::pair<LazyMemberLoader *, uint64_t>
ExtensionDecl::takeConformanceLoaderSlow() {
  assert(ExtensionDeclBits.HasLazyConformances && "no conformance loader?");
  ExtensionDeclBits.HasLazyConformances = false;

  auto contextInfo =
    getASTContext().getOrCreateLazyIterableContextData(this, nullptr);
  return { contextInfo->loader, contextInfo->allConformancesData };
}

bool ExtensionDecl::isConstrainedExtension() const {
  // Non-generic extension.
  if (!getGenericSignature())
    return false;

  auto nominal = getExtendedType()->getAnyNominal();
  assert(nominal);

  // If the generic signature differs from that of the nominal type, it's a
  // constrained extension.
  return getGenericSignature()->getCanonicalSignature()
    != nominal->getGenericSignature()->getCanonicalSignature();
}


PatternBindingDecl::PatternBindingDecl(SourceLoc StaticLoc,
                                       StaticSpellingKind StaticSpelling,
                                       SourceLoc VarLoc,
                                       unsigned NumPatternEntries,
                                       DeclContext *Parent)
  : Decl(DeclKind::PatternBinding, Parent),
    StaticLoc(StaticLoc), VarLoc(VarLoc) {
  PatternBindingDeclBits.IsStatic = StaticLoc.isValid();
  PatternBindingDeclBits.StaticSpelling =
       static_cast<unsigned>(StaticSpelling);
  PatternBindingDeclBits.NumPatternEntries = NumPatternEntries;
}

PatternBindingDecl *
PatternBindingDecl::create(ASTContext &Ctx, SourceLoc StaticLoc,
                           StaticSpellingKind StaticSpelling,
                           SourceLoc VarLoc,
                           Pattern *Pat, Expr *E,
                           DeclContext *Parent) {
  DeclContext *BindingInitContext = nullptr;
  if (!Parent->isLocalContext())
    BindingInitContext = new (Ctx) PatternBindingInitializer(Parent);

  auto Result = create(Ctx, StaticLoc, StaticSpelling, VarLoc,
                       PatternBindingEntry(Pat, E, BindingInitContext),
                       Parent);

  if (BindingInitContext)
    cast<PatternBindingInitializer>(BindingInitContext)->setBinding(Result, 0);

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
  auto entries = PBD->getMutablePatternList();
  unsigned elt = 0U-1;
  for (auto pe : PatternList) {
    ++elt;
    auto &newEntry = entries[elt];
    newEntry = pe; // This should take care of initializer with flags
    DeclContext *initContext = pe.getInitContext();
    if (!initContext && !Parent->isLocalContext()) {
      auto pbi = new (Ctx) PatternBindingInitializer(Parent);
      pbi->setBinding(PBD, elt);
      initContext = pbi;
    }

    PBD->setPattern(elt, pe.getPattern(), initContext);
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
    entry = PatternBindingEntry(nullptr, nullptr, nullptr);
  }
  return PBD;
}

static bool patternContainsVarDeclBinding(const Pattern *P, const VarDecl *VD) {
  bool Result = false;
  P->forEachVariable([&](VarDecl *FoundVD) {
    Result |= FoundVD == VD;
  });
  return Result;
}

unsigned PatternBindingDecl::getPatternEntryIndexForVarDecl(const VarDecl *VD) const {
  assert(VD && "Cannot find a null VarDecl");
  
  auto List = getPatternList();
  if (List.size() == 1) {
    assert(patternContainsVarDeclBinding(List[0].getPattern(), VD) &&
           "Single entry PatternBindingDecl is set up wrong");
    return 0;
  }
  
  unsigned Result = 0;
  for (auto entry : List) {
    if (patternContainsVarDeclBinding(entry.getPattern(), VD))
      return Result;
    ++Result;
  }
  
  assert(0 && "PatternBindingDecl doesn't bind the specified VarDecl!");
  return ~0U;
}

SourceRange PatternBindingEntry::getOrigInitRange() const {
  auto Init = InitCheckedAndRemoved.getPointer();
  return Init ? Init->getSourceRange() : SourceRange();
}

void PatternBindingEntry::setInit(Expr *E) {
  auto F = InitCheckedAndRemoved.getInt();
  if (E) {
    InitCheckedAndRemoved.setInt(F - Flags::Removed);
    InitCheckedAndRemoved.setPointer(E);
  } else {
    InitCheckedAndRemoved.setInt(F | Flags::Removed);
  }
}

VarDecl *PatternBindingEntry::getAnchoringVarDecl() const {
  SmallVector<VarDecl *, 8> variables;
  getPattern()->collectVariables(variables);
  assert(variables.size() > 0);
  return variables[0];
}

SourceRange PatternBindingEntry::getSourceRange(bool omitAccessors) const {
  // Patterns end at the initializer, if present.
  SourceLoc endLoc = getOrigInitRange().End;

  // If we're not banned from handling accessors, they follow the initializer.
  if (!omitAccessors) {
    getPattern()->forEachVariable([&](VarDecl *var) {
      auto accessorsEndLoc = var->getBracesRange().End;
      if (accessorsEndLoc.isValid())
        endLoc = accessorsEndLoc;
    });
  }

  // If we didn't find an end yet, check the pattern.
  if (endLoc.isInvalid())
    endLoc = getPattern()->getEndLoc();

  SourceLoc startLoc = getPattern()->getStartLoc();
  if (startLoc.isValid() != endLoc.isValid()) return SourceRange();

  return SourceRange(startLoc, endLoc);
}

SourceRange PatternBindingDecl::getSourceRange() const {
  SourceLoc startLoc = getStartLoc();
  SourceLoc endLoc = getPatternList().back().getSourceRange().End;
  if (startLoc.isValid() != endLoc.isValid()) return SourceRange();
  return { startLoc, endLoc };
}

static StaticSpellingKind getCorrectStaticSpellingForDecl(const Decl *D) {
  if (!D->getDeclContext()->getAsClassOrClassExtensionContext())
    return StaticSpellingKind::KeywordStatic;

  return StaticSpellingKind::KeywordClass;
}

StaticSpellingKind PatternBindingDecl::getCorrectStaticSpelling() const {
  if (!isStatic())
    return StaticSpellingKind::None;
  if (getStaticSpelling() != StaticSpellingKind::None)
    return getStaticSpelling();

  return getCorrectStaticSpellingForDecl(this);
}


bool PatternBindingDecl::hasStorage() const {
  // Walk the pattern, to check to see if any of the VarDecls included in it
  // have storage.
  for (auto entry : getPatternList())
    if (entry.getPattern()->hasStorage())
      return true;
  return false;
}

void PatternBindingDecl::setPattern(unsigned i, Pattern *P,
                                    DeclContext *InitContext) {
  auto PatternList = getMutablePatternList();
  PatternList[i].setPattern(P);
  PatternList[i].setInitContext(InitContext);
  
  // Make sure that any VarDecl's contained within the pattern know about this
  // PatternBindingDecl as their parent.
  if (P)
    P->forEachVariable([&](VarDecl *VD) {
      VD->setParentPatternBinding(this);
    });
}


VarDecl *PatternBindingDecl::getSingleVar() const {
  if (getNumPatternEntries() == 1)
    return getPatternList()[0].getPattern()->getSingleVar();
  return nullptr;
}

SourceLoc TopLevelCodeDecl::getStartLoc() const {
  return Body->getStartLoc();
}

SourceRange TopLevelCodeDecl::getSourceRange() const {
  return Body->getSourceRange();
}

SourceRange IfConfigDecl::getSourceRange() const {
  return SourceRange(getLoc(), EndLoc);
}

static bool isPolymorphic(const AbstractStorageDecl *storage) {
  auto nominal = storage->getDeclContext()
      ->getAsNominalTypeOrNominalTypeExtensionContext();
  if (!nominal) return false;

  switch (nominal->getKind()) {
#define DECL(ID, BASE) case DeclKind::ID:
#define NOMINAL_TYPE_DECL(ID, BASE)
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("not a nominal type!");

  case DeclKind::Struct:
  case DeclKind::Enum:
    return false;

  case DeclKind::Protocol:
    return true;

  case DeclKind::Class:
    // Final properties can always be direct, even in classes.
    return !storage->isFinal();
  }
  llvm_unreachable("bad DeclKind");
}

/// Determines the access semantics to use in a DeclRefExpr or
/// MemberRefExpr use of this value in the specified context.
AccessSemantics
ValueDecl::getAccessSemanticsFromContext(const DeclContext *UseDC) const {
  // If we're inside a @_transparent function, use the most conservative
  // access pattern, since we may be inlined from a different resilience
  // domain.
  ResilienceExpansion expansion = UseDC->getResilienceExpansion();

  if (auto *var = dyn_cast<AbstractStorageDecl>(this)) {
    // Observing member are accessed directly from within their didSet/willSet
    // specifiers.  This prevents assignments from becoming infinite loops.
    if (auto *UseFD = dyn_cast<FuncDecl>(UseDC))
      if (var->hasStorage() && var->hasAccessorFunctions() &&
          UseFD->getAccessorStorageDecl() == var)
        return AccessSemantics::DirectToStorage;
    
    // "StoredWithTrivialAccessors" are generally always accessed indirectly,
    // but if we know that the trivial accessor will always produce the same
    // thing as the getter/setter (i.e., it can't be overridden), then just do a
    // direct access.
    //
    // This is true in structs and for final properties.
    // TODO: What about static properties?
    switch (var->getStorageKind()) {
    case AbstractStorageDecl::Stored:
    case AbstractStorageDecl::Addressed:
      // The storage is completely trivial. Always do direct access.
      return AccessSemantics::DirectToStorage;

    case AbstractStorageDecl::StoredWithTrivialAccessors:
    case AbstractStorageDecl::AddressedWithTrivialAccessors: {
      // If the property is defined in a non-final class or a protocol, the
      // accessors are dynamically dispatched, and we cannot do direct access.
      if (isPolymorphic(var))
        return AccessSemantics::Ordinary;

      // If the property does not have a fixed layout from the given context,
      // we cannot do direct access.
      if (!var->hasFixedLayout(UseDC->getParentModule(), expansion))
        return AccessSemantics::Ordinary;

      // We know enough about the property to perform direct access.
      return AccessSemantics::DirectToStorage;
    }

    case AbstractStorageDecl::StoredWithObservers:
    case AbstractStorageDecl::InheritedWithObservers:
    case AbstractStorageDecl::Computed:
    case AbstractStorageDecl::ComputedWithMutableAddress:
    case AbstractStorageDecl::AddressedWithObservers:
      // Property is not trivially backed by storage, do not perform
      // direct access.
      break;
    }
  }

  return AccessSemantics::Ordinary;
}

AccessStrategy
AbstractStorageDecl::getAccessStrategy(AccessSemantics semantics,
                                       AccessKind accessKind) const {
  switch (semantics) {
  case AccessSemantics::DirectToStorage:
    switch (getStorageKind()) {
    case Stored:
    case StoredWithTrivialAccessors:
    case StoredWithObservers:
      return AccessStrategy::Storage;

    case Addressed:
    case AddressedWithTrivialAccessors:
    case AddressedWithObservers:
    case ComputedWithMutableAddress:
      return AccessStrategy::Addressor;

    case InheritedWithObservers:
    case Computed:
      llvm_unreachable("cannot have direct-to-storage access to "
                       "computed storage");
    }
    llvm_unreachable("bad storage kind");

  case AccessSemantics::DirectToAccessor:
    assert(hasAccessorFunctions() &&
           "direct-to-accessors access to storage without accessors?");
    return AccessStrategy::DirectToAccessor;

  case AccessSemantics::Ordinary:
    switch (auto storageKind = getStorageKind()) {
    case Stored:
      return AccessStrategy::Storage;
    case Addressed:
      return AccessStrategy::Addressor;

    case StoredWithObservers:
    case InheritedWithObservers:
    case AddressedWithObservers:
      // An observing property backed by its own storage (i.e. which
      // doesn't override anything) has a trivial getter implementation,
      // but its setter is interesting.
      if (accessKind != AccessKind::Read ||
          storageKind == InheritedWithObservers) {
        if (isPolymorphic(this))
          return AccessStrategy::DispatchToAccessor;
        return AccessStrategy::DirectToAccessor;
      }

      // Fall through to the trivial-implementation case.
      LLVM_FALLTHROUGH;

    case StoredWithTrivialAccessors:
    case AddressedWithTrivialAccessors: {
      // If the property is defined in a non-final class or a protocol, the
      // accessors are dynamically dispatched, and we cannot do direct access.
      if (isPolymorphic(this))
        return AccessStrategy::DispatchToAccessor;

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
      if (!hasFixedLayout())
        return AccessStrategy::DirectToAccessor;

      if (storageKind == StoredWithObservers ||
          storageKind == StoredWithTrivialAccessors) {
        return AccessStrategy::Storage;
      } else {
        assert(storageKind == AddressedWithObservers ||
               storageKind == AddressedWithTrivialAccessors);
        return AccessStrategy::Addressor;
      }
    }

    case ComputedWithMutableAddress:
      if (isPolymorphic(this))
        return AccessStrategy::DispatchToAccessor;
      if (accessKind == AccessKind::Read)
        return AccessStrategy::DirectToAccessor;
      return AccessStrategy::Addressor;

    case Computed:
      if (isPolymorphic(this))
        return AccessStrategy::DispatchToAccessor;
      return AccessStrategy::DirectToAccessor;
    }
    llvm_unreachable("bad storage kind");
  case AccessSemantics::BehaviorInitialization:
    // Behavior initialization writes to the property as if it has storage.
    // SIL definite initialization will introduce the logical accesses.
    // Reads or inouts still go through the getter.
    switch (accessKind) {
    case AccessKind::Write:
      return AccessStrategy::BehaviorStorage;
    case AccessKind::ReadWrite:
    case AccessKind::Read:
      return AccessStrategy::DispatchToAccessor;
    }
  }
  llvm_unreachable("bad access semantics");
}

bool AbstractStorageDecl::hasFixedLayout() const {
  // If we're in a nominal type, just query the type.
  auto *dc = getDeclContext();

  if (dc->isTypeContext()) {
    auto declaredType = dc->getDeclaredTypeOfContext();
    if (declaredType->hasError())
      return true;
    return declaredType->getAnyNominal()->hasFixedLayout();
  }

  // Private and (unversioned) internal variables always have a
  // fixed layout.
  if (getEffectiveAccess() < Accessibility::Public)
    return true;

  // Check for an explicit @_fixed_layout attribute.
  if (getAttrs().hasAttribute<FixedLayoutAttr>())
    return true;

  // Must use resilient access patterns.
  assert(getDeclContext()->isModuleScopeContext());
  switch (getDeclContext()->getParentModule()->getResilienceStrategy()) {
  case ResilienceStrategy::Resilient:
    return false;
  case ResilienceStrategy::Fragile:
  case ResilienceStrategy::Default:
    return true;
  }

  llvm_unreachable("Unhandled ResilienceStrategy in switch.");
}

bool AbstractStorageDecl::hasFixedLayout(ModuleDecl *M,
                                         ResilienceExpansion expansion) const {
  switch (expansion) {
  case ResilienceExpansion::Minimal:
    return hasFixedLayout();
  case ResilienceExpansion::Maximal:
    return hasFixedLayout() || M == getModuleContext();
  }
  llvm_unreachable("bad resilience expansion");
}


bool ValueDecl::isDefinition() const {
  switch (getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::EnumCase:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::IfConfig:
  case DeclKind::PrecedenceGroup:
    assert(!isa<ValueDecl>(this));
    llvm_unreachable("non-value decls shouldn't get here");

  case DeclKind::Func:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
    return cast<AbstractFunctionDecl>(this)->hasBody();

  case DeclKind::Subscript:
  case DeclKind::Var:
  case DeclKind::Param:
  case DeclKind::Enum:
  case DeclKind::EnumElement:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::TypeAlias:
  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType:
  case DeclKind::Protocol:
  case DeclKind::Module:
    return true;
  }
  llvm_unreachable("bad DeclKind");
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
  case DeclKind::IfConfig:
  case DeclKind::PrecedenceGroup:
    llvm_unreachable("Not a ValueDecl");

  case DeclKind::Class:
  case DeclKind::Enum:
  case DeclKind::Protocol:
  case DeclKind::Struct:
  case DeclKind::TypeAlias:
  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType:
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
    // Non-static methods are instance members.
    return !cast<FuncDecl>(this)->isStatic();

  case DeclKind::EnumElement:
  case DeclKind::Param:
    // enum elements and function parameters are not instance members.
    return false;

  case DeclKind::Subscript:
    // Subscripts are always instance members.
    return true;

  case DeclKind::Var:
    // Non-static variables are instance members.
    return !cast<VarDecl>(this)->isStatic();

  case DeclKind::Module:
    // Modules are never instance members.
    return false;
  }
  llvm_unreachable("bad DeclKind");
}

bool ValueDecl::needsCapture() const {
  // We don't need to capture anything from non-local contexts.
  if (!getDeclContext()->isLocalContext())
    return false;
  // We don't need to capture types.
  if (isa<TypeDecl>(this))
    return false;
  return true;
}

ValueDecl *ValueDecl::getOverriddenDecl() const {
  if (auto fd = dyn_cast<FuncDecl>(this))
    return fd->getOverriddenDecl();
  if (auto sdd = dyn_cast<AbstractStorageDecl>(this))
    return sdd->getOverriddenDecl();
  if (auto cd = dyn_cast<ConstructorDecl>(this))
    return cd->getOverriddenDecl();
  return nullptr;
}

bool swift::conflicting(const OverloadSignature& sig1,
                        const OverloadSignature& sig2) {
  // A member of a protocol extension never conflicts with a member of a
  // protocol.
  if (sig1.InProtocolExtension != sig2.InProtocolExtension)
    return false;

  // If the base names are different, they can't conflict.
  if (sig1.Name.getBaseName() != sig2.Name.getBaseName())
    return false;
  
  // If one is a compound name and the other is not, they do not conflict
  // if one is a property and the other is a non-nullary function.
  if (sig1.Name.isCompoundName() != sig2.Name.isCompoundName()) {
    return !((sig1.IsProperty && sig2.Name.getArgumentNames().size() > 0) ||
             (sig2.IsProperty && sig1.Name.getArgumentNames().size() > 0));
  }
  
  return sig1.Name == sig2.Name &&
         sig1.InterfaceType == sig2.InterfaceType &&
         sig1.UnaryOperator == sig2.UnaryOperator &&
         sig1.IsInstanceMember == sig2.IsInstanceMember;
}

static Type mapSignatureFunctionType(ASTContext &ctx, Type type,
                                     bool topLevelFunction,
                                     bool isMethod,
                                     bool isInitializer,
                                     unsigned curryLevels);

/// Map a type within the signature of a declaration.
static Type mapSignatureType(ASTContext &ctx, Type type) {
  return type.transform([&](Type type) -> Type {
      if (type->is<FunctionType>()) {
        return mapSignatureFunctionType(ctx, type, false, false, false, 1);
      }
      
      return type;
    });
}

/// Map a signature type for a parameter.
static Type mapSignatureParamType(ASTContext &ctx, Type type) {
  /// Translate implicitly unwrapped optionals into strict optionals.
  if (auto uncheckedOptOf = type->getImplicitlyUnwrappedOptionalObjectType()) {
    type = OptionalType::get(uncheckedOptOf);
  }

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
  return AnyFunctionType::ExtInfo()
      .withRepresentation(info.getRepresentation())
      .withIsAutoClosure(info.isAutoClosure())
      .withThrows(info.throws());
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
  if (curryLevels == 0) {
    // In an initializer, ignore optionality.
    if (isInitializer) {
      if (auto objectType = type->getAnyOptionalObjectType())
        type = objectType;
    }

    // Translate implicitly unwrapped optionals into strict optionals.
    if (auto uncheckedOptOf = type->getImplicitlyUnwrappedOptionalObjectType()) {
      type = OptionalType::get(uncheckedOptOf);
    }

    return mapSignatureType(ctx, type);
  }

  auto funcTy = type->castTo<AnyFunctionType>();
  auto argTy = funcTy->getInput();

  if (auto tupleTy = argTy->getAs<TupleType>()) {
    SmallVector<TupleTypeElt, 4> elements;
    bool anyChanged = false;
    unsigned idx = 0;
    // Remap our parameters, and make sure to strip off @escaping
    for (const auto &elt : tupleTy->getElements()) {
      auto newEltTy = mapSignatureParamType(ctx, elt.getType());
      auto newParamFlags = elt.getParameterFlags().withEscaping(false);
      bool exactlyTheSame = newParamFlags == elt.getParameterFlags() &&
                            newEltTy.getPointer() == elt.getType().getPointer();

      // Don't build up anything if we never see any difference
      if (!anyChanged && exactlyTheSame) {
        ++idx;
        continue;
      }

      // First time we see a diff, copy over all the prior
      if (!anyChanged && !exactlyTheSame) {
        elements.append(tupleTy->getElements().begin(),
                        tupleTy->getElements().begin() + idx);
        anyChanged = true;
      }

      elements.emplace_back(newEltTy, elt.getName(), newParamFlags);
    }
    if (anyChanged) {
      argTy = TupleType::get(elements, ctx);
    }
  } else {
    argTy = mapSignatureParamType(ctx, argTy);

    if (isMethod) {
      // In methods, strip the 'inout' off of 'self' so that mutating and
      // non-mutating methods have the same self parameter type.
      if (auto inoutTy = argTy->getAs<InOutType>()) {
        argTy = inoutTy->getObjectType();
      }
    }
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
                                    argTy, resultTy, info);

  return FunctionType::get(argTy, resultTy, info);
}

OverloadSignature ValueDecl::getOverloadSignature() const {
  OverloadSignature signature;

  signature.Name = getFullName();
  signature.InProtocolExtension
    = getDeclContext()->getAsProtocolExtensionContext();

  // Functions, initializers, and de-initializers include their
  // interface types in their signatures as well as whether they are
  // instance members.
  if (auto afd = dyn_cast<AbstractFunctionDecl>(this)) {
    signature.InterfaceType =
        mapSignatureFunctionType(
            getASTContext(), getInterfaceType(),
            /*topLevelFunction=*/true,
            /*isMethod=*/afd->getImplicitSelfDecl() != nullptr,
            /*isInitializer=*/isa<ConstructorDecl>(afd),
             afd->getNumParameterLists())->getCanonicalType();

    signature.IsInstanceMember = isInstanceMember();
    // Unary operators also include prefix/postfix.
    if (auto func = dyn_cast<FuncDecl>(this)) {
      if (func->isUnaryOperator()) {
        signature.UnaryOperator = func->getAttrs().getUnaryOperatorKind();
      }
    }
  } else if (isa<SubscriptDecl>(this)) {
    signature.InterfaceType = getInterfaceType()->getCanonicalType();

    // If the subscript occurs within a generic extension context,
    // consider the generic signature of the extension.
    if (auto ext = dyn_cast<ExtensionDecl>(getDeclContext())) {
      if (auto genericSig = ext->getGenericSignature()) {
        if (auto funcTy = signature.InterfaceType->getAs<AnyFunctionType>()) {
          signature.InterfaceType
            = GenericFunctionType::get(genericSig,
                                       funcTy->getInput(),
                                       funcTy->getResult(),
                                       funcTy->getExtInfo())
                ->getCanonicalType();
        }
      }
    }
  } else if (isa<VarDecl>(this)) {
    signature.IsProperty = true;
    signature.IsInstanceMember = isInstanceMember();

    // If the property occurs within a generic extension context,
    // consider the generic signature of the extension.
    if (auto ext = dyn_cast<ExtensionDecl>(getDeclContext())) {
      if (auto genericSig = ext->getGenericSignature()) {
        ASTContext &ctx = getASTContext();
        signature.InterfaceType
          = GenericFunctionType::get(genericSig,
                                     TupleType::getEmpty(ctx),
                                     TupleType::getEmpty(ctx),
                                     AnyFunctionType::ExtInfo())
              ->getCanonicalType();
      }
    }
  }

  return signature;
}

void ValueDecl::setIsObjC(bool Value) {
  bool CurrentValue = isObjC();
  if (CurrentValue == Value)
    return;

  if (!Value) {
    for (auto *Attr : getAttrs()) {
      if (auto *OA = dyn_cast<ObjCAttr>(Attr))
        OA->setInvalid();
    }
  } else {
    getAttrs().add(ObjCAttr::createUnnamedImplicit(getASTContext()));
  }
}

bool ValueDecl::canBeAccessedByDynamicLookup() const {
  if (!hasName())
    return false;

  // Dynamic lookup can only find @objc members.
  if (!isObjC())
    return false;

  // Dynamic lookup can only find class and protocol members, or extensions of
  // classes.
  auto declaredType = getDeclContext()->getDeclaredTypeOfContext();
  
  if (!declaredType)
    return false;
  
  auto nominalDC = declaredType->getAnyNominal();
  if (!nominalDC ||
      (!isa<ClassDecl>(nominalDC) && !isa<ProtocolDecl>(nominalDC)))
    return false;

  // Dynamic lookup cannot find results within a non-protocol generic context,
  // because there is no sensible way to infer the generic arguments.
  if (getDeclContext()->isGenericContext() && !isa<ProtocolDecl>(nominalDC))
    return false;

  // Dynamic lookup can find functions, variables, and subscripts.
  if (isa<FuncDecl>(this) || isa<VarDecl>(this) || isa<SubscriptDecl>(this))
    return true;

  return false;
}

ArrayRef<ValueDecl *>
ValueDecl::getSatisfiedProtocolRequirements(bool Sorted) const {
  // Dig out the nominal type.
  NominalTypeDecl *NTD =
    getDeclContext()->getAsNominalTypeOrNominalTypeExtensionContext();
  if (!NTD || isa<ProtocolDecl>(NTD))
    return {};

  return NTD->getSatisfiedProtocolRequirementsForMember(this, Sorted);
}

bool ValueDecl::isProtocolRequirement() const {
  assert(isa<ProtocolDecl>(getDeclContext()));

  if (auto *FD = dyn_cast<FuncDecl>(this))
    if (FD->isAccessor())
      return false;
  if (isa<TypeAliasDecl>(this) ||
      isa<NominalTypeDecl>(this))
    return false;
  return true;
}

bool ValueDecl::hasInterfaceType() const {
  return !TypeAndAccess.getPointer().isNull();
}

Type ValueDecl::getInterfaceType() const {
  assert(hasInterfaceType() && "No interface type was set");
  return TypeAndAccess.getPointer();
}

void ValueDecl::setInterfaceType(Type type) {
  // lldb creates global typealiases with archetypes in them.
  // FIXME: Add an isDebugAlias() flag, like isDebugVar().
  //
  // Also, ParamDecls in closure contexts can have type variables
  // archetype in them during constraint generation.
  if (!type.isNull() &&
      !isa<TypeAliasDecl>(this) &&
      !(isa<ParamDecl>(this) &&
        isa<AbstractClosureExpr>(getDeclContext()))) {
    assert(!type->hasArchetype() &&
           "Archetype in interface type");
    assert(!type->hasTypeVariable() &&
           "Archetype in interface type");
  }

  TypeAndAccess.setPointer(type);
}

Optional<ObjCSelector> ValueDecl::getObjCRuntimeName() const {
  if (auto func = dyn_cast<AbstractFunctionDecl>(this))
    return func->getObjCSelector();

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

  return None;
}

bool ValueDecl::canInferObjCFromRequirement(ValueDecl *requirement) {
  // Only makes sense for a requirement of an @objc protocol.
  auto proto = cast<ProtocolDecl>(requirement->getDeclContext());
  if (!proto->isObjC()) return false;

  // Only makes sense when this declaration is within a nominal type
  // or extension thereof.
  auto nominal =
    getDeclContext()->getAsNominalTypeOrNominalTypeExtensionContext();
  if (!nominal) return false;

  // If there is already an @objc attribute with an explicit name, we
  // can't infer a name (it's already there).
  if (auto objcAttr = getAttrs().getAttribute<ObjCAttr>()) {
    if (!objcAttr->isNameImplicit()) return false;
  }

  // If the nominal type doesn't conform to the protocol at all, we
  // cannot infer @objc no matter what we do.
  SmallVector<ProtocolConformance *, 1> conformances;
  if (!nominal->lookupConformance(getModuleContext(), proto, conformances))
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

SourceLoc ValueDecl::getAttributeInsertionLoc(bool forModifier) const {
  if (auto var = dyn_cast<VarDecl>(this)) {
    // [attrs] var ...
    // The attributes are part of the VarDecl, but the 'var' is part of the PBD.
    SourceLoc resultLoc = var->getAttrs().getStartLoc(forModifier);
    if (resultLoc.isValid()) {
      return resultLoc;
    } else if (auto pbd = var->getParentPatternBinding()) {
      return pbd->getStartLoc();
    } else {
      return var->getStartLoc();
    }
  }

  SourceLoc resultLoc = getAttrs().getStartLoc(forModifier);
  return resultLoc.isValid() ? resultLoc : getStartLoc();
}

/// Returns true if \p VD needs to be treated as publicly-accessible
/// at the SIL, LLVM, and machine levels due to being versioned.
static bool isVersionedInternalDecl(const ValueDecl *VD) {
  assert(VD->getFormalAccess() == Accessibility::Internal);

  if (VD->getAttrs().hasAttribute<VersionedAttr>())
    return true;

  if (auto *fn = dyn_cast<FuncDecl>(VD))
    if (auto *ASD = fn->getAccessorStorageDecl())
      if (ASD->getAttrs().hasAttribute<VersionedAttr>())
        return true;

  return false;
}

/// Return the accessibility of an internal or public declaration
/// that's been testably imported.
static Accessibility getTestableAccess(const ValueDecl *decl) {
  // Non-final classes are considered open to @testable importers.
  if (auto cls = dyn_cast<ClassDecl>(decl)) {
    if (!cls->isFinal())
      return Accessibility::Open;

  // Non-final overridable class members are considered open to
  // @testable importers.
  } else if (decl->isPotentiallyOverridable()) {
    if (!cast<ValueDecl>(decl)->isFinal())
      return Accessibility::Open;
  }

  // Everything else is considered public.
  return Accessibility::Public;
}

Accessibility ValueDecl::getEffectiveAccess() const {
  Accessibility effectiveAccess = getFormalAccess();

  // Handle @testable.
  switch (effectiveAccess) {
  case Accessibility::Public:
    if (getModuleContext()->isTestingEnabled())
      effectiveAccess = getTestableAccess(this);
    break;
  case Accessibility::Open:
    break;
  case Accessibility::Internal:
    if (getModuleContext()->isTestingEnabled())
      effectiveAccess = getTestableAccess(this);
    else if (isVersionedInternalDecl(this))
      effectiveAccess = Accessibility::Public;
    break;
  case Accessibility::FilePrivate:
    break;
  case Accessibility::Private:
    effectiveAccess = Accessibility::FilePrivate;
    break;
  }

  if (auto enclosingNominal = dyn_cast<NominalTypeDecl>(getDeclContext())) {
    effectiveAccess = std::min(effectiveAccess,
                               enclosingNominal->getEffectiveAccess());

  } else if (auto enclosingExt = dyn_cast<ExtensionDecl>(getDeclContext())) {
    // Just check the base type. If it's a constrained extension, Sema should
    // have already enforced access more strictly.
    if (auto extendedTy = enclosingExt->getExtendedType()) {
      if (auto nominal = extendedTy->getAnyNominal()) {
        effectiveAccess = std::min(effectiveAccess,
                                   nominal->getEffectiveAccess());
      }
    }

  } else if (getDeclContext()->isLocalContext()) {
    effectiveAccess = Accessibility::FilePrivate;
  }

  return effectiveAccess;
}

Accessibility ValueDecl::getFormalAccessImpl(const DeclContext *useDC) const {
  assert((getFormalAccess() == Accessibility::Internal ||
          getFormalAccess() == Accessibility::Public) &&
         "should be able to fast-path non-internal cases");
  assert(useDC && "should fast-path non-scoped cases");
  if (auto *useSF = dyn_cast<SourceFile>(useDC->getModuleScopeContext()))
    if (useSF->hasTestableImport(getModuleContext()))
      return getTestableAccess(this);
  return getFormalAccess();
}

AccessScope ValueDecl::getFormalAccessScope(const DeclContext *useDC) const {
  const DeclContext *result = getDeclContext();
  Accessibility access = getFormalAccess(useDC);

  while (!result->isModuleScopeContext()) {
    if (result->isLocalContext() || access == Accessibility::Private)
      return AccessScope(result, true);

    if (auto enclosingNominal = dyn_cast<NominalTypeDecl>(result)) {
      access = std::min(access, enclosingNominal->getFormalAccess(useDC));

    } else if (auto enclosingExt = dyn_cast<ExtensionDecl>(result)) {
      // Just check the base type. If it's a constrained extension, Sema should
      // have already enforced access more strictly.
      if (auto extendedTy = enclosingExt->getExtendedType()) {
        if (auto nominal = extendedTy->getAnyNominal()) {
          access = std::min(access, nominal->getFormalAccess(useDC));
        }
      }

    } else {
      llvm_unreachable("unknown DeclContext kind");
    }

    result = result->getParent();
  }

  switch (access) {
  case Accessibility::Private:
  case Accessibility::FilePrivate:
    assert(result->isModuleScopeContext());
    return AccessScope(result, access == Accessibility::Private);
  case Accessibility::Internal:
    return AccessScope(result->getParentModule());
  case Accessibility::Public:
  case Accessibility::Open:
    return AccessScope::getPublic();
  }

  llvm_unreachable("unknown accessibility level");
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

  Type interfaceType = hasInterfaceType() ? getInterfaceType() : nullptr;
  if (interfaceType.isNull() || interfaceType->is<ErrorType>())
    return interfaceType;

  if (isa<ModuleDecl>(this))
    return interfaceType;

  return interfaceType->castTo<MetatypeType>()->getInstanceType();
}

bool NominalTypeDecl::hasFixedLayout() const {
  // Private and (unversioned) internal types always have a
  // fixed layout.
  if (getEffectiveAccess() < Accessibility::Public)
    return true;

  // Check for an explicit @_fixed_layout attribute.
  if (getAttrs().hasAttribute<FixedLayoutAttr>())
    return true;

  // Structs and enums imported from C *always* have a fixed layout.
  // We know their size, and pass them as values in SIL and IRGen.
  if (hasClangNode())
    return true;

  // @objc enums and protocols always have a fixed layout.
  if ((isa<EnumDecl>(this) || isa<ProtocolDecl>(this)) && isObjC())
    return true;

  // Otherwise, access via indirect "resilient" interfaces.
  switch (getParentModule()->getResilienceStrategy()) {
  case ResilienceStrategy::Resilient:
    return false;
  case ResilienceStrategy::Fragile:
  case ResilienceStrategy::Default:
    return true;
  }

  llvm_unreachable("Unhandled ResilienceStrategy in switch.");
}

bool NominalTypeDecl::hasFixedLayout(ModuleDecl *M,
                                     ResilienceExpansion expansion) const {
  switch (expansion) {
  case ResilienceExpansion::Minimal:
    return hasFixedLayout();
  case ResilienceExpansion::Maximal:
    return hasFixedLayout() || M == getModuleContext();
  }
  llvm_unreachable("bad resilience expansion");
}


bool NominalTypeDecl::derivesProtocolConformance(ProtocolDecl *protocol) const {
  // Only known protocols can be derived.
  auto knownProtocol = protocol->getKnownProtocolKind();
  if (!knownProtocol)
    return false;

  if (auto *enumDecl = dyn_cast<EnumDecl>(this)) {
    switch (*knownProtocol) {
    // The presence of a raw type is an explicit declaration that
    // the compiler should derive a RawRepresentable conformance.
    case KnownProtocolKind::RawRepresentable:
      return enumDecl->hasRawType();
    
    // Enums without associated values can implicitly derive Equatable and
    // Hashable conformance.
    case KnownProtocolKind::Equatable:
    case KnownProtocolKind::Hashable:
      return enumDecl->hasOnlyCasesWithoutAssociatedValues();
    
    // @objc enums can explicitly derive their _BridgedNSError conformance.
    case KnownProtocolKind::BridgedNSError:
      return isObjC() && enumDecl->hasOnlyCasesWithoutAssociatedValues();

    default:
      return false;
    }
  }
  return false;
}

void NominalTypeDecl::computeType() {
  ASTContext &ctx = getASTContext();

  // A protocol has an implicit generic parameter list consisting of a single
  // generic parameter, Self, that conforms to the protocol itself. This
  // parameter is always implicitly bound.
  //
  // If this protocol has been deserialized, it already has generic parameters.
  // Don't add them again.
  if (auto proto = dyn_cast<ProtocolDecl>(this))
    proto->createGenericParamsIfMissing();

  Type declaredInterfaceTy = getDeclaredInterfaceType();
  setInterfaceType(MetatypeType::get(declaredInterfaceTy, ctx));

  if (declaredInterfaceTy->hasError())
    setInvalid();
}

enum class DeclTypeKind : unsigned {
  DeclaredType,
  DeclaredTypeInContext,
  DeclaredInterfaceType
};

static Type computeNominalType(NominalTypeDecl *decl, DeclTypeKind kind) {
  ASTContext &ctx = decl->getASTContext();

  // Handle the declared type in context.
  if (kind == DeclTypeKind::DeclaredTypeInContext) {
    auto interfaceType =
      computeNominalType(decl, DeclTypeKind::DeclaredInterfaceType);

    if (!decl->isGenericContext())
      return interfaceType;

    auto *genericEnv = decl->getGenericEnvironmentOfContext();
    return GenericEnvironment::mapTypeIntoContext(
        genericEnv, interfaceType);
  }

  // Get the parent type.
  Type Ty;
  DeclContext *dc = decl->getDeclContext();
  if (dc->isTypeContext()) {
    switch (kind) {
    case DeclTypeKind::DeclaredType:
      Ty = dc->getDeclaredTypeOfContext();
      break;
    case DeclTypeKind::DeclaredTypeInContext:
      llvm_unreachable("Handled above");
    case DeclTypeKind::DeclaredInterfaceType:
      Ty = dc->getDeclaredInterfaceType();
      break;
    }
    if (!Ty)
      return Type();
    if (Ty->is<ErrorType>())
      Ty = Type();
  }

  if (decl->getGenericParams() &&
      !isa<ProtocolDecl>(decl)) {
    switch (kind) {
    case DeclTypeKind::DeclaredType:
      return UnboundGenericType::get(decl, Ty, ctx);
    case DeclTypeKind::DeclaredTypeInContext:
      llvm_unreachable("Handled above");
    case DeclTypeKind::DeclaredInterfaceType: {
      // Note that here, we need to be able to produce a type
      // before the decl has been validated, so we rely on
      // the generic parameter list directly instead of looking
      // at the signature.
      SmallVector<Type, 4> args;
      for (auto param : decl->getGenericParams()->getParams())
        args.push_back(param->getDeclaredInterfaceType());

      return BoundGenericType::get(decl, Ty, args);
    }
    }

    llvm_unreachable("Unhandled DeclTypeKind in switch.");
  } else {
    return NominalType::get(decl, Ty, ctx);
  }
}

Type NominalTypeDecl::getDeclaredType() const {
  if (DeclaredTy)
    return DeclaredTy;

  auto *decl = const_cast<NominalTypeDecl *>(this);
  decl->DeclaredTy = computeNominalType(decl, DeclTypeKind::DeclaredType);
  return DeclaredTy;
}

Type NominalTypeDecl::getDeclaredTypeInContext() const {
  if (DeclaredTyInContext)
    return DeclaredTyInContext;

  auto *decl = const_cast<NominalTypeDecl *>(this);
  decl->DeclaredTyInContext = computeNominalType(decl,
                                                 DeclTypeKind::DeclaredTypeInContext);
  return DeclaredTyInContext;
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
    return;
  }

  // Add to the end of the list.
  LastExtension->NextExtension.setPointer(extension);
  LastExtension = extension;
}

OptionalTypeKind NominalTypeDecl::classifyAsOptionalType() const {
  const ASTContext &ctx = getASTContext();
  if (this == ctx.getOptionalDecl()) {
    return OTK_Optional;
  } else if (this == ctx.getImplicitlyUnwrappedOptionalDecl()) {
    return OTK_ImplicitlyUnwrappedOptional;
  } else {
    return OTK_None;
  }
}

GenericTypeDecl::GenericTypeDecl(DeclKind K, DeclContext *DC,
                                 Identifier name, SourceLoc nameLoc,
                                 MutableArrayRef<TypeLoc> inherited,
                                 GenericParamList *GenericParams) :
    TypeDecl(K, DC, name, nameLoc, inherited),
    GenericContext(DeclContextKind::GenericTypeDecl, DC) {
  setGenericParams(GenericParams);
}

TypeAliasDecl::TypeAliasDecl(SourceLoc TypeAliasLoc, SourceLoc EqualLoc,
                             Identifier Name, SourceLoc NameLoc,
                             GenericParamList *GenericParams, DeclContext *DC)
  : GenericTypeDecl(DeclKind::TypeAlias, DC, Name, NameLoc, {}, GenericParams),
    TypeAliasLoc(TypeAliasLoc), EqualLoc(EqualLoc) {}

SourceRange TypeAliasDecl::getSourceRange() const {
  if (UnderlyingTy.hasLocation())
    return { TypeAliasLoc, UnderlyingTy.getSourceRange().End };
  return { TypeAliasLoc, getNameLoc() };
}

void TypeAliasDecl::setUnderlyingType(Type underlying) {
  setValidationStarted();

  // lldb creates global typealiases containing archetypes
  // sometimes...
  if (underlying->hasArchetype() && isGenericContext())
    underlying = mapTypeOutOfContext(underlying);
  UnderlyingTy.setType(underlying);

  // Create a NameAliasType which will resolve to the underlying type.
  ASTContext &Ctx = getASTContext();
  auto aliasTy = new (Ctx, AllocationArena::Permanent) NameAliasType(this);
  aliasTy->setRecursiveProperties(getUnderlyingTypeLoc().getType()
      ->getRecursiveProperties());

  // Set the interface type of this declaration.
  setInterfaceType(MetatypeType::get(aliasTy, Ctx));
}

UnboundGenericType *TypeAliasDecl::getUnboundGenericType() const {
  assert(getGenericParams());
  return UnboundGenericType::get(
      const_cast<TypeAliasDecl *>(this),
      getDeclContext()->getDeclaredTypeOfContext(),
      getASTContext());
}

Type AbstractTypeParamDecl::getSuperclass() const {
  auto *dc = getDeclContext();
  auto contextTy = dc->mapTypeIntoContext(getDeclaredInterfaceType());
  if (auto *archetype = contextTy->getAs<ArchetypeType>())
    return archetype->getSuperclass();

  // FIXME: Assert that this is never queried.
  return nullptr;
}

ArrayRef<ProtocolDecl *>
AbstractTypeParamDecl::getConformingProtocols() const {
  auto *dc = getDeclContext();
  auto contextTy = dc->mapTypeIntoContext(getDeclaredInterfaceType());
  if (auto *archetype = contextTy->getAs<ArchetypeType>())
    return archetype->getConformsTo();

  // FIXME: Assert that this is never queried.
  return { };
}

GenericTypeParamDecl::GenericTypeParamDecl(DeclContext *dc, Identifier name,
                                           SourceLoc nameLoc,
                                           unsigned depth, unsigned index)
  : AbstractTypeParamDecl(DeclKind::GenericTypeParam, dc, name, nameLoc),
    Depth(depth), Index(index)
{
  auto &ctx = dc->getASTContext();
  auto type = new (ctx, AllocationArena::Permanent) GenericTypeParamType(this);
  setInterfaceType(MetatypeType::get(type, ctx));
}

SourceRange GenericTypeParamDecl::getSourceRange() const {
  SourceLoc endLoc = getNameLoc();

  if (!getInherited().empty()) {
    endLoc = getInherited().back().getSourceRange().End;
  }
  return SourceRange(getNameLoc(), endLoc);
}

AssociatedTypeDecl::AssociatedTypeDecl(DeclContext *dc, SourceLoc keywordLoc,
                                       Identifier name, SourceLoc nameLoc,
                                       TypeLoc defaultDefinition,
                                       TrailingWhereClause *trailingWhere)
    : AbstractTypeParamDecl(DeclKind::AssociatedType, dc, name, nameLoc),
      KeywordLoc(keywordLoc), DefaultDefinition(defaultDefinition),
      TrailingWhere(trailingWhere) {}

AssociatedTypeDecl::AssociatedTypeDecl(DeclContext *dc, SourceLoc keywordLoc,
                                       Identifier name, SourceLoc nameLoc,
                                       TrailingWhereClause *trailingWhere,
                                       LazyMemberLoader *definitionResolver,
                                       uint64_t resolverData)
    : AbstractTypeParamDecl(DeclKind::AssociatedType, dc, name, nameLoc),
      KeywordLoc(keywordLoc), TrailingWhere(trailingWhere),
      Resolver(definitionResolver), ResolverContextData(resolverData) {
  assert(Resolver && "missing resolver");
}

void AssociatedTypeDecl::computeType() {
  auto &ctx = getASTContext();
  auto interfaceTy = getDeclaredInterfaceType();
  setInterfaceType(MetatypeType::get(interfaceTy, ctx));
}

TypeLoc &AssociatedTypeDecl::getDefaultDefinitionLoc() {
  if (Resolver) {
    DefaultDefinition =
      Resolver->loadAssociatedTypeDefault(this, ResolverContextData);
    Resolver = nullptr;
  }
  return DefaultDefinition;
}

SourceRange AssociatedTypeDecl::getSourceRange() const {
  SourceLoc endLoc = getNameLoc();

  if (!getInherited().empty()) {
    endLoc = getInherited().back().getSourceRange().End;
  }
  return SourceRange(KeywordLoc, endLoc);
}

EnumDecl::EnumDecl(SourceLoc EnumLoc,
                     Identifier Name, SourceLoc NameLoc,
                     MutableArrayRef<TypeLoc> Inherited,
                     GenericParamList *GenericParams, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::Enum, Parent, Name, NameLoc, Inherited,
                    GenericParams),
    EnumLoc(EnumLoc)
{
  EnumDeclBits.Circularity
    = static_cast<unsigned>(CircularityCheck::Unchecked);
}

StructDecl::StructDecl(SourceLoc StructLoc, Identifier Name, SourceLoc NameLoc,
                       MutableArrayRef<TypeLoc> Inherited,
                       GenericParamList *GenericParams, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::Struct, Parent, Name, NameLoc, Inherited,
                    GenericParams),
    StructLoc(StructLoc)
{
  StructDeclBits.HasUnreferenceableStorage = false;
}

ClassDecl::ClassDecl(SourceLoc ClassLoc, Identifier Name, SourceLoc NameLoc,
                     MutableArrayRef<TypeLoc> Inherited,
                     GenericParamList *GenericParams, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::Class, Parent, Name, NameLoc, Inherited,
                    GenericParams),
    ClassLoc(ClassLoc) {
  ClassDeclBits.Circularity
    = static_cast<unsigned>(CircularityCheck::Unchecked);
  ClassDeclBits.RequiresStoredPropertyInits = 0;
  ClassDeclBits.InheritsSuperclassInits
    = static_cast<unsigned>(StoredInheritsSuperclassInits::Unchecked);
  ClassDeclBits.RawForeignKind = 0;
  ClassDeclBits.HasDestructorDecl = 0;
  ClassDeclBits.ObjCClassKind = 0;
}

DestructorDecl *ClassDecl::getDestructor() {
  auto name = getASTContext().Id_deinit;
  auto results = lookupDirect(name);
  assert(!results.empty() && "Class without destructor?");
  assert(results.size() == 1 && "More than one destructor?");
  return cast<DestructorDecl>(results.front());
}

bool ClassDecl::inheritsSuperclassInitializers(LazyResolver *resolver) {
  // Check whether we already have a cached answer.
  switch (static_cast<StoredInheritsSuperclassInits>(
            ClassDeclBits.InheritsSuperclassInits)) {
  case StoredInheritsSuperclassInits::Unchecked:
    // Compute below.
    break;

  case StoredInheritsSuperclassInits::Inherited:
    return true;

  case StoredInheritsSuperclassInits::NotInherited:
    return false;
  }

  // If there's no superclass, there's nothing to inherit.
  ClassDecl *superclassDecl;
  if (!getSuperclass() ||
      !(superclassDecl = getSuperclass()->getClassOrBoundGenericClass())) {
    ClassDeclBits.InheritsSuperclassInits
      = static_cast<unsigned>(StoredInheritsSuperclassInits::NotInherited);
    return false;
  }

  // Look at all of the initializers of the subclass to gather the initializers
  // they override from the superclass.
  auto &ctx = getASTContext();
  llvm::SmallPtrSet<ConstructorDecl *, 4> overriddenInits;
  if (resolver)
    resolver->resolveImplicitConstructors(this);
  for (auto member : lookupDirect(ctx.Id_init)) {
    auto ctor = dyn_cast<ConstructorDecl>(member);
    if (!ctor)
      continue;

    // Swift initializers added in extensions of Objective-C classes can never
    // be overrides.
    if (hasClangNode() && !ctor->hasClangNode())
      return false;

    // Resolve this initializer, if needed.
    if (!ctor->hasInterfaceType())
      resolver->resolveDeclSignature(ctor);

    // Ignore any stub implementations.
    if (ctor->hasStubImplementation())
      continue;

    if (auto overridden = ctor->getOverriddenDecl()) {
      if (overridden->isDesignatedInit())
        overriddenInits.insert(overridden);
    }
  }

  // Check all of the designated initializers in the direct superclass.
  // Note: This should be treated as a lookup for intra-module dependency
  // purposes, but a subclass already depends on its superclasses and any
  // extensions for many other reasons.
  for (auto member : superclassDecl->lookupDirect(ctx.Id_init)) {
    if (AvailableAttr::isUnavailable(member))
      continue;

    // We only care about designated initializers.
    auto ctor = dyn_cast<ConstructorDecl>(member);
    if (!ctor || !ctor->isDesignatedInit() || ctor->hasStubImplementation())
      continue;

    // If this designated initializer wasn't overridden, we can't inherit.
    if (overriddenInits.count(ctor) == 0) {
      ClassDeclBits.InheritsSuperclassInits
        = static_cast<unsigned>(StoredInheritsSuperclassInits::NotInherited);
      return false;
    }
  }

  // All of the direct superclass's designated initializers have been overridden
  // by the subclass. Initializers can be inherited.
  ClassDeclBits.InheritsSuperclassInits
    = static_cast<unsigned>(StoredInheritsSuperclassInits::Inherited);
  return true;
}

ObjCClassKind ClassDecl::checkObjCAncestry() const {
  // See if we've already computed this.
  if (ClassDeclBits.ObjCClassKind)
    return ObjCClassKind(ClassDeclBits.ObjCClassKind - 1);

  llvm::SmallPtrSet<const ClassDecl *, 8> visited;
  bool genericAncestry = false, isObjC = false;
  const ClassDecl *CD = this;

  for (;;) {
    // If we hit circularity, we will diagnose at some point in typeCheckDecl().
    // However we have to explicitly guard against that here because we get
    // called as part of validateDecl().
    if (!visited.insert(CD).second)
      break;

    if (CD->isGenericContext())
      genericAncestry = true;

    if (CD->isObjC())
      isObjC = true;

    if (!CD->hasSuperclass())
      break;
    CD = CD->getSuperclass()->getClassOrBoundGenericClass();
    // If we don't have a valid class here, we should have diagnosed
    // elsewhere.
    if (!CD)
      break;
  }

  ObjCClassKind kind = ObjCClassKind::ObjC;
  if (!isObjC)
    kind = ObjCClassKind::NonObjC;
  else if (genericAncestry)
    kind = ObjCClassKind::ObjCMembers;
  else if (CD == this || !CD->isObjC())
    kind = ObjCClassKind::ObjCWithSwiftRoot;

  // Save the result for later.
  const_cast<ClassDecl *>(this)->ClassDeclBits.ObjCClassKind
    = unsigned(kind) + 1;
  return kind;
}

/// Mangle the name of a protocol or class for use in the Objective-C
/// runtime.
static StringRef mangleObjCRuntimeName(const NominalTypeDecl *nominal,
                                       llvm::SmallVectorImpl<char> &buffer) {
  {
    NewMangling::ASTMangler Mangler;
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
  if (auto objc = getAttrs().getAttribute<ObjCAttr>()) {
    if (auto name = objc->getName())
      return name->getString(buffer);
  }

  // Produce the mangled name for this class.
  return mangleObjCRuntimeName(this, buffer);
}

ArtificialMainKind ClassDecl::getArtificialMainKind() const {
  if (getAttrs().hasAttribute<UIApplicationMainAttr>())
    return ArtificialMainKind::UIApplicationMain;
  if (getAttrs().hasAttribute<NSApplicationMainAttr>())
    return ArtificialMainKind::NSApplicationMain;
  llvm_unreachable("class has no @ApplicationMain attr?!");
}

AbstractFunctionDecl *
ClassDecl::findOverridingDecl(const AbstractFunctionDecl *Method) const {
  auto Members = getMembers();
  for (auto M : Members) {
    AbstractFunctionDecl *CurMethod = dyn_cast<AbstractFunctionDecl>(M);
    if (!CurMethod)
      continue;
    if (CurMethod->isOverridingDecl(Method)) {
      return CurMethod;
    }
  }
  return nullptr;
}

bool AbstractFunctionDecl::isOverridingDecl(
    const AbstractFunctionDecl *Method) const {
  const AbstractFunctionDecl *CurMethod = this;
  while (CurMethod) {
    if (CurMethod == Method)
      return true;
    CurMethod = CurMethod->getOverriddenDecl();
  }
  return false;
}

AbstractFunctionDecl *
ClassDecl::findImplementingMethod(const AbstractFunctionDecl *Method) const {
  const ClassDecl *C = this;
  while (C) {
    auto Members = C->getMembers();
    for (auto M : Members) {
      AbstractFunctionDecl *CurMethod = dyn_cast<AbstractFunctionDecl>(M);
      if (!CurMethod)
        continue;
      if (Method == CurMethod)
        return CurMethod;
      if (CurMethod->isOverridingDecl(Method)) {
        // This class implements a method
        return CurMethod;
      }
    }
    // Check the superclass
    if (!C->hasSuperclass())
      break;
    C = C->getSuperclass()->getClassOrBoundGenericClass();
  }
  return nullptr;
}


EnumCaseDecl *EnumCaseDecl::create(SourceLoc CaseLoc,
                                   ArrayRef<EnumElementDecl *> Elements,
                                   DeclContext *DC) {
  void *buf = DC->getASTContext()
    .Allocate(sizeof(EnumCaseDecl) +
                    sizeof(EnumElementDecl*) * Elements.size(),
                  alignof(EnumCaseDecl));
  return ::new (buf) EnumCaseDecl(CaseLoc, Elements, DC);
}

EnumElementDecl *EnumDecl::getElement(Identifier Name) const {
  // FIXME: Linear search is not great for large enum decls.
  for (EnumElementDecl *Elt : getAllElements())
    if (Elt->getName() == Name)
      return Elt;
  return nullptr;
}

bool EnumDecl::hasOnlyCasesWithoutAssociatedValues() const {
  // FIXME: Should probably cache this.
  bool hasElements = false;
  for (auto elt : getAllElements()) {
    hasElements = true;
    if (!elt->getArgumentTypeLoc().isNull())
      return false;
  }
  return hasElements;
}

ProtocolDecl::ProtocolDecl(DeclContext *DC, SourceLoc ProtocolLoc,
                           SourceLoc NameLoc, Identifier Name,
                           MutableArrayRef<TypeLoc> Inherited)
  : NominalTypeDecl(DeclKind::Protocol, DC, Name, NameLoc, Inherited,
                    nullptr),
    ProtocolLoc(ProtocolLoc)
{
  ProtocolDeclBits.RequiresClassValid = false;
  ProtocolDeclBits.RequiresClass = false;
  ProtocolDeclBits.ExistentialConformsToSelfValid = false;
  ProtocolDeclBits.ExistentialConformsToSelf = false;
  KnownProtocol = 0;
  ProtocolDeclBits.Circularity
    = static_cast<unsigned>(CircularityCheck::Unchecked);
  HasMissingRequirements = false;
}

llvm::TinyPtrVector<ProtocolDecl *>
ProtocolDecl::getInheritedProtocols() const {
  llvm::TinyPtrVector<ProtocolDecl *> result;

  // FIXME: Gather inherited protocols from the "inherited" list.
  // We shouldn't need this, but it shows up in recursive invocations.
  if (!isRequirementSignatureComputed()) {
    for (auto inherited : getInherited()) {
      SmallPtrSet<ProtocolDecl *, 4> known;
      if (auto type = inherited.getType()) {
        SmallVector<ProtocolDecl *, 4> protocols;
        if (type->isExistentialType(protocols)) {
          for (auto proto : protocols) {
            if (known.insert(proto).second)
              result.push_back(proto);
          }
        }
      }
    }
    return result;
  }

  // Gather inherited protocols from the requirement signature.
  auto selfType = getProtocolSelfType();
  for (auto req : getRequirementSignature()->getRequirements()) {
    if (req.getKind() == RequirementKind::Conformance &&
        req.getFirstType()->isEqual(selfType))
      result.push_back(req.getSecondType()->castTo<ProtocolType>()->getDecl());
  }
  return result;
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

    case TypeWalker::Action::SkipChildren:
      break;
    }
  }

  return false;

}

bool ProtocolDecl::inheritsFrom(const ProtocolDecl *super) const {
  if (this == super)
    return false;
  
  auto allProtocols = getLocalProtocols();
  return std::find(allProtocols.begin(), allProtocols.end(), super)
           != allProtocols.end();
}

bool ProtocolDecl::requiresClassSlow() {
  ProtocolDeclBits.RequiresClass =
    walkInheritedProtocols([&](ProtocolDecl *proto) {
      // If the 'requires class' bit is valid, we don't need to search any
      // further.
      if (proto->ProtocolDeclBits.RequiresClassValid) {
        // If this protocol has a class requirement, we're done.
        if (proto->ProtocolDeclBits.RequiresClass)
          return TypeWalker::Action::Stop;

        return TypeWalker::Action::SkipChildren;
      }

      // Quick check: @objc indicates that it requires a class.
      if (proto->getAttrs().hasAttribute<ObjCAttr>() || proto->isObjC())
        return TypeWalker::Action::Stop;

      // Keep looking.
      return TypeWalker::Action::Continue;
    });

  return ProtocolDeclBits.RequiresClass;
}

bool ProtocolDecl::existentialConformsToSelfSlow() {
  // Assume for now that the existential conforms to itself; this
  // prevents circularity issues.
  ProtocolDeclBits.ExistentialConformsToSelfValid = true;
  ProtocolDeclBits.ExistentialConformsToSelf = true;

  if (isSpecificProtocol(KnownProtocolKind::AnyObject))
    return true;

  if (!isObjC()) {
    ProtocolDeclBits.ExistentialConformsToSelf = false;
    return false;
  }

  // Check whether this protocol conforms to itself.
  for (auto member : getMembers()) {
    if (member->isInvalid())
      continue;

    if (auto vd = dyn_cast<ValueDecl>(member)) {
      if (!vd->isInstanceMember()) {
        // A protocol cannot conform to itself if it has static members.
        ProtocolDeclBits.ExistentialConformsToSelf = false;
        return false;
      }
    }
  }

  // Check whether any of the inherited protocols fail to conform to
  // themselves.
  for (auto proto : getInheritedProtocols()) {
    if (!proto->existentialConformsToSelf()) {
      ProtocolDeclBits.ExistentialConformsToSelf = false;
      return false;
    }
  }
  return true;
}

/// Classify usages of Self in the given type.
static SelfReferenceKind
findProtocolSelfReferences(const ProtocolDecl *proto, Type type,
                           bool skipAssocTypes) {
  // Tuples preserve variance.
  if (auto tuple = type->getAs<TupleType>()) {
    auto kind = SelfReferenceKind::None();
    for (auto &elt: tuple->getElements()) {
      kind |= findProtocolSelfReferences(proto, elt.getType(),
                                         skipAssocTypes);
    }
    return kind;
  } 

  // Function preserve variance in the result type, and flip variance in
  // the parameter type.
  if (auto funcTy = type->getAs<AnyFunctionType>()) {
    auto inputKind = findProtocolSelfReferences(proto, funcTy->getInput(),
                                                skipAssocTypes);
    auto resultKind = findProtocolSelfReferences(proto, funcTy->getResult(),
                                                 skipAssocTypes);

    auto kind = inputKind.flip();
    kind |= resultKind;
    return kind;
  }

  // Metatypes preserve variance.
  if (auto metaTy = type->getAs<MetatypeType>()) {
    return findProtocolSelfReferences(proto, metaTy->getInstanceType(),
                                      skipAssocTypes);
  }

  // Optionals preserve variance.
  if (auto optType = type->getAnyOptionalObjectType()) {
    return findProtocolSelfReferences(proto, optType,
                                      skipAssocTypes);
  }

  // DynamicSelfType preserves variance.
  // FIXME: This shouldn't ever appear in protocol requirement
  // signatures.
  if (auto selfType = type->getAs<DynamicSelfType>()) {
    return findProtocolSelfReferences(proto, selfType->getSelfType(),
                                      skipAssocTypes);
  }

  // InOut types are invariant.
  if (auto inOutType = type->getAs<InOutType>()) {
    if (findProtocolSelfReferences(proto, inOutType->getObjectType(),
                                   skipAssocTypes)) {
      return SelfReferenceKind::Other();
    }
  }

  // Bound generic types are invariant.
  if (auto boundGenericType = type->getAs<BoundGenericType>()) {
    for (auto paramType : boundGenericType->getGenericArgs()) {
      if (findProtocolSelfReferences(proto, paramType,
                                     skipAssocTypes)) {
        return SelfReferenceKind::Other();
      }
    }
  }

  // A direct reference to 'Self' is covariant.
  if (proto->getProtocolSelfType()->isEqual(type))
    return SelfReferenceKind::Result();

  // Special handling for associated types.
  if (!skipAssocTypes && type->is<DependentMemberType>()) {
    type = type->getRootGenericParam();
    if (proto->getProtocolSelfType()->isEqual(type))
      return SelfReferenceKind::Other();
  }

  return SelfReferenceKind::None();
}

/// Find Self references within the given requirement.
SelfReferenceKind
ProtocolDecl::findProtocolSelfReferences(const ValueDecl *value,
                                         bool allowCovariantParameters,
                                         bool skipAssocTypes) const {
  // Types never refer to 'Self'.
  if (isa<TypeDecl>(value))
    return SelfReferenceKind::None();

  auto type = value->getInterfaceType();

  // FIXME: Deal with broken recursion.
  if (!type)
    return SelfReferenceKind::None();

  // Skip invalid declarations.
  if (type->hasError())
    return SelfReferenceKind::None();

  if (isa<AbstractFunctionDecl>(value)) {
    // Skip the 'self' parameter.
    type = type->castTo<AnyFunctionType>()->getResult();

    // Methods of non-final classes can only contain a covariant 'Self'
    // as a function result type.
    if (!allowCovariantParameters) {
      auto inputType = type->castTo<AnyFunctionType>()->getInput();
      auto inputKind = ::findProtocolSelfReferences(this, inputType,
                                                    skipAssocTypes);
      if (inputKind.parameter)
        return SelfReferenceKind::Other();
    }

    return ::findProtocolSelfReferences(this, type,
                                        skipAssocTypes);
  } else if (isa<SubscriptDecl>(value)) {
    return ::findProtocolSelfReferences(this, type,
                                        skipAssocTypes);
  } else {
    if (::findProtocolSelfReferences(this, type,
                                     skipAssocTypes)) {
      return SelfReferenceKind::Other();
    }
    return SelfReferenceKind::None();
  }
}

bool ProtocolDecl::isAvailableInExistential(const ValueDecl *decl) const {
  // If the member type uses 'Self' in non-covariant position,
  // we cannot use the existential type.
  auto selfKind = findProtocolSelfReferences(decl,
                                             /*allowCovariantParameters=*/true,
                                             /*skipAssocTypes=*/false);
  if (selfKind.parameter || selfKind.other)
    return false;

  return true;
}

bool ProtocolDecl::existentialTypeSupportedSlow(LazyResolver *resolver) {
  // Assume for now that the existential type is supported; this
  // prevents circularity issues.
  ProtocolDeclBits.ExistentialTypeSupportedValid = true;
  ProtocolDeclBits.ExistentialTypeSupported = true;

  // Resolve the protocol's type.
  if (resolver && !hasInterfaceType())
    resolver->resolveDeclSignature(this);

  for (auto member : getMembers()) {
    if (auto vd = dyn_cast<ValueDecl>(member)) {
      if (resolver && !vd->hasInterfaceType())
        resolver->resolveDeclSignature(vd);
    }

    if (member->isInvalid())
      continue;

    // Check for associated types.
    if (isa<AssociatedTypeDecl>(member)) {
      // An existential type cannot be used if the protocol has an
      // associated type.
      ProtocolDeclBits.ExistentialTypeSupported = false;
      return false;
    }

    // For value members, look at their type signatures.
    if (auto valueMember = dyn_cast<ValueDecl>(member)) {
      // materializeForSet has a funny type signature.
      if (auto func = dyn_cast<FuncDecl>(member)) {
        if (func->getAccessorKind() == AccessorKind::IsMaterializeForSet)
          continue;
      }

      if (!isAvailableInExistential(valueMember)) {
        ProtocolDeclBits.ExistentialTypeSupported = false;
        return false;
      }
    }
  }

  // Check whether all of the inherited protocols can have existential
  // types themselves.
  for (auto proto : getInheritedProtocols()) {
    if (!proto->existentialTypeSupported(resolver)) {
      ProtocolDeclBits.ExistentialTypeSupported = false;
      return false;
    }
  }
  return true;
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

GenericParamList *ProtocolDecl::createGenericParams(DeclContext *dc) {
  auto *outerGenericParams = dc->getParent()->getGenericParamsOfContext();

  // The generic parameter 'Self'.
  auto &ctx = getASTContext();
  auto selfId = ctx.Id_Self;
  auto selfDecl = new (ctx) GenericTypeParamDecl(
      dc, selfId,
      SourceLoc(),
      GenericTypeParamDecl::InvalidDepth, /*index=*/0);
  auto protoType = getDeclaredType();
  TypeLoc selfInherited[1] = { TypeLoc::withoutLoc(protoType) };
  selfDecl->setInherited(ctx.AllocateCopy(selfInherited));
  selfDecl->setImplicit();

  // The generic parameter list itself.
  auto result = GenericParamList::create(ctx, SourceLoc(), selfDecl,
                                         SourceLoc());
  result->setOuterParameters(outerGenericParams);
  return result;
}

void ProtocolDecl::createGenericParamsIfMissing() {
  if (!getGenericParams())
    setGenericParams(createGenericParams(this));
}

void ProtocolDecl::computeRequirementSignature() {
  assert(!RequirementSignature && "already computed requirement signature");

  auto module = getParentModule();

  auto genericSig = getGenericSignature();
  // The signature should look like <Self where Self : ThisProtocol>, and we
  // reuse the two parts of it because the parameter and the requirement are
  // exactly what we need.
  auto validSig = genericSig->getGenericParams().size() == 1 &&
                  genericSig->getRequirements().size() == 1;
  if (!validSig) {
    // This doesn't look like a protocol we can handle, so some other error must
    // have occurred (usually a protocol nested within another declaration)
    return;
  }

  auto selfType = genericSig->getGenericParams()[0];
  auto requirement = genericSig->getRequirements()[0];

  GenericSignatureBuilder builder(getASTContext(),
                                  LookUpConformanceInModule(module));
  builder.addGenericParameter(selfType);
  builder.addRequirement(
         requirement,
         GenericSignatureBuilder::RequirementSource
          ::forRequirementSignature(builder.resolveArchetype(selfType), this));
  builder.finalize(SourceLoc(), { selfType });
  
  RequirementSignature = builder.getGenericSignature();
}

/// Returns the default witness for a requirement, or nullptr if there is
/// no default.
Witness ProtocolDecl::getDefaultWitness(ValueDecl *requirement) const {
  loadAllMembers();

  auto found = DefaultWitnesses.find(requirement);
  if (found == DefaultWitnesses.end())
    return Witness();
  return found->second;
}

/// Record the default witness for a requirement.
void ProtocolDecl::setDefaultWitness(ValueDecl *requirement, Witness witness) {
  assert(witness);
  // The first type we insert a default witness, register a destructor for
  // this type.
  if (DefaultWitnesses.empty())
    getASTContext().addDestructorCleanup(DefaultWitnesses);
  auto pair = DefaultWitnesses.insert(std::make_pair(requirement, witness));
  assert(pair.second && "Already have a default witness!");
  (void) pair;
}

/// \brief Return true if the 'getter' is mutating, i.e. that it requires an
/// lvalue base to be accessed.
bool AbstractStorageDecl::isGetterMutating() const {
  switch (getStorageKind()) {
  case AbstractStorageDecl::Stored:
    return false;
    
  case AbstractStorageDecl::StoredWithObservers:
  case AbstractStorageDecl::StoredWithTrivialAccessors:
  case AbstractStorageDecl::InheritedWithObservers:
  case AbstractStorageDecl::ComputedWithMutableAddress:
  case AbstractStorageDecl::Computed:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::AddressedWithObservers:
    assert(getGetter());
    return getGetter()->isMutating();
    
  case AbstractStorageDecl::Addressed:
    assert(getAddressor());
    return getAddressor()->isMutating();
  }

  llvm_unreachable("Unhandled AbstractStorageDecl in switch.");
}

/// \brief Return true if the 'setter' is nonmutating, i.e. that it can be
/// called even on an immutable base value.
bool AbstractStorageDecl::isSetterNonMutating() const {
  // Setters declared in reference type contexts are never mutating.
  if (auto contextType = getDeclContext()->getDeclaredInterfaceType()) {
    if (contextType->hasReferenceSemantics())
      return true;
  }

  switch (getStorageKind()) {
  case AbstractStorageDecl::Stored:
  case AbstractStorageDecl::StoredWithTrivialAccessors:
    return false;
    
  case AbstractStorageDecl::StoredWithObservers:
  case AbstractStorageDecl::InheritedWithObservers:
  case AbstractStorageDecl::Computed:
    assert(getSetter());
    return !getSetter()->isMutating();
    
  case AbstractStorageDecl::Addressed:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::AddressedWithObservers:
  case AbstractStorageDecl::ComputedWithMutableAddress:
    assert(getMutableAddressor());
    return !getMutableAddressor()->isMutating();
  }
  llvm_unreachable("bad storage kind");
}

FuncDecl *AbstractStorageDecl::getAccessorFunction(AccessorKind kind) const {
  switch (kind) {
  case AccessorKind::IsGetter: return getGetter();
  case AccessorKind::IsSetter: return getSetter();
  case AccessorKind::IsMaterializeForSet: return getMaterializeForSetFunc();
  case AccessorKind::IsAddressor: return getAddressor();
  case AccessorKind::IsMutableAddressor: return getMutableAddressor();
  case AccessorKind::IsDidSet: return getDidSetFunc();
  case AccessorKind::IsWillSet: return getWillSetFunc();
  case AccessorKind::NotAccessor: llvm_unreachable("called with NotAccessor");
  }
  llvm_unreachable("bad accessor kind!");
}

void AbstractStorageDecl::configureGetSetRecord(GetSetRecord *getSetInfo,
                                                FuncDecl *getter,
                                                FuncDecl *setter,
                                                FuncDecl *materializeForSet) {
  getSetInfo->Get = getter;
  if (getter)
    getter->makeAccessor(this, AccessorKind::IsGetter);

  configureSetRecord(getSetInfo, setter, materializeForSet);
}

void AbstractStorageDecl::configureSetRecord(GetSetRecord *getSetInfo,
                                             FuncDecl *setter,
                                             FuncDecl *materializeForSet) {
  getSetInfo->Set = setter;
  getSetInfo->MaterializeForSet = materializeForSet;

  auto setSetterAccess = [&](FuncDecl *fn) {
    if (auto setterAccess = GetSetInfo.getInt()) {
      assert(!fn->hasAccessibility() ||
             fn->getFormalAccess() == setterAccess.getValue());
      fn->overwriteAccessibility(setterAccess.getValue());
    }    
  };

  if (setter) {
    setter->makeAccessor(this, AccessorKind::IsSetter);
    setSetterAccess(setter);
  }

  if (materializeForSet) {
    materializeForSet->makeAccessor(this, AccessorKind::IsMaterializeForSet);
    setSetterAccess(materializeForSet);
  }
}

void AbstractStorageDecl::configureAddressorRecord(AddressorRecord *record,
                                                   FuncDecl *addressor,
                                                   FuncDecl *mutableAddressor) {
  record->Address = addressor;
  record->MutableAddress = mutableAddressor;

  if (addressor) {
    addressor->makeAccessor(this, AccessorKind::IsAddressor);
  }

  if (mutableAddressor) {
    mutableAddressor->makeAccessor(this, AccessorKind::IsMutableAddressor);
  }
}

void AbstractStorageDecl::configureObservingRecord(ObservingRecord *record,
                                                   FuncDecl *willSet,
                                                   FuncDecl *didSet) {
  record->WillSet = willSet;
  record->DidSet = didSet;

  if (willSet) {
    willSet->makeAccessor(this, AccessorKind::IsWillSet);
  }

  if (didSet) {
    didSet->makeAccessor(this, AccessorKind::IsDidSet);
  }
}

void AbstractStorageDecl::makeComputed(SourceLoc LBraceLoc,
                                       FuncDecl *Get, FuncDecl *Set,
                                       FuncDecl *MaterializeForSet,
                                       SourceLoc RBraceLoc) {
  assert(getStorageKind() == Stored && "StorageKind already set");
  auto &Context = getASTContext();
  void *Mem = Context.Allocate(sizeof(GetSetRecord), alignof(GetSetRecord));
  auto *getSetInfo = new (Mem) GetSetRecord();
  getSetInfo->Braces = SourceRange(LBraceLoc, RBraceLoc);
  GetSetInfo.setPointer(getSetInfo);
  configureGetSetRecord(getSetInfo, Get, Set, MaterializeForSet);

  // Mark that this is a computed property.
  setStorageKind(Computed);
}

void AbstractStorageDecl::setComputedSetter(FuncDecl *Set) {
  assert(getStorageKind() == Computed && "Not a computed variable");
  assert(getGetter() && "sanity check: missing getter");
  assert(!getSetter() && "already has a setter");
  assert(hasClangNode() && "should only be used for ObjC properties");
  assert(Set && "should not be called for readonly properties");
  GetSetInfo.getPointer()->Set = Set;
  Set->makeAccessor(this, AccessorKind::IsSetter);
  if (auto setterAccess = GetSetInfo.getInt()) {
    assert(!Set->hasAccessibility() ||
           Set->getFormalAccess() == setterAccess.getValue());
    Set->overwriteAccessibility(setterAccess.getValue());
  }
}

void AbstractStorageDecl::addBehavior(TypeRepr *Type,
                                      Expr *Param) {
  assert(BehaviorInfo.getPointer() == nullptr && "already set behavior!");
  auto mem = getASTContext().Allocate(sizeof(BehaviorRecord),
                                      alignof(BehaviorRecord));
  auto behavior = new (mem) BehaviorRecord{Type, Param};
  BehaviorInfo.setPointer(behavior);
}

void AbstractStorageDecl::makeComputedWithMutableAddress(SourceLoc lbraceLoc,
                                                FuncDecl *get, FuncDecl *set,
                                                FuncDecl *materializeForSet,
                                                FuncDecl *mutableAddressor,
                                                SourceLoc rbraceLoc) {
  assert(getStorageKind() == Stored && "StorageKind already set");
  assert(get);
  assert(mutableAddressor);
  auto &ctx = getASTContext();

  static_assert(alignof(AddressorRecord) == alignof(GetSetRecord),
                "inconsistent alignment");
  void *mem = ctx.Allocate(sizeof(AddressorRecord) + sizeof(GetSetRecord),
                           alignof(AddressorRecord));
  auto addressorInfo = new (mem) AddressorRecord();
  auto info = new (addressorInfo->getGetSet()) GetSetRecord();
  info->Braces = SourceRange(lbraceLoc, rbraceLoc);
  GetSetInfo.setPointer(info);
  setStorageKind(ComputedWithMutableAddress);

  configureAddressorRecord(addressorInfo, nullptr, mutableAddressor);
  configureGetSetRecord(info, get, set, materializeForSet);
}

void AbstractStorageDecl::setMaterializeForSetFunc(FuncDecl *accessor) {
  assert(hasAccessorFunctions() && "No accessors for declaration!");
  assert(getSetter() && "declaration is not settable");
  assert(!getMaterializeForSetFunc() && "already has a materializeForSet");
  GetSetInfo.getPointer()->MaterializeForSet = accessor;
  accessor->makeAccessor(this, AccessorKind::IsMaterializeForSet);
  if (auto setterAccess = GetSetInfo.getInt()) {
    assert(!accessor->hasAccessibility() ||
           accessor->getFormalAccess() == setterAccess.getValue());
    accessor->overwriteAccessibility(setterAccess.getValue());
  }
}

/// \brief Turn this into a StoredWithTrivialAccessors var, specifying the
/// accessors (getter and setter) that go with it.
void AbstractStorageDecl::addTrivialAccessors(FuncDecl *Get,
                                 FuncDecl *Set, FuncDecl *MaterializeForSet) {
  assert((getStorageKind() == Stored ||
          getStorageKind() == Addressed) && "StorageKind already set");
  assert(Get);

  auto &ctx = getASTContext();
  GetSetRecord *getSetInfo;
  if (getStorageKind() == Addressed) {
    getSetInfo = GetSetInfo.getPointer();
    setStorageKind(AddressedWithTrivialAccessors);
  } else {
    void *mem = ctx.Allocate(sizeof(GetSetRecord), alignof(GetSetRecord));
    getSetInfo = new (mem) GetSetRecord();
    getSetInfo->Braces = SourceRange();
    GetSetInfo.setPointer(getSetInfo);
    setStorageKind(StoredWithTrivialAccessors);
  }
  configureGetSetRecord(getSetInfo, Get, Set, MaterializeForSet);
}

void AbstractStorageDecl::makeAddressed(SourceLoc lbraceLoc, FuncDecl *addressor,
                                        FuncDecl *mutableAddressor,
                                        SourceLoc rbraceLoc) {
  assert(getStorageKind() == Stored && "StorageKind already set");
  assert(addressor && "addressed mode, but no addressor function?");

  auto &ctx = getASTContext();

  static_assert(alignof(AddressorRecord) == alignof(GetSetRecord),
                "inconsistent alignment");
  void *mem = ctx.Allocate(sizeof(AddressorRecord) + sizeof(GetSetRecord),
                           alignof(AddressorRecord));
  auto addressorInfo = new (mem) AddressorRecord();
  auto info = new (addressorInfo->getGetSet()) GetSetRecord();
  info->Braces = SourceRange(lbraceLoc, rbraceLoc);
  GetSetInfo.setPointer(info);
  setStorageKind(Addressed);

  configureAddressorRecord(addressorInfo, addressor, mutableAddressor);
}

void AbstractStorageDecl::makeStoredWithObservers(SourceLoc LBraceLoc,
                                                  FuncDecl *WillSet,
                                                  FuncDecl *DidSet,
                                                  SourceLoc RBraceLoc) {
  assert(getStorageKind() == Stored && "VarDecl StorageKind already set");
  assert((WillSet || DidSet) &&
         "Can't be Observing without one or the other");
  auto &Context = getASTContext();
  void *Mem = Context.Allocate(sizeof(ObservingRecord),
                               alignof(ObservingRecord));
  auto *observingInfo = new (Mem) ObservingRecord;
  observingInfo->Braces = SourceRange(LBraceLoc, RBraceLoc);
  GetSetInfo.setPointer(observingInfo);

  // Mark that this is an observing property.
  setStorageKind(StoredWithObservers);

  configureObservingRecord(observingInfo, WillSet, DidSet);
}

void AbstractStorageDecl::makeInheritedWithObservers(SourceLoc lbraceLoc,
                                                     FuncDecl *willSet,
                                                     FuncDecl *didSet,
                                                     SourceLoc rbraceLoc) {
  assert(getStorageKind() == Stored && "StorageKind already set");
  assert((willSet || didSet) &&
         "Can't be Observing without one or the other");
  auto &ctx = getASTContext();
  void *mem = ctx.Allocate(sizeof(ObservingRecord), alignof(ObservingRecord));
  auto *observingInfo = new (mem) ObservingRecord;
  observingInfo->Braces = SourceRange(lbraceLoc, rbraceLoc);
  GetSetInfo.setPointer(observingInfo);

  // Mark that this is an observing property.
  setStorageKind(InheritedWithObservers);

  configureObservingRecord(observingInfo, willSet, didSet);
}

void AbstractStorageDecl::makeAddressedWithObservers(SourceLoc lbraceLoc,
                                                     FuncDecl *addressor,
                                                     FuncDecl *mutableAddressor,
                                                     FuncDecl *willSet,
                                                     FuncDecl *didSet,
                                                     SourceLoc rbraceLoc) {
  assert(getStorageKind() == Stored && "VarDecl StorageKind already set");
  assert(addressor);
  assert(mutableAddressor && "observing but immutable?");
  assert((willSet || didSet) &&
         "Can't be Observing without one or the other");

  auto &ctx = getASTContext();
  static_assert(alignof(AddressorRecord) == alignof(ObservingRecord),
                "inconsistent alignment");
  void *mem = ctx.Allocate(sizeof(AddressorRecord) + sizeof(ObservingRecord),
                           alignof(AddressorRecord));
  auto addressorInfo = new (mem) AddressorRecord();
  auto observerInfo = new (addressorInfo->getGetSet()) ObservingRecord();
  observerInfo->Braces = SourceRange(lbraceLoc, rbraceLoc);
  GetSetInfo.setPointer(observerInfo);
  setStorageKind(AddressedWithObservers);

  configureAddressorRecord(addressorInfo, addressor, mutableAddressor);
  configureObservingRecord(observerInfo, willSet, didSet);
}

/// \brief Specify the synthesized get/set functions for a Observing var.
/// This is used by Sema.
void AbstractStorageDecl::setObservingAccessors(FuncDecl *Get,
                                                FuncDecl *Set,
                                                FuncDecl *MaterializeForSet) {
  assert(hasObservers() && "VarDecl is wrong type");
  assert(!getGetter() && !getSetter() && "getter and setter already set");
  assert(Get && Set && "Must specify getter and setter");
  configureGetSetRecord(GetSetInfo.getPointer(), Get, Set, MaterializeForSet);
}

void AbstractStorageDecl::setInvalidBracesRange(SourceRange BracesRange) {
  assert(!GetSetInfo.getPointer() && "Braces range has already been set");

  auto &Context = getASTContext();
  void *Mem = Context.Allocate(sizeof(GetSetRecord), alignof(GetSetRecord));
  auto *getSetInfo = new (Mem) GetSetRecord();
  getSetInfo->Braces = BracesRange;
  getSetInfo->Get = nullptr;
  getSetInfo->Set = nullptr;
  getSetInfo->MaterializeForSet = nullptr;
  GetSetInfo.setPointer(getSetInfo);
}

static Optional<ObjCSelector>
getNameFromObjcAttribute(const ObjCAttr *attr, DeclName preferredName) {
  if (!attr)
    return None;
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
  return None;
}

ObjCSelector AbstractStorageDecl::getObjCGetterSelector(
               LazyResolver *resolver, Identifier preferredName) const {
  // If the getter has an @objc attribute with a name, use that.
  if (auto getter = getGetter()) {
      if (auto name = getNameFromObjcAttribute(getter->getAttrs().
          getAttribute<ObjCAttr>(), preferredName))
        return *name;
  }

  // Subscripts use a specific selector.
  auto &ctx = getASTContext();
  if (auto *SD = dyn_cast<SubscriptDecl>(this)) {
    switch (SD->getObjCSubscriptKind(resolver)) {
    case ObjCSubscriptKind::None:
      llvm_unreachable("Not an Objective-C subscript");
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

ObjCSelector AbstractStorageDecl::getObjCSetterSelector(
               LazyResolver *resolver, Identifier preferredName) const {
  // If the setter has an @objc attribute with a name, use that.
  auto setter = getSetter();
  auto objcAttr = setter ? setter->getAttrs().getAttribute<ObjCAttr>()
                         : nullptr;
  if (auto name = getNameFromObjcAttribute(objcAttr, DeclName(preferredName))) {
    return *name;
  }

  // Subscripts use a specific selector.
  auto &ctx = getASTContext();
  if (auto *SD = dyn_cast<SubscriptDecl>(this)) {
    switch (SD->getObjCSubscriptKind(resolver)) {
    case ObjCSubscriptKind::None:
      llvm_unreachable("Not an Objective-C subscript");

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

static bool isSettable(const AbstractStorageDecl *decl) {
  switch (decl->getStorageKind()) {
  case AbstractStorageDecl::Stored:
    return true;

  case AbstractStorageDecl::StoredWithTrivialAccessors:
    return decl->getSetter() != nullptr;

  case AbstractStorageDecl::Addressed:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
    return decl->getMutableAddressor() != nullptr;

  case AbstractStorageDecl::StoredWithObservers:
  case AbstractStorageDecl::InheritedWithObservers:
  case AbstractStorageDecl::AddressedWithObservers:
  case AbstractStorageDecl::ComputedWithMutableAddress:
    return true;

  case AbstractStorageDecl::Computed:
    return decl->getSetter() != nullptr;
  }
  llvm_unreachable("bad storage kind");
}

void VarDecl::setType(Type t) {
  typeInContext = t;
  if (t && t->hasError())
    setInvalid();
}

void VarDecl::markInvalid() {
  auto &Ctx = getASTContext();
  setType(ErrorType::get(Ctx));
  setInterfaceType(ErrorType::get(Ctx));
  setInvalid();
}

/// \brief Returns whether the var is settable in the specified context: this
/// is either because it is a stored var, because it has a custom setter, or
/// is a let member in an initializer.
bool VarDecl::isSettable(const DeclContext *UseDC,
                         const DeclRefExpr *base) const {
  // If this is a 'var' decl, then we're settable if we have storage or a
  // setter.
  if (!isLet())
    return ::isSettable(this);

  // If the decl has a value bound to it but has no PBD, then it is
  // initialized.
  if (hasNonPatternBindingInit())
    return false;
  
  // 'let' parameters are never settable.
  if (isa<ParamDecl>(this))
    return false;
  
  // Properties in structs/classes are only ever mutable in their designated
  // initializer(s).
  if (isInstanceMember()) {
    auto *CD = dyn_cast_or_null<ConstructorDecl>(UseDC);
    if (!CD) return false;
    
    auto *CDC = CD->getDeclContext();
      
    // If this init is defined inside of the same type (or in an extension
    // thereof) as the let property, then it is mutable.
    if (!CDC->isTypeContext() ||
        CDC->getAsNominalTypeOrNominalTypeExtensionContext() !=
        getDeclContext()->getAsNominalTypeOrNominalTypeExtensionContext())
      return false;

    if (base && CD->getImplicitSelfDecl() != base->getDecl())
      return false;

    // If this is a convenience initializer (i.e. one that calls
    // self.init), then let properties are never mutable in it.  They are
    // only mutable in designated initializers.
    if (CD->getDelegatingOrChainedInitKind(nullptr) ==
        ConstructorDecl::BodyInitKind::Delegating)
      return false;

    return true;
  }

  // If the decl has an explicitly written initializer with a pattern binding,
  // then it isn't settable.
  if (getParentInitializer() != nullptr)
    return false;

  // Normal lets (e.g. globals) are only mutable in the context of the
  // declaration.  To handle top-level code properly, we look through
  // the TopLevelCode decl on the use (if present) since the vardecl may be
  // one level up.
  if (getDeclContext() == UseDC)
    return true;

  if (UseDC && isa<TopLevelCodeDecl>(UseDC) &&
      getDeclContext() == UseDC->getParent())
    return true;

  return false;
}

bool SubscriptDecl::isSettable() const {
  return ::isSettable(this);
}

SourceRange VarDecl::getSourceRange() const {
  if (auto Param = dyn_cast<ParamDecl>(this))
    return Param->getSourceRange();
  return getNameLoc();
}

SourceRange VarDecl::getTypeSourceRangeForDiagnostics() const {
  // For a parameter, map back to its parameter to get the TypeLoc.
  if (auto *PD = dyn_cast<ParamDecl>(this)) {
    if (auto typeRepr = PD->getTypeLoc().getTypeRepr())
      return typeRepr->getSourceRange();
  }
  
  Pattern *Pat = getParentPattern();
  if (!Pat || Pat->isImplicit())
    return SourceRange();

  if (auto *VP = dyn_cast<VarPattern>(Pat))
    Pat = VP->getSubPattern();
  if (auto *TP = dyn_cast<TypedPattern>(Pat))
    if (auto typeRepr = TP->getTypeLoc().getTypeRepr())
      return typeRepr->getSourceRange();

  return SourceRange();
}

static bool isVarInPattern(const VarDecl *VD, Pattern *P) {
  bool foundIt = false;
  P->forEachVariable([&](VarDecl *FoundFD) {
    foundIt |= FoundFD == VD;
  });
  return foundIt;
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
  if (auto *PBD = getParentPatternBinding())
    return PBD->getPatternEntryForVarDecl(this).getPattern();
  
  // If this is a statement parent, dig the pattern out of it.
  if (auto *stmt = getParentPatternStmt()) {
    if (auto *FES = dyn_cast<ForEachStmt>(stmt))
      return FES->getPattern();
    
    if (auto *CS = dyn_cast<CatchStmt>(stmt))
      return CS->getErrorPattern();
    
    if (auto *cs = dyn_cast<CaseStmt>(stmt)) {
      // In a case statement, search for the pattern that contains it.  This is
      // a bit silly, because you can't have something like "case x, y:" anyway.
      for (auto items : cs->getCaseLabelItems()) {
        if (isVarInPattern(this, items.getPattern()))
          return items.getPattern();
      }
    } else if (auto *LCS = dyn_cast<LabeledConditionalStmt>(stmt)) {
      for (auto &elt : LCS->getCond())
        if (auto pat = elt.getPatternOrNull())
          if (isVarInPattern(this, pat))
            return pat;
    }
    
    //stmt->dump();
    assert(0 && "Unknown parent pattern statement?");
  }
  
  return nullptr;
}

bool VarDecl::isSelfParameter() const {
  if (isa<ParamDecl>(this))
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(getDeclContext()))
      return AFD->getImplicitSelfDecl() == this;

  return false;
}

/// Return true if this stored property needs to be accessed with getters and
/// setters for Objective-C.
bool AbstractStorageDecl::hasForeignGetterAndSetter() const {
  if (auto override = getOverriddenDecl())
    return override->hasForeignGetterAndSetter();

  if (!isObjC())
    return false;

  return true;
}

bool AbstractStorageDecl::requiresForeignGetterAndSetter() const {
  if (isFinal())
    return false;
  if (hasAccessorFunctions() && getGetter()->isImportAsMember())
    return true;
  if (!hasForeignGetterAndSetter())
    return false;
  // Imported accessors are foreign and only have objc entry points.
  if (hasClangNode())
    return true;
  // Otherwise, we only dispatch by @objc if the declaration is dynamic or
  // NSManaged.
  return getAttrs().hasAttribute<DynamicAttr>() ||
         getAttrs().hasAttribute<NSManagedAttr>();
}


bool VarDecl::isAnonClosureParam() const {
  auto name = getName();
  if (name.empty())
    return false;

  auto nameStr = name.str();
  if (nameStr.empty())
    return false;

  return nameStr[0] == '$';
}

StaticSpellingKind VarDecl::getCorrectStaticSpelling() const {
  if (!isStatic())
    return StaticSpellingKind::None;
  if (auto *PBD = getParentPatternBinding()) {
    if (PBD->getStaticSpelling() != StaticSpellingKind::None)
      return PBD->getStaticSpelling();
  }

  return getCorrectStaticSpellingForDecl(this);
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
      // Do not suggest the fix it in implicit getters
      if (FD->isGetter() && !FD->getAccessorKeywordLoc().isValid()) return;
                   
      auto &d = getASTContext().Diags;
      d.diagnose(FD->getFuncLoc(), diag::change_to_mutating, FD->isAccessor())
       .fixItInsert(FD->getFuncLoc(), "mutating ");
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

ParamDecl::ParamDecl(bool isLet,
                     SourceLoc letVarInOutLoc, SourceLoc argumentNameLoc,
                     Identifier argumentName, SourceLoc parameterNameLoc,
                     Identifier parameterName, Type ty, DeclContext *dc)
  : VarDecl(DeclKind::Param, /*IsStatic*/false, /*IsLet*/isLet,
            /*IsCaptureList*/false, parameterNameLoc, parameterName, ty, dc),
  ArgumentName(argumentName), ArgumentNameLoc(argumentNameLoc),
  LetVarInOutLoc(letVarInOutLoc) {
}

/// Clone constructor, allocates a new ParamDecl identical to the first.
/// Intentionally not defined as a copy constructor to avoid accidental copies.
ParamDecl::ParamDecl(ParamDecl *PD)
  : VarDecl(DeclKind::Param, /*IsStatic*/false, /*IsLet*/PD->isLet(),
            /*IsCaptureList*/false, PD->getNameLoc(), PD->getName(),
            PD->hasType() ? PD->getType() : Type(), PD->getDeclContext()),
    ArgumentName(PD->getArgumentName()),
    ArgumentNameLoc(PD->getArgumentNameLoc()),
    LetVarInOutLoc(PD->getLetVarInOutLoc()),
    DefaultValueAndIsVariadic(nullptr, PD->DefaultValueAndIsVariadic.getInt()),
    IsTypeLocImplicit(PD->IsTypeLocImplicit),
    defaultArgumentKind(PD->defaultArgumentKind) {
  typeLoc = PD->getTypeLoc().clone(PD->getASTContext());
  if (PD->hasInterfaceType())
    setInterfaceType(PD->getInterfaceType());
}


/// \brief Retrieve the type of 'self' for the given context.
Type DeclContext::getSelfTypeInContext() const {
  assert(isTypeContext());

  // For a protocol or extension thereof, the type is 'Self'.
  if (getAsProtocolOrProtocolExtensionContext())
    return mapTypeIntoContext(getProtocolSelfType());
  return getDeclaredTypeInContext();
}

/// \brief Retrieve the interface type of 'self' for the given context.
Type DeclContext::getSelfInterfaceType() const {
  assert(isTypeContext());

  // For a protocol or extension thereof, the type is 'Self'.
  if (getAsProtocolOrProtocolExtensionContext())
    return getProtocolSelfType();
  return getDeclaredInterfaceType();
}

/// Create an implicit 'self' decl for a method in the specified decl context.
/// If 'static' is true, then this is self for a static method in the type.
///
/// Note that this decl is created, but it is returned with an incorrect
/// DeclContext that needs to be set correctly.  This is automatically handled
/// when a function is created with this as part of its argument list.
/// For a generic context, this also gives the parameter an unbound generic
/// type with the expectation that type-checking will fill in the context
/// generic parameters.
ParamDecl *ParamDecl::createUnboundSelf(SourceLoc loc, DeclContext *DC) {
  ASTContext &C = DC->getASTContext();
  auto *selfDecl = new (C) ParamDecl(/*IsLet*/true, SourceLoc(), SourceLoc(),
                                     Identifier(), loc, C.Id_self, Type(), DC);
  selfDecl->setImplicit();
  return selfDecl;
}

/// Create an implicit 'self' decl for a method in the specified decl context.
/// If 'static' is true, then this is self for a static method in the type.
///
/// Note that this decl is created, but it is returned with an incorrect
/// DeclContext that needs to be set correctly.  This is automatically handled
/// when a function is created with this as part of its argument list.
/// For a generic context, this also gives the parameter an unbound generic
/// type with the expectation that type-checking will fill in the context
/// generic parameters.
ParamDecl *ParamDecl::createSelf(SourceLoc loc, DeclContext *DC,
                                 bool isStaticMethod, bool isInOut) {
  ASTContext &C = DC->getASTContext();
  auto selfType = DC->getSelfTypeInContext();
  auto selfInterfaceType = DC->getSelfInterfaceType();
  assert(selfType && selfInterfaceType);

  if (isStaticMethod) {
    selfType = MetatypeType::get(selfType);
    selfInterfaceType = MetatypeType::get(selfInterfaceType);
  }
    
  if (isInOut) {
    selfType = InOutType::get(selfType);
    selfInterfaceType = InOutType::get(selfInterfaceType);
  }

  auto *selfDecl = new (C) ParamDecl(/*IsLet*/!isInOut, SourceLoc(),SourceLoc(),
                                     Identifier(), loc, C.Id_self, selfType,DC);
  selfDecl->setImplicit();
  selfDecl->setInterfaceType(selfInterfaceType);
  return selfDecl;
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
  else {
    startLoc = getTypeLoc().getSourceRange().Start;
  }
  if (startLoc.isInvalid())
    return SourceRange();

  // It would be nice to extend the front of the range to show where inout is,
  // but we don't have that location info.  Extend the back of the range to the
  // location of the default argument, or the typeloc if they are valid.
  if (auto expr = getDefaultValue()) {
    auto endLoc = expr->getEndLoc();
    if (endLoc.isValid())
      return SourceRange(startLoc, endLoc);
  }
  
  // If the typeloc has a valid location, use it to end the range.
  if (auto typeRepr = getTypeLoc().getTypeRepr()) {
    auto endLoc = typeRepr->getEndLoc();
    if (endLoc.isValid() && !isTypeLocImplicit())
      return SourceRange(startLoc, endLoc);
  }

  // The name has a location we can use.
  if (nameLoc.isValid())
    return SourceRange(startLoc, nameLoc);

  return startLoc;
}

Type ParamDecl::getVarargBaseTy(Type VarArgT) {
  TypeBase *T = VarArgT.getPointer();
  if (ArraySliceType *AT = dyn_cast<ArraySliceType>(T))
    return AT->getBaseType();
  if (BoundGenericType *BGT = dyn_cast<BoundGenericType>(T)) {
    // It's the stdlib Array<T>.
    return BGT->getGenericArgs()[0];
  }
  assert(T->hasError());
  return T;
}

void ParamDecl::setDefaultValue(Expr *E) {
  if (!DefaultValueAndIsVariadic.getPointer()) {
    if (!E) return;

    DefaultValueAndIsVariadic.setPointer(
      getASTContext().Allocate<StoredDefaultArgument>());
  }

  DefaultValueAndIsVariadic.getPointer()->DefaultArg = E;
}

void ParamDecl::setDefaultArgumentInitContext(Initializer *initContext) {
  assert(DefaultValueAndIsVariadic.getPointer());
  DefaultValueAndIsVariadic.getPointer()->InitContext = initContext;
}

void DefaultArgumentInitializer::changeFunction(AbstractFunctionDecl *parent) {
  assert(parent->isLocalContext());
  setParent(parent);

  unsigned offset = getIndex();
  for (auto list : parent->getParameterLists()) {
    if (offset < list->size()) {
      auto param = list->get(offset);
      if (param->getDefaultValue())
        param->setDefaultArgumentInitContext(this);
      return;
    }

    offset -= list->size();
  }
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

Type SubscriptDecl::getIndicesInterfaceType() const {
  auto indicesTy = getInterfaceType();
  if (indicesTy->hasError())
    return indicesTy;
  return indicesTy->castTo<AnyFunctionType>()->getInput();
}

Type SubscriptDecl::getElementInterfaceType() const {
  auto elementTy = getInterfaceType();
  if (elementTy->hasError())
    return elementTy;
  return elementTy->castTo<AnyFunctionType>()->getResult();
}

ObjCSubscriptKind SubscriptDecl::getObjCSubscriptKind(
                    LazyResolver *resolver) const {
  auto indexTy = getIndicesInterfaceType();

  // Look through a named 1-tuple.
  indexTy = indexTy->getWithoutImmediateLabel();

  // If the index type is an integral type, we have an indexed
  // subscript.
  if (isIntegralType(indexTy))
    return ObjCSubscriptKind::Indexed;

  // If the index type is an object type in Objective-C, we have a
  // keyed subscript.
  if (Type objectTy = indexTy->getAnyOptionalObjectType())
    indexTy = objectTy;

  return ObjCSubscriptKind::Keyed;
}

SourceRange SubscriptDecl::getSourceRange() const {
  if (getBracesRange().isValid())
    return { getSubscriptLoc(), getBracesRange().End };
  return { getSubscriptLoc(), ElementTy.getSourceRange().End };
}

Type AbstractFunctionDecl::computeInterfaceSelfType(bool isInitializingCtor,
                                                    bool wantDynamicSelf) {
  auto *dc = getDeclContext();
  auto &Ctx = dc->getASTContext();
  
  // Determine the type of the container.
  auto containerTy = dc->getDeclaredInterfaceType();
  if (!containerTy || containerTy->hasError())
    return ErrorType::get(Ctx);

  // Determine the type of 'self' inside the container.
  auto selfTy = dc->getSelfInterfaceType();
  if (!selfTy || selfTy->hasError())
    return ErrorType::get(Ctx);

  bool isStatic = false;
  bool isMutating = false;

  if (auto *FD = dyn_cast<FuncDecl>(this)) {
    isStatic = FD->isStatic();
    isMutating = FD->isMutating();

    if (wantDynamicSelf && FD->hasDynamicSelf())
      selfTy = DynamicSelfType::get(selfTy, Ctx);
  } else if (isa<ConstructorDecl>(this)) {
    if (isInitializingCtor) {
      // initializing constructors of value types always have an implicitly
      // inout self.
      isMutating = true;
    } else {
      // allocating constructors have metatype 'self'.
      isStatic = true;
    }
  } else if (isa<DestructorDecl>(this)) {
    // destructors of value types always have an implicitly inout self.
    isMutating = true;
  }

  // 'static' functions have 'self' of type metatype<T>.
  if (isStatic)
    return MetatypeType::get(selfTy, Ctx);

  // Reference types have 'self' of type T.
  if (containerTy->hasReferenceSemantics())
    return selfTy;

  // Mutating methods are always passed inout so we can receive the side
  // effect.
  if (isMutating)
    return InOutType::get(selfTy);

  // Nonmutating methods on structs and enums pass the receiver by value.
  return selfTy;
}

DeclName AbstractFunctionDecl::getEffectiveFullName() const {
  if (getFullName())
    return getFullName();

  if (auto func = dyn_cast<FuncDecl>(this)) {
    if (auto afd = func->getAccessorStorageDecl()) {
      auto &ctx = getASTContext();
      auto subscript = dyn_cast<SubscriptDecl>(afd);
      switch (auto accessorKind = func->getAccessorKind()) {
        case AccessorKind::NotAccessor:
          break;

        // These don't have any extra implicit parameters.
        case AccessorKind::IsAddressor:
        case AccessorKind::IsMutableAddressor:
        case AccessorKind::IsGetter:
          return subscript ? subscript->getFullName()
                           : DeclName(ctx, afd->getName(),
                                      ArrayRef<Identifier>());

        case AccessorKind::IsSetter:
        case AccessorKind::IsMaterializeForSet:
        case AccessorKind::IsDidSet:
        case AccessorKind::IsWillSet: {
          SmallVector<Identifier, 4> argNames;
          // The implicit value/buffer parameter.
          argNames.push_back(Identifier());
          // The callback storage parameter on materializeForSet.
          if (accessorKind  == AccessorKind::IsMaterializeForSet)
            argNames.push_back(Identifier());
          // The subscript index parameters.
          if (subscript) {
            argNames.append(subscript->getFullName().getArgumentNames().begin(),
                            subscript->getFullName().getArgumentNames().end());
          }
          return DeclName(ctx, afd->getName(), argNames);
        }
      }
    }
  }

  return DeclName();
}

/// \brief This method returns the implicit 'self' decl.
///
/// Note that some functions don't have an implicit 'self' decl, for example,
/// free functions.  In this case nullptr is returned.
ParamDecl *AbstractFunctionDecl::getImplicitSelfDecl() {
  if (!getDeclContext()->isTypeContext())
    return nullptr;

  // "self" is always the first parameter list.
  auto paramLists = getParameterLists();
  assert(paramLists.size() >= 1);
  assert(paramLists[0]->size() == 1);

  auto selfParam = paramLists[0]->get(0);
  assert(selfParam->getName() == getASTContext().Id_self);
  assert(selfParam->isImplicit());

  return selfParam;
}

std::pair<DefaultArgumentKind, Type>
AbstractFunctionDecl::getDefaultArg(unsigned Index) const {
  auto paramLists = getParameterLists();

  if (getImplicitSelfDecl()) // Skip the 'self' parameter; it is not counted.
    paramLists = paramLists.slice(1);

  for (auto paramList : paramLists) {
    if (Index < paramList->size()) {
      auto param = paramList->get(Index);
      return { param->getDefaultArgumentKind(), param->getInterfaceType() };
    }
    
    Index -= paramList->size();
  }

  llvm_unreachable("Invalid parameter index");
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

SourceRange AbstractFunctionDecl::getBodySourceRange() const {
  switch (getBodyKind()) {
  case BodyKind::None:
  case BodyKind::MemberwiseInitializer:
    return SourceRange();

  case BodyKind::Parsed:
  case BodyKind::Synthesize:
  case BodyKind::TypeChecked:
    if (auto body = getBody())
      return body->getSourceRange();

    return SourceRange();

  case BodyKind::Skipped:
  case BodyKind::Unparsed:
    return BodyRange;
  }
  llvm_unreachable("bad BodyKind");
}

SourceRange AbstractFunctionDecl::getSignatureSourceRange() const {
  if (isImplicit())
    return SourceRange();

  auto paramLists = getParameterLists();
  if (paramLists.empty())
    return getNameLoc();

  for (auto *paramList : reversed(paramLists)) {
    auto endLoc = paramList->getSourceRange().End;
    if (endLoc.isValid())
      return SourceRange(getNameLoc(), endLoc);
  }
  return getNameLoc();
}

ObjCSelector AbstractFunctionDecl::getObjCSelector(
               LazyResolver *resolver, DeclName preferredName) const {
  // If there is an @objc attribute with a name, use that name.
  auto *objc = getAttrs().getAttribute<ObjCAttr>();
  if (auto name = getNameFromObjcAttribute(objc, preferredName)) {
    return *name;
  }

  auto &ctx = getASTContext();
  auto baseName = getName();
  auto argNames = getFullName().getArgumentNames();

  // Use the preferred name if specified
  if (preferredName) {
    // Return invalid selector if argument count doesn't match.
    if (argNames.size() != preferredName.getArgumentNames().size()) {
      return ObjCSelector();
    }
    baseName = preferredName.getBaseName();
    argNames = preferredName.getArgumentNames();
  }

  auto func = dyn_cast<FuncDecl>(this);
  if (func) {
    // For a getter or setter, go through the variable or subscript decl.
    if (func->isGetterOrSetter()) {
      auto asd = cast<AbstractStorageDecl>(func->getAccessorStorageDecl());
      return func->isGetter() ?
        asd->getObjCGetterSelector(resolver, baseName) :
        asd->getObjCSetterSelector(resolver, baseName);
    }
  }

  // Deinitializers are always called "dealloc".
  if (isa<DestructorDecl>(this)) {
    return ObjCSelector(ctx, 0, ctx.Id_dealloc);
  }


  // If this is a zero-parameter initializer with a long selector
  // name, form that selector.
  auto ctor = dyn_cast<ConstructorDecl>(this);
  if (ctor && ctor->isObjCZeroParameterWithLongSelector()) {
    Identifier firstName = argNames[0];
    llvm::SmallString<16> scratch;
    scratch += "init";

    // If the first argument name doesn't start with a preposition, add "with".
    if (getPrepositionKind(camel_case::getFirstWord(firstName.str()))
          == PK_None) {
      camel_case::appendSentenceCase(scratch, "With");
    }

    camel_case::appendSentenceCase(scratch, firstName.str());
    return ObjCSelector(ctx, 0, ctx.getIdentifier(scratch));
  }

  // The number of selector pieces we'll have.
  Optional<ForeignErrorConvention> errorConvention
    = getForeignErrorConvention();
  unsigned numSelectorPieces
    = argNames.size() + (errorConvention.hasValue() ? 1 : 0);

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

    // For the first selector piece, attach either the first parameter
    // or "AndReturnError" to the base name, if appropriate.
    auto firstPiece = baseName;
    llvm::SmallString<32> scratch;
    scratch += firstPiece.str();
    if (errorConvention && piece == errorConvention->getErrorParameterIndex()) {
      // The error is first; append "AndReturnError".
      camel_case::appendSentenceCase(scratch, "AndReturnError");

      firstPiece = ctx.getIdentifier(scratch);
      didStringManipulation = true;
    } else if (!argNames[argIndex].empty()) {
      // If the first argument name doesn't start with a preposition, and the
      // method name doesn't end with a preposition, add "with".
      auto firstName = argNames[argIndex++];
      if (getPrepositionKind(camel_case::getFirstWord(firstName.str()))
            == PK_None &&
          getPrepositionKind(camel_case::getLastWord(firstPiece.str()))
            == PK_None) {
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

AbstractFunctionDecl *AbstractFunctionDecl::getOverriddenDecl() const {
  if (auto func = dyn_cast<FuncDecl>(this))
    return func->getOverriddenDecl();
  if (auto ctor = dyn_cast<ConstructorDecl>(this))
    return ctor->getOverriddenDecl();
  
  return nullptr;
}

FuncDecl *FuncDecl::createImpl(ASTContext &Context,
                               SourceLoc StaticLoc,
                               StaticSpellingKind StaticSpelling,
                               SourceLoc FuncLoc,
                               DeclName Name, SourceLoc NameLoc,
                               bool Throws, SourceLoc ThrowsLoc,
                               SourceLoc AccessorKeywordLoc,
                               GenericParamList *GenericParams,
                               unsigned NumParamPatterns,
                               DeclContext *Parent,
                               ClangNode ClangN) {
  assert(NumParamPatterns > 0);
  size_t Size = totalSizeToAlloc<ParameterList *>(NumParamPatterns);
  void *DeclPtr = allocateMemoryForDecl<FuncDecl>(Context, Size,
                                                  !ClangN.isNull());
  auto D = ::new (DeclPtr)
      FuncDecl(StaticLoc, StaticSpelling, FuncLoc,
               Name, NameLoc, Throws, ThrowsLoc,
               AccessorKeywordLoc, NumParamPatterns,
               GenericParams, Parent);
  if (ClangN)
    D->setClangNode(ClangN);
  return D;
}

FuncDecl *FuncDecl::createDeserialized(ASTContext &Context,
                                       SourceLoc StaticLoc,
                                       StaticSpellingKind StaticSpelling,
                                       SourceLoc FuncLoc,
                                       DeclName Name, SourceLoc NameLoc,
                                       bool Throws, SourceLoc ThrowsLoc,
                                       SourceLoc AccessorKeywordLoc,
                                       GenericParamList *GenericParams,
                                       unsigned NumParamPatterns,
                                       DeclContext *Parent) {
  return createImpl(Context, StaticLoc, StaticSpelling, FuncLoc,
                    Name, NameLoc, Throws, ThrowsLoc,
                    AccessorKeywordLoc, GenericParams,
                    NumParamPatterns, Parent,
                    ClangNode());
}

FuncDecl *FuncDecl::create(ASTContext &Context, SourceLoc StaticLoc,
                           StaticSpellingKind StaticSpelling,
                           SourceLoc FuncLoc,
                           DeclName Name, SourceLoc NameLoc,
                           bool Throws, SourceLoc ThrowsLoc,
                           SourceLoc AccessorKeywordLoc,
                           GenericParamList *GenericParams,
                           ArrayRef<ParameterList*> BodyParams,
                           TypeLoc FnRetType, DeclContext *Parent,
                           ClangNode ClangN) {
  const unsigned NumParamPatterns = BodyParams.size();
  auto *FD = FuncDecl::createImpl(
      Context, StaticLoc, StaticSpelling, FuncLoc,
      Name, NameLoc, Throws, ThrowsLoc,
      AccessorKeywordLoc, GenericParams,
      NumParamPatterns, Parent, ClangN);
  FD->setDeserializedSignature(BodyParams, FnRetType);
  return FD;
}

StaticSpellingKind FuncDecl::getCorrectStaticSpelling() const {
  assert(getDeclContext()->isTypeContext());
  if (!isStatic())
    return StaticSpellingKind::None;
  if (getStaticSpelling() != StaticSpellingKind::None)
    return getStaticSpelling();

  return getCorrectStaticSpellingForDecl(this);
}

bool FuncDecl::isExplicitNonMutating() const {
  return !isMutating() &&
         isAccessor() && !isGetter() &&
         isInstanceMember() &&
         !getDeclContext()->getDeclaredInterfaceType()->hasReferenceSemantics();
}

void FuncDecl::setDeserializedSignature(ArrayRef<ParameterList *> BodyParams,
                                        TypeLoc FnRetType) {
  MutableArrayRef<ParameterList *> BodyParamsRef = getParameterLists();
  unsigned NumParamPatterns = BodyParamsRef.size();

#ifndef NDEBUG
  unsigned NumParams = BodyParams[getDeclContext()->isTypeContext()]->size();
  auto Name = getFullName();
  assert((!Name || !Name.isSimpleName()) && "Must have a simple name");
  assert(!Name || (Name.getArgumentNames().size() == NumParams));
#endif

  for (unsigned i = 0; i != NumParamPatterns; ++i)
    BodyParamsRef[i] = BodyParams[i];

  // Set the decl context of any vardecls to this FuncDecl.
  for (auto P : BodyParams)
    if (P)
      P->setDeclContextOfParamDecls(this);

  this->FnRetType = FnRetType;
}

Type FuncDecl::getResultInterfaceType() const {
  if (!hasInterfaceType())
    return nullptr;

  Type resultTy = getInterfaceType();
  if (resultTy->hasError())
    return resultTy;

  for (unsigned i = 0, e = getNumParameterLists(); i != e; ++i)
    resultTy = resultTy->castTo<AnyFunctionType>()->getResult();

  if (!resultTy)
    resultTy = TupleType::getEmpty(getASTContext());

  return resultTy;
}

bool FuncDecl::isUnaryOperator() const {
  if (!isOperator())
    return false;
  
  auto *params = getParameterList(getDeclContext()->isTypeContext());
  return params->size() == 1 && !params->get(0)->isVariadic();
}

bool FuncDecl::isBinaryOperator() const {
  if (!isOperator())
    return false;
  
  auto *params = getParameterList(getDeclContext()->isTypeContext());
  return params->size() == 2 &&
    !params->get(0)->isVariadic() &&
    !params->get(1)->isVariadic();
}

ConstructorDecl::ConstructorDecl(DeclName Name, SourceLoc ConstructorLoc,
                                 OptionalTypeKind Failability, 
                                 SourceLoc FailabilityLoc,
                                 bool Throws,
                                 SourceLoc ThrowsLoc,
                                 ParamDecl *SelfDecl,
                                 ParameterList *BodyParams,
                                 GenericParamList *GenericParams,
                                 DeclContext *Parent)
  : AbstractFunctionDecl(DeclKind::Constructor, Parent, Name, ConstructorLoc,
                         Throws, ThrowsLoc, /*NumParameterLists=*/2,
                         GenericParams),
    FailabilityLoc(FailabilityLoc)
{
  setParameterLists(SelfDecl, BodyParams);
  
  ConstructorDeclBits.ComputedBodyInitKind = 0;
  ConstructorDeclBits.InitKind
    = static_cast<unsigned>(CtorInitializerKind::Designated);
  ConstructorDeclBits.HasStubImplementation = 0;
  this->Failability = static_cast<unsigned>(Failability);
}

void ConstructorDecl::setParameterLists(ParamDecl *selfDecl,
                                        ParameterList *bodyParams) {
  if (selfDecl) {
    ParameterLists[0] = ParameterList::createWithoutLoc(selfDecl);
    ParameterLists[0]->setDeclContextOfParamDecls(this);
  } else {
    ParameterLists[0] = nullptr;
  }
  
  ParameterLists[1] = bodyParams;
  if (bodyParams)
    bodyParams->setDeclContextOfParamDecls(this);
  
  assert(!getFullName().isSimpleName() && "Constructor name must be compound");
  assert(!bodyParams || 
         (getFullName().getArgumentNames().size() == bodyParams->size()));
}

bool ConstructorDecl::isObjCZeroParameterWithLongSelector() const {
  // The initializer must have a single, non-empty argument name.
  if (getFullName().getArgumentNames().size() != 1 ||
      getFullName().getArgumentNames()[0].empty())
    return false;

  auto *params = getParameterList(1);
  if (params->size() != 1)
    return false;

  return params->get(0)->getInterfaceType()->isVoid();
}

DestructorDecl::DestructorDecl(Identifier NameHack, SourceLoc DestructorLoc,
                               ParamDecl *selfDecl, DeclContext *Parent)
  : AbstractFunctionDecl(DeclKind::Destructor, Parent, NameHack, DestructorLoc,
                         /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                         /*NumParameterLists=*/1, nullptr) {
  setSelfDecl(selfDecl);
}

void DestructorDecl::setSelfDecl(ParamDecl *selfDecl) {
  if (selfDecl) {
    SelfParameter = ParameterList::createWithoutLoc(selfDecl);
    SelfParameter->setDeclContextOfParamDecls(this);
  } else {
    SelfParameter = nullptr;
  }
}

SourceRange FuncDecl::getSourceRange() const {
  SourceLoc StartLoc = getStartLoc();
  if (StartLoc.isInvalid() ||
      getBodyKind() == BodyKind::Synthesize)
    return SourceRange();

  if (getBodyKind() == BodyKind::Unparsed ||
      getBodyKind() == BodyKind::Skipped)
    return { StartLoc, BodyRange.End };

  if (auto *B = getBody()) {
    if (!B->isImplicit())
      return { StartLoc, B->getEndLoc() };
  }

  if (this->isAccessor())
    return StartLoc;

  if (getBodyResultTypeLoc().hasLocation() &&
      getBodyResultTypeLoc().getSourceRange().End.isValid())
    return { StartLoc, getBodyResultTypeLoc().getSourceRange().End };
  auto LastParamListEndLoc = getParameterLists().back()->getSourceRange().End;
  if (LastParamListEndLoc.isValid())
    return { StartLoc, LastParamListEndLoc };
  return StartLoc;
}

SourceRange EnumElementDecl::getSourceRange() const {
  if (RawValueExpr && !RawValueExpr->isImplicit())
    return {getStartLoc(), RawValueExpr->getEndLoc()};
  if (ArgumentType.hasLocation())
    return {getStartLoc(), ArgumentType.getSourceRange().End};
  return {getStartLoc(), getNameLoc()};
}

bool EnumElementDecl::computeType() {
  EnumDecl *ED = getParentEnum();
  Type resultTy = ED->getDeclaredInterfaceType();

  if (resultTy->hasError()) {
    setInterfaceType(resultTy);
    setInvalid();
    return false;
  }

  Type selfTy = MetatypeType::get(resultTy);

  // The type of the enum element is either (T) -> T or (T) -> ArgType -> T.
  if (auto inputTy = getArgumentTypeLoc().getType()) {
    resultTy = FunctionType::get(ED->mapTypeOutOfContext(inputTy), resultTy);
  }

  if (auto *genericSig = ED->getGenericSignatureOfContext())
    resultTy = GenericFunctionType::get(genericSig, selfTy, resultTy,
                                        AnyFunctionType::ExtInfo());
  else
    resultTy = FunctionType::get(selfTy, resultTy);

  // Record the interface type.
  setInterfaceType(resultTy);

  return true;
}

Type EnumElementDecl::getArgumentInterfaceType() const {
  if (!EnumElementDeclBits.HasArgumentType)
    return nullptr;

  auto interfaceType = getInterfaceType();
  if (interfaceType->hasError()) {
    return interfaceType;
  }

  auto funcTy = interfaceType->castTo<AnyFunctionType>();
  funcTy = funcTy->getResult()->castTo<AnyFunctionType>();
  return funcTy->getInput();
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

SourceRange ConstructorDecl::getSourceRange() const {
  if (isImplicit())
    return getConstructorLoc();

  if (getBodyKind() == BodyKind::Unparsed ||
      getBodyKind() == BodyKind::Skipped)
    return { getConstructorLoc(), BodyRange.End };

  SourceLoc End;
  if (auto body = getBody())
    End = body->getEndLoc();
  if (End.isInvalid())
    End = getSignatureSourceRange().End;

  return { getConstructorLoc(), End };
}

Type ConstructorDecl::getArgumentInterfaceType() const {
  Type ArgTy = getInterfaceType();
  ArgTy = ArgTy->castTo<AnyFunctionType>()->getResult();
  ArgTy = ArgTy->castTo<AnyFunctionType>()->getInput();
  return ArgTy;
}

Type ConstructorDecl::getResultInterfaceType() const {
  Type ArgTy = getInterfaceType();
  ArgTy = ArgTy->castTo<AnyFunctionType>()->getResult();
  ArgTy = ArgTy->castTo<AnyFunctionType>()->getResult();
  return ArgTy;
}

Type ConstructorDecl::getInitializerInterfaceType() {
  return InitializerInterfaceType;
}

void ConstructorDecl::setInitializerInterfaceType(Type t) {
  InitializerInterfaceType = t;
}

ConstructorDecl::BodyInitKind
ConstructorDecl::getDelegatingOrChainedInitKind(DiagnosticEngine *diags,
                                                ApplyExpr **init) const {
  assert(hasBody() && "Constructor does not have a definition");

  if (init)
    *init = nullptr;

  // If we already computed the result, return it.
  if (ConstructorDeclBits.ComputedBodyInitKind) {
    return static_cast<BodyInitKind>(
             ConstructorDeclBits.ComputedBodyInitKind - 1);
  }


  struct FindReferenceToInitializer : ASTWalker {
    const ConstructorDecl *Decl;
    BodyInitKind Kind = BodyInitKind::None;
    ApplyExpr *InitExpr = nullptr;
    DiagnosticEngine *Diags;

    FindReferenceToInitializer(const ConstructorDecl *decl,
                               DiagnosticEngine *diags)
        : Decl(decl), Diags(diags) { }

    bool isSelfExpr(Expr *E) {
      E = E->getSemanticsProvidingExpr();

      if (auto ATSE = dyn_cast<ArchetypeToSuperExpr>(E))
        E = ATSE->getSubExpr();
      if (auto IOE = dyn_cast<InOutExpr>(E))
        E = IOE->getSubExpr();
      if (auto LE = dyn_cast<LoadExpr>(E))
        E = LE->getSubExpr();
      if (auto DRE = dyn_cast<DeclRefExpr>(E))
        return DRE->getDecl() == Decl->getImplicitSelfDecl();

      return false;
    }

    bool walkToDeclPre(class Decl *D) override {
      // Don't walk into further nominal decls.
      if (isa<NominalTypeDecl>(D))
        return false;
      return true;
    }
    
    std::pair<bool, Expr*> walkToExprPre(Expr *E) override {
      // Don't walk into closures.
      if (isa<ClosureExpr>(E))
        return { false, E };
      
      // Look for calls of a constructor on self or super.
      auto apply = dyn_cast<ApplyExpr>(E);
      if (!apply)
        return { true, E };

      auto Callee = apply->getSemanticFn();
      
      Expr *arg;

      if (isa<OtherConstructorDeclRefExpr>(Callee)) {
        arg = apply->getArg();
      } else if (auto *CRE = dyn_cast<ConstructorRefCallExpr>(Callee)) {
        arg = CRE->getArg();
      } else if (auto *dotExpr = dyn_cast<UnresolvedDotExpr>(Callee)) {
        if (dotExpr->getName().getBaseName().str() != "init")
          return { true, E };

        arg = dotExpr->getBase();
      } else {
        // Not a constructor call.
        return { true, E };
      }

      // Look for a base of 'self' or 'super'.
      BodyInitKind myKind;
      if (arg->isSuperExpr())
        myKind = BodyInitKind::Chained;
      else if (isSelfExpr(arg))
        myKind = BodyInitKind::Delegating;
      else {
        // We're constructing something else.
        return { true, E };
      }
      
      if (Kind == BodyInitKind::None) {
        Kind = myKind;

        // If we're not emitting diagnostics, we're done.
        if (!Diags)
          return { false, nullptr };

        InitExpr = apply;
        return { true, E };
      }

      assert(Diags && "Failed to abort traversal early");

      // If the kind changed, complain.
      if (Kind != myKind) {
        // The kind changed. Complain.
        Diags->diagnose(E->getLoc(), diag::init_delegates_and_chains);
        Diags->diagnose(InitExpr->getLoc(), diag::init_delegation_or_chain,
                        Kind == BodyInitKind::Chained);
      }

      return { true, E };
    }
  };
  
  FindReferenceToInitializer finder(this, diags);
  getBody()->walk(finder);

  // get the kind out of the finder.
  auto Kind = finder.Kind;


  // If we didn't find any delegating or chained initializers, check whether
  // the initializer was explicitly marked 'convenience'.
  if (Kind == BodyInitKind::None && getAttrs().hasAttribute<ConvenienceAttr>())
    Kind = BodyInitKind::Delegating;

  // If we still don't know, check whether we have a class with a superclass: it
  // gets an implicit chained initializer.
  if (Kind == BodyInitKind::None) {
    if (auto classDecl = getDeclContext()->getAsClassOrClassExtensionContext()) {
      if (classDecl->getSuperclass())
        Kind = BodyInitKind::ImplicitChained;
    }
  }

  // Cache the result if it is trustworthy.
  if (diags) {
    auto *mutableThis = const_cast<ConstructorDecl *>(this);
    mutableThis->ConstructorDeclBits.ComputedBodyInitKind =
        static_cast<unsigned>(Kind) + 1;
    if (init)
      *init = finder.InitExpr;
  }

  return Kind;
}

SourceRange DestructorDecl::getSourceRange() const {
  if (getBodyKind() == BodyKind::Unparsed ||
      getBodyKind() == BodyKind::Skipped)
    return { getDestructorLoc(), BodyRange.End };

  if (getBodyKind() == BodyKind::None)
    return getDestructorLoc();

  return { getDestructorLoc(), getBody()->getEndLoc() };
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
  PrecedenceGroupDeclBits.Associativity = unsigned(associativity);
  PrecedenceGroupDeclBits.IsAssignment = isAssignment;
  memcpy(getHigherThanBuffer(), higherThan.data(),
         higherThan.size() * sizeof(Relation));
  memcpy(getLowerThanBuffer(), lowerThan.data(),
         lowerThan.size() * sizeof(Relation));
}

void PrecedenceGroupDecl::collectOperatorKeywordRanges(
                                    SmallVectorImpl<CharSourceRange> &Ranges) {
  auto AddToRange = [&] (SourceLoc Loc, StringRef Word) {
    if (Loc.isValid())
      Ranges.push_back(CharSourceRange(Loc, strlen(Word.data())));
  };
  AddToRange(AssociativityKeywordLoc, "associativity");
  AddToRange(AssignmentKeywordLoc, "assignment");
  AddToRange(HigherThanLoc, "higherThan");
  AddToRange(LowerThanLoc, "lowerThan");
}

bool FuncDecl::isDeferBody() const {
  return getName() == getASTContext().getIdentifier("$defer");
}

Type TypeBase::getSwiftNewtypeUnderlyingType() {
  auto structDecl = getStructOrBoundGenericStruct();
  if (!structDecl)
    return {};

  // Make sure the clang node has swift_newtype attribute
  auto clangNode = structDecl->getClangDecl();
  if (!clangNode || !clangNode->hasAttr<clang::SwiftNewtypeAttr>())
    return {};

  // Underlying type is the type of rawValue
  for (auto member : structDecl->getMembers())
    if (auto varDecl = dyn_cast<VarDecl>(member))
      if (varDecl->getName() == getASTContext().Id_rawValue)
        return varDecl->getType();

  return {};
}

ClassDecl *ClassDecl::getSuperclassDecl() const {
  if (auto superclass = getSuperclass())
    return superclass->getClassOrBoundGenericClass();
  return nullptr;
}

void ClassDecl::setSuperclass(Type superclass) {
  assert((!superclass || !superclass->hasArchetype())
         && "superclass must be interface type");
  LazySemanticInfo.Superclass.setPointerAndInt(superclass, true);
}
