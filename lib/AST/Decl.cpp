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
#include "swift/AST/AccessRequests.h"
#include "swift/AST/AccessScope.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeLoc.h"
#include "swift/AST/SwiftNameTranslation.h"
#include "clang/Lex/MacroInfo.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"
#include "swift/Basic/ExperimentalDependencies.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Basic/Statistic.h"

#include "clang/Basic/CharInfo.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"

#include "InlinableText.h"
#include <algorithm>

using namespace swift;

#define DEBUG_TYPE "Serialization"

STATISTIC(NumLazyGenericEnvironments,
          "# of lazily-deserialized generic environments known");
STATISTIC(NumLazyGenericEnvironmentsLoaded,
          "# of lazily-deserialized generic environments loaded");

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
  TRIVIAL_KIND(Subscript);
  TRIVIAL_KIND(Constructor);
  TRIVIAL_KIND(Destructor);
  TRIVIAL_KIND(EnumElement);
  TRIVIAL_KIND(Param);
  TRIVIAL_KIND(Module);
  TRIVIAL_KIND(MissingMember);

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
       if (var->getDeclContext()->isTypeContext())
         return DescriptiveDeclKind::Property;
       return var->isLet() ? DescriptiveDeclKind::Let
                           : DescriptiveDeclKind::Var;
     case StaticSpellingKind::KeywordStatic:
       return DescriptiveDeclKind::StaticProperty;
     case StaticSpellingKind::KeywordClass:
       return DescriptiveDeclKind::ClassProperty;
     }
   }

   case DeclKind::Accessor: {
     auto accessor = cast<AccessorDecl>(this);

     switch (accessor->getAccessorKind()) {
     case AccessorKind::Get:
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
       return DescriptiveDeclKind::ReadAccessor;

     case AccessorKind::Modify:
       return DescriptiveDeclKind::ModifyAccessor;
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
  ENTRY(PoundDiagnostic, "diagnostic");
  ENTRY(PatternBinding, "pattern binding");
  ENTRY(Var, "var");
  ENTRY(Param, "parameter");
  ENTRY(Let, "let");
  ENTRY(Property, "property");
  ENTRY(StaticProperty, "static property");
  ENTRY(ClassProperty, "class property");
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
  ENTRY(Protocol, "protocol");
  ENTRY(GenericEnum, "generic enum");
  ENTRY(GenericStruct, "generic struct");
  ENTRY(GenericClass, "generic class");
  ENTRY(GenericType, "generic type");
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
  ENTRY(Addressor, "address accessor");
  ENTRY(MutableAddressor, "mutableAddress accessor");
  ENTRY(ReadAccessor, "_read accessor");
  ENTRY(ModifyAccessor, "_modify accessor");
  ENTRY(EnumElement, "enum case");
  ENTRY(Module, "module");
  ENTRY(MissingMember, "missing member placeholder");
  ENTRY(Requirement, "requirement");
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

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &OS,
                                     ReferenceOwnership RO) {
  if (RO == ReferenceOwnership::Strong)
    return OS << "'strong'";
  return OS << "'" << keywordOf(RO) << "'";
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
         (!isa<AbstractTypeParamDecl>(this) || isa<AssociatedTypeDecl>(this));
}

ModuleDecl *Decl::getModuleContext() const {
  return getDeclContext()->getParentModule();
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
    if (!AD->getAccessorKeywordLoc().isValid())
      return Range;

    // Otherwise, include attributes directly attached to the accessor.
    SourceLoc VarLoc = AD->getStorage()->getStartLoc();
    for (auto Attr : getAttrs()) {
      if (!Attr->getRange().isValid())
        continue;

      SourceLoc AttrStartLoc = Attr->getRangeWithAt().Start;
      if (getASTContext().SourceMgr.isBeforeInBuffer(VarLoc, AttrStartLoc))
        Range.widen(AttrStartLoc);
    }
    return Range;
  }

  // Attributes on VarDecl syntactically belong to PatternBindingDecl.
  if (isa<VarDecl>(this))
    return Range;

  // Attributes on PatternBindingDecls are attached to VarDecls in AST.
  if (auto *PBD = dyn_cast<PatternBindingDecl>(this)) {
    for (auto Entry : PBD->getPatternList())
      Entry.getPattern()->forEachVariable([&](VarDecl *VD) {
        for (auto Attr : VD->getAttrs())
          if (Attr->getRange().isValid())
            Range.widen(Attr->getRangeWithAt());
      });
  }

  for (auto Attr : getAttrs()) {
    if (Attr->getRange().isValid())
      Range.widen(Attr->getRangeWithAt());
  }
  return Range;
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

SourceLoc BehaviorRecord::getLoc() const { return ProtocolName->getLoc(); }

bool AbstractStorageDecl::isTransparent() const {
  return getAttrs().hasAttribute<TransparentAttr>();
}

bool AbstractFunctionDecl::isTransparent() const {
  // Check if the declaration had the attribute.
  if (getAttrs().hasAttribute<TransparentAttr>())
    return true;

  // If this is an accessor, check if the transparent attribute was set
  // on the storage decl.
  if (const auto *AD = dyn_cast<AccessorDecl>(this)) {
    return AD->getStorage()->isTransparent();
  }

  return false;
}

bool Decl::isPrivateStdlibDecl(bool treatNonBuiltinProtocolsAsPublic) const {
  const Decl *D = this;
  if (auto ExtD = dyn_cast<ExtensionDecl>(D)) {
    Type extTy = ExtD->getExtendedType();
    return extTy.isPrivateStdlibType(treatNonBuiltinProtocolsAsPublic);
  }

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
    // If it's a function with a parameter with leading underscore, it's a
    // private function.
    if (hasInternalParameter(AFD->getParameters()))
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
    if (treatNonBuiltinProtocolsAsPublic)
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
  if (!VD->getBaseName().isSpecial() &&
      VD->getBaseName().getIdentifier().str().startswith("_"))
    return true;

  return false;
}

bool Decl::isWeakImported(ModuleDecl *fromModule) const {
  // For a Clang declaration, trust Clang.
  if (auto clangDecl = getClangDecl()) {
    return clangDecl->isWeakImported();
  }

  auto *containingModule = getModuleContext();
  if (containingModule == fromModule)
    return false;

  if (getAttrs().hasAttribute<WeakLinkedAttr>())
    return true;

  // FIXME: Also check availability when containingModule is resilient.
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
          reqt.getSeparatorLoc(),
          second.clone(ctx));
      break;
    }
    case RequirementReprKind::SameType: {
      auto first = reqt.getFirstTypeLoc();
      auto second = reqt.getSecondTypeLoc();
      reqt = RequirementRepr::getSameType(
          first.clone(ctx),
          reqt.getSeparatorLoc(),
          second.clone(ctx));
      break;
    }
    case RequirementReprKind::LayoutConstraint: {
      auto first = reqt.getSubjectLoc();
      auto layout = reqt.getLayoutConstraintLoc();
      reqt = RequirementRepr::getLayoutConstraint(
          first.clone(ctx),
          reqt.getSeparatorLoc(),
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

TypeArrayView<GenericTypeParamType>
GenericContext::getInnermostGenericParamTypes() const {
  if (auto sig = getGenericSignature())
    return sig->getInnermostGenericParams();
  else
    return { };
}

/// Retrieve the generic requirements.
ArrayRef<Requirement> GenericContext::getGenericRequirements() const {
  if (auto sig = getGenericSignature())
    return sig->getRequirements();
  else
    return { };
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

bool GenericContext::hasLazyGenericEnvironment() const {
  return GenericSigOrEnv.dyn_cast<GenericSignature *>() != nullptr;
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
  // FIXME: (transitional) increment the redundant "always-on" counter.
  if (getASTContext().Stats)
    getASTContext().Stats->getFrontendCounters().NumLazyGenericEnvironmentsLoaded++;
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
  // FIXME: (transitional) increment the redundant "always-on" counter.
  if (getASTContext().Stats)
    getASTContext().Stats->getFrontendCounters().NumLazyGenericEnvironments++;

}

SourceRange GenericContext::getGenericTrailingWhereClauseSourceRange() const {
  if (!isGeneric())
    return SourceRange();
  return getGenericParams()->getTrailingWhereClauseSourceRange();
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
  : Decl(DeclKind::Import, DC), ImportLoc(ImportLoc), KindLoc(KindLoc) {
  Bits.ImportDecl.NumPathElements = Path.size();
  assert(Bits.ImportDecl.NumPathElements == Path.size() && "Truncation error");
  Bits.ImportDecl.ImportKind = static_cast<unsigned>(K);
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
  case DeclKind::PoundDiagnostic:
  case DeclKind::PrecedenceGroup:
  case DeclKind::MissingMember:
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
    auto *nominal = type->getAnyNominal();
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

ExtensionDecl::ExtensionDecl(SourceLoc extensionLoc,
                             TypeLoc extendedType,
                             MutableArrayRef<TypeLoc> inherited,
                             DeclContext *parent,
                             TrailingWhereClause *trailingWhereClause)
  : GenericContext(DeclContextKind::ExtensionDecl, parent),
    Decl(DeclKind::Extension, parent),
    IterableDeclContext(IterableDeclContextKind::ExtensionDecl),
    ExtensionLoc(extensionLoc),
    ExtendedType(extendedType),
    Inherited(inherited)
{
  Bits.ExtensionDecl.DefaultAndMaxAccessLevel = 0;
  Bits.ExtensionDecl.HasLazyConformances = false;
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
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    ExtendedNominalRequest{const_cast<ExtensionDecl *>(this)}, nullptr);
}

bool ExtensionDecl::isConstrainedExtension() const {
  // Non-generic extension.
  if (!getGenericSignature())
    return false;

  auto nominal = getExtendedNominal();
  assert(nominal);

  // If the generic signature differs from that of the nominal type, it's a
  // constrained extension.
  return getGenericSignature()->getCanonicalSignature()
    != nominal->getGenericSignature()->getCanonicalSignature();
}

bool ExtensionDecl::isEquivalentToExtendedContext() const {
  auto decl = getExtendedNominal();
  return getParentModule() == decl->getParentModule()
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
  DeclContext *BindingInitContext = nullptr;
  if (!Parent->isLocalContext())
    BindingInitContext = new (Ctx) PatternBindingInitializer(Parent);

  auto PBE = PatternBindingEntry(Pat, EqualLoc, E, BindingInitContext);
  auto *Result = create(Ctx, StaticLoc, StaticSpelling, VarLoc, PBE, Parent);

  if (BindingInitContext)
    cast<PatternBindingInitializer>(BindingInitContext)->setBinding(Result, 0);

  return Result;
}

PatternBindingDecl *PatternBindingDecl::createImplicit(
    ASTContext &Ctx, StaticSpellingKind StaticSpelling, Pattern *Pat, Expr *E,
    DeclContext *Parent, SourceLoc VarLoc) {
  auto *Result = create(Ctx, /*StaticLoc*/ SourceLoc(), StaticSpelling, VarLoc,
                        Pat, /*EqualLoc*/ SourceLoc(), E, Parent);
  Result->setImplicit();
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
    entry = PatternBindingEntry(/*Pattern*/ nullptr, /*EqualLoc*/ SourceLoc(),
                                /*Init*/ nullptr, /*InitContext*/ nullptr);
  }
  return PBD;
}

ParamDecl *PatternBindingInitializer::getImplicitSelfDecl() {
  if (SelfParam)
    return SelfParam;

  if (auto singleVar = getInitializedLazyVar()) {
    auto DC = singleVar->getDeclContext();
    if (DC->isTypeContext()) {
      auto specifier = (DC->getDeclaredInterfaceType()->hasReferenceSemantics()
                        ? VarDecl::Specifier::Default
                        : VarDecl::Specifier::InOut);

      ASTContext &C = DC->getASTContext();
      SelfParam = new (C) ParamDecl(specifier, SourceLoc(), SourceLoc(),
                                    Identifier(), singleVar->getLoc(),
                                    C.Id_self, this);
      SelfParam->setImplicit();
      SelfParam->setInterfaceType(DC->getSelfInterfaceType());
      SelfParam->setValidationToChecked();
    }
  }

  return SelfParam;
}

VarDecl *PatternBindingInitializer::getInitializedLazyVar() const {
  if (auto var = getBinding()->getSingleVar()) {
    if (var->getAttrs().hasAttribute<LazyAttr>())
      return var;
  }
  return nullptr;
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
  auto Init = getInitAsWritten();
  return Init ? Init->getSourceRange() : SourceRange();
}

void PatternBindingEntry::setInit(Expr *E) {
  auto F = PatternAndFlags.getInt();
  if (E) {
    PatternAndFlags.setInt(F - Flags::Removed);
  } else {
    PatternAndFlags.setInt(F | Flags::Removed);
  }
  InitExpr.Node = E;
  InitContextAndIsText.setInt(false);
}

VarDecl *PatternBindingEntry::getAnchoringVarDecl() const {
  SmallVector<VarDecl *, 8> variables;
  getPattern()->collectVariables(variables);
  assert(!variables.empty());
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

bool PatternBindingEntry::hasInitStringRepresentation() const {
  if (InitContextAndIsText.getInt())
    return !InitStringRepresentation.empty();
  return getInit() && getInit()->getSourceRange().isValid();
}

StringRef PatternBindingEntry::getInitStringRepresentation(
  SmallVectorImpl<char> &scratch) const {

  assert(hasInitStringRepresentation() &&
         "must check if pattern has string representation");

  if (InitContextAndIsText.getInt() && !InitStringRepresentation.empty())
    return InitStringRepresentation;
  auto &sourceMgr = getAnchoringVarDecl()->getASTContext().SourceMgr;
  auto init = getInit();
  return extractInlinableText(sourceMgr, init, scratch);
}

SourceRange PatternBindingDecl::getSourceRange() const {
  SourceLoc startLoc = getStartLoc();
  SourceLoc endLoc = getPatternList().back().getSourceRange().End;
  if (startLoc.isValid() != endLoc.isValid()) return SourceRange();
  return { startLoc, endLoc };
}

static StaticSpellingKind getCorrectStaticSpellingForDecl(const Decl *D) {
  if (!D->getDeclContext()->getSelfClassDecl())
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

bool VarDecl::isInitExposedToClients() const {
  auto parent = dyn_cast<NominalTypeDecl>(getDeclContext());
  if (!parent) return false;
  if (!hasInitialValue()) return false;
  if (isStatic()) return false;
  return parent->getAttrs().hasAttribute<FixedLayoutAttr>();
}

/// Check whether the given type representation will be
/// default-initializable.
static bool isDefaultInitializable(const TypeRepr *typeRepr) {
  // Look through most attributes.
  if (const auto attributed = dyn_cast<AttributedTypeRepr>(typeRepr)) {
    // Ownership kinds have optionalness requirements.
    if (optionalityOf(attributed->getAttrs().getOwnership()) ==
        ReferenceOwnershipOptionality::Required)
      return true;

    return isDefaultInitializable(attributed->getTypeRepr());
  }

  // Optional types are default-initializable.
  if (isa<OptionalTypeRepr>(typeRepr) ||
      isa<ImplicitlyUnwrappedOptionalTypeRepr>(typeRepr))
    return true;

  // Tuple types are default-initializable if all of their element
  // types are.
  if (const auto tuple = dyn_cast<TupleTypeRepr>(typeRepr)) {
    // ... but not variadic ones.
    if (tuple->hasEllipsis())
      return false;

    for (const auto elt : tuple->getElements()) {
      if (!isDefaultInitializable(elt.Type))
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

bool PatternBindingDecl::isDefaultInitializable(unsigned i) const {
  const auto entry = getPatternList()[i];

  // If it has an initializer expression, this is trivially true.
  if (entry.getInit())
    return true;

  if (entry.getPattern()->isNeverDefaultInitializable())
    return false;

  // If the pattern is typed as optional (or tuples thereof), it is
  // default initializable.
  if (const auto typedPattern = dyn_cast<TypedPattern>(entry.getPattern())) {
    if (const auto typeRepr = typedPattern->getTypeLoc().getTypeRepr()) {
      if (::isDefaultInitializable(typeRepr))
        return true;
    } else if (typedPattern->isImplicit()) {
      // Lazy vars have implicit storage assigned to back them. Because the
      // storage is implicit, the pattern is typed and has a TypeLoc, but not a
      // TypeRepr.
      //
      // All lazy storage is implicitly default initializable, though, because
      // lazy backing storage is optional.
      if (const auto *varDecl = typedPattern->getSingleVar())
        // Lazy storage is never user accessible.
        if (!varDecl->isUserAccessible())
          if (typedPattern->getTypeLoc().getType()->getOptionalObjectType())
            return true;
    }
  }

  // Otherwise, we can't default initialize this binding.
  return false;
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
  if (storage->isDynamic())
    return true;

  // Imported declarations behave like they are dynamic, even if they're
  // not marked as such explicitly.
  if (storage->isObjC() && storage->hasClangNode())
    return true;

  if (auto *classDecl = dyn_cast<ClassDecl>(storage->getDeclContext())) {
    if (storage->isFinal() || classDecl->isFinal())
      return false;

    return true;
  }

  if (isa<ProtocolDecl>(storage->getDeclContext()))
    return true;

  return false;
}

static bool isDirectToStorageAccess(const AccessorDecl *accessor,
                                    const VarDecl *var, bool isAccessOnSelf) {
  // All accesses have ordinary semantics except those to variables
  // with storage from within their own accessors.
  if (accessor->getStorage() != var)
    return false;

  if (!var->hasStorage())
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

/// Determines the access semantics to use in a DeclRefExpr or
/// MemberRefExpr use of this value in the specified context.
AccessSemantics
ValueDecl::getAccessSemanticsFromContext(const DeclContext *UseDC,
                                         bool isAccessOnSelf) const {
  // The condition most likely to fast-path us is not being in an accessor,
  // so we check that first.
  if (auto *accessor = dyn_cast<AccessorDecl>(UseDC)) {
    if (auto *var = dyn_cast<VarDecl>(this)) {
      if (isDirectToStorageAccess(accessor, var, isAccessOnSelf))
        return AccessSemantics::DirectToStorage;
    }
  }

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
  }
  llvm_unreachable("bad impl kind");
}

static AccessStrategy
getDirectWriteAccessStrategy(const AbstractStorageDecl *storage) {
  switch (storage->getWriteImpl()) {
  case WriteImplKind::Immutable:
    assert(isa<VarDecl>(storage) && cast<VarDecl>(storage)->isLet() &&
           "mutation of a immutable variable that isn't a let");
    return AccessStrategy::getStorage();
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
  }
  llvm_unreachable("bad impl kind");
}

static AccessStrategy
getDirectReadWriteAccessStrategy(const AbstractStorageDecl *storage) {
  switch (storage->getReadWriteImpl()) {
  case ReadWriteImplKind::Immutable:
    assert(isa<VarDecl>(storage) && cast<VarDecl>(storage)->isLet() &&
           "mutation of a immutable variable that isn't a let");
    return AccessStrategy::getStorage();
  case ReadWriteImplKind::Stored:
    return AccessStrategy::getStorage();
  case ReadWriteImplKind::MutableAddress:
    return AccessStrategy::getAccessor(AccessorKind::MutableAddress,
                                       /*dispatch*/ false);
  case ReadWriteImplKind::Modify:
    return AccessStrategy::getAccessor(AccessorKind::Modify,
                                       /*dispatch*/ false);
  case ReadWriteImplKind::MaterializeToTemporary:
    return AccessStrategy::getMaterializeToTemporary(
                                       getDirectReadAccessStrategy(storage),
                                       getDirectWriteAccessStrategy(storage));
  }
  llvm_unreachable("bad impl kind");
}

static AccessStrategy
getOpaqueReadAccessStrategy(const AbstractStorageDecl *storage, bool dispatch) {
  return AccessStrategy::getAccessor(AccessorKind::Get, dispatch);
}

static AccessStrategy
getOpaqueWriteAccessStrategy(const AbstractStorageDecl *storage, bool dispatch){
  return AccessStrategy::getAccessor(AccessorKind::Set, dispatch);
}

static AccessStrategy
getOpaqueReadWriteAccessStrategy(const AbstractStorageDecl *storage,
                                 bool dispatch) {
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
                                       DeclContext *accessFromDC) const {
  switch (semantics) {
  case AccessSemantics::DirectToStorage:
    assert(hasStorage());
    return AccessStrategy::getStorage();

  case AccessSemantics::Ordinary:
    // Skip these checks for local variables, both because they're unnecessary
    // and because we won't necessarily have computed access.
    if (!getDeclContext()->isLocalContext()) {
      // If the property is defined in a non-final class or a protocol, the
      // accessors are dynamically dispatched, and we cannot do direct access.
      if (isPolymorphic(this))
        return getOpaqueAccessStrategy(this, accessKind, /*dispatch*/ true);

      // If the storage is resilient to the given use DC (perhaps because
      // it's @_transparent and we have to be careful about it being inlined
      // across module lines), we cannot use direct access.
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
      if (accessFromDC)
        resilient = isResilient(accessFromDC->getParentModule(),
                                accessFromDC->getResilienceExpansion());
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

  case AccessSemantics::BehaviorInitialization:
    // Behavior initialization writes to the property as if it has storage.
    // SIL definite initialization will introduce the logical accesses.
    // Reads or inouts go through the ordinary path.
    switch (accessKind) {
    case AccessKind::Write:
      return AccessStrategy::getBehaviorStorage();
    case AccessKind::Read:
    case AccessKind::ReadWrite:
      return getAccessStrategy(AccessSemantics::Ordinary,
                               accessKind, accessFromDC);
    }
  }
  llvm_unreachable("bad access semantics");
}

bool AbstractStorageDecl::requiresOpaqueAccessor(AccessorKind kind) const {
  switch (kind) {
  case AccessorKind::Get:
    return requiresOpaqueGetter();
  case AccessorKind::Set:
    return requiresOpaqueSetter();
  case AccessorKind::Read:
    return requiresOpaqueReadCoroutine();
  case AccessorKind::Modify:
    return requiresOpaqueModifyCoroutine();

  // Other accessors are never part of the opaque-accessors set.
#define OPAQUE_ACCESSOR(ID, KEYWORD)
#define ACCESSOR(ID) \
  case AccessorKind::ID:
#include "swift/AST/AccessorKinds.def"
    return false;
  }
  llvm_unreachable("bad accessor kind");
}

bool AbstractStorageDecl::requiresOpaqueModifyCoroutine() const {
  // Only for mutable storage.
  if (!supportsMutation())
    return false;

  // Imported storage declarations don't have eagerly-generated modify
  // accessors.
  if (hasClangNode())
    return false;

  // Dynamic storage suppresses the modify coroutine.
  // If we add a Swift-native concept of `dynamic`, this should be restricted
  // to the ObjC-supported concept.
  if (isDynamic())
    return false;

  // We only need the modify coroutine in type contexts.
  // TODO: resilient global variables?
  auto *dc = getDeclContext();
  if (!dc->isTypeContext())
    return false;

  // Requirements of ObjC protocols don't support the modify coroutine.
  if (auto protoDecl = dyn_cast<ProtocolDecl>(dc))
    if (protoDecl->isObjC())
      return false;

  return true;
}

void AbstractStorageDecl::visitExpectedOpaqueAccessors(
                        llvm::function_ref<void (AccessorKind)> visit) const {
  if (requiresOpaqueGetter())
    visit(AccessorKind::Get);

  if (requiresOpaqueReadCoroutine())
    visit(AccessorKind::Read);

  // All mutable storage should have a setter.
  if (requiresOpaqueSetter())
    visit(AccessorKind::Set);

  // Include the modify coroutine if it's required.
  if (requiresOpaqueModifyCoroutine())
    visit(AccessorKind::Modify);
}

void AbstractStorageDecl::visitOpaqueAccessors(
                        llvm::function_ref<void (AccessorDecl*)> visit) const {
  visitExpectedOpaqueAccessors([&](AccessorKind kind) {
    auto accessor = getAccessor(kind);
    assert(accessor && "didn't have expected opaque accessor");
    visit(accessor);
  });
}

static bool hasPrivateOrFilePrivateFormalAccess(const ValueDecl *D) {
  return D->getFormalAccess() <= AccessLevel::FilePrivate;
}

/// Returns true if one of the ancestor DeclContexts of this ValueDecl is either
/// marked private or fileprivate or is a local context.
static bool isInPrivateOrLocalContext(const ValueDecl *D) {
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

bool ValueDecl::isOutermostPrivateOrFilePrivateScope() const {
  return hasPrivateOrFilePrivateFormalAccess(this) &&
         !isInPrivateOrLocalContext(this);
}

bool AbstractStorageDecl::isFormallyResilient() const {
  // Check for an explicit @_fixed_layout attribute.
  if (getAttrs().hasAttribute<FixedLayoutAttr>())
    return false;

  // Private and (unversioned) internal variables always have a
  // fixed layout.
  if (!getFormalAccessScope(/*useDC=*/nullptr,
                            /*treatUsableFromInlineAsPublic=*/true).isPublic())
    return false;

  // If we're an instance property of a nominal type, query the type.
  auto *dc = getDeclContext();
  if (!isStatic())
    if (auto *nominalDecl = dc->getSelfNominalTypeDecl())
      return nominalDecl->isResilient();

  return true;
}

bool AbstractStorageDecl::isResilient() const {
  if (!isFormallyResilient())
    return false;

  switch (getDeclContext()->getParentModule()->getResilienceStrategy()) {
  case ResilienceStrategy::Resilient:
    return true;
  case ResilienceStrategy::Default:
    return false;
  }

  llvm_unreachable("Unhandled ResilienceStrategy in switch.");
}

bool AbstractStorageDecl::isResilient(ModuleDecl *M,
                                      ResilienceExpansion expansion) const {
  switch (expansion) {
  case ResilienceExpansion::Minimal:
    return isResilient();
  case ResilienceExpansion::Maximal:
    return isResilient() && M != getModuleContext();
  }
  llvm_unreachable("bad resilience expansion");
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
  case DeclKind::PoundDiagnostic:
  case DeclKind::PrecedenceGroup:
  case DeclKind::MissingMember:
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
  case DeclKind::Accessor:
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

unsigned ValueDecl::getLocalDiscriminator() const {
  return LocalDiscriminator;
}

void ValueDecl::setLocalDiscriminator(unsigned index) {
  assert(getDeclContext()->isLocalContext());
  assert(LocalDiscriminator == 0 && "LocalDiscriminator is set multiple times");
  LocalDiscriminator = index;
}

ValueDecl *ValueDecl::getOverriddenDecl() const {
  auto overridden = getOverriddenDecls();
  if (overridden.empty()) return nullptr;

  // FIXME: Arbitrarily pick the first overridden declaration.
  return overridden.front();
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

  // If one is a compound name and the other is not, they do not conflict
  // if one is a property and the other is a non-nullary function.
  if (sig1.Name.isCompoundName() != sig2.Name.isCompoundName()) {
    return !((sig1.IsVariable && !sig2.Name.getArgumentNames().empty()) ||
             (sig2.IsVariable && !sig1.Name.getArgumentNames().empty()));
  }
  
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
  return type.transform([&](Type type) -> Type {
      if (type->is<FunctionType>()) {
        return mapSignatureFunctionType(ctx, type, false, false, false, 1);
      }
      
      return type;
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
      if (auto objectType = type->getOptionalObjectType()) {
        type = objectType;
      }
    }

    return mapSignatureParamType(ctx, type);
  }

  auto funcTy = type->castTo<AnyFunctionType>();
  SmallVector<AnyFunctionType::Param, 4> newParams;
  for (const auto &param : funcTy->getParams()) {
    auto newParamType = mapSignatureParamType(ctx, param.getPlainType());
    ParameterTypeFlags newFlags = param.getParameterFlags().withEscaping(false);

    // For the 'self' of a method, strip off 'inout'.
    if (isMethod) {
      newFlags = newFlags.withInOut(false);
    }

    AnyFunctionType::Param newParam(newParamType, param.getLabel(), newFlags);
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

  signature.Name = getFullName();
  signature.InProtocolExtension
    = static_cast<bool>(getDeclContext()->getExtendedProtocolDecl());
  signature.IsInstanceMember = isInstanceMember();
  signature.IsVariable = isa<VarDecl>(this);
  signature.IsFunction = isa<AbstractFunctionDecl>(this);

  // Unary operators also include prefix/postfix.
  if (auto func = dyn_cast<FuncDecl>(this)) {
    if (func->isUnaryOperator()) {
      signature.UnaryOperator = func->getAttrs().getUnaryOperatorKind();
    }
  }

  if (auto *extension = dyn_cast<ExtensionDecl>(getDeclContext()))
    if (extension->isGeneric())
      signature.InExtensionOfGenericType = true;

  return signature;
}

CanType ValueDecl::getOverloadSignatureType() const {
  if (auto *afd = dyn_cast<AbstractFunctionDecl>(this)) {
    bool isMethod = afd->hasImplicitSelfDecl();
    return mapSignatureFunctionType(
                           getASTContext(), getInterfaceType(),
                           /*topLevelFunction=*/true,
                           isMethod,
                           /*isInitializer=*/isa<ConstructorDecl>(afd),
                           isMethod ? 2 : 1)->getCanonicalType();
  }

  if (isa<AbstractStorageDecl>(this)) {
    // First, get the default overload signature type for the decl. For vars,
    // this is the empty tuple type, as variables cannot be overloaded directly
    // by type. For subscripts, it's their interface type.
    CanType defaultSignatureType;
    if (isa<VarDecl>(this)) {
      defaultSignatureType = TupleType::getEmpty(getASTContext());
    } else {
      defaultSignatureType = getInterfaceType()->getCanonicalType();
    }

    // We want to curry the default signature type with the 'self' type of the
    // given context (if any) in order to ensure the overload signature type
    // is unique across different contexts, such as between a protocol extension
    // and struct decl.
    return defaultSignatureType->addCurriedSelfType(getDeclContext())
                               ->getCanonicalType();
  }

  // Note: If you add more cases to this function, you should update the
  // implementation of the swift::conflicting overload that deals with
  // overload types, in order to account for cases where the overload types
  // don't match, but the decls differ and therefore always conflict.

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

bool ValueDecl::isDynamic() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    IsDynamicRequest{const_cast<ValueDecl *>(this)},
    getAttrs().hasAttribute<DynamicAttr>());
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

bool ValueDecl::canBeAccessedByDynamicLookup() const {
  if (!hasName())
    return false;

  // Dynamic lookup can only find class and protocol members, or extensions of
  // classes.
  auto nominalDC = getDeclContext()->getSelfNominalTypeDecl();
  if (!nominalDC ||
      (!isa<ClassDecl>(nominalDC) && !isa<ProtocolDecl>(nominalDC)))
    return false;

  // Dynamic lookup cannot find results within a non-protocol generic context,
  // because there is no sensible way to infer the generic arguments.
  if (getDeclContext()->isGenericContext() && !isa<ProtocolDecl>(nominalDC))
    return false;

  // Dynamic lookup can find functions, variables, and subscripts.
  if (!isa<FuncDecl>(this) && !isa<VarDecl>(this) && !isa<SubscriptDecl>(this))
    return false;

  // Dynamic lookup can only find @objc members.
  if (!isObjC())
    return false;

  return true;
}

ArrayRef<ValueDecl *>
ValueDecl::getSatisfiedProtocolRequirements(bool Sorted) const {
  // Dig out the nominal type.
  NominalTypeDecl *NTD = getDeclContext()->getSelfNominalTypeDecl();
  if (!NTD || isa<ProtocolDecl>(NTD))
    return {};

  return NTD->getSatisfiedProtocolRequirementsForMember(this, Sorted);
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

Type ValueDecl::getInterfaceType() const {
  assert(hasInterfaceType() && "No interface type was set");
  return TypeAndAccess.getPointer();
}

void ValueDecl::setInterfaceType(Type type) {
  if (type) {
    assert(!type->hasTypeVariable() && "Type variable in interface type");
    assert(!type->is<InOutType>() && "Interface type must be materializable");

    // lldb creates global typealiases with archetypes in them.
    // FIXME: Add an isDebugAlias() flag, like isDebugVar().
    //
    // Also, ParamDecls in closure contexts can have type variables
    // archetype in them during constraint generation.
    if (!isa<TypeAliasDecl>(this) &&
        !(isa<ParamDecl>(this) &&
          isa<AbstractClosureExpr>(getDeclContext()))) {
      assert(!type->hasArchetype() &&
             "Archetype in interface type");
    }

    if (type->hasError())
      setInvalid();
  }

  TypeAndAccess.setPointer(type);
}

bool ValueDecl::hasValidSignature() const {
  if (!hasInterfaceType())
    return false;
  // FIXME -- The build blows up if the correct code is used:
  // return getValidationState() > ValidationState::CheckingWithValidSignature;
  return getValidationState() != ValidationState::Checking;
}

Optional<ObjCSelector> ValueDecl::getObjCRuntimeName(
                                              bool skipIsObjCResolution) const {
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

  return None;
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
  if (isImplicit())
    return SourceLoc();

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
/// at the SIL, LLVM, and machine levels due to being @usableFromInline.
bool ValueDecl::isUsableFromInline() const {
  assert(getFormalAccess() == AccessLevel::Internal);

  if (getAttrs().hasAttribute<UsableFromInlineAttr>() ||
      getAttrs().hasAttribute<InlinableAttr>())
    return true;

  if (auto *accessor = dyn_cast<AccessorDecl>(this)) {
    auto *storage = accessor->getStorage();
    if (storage->getAttrs().hasAttribute<UsableFromInlineAttr>() ||
        storage->getAttrs().hasAttribute<InlinableAttr>())
      return true;
  }

  if (auto *EED = dyn_cast<EnumElementDecl>(this))
    if (EED->getParentEnum()->getAttrs().hasAttribute<UsableFromInlineAttr>())
      return true;

  if (auto *containingProto = dyn_cast<ProtocolDecl>(getDeclContext())) {
    if (isProtocolRequirement() &&
        containingProto->getAttrs().hasAttribute<UsableFromInlineAttr>())
      return true;
  }

  if (auto *DD = dyn_cast<DestructorDecl>(this))
    if (auto *CD = dyn_cast<ClassDecl>(DD->getDeclContext()))
      if (CD->getAttrs().hasAttribute<UsableFromInlineAttr>())
        return true;

  return false;
}

/// Return the access level of an internal or public declaration
/// that's been testably imported.
static AccessLevel getTestableAccess(const ValueDecl *decl) {
  // Non-final classes are considered open to @testable importers.
  if (auto cls = dyn_cast<ClassDecl>(decl)) {
    if (!cls->isFinal())
      return AccessLevel::Open;

  // Non-final overridable class members are considered open to
  // @testable importers.
  } else if (decl->isPotentiallyOverridable()) {
    if (!cast<ValueDecl>(decl)->isFinal())
      return AccessLevel::Open;
  }

  // Everything else is considered public.
  return AccessLevel::Public;
}

/// Adjust \p access based on whether \p VD is \@usableFromInline or has been
/// testably imported from \p useDC.
///
/// \p access isn't always just `VD->getFormalAccess()` because this adjustment
/// may be for a write, in which case the setter's access might be used instead.
static AccessLevel getAdjustedFormalAccess(const ValueDecl *VD,
                                           AccessLevel access,
                                           const DeclContext *useDC,
                                           bool treatUsableFromInlineAsPublic) {
  if (treatUsableFromInlineAsPublic &&
      access == AccessLevel::Internal &&
      VD->isUsableFromInline()) {
    return AccessLevel::Public;
  }

  if (useDC && (access == AccessLevel::Internal ||
                access == AccessLevel::Public)) {
    if (auto *useSF = dyn_cast<SourceFile>(useDC->getModuleScopeContext()))
      if (useSF->hasTestableImport(VD->getModuleContext()))
        return getTestableAccess(VD);
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

  // Handle @testable.
  switch (effectiveAccess) {
  case AccessLevel::Open:
    break;
  case AccessLevel::Public:
  case AccessLevel::Internal:
    if (getModuleContext()->isTestingEnabled())
      effectiveAccess = getTestableAccess(this);
    break;
  case AccessLevel::FilePrivate:
    break;
  case AccessLevel::Private:
    effectiveAccess = AccessLevel::FilePrivate;
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
  assert(isa<ClassDecl>(this) || isa<ConstructorDecl>(this) ||
         isPotentiallyOverridable());

  AccessLevel access =
      getAdjustedFormalAccess(this, useDC,
                              /*treatUsableFromInlineAsPublic*/false);
  return access == AccessLevel::Open;
}

/// Given the formal access level for using \p VD, compute the scope where
/// \p VD may be accessed, taking \@usableFromInline, \@testable imports,
/// and enclosing access levels into account.
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
      return AccessScope(resultDC, /*private*/true);

    if (auto enclosingNominal = dyn_cast<NominalTypeDecl>(resultDC)) {
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

  switch (access) {
  case AccessLevel::Private:
  case AccessLevel::FilePrivate:
    assert(resultDC->isModuleScopeContext());
    return AccessScope(resultDC, access == AccessLevel::Private);
  case AccessLevel::Internal:
    return AccessScope(resultDC->getParentModule());
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
/// See ValueDecl::isAccessibleFrom for a description of \p forConformance.
static bool checkAccessUsingAccessScopes(const DeclContext *useDC,
                                         const ValueDecl *VD,
                                         AccessLevel access) {
  AccessScope accessScope =
      getAccessScopeForFormalAccess(VD, access, useDC,
                                    /*treatUsableFromInlineAsPublic*/false);
  return accessScope.getDeclContext() == useDC ||
         AccessScope(useDC).isChildOf(accessScope);
}

/// Checks if \p VD may be used from \p useDC, taking \@testable imports into
/// account.
///
/// When \p access is the same as `VD->getFormalAccess()` and the enclosing
/// context of \p VD is usable from \p useDC, this ought to be the same as
/// getting the AccessScope for `VD` and checking if \p useDC is within it.
/// However, there's a source compatibility hack around protocol extensions
/// that makes it not quite the same.
///
/// See ValueDecl::isAccessibleFrom for a description of \p forConformance.
static bool checkAccess(const DeclContext *useDC, const ValueDecl *VD,
                        AccessLevel access, bool forConformance) {
  auto *sourceDC = VD->getDeclContext();

  if (!forConformance) {
    if (auto *proto = sourceDC->getSelfProtocolDecl()) {
      // FIXME: Swift 4.1 allowed accessing protocol extension methods that were
      // marked 'public' if the protocol was '@_versioned' (now
      // '@usableFromInline'). Which works at the ABI level, so let's keep
      // supporting that here by explicitly checking for it.
      if (access == AccessLevel::Public &&
          proto->getFormalAccess() == AccessLevel::Internal &&
          proto->isUsableFromInline()) {
        return true;
      }

      // Skip the fast path below and just compare access scopes.
      return checkAccessUsingAccessScopes(useDC, VD, access);
    }
  }

  // Fast path: assume that the client context already has access to our parent
  // DeclContext, and only check what might be different about this declaration.
  if (!useDC)
    return access >= AccessLevel::Public;

  switch (access) {
  case AccessLevel::Private:
    return (useDC == sourceDC ||
      AccessScope::allowsPrivateAccess(useDC, sourceDC));
  case AccessLevel::FilePrivate:
    return useDC->getModuleScopeContext() == sourceDC->getModuleScopeContext();
  case AccessLevel::Internal: {
    const ModuleDecl *sourceModule = sourceDC->getParentModule();
    const DeclContext *useFile = useDC->getModuleScopeContext();
    if (useFile->getParentModule() == sourceModule)
      return true;
    if (auto *useSF = dyn_cast<SourceFile>(useFile))
      if (useSF->hasTestableImport(sourceModule))
        return true;
    return false;
  }
  case AccessLevel::Public:
  case AccessLevel::Open:
    return true;
  }
  llvm_unreachable("bad access level");
}

bool ValueDecl::isAccessibleFrom(const DeclContext *useDC,
                                 bool forConformance) const {
  auto access = getFormalAccess();
  bool result = checkAccess(useDC, this, access, forConformance);

  // For everything outside of protocols and operators, we should get the same
  // result using either implementation of checkAccess, because useDC must
  // already have access to this declaration's DeclContext.
  // FIXME: Arguably, we're doing the wrong thing for operators here too,
  // because we're finding internal operators within private types. Fortunately
  // we have a requirement that a member operator take the enclosing type as an
  // argument, so it won't ever match.
  assert(getDeclContext()->getSelfProtocolDecl() ||
         isOperator() ||
         result == checkAccessUsingAccessScopes(useDC, this, access));

  return result;
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

  auto access = getSetterFormalAccess();
  return checkAccess(DC, this, access, forConformance);
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
  if (access == AccessLevel::Open && !isPotentiallyOverridable()) {
    assert(!isa<ClassDecl>(this) &&
           "copying 'open' onto a class has complications");
    access = AccessLevel::Public;
  }

  setAccess(access);

  // Inherit the @usableFromInline attribute.
  if (source->getAttrs().hasAttribute<UsableFromInlineAttr>() &&
      !getAttrs().hasAttribute<UsableFromInlineAttr>() &&
      !getAttrs().hasAttribute<InlinableAttr>() &&
      DeclAttribute::canAttributeAppearOnDecl(DAK_UsableFromInline, this)) {
    auto &ctx = getASTContext();
    auto *clonedAttr = new (ctx) UsableFromInlineAttr(/*implicit=*/true);
    getAttrs().add(clonedAttr);
  }
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

  if (int result = type1->getBaseName().getIdentifier().str().compare(
                                  type2->getBaseName().getIdentifier().str()))
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
                            /*treatUsableFromInlineAsPublic=*/true).isPublic())
    return false;

  // Check for an explicit @_fixed_layout or @_frozen attribute.
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
  // If we're not formally resilient, don't check the module resilience
  // strategy.
  if (!isFormallyResilient())
    return false;

  // Otherwise, check the module.
  switch (getParentModule()->getResilienceStrategy()) {
  case ResilienceStrategy::Resilient:
    return true;
  case ResilienceStrategy::Default:
    return false;
  }

  llvm_unreachable("Unhandled ResilienceStrategy in switch.");
}

bool NominalTypeDecl::isResilient(ModuleDecl *M,
                                  ResilienceExpansion expansion) const {
  switch (expansion) {
  case ResilienceExpansion::Minimal:
    return isResilient();
  case ResilienceExpansion::Maximal:
    return isResilient() && M != getModuleContext();
  }
  llvm_unreachable("bad resilience expansion");
}

void NominalTypeDecl::computeType() {
  assert(!hasInterfaceType());

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
  DeclaredInterfaceType
};

static Type computeNominalType(NominalTypeDecl *decl, DeclTypeKind kind) {
  ASTContext &ctx = decl->getASTContext();

  // Get the parent type.
  Type Ty;
  DeclContext *dc = decl->getDeclContext();
  if (dc->isTypeContext()) {
    switch (kind) {
    case DeclTypeKind::DeclaredType: {
      auto *nominal = dc->getSelfNominalTypeDecl();
      if (nominal)
        Ty = nominal->getDeclaredType();
      break;
    }
    case DeclTypeKind::DeclaredInterfaceType:
      Ty = dc->getDeclaredInterfaceType();
      if (Ty->is<ErrorType>())
        Ty = Type();
      break;
    }
  }

  if (decl->getGenericParams() &&
      !isa<ProtocolDecl>(decl)) {
    switch (kind) {
    case DeclTypeKind::DeclaredType:
      return UnboundGenericType::get(decl, Ty, ctx);
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

  auto interfaceType = getDeclaredInterfaceType();
  decl->DeclaredTyInContext = mapTypeIntoContext(interfaceType);

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
  assert(!extension->alreadyBoundToNominal() && "Already added extension");
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

auto NominalTypeDecl::getStoredProperties(bool skipInaccessible) const
    -> StoredPropertyRange {
  // Clang-imported classes never have stored properties.
  if (hasClangNode() && isa<ClassDecl>(this))
    return StoredPropertyRange(DeclRange(nullptr, nullptr),
                               ToStoredProperty(skipInaccessible));

  return StoredPropertyRange(getMembers(),
                             ToStoredProperty(skipInaccessible));
}

bool NominalTypeDecl::isOptionalDecl() const {
  return this == getASTContext().getOptionalDecl();
}

GenericTypeDecl::GenericTypeDecl(DeclKind K, DeclContext *DC,
                                 Identifier name, SourceLoc nameLoc,
                                 MutableArrayRef<TypeLoc> inherited,
                                 GenericParamList *GenericParams) :
    GenericContext(DeclContextKind::GenericTypeDecl, DC),
    TypeDecl(K, DC, name, nameLoc, inherited) {
  setGenericParams(GenericParams);
}

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
  return { TypeAliasLoc, getNameLoc() };
}

void TypeAliasDecl::setUnderlyingType(Type underlying) {
  setValidationToChecked();

  // lldb creates global typealiases containing archetypes
  // sometimes...
  if (underlying->hasArchetype() && isGenericContext())
    underlying = underlying->mapTypeOutOfContext();
  UnderlyingTy.setType(underlying);

  // FIXME -- if we already have an interface type, we're changing the
  // underlying type. See the comment in the ProtocolDecl case of
  // validateDecl().
  if (!hasInterfaceType()) {
    // Set the interface type of this declaration.
    ASTContext &ctx = getASTContext();

    auto *genericSig = getGenericSignature();
    auto subs = SubstitutionMap::get(
        genericSig, [&](SubstitutableType *type) -> Type { return type; },
        MakeAbstractConformanceForGenericType());

    Type parent;
    auto parentDC = getDeclContext();
    if (parentDC->isTypeContext())
      parent = parentDC->getDeclaredInterfaceType();
    auto sugaredType = NameAliasType::get(this, parent, subs, underlying);
    setInterfaceType(MetatypeType::get(sugaredType, ctx));
  }
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

Type AbstractTypeParamDecl::getSuperclass() const {
  auto *genericEnv = getDeclContext()->getGenericEnvironmentOfContext();
  assert(genericEnv != nullptr && "Too much circularity");

  auto contextTy = genericEnv->mapTypeIntoContext(getDeclaredInterfaceType());
  if (auto *archetype = contextTy->getAs<ArchetypeType>())
    return archetype->getSuperclass();

  // FIXME: Assert that this is never queried.
  return nullptr;
}

ArrayRef<ProtocolDecl *>
AbstractTypeParamDecl::getConformingProtocols() const {
  auto *genericEnv = getDeclContext()->getGenericEnvironmentOfContext();
  assert(genericEnv != nullptr && "Too much circularity");

  auto contextTy = genericEnv->mapTypeIntoContext(getDeclaredInterfaceType());
  if (auto *archetype = contextTy->getAs<ArchetypeType>())
    return archetype->getConformsTo();

  // FIXME: Assert that this is never queried.
  return { };
}

GenericTypeParamDecl::GenericTypeParamDecl(DeclContext *dc, Identifier name,
                                           SourceLoc nameLoc,
                                           unsigned depth, unsigned index)
  : AbstractTypeParamDecl(DeclKind::GenericTypeParam, dc, name, nameLoc) {
  Bits.GenericTypeParamDecl.Depth = depth;
  assert(Bits.GenericTypeParamDecl.Depth == depth && "Truncation");
  Bits.GenericTypeParamDecl.Index = index;
  assert(Bits.GenericTypeParamDecl.Index == index && "Truncation");
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
      TrailingWhere(trailingWhere) {
}

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
  assert(!hasInterfaceType());

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
  SourceLoc endLoc;
  if (auto TWC = getTrailingWhereClause())
    endLoc = TWC->getSourceRange().End;
  else if (!getInherited().empty()) {
    endLoc = getInherited().back().getSourceRange().End;
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
    overridden = AbstractTypeParamDecl::getOverriddenDecls();

  llvm::TinyPtrVector<AssociatedTypeDecl *> assocTypes;
  for (auto decl : overridden) {
    assocTypes.push_back(cast<AssociatedTypeDecl>(decl));
  }
  return assocTypes;
}

AssociatedTypeDecl *AssociatedTypeDecl::getAssociatedTypeAnchor() const {
  auto overridden = getOverriddenDecls();

  // If this declaration does not override any other declarations, it's
  // the anchor.
  if (overridden.empty()) return const_cast<AssociatedTypeDecl *>(this);

  // Find the best anchor among the anchors of the overridden decls.
  AssociatedTypeDecl *bestAnchor = nullptr;
  for (auto assocType : overridden) {
    auto anchor = assocType->getAssociatedTypeAnchor();
    if (!bestAnchor || compare(anchor, bestAnchor) < 0)
      bestAnchor = anchor;
  }

  return bestAnchor;
}

EnumDecl::EnumDecl(SourceLoc EnumLoc,
                     Identifier Name, SourceLoc NameLoc,
                     MutableArrayRef<TypeLoc> Inherited,
                     GenericParamList *GenericParams, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::Enum, Parent, Name, NameLoc, Inherited,
                    GenericParams),
    EnumLoc(EnumLoc)
{
  Bits.EnumDecl.Circularity
    = static_cast<unsigned>(CircularityCheck::Unchecked);
  Bits.EnumDecl.HasAssociatedValues
    = static_cast<unsigned>(AssociatedValueCheck::Unchecked);
  Bits.EnumDecl.HasAnyUnavailableValues
    = false;
}

Type EnumDecl::getRawType() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    EnumRawTypeRequest{const_cast<EnumDecl *>(this),
                       TypeResolutionStage::Interface}, Type());
}

StructDecl::StructDecl(SourceLoc StructLoc, Identifier Name, SourceLoc NameLoc,
                       MutableArrayRef<TypeLoc> Inherited,
                       GenericParamList *GenericParams, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::Struct, Parent, Name, NameLoc, Inherited,
                    GenericParams),
    StructLoc(StructLoc)
{
  Bits.StructDecl.HasUnreferenceableStorage = false;
}


ClassDecl::ClassDecl(SourceLoc ClassLoc, Identifier Name, SourceLoc NameLoc,
                     MutableArrayRef<TypeLoc> Inherited,
                     GenericParamList *GenericParams, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::Class, Parent, Name, NameLoc, Inherited,
                    GenericParams),
    ClassLoc(ClassLoc) {
  Bits.ClassDecl.Circularity
    = static_cast<unsigned>(CircularityCheck::Unchecked);
  Bits.ClassDecl.RequiresStoredPropertyInits = 0;
  Bits.ClassDecl.InheritsSuperclassInits = 0;
  Bits.ClassDecl.RawForeignKind = 0;
  Bits.ClassDecl.HasDestructorDecl = 0;
  Bits.ClassDecl.ObjCKind = 0;
  Bits.ClassDecl.HasMissingDesignatedInitializers = 0;
  Bits.ClassDecl.HasMissingVTableEntries = 0;
}

DestructorDecl *ClassDecl::getDestructor() {
  auto results = lookupDirect(DeclBaseName::createDestructor());
  assert(!results.empty() && "Class without destructor?");
  assert(results.size() == 1 && "More than one destructor?");
  return cast<DestructorDecl>(results.front());
}

void ClassDecl::addImplicitDestructor() {
  if (hasDestructor() || isInvalid())
    return;

  auto &ctx = getASTContext();
  auto *DD = new (ctx) DestructorDecl(getLoc(), this);

  DD->setImplicit();
  DD->setValidationToChecked();

  // Create an empty body for the destructor.
  DD->setBody(BraceStmt::create(ctx, getLoc(), { }, getLoc(), true));
  addMember(DD);
  setHasDestructor();

  // Propagate access control and versioned-ness.
  DD->copyFormalAccessFrom(this, /*sourceIsParentContext*/true);

  // Wire up generic environment of DD.
  DD->setGenericEnvironment(getGenericEnvironmentOfContext());

  // Mark DD as ObjC, as all dtors are.
  DD->setIsObjC(getASTContext().LangOpts.EnableObjCInterop);
  if (getASTContext().LangOpts.EnableObjCInterop) {
    recordObjCMethod(DD, DD->getObjCSelector());
  }

  // Assign DD the interface type (Self) -> () -> ()
  DD->computeType();
}


bool ClassDecl::hasMissingDesignatedInitializers() const {
  auto *mutableThis = const_cast<ClassDecl *>(this);
  auto flags = OptionSet<LookupDirectFlags>();
  flags |= LookupDirectFlags::IgnoreNewExtensions;
  (void)mutableThis->lookupDirect(DeclBaseName::createConstructor(),
                                  flags);
  return Bits.ClassDecl.HasMissingDesignatedInitializers;
}

bool ClassDecl::hasMissingVTableEntries() const {
  (void)getMembers();
  return Bits.ClassDecl.HasMissingVTableEntries;
}

bool ClassDecl::inheritsSuperclassInitializers(LazyResolver *resolver) {
  // Check whether we already have a cached answer.
  if (addedImplicitInitializers())
    return Bits.ClassDecl.InheritsSuperclassInits;

  // If there's no superclass, there's nothing to inherit.
  ClassDecl *superclassDecl;
  if (!(superclassDecl = getSuperclassDecl())) {
    setAddedImplicitInitializers();
    return false;
  }

  // If the superclass has known-missing designated initializers, inheriting
  // is unsafe.
  if (superclassDecl->hasMissingDesignatedInitializers())
    return false;

  // Otherwise, do all the work of resolving constructors, which will also
  // calculate the right answer.
  if (resolver == nullptr)
    resolver = getASTContext().getLazyResolver();
  if (resolver)
    resolver->resolveImplicitConstructors(this);

  return Bits.ClassDecl.InheritsSuperclassInits;
}

ObjCClassKind ClassDecl::checkObjCAncestry() const {
  // See if we've already computed this.
  if (Bits.ClassDecl.ObjCKind)
    return ObjCClassKind(Bits.ClassDecl.ObjCKind - 1);

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

    // Is this class @objc? For the current class, only look at the attribute
    // to avoid cycles; for superclasses, compute @objc completely.
    if ((CD == this && CD->getAttrs().hasAttribute<ObjCAttr>()) ||
        (CD != this && CD->isObjC()))
      isObjC = true;

    if (!CD->hasSuperclass())
      break;
    CD = CD->getSuperclassDecl();
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
  const_cast<ClassDecl *>(this)->Bits.ClassDecl.ObjCKind
    = unsigned(kind) + 1;
  return kind;
}

ClassDecl::MetaclassKind ClassDecl::getMetaclassKind() const {
  assert(getASTContext().LangOpts.EnableObjCInterop &&
         "querying metaclass kind without objc interop");
  auto objc = checkObjCAncestry() != ObjCClassKind::NonObjC;
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
    auto *CurMethod = dyn_cast<AbstractFunctionDecl>(M);
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
      auto *CurMethod = dyn_cast<AbstractFunctionDecl>(M);
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
    C = C->getSuperclassDecl();
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

bool EnumDecl::hasPotentiallyUnavailableCaseValue() const {
  switch (static_cast<AssociatedValueCheck>(Bits.EnumDecl.HasAssociatedValues)) {
    case AssociatedValueCheck::Unchecked:
      // Compute below
      this->hasOnlyCasesWithoutAssociatedValues();
      LLVM_FALLTHROUGH;
    default:
      return static_cast<bool>(Bits.EnumDecl.HasAnyUnavailableValues);
  }
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
  for (auto elt : getAllElements()) {
    for (auto Attr : elt->getAttrs()) {
      if (auto AvAttr = dyn_cast<AvailableAttr>(Attr)) {
        if (!AvAttr->isInvalid()) {
          const_cast<EnumDecl*>(this)->Bits.EnumDecl.HasAnyUnavailableValues
            = true;
        }
      }
    }

    if (elt->hasAssociatedValues()) {
      const_cast<EnumDecl*>(this)->Bits.EnumDecl.HasAssociatedValues
        = static_cast<unsigned>(AssociatedValueCheck::HasAssociatedValues);
      return false;
    }
  }
  const_cast<EnumDecl*>(this)->Bits.EnumDecl.HasAssociatedValues
    = static_cast<unsigned>(AssociatedValueCheck::NoAssociatedValues);
  return true;
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
  switch (containingModule->getResilienceStrategy()) {
  case ResilienceStrategy::Default:
    return true;
  case ResilienceStrategy::Resilient:
    break;
  }

  // Non-public, non-versioned enums are always exhaustive.
  AccessScope accessScope = getFormalAccessScope(/*useDC*/nullptr,
                                                 /*respectVersioned*/true);
  if (!accessScope.isPublic())
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
    if (useSF->hasTestableImport(containingModule))
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

  // Otherwise, the only non-exhaustive cases are those that don't have a fixed
  // layout.
  assert(isFormallyExhaustive(M) == !isResilient(M,ResilienceExpansion::Maximal)
         && "ignoring the effects of @inlinable, @testable, and @objc, "
            "these should match up");
  return !isResilient(M, expansion);
}

ProtocolDecl::ProtocolDecl(DeclContext *DC, SourceLoc ProtocolLoc,
                           SourceLoc NameLoc, Identifier Name,
                           MutableArrayRef<TypeLoc> Inherited,
                           TrailingWhereClause *TrailingWhere)
    : NominalTypeDecl(DeclKind::Protocol, DC, Name, NameLoc, Inherited,
                      nullptr),
      ProtocolLoc(ProtocolLoc) {
  Bits.ProtocolDecl.RequiresClassValid = false;
  Bits.ProtocolDecl.RequiresClass = false;
  Bits.ProtocolDecl.ExistentialConformsToSelfValid = false;
  Bits.ProtocolDecl.ExistentialConformsToSelf = false;
  Bits.ProtocolDecl.Circularity
    = static_cast<unsigned>(CircularityCheck::Unchecked);
  Bits.ProtocolDecl.NumRequirementsInSignature = 0;
  Bits.ProtocolDecl.HasMissingRequirements = false;
  Bits.ProtocolDecl.KnownProtocol = 0;
  setTrailingWhereClause(TrailingWhere);
}

llvm::TinyPtrVector<ProtocolDecl *>
ProtocolDecl::getInheritedProtocols() const {
  llvm::TinyPtrVector<ProtocolDecl *> result;
  SmallPtrSet<const ProtocolDecl *, 4> known;
  known.insert(this);
  bool anyObject = false;
  for (const auto found :
           getDirectlyInheritedNominalTypeDecls(
             const_cast<ProtocolDecl *>(this), anyObject)) {
    if (auto proto = dyn_cast<ProtocolDecl>(found.second)) {
      if (known.insert(proto).second)
        result.push_back(proto);
    }
  }

  return result;
}

llvm::TinyPtrVector<AssociatedTypeDecl *>
ProtocolDecl::getAssociatedTypeMembers() const {
  llvm::TinyPtrVector<AssociatedTypeDecl *> result;

  // Clang-imported protocols never have associated types.
  if (hasClangNode())
    return result;

  // Deserialized @objc protocols never have associated types.
  if (!getParentSourceFile() && isObjC())
    return result;

  // Find the associated type declarations.
  for (auto member : getMembers()) {
    if (auto ATD = dyn_cast<AssociatedTypeDecl>(member)) {
      result.push_back(ATD);
    }
  }

  return result;
}

Type ProtocolDecl::getSuperclass() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    SuperclassTypeRequest{const_cast<ProtocolDecl *>(this),
                          TypeResolutionStage::Interface},
    Type());
}

ClassDecl *ProtocolDecl::getSuperclassDecl() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    SuperclassDeclRequest{const_cast<ProtocolDecl *>(this)}, nullptr);
}

void ProtocolDecl::setSuperclass(Type superclass) {
  assert((!superclass || !superclass->hasArchetype())
         && "superclass must be interface type");
  LazySemanticInfo.Superclass.setPointerAndInt(superclass, true);
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

  return walkInheritedProtocols([super](ProtocolDecl *inherited) {
    if (inherited == super)
      return TypeWalker::Action::Stop;

    return TypeWalker::Action::Continue;
  });
}

bool ProtocolDecl::requiresClassSlow() {
  // Set this first to catch (invalid) circular inheritance.
  Bits.ProtocolDecl.RequiresClassValid = true;
  Bits.ProtocolDecl.RequiresClass = false;

  // Quick check: @objc protocols require a class.
  if (isObjC())
    return Bits.ProtocolDecl.RequiresClass = true;

  // Determine the set of nominal types that this protocol inherits.
  bool anyObject = false;
  auto allInheritedNominals =
    getDirectlyInheritedNominalTypeDecls(this, anyObject);

  // Quick check: do we inherit AnyObject?
  if (anyObject) {
    Bits.ProtocolDecl.RequiresClass = true;
    return true;
  }

  // Look through all of the inherited nominals for a superclass or a
  // class-bound protocol.
  for (const auto found : allInheritedNominals) {
    // Superclass bound.
    if (isa<ClassDecl>(found.second))
      return Bits.ProtocolDecl.RequiresClass = true;

    // A protocol that might be class-constrained;
    if (auto proto = dyn_cast<ProtocolDecl>(found.second)) {
      if (proto->requiresClass())
        return Bits.ProtocolDecl.RequiresClass = true;
    }
  }

  return Bits.ProtocolDecl.RequiresClass;
}

bool ProtocolDecl::existentialConformsToSelfSlow() {
  // Assume for now that the existential conforms to itself; this
  // prevents circularity issues.
  Bits.ProtocolDecl.ExistentialConformsToSelfValid = true;
  Bits.ProtocolDecl.ExistentialConformsToSelf = true;

  if (!isObjC()) {
    Bits.ProtocolDecl.ExistentialConformsToSelf = false;
    return false;
  }

  // Check whether this protocol conforms to itself.
  for (auto member : getMembers()) {
    if (member->isInvalid())
      continue;

    if (auto vd = dyn_cast<ValueDecl>(member)) {
      if (!vd->isInstanceMember()) {
        // A protocol cannot conform to itself if it has static members.
        Bits.ProtocolDecl.ExistentialConformsToSelf = false;
        return false;
      }
    }
  }

  // Check whether any of the inherited protocols fail to conform to
  // themselves.
  for (auto proto : getInheritedProtocols()) {
    if (!proto->existentialConformsToSelf()) {
      Bits.ProtocolDecl.ExistentialConformsToSelf = false;
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
    for (auto &elt : tuple->getElements()) {
      kind |= findProtocolSelfReferences(proto, elt.getType(), skipAssocTypes);
    }
    return kind;
  } 

  // Function preserve variance in the result type, and flip variance in
  // the parameter type.
  if (auto funcTy = type->getAs<AnyFunctionType>()) {
    auto inputKind = SelfReferenceKind::None();
    for (auto &elt : funcTy->getParams()) {
      inputKind |= findProtocolSelfReferences(proto, elt.getOldType(),
                                              skipAssocTypes);
    }
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
  if (auto optType = type->getOptionalObjectType()) {
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

/// Find Self references in a generic signature's same-type requirements.
static SelfReferenceKind
findProtocolSelfReferences(const ProtocolDecl *protocol,
                           GenericSignature *genericSig){
  if (!genericSig) return SelfReferenceKind::None();

  auto selfTy = protocol->getSelfInterfaceType();
  for (const auto &req : genericSig->getRequirements()) {
    if (req.getKind() != RequirementKind::SameType)
      continue;

    if (req.getFirstType()->isEqual(selfTy) ||
        req.getSecondType()->isEqual(selfTy))
      return SelfReferenceKind::Requirement();
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

  if (auto func = dyn_cast<AbstractFunctionDecl>(value)) {
    // Skip the 'self' parameter.
    type = type->castTo<AnyFunctionType>()->getResult();

    // Methods of non-final classes can only contain a covariant 'Self'
    // as a function result type.
    if (!allowCovariantParameters) {
      auto inputKind = SelfReferenceKind::None();
      for (auto &elt : type->castTo<AnyFunctionType>()->getParams()) {
        inputKind |= ::findProtocolSelfReferences(this, elt.getOldType(),
                                                  skipAssocTypes);
      }

      if (inputKind.parameter)
        return SelfReferenceKind::Other();
    }

    // Check the requirements of a generic function.
    if (func->isGeneric()) {
      if (auto result =
            ::findProtocolSelfReferences(this, func->getGenericSignature()))
        return result;
    }

    return ::findProtocolSelfReferences(this, type,
                                        skipAssocTypes);
  } else if (auto subscript = dyn_cast<SubscriptDecl>(value)) {
    // Check the requirements of a generic subscript.
    if (subscript->isGeneric()) {
      if (auto result =
            ::findProtocolSelfReferences(this,
                                         subscript->getGenericSignature()))
        return result;
    }

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
  Bits.ProtocolDecl.ExistentialTypeSupportedValid = true;
  Bits.ProtocolDecl.ExistentialTypeSupported = true;

  // ObjC protocols can always be existential.
  if (isObjC())
    return true;

  for (auto member : getMembers()) {
    // Check for associated types.
    if (isa<AssociatedTypeDecl>(member)) {
      // An existential type cannot be used if the protocol has an
      // associated type.
      Bits.ProtocolDecl.ExistentialTypeSupported = false;
      return false;
    }

    // For value members, look at their type signatures.
    if (auto valueMember = dyn_cast<ValueDecl>(member)) {
      if (resolver && !valueMember->hasInterfaceType())
        resolver->resolveDeclSignature(valueMember);

      if (!isAvailableInExistential(valueMember)) {
        Bits.ProtocolDecl.ExistentialTypeSupported = false;
        return false;
      }
    }
  }

  // Check whether all of the inherited protocols can have existential
  // types themselves.
  for (auto proto : getInheritedProtocols()) {
    if (!proto->existentialTypeSupported(resolver)) {
      Bits.ProtocolDecl.ExistentialTypeSupported = false;
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
  auto *outerGenericParams = getParent()->getGenericParamsOfContext();

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

  // Compute and record the signature.
  auto requirementSig =
    GenericSignatureBuilder::computeRequirementSignature(this);
  RequirementSignature = requirementSig->getRequirements().data();
  assert(RequirementSignature != nullptr);
  Bits.ProtocolDecl.NumRequirementsInSignature =
    requirementSig->getRequirements().size();
}

void ProtocolDecl::setRequirementSignature(ArrayRef<Requirement> requirements) {
  assert(!RequirementSignature && "already computed requirement signature");
  if (requirements.empty()) {
    RequirementSignature = reinterpret_cast<Requirement *>(this + 1);
    Bits.ProtocolDecl.NumRequirementsInSignature = 0;
  } else {
    RequirementSignature = getASTContext().AllocateCopy(requirements).data();
    Bits.ProtocolDecl.NumRequirementsInSignature = requirements.size();
  }
}

void AbstractStorageDecl::overwriteImplInfo(StorageImplInfo implInfo) {
  setFieldsFromImplInfo(implInfo);
  Accessors.getPointer()->overwriteImplInfo(implInfo);
}

void AbstractStorageDecl::setAccessors(StorageImplInfo implInfo,
                                       SourceLoc lbraceLoc,
                                       ArrayRef<AccessorDecl *> accessors,
                                       SourceLoc rbraceLoc) {
  setFieldsFromImplInfo(implInfo);

  // This method is called after we've already recorded an accessors clause
  // only on recovery paths and only when that clause was empty.
  auto record = Accessors.getPointer();
  if (record) {
    assert(record->getAllAccessors().empty());
    for (auto accessor : accessors) {
      (void) record->addOpaqueAccessor(accessor);
    }
  } else {
    record = AccessorRecord::create(getASTContext(),
                                    SourceRange(lbraceLoc, rbraceLoc),
                                    implInfo, accessors);
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
                                            StorageImplInfo storageInfo,
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
          numMissingOpaque--;                 \
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
  return new (mem) AccessorRecord(braces, storageInfo,
                                  accessors, accessorsCapacity);
}

AbstractStorageDecl::AccessorRecord::AccessorRecord(SourceRange braces,
                                           StorageImplInfo implInfo,
                                           ArrayRef<AccessorDecl *> accessors,
                                           AccessorIndex accessorsCapacity)
    : Braces(braces), ImplInfo(implInfo), NumAccessors(accessors.size()),
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

#ifndef NDEBUG
static bool isAccessor(AccessorDecl *accessor, AccessorKind kind,
                       AbstractStorageDecl *storage) {
  // TODO: this should check that the accessor belongs to this storage, but
  // the Clang importer currently likes to violate that condition.
  return (accessor && accessor->getAccessorKind() == kind);
}
#endif

void AbstractStorageDecl::setComputedSetter(AccessorDecl *setter) {
  assert(getImplInfo().getReadImpl() == ReadImplKind::Get);
  assert(!getImplInfo().supportsMutation());
  assert(getGetter() && "sanity check: missing getter");
  assert(!getSetter() && "already has a setter");
  assert(hasClangNode() && "should only be used for ObjC properties");
  assert(isAccessor(setter, AccessorKind::Set, this));
  assert(setter && "should not be called for readonly properties");

  overwriteImplInfo(StorageImplInfo::getMutableComputed());
  Accessors.getPointer()->addOpaqueAccessor(setter);
}

void
AbstractStorageDecl::setSynthesizedGetter(AccessorDecl *accessor) {
  assert(!getGetter() && "declaration doesn't already have getter!");
  assert(isAccessor(accessor, AccessorKind::Get, this));

  auto accessors = Accessors.getPointer();
  if (!accessors) {
    accessors = AccessorRecord::create(getASTContext(), SourceRange(),
                                       getImplInfo(), {});
    Accessors.setPointer(accessors);
  }

  accessors->addOpaqueAccessor(accessor);
}

void
AbstractStorageDecl::setSynthesizedReadCoroutine(AccessorDecl *accessor) {
  assert(!getReadCoroutine() && "already has a read accessor");
  assert(isAccessor(accessor, AccessorKind::Read, this));

  auto accessors = Accessors.getPointer();
  if (!accessors) {
    accessors = AccessorRecord::create(getASTContext(), SourceRange(),
                                       getImplInfo(), {});
    Accessors.setPointer(accessors);
  }

  accessors->addOpaqueAccessor(accessor);
}

void
AbstractStorageDecl::setSynthesizedSetter(AccessorDecl *accessor) {
  assert(getGetter() && "declaration doesn't already have getter!");
  assert(supportsMutation() && "adding setter to immutable storage");
  assert(isAccessor(accessor, AccessorKind::Set, this));

  Accessors.getPointer()->addOpaqueAccessor(accessor);
}

void
AbstractStorageDecl::setSynthesizedModifyCoroutine(AccessorDecl *accessor) {
  assert(getGetter() && "declaration doesn't already have getter!");
  assert(getSetter() && "declaration doesn't already have setter!");
  assert(supportsMutation() && "adding modify to immutable storage");
  assert(!getModifyCoroutine() && "already has a modify accessor");
  assert(isAccessor(accessor, AccessorKind::Modify, this));

  Accessors.getPointer()->addOpaqueAccessor(accessor);
}

void AbstractStorageDecl::addBehavior(TypeRepr *Type,
                                      Expr *Param) {
  assert(BehaviorInfo.getPointer() == nullptr && "already set behavior!");
  auto mem = getASTContext().Allocate(sizeof(BehaviorRecord),
                                      alignof(BehaviorRecord));
  auto behavior = new (mem) BehaviorRecord{Type, Param};
  BehaviorInfo.setPointer(behavior);
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

ObjCSelector
AbstractStorageDecl::getObjCGetterSelector(Identifier preferredName) const {
  // If the getter has an @objc attribute with a name, use that.
  if (auto getter = getGetter()) {
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
  auto setter = getSetter();
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

Type VarDecl::getType() const {
  if (!typeInContext) {
    const_cast<VarDecl *>(this)->typeInContext =
      getDeclContext()->mapTypeIntoContext(
        getInterfaceType());
  }

  return typeInContext;
}

void VarDecl::setType(Type t) {
  assert(t.isNull() || !t->is<InOutType>());
  typeInContext = t;
}

void VarDecl::markInvalid() {
  auto &Ctx = getASTContext();
  setType(ErrorType::get(Ctx));
  setInterfaceType(ErrorType::get(Ctx));
}

/// \brief Returns whether the var is settable in the specified context: this
/// is either because it is a stored var, because it has a custom setter, or
/// is a let member in an initializer.
bool VarDecl::isSettable(const DeclContext *UseDC,
                         const DeclRefExpr *base) const {
  // If this is a 'var' decl, then we're settable if we have storage or a
  // setter.
  if (!isImmutable())
    return supportsMutation();

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

    // 'let' properties are not valid inside protocols.
    if (CDC->getExtendedProtocolDecl())
      return false;

    // If this init is defined inside of the same type (or in an extension
    // thereof) as the let property, then it is mutable.
    if (!CDC->isTypeContext() ||
        CDC->getSelfNominalTypeDecl() !=
        getDeclContext()->getSelfNominalTypeDecl())
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

bool VarDecl::isLazilyInitializedGlobal() const {
  assert(!getDeclContext()->isLocalContext() &&
         "not a global variable!");
  assert(hasStorage() && "not a stored global variable!");

  // Imports from C are never lazily initialized.
  if (hasClangNode())
    return false;

  if (isDebuggerVar())
    return false;

  // Top-level global variables in the main source file and in the REPL are not
  // lazily initialized.
  auto sourceFileContext = dyn_cast<SourceFile>(getDeclContext());
  if (!sourceFileContext)
    return true;

  return !sourceFileContext->isScriptMode();
}

bool SubscriptDecl::isSettable() const {
  return supportsMutation();
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
  if (isa<ParamDecl>(this)) {
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(getDeclContext()))
      return AFD->getImplicitSelfDecl(/*createIfNeeded=*/false) == this;
    if (auto *PBI = dyn_cast<PatternBindingInitializer>(getDeclContext()))
      return PBI->getImplicitSelfDecl() == this;
  }

  return false;
}

void VarDecl::setSpecifier(Specifier specifier) {
  Bits.VarDecl.Specifier = static_cast<unsigned>(specifier);
  setSupportsMutationIfStillStored(
                          StorageIsMutable_t(!isImmutableSpecifier(specifier)));
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
      if (auto AD = dyn_cast<AccessorDecl>(FD)) {
        if (AD->isGetter() && !AD->getAccessorKeywordLoc().isValid())
          return;
      }
                   
      auto &d = getASTContext().Diags;
      d.diagnose(FD->getFuncLoc(), diag::change_to_mutating,
                 isa<AccessorDecl>(FD))
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

ParamDecl::ParamDecl(Specifier specifier,
                     SourceLoc specifierLoc, SourceLoc argumentNameLoc,
                     Identifier argumentName, SourceLoc parameterNameLoc,
                     Identifier parameterName, DeclContext *dc)
  : VarDecl(DeclKind::Param, /*IsStatic*/false, specifier,
            /*IsCaptureList*/false, parameterNameLoc, parameterName, dc),
  ArgumentName(argumentName), ArgumentNameLoc(argumentNameLoc),
  SpecifierLoc(specifierLoc) {

  assert(specifier != Specifier::Var &&
         "'var' cannot appear on parameters; you meant 'inout'");
  Bits.ParamDecl.IsTypeLocImplicit = false;
  Bits.ParamDecl.defaultArgumentKind =
    static_cast<unsigned>(DefaultArgumentKind::None);
}

/// Clone constructor, allocates a new ParamDecl identical to the first.
/// Intentionally not defined as a copy constructor to avoid accidental copies.
ParamDecl::ParamDecl(ParamDecl *PD, bool withTypes)
  : VarDecl(DeclKind::Param, /*IsStatic*/false, PD->getSpecifier(),
            /*IsCaptureList*/false, PD->getNameLoc(), PD->getName(),
            PD->getDeclContext()),
    ArgumentName(PD->getArgumentName()),
    ArgumentNameLoc(PD->getArgumentNameLoc()),
    SpecifierLoc(PD->getSpecifierLoc()),
    DefaultValueAndIsVariadic(nullptr, PD->DefaultValueAndIsVariadic.getInt()) {
  Bits.ParamDecl.IsTypeLocImplicit = PD->Bits.ParamDecl.IsTypeLocImplicit;
  Bits.ParamDecl.defaultArgumentKind = PD->Bits.ParamDecl.defaultArgumentKind;
  typeLoc = PD->getTypeLoc().clone(PD->getASTContext());
  if (!withTypes && typeLoc.getTypeRepr())
    typeLoc.setType(Type());

  if (withTypes && PD->hasInterfaceType())
    setInterfaceType(PD->getInterfaceType());

  // FIXME: We should clone the entire attribute list.
  if (PD->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>())
    getAttrs().add(new (PD->getASTContext())
                       ImplicitlyUnwrappedOptionalAttr(/* implicit= */ true));
}


/// \brief Retrieve the type of 'self' for the given context.
Type DeclContext::getSelfTypeInContext() const {
  assert(isTypeContext());

  // For a protocol or extension thereof, the type is 'Self'.
  if (getSelfProtocolDecl())
    return mapTypeIntoContext(getProtocolSelfType());
  return getDeclaredTypeInContext();
}

/// \brief Retrieve the interface type of 'self' for the given context.
Type DeclContext::getSelfInterfaceType() const {
  assert(isTypeContext());

  // For a protocol or extension thereof, the type is 'Self'.
  if (getSelfProtocolDecl())
    return getProtocolSelfType();
  return getDeclaredInterfaceType();
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
  if (auto *AT = dyn_cast<ArraySliceType>(T))
    return AT->getBaseType();
  if (auto *BGT = dyn_cast<BoundGenericType>(T)) {
    // It's the stdlib Array<T>.
    return BGT->getGenericArgs()[0];
  }
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

StringRef
ParamDecl::getDefaultValueStringRepresentation(
  SmallVectorImpl<char> &scratch) const {
  switch (getDefaultArgumentKind()) {
  case DefaultArgumentKind::None:
    llvm_unreachable("called on a ParamDecl with no default value");
  case DefaultArgumentKind::Normal: {
    assert(DefaultValueAndIsVariadic.getPointer() &&
           "default value not provided yet");
    auto existing =
      DefaultValueAndIsVariadic.getPointer()->StringRepresentation;
    if (!existing.empty())
      return existing;
    return extractInlinableText(getASTContext().SourceMgr, getDefaultValue(),
                                scratch);
  }
  case DefaultArgumentKind::Inherited:
    // FIXME: This needs /some/ kind of textual representation, but this isn't
    // a great one.
    return "super";
  case DefaultArgumentKind::File: return "#file";
  case DefaultArgumentKind::Line: return "#line";
  case DefaultArgumentKind::Column: return "#column";
  case DefaultArgumentKind::Function: return "#function";
  case DefaultArgumentKind::DSOHandle: return "#dsohandle";
  case DefaultArgumentKind::NilLiteral: return "nil";
  case DefaultArgumentKind::EmptyArray: return "[]";
  case DefaultArgumentKind::EmptyDictionary: return "[:]";
  }
  llvm_unreachable("unhandled kind");
}

void
ParamDecl::setDefaultValueStringRepresentation(StringRef stringRepresentation) {
  assert(getDefaultArgumentKind() == DefaultArgumentKind::Normal);
  assert(!stringRepresentation.empty());

  if (!DefaultValueAndIsVariadic.getPointer()) {
    DefaultValueAndIsVariadic.setPointer(
      getASTContext().Allocate<StoredDefaultArgument>());
  }

  DefaultValueAndIsVariadic.getPointer()->StringRepresentation =
      stringRepresentation;
}

void DefaultArgumentInitializer::changeFunction(
    DeclContext *parent, ParameterList *paramList) {
  if (parent->isLocalContext()) {
    setParent(parent);
  }

  auto param = paramList->get(getIndex());
  if (param->getDefaultValue())
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
  auto elementTy = getInterfaceType();
  if (elementTy->is<ErrorType>())
    return elementTy;
  return elementTy->castTo<AnyFunctionType>()->getResult();
}

void SubscriptDecl::computeType() {
  auto elementTy = getElementTypeLoc().getType();

  SmallVector<AnyFunctionType::Param, 2> argTy;
  getIndices()->getParams(argTy);

  Type funcTy;
  if (auto *sig = getGenericSignature())
    funcTy = GenericFunctionType::get(sig, argTy, elementTy);
  else
    funcTy = FunctionType::get(argTy, elementTy);

  // Record the interface type.
  setInterfaceType(funcTy);
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

SourceRange SubscriptDecl::getSourceRange() const {
  if (getBracesRange().isValid()) {
    return { getSubscriptLoc(), getBracesRange().End };
  } else if (ElementTy.getSourceRange().End.isValid()) {
    return { getSubscriptLoc(), ElementTy.getSourceRange().End };
  } else if (ArrowLoc.isValid()) {
    return { getSubscriptLoc(), ArrowLoc };
  } else {
    return getSubscriptLoc();
  }
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
  if (getFullName())
    return getFullName();

  if (auto accessor = dyn_cast<AccessorDecl>(this)) {
    auto &ctx = getASTContext();
    auto storage = accessor->getStorage();
    auto subscript = dyn_cast<SubscriptDecl>(storage);
    switch (auto accessorKind = accessor->getAccessorKind()) {
    // These don't have any extra implicit parameters.
    case AccessorKind::Address:
    case AccessorKind::MutableAddress:
    case AccessorKind::Get:
    case AccessorKind::Read:
    case AccessorKind::Modify:
      return subscript ? subscript->getFullName()
                       : DeclName(ctx, storage->getBaseName(),
                                  ArrayRef<Identifier>());

    case AccessorKind::Set:
    case AccessorKind::DidSet:
    case AccessorKind::WillSet: {
      SmallVector<Identifier, 4> argNames;
      // The implicit value/buffer parameter.
      argNames.push_back(Identifier());
      // The subscript index parameters.
      if (subscript) {
        argNames.append(subscript->getFullName().getArgumentNames().begin(),
                        subscript->getFullName().getArgumentNames().end());
      }
      return DeclName(ctx, storage->getBaseName(), argNames);
    }
    }
    llvm_unreachable("bad accessor kind");
  }

  return DeclName();
}

std::pair<DefaultArgumentKind, Type>
swift::getDefaultArgumentInfo(ValueDecl *source, unsigned Index) {
  const ParameterList *paramList;
  if (auto *AFD = dyn_cast<AbstractFunctionDecl>(source)) {
    paramList = AFD->getParameters();
  } else {
    paramList = cast<EnumElementDecl>(source)->getParameterList();
  }

  auto param = paramList->get(Index);
  return { param->getDefaultArgumentKind(), param->getInterfaceType() };
}

Type AbstractFunctionDecl::getMethodInterfaceType() const {
  assert(getDeclContext()->isTypeContext());
  auto Ty = getInterfaceType();
  if (Ty->hasError())
    return ErrorType::get(getASTContext());
  return Ty->castTo<AnyFunctionType>()->getResult();
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
  case BodyKind::Deserialized:
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

  auto paramList = getParameters();

  auto endLoc = paramList->getSourceRange().End;
  if (endLoc.isValid())
    return SourceRange(getNameLoc(), endLoc);

  return getNameLoc();
}

ObjCSelector
AbstractFunctionDecl::getObjCSelector(DeclName preferredName,
                                      bool skipIsObjCResolution) const {
  // FIXME: Forces computation of the Objective-C selector.
  if (getASTContext().getLazyResolver() && !skipIsObjCResolution)
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
    baseNameStr = func->getName().str();
  } else if (isa<ConstructorDecl>(this)) {
    baseNameStr = "init";
  } else {
    llvm_unreachable("Unknown subclass of AbstractFunctionDecl");
  }

  auto argNames = getFullName().getArgumentNames();

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

static bool requiresNewVTableEntry(const AbstractFunctionDecl *decl) {
  assert(isa<FuncDecl>(decl) || isa<ConstructorDecl>(decl));

  // Final members are always be called directly.
  // Dynamic methods are always accessed by objc_msgSend().
  if (decl->isFinal() || decl->isDynamic() || decl->hasClangNode())
    return false;
  
  // Initializers are not normally inherited, but required initializers can
  // be overridden for invocation from dynamic types, and convenience initializers
  // are conditionally inherited when all designated initializers are available,
  // working by dynamically invoking the designated initializer implementation
  // from the subclass. Convenience initializers can also override designated
  // initializer implementations from their superclass.
  if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
    if (!ctor->isRequired() && !ctor->isDesignatedInit()) {
      return false;
    }
  }

  if (auto *accessor = dyn_cast<AccessorDecl>(decl)) {
    // Check to see if it's one of the opaque accessors for the declaration.
    auto storage = accessor->getStorage();
    if (!storage->requiresOpaqueAccessor(accessor->getAccessorKind()))
      return false;
  }

  auto base = decl->getOverriddenDecl();
  if (!base || base->hasClangNode() || base->isDynamic())
    return true;
  
  // As above, convenience initializers are not formally overridable in Swift
  // vtables, although same-named initializers are modeled as overriding for
  // various QoI and objc interop reasons. Even if we "override" a non-required
  // convenience init, we still need a distinct vtable entry.
  if (auto baseCtor = dyn_cast<ConstructorDecl>(base)) {
    if (!baseCtor->isRequired() && !baseCtor->isDesignatedInit()) {
      return true;
    }
  }

  // If the method overrides something, we only need a new entry if the
  // override has a more general AST type. However an abstraction
  // change is OK; we don't want to add a whole new vtable entry just
  // because an @in parameter because @owned, or whatever.
  auto baseInterfaceTy = base->getInterfaceType();
  auto derivedInterfaceTy = decl->getInterfaceType();

  auto selfInterfaceTy = decl->getDeclContext()->getDeclaredInterfaceType();

  auto overrideInterfaceTy = selfInterfaceTy->adjustSuperclassMemberDeclType(
      base, decl, baseInterfaceTy);

  return !derivedInterfaceTy->matches(overrideInterfaceTy,
                                      TypeMatchFlags::AllowABICompatible);
}

void AbstractFunctionDecl::computeNeedsNewVTableEntry() {
  setNeedsNewVTableEntry(requiresNewVTableEntry(this));
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
  *selfDecl = new (ctx) ParamDecl(VarDecl::Specifier::Default,
                                  SourceLoc(), SourceLoc(), Identifier(),
                                  getLoc(), ctx.Id_self, this);
  (*selfDecl)->setImplicit();

  // If we already have an interface type, compute the 'self' parameter type.
  // Otherwise, we'll do it later.
  if (hasInterfaceType())
    computeSelfDeclType();

  return *selfDecl;
}

void AbstractFunctionDecl::computeSelfDeclType() {
  assert(hasImplicitSelfDecl());
  assert(hasInterfaceType());

  auto *selfDecl = getImplicitSelfDecl(/*createIfNeeded=*/false);

  // If we haven't created a 'self' parameter yet, do nothing, we'll compute
  // the type later.
  if (selfDecl == nullptr)
    return;

  auto selfParam = computeSelfParam(this,
                                    /*isInitializingCtor*/true,
                                    /*wantDynamicSelf*/true);
  selfDecl->setInterfaceType(selfParam.getPlainType());

  auto specifier = selfParam.getParameterFlags().isInOut()
                       ? VarDecl::Specifier::InOut
                       : VarDecl::Specifier::Default;
  selfDecl->setSpecifier(specifier);

  selfDecl->setValidationToChecked();
}

void AbstractFunctionDecl::setParameters(ParameterList *BodyParams) {
#ifndef NDEBUG
  auto Name = getFullName();
  if (!isa<DestructorDecl>(this))
    assert((!Name || !Name.isSimpleName()) && "Must have a compound name");
  assert(!Name || (Name.getArgumentNames().size() == BodyParams->size()));
#endif

  Params = BodyParams;
  BodyParams->setDeclContextOfParamDecls(this);
}

void AbstractFunctionDecl::computeType(AnyFunctionType::ExtInfo info) {
  auto &ctx = getASTContext();
  auto *sig = getGenericSignature();
  bool hasSelf = hasImplicitSelfDecl();

  // Result
  Type resultTy;
  if (auto fn = dyn_cast<FuncDecl>(this)) {
    resultTy = fn->getBodyResultTypeLoc().getType();
    if (!resultTy)
      resultTy = TupleType::getEmpty(ctx);

  } else if (auto ctor = dyn_cast<ConstructorDecl>(this)) {
    auto *dc = ctor->getDeclContext();

    if (hasSelf) {
      if (!dc->isTypeContext())
        resultTy = ErrorType::get(ctx);
      else
        resultTy = dc->getSelfInterfaceType();
    }

    // Adjust result type for failability.
    if (ctor->getFailability() != OTK_None)
      resultTy = OptionalType::get(resultTy);
  } else {
    assert(isa<DestructorDecl>(this));
    resultTy = TupleType::getEmpty(ctx);
  }

  // (Args...) -> Result
  Type funcTy;

  {
    SmallVector<AnyFunctionType::Param, 4> argTy;
    getParameters()->getParams(argTy);

    // 'throws' only applies to the innermost function.
    info = info.withThrows(hasThrows());
    // Defer bodies must not escape.
    if (auto fd = dyn_cast<FuncDecl>(this))
      info = info.withNoEscape(fd->isDeferBody());

    if (sig && !hasSelf) {
      funcTy = GenericFunctionType::get(sig, argTy, resultTy, info);
    } else {
      funcTy = FunctionType::get(argTy, resultTy, info);
    }
  }

  // (Self) -> (Args...) -> Result
  if (hasSelf) {
    // Substitute in our own 'self' parameter.
    auto selfParam = computeSelfParam(this);
    if (sig)
      funcTy = GenericFunctionType::get(sig, {selfParam}, funcTy);
    else
      funcTy = FunctionType::get({selfParam}, funcTy);
  }

  // Record the interface type.
  setInterfaceType(funcTy);

  // Compute the type of the 'self' parameter if we're created one already.
  if (hasSelf)
    computeSelfDeclType();
}

bool AbstractFunctionDecl::hasInlinableBodyText() const {
  switch (getBodyKind()) {
  case BodyKind::Deserialized:
    return true;
  case BodyKind::Parsed:
  case BodyKind::TypeChecked:
    return getBody() && !getBody()->isImplicit();
  case BodyKind::None:
  case BodyKind::Unparsed:
  case BodyKind::Synthesize:
  case BodyKind::Skipped:
  case BodyKind::MemberwiseInitializer:
    return false;
  }
}

StringRef AbstractFunctionDecl::getInlinableBodyText(
  SmallVectorImpl<char> &scratch) const {
  assert(hasInlinableBodyText() &&
         "can't get string representation of function with no text");

  if (getBodyKind() == BodyKind::Deserialized)
    return BodyStringRepresentation;

  auto body = getBody();
  return extractInlinableText(getASTContext().SourceMgr, body, scratch);
}

FuncDecl *FuncDecl::createImpl(ASTContext &Context,
                               SourceLoc StaticLoc,
                               StaticSpellingKind StaticSpelling,
                               SourceLoc FuncLoc,
                               DeclName Name, SourceLoc NameLoc,
                               bool Throws, SourceLoc ThrowsLoc,
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
               Name, NameLoc, Throws, ThrowsLoc,
               HasImplicitSelfDecl, GenericParams, Parent);
  if (ClangN)
    D->setClangNode(ClangN);
  if (HasImplicitSelfDecl)
    *D->getImplicitSelfDeclStorage() = nullptr;

  return D;
}

FuncDecl *FuncDecl::createDeserialized(ASTContext &Context,
                                       SourceLoc StaticLoc,
                                       StaticSpellingKind StaticSpelling,
                                       SourceLoc FuncLoc,
                                       DeclName Name, SourceLoc NameLoc,
                                       bool Throws, SourceLoc ThrowsLoc,
                                       GenericParamList *GenericParams,
                                       DeclContext *Parent) {
  return createImpl(Context, StaticLoc, StaticSpelling, FuncLoc,
                    Name, NameLoc, Throws, ThrowsLoc,
                    GenericParams, Parent,
                    ClangNode());
}

FuncDecl *FuncDecl::create(ASTContext &Context, SourceLoc StaticLoc,
                           StaticSpellingKind StaticSpelling,
                           SourceLoc FuncLoc,
                           DeclName Name, SourceLoc NameLoc,
                           bool Throws, SourceLoc ThrowsLoc,
                           GenericParamList *GenericParams,
                           ParameterList *BodyParams,
                           TypeLoc FnRetType, DeclContext *Parent,
                           ClangNode ClangN) {
  auto *FD = FuncDecl::createImpl(
      Context, StaticLoc, StaticSpelling, FuncLoc,
      Name, NameLoc, Throws, ThrowsLoc,
      GenericParams, Parent, ClangN);
  FD->setParameters(BodyParams);
  FD->getBodyResultTypeLoc() = FnRetType;
  return FD;
}

AccessorDecl *AccessorDecl::createImpl(ASTContext &ctx,
                                       SourceLoc declLoc,
                                       SourceLoc accessorKeywordLoc,
                                       AccessorKind accessorKind,
                                       AddressorKind addressorKind,
                                       AbstractStorageDecl *storage,
                                       SourceLoc staticLoc,
                                       StaticSpellingKind staticSpelling,
                                       bool throws, SourceLoc throwsLoc,
                                       GenericParamList *genericParams,
                                       DeclContext *parent,
                                       ClangNode clangNode) {
  bool hasImplicitSelfDecl = parent->isTypeContext();
  size_t size = sizeof(AccessorDecl) + (hasImplicitSelfDecl
                                        ? sizeof(ParamDecl *)
                                        : 0);
  void *buffer = allocateMemoryForDecl<AccessorDecl>(ctx, size,
                                                     !clangNode.isNull());
  auto D = ::new (buffer)
      AccessorDecl(declLoc, accessorKeywordLoc, accessorKind, addressorKind,
                   storage, staticLoc, staticSpelling, throws, throwsLoc,
                   hasImplicitSelfDecl, genericParams, parent);
  if (clangNode)
    D->setClangNode(clangNode);
  if (hasImplicitSelfDecl)
    *D->getImplicitSelfDeclStorage() = nullptr;

  return D;
}

AccessorDecl *AccessorDecl::createDeserialized(ASTContext &ctx,
                                               SourceLoc declLoc,
                                               SourceLoc accessorKeywordLoc,
                                               AccessorKind accessorKind,
                                               AddressorKind addressorKind,
                                               AbstractStorageDecl *storage,
                                               SourceLoc staticLoc,
                                              StaticSpellingKind staticSpelling,
                                               bool throws, SourceLoc throwsLoc,
                                               GenericParamList *genericParams,
                                               DeclContext *parent) {
  return createImpl(ctx, declLoc, accessorKeywordLoc, accessorKind,
                    addressorKind, storage, staticLoc, staticSpelling,
                    throws, throwsLoc, genericParams, parent,
                    ClangNode());
}

AccessorDecl *AccessorDecl::create(ASTContext &ctx,
                                   SourceLoc declLoc,
                                   SourceLoc accessorKeywordLoc,
                                   AccessorKind accessorKind,
                                   AddressorKind addressorKind,
                                   AbstractStorageDecl *storage,
                                   SourceLoc staticLoc,
                                   StaticSpellingKind staticSpelling,
                                   bool throws, SourceLoc throwsLoc,
                                   GenericParamList *genericParams,
                                   ParameterList * bodyParams,
                                   TypeLoc fnRetType,
                                   DeclContext *parent,
                                   ClangNode clangNode) {
  auto *D = AccessorDecl::createImpl(
      ctx, declLoc, accessorKeywordLoc, accessorKind, addressorKind, storage,
      staticLoc, staticSpelling, throws, throwsLoc,
      genericParams, parent, clangNode);
  D->setParameters(bodyParams);
  D->getBodyResultTypeLoc() = fnRetType;
  return D;
}

bool AccessorDecl::isAssumedNonMutating() const {
  switch (getAccessorKind()) {
  case AccessorKind::Get:
  case AccessorKind::Address:
  case AccessorKind::Read:
    return true;

  case AccessorKind::Set:
  case AccessorKind::WillSet:
  case AccessorKind::DidSet:
  case AccessorKind::MutableAddress:
  case AccessorKind::Modify:
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

StaticSpellingKind FuncDecl::getCorrectStaticSpelling() const {
  assert(getDeclContext()->isTypeContext());
  if (!isStatic())
    return StaticSpellingKind::None;
  if (getStaticSpelling() != StaticSpellingKind::None)
    return getStaticSpelling();

  return getCorrectStaticSpellingForDecl(this);
}

Type FuncDecl::getResultInterfaceType() const {
  if (!hasInterfaceType())
    return nullptr;

  Type resultTy = getInterfaceType();
  if (resultTy->is<ErrorType>())
    return resultTy;

  if (hasImplicitSelfDecl())
    resultTy = resultTy->castTo<AnyFunctionType>()->getResult();

  return resultTy->castTo<AnyFunctionType>()->getResult();
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

ConstructorDecl::ConstructorDecl(DeclName Name, SourceLoc ConstructorLoc,
                                 OptionalTypeKind Failability, 
                                 SourceLoc FailabilityLoc,
                                 bool Throws,
                                 SourceLoc ThrowsLoc,
                                 ParameterList *BodyParams,
                                 GenericParamList *GenericParams,
                                 DeclContext *Parent)
  : AbstractFunctionDecl(DeclKind::Constructor, Parent, Name, ConstructorLoc,
                         Throws, ThrowsLoc, /*HasImplicitSelfDecl=*/true,
                         GenericParams),
    FailabilityLoc(FailabilityLoc),
    SelfDecl(nullptr)
{
  if (BodyParams)
    setParameters(BodyParams);
  
  Bits.ConstructorDecl.ComputedBodyInitKind = 0;
  Bits.ConstructorDecl.HasStubImplementation = 0;
  Bits.ConstructorDecl.InitKind = static_cast<unsigned>(CtorInitializerKind::Designated);
  Bits.ConstructorDecl.Failability = static_cast<unsigned>(Failability);

  assert(Name.getBaseName() == DeclBaseName::createConstructor());
}

bool ConstructorDecl::isObjCZeroParameterWithLongSelector() const {
  // The initializer must have a single, non-empty argument name.
  if (getFullName().getArgumentNames().size() != 1 ||
      getFullName().getArgumentNames()[0].empty())
    return false;

  auto *params = getParameters();
  if (params->size() != 1)
    return false;

  return params->get(0)->getInterfaceType()->isVoid();
}

DestructorDecl::DestructorDecl(SourceLoc DestructorLoc, DeclContext *Parent)
  : AbstractFunctionDecl(DeclKind::Destructor, Parent,
                         DeclBaseName::createDestructor(), DestructorLoc,
                         /*Throws=*/false,
                         /*ThrowsLoc=*/SourceLoc(),
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

  if (isa<AccessorDecl>(this))
    return StartLoc;

  auto TrailingWhereClauseSourceRange = getGenericTrailingWhereClauseSourceRange();
  if (TrailingWhereClauseSourceRange.isValid())
    return { StartLoc, TrailingWhereClauseSourceRange.End };

  if (getBodyResultTypeLoc().hasLocation() &&
      getBodyResultTypeLoc().getSourceRange().End.isValid())
    return { StartLoc, getBodyResultTypeLoc().getSourceRange().End };

  if (hasThrows())
    return { StartLoc, getThrowsLoc() };

  auto LastParamListEndLoc = getParameters()->getSourceRange().End;
  if (LastParamListEndLoc.isValid())
    return { StartLoc, LastParamListEndLoc };
  return StartLoc;
}

SourceRange EnumElementDecl::getSourceRange() const {
  if (RawValueExpr && !RawValueExpr->isImplicit())
    return {getStartLoc(), RawValueExpr->getEndLoc()};
  if (auto *PL = getParameterList())
    return {getStartLoc(), PL->getSourceRange().End};
  return {getStartLoc(), getNameLoc()};
}

void EnumElementDecl::computeType() {
  assert(!hasInterfaceType());

  auto &ctx = getASTContext();
  auto *ED = getParentEnum();

  // The type of the enum element is either (Self.Type) -> Self
  // or (Self.Type) -> (Args...) -> Self.
  auto resultTy = ED->getDeclaredInterfaceType();

  AnyFunctionType::Param selfTy(MetatypeType::get(resultTy, ctx));

  if (auto *PL = getParameterList()) {
    SmallVector<AnyFunctionType::Param, 4> argTy;
    PL->getParams(argTy);

    resultTy = FunctionType::get(argTy, resultTy);
  }

  if (auto *genericSig = ED->getGenericSignature())
    resultTy = GenericFunctionType::get(genericSig, {selfTy}, resultTy);
  else
    resultTy = FunctionType::get({selfTy}, resultTy);

  // Record the interface type.
  setInterfaceType(resultTy);
}

Type EnumElementDecl::getArgumentInterfaceType() const {
  if (!hasAssociatedValues())
    return nullptr;

  auto interfaceType = getInterfaceType();
  if (interfaceType->is<ErrorType>()) {
    return interfaceType;
  }

  auto funcTy = interfaceType->castTo<AnyFunctionType>();
  funcTy = funcTy->getResult()->castTo<FunctionType>();
  return AnyFunctionType::composeInput(funcTy->getASTContext(),
                                       funcTy->getParams(),
                                       /*canonicalVararg=*/false);
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
    End = getGenericTrailingWhereClauseSourceRange().End;
  if (End.isInvalid())
    End = getThrowsLoc();
  if (End.isInvalid())
    End = getSignatureSourceRange().End;

  return { getConstructorLoc(), End };
}

Type ConstructorDecl::getResultInterfaceType() const {
  Type ArgTy = getInterfaceType();
  ArgTy = ArgTy->castTo<AnyFunctionType>()->getResult();
  ArgTy = ArgTy->castTo<AnyFunctionType>()->getResult();
  return ArgTy;
}

Type ConstructorDecl::getInitializerInterfaceType() {
  if (InitializerInterfaceType)
    return InitializerInterfaceType;

  // Lazily calculate initializer type.
  auto funcTy = getInterfaceType()->castTo<AnyFunctionType>()->getResult();
  assert(funcTy->is<FunctionType>());

  // Constructors have an initializer type that takes an instance
  // instead of a metatype.
  auto initSelfParam = computeSelfParam(this, /*isInitializingCtor=*/true);
  Type initFuncTy;
  if (auto *sig = getGenericSignature())
    initFuncTy = GenericFunctionType::get(sig, {initSelfParam}, funcTy);
  else
    initFuncTy = FunctionType::get({initSelfParam}, funcTy);
  InitializerInterfaceType = initFuncTy;

  return InitializerInterfaceType;
}

ConstructorDecl::BodyInitKind
ConstructorDecl::getDelegatingOrChainedInitKind(DiagnosticEngine *diags,
                                                ApplyExpr **init) const {
  assert(hasBody() && "Constructor does not have a definition");

  if (init)
    *init = nullptr;

  // If we already computed the result, return it.
  if (Bits.ConstructorDecl.ComputedBodyInitKind) {
    return static_cast<BodyInitKind>(
             Bits.ConstructorDecl.ComputedBodyInitKind - 1);
  }


  struct FindReferenceToInitializer : ASTWalker {
    const ConstructorDecl *Decl;
    BodyInitKind Kind = BodyInitKind::None;
    ApplyExpr *InitExpr = nullptr;
    DiagnosticEngine *Diags;

    FindReferenceToInitializer(const ConstructorDecl *decl,
                               DiagnosticEngine *diags)
        : Decl(decl), Diags(diags) { }

    bool walkToDeclPre(class Decl *D) override {
      // Don't walk into further nominal decls.
      return !isa<NominalTypeDecl>(D);
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
        if (dotExpr->getName().getBaseName() != DeclBaseName::createConstructor())
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
      else if (arg->isSelfExprOf(Decl, /*sameBase*/true))
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

  auto *NTD = getDeclContext()->getSelfNominalTypeDecl();

  // Protocol extension and enum initializers are always delegating.
  if (Kind == BodyInitKind::None) {
    if (isa<ProtocolDecl>(NTD) || isa<EnumDecl>(NTD)) {
      Kind = BodyInitKind::Delegating;
    }
  }

  // Struct initializers that cannot see the layout of the struct type are
  // always delegating. This occurs if the struct type is not fixed layout,
  // and the constructor is either inlinable or defined in another module.
  if (Kind == BodyInitKind::None && isa<StructDecl>(NTD)) {
    // Note: This is specifically not using isFormallyResilient. We relax this
    // rule for structs in non-resilient modules so that they can have inlinable
    // constructors, as long as those constructors don't reference private
    // declarations.
    if (NTD->isResilient() &&
        getResilienceExpansion() == ResilienceExpansion::Minimal) {
      Kind = BodyInitKind::Delegating;

    } else if (isa<ExtensionDecl>(getDeclContext())) {
      const ModuleDecl *containingModule = getParentModule();
      // Prior to Swift 5, cross-module initializers were permitted to be
      // non-delegating. However, if the struct isn't fixed-layout, we have to
      // be delegating because, well, we don't know the layout.
      if (NTD->isResilient() ||
          containingModule->getASTContext().isSwiftVersionAtLeast(5)) {
        if (containingModule != NTD->getParentModule())
          Kind = BodyInitKind::Delegating;
      }
    }
  }

  // If we didn't find any delegating or chained initializers, check whether
  // the initializer was explicitly marked 'convenience'.
  if (Kind == BodyInitKind::None && getAttrs().hasAttribute<ConvenienceAttr>())
    Kind = BodyInitKind::Delegating;

  // If we still don't know, check whether we have a class with a superclass: it
  // gets an implicit chained initializer.
  if (Kind == BodyInitKind::None) {
    if (auto classDecl = getDeclContext()->getSelfClassDecl()) {
      if (classDecl->hasSuperclass())
        Kind = BodyInitKind::ImplicitChained;
    }
  }

  // Cache the result if it is trustworthy.
  if (diags) {
    auto *mutableThis = const_cast<ConstructorDecl *>(this);
    mutableThis->Bits.ConstructorDecl.ComputedBodyInitKind =
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

bool FuncDecl::isDeferBody() const {
  return getName() == getASTContext().getIdentifier("$defer");
}

bool FuncDecl::isPotentialIBActionTarget() const {
  return isInstanceMember() &&
    getDeclContext()->getSelfClassDecl() &&
    !isa<AccessorDecl>(this);
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

Type ClassDecl::getSuperclass() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    SuperclassTypeRequest{const_cast<ClassDecl *>(this),
                          TypeResolutionStage::Interface},
    Type());
}

ClassDecl *ClassDecl::getSuperclassDecl() const {
  ASTContext &ctx = getASTContext();
  return evaluateOrDefault(ctx.evaluator,
    SuperclassDeclRequest{const_cast<ClassDecl *>(this)}, nullptr);
}

void ClassDecl::setSuperclass(Type superclass) {
  assert((!superclass || !superclass->hasArchetype())
         && "superclass must be interface type");
  LazySemanticInfo.Superclass.setPointerAndInt(superclass, true);
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

bool Decl::getEnableExperimentalDependencies() const {
  return getASTContext().LangOpts.EnableExperimentalDependencies;
}

// See swift/Basic/Statistic.h for declaration: this enables tracing Decls, is
// defined here to avoid too much layering violation / circular linkage
// dependency.

struct DeclTraceFormatter : public UnifiedStatsReporter::TraceFormatter {
  void traceName(const void *Entity, raw_ostream &OS) const {
    if (!Entity)
      return;
    const Decl *D = static_cast<const Decl *>(Entity);
    if (auto const *VD = dyn_cast<const ValueDecl>(D)) {
      VD->getFullName().print(OS, false);
    } else {
      OS << "<"
         << Decl::getDescriptiveKindName(D->getDescriptiveKind())
         << ">";
    }
  }
  void traceLoc(const void *Entity, SourceManager *SM,
                clang::SourceManager *CSM, raw_ostream &OS) const {
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
    simple_display(out, value);
  } else if (auto ext = dyn_cast<ExtensionDecl>(decl)) {
    out << "extension of ";
    if (auto typeRepr = ext->getExtendedTypeLoc().getTypeRepr())
      typeRepr->print(out);
    else
      ext->getSelfNominalTypeDecl()->dumpRef(out);
  } else {
    out << "(unknown decl)";
  }
}

void swift::simple_display(llvm::raw_ostream &out, const ValueDecl *decl) {
  if (decl) decl->dumpRef(out);
  else out << "(null)";
}

// ExperimentalDependencies

  
ExperimentalDependencies::unimpLocation_t Decl::updateExpDepDeclHash(llvm::MD5& hash) const {
  ExperimentalDependencies::updateExpDepFromBits(hash, Bits);
  TRY_UPDATE_HASH(getAttrs().updateExpDepHash(hash))
  DeclContext *dc = getDeclContext();
  TRY_UPDATE_HASH(dc->updateExpDepDeclCtxHash(hash));

  switch (getKind()) {
    case DeclKind::Enum:
      return cast<const EnumDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::Struct:
      return cast<const StructDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::Class:
      return cast<const ClassDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::Protocol:
      return cast<const ProtocolDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::TypeAlias:
      return cast<const TypeAliasDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::GenericTypeParam:
      return cast<const GenericTypeParamDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::AssociatedType:
      return cast<const AssociatedTypeDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::Module:
      return cast<const ModuleDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::Var:
      return cast<const VarDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::Param:
      return cast<const ParamDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::Subscript:
      return cast<const SubscriptDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::Constructor:
      return cast<const ConstructorDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::Destructor:
      return cast<const DestructorDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::Func:
      return cast<const FuncDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::Accessor:
      return cast<const AccessorDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::EnumElement:
      return cast<const EnumElementDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::Extension:
      return cast<const ExtensionDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::TopLevelCode:
      return cast<const TopLevelCodeDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::Import:
      return cast<const ImportDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::IfConfig:
      return cast<const IfConfigDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::PoundDiagnostic:
      return cast<const PoundDiagnosticDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::PrecedenceGroup:
      return cast<const PrecedenceGroupDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::MissingMember:
      return cast<const MissingMemberDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::PatternBinding:
      return cast<const PatternBindingDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::EnumCase:
      return cast<const EnumCaseDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::InfixOperator:
      return cast<const InfixOperatorDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::PrefixOperator:
      return cast<const PrefixOperatorDecl>(this)->updateExpDepDeclHashInner(hash);
    case DeclKind::PostfixOperator:
      return cast<const PostfixOperatorDecl>(this)->updateExpDepDeclHashInner(hash);
  }

}



ExperimentalDependencies::unimpLocation_t  OperatorDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  return name.updateExpDepHash(hash);
}

ExperimentalDependencies::unimpLocation_t  NominalTypeDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
//  Type DeclaredTy;
//  Type DeclaredTyInContext;
//  Type DeclaredInterfaceTy;
//  extensions
//  GenericTypeDecl::updateExpDepDeclHash(hash);
  return GenericTypeDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  PrecedenceGroupDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;

}

ExperimentalDependencies::unimpLocation_t  ValueDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
}


ExperimentalDependencies::unimpLocation_t  TrailingWhereClause::updateExpDepHash(llvm::MD5 &hash) const {
  RETURN_UNIMP;
}

ExperimentalDependencies::unimpLocation_t  TypeAliasDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
   RETURN_UNIMP;
  return GenericTypeDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t EnumDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  assert(hasRawType());
  TRY_UPDATE_HASH(getRawType().updateExpDepTypeHash(hash));
  return NominalTypeDecl::updateExpDepDeclHashInner(hash);
  
}

ExperimentalDependencies::unimpLocation_t StructDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  return NominalTypeDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  ClassDecl::updateExpDepDeclHashInner(llvm::MD5 &hash) const {
//  if (hasSuperclass()) {
//    TRY_UPDATE_HASH(getSuperclass().updateExpDepDeclHash(hash)); // NEEDED?
//  }
  if (getSuperclassDecl()) {
    TRY_UPDATE_HASH(getSuperclassDecl()->updateExpDepDeclHash(hash));
  }
  const ForeignKind fk = getForeignClassKind();
  ExperimentalDependencies::updateExpDepFromBits(hash, fk);
  
  const bool flags[] = {
    requiresStoredPropertyInits(),
    hasMissingDesignatedInitializers(),
    hasMissingVTableEntries(),
    hasDestructor(),
    usesObjCGenericsModel(),
    hasKnownSwiftImplementation()
  };
  ExperimentalDependencies::updateExpDepFromBits(hash, flags);
  
  if (hasDestructor()) {
    RETURN_UNIMP; // non-const getDestructor
    //    TRY_UPDATE_HASH(getDestructor()->updateExpDepDeclHash(hash));
  }
  const MetaclassKind mk = getMetaclassKind();
  ExperimentalDependencies::updateExpDepFromBits(hash, mk);
  llvm::SmallString<64> scratch;
  hash.update(getObjCRuntimeName(scratch));
  // getArtificialMainKind
  
  return NominalTypeDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  ProtocolDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  return NominalTypeDecl::updateExpDepDeclHashInner(hash);
}


ExperimentalDependencies::unimpLocation_t  GenericTypeParamDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  return AbstractTypeParamDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  AssociatedTypeDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  return AbstractTypeParamDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  ModuleDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  return TypeDecl::updateExpDepDeclHashInner(hash);}

ExperimentalDependencies::unimpLocation_t  VarDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  return AbstractStorageDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  ParamDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  return VarDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  SubscriptDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  // GenericContext
  return AbstractStorageDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  ConstructorDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  return AbstractFunctionDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  DestructorDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  return AbstractFunctionDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  FuncDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  return AbstractFunctionDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  AccessorDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  return FuncDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  EnumElementDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  return ValueDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  ExtensionDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  //  GenericContext
  //  IterableDeclContext
  RETURN_UNIMP;
}

ExperimentalDependencies::unimpLocation_t  TopLevelCodeDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  // DeclContext
  RETURN_UNIMP;
}

ExperimentalDependencies::unimpLocation_t  ImportDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
}

ExperimentalDependencies::unimpLocation_t  IfConfigDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
}

ExperimentalDependencies::unimpLocation_t  PoundDiagnosticDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
}

ExperimentalDependencies::unimpLocation_t  MissingMemberDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
}

ExperimentalDependencies::unimpLocation_t  PatternBindingDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
}

ExperimentalDependencies::unimpLocation_t  InfixOperatorDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  return OperatorDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  PrefixOperatorDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  return OperatorDecl::updateExpDepDeclHashInner(hash);
}

ExperimentalDependencies::unimpLocation_t  PostfixOperatorDecl::updateExpDepDeclHashInner(llvm::MD5& hash) const {
  RETURN_UNIMP;
  return OperatorDecl::updateExpDepDeclHashInner(hash);
}

