//===--- Decl.cpp - Swift Language Decl ASTs ------------------------------===//
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
//  This file implements the Decl class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Expr.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/TypeLoc.h"
#include "clang/Lex/MacroInfo.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/raw_ostream.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Basic/Fallthrough.h"

#include "clang/Basic/CharInfo.h"
#include "clang/AST/DeclObjC.h"

using namespace swift;

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

// Only allow allocation of Decls using the allocator in ASTContext.
void *Decl::operator new(size_t Bytes, ASTContext &C,
                         unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

// Only allow allocation of Modules using the allocator in ASTContext.
void *Module::operator new(size_t Bytes, ASTContext &C,
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

     if (!func->getName().empty() && func->getName().isOperator())
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
  ENTRY(IfConfig, "if configuration");
  ENTRY(PatternBinding, "pattern binding");
  ENTRY(Var, "var");
  ENTRY(Param, "parameter");
  ENTRY(Let, "let");
  ENTRY(StaticVar, "static var");
  ENTRY(StaticLet, "static let");
  ENTRY(ClassVar, "class var");
  ENTRY(ClassLet, "class let");
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

DeclContext *Decl::getInnermostDeclContext() {
  if (auto func = dyn_cast<AbstractFunctionDecl>(this))
    return func;
  if (auto nominal = dyn_cast<NominalTypeDecl>(this))
    return nominal;
  if (auto ext = dyn_cast<ExtensionDecl>(this))
    return ext;
  if (auto topLevel = dyn_cast<TopLevelCodeDecl>(this))
    return topLevel;

  return getDeclContext();

}

void Decl::setDeclContext(DeclContext *DC) { 
  Context = DC;
}

Module *Decl::getModuleContext() const {
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

bool Decl::isTransparent() const {
  // Check if the declaration had the attribute.
  if (getAttrs().hasAttribute<TransparentAttr>())
    return true;

  // Check if this is a function declaration which is within a transparent
  // extension.
  if (const AbstractFunctionDecl *FD = dyn_cast<AbstractFunctionDecl>(this)) {
    if (const ExtensionDecl *ED = dyn_cast<ExtensionDecl>(FD->getParent()))
      return ED->isTransparent();
  }

  // If this is an accessor, check if the transparent attribute was set
  // on the value decl.
  if (const FuncDecl *FD = dyn_cast<FuncDecl>(this)) {
    if (auto *ASD = FD->getAccessorStorageDecl())
      return ASD->isTransparent();
  }

  return false;
}

bool Decl::isPrivateStdlibDecl() const {
  const Decl *D = this;
  if (auto ExtD = dyn_cast<ExtensionDecl>(D))
    return ExtD->getExtendedType().isPrivateStdlibType();

  DeclContext *DC = D->getDeclContext()->getModuleScopeContext();
  if (DC->getParentModule()->isBuiltinModule())
    return true;
  if (!DC->getParentModule()->isSystemModule())
    return false;
  auto FU = dyn_cast<FileUnit>(DC);
  if (!FU)
    return false;
  // Check for Swift module and overlays.
  if (FU->getKind() != FileUnitKind::SerializedAST)
    return false;

  if (auto AFD = dyn_cast<AbstractFunctionDecl>(D)) {
    // Hide '~>' functions (but show the operator, because it defines
    // precedence).
    if (AFD->getNameStr() == "~>")
      return true;
  }

  // Whitelist protocols and protocol requirements.
  if (auto PD = dyn_cast<ProtocolDecl>(D)) {
    return PD->getNameStr().startswith("_Builtin");
  }
  if (isa<ProtocolDecl>(D->getDeclContext()))
    return false;


  auto VD = dyn_cast<ValueDecl>(D);
  if (!VD || !VD->hasName())
    return false;

  // If the name has leading underscore then it's a private symbol.
  if (VD->getNameStr().startswith("_"))
    return true;

  // If it's a constructor with a parameter with leading underscore, it's a
  // private function.
  if (auto CD = dyn_cast<ConstructorDecl>(VD)) {
    bool hasInternalParameter = false;
    for (auto Pat : CD->getBodyParamPatterns()) {
      Pat->forEachVariable([&](VarDecl *Param) {
        if (Param->hasName() && Param->getNameStr().startswith("_")) {
          hasInternalParameter = true;
        }
      });
      if (hasInternalParameter)
        return true;
    }
  }

  return false;
}

bool Decl::isWeakImported(Module *fromModule) const {
  // For a Clang declaration, trust Clang.
  if (auto clangDecl = getClangDecl()) {
    return clangDecl->isWeakImported();
  }

  // FIXME: Implement using AvailabilityAttr::getMinVersionAvailability().
  return false;
}

GenericParamList::GenericParamList(SourceLoc LAngleLoc,
                                   ArrayRef<GenericTypeParamDecl *> Params,
                                   SourceLoc WhereLoc,
                                   MutableArrayRef<RequirementRepr> Requirements,
                                   SourceLoc RAngleLoc)
  : Brackets(LAngleLoc, RAngleLoc), NumParams(Params.size()),
    WhereLoc(WhereLoc), Requirements(Requirements),
    OuterParameters(nullptr), Builder(nullptr)
{
  std::uninitialized_copy(Params.begin(), Params.end(),
                          reinterpret_cast<GenericTypeParamDecl **>(this + 1));
}

GenericParamList *
GenericParamList::create(ASTContext &Context,
                         SourceLoc LAngleLoc,
                         ArrayRef<GenericTypeParamDecl *> Params,
                         SourceLoc RAngleLoc) {
  unsigned Size = sizeof(GenericParamList)
                + sizeof(GenericTypeParamDecl *) * Params.size();
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
                         MutableArrayRef<RequirementRepr> Requirements,
                         SourceLoc RAngleLoc) {
  unsigned Size = sizeof(GenericParamList)
                + sizeof(GenericTypeParamDecl *) * Params.size();
  void *Mem = Context.Allocate(Size, alignof(GenericParamList));
  return new (Mem) GenericParamList(LAngleLoc, Params,
                                    WhereLoc,
                                    Context.AllocateCopy(Requirements),
                                    RAngleLoc);
}

GenericSignature *
GenericParamList::getAsCanonicalGenericSignature(
                            llvm::DenseMap<ArchetypeType *, Type> &archetypeMap,
                            ASTContext &C) const {
  SmallVector<GenericTypeParamType *, 4> params;
  SmallVector<Requirement, 4> requirements;
  
  getAsGenericSignatureElements(C, archetypeMap, params, requirements);

  // Canonicalize the types in the signature.
  for (auto &param : params)
    param = cast<GenericTypeParamType>(param->getCanonicalType());
  
  for (auto &reqt : requirements)
    reqt = Requirement(reqt.getKind(),
                       reqt.getFirstType()->getCanonicalType(),
                       reqt.getSecondType()->getCanonicalType());
  
  return GenericSignature::get(params, requirements);
}

// Helper for getAsGenericSignatureElements to remap an archetype in a
// requirement to a canonical dependent type.
Type
ArchetypeType::getAsDependentType(
                   const llvm::DenseMap<ArchetypeType*, Type> &archetypeMap) {
  // Map associated archetypes to DependentMemberTypes.
  if (auto parent = getParent()) {
    auto assocTy = getAssocType();
    assert(assocTy);
    Type base = parent->getAsDependentType(archetypeMap);
    return DependentMemberType::get(base, assocTy, getASTContext());
  }
  // Map primary archetypes to generic type parameters.
  auto found = archetypeMap.find(this);
  assert(found != archetypeMap.end()
         && "did not find generic param for archetype");
  return found->second;
}

static Type getAsDependentType(Type t,
                     const llvm::DenseMap<ArchetypeType*, Type> &archetypeMap) {
  if (!t->hasArchetype())
    return t;

  return t.transform([&](Type type) -> Type {
    if (auto arch = type->getAs<ArchetypeType>())
      return arch->getAsDependentType(archetypeMap);
    return type;
  });
}

// A helper to recursively collect the generic parameters from the outer levels
// of a generic parameter list.
void
GenericParamList::getAsGenericSignatureElements(ASTContext &C,
                        llvm::DenseMap<ArchetypeType *, Type> &archetypeMap,
                        SmallVectorImpl<GenericTypeParamType *> &genericParams,
                        SmallVectorImpl<Requirement> &requirements) const {
  // Collect outer generic parameters first.
  if (OuterParameters) {
    OuterParameters->getAsGenericSignatureElements(C, archetypeMap,
                                                   genericParams,
                                                   requirements);
  }

  // Collect our parameters.
  for (auto paramIndex : indices(getParams())) {
    auto param = getParams()[paramIndex];
    
    auto typeParamTy = param->getDeclaredType()->castTo<GenericTypeParamType>();

    // Make sure we didn't visit this param already in the parent.
    auto found = archetypeMap.find(param->getArchetype());
    if (found != archetypeMap.end()) {
      assert(found->second->isEqual(typeParamTy));
      continue;
    }
    
    // Set up a mapping we can use to remap requirements to dependent types.
    ArchetypeType *archetype = getPrimaryArchetypes()[paramIndex];
    archetypeMap[archetype] = typeParamTy;

    genericParams.push_back(typeParamTy);
    requirements.push_back(Requirement(RequirementKind::WitnessMarker,
                                       typeParamTy, typeParamTy));
    
    // Collect conformance requirements declared on the archetype.
    if (auto super = archetype->getSuperclass()) {
      requirements.push_back(Requirement(RequirementKind::Conformance,
                                         typeParamTy, super));
    }
    for (auto proto : archetype->getConformsTo()) {
      requirements.push_back(Requirement(RequirementKind::Conformance,
                                       typeParamTy, proto->getDeclaredType()));
    }
  }
  
  // FIXME: Emit WitnessMarker requirements for associated types in an order
  // that preserves AllArchetypes order but otherwise makes no sense.
  for (auto assocTy : getAssociatedArchetypes()) {
    auto depTy = getAsDependentType(assocTy, archetypeMap);
    requirements.push_back(Requirement(RequirementKind::WitnessMarker,
                                       depTy, depTy));

    // Add conformance requirements for this associated archetype.
    for (const auto &repr : getRequirements()) {
      // Handle same-type requirements at last.
      if (repr.getKind() != RequirementKind::Conformance)
        continue;

      // Primary conformance declarations would have already been gathered as
      // conformance requirements of the archetype.
      if (auto arch = repr.getSubject()->getAs<ArchetypeType>())
        if (!arch->getParent())
          continue;

      auto depTyOfReqt = getAsDependentType(repr.getSubject(), archetypeMap);
      if (depTyOfReqt.getPointer() != depTy.getPointer())
        continue;

      Requirement reqt(RequirementKind::Conformance,
                       getAsDependentType(repr.getSubject(), archetypeMap),
                       getAsDependentType(repr.getConstraint(), archetypeMap));
      requirements.push_back(reqt);
    }
  }
  
  // Add all of the same-type requirements.
  if (Builder) {
    for (auto req : Builder->getSameTypeRequirements()) {
      auto firstType = req.first->getDependentType(*Builder, false);
      Type secondType;
      if (auto concrete = req.second.dyn_cast<Type>())
        secondType = getAsDependentType(concrete, archetypeMap);
      else if (auto secondPA =
               req.second.dyn_cast<ArchetypeBuilder::PotentialArchetype*>())
        secondType = secondPA->getDependentType(*Builder, false);

      if (firstType->is<ErrorType>() || secondType->is<ErrorType>())
        continue;

      requirements.push_back(Requirement(RequirementKind::SameType,
                                         firstType, secondType));
    }
  }
}

/// \brief Add the nested archetypes of the given archetype to the set
/// of all archetypes.
void GenericParamList::addNestedArchetypes(ArchetypeType *archetype,
                                      SmallPtrSetImpl<ArchetypeType*> &known,
                                      SmallVectorImpl<ArchetypeType*> &all) {
  for (auto nested : archetype->getNestedTypes()) {
    auto nestedArch = nested.second.getAsArchetype();
    if (!nestedArch)
      continue;
    if (known.insert(nestedArch).second) {
      assert(!nestedArch->isPrimary() && "Unexpected primary archetype");
      all.push_back(nestedArch);
      addNestedArchetypes(nestedArch, known, all);
    }
  }
}

ArrayRef<ArchetypeType*>
GenericParamList::deriveAllArchetypes(ArrayRef<GenericTypeParamDecl *> params,
                                      SmallVectorImpl<ArchetypeType*> &all) {
  // This should be kept in sync with ArchetypeBuilder::getAllArchetypes().

  assert(all.empty());
  llvm::SmallPtrSet<ArchetypeType*, 8> known;

  // Collect all the primary archetypes.
  for (auto param : params) {
    auto archetype = param->getArchetype();
    if (archetype->isPrimary() && known.insert(archetype).second)
      all.push_back(archetype);
  }

  // Collect all the nested archetypes.
  for (auto param : params) {
    auto archetype = param->getArchetype();
    addNestedArchetypes(archetype, known, all);
  }

  return all;
}

ImportDecl *ImportDecl::create(ASTContext &Ctx, DeclContext *DC,
                               SourceLoc ImportLoc, ImportKind Kind,
                               SourceLoc KindLoc,
                               ArrayRef<AccessPathElement> Path,
                               const clang::Module *ClangMod) {
  assert(!Path.empty());
  assert(Kind == ImportKind::Module || Path.size() > 1);
  size_t Size = sizeof(ImportDecl) + Path.size() * sizeof(AccessPathElement);
  void *ptr = allocateMemoryForDecl<ImportDecl>(Ctx, Size, ClangMod != nullptr);
  auto D = new (ptr) ImportDecl(DC, ImportLoc, Kind, KindLoc, Path);
  if (ClangMod)
    D->setClangNode(ClangMod);
  return D;
}

ImportDecl::ImportDecl(DeclContext *DC, SourceLoc ImportLoc, ImportKind K,
                       SourceLoc KindLoc, ArrayRef<AccessPathElement> Path)
  : Decl(DeclKind::Import, DC), ImportLoc(ImportLoc), KindLoc(KindLoc),
    NumPathElements(Path.size()) {
  ImportDeclBits.ImportKind = static_cast<unsigned>(K);
  assert(getImportKind() == K && "not enough bits for ImportKind");
  std::uninitialized_copy(Path.begin(), Path.end(), getPathBuffer());
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
    Type underlyingTy = cast<TypeAliasDecl>(VD)->getUnderlyingType();
    return getBestImportKind(underlyingTy->getAnyNominal());
  }

  case DeclKind::Func:
    return ImportKind::Func;

  case DeclKind::Var:
    return ImportKind::Var;
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

template <typename T>
static void
loadAllConformances(const T *container,
                    const LazyLoaderArray<ProtocolConformance*> &loaderInfo) {
  if (!loaderInfo.isLazy())
    return;

  // Don't try to load conformances re-entrant-ly.
  auto resolver = loaderInfo.getLoader();
  auto contextData = loaderInfo.getLoaderContextData();
  const_cast<LazyLoaderArray<ProtocolConformance*> &>(loaderInfo) = {};

  SmallVector<ProtocolConformance *, 8> Conformances;
  resolver->loadAllConformances(container, contextData, Conformances);
  const_cast<T *>(container)->setConformances(
                        container->getASTContext().AllocateCopy(Conformances));
}

DeclRange NominalTypeDecl::getMembers(bool forceDelayedMembers) const {
  loadAllMembers();
  if (forceDelayedMembers)
    const_cast<NominalTypeDecl*>(this)->forceDelayedMemberDecls();
  return IterableDeclContext::getMembers();
}

void NominalTypeDecl::setMemberLoader(LazyMemberLoader *resolver,
                                      uint64_t contextData) {
  IterableDeclContext::setLoader(resolver, contextData);
}

ArrayRef<ProtocolConformance*> NominalTypeDecl::getConformances() const {
  loadAllConformances(this, Conformances);
  return Conformances.getArray();
}

void NominalTypeDecl::setConformanceLoader(LazyMemberLoader *resolver,
                                           uint64_t contextData) {
  assert(!Conformances.isLazy() && "already have a resolver");
  assert(Conformances.getArray().empty() && "already have conformances");
  Conformances.setLoader(resolver, contextData);
}

ExtensionDecl::ExtensionDecl(SourceLoc extensionLoc,
                             ArrayRef<RefComponent> refComponents,
                             MutableArrayRef<TypeLoc> inherited,
                             DeclContext *parent)
  : Decl(DeclKind::Extension, parent),
    DeclContext(DeclContextKind::ExtensionDecl, parent),
    IterableDeclContext(IterableDeclContextKind::ExtensionDecl),
    ExtensionLoc(extensionLoc),
    Inherited(inherited)
{
  ExtensionDeclBits.Validated = false;
  ExtensionDeclBits.CheckedInheritanceClause = false;
  ExtensionDeclBits.DefaultAccessLevel = 0;
  ExtensionDeclBits.NumRefComponents = refComponents.size();

  std::copy(refComponents.begin(), refComponents.end(),
            getRefComponents().data());
}

ExtensionDecl *ExtensionDecl::create(ASTContext &ctx, SourceLoc extensionLoc,
                                     ArrayRef<RefComponent> refComponents,
                                     MutableArrayRef<TypeLoc> inherited,
                                     DeclContext *parent,
                                     ClangNode clangNode) {
  // Determine how much storage we require for this declaration.
  unsigned size = sizeof(ExtensionDecl)
                + refComponents.size() * sizeof(RefComponent);

  void *declPtr = allocateMemoryForDecl<ExtensionDecl>(ctx, size,
                                                       !clangNode.isNull());

  // Construct the extension.
  auto result = ::new (declPtr) ExtensionDecl(extensionLoc, refComponents,
                                              inherited, parent);
  if (clangNode)
    result->setClangNode(clangNode);

  return result;
}

SourceRange ExtensionDecl::getExtendedTypeRange() const {
  SourceRange range;
  range.Start = getRefComponents().front().NameLoc;
  // FIXME: Consider generic parameters.
  range.End = getRefComponents().back().NameLoc;
  return range;
}

void ExtensionDecl::setGenericSignature(GenericSignature *sig) {
  assert(!GenericSig && "Already have generic signature");
  GenericSig = sig;
}

DeclRange ExtensionDecl::getMembers(bool forceDelayedMembers) const {
  loadAllMembers();
  return IterableDeclContext::getMembers();
}

void ExtensionDecl::setMemberLoader(LazyMemberLoader *resolver,
                                    uint64_t contextData) {
  IterableDeclContext::setLoader(resolver, contextData);
}

ArrayRef<ProtocolConformance*> ExtensionDecl::getConformances() const {
  loadAllConformances(this, Conformances);
  return Conformances.getArray();
}

void ExtensionDecl::setConformanceLoader(LazyMemberLoader *resolver,
                                         uint64_t contextData) {
  assert(!Conformances.isLazy() && "already have a resolver");
  assert(Conformances.getArray().empty() && "already have conformances");
  Conformances.setLoader(resolver, contextData);
}

SourceRange PatternBindingDecl::getSourceRange() const {
  SourceLoc startLoc = getStartLoc();
  if (auto init = getInit()) {
    SourceLoc EndLoc = init->getSourceRange().End;
    if (EndLoc.isValid())
      return { startLoc, EndLoc };
  }
  return { startLoc, Pat->getSourceRange().End };
}

static StaticSpellingKind getCorrectStaticSpellingForDecl(const Decl *D) {
  auto StaticSpelling = StaticSpellingKind::KeywordStatic;
  if (Type T = D->getDeclContext()->getDeclaredTypeInContext()) {
    if (auto NTD = T->getAnyNominal()) {
      if (isa<ClassDecl>(NTD))
        StaticSpelling = StaticSpellingKind::KeywordClass;
    }
  }
  return StaticSpelling;
}

StaticSpellingKind PatternBindingDecl::getCorrectStaticSpelling() const {
  if (!isStatic())
    return StaticSpellingKind::None;

  return getCorrectStaticSpellingForDecl(this);
}

bool PatternBindingDecl::hasStorage() const {
  // Walk the pattern, to check to see if any of the VarDecls included in it
  // have storage.
  bool HasStorage = false;
  getPattern()->forEachVariable([&](VarDecl *VD) {
    if (VD->hasStorage())
      HasStorage = true;
  });

  return HasStorage;
}

void PatternBindingDecl::setPattern(Pattern *P) {
  Pat = P;
  
  // Make sure that any VarDecl's contained within the pattern know about this
  // PatternBindingDecl as their parent.
  if (P)
    P->forEachVariable([&](VarDecl *VD) {
      VD->setParentPattern(this);
    });
}


VarDecl *PatternBindingDecl::getSingleVar() const {
  return getPattern()->getSingleVar();
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
  auto ctx = storage->getDeclContext()->getDeclaredTypeInContext();
  if (!ctx) return false;

  auto nominal = ctx->getNominalOrBoundGenericNominal();
  assert(nominal && "context wasn't a nominal type?");
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
  if (auto *var = dyn_cast<AbstractStorageDecl>(this)) {
    // Observing member are accessed directly from within their didSet/willSet
    // specifiers.  This prevents assignments from becoming infinite loops.
    if (auto *UseFD = dyn_cast<FuncDecl>(UseDC))
      if (var->hasStorage() && var->hasAccessorFunctions() &&
          UseFD->getAccessorStorageDecl() == var)
        return AccessSemantics::DirectToStorage;
    
    // "StoredWithTrivialAccessors" are generally always accessed indirectly,
    // but if we know that the trivial accessor will always produce the same
    // thing as the getter/setter (i.e., it can't be overriden), then just do a
    // direct access.
    //
    // This is true in structs and for final properties.
    // TODO: What about static properties?
    switch (var->getStorageKind()) {
    case AbstractStorageDecl::Stored:
    case AbstractStorageDecl::StoredWithTrivialAccessors:
    case AbstractStorageDecl::Addressed:
    case AbstractStorageDecl::AddressedWithTrivialAccessors:
      if (!isPolymorphic(var))
        return AccessSemantics::DirectToStorage;
      break;

    case AbstractStorageDecl::StoredWithObservers:
    case AbstractStorageDecl::InheritedWithObservers:
    case AbstractStorageDecl::Computed:
    case AbstractStorageDecl::ComputedWithMutableAddress:
    case AbstractStorageDecl::AddressedWithObservers:
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
      SWIFT_FALLTHROUGH;

    case StoredWithTrivialAccessors:
    case AddressedWithTrivialAccessors:
      // If the storage is polymorphic, either the getter or the
      // setter could be overridden by something more interesting.
      if (isPolymorphic(this))
        return AccessStrategy::DispatchToAccessor;

      // Otherwise, just access the storage directly.
      if (storageKind == StoredWithObservers ||
          storageKind == StoredWithTrivialAccessors) {
        return AccessStrategy::Storage;
      } else {
        assert(storageKind == AddressedWithObservers ||
               storageKind == AddressedWithTrivialAccessors);
        return AccessStrategy::Addressor;
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
  }
  llvm_unreachable("bad access semantics");
}

bool ValueDecl::isDefinition() const {
  switch (getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::EnumCase:
  case DeclKind::Subscript:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::IfConfig:
    llvm_unreachable("non-value decls shouldn't get here");

  case DeclKind::Func:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
    return cast<AbstractFunctionDecl>(this)->hasBody();

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

/// Map a function's type to the type used for computing signatures,
/// which involves stripping noreturn, stripping default arguments,
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
    for (const auto &elt : tupleTy->getFields()) {
      Type eltTy = mapSignatureParamType(ctx, elt.getType());
      if (anyChanged || eltTy.getPointer() != elt.getType().getPointer() ||
          elt.hasInit()) {
        if (!anyChanged) {
          elements.reserve(tupleTy->getFields().size());
          for (unsigned i = 0; i != idx; ++i) {
            const TupleTypeElt &elt = tupleTy->getFields()[i];
            elements.push_back(TupleTypeElt(elt.getType(), elt.getName(),
                                            DefaultArgumentKind::None,
                                            elt.isVararg()));
          }
          anyChanged = true;
        }

        elements.push_back(TupleTypeElt(eltTy, elt.getName(),
                                        DefaultArgumentKind::None,
                                        elt.isVararg()));
      }
      ++idx;
    }
    
    if (anyChanged) {
      argTy = TupleType::get(elements, ctx);
    }
  } else {
    if (isMethod) {
      // In methods, map 'self' to an empty tuple to allow comparing two
      // methods from different types to determine if they would conflict when
      // placed into a single nominal decl.
      argTy = TupleType::getEmpty(ctx);
    } else
      argTy = mapSignatureParamType(ctx, argTy);
  }

  // Map the result type.
  auto resultTy = mapSignatureFunctionType(
    ctx, funcTy->getResult(), topLevelFunction, false, isInitializer,
    curryLevels - 1);

  // At the top level, none of the extended information is relevant.
  AnyFunctionType::ExtInfo info;
  if (!topLevelFunction)
    info = funcTy->getExtInfo();

  // Rebuild the resulting function type.
  //
  // If the original function was generic, but our transformations removed all
  // generic parameters from the signature, build a non-generic function type.
  if (argTy->isDependentType() || resultTy->isDependentType())
    if (auto genericFuncTy = dyn_cast<GenericFunctionType>(funcTy))
      return GenericFunctionType::get(genericFuncTy->getGenericSignature(),
                                      argTy, resultTy, info);

  return FunctionType::get(argTy, resultTy, info);
}

OverloadSignature ValueDecl::getOverloadSignature() const {
  OverloadSignature signature;

  signature.Name = getFullName();

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
            afd->getNumParamPatterns())->getCanonicalType();

    signature.IsInstanceMember = isInstanceMember();
    // Unary operators also include prefix/postfix.
    if (auto func = dyn_cast<FuncDecl>(this)) {
      if (func->isUnaryOperator()) {
        signature.UnaryOperator = func->getAttrs().getUnaryOperatorKind();
      }
    }
  } else if (isa<SubscriptDecl>(this)) {
    signature.InterfaceType
      = getInterfaceType()->getWithoutDefaultArgs(getASTContext())
          ->getCanonicalType();    
  } else if (isa<VarDecl>(this)) {
    signature.IsProperty = true;
    signature.IsInstanceMember = isInstanceMember();
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

  // Dynamic lookup can only find [objc] members.
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

ArrayRef<ValueDecl *> ValueDecl::getConformances() {
  if (!conformsToProtocolRequirement())
    return ArrayRef<ValueDecl *>();

  return getASTContext().getConformances(this);
}

void ValueDecl::setType(Type T) {
  assert(!hasType() && "changing type of declaration");
  overwriteType(T);
}

void ValueDecl::overwriteType(Type T) {
  TypeAndAccess.setPointer(T);
  if (!T.isNull() && T->is<ErrorType>())
    setInvalid();
}

DeclContext *ValueDecl::getPotentialGenericDeclContext() {
  if (auto func = dyn_cast<AbstractFunctionDecl>(this))
    return func;
  if (auto NTD = dyn_cast<NominalTypeDecl>(this))
    return NTD;

  auto parentDC = getDeclContext();
  if (parentDC->isTypeContext())
    return parentDC;

  return nullptr;
}

Type ValueDecl::getInterfaceType() const {
  if (InterfaceTy)
    return InterfaceTy;

  if (auto nominal = dyn_cast<NominalTypeDecl>(this))
    return nominal->computeInterfaceType();

  if (auto assocType = dyn_cast<AssociatedTypeDecl>(this)) {
    auto proto = cast<ProtocolDecl>(getDeclContext());
    (void)proto->getType(); // make sure we've computed the type.
    auto selfTy = proto->getGenericParamTypes()[0];
    auto &ctx = getASTContext();
    InterfaceTy = DependentMemberType::get(
                    selfTy,
                    const_cast<AssociatedTypeDecl *>(assocType),
                    ctx);
    InterfaceTy = MetatypeType::get(InterfaceTy, ctx);
    return InterfaceTy;
  }

  if (!hasType())
    return Type();

  // If the type involves a type variable, don't cache it.
  auto type = getType();
  assert((type.isNull() || !type->is<PolymorphicFunctionType>())
         && "decl has polymorphic function type but no interface type");

  if (type->hasTypeVariable())
    return type;

  InterfaceTy = type;
  return InterfaceTy;
}

void ValueDecl::setInterfaceType(Type type) {
  assert((type.isNull() || !type->hasTypeVariable()) &&
         "Type variable in interface type");
  assert((type.isNull() || !type->is<PolymorphicFunctionType>()) &&
         "setting polymorphic function type as interface type");
  
  InterfaceTy = type;
}

Type TypeDecl::getDeclaredType() const {
  if (auto TAD = dyn_cast<TypeAliasDecl>(this)) {
    if (isa<ErrorType>(TAD->getType()->getCanonicalType())) {
      return TAD->getType();
    }
    return TAD->getAliasType();
  }
  if (auto typeParam = dyn_cast<AbstractTypeParamDecl>(this))
    return typeParam->getType()->castTo<MetatypeType>()->getInstanceType();
  return cast<NominalTypeDecl>(this)->getDeclaredType();
}

Type TypeDecl::getDeclaredInterfaceType() const {
  return getInterfaceType()->castTo<MetatypeType>()->getInstanceType();
}

ArrayRef<ProtocolDecl *> TypeDecl::getProtocols(bool forceDelayedMembers) const {
  if (auto *NTD = dyn_cast<NominalTypeDecl>(this))
    return NTD->getProtocols(forceDelayedMembers);
  return Protocols;
}

/// Provide the set of parameters to a generic type, or null if
/// this function is not generic.
void NominalTypeDecl::setGenericParams(GenericParamList *params) {
  assert(!GenericParams && "Already has generic parameters");
  GenericParams = params;
  
  if (params)
    for (auto Param : *params)
      Param->setDeclContext(this);
}


bool NominalTypeDecl::derivesProtocolConformance(ProtocolDecl *protocol) const {
  if (auto *enumDecl = dyn_cast<EnumDecl>(this)) {
    // Enums with raw types can derive their RawRepresentable conformance.
    if (protocol
          == getASTContext().getProtocol(
            KnownProtocolKind::RawRepresentable))
      return enumDecl->hasRawType();
    
    // Simple enums can derive Equatable and Hashable conformance.
    if (protocol
          == getASTContext().getProtocol(KnownProtocolKind::Equatable)
        || protocol
             == getASTContext().getProtocol(KnownProtocolKind::Hashable))
      return enumDecl->isSimpleEnum();
  }
  return false;
}

GenericSignature::GenericSignature(ArrayRef<GenericTypeParamType *> params,
                                   ArrayRef<Requirement> requirements)
  : NumGenericParams(params.size()), NumRequirements(requirements.size()),
    CanonicalSignatureOrASTContext()
{
  bool isCanonical = true;
  
  auto paramsBuffer = getGenericParamsBuffer();
  for (unsigned i = 0; i < NumGenericParams; ++i) {
    paramsBuffer[i] = params[i];
    isCanonical &= params[i]->isCanonical();
  }
  
  auto reqtsBuffer = getRequirementsBuffer();
  for (unsigned i = 0; i < NumRequirements; ++i) {
    reqtsBuffer[i] = requirements[i];
    isCanonical &= requirements[i].getFirstType()->isCanonical();
    isCanonical &= !requirements[i].getSecondType()
                    || requirements[i].getSecondType()->isCanonical();
  }
  
  if (isCanonical)
    CanonicalSignatureOrASTContext = (ASTContext *)nullptr;
}

ArrayRef<GenericTypeParamType *> 
GenericSignature::getInnermostGenericParams() const {
  auto params = getGenericParams();

  // Find the point at which the depth changes.
  unsigned depth = params.back()->getDepth();
  for (unsigned n = params.size(); n > 0; --n) {
    if (params[n-1]->getDepth() != depth) {
      return params.slice(n);
    }
  }

  // All parameters are at the same depth.
  return params;
}

void NominalTypeDecl::setGenericSignature(GenericSignature *sig) {
  assert(!GenericSig && "Already have generic signature");
  GenericSig = sig;
}

void NominalTypeDecl::computeType() {
  assert(!hasType() && "Nominal type declaration already has a type");

  // Compute the declared type.
  Type parentTy = getDeclContext()->getDeclaredTypeInContext();
  ASTContext &ctx = getASTContext();
  if (auto proto = dyn_cast<ProtocolDecl>(this)) {
    if (!DeclaredTy)
      DeclaredTy = ProtocolType::get(proto, ctx);
  } else if (getGenericParams()) {
    DeclaredTy = UnboundGenericType::get(this, parentTy, ctx);
  } else {
    DeclaredTy = NominalType::get(this, parentTy, ctx);
  }

  // Set the type.
  setType(MetatypeType::get(DeclaredTy, ctx));

  // A protocol has an implicit generic parameter list consisting of a single
  // generic parameter, Self, that conforms to the protocol itself. This
  // parameter is always implicitly bound.
  //
  // If this protocol has been deserialized, it already has generic parameters.
  // Don't add them again.
  if (!getGenericParams()) {
    if (auto proto = dyn_cast<ProtocolDecl>(this)) {
      // The generic parameter 'Self'.
      auto selfId = ctx.Id_Self;
      auto selfDecl = new (ctx) GenericTypeParamDecl(proto, selfId,
                                                     proto->getLoc(), 0, 0);
      auto protoRef = new (ctx) SimpleIdentTypeRepr(proto->getLoc(),
                                                    proto->getName());
      protoRef->setValue(proto);
      TypeLoc selfInherited[1] = { TypeLoc(protoRef) };
      selfInherited[0].setType(DeclaredTy);
      selfDecl->setInherited(ctx.AllocateCopy(selfInherited));
      selfDecl->setImplicit();
      
      // The generic parameter list itself.
      GenericParams = GenericParamList::create(ctx, SourceLoc(), selfDecl,
                                               SourceLoc());
    }
  }
}

Type NominalTypeDecl::getDeclaredTypeInContext() const {
  if (DeclaredTyInContext)
    return DeclaredTyInContext;
  
  Type Ty = getDeclaredType();
  if (UnboundGenericType *UGT = Ty->getAs<UnboundGenericType>()) {
    // If we have an unbound generic type, bind the type to the archetypes
    // in the type's definition.
    NominalTypeDecl *D = UGT->getDecl();
    SmallVector<Type, 4> GenericArgs;
    for (auto Param : *D->getGenericParams())
      GenericArgs.push_back(Param->getArchetype());
    Ty = BoundGenericType::get(D, getDeclContext()->getDeclaredTypeInContext(),
                               GenericArgs);
  }
  const_cast<NominalTypeDecl *>(this)->DeclaredTyInContext = Ty;
  return DeclaredTyInContext;
}

Type NominalTypeDecl::computeInterfaceType() const {
  if (InterfaceTy)
    return InterfaceTy;

  // Figure out the interface type of the parent.
  Type parentType;
  if (auto typeOfParentContext = getDeclContext()->getDeclaredTypeOfContext())
    parentType = typeOfParentContext->getAnyNominal()
                   ->getDeclaredInterfaceType();

  Type type;
  if (auto proto = dyn_cast<ProtocolDecl>(this)) {
    type = ProtocolType::get(const_cast<ProtocolDecl *>(proto),getASTContext());
  } else if (auto params = getGenericParams()) {
    // If we have a generic type, bind the type to the archetypes
    // in the type's definition.
    SmallVector<Type, 4> genericArgs;
    for (auto param : *params)
      genericArgs.push_back(param->getDeclaredType());

    type = BoundGenericType::get(const_cast<NominalTypeDecl *>(this),
                                 parentType, genericArgs);
  } else {
    type = NominalType::get(const_cast<NominalTypeDecl *>(this), parentType,
                            getASTContext());
  }

  InterfaceTy = MetatypeType::get(type, getASTContext());
  return InterfaceTy;

}

ExtensionRange NominalTypeDecl::getExtensions() {
  auto &context = Decl::getASTContext();

  // If our list of extensions is out of date, update it now.
  if (context.getCurrentGeneration() > ExtensionGeneration) {
    unsigned previousGeneration = ExtensionGeneration;
    ExtensionGeneration = context.getCurrentGeneration();
    context.loadExtensions(this, previousGeneration);
  }

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

void NominalTypeDecl::getImplicitProtocols(
       SmallVectorImpl<ProtocolDecl *> &protocols) {
  // If this is a class, it conforms to the AnyObject protocol.
  if (isa<ClassDecl>(this)) {
    if (auto anyObject
          = getASTContext().getProtocol(KnownProtocolKind::AnyObject)) {
      protocols.push_back(anyObject);
    }
  }
  
  // If this is a simple enum, it conforms to the Hashable and Equatable
  // protocols.
  if (auto theEnum = dyn_cast<EnumDecl>(this)) {
    if (theEnum->isSimpleEnum()) {
      if (auto equatable = getASTContext().getProtocol(
                                                 KnownProtocolKind::Equatable))
        protocols.push_back(equatable);
        
      if (auto hashable = getASTContext().getProtocol(
                                                 KnownProtocolKind::Hashable))
        protocols.push_back(hashable);
    }
  }
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

TypeAliasDecl::TypeAliasDecl(SourceLoc TypeAliasLoc, Identifier Name,
                             SourceLoc NameLoc, TypeLoc UnderlyingTy,
                             DeclContext *DC)
  : TypeDecl(DeclKind::TypeAlias, DC, Name, NameLoc, {}),
    TypeAliasLoc(TypeAliasLoc),
    UnderlyingTy(UnderlyingTy)
{
  // Set the type of the TypeAlias to the right MetatypeType.
  ASTContext &Ctx = getASTContext();
  AliasTy = new (Ctx, AllocationArena::Permanent) NameAliasType(this);
  setType(MetatypeType::get(AliasTy, Ctx));
}

SourceRange TypeAliasDecl::getSourceRange() const {
  if (UnderlyingTy.hasLocation())
    return { TypeAliasLoc, UnderlyingTy.getSourceRange().End };
  // FIXME: Inherits clauses
  return { TypeAliasLoc, getNameLoc() };
}

GenericTypeParamDecl::GenericTypeParamDecl(DeclContext *dc, Identifier name,
                                           SourceLoc nameLoc,
                                           unsigned depth, unsigned index)
  : AbstractTypeParamDecl(DeclKind::GenericTypeParam, dc, name, nameLoc),
    Depth(depth), Index(index)
{
  auto &ctx = dc->getASTContext();
  auto type = new (ctx, AllocationArena::Permanent) GenericTypeParamType(this);
  setType(MetatypeType::get(type, ctx));
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
                                       TypeLoc defaultDefinition)
  : AbstractTypeParamDecl(DeclKind::AssociatedType, dc, name, nameLoc),
    KeywordLoc(keywordLoc), DefaultDefinition(defaultDefinition)
{
  auto &ctx = dc->getASTContext();
  auto type = new (ctx, AllocationArena::Permanent) AssociatedTypeType(this);
  setType(MetatypeType::get(type, ctx));
}

AssociatedTypeDecl::AssociatedTypeDecl(DeclContext *dc, SourceLoc keywordLoc,
                                       Identifier name, SourceLoc nameLoc,
                                       LazyMemberLoader *definitionResolver,
                                       uint64_t resolverData)
  : AbstractTypeParamDecl(DeclKind::AssociatedType, dc, name, nameLoc),
    KeywordLoc(keywordLoc), Resolver(definitionResolver),
    ResolverContextData(resolverData)
{
  assert(Resolver && "missing resolver");
  auto &ctx = dc->getASTContext();
  auto type = new (ctx, AllocationArena::Permanent) AssociatedTypeType(this);
  setType(MetatypeType::get(type, ctx));
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
  ClassDeclBits.Foreign = false;
  ClassDeclBits.HasDestructorDecl = 0;
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

    // Resolve this initializer, if needed.
    if (!ctor->hasType())
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
  for (auto member : superclassDecl->lookupDirect(ctx.Id_init)) {
    if (AvailabilityAttr::isUnavailable(member))
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
  // by the sublcass. Initializers can be inherited.
  ClassDeclBits.InheritsSuperclassInits
    = static_cast<unsigned>(StoredInheritsSuperclassInits::Inherited);
  return true;
}

/// Mangle the name of a protocol or class for use in the Objective-C
/// runtime.
static StringRef mangleObjCRuntimeName(const NominalTypeDecl *nominal,
                                       llvm::SmallVectorImpl<char> &buffer) {
  {
    buffer.clear();
    llvm::raw_svector_ostream os(buffer);

    // We add the "_Tt" prefix to make this a reserved name that will
    // not conflict with any valid Objective-C class or protocol name.
    os << "_Tt";

    // Mangle the type.
    Mangle::Mangler mangler(os, false/*dwarf*/, false/*punycode*/);
    NominalTypeDecl *NTD = const_cast<NominalTypeDecl*>(nominal);
    if (isa<ClassDecl>(nominal)) {
      mangler.mangleNominalType(NTD,
                                ResilienceExpansion::Minimal,
                                Mangle::Mangler::BindGenerics::None);
    } else {
      mangler.mangleProtocolDecl(cast<ProtocolDecl>(NTD));
    }
  }

  return StringRef(buffer.data(), buffer.size());
}

StringRef ClassDecl::getObjCRuntimeName(
                       llvm::SmallVectorImpl<char> &buffer) const {
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

FuncDecl *ClassDecl::findOverridingDecl(const FuncDecl *Method) const {
  auto Members = getMembers();
  for (auto M : Members) {
    FuncDecl *CurMethod = dyn_cast<FuncDecl>(M);
    if (!CurMethod)
      continue;
    if (CurMethod->isOverridingDecl(Method)) {
      return CurMethod;
    }
  }
  return nullptr;
}

FuncDecl * ClassDecl::findImplementingMethod(const FuncDecl *Method) const {
  const ClassDecl *C = this;
  while (C) {
    auto Members = C->getMembers();
    for (auto M : Members) {
      FuncDecl *CurMethod = dyn_cast<FuncDecl>(M);
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

bool EnumDecl::isSimpleEnum() const {
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
  ProtocolDeclBits.KnownProtocol = 0;
  ProtocolDeclBits.Circularity
    = static_cast<unsigned>(CircularityCheck::Unchecked);
  ProtocolDeclBits.HasMissingRequirements = false;
}

bool ProtocolDecl::inheritsFrom(const ProtocolDecl *Super) const {
  if (this == Super)
    return false;
  
  llvm::SmallPtrSet<const ProtocolDecl *, 4> Visited;
  SmallVector<const ProtocolDecl *, 4> Stack;
  
  Stack.push_back(this);
  Visited.insert(this);
  while (!Stack.empty()) {
    const ProtocolDecl *Current = Stack.back();
    Stack.pop_back();

    for (auto InheritedProto : Current->getProtocols()) {
      if (InheritedProto == Super)
        return true;

      if (Visited.insert(InheritedProto).second)
        Stack.push_back(InheritedProto);
    }
  }
  
  return false;
}

void ProtocolDecl::collectInherited(
       llvm::SmallPtrSet<ProtocolDecl *, 4> &Inherited) {
  SmallVector<const ProtocolDecl *, 4> Stack;
  
  Stack.push_back(this);
  while (!Stack.empty()) {
    const ProtocolDecl *Current = Stack.back();
    Stack.pop_back();

    for (auto InheritedProto : Current->getProtocols()) {
      if (Inherited.insert(InheritedProto).second)
        Stack.push_back(InheritedProto);
    }
  }
}

bool ProtocolDecl::requiresClassSlow() {
  ProtocolDeclBits.RequiresClass = false;

  // Ensure that the result can not change in future.
  assert(isProtocolsValid());

  if (getAttrs().hasAttribute<ObjCAttr>() || isObjC()) {
    ProtocolDeclBits.RequiresClass = true;
    return true;
  }

  // Check inherited protocols for class-ness.
  for (auto *proto : getProtocols()) {
    if (proto->requiresClass()) {
      ProtocolDeclBits.RequiresClass = true;
      return true;
    }
  }

  return false;
}

GenericTypeParamDecl *ProtocolDecl::getSelf() const {
  return getGenericParams()->getParams().front();
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
  if (getter) {
    getter->makeAccessor(this, AccessorKind::IsGetter);
  }

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
             fn->getAccessibility() == setterAccess.getValue());
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
           Set->getAccessibility() == setterAccess.getValue());
    Set->overwriteAccessibility(setterAccess.getValue());
  }
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

  void *mem = ctx.Allocate(sizeof(GetSetRecordWithAddressors),
                           alignof(GetSetRecordWithAddressors));
  auto info = new (mem) GetSetRecordWithAddressors();
  info->Braces = SourceRange(lbraceLoc, rbraceLoc);
  GetSetInfo.setPointer(info);
  setStorageKind(ComputedWithMutableAddress);

  configureAddressorRecord(info, nullptr, mutableAddressor);
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
           accessor->getAccessibility() == setterAccess.getValue());
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

  void *mem = ctx.Allocate(sizeof(GetSetRecordWithAddressors),
                           alignof(GetSetRecordWithAddressors));
  auto info = new (mem) GetSetRecordWithAddressors();
  info->Braces = SourceRange(lbraceLoc, rbraceLoc);
  GetSetInfo.setPointer(info);
  setStorageKind(Addressed);

  configureAddressorRecord(info, addressor, mutableAddressor);
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
  void *mem = ctx.Allocate(sizeof(ObservingRecordWithAddressors),
                           alignof(ObservingRecordWithAddressors));
  auto info = new (mem) ObservingRecordWithAddressors();
  info->Braces = SourceRange(lbraceLoc, rbraceLoc);
  GetSetInfo.setPointer(info);
  setStorageKind(AddressedWithObservers);

  configureAddressorRecord(info, addressor, mutableAddressor);
  configureObservingRecord(info, willSet, didSet);
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

ObjCSelector AbstractStorageDecl::getObjCGetterSelector() const {
  // If the getter has an @objc attribute with a name, use that.
  if (auto getter = getGetter()) {
    if (auto objcAttr = getter->getAttrs().getAttribute<ObjCAttr>()) {
      if (auto name = objcAttr->getName())
        return *name;
    }
  }

  // Subscripts use a specific selector.
  auto &ctx = getASTContext();
  if (auto *SD = dyn_cast<SubscriptDecl>(this)) {
    switch (SD->getObjCSubscriptKind(nullptr)) {
    case ObjCSubscriptKind::None:
      llvm_unreachable("Not an Objective-C subscript");
    case ObjCSubscriptKind::Indexed:
      return ObjCSelector(ctx, 1, ctx.Id_objectAtIndexedSubscript);
    case ObjCSubscriptKind::Keyed:
      return ObjCSelector(ctx, 1, ctx.Id_objectForKeyedSubscript);
    }
  }

  // The getter selector is the property name itself.
  return ObjCSelector(ctx, 0, getName());
}

ObjCSelector AbstractStorageDecl::getObjCSetterSelector() const {
  // If the setter has an @objc attribute with a name, use that.
  auto setter = getSetter();
  auto objcAttr = setter ? setter->getAttrs().getAttribute<ObjCAttr>()
                         : nullptr;
  if (objcAttr) {
    if (auto name = objcAttr->getName())
      return *name;
  }

  // Subscripts use a specific selector.
  auto &ctx = getASTContext();
  if (auto *SD = dyn_cast<SubscriptDecl>(this)) {
    switch (SD->getObjCSubscriptKind(nullptr)) {
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
  llvm::SmallString<16> scratch;
  scratch += "set";
  camel_case::appendSentenceCase(scratch, getName().str());

  auto result = ObjCSelector(ctx, 1, ctx.getIdentifier(scratch));

  // Cache the result, so we don't perform string manipulation again.
  if (objcAttr)
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

/// \brief Returns whether the var is settable in the specified context: this
/// is either because it is a stored var, because it has a custom setter, or
/// is a let member in an initializer.
bool VarDecl::isSettable(DeclContext *UseDC) const {
  // 'let' properties are immutable, but enforcement of this is enforced by
  // SIL level passes.  If the 'let' property has an initializer, it can never
  // be reassigned, so we model it as not settable here.
  if (isLet()) {
    // If the decl has an explicitly written initializer with a pattern binding,
    // then it isn't settable.
    if (auto *P = getParentPattern())
      if (P->hasInit())
        return false;

    // If the decl has a value bound to it but has no PBD, then it is
    // initialized.
    if (hasNonPatternBindingInit())
      return false;
    
    // 'let' parameters are never settable.
    if (isa<ParamDecl>(this))
      return false;
    
    // Properties in structs/classes are only ever mutable in their designated
    // initializer.
    if (getDeclContext()->isTypeContext()) {
      auto *CD = dyn_cast_or_null<ConstructorDecl>(UseDC);
      if (!CD) return false;
      
      auto *CDC = CD->getDeclContext();
        
      // If this init is defined inside of the same type (or in an extension
      // thereof) as the let property, then it is mutable.
      if (CDC->isTypeContext() &&
          CDC->getDeclaredTypeInContext()->getAnyNominal() ==
          getDeclContext()->getDeclaredTypeInContext()->getAnyNominal()) {
        // If this is a convenience initializer (i.e. one that calls
        // self.init), then let properties are never mutable in it.  They are
        // only mutable in designated initializers.
        if (CD->getDelegatingOrChainedInitKind(nullptr) ==
            ConstructorDecl::BodyInitKind::Delegating)
          return false;
        return true;
      }
    } else {
      // Normal variables (e.g. globals) are only mutable in the context of the
      // declaration.  To handle top-level code properly, we look through
      // the TopLevelCode decl on the use (if present) since the vardecl will be
      // one level up.
      if (UseDC && isa<TopLevelCodeDecl>(UseDC))
        UseDC = UseDC->getParent();
      
      if (getDeclContext() != UseDC)
        return false;
    }
  }

  return ::isSettable(this);
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
  Pattern *Pat = nullptr;
  if (auto PatBind = ParentPattern.dyn_cast<PatternBindingDecl *>())
    Pat = PatBind->getPattern();
  else
    Pat = ParentPattern.dyn_cast<Pattern *>();

  if (!Pat || Pat->isImplicit())
    return getSourceRange();

  if (auto *VP = dyn_cast<VarPattern>(Pat))
    Pat = VP->getSubPattern();
  if (auto *TP = dyn_cast<TypedPattern>(Pat))
    return TP->getTypeLoc().getTypeRepr()->getSourceRange();
  return getSourceRange();
}

/// Return true if this stored property needs to be accessed with getters and
/// setters for Objective-C.
bool AbstractStorageDecl::hasObjCGetterAndSetter() const {
  // We don't export generic methods or subclasses to IRGen yet.
  auto *DC = getDeclContext();
  if (DC->getDeclaredTypeInContext() &&
      DC->getDeclaredTypeInContext()->is<BoundGenericType>() &&
      !isa<ProtocolDecl>(DC))
    return false;

  if (auto override = getOverriddenDecl())
    return override->hasObjCGetterAndSetter();

  if (!isObjC())
    return false;

  // Don't expose objc properties for variables with thin function type.
  // We can't bridge them.
  if (isa<VarDecl>(this)) {
    if (auto ft = getType()->getAs<AnyFunctionType>()) {
      switch (ft->getRepresentation()) {
      case AnyFunctionType::Representation::Thick:
      case AnyFunctionType::Representation::Block:
        return true;
      case AnyFunctionType::Representation::Thin:
        return false;
      }
    }
  }

  return true;
}

bool AbstractStorageDecl::requiresObjCGetterAndSetter() const {
  if (isFinal())
    return false;
  if (!hasObjCGetterAndSetter())
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

  return getCorrectStaticSpellingForDecl(this);
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
    return singleVar->getType()->is<BuiltinIntegerType>();
  }

  return false;
}

void SubscriptDecl::setIndices(Pattern *p) {
  Indices = p;
  
  // FIXME: What context should the indices patterns be in?
}

Type SubscriptDecl::getIndicesType() const {
  return getType()->castTo<AnyFunctionType>()->getInput();
}

Type SubscriptDecl::getIndicesInterfaceType() const {
  // FIXME: Unfortunate that we can't really capture the generic parameters
  // here.
  return getInterfaceType()->castTo<AnyFunctionType>()->getInput();
}

ObjCSubscriptKind SubscriptDecl::getObjCSubscriptKind(
                    LazyResolver *resolver) const {
  auto indexTy = getIndicesType();

  // Look through a named 1-tuple.
  if (auto tupleTy = indexTy->getAs<TupleType>()) {
    if (tupleTy->getNumElements() == 1 &&
        !tupleTy->getFields()[0].isVararg()) {
      indexTy = tupleTy->getElementType(0);
    }
  }

  // If the index type is an integral type, we have an indexed
  // subscript.
  if (isIntegralType(indexTy))
    return ObjCSubscriptKind::Indexed;

  // If the index type is an object type in Objective-C, we have a
  // keyed subscript.
  if (Type objectTy = indexTy->getAnyOptionalObjectType())
    indexTy = objectTy;

  if (getASTContext().getBridgedToObjC(getDeclContext(), false, indexTy,
                                       resolver))
    return ObjCSubscriptKind::Keyed;

  return ObjCSubscriptKind::None;
}

SourceRange SubscriptDecl::getSourceRange() const {
  if (getBracesRange().isValid())
    return { getSubscriptLoc(), getBracesRange().End };
  return { getSubscriptLoc(), ElementTy.getSourceRange().End };
}

static Type getSelfTypeForContainer(AbstractFunctionDecl *theMethod,
                                    bool isInitializingCtor,
                                    bool wantInterfaceType,
                                    GenericParamList **outerGenericParams) {
  auto *dc = theMethod->getDeclContext();
  
  // Determine the type of the container.
  Type containerTy = wantInterfaceType ? dc->getDeclaredInterfaceType()
                                       : dc->getDeclaredTypeInContext();
  assert(containerTy && "stand alone functions don't have 'self'");
  if (!containerTy) return Type();

  bool isStatic = false;
  bool isMutating = false;
  Type selfTypeOverride;
  
  if (auto *FD = dyn_cast<FuncDecl>(theMethod)) {
    isStatic = FD->isStatic();
    isMutating = FD->isMutating();

    // The non-interface type of a method that returns DynamicSelf
    // uses DynamicSelf for the type of 'self', which is important
    // when type checking the body of the function.
    if (!wantInterfaceType)
      selfTypeOverride = FD->getDynamicSelf();
  } else if (isa<ConstructorDecl>(theMethod)) {
    if (isInitializingCtor) {
      // initializing constructors of value types always have an implicitly
      // inout self.
      isMutating = true;
    } else {
      // allocating constructors have metatype 'self'.
      isStatic = true;
    }
  } else if (isa<DestructorDecl>(theMethod)) {
    // destructors of value types always have an implicitly inout self.
    isMutating = true;
  }

  
  if (outerGenericParams)
    *outerGenericParams = nullptr;
  
  Type selfTy = selfTypeOverride;
  if (!selfTy) {
    // For a protocol, the type of 'self' is the parameter type 'Self', not
    // the protocol itself.
    selfTy = containerTy;
    if (auto proto = containerTy->getAs<ProtocolType>()) {
      auto self = proto->getDecl()->getSelf();
      assert(self && "Missing 'Self' type in protocol");
      if (wantInterfaceType)
        selfTy = self->getDeclaredType();
      else
        selfTy = self->getArchetype();
    }
  }
  
  // If the self type is the result of an upstream error, return it
  if (selfTy->is<ErrorType>())
    return selfTy;
  
  // Capture the generic parameters, if requested.
  if (outerGenericParams)
    *outerGenericParams = dc->getGenericParamsOfContext();
  
  // 'static' functions have 'self' of type metatype<T>.
  if (isStatic)
    return MetatypeType::get(selfTy, dc->getASTContext());
  
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
                           : DeclName(ctx, afd->getName(), { });

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

void AbstractFunctionDecl::setGenericParams(GenericParamList *GP) {
  // Set the specified generic parameters onto this abstract function, setting
  // the parameters' context to the function along the way.
  GenericParams = GP;
  if (GP)
    for (auto Param : *GP)
      Param->setDeclContext(this);
}


Type AbstractFunctionDecl::
computeSelfType(GenericParamList **outerGenericParams) {
  return getSelfTypeForContainer(this, true, false, outerGenericParams);
}

Type AbstractFunctionDecl::computeInterfaceSelfType(bool isInitializingCtor) {
  return getSelfTypeForContainer(this, isInitializingCtor, true, nullptr);
}

/// \brief This method returns the implicit 'self' decl.
///
/// Note that some functions don't have an implicit 'self' decl, for example,
/// free functions.  In this case nullptr is returned.
VarDecl *AbstractFunctionDecl::getImplicitSelfDecl() const {
  ArrayRef<const Pattern *> ParamPatterns = getBodyParamPatterns();
  if (ParamPatterns.empty())
    return nullptr;

  // "self" is represented as (typed_pattern (named_pattern (var_decl 'self')).
  const Pattern *P = ParamPatterns[0]->getSemanticsProvidingPattern();

  // The decl should be named 'self' and be implicit.
  auto NP = dyn_cast<NamedPattern>(P);
  if (NP && NP->isImplicit() &&
      NP->getDecl()->getName() == getASTContext().Id_self)
    return NP->getDecl();
  return nullptr;
}

Type AbstractFunctionDecl::getExtensionType() const {
  return getDeclContext()->getDeclaredTypeInContext();
}

std::pair<DefaultArgumentKind, Type>
AbstractFunctionDecl::getDefaultArg(unsigned Index) const {
  ArrayRef<const Pattern *> Patterns = getBodyParamPatterns();

  if (getImplicitSelfDecl()) {
    // Skip the 'self' parameter; it is not counted.
    Patterns = Patterns.slice(1);
  }

  // Find the (sub-)pattern for this index.
  // FIXME: This is O(n), which is lame. We should fix the FuncDecl
  // representation.
  const TuplePatternElt *Found = nullptr;
  for (auto OrigPattern : Patterns) {
    auto Params =
        dyn_cast<TuplePattern>(OrigPattern->getSemanticsProvidingPattern());
    if (!Params) {
      if (Index == 0) {
        return { DefaultArgumentKind::None, Type() };
      }

      --Index;
      continue;
    }

    for (auto &Elt : Params->getFields()) {
      if (Index == 0) {
        Found = &Elt;
        break;
      }
      --Index;
    }

    if (Found)
      break;
  }

  assert(Found && "No argument with this index");
  return { Found->getDefaultArgKind(), Found->getPattern()->getType() };
}

bool AbstractFunctionDecl::argumentNameIsAPIByDefault(unsigned i) const {
  // All initializer argument names are API by default.
  if (isa<ConstructorDecl>(this))
    return true;

  if (auto func = dyn_cast<FuncDecl>(this)) {
    // No argument names for operators or global functions are API by default.
    if (func->isOperator() || !func->getDeclContext()->isTypeContext())
      return false;

    // For methods, argument names after the first argument are API by default.
    return i > 0;
  }

  assert(isa<DestructorDecl>(this));
  return false;
}

SourceRange AbstractFunctionDecl::getBodySourceRange() const {
  switch (getBodyKind()) {
  case BodyKind::None:
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

  auto Pats = getBodyParamPatterns();
  if (Pats.empty())
    return getNameLoc();

  for (int I = Pats.size() - 1; I >= 0; I--) {
    auto endLoc = Pats[I]->getEndLoc();
    if (endLoc.isValid()) {
      return SourceRange(getNameLoc(), endLoc);
    }
  }
  return getNameLoc();
}

ObjCSelector AbstractFunctionDecl::getObjCSelector() const {
  if (auto func = dyn_cast<FuncDecl>(this))
    return func->getObjCSelector();
  if (auto ctor = dyn_cast<ConstructorDecl>(this))
    return ctor->getObjCSelector();
  if (auto dtor = dyn_cast<DestructorDecl>(this))
    return dtor->getObjCSelector();
  llvm_unreachable("Unhandled AbstractFunctionDecl subclass");
}

AbstractFunctionDecl *AbstractFunctionDecl::getOverriddenDecl() const {
  if (auto func = dyn_cast<FuncDecl>(this))
    return func->getOverriddenDecl();
  if (auto ctor = dyn_cast<ConstructorDecl>(this))
    return ctor->getOverriddenDecl();
  
  return nullptr;
}

/// Set the DeclContext of any VarDecls in P to the specified DeclContext.
static void setDeclContextOfPatternVars(Pattern *P, DeclContext *DC) {
  if (!P) return;
  P->forEachVariable([&](VarDecl *VD) {
    assert(isa<ParamDecl>(VD) && "Pattern variable is not a parameter?");
    VD->setDeclContext(DC);
  });
}

FuncDecl *FuncDecl::createImpl(ASTContext &Context,
                               SourceLoc StaticLoc,
                               StaticSpellingKind StaticSpelling,
                               SourceLoc FuncLoc,
                               DeclName Name, SourceLoc NameLoc,
                               GenericParamList *GenericParams,
                               Type Ty, unsigned NumParamPatterns,
                               DeclContext *Parent,
                               ClangNode ClangN) {
  assert(NumParamPatterns > 0);
  size_t Size = sizeof(FuncDecl) + NumParamPatterns * sizeof(Pattern *);
  void *DeclPtr = allocateMemoryForDecl<FuncDecl>(Context, Size,
                                                  !ClangN.isNull());
  auto D = ::new (DeclPtr)
      FuncDecl(StaticLoc, StaticSpelling, FuncLoc, Name, NameLoc,
               NumParamPatterns, GenericParams, Ty, Parent);
  if (ClangN)
    D->setClangNode(ClangN);
  return D;
}

FuncDecl *FuncDecl::createDeserialized(ASTContext &Context,
                                       SourceLoc StaticLoc,
                                       StaticSpellingKind StaticSpelling,
                                       SourceLoc FuncLoc,
                                       DeclName Name, SourceLoc NameLoc,
                                       GenericParamList *GenericParams,
                                       Type Ty, unsigned NumParamPatterns,
                                       DeclContext *Parent) {
  return createImpl(Context, StaticLoc, StaticSpelling, FuncLoc, Name, NameLoc,
                    GenericParams, Ty, NumParamPatterns, Parent, ClangNode());
}

FuncDecl *FuncDecl::create(ASTContext &Context, SourceLoc StaticLoc,
                           StaticSpellingKind StaticSpelling,
                           SourceLoc FuncLoc, DeclName Name,
                           SourceLoc NameLoc, GenericParamList *GenericParams,
                           Type Ty, ArrayRef<Pattern *> BodyParams,
                           TypeLoc FnRetType, DeclContext *Parent,
                           ClangNode ClangN) {
  const unsigned NumParamPatterns = BodyParams.size();
  auto *FD = FuncDecl::createImpl(
      Context, StaticLoc, StaticSpelling, FuncLoc, Name, NameLoc,
      GenericParams, Ty, NumParamPatterns, Parent, ClangN);
  FD->setDeserializedSignature(BodyParams, FnRetType);
  return FD;
}

StaticSpellingKind FuncDecl::getCorrectStaticSpelling() const {
  assert(getDeclContext()->isTypeContext());
  if (!isStatic())
    return StaticSpellingKind::None;

  return getCorrectStaticSpellingForDecl(this);
}

bool FuncDecl::isExplicitNonMutating() const {
  return !isMutating() &&
         isAccessor() && !isGetter() &&
         isInstanceMember() &&
         !getDeclContext()->getDeclaredTypeInContext()->hasReferenceSemantics();
}

void FuncDecl::setDeserializedSignature(ArrayRef<Pattern *> BodyParams,
                                        TypeLoc FnRetType) {
  MutableArrayRef<Pattern *> BodyParamsRef = getBodyParamPatterns();
  unsigned NumParamPatterns = BodyParamsRef.size();

#ifndef NDEBUG
  unsigned NumParams = getDeclContext()->isTypeContext()
                         ? BodyParams[1]->numTopLevelVariables()
                         : BodyParams[0]->numTopLevelVariables();
  auto Name = getFullName();
  assert((!Name || !Name.isSimpleName()) && "Must have a simple name");
  assert(!Name || (Name.getArgumentNames().size() == NumParams));
#endif

  for (unsigned i = 0; i != NumParamPatterns; ++i)
    BodyParamsRef[i] = BodyParams[i];

  // Set the decl context of any vardecls to this FuncDecl.
  for (auto P : BodyParams)
    setDeclContextOfPatternVars(P, this);

  this->FnRetType = FnRetType;
}

Type FuncDecl::getResultType() const {
  if (!hasType())
    return nullptr;

  Type resultTy = getType();
  if (resultTy->is<ErrorType>())
    return resultTy;

  for (unsigned i = 0, e = getNaturalArgumentCount(); i != e; ++i)
    resultTy = resultTy->castTo<AnyFunctionType>()->getResult();

  if (!resultTy)
    resultTy = TupleType::getEmpty(getASTContext());

  return resultTy;
}

bool FuncDecl::isUnaryOperator() const {
  if (!isOperator())
    return false;
  
  unsigned opArgIndex = isa<ProtocolDecl>(getDeclContext()) ? 1 : 0;
  
  auto *argTuple = dyn_cast<TuplePattern>(getBodyParamPatterns()[opArgIndex]);
  if (!argTuple)
    return true;

  return argTuple->getNumFields() == 1 && !argTuple->hasVararg();
}

bool FuncDecl::isBinaryOperator() const {
  if (!isOperator())
    return false;
  
  unsigned opArgIndex = isa<ProtocolDecl>(getDeclContext()) ? 1 : 0;
  
  auto *argTuple = dyn_cast<TuplePattern>(getBodyParamPatterns()[opArgIndex]);
  if (!argTuple)
    return false;
  
  return argTuple->getNumFields() == 2
    || (argTuple->getNumFields() == 1 && argTuple->hasVararg());
}

bool FuncDecl::isOverridingDecl(const FuncDecl *Method) const {
  const FuncDecl *CurMethod = this;
  while (CurMethod) {
    if (CurMethod == Method)
      return true;
    CurMethod = CurMethod->getOverriddenDecl();
  }
  return false;
}

ConstructorDecl::ConstructorDecl(DeclName Name, SourceLoc ConstructorLoc,
                                 OptionalTypeKind Failability, 
                                 SourceLoc FailabilityLoc,
                                 Pattern *SelfBodyParam, Pattern *BodyParams,
                                 GenericParamList *GenericParams,
                                 DeclContext *Parent)
  : AbstractFunctionDecl(DeclKind::Constructor, Parent, Name,
                         ConstructorLoc, 2, GenericParams),
    FailabilityLoc(FailabilityLoc) 
{
  setBodyParams(SelfBodyParam, BodyParams);
  
  ConstructorDeclBits.ComputedBodyInitKind = 0;
  ConstructorDeclBits.InitKind
    = static_cast<unsigned>(CtorInitializerKind::Designated);
  ConstructorDeclBits.HasStubImplementation = 0;
  this->Failability = static_cast<unsigned>(Failability);
}

void ConstructorDecl::setBodyParams(Pattern *selfPattern, Pattern *bodyParams) {
  BodyParams[0] = selfPattern;
  BodyParams[1] = bodyParams;
  setDeclContextOfPatternVars(selfPattern, this);
  setDeclContextOfPatternVars(bodyParams, this);
  
  assert(!getFullName().isSimpleName() && "Constructor name must be compound");
  assert(!bodyParams || 
         (getFullName().getArgumentNames().size() 
          == bodyParams->numTopLevelVariables()));
}

DestructorDecl::DestructorDecl(Identifier NameHack, SourceLoc DestructorLoc,
                               Pattern *SelfPattern, DeclContext *Parent)
  : AbstractFunctionDecl(DeclKind::Destructor, Parent, NameHack,
                         DestructorLoc, 1, nullptr) {
  setSelfPattern(SelfPattern);
}

void DestructorDecl::setSelfPattern(Pattern *selfPattern) {
  SelfPattern = selfPattern;
  setDeclContextOfPatternVars(SelfPattern, this);
}


DynamicSelfType *FuncDecl::getDynamicSelf() const {
  if (!hasDynamicSelf())
    return nullptr;

  auto extType = getExtensionType();
  if (auto protoTy = extType->getAs<ProtocolType>())
    return DynamicSelfType::get(protoTy->getDecl()->getSelf()->getArchetype(),
                                getASTContext());

  return DynamicSelfType::get(extType, getASTContext());
}

DynamicSelfType *FuncDecl::getDynamicSelfInterface() const {
  if (!hasDynamicSelf())
    return nullptr;

  auto extType = getDeclContext()->getDeclaredInterfaceType();
  if (auto protoTy = extType->getAs<ProtocolType>())
    return DynamicSelfType::get(protoTy->getDecl()->getSelf()->getDeclaredType(),
                                getASTContext());

  return DynamicSelfType::get(extType, getASTContext());
}

/// Produce the selector for this "Objective-C method" in the given buffer.
ObjCSelector FuncDecl::getObjCSelector() const {
  // For a getter or setter, go through the variable or subscript decl.
  if (isGetterOrSetter()) {
    auto asd = cast<AbstractStorageDecl>(getAccessorStorageDecl());
    return isGetter() ? asd->getObjCGetterSelector()
                      : asd->getObjCSetterSelector();
  }

  // If there is an @objc attribute with a name, use that name.
  auto objc = getAttrs().getAttribute<ObjCAttr>();
  if (objc) {
    if (auto name = objc->getName())
      return *name;
  }

  // We should always have exactly two levels of argument pattern.
  auto argNames = getFullName().getArgumentNames();
  auto &ctx = getASTContext();

  // If we have no arguments, it's a nullary selector.
  if (argNames.size() == 0) {
    return ObjCSelector(ctx, 0, getName());
  }

  // If it's a unary selector with no name for the first argument, we're done.
  if (argNames.size() == 1 && argNames[0].empty()) {
    return ObjCSelector(ctx, 1, getName());
  }

  // Attach the first parameter name to the base name.
  auto firstPiece = getName();
  bool didStringManipulation = false;
  llvm::SmallString<32> scratch;
  scratch += firstPiece.str();
  auto firstName = argNames[0];
  if (!firstName.empty()) {
    // If the first argument name doesn't start with a preposition, and the
    // method name doesn't end with a preposition, add "with".
    if (getPrepositionKind(camel_case::getFirstWord(firstName.str()))
          == PK_None &&
        getPrepositionKind(camel_case::getLastWord(firstPiece.str()))
          == PK_None) {
      camel_case::appendSentenceCase(scratch, "With");
    }

    camel_case::appendSentenceCase(scratch, firstName.str());
    firstPiece = ctx.getIdentifier(scratch);
    didStringManipulation = true;
  }

  // For every element beyond the first, add a selector component.
  SmallVector<Identifier, 4> argumentNames;
  argumentNames.push_back(firstPiece);
  argumentNames.append(argNames.begin() + 1, argNames.end());

  // Form the result.
  auto result = ObjCSelector(ctx, argumentNames.size(), argumentNames);

  // If we did any string manipulation, cache the result. We don't want to
  // do that again.
  if (didStringManipulation && objc)
    const_cast<ObjCAttr *>(objc)->setName(result, /*implicit=*/true);

  return result;
}

SourceRange FuncDecl::getSourceRange() const {
  SourceLoc StartLoc = getStartLoc();
  if (StartLoc.isInvalid()) return SourceRange();

  if (getBodyKind() == BodyKind::Unparsed ||
      getBodyKind() == BodyKind::Skipped)
    return { StartLoc, BodyRange.End };

  if (auto *B = getBody())
    return { StartLoc, B->getEndLoc() };
  if (getBodyResultTypeLoc().hasLocation() &&
      getBodyResultTypeLoc().getSourceRange().End.isValid())
    return { StartLoc, getBodyResultTypeLoc().getSourceRange().End };
  const Pattern *LastPat = getBodyParamPatterns().back();
  if (!LastPat->isImplicit())
    return { StartLoc, LastPat->getEndLoc() };
  return StartLoc;
}

SourceRange EnumElementDecl::getSourceRange() const {
  if (RawValueExpr && !RawValueExpr->isImplicit())
    return {getStartLoc(), RawValueExpr->getEndLoc()};
  if (ArgumentType.hasLocation())
    return {getStartLoc(), ArgumentType.getSourceRange().End};
  return {getStartLoc(), getNameLoc()};
}

Type EnumElementDecl::getArgumentInterfaceType() const {
  if (!hasArgumentType())
    return nullptr;

  auto interfaceType = getInterfaceType();
  if (interfaceType->is<ErrorType>()) {
    return interfaceType;
  }

  auto funcTy = interfaceType->castTo<AnyFunctionType>();
  funcTy = funcTy->getResult()->castTo<AnyFunctionType>();
  return funcTy->getInput();
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

Type ConstructorDecl::getArgumentType() const {
  Type ArgTy = getType();
  ArgTy = ArgTy->castTo<AnyFunctionType>()->getResult();
  ArgTy = ArgTy->castTo<AnyFunctionType>()->getInput();
  return ArgTy;
}

Type ConstructorDecl::getResultType() const {
  Type ArgTy = getType();
  ArgTy = ArgTy->castTo<AnyFunctionType>()->getResult();
  ArgTy = ArgTy->castTo<AnyFunctionType>()->getResult();
  return ArgTy;
}

ObjCSelector ConstructorDecl::getObjCSelector() const {
  // If there is an @objc attribute with a name, use that name.
  auto objc = getAttrs().getAttribute<ObjCAttr>();
  if (objc) {
    if (auto name = objc->getName())
      return *name;
  }

  auto &ctx = getASTContext();

  // If there are no parameters, this is just 'init()'.
  auto argNames = getFullName().getArgumentNames();
  if (argNames.size() == 0)
    return ObjCSelector(ctx, 0, ctx.Id_init);

  // The first field is special: we uppercase the name.
  bool didStringManipulation = false;
  SmallVector<Identifier, 4> selectorPieces;
  auto firstName = argNames[0];
  if (firstName.empty())
    selectorPieces.push_back(ctx.Id_init);
  else {
    llvm::SmallString<16> scratch;
    scratch += "init";

    // If the first argument name doesn't start with a preposition, add "with".
    if (getPrepositionKind(camel_case::getFirstWord(firstName.str()))
          == PK_None) {
      camel_case::appendSentenceCase(scratch, "With");
    }

    camel_case::appendSentenceCase(scratch, firstName.str());
    selectorPieces.push_back(ctx.getIdentifier(scratch));
    didStringManipulation = true;
  }

  // If we have just one field, check whether this is actually a
  // nullary selector that we mapped to a single-element initializer to catch
  // the name after "init".
  const TuplePattern *tuple = nullptr;
  if (argNames.size() == 1 && 
      (tuple = dyn_cast<TuplePattern>(getBodyParamPatterns()[1]))) {
    const auto &elt = tuple->getFields()[0];
    auto pattern = elt.getPattern()->getSemanticsProvidingPattern();
    if (pattern->hasType()) {
      if (pattern->getType()->isEqual(TupleType::getEmpty(ctx))) {
        auto result = ObjCSelector(ctx, 0, selectorPieces[0]);

        // Cache the name in the 'objc' attribute. We don't want to perform
        // string manipulation again.
        if (objc)
          const_cast<ObjCAttr *>(objc)->setName(result, /*implicit=*/true);
        return result;
      }
    } else {
      // If we couldn't check the type, don't cache the result.
      didStringManipulation = false;
    }
  }

  // For every remaining element, add a selector component.
  selectorPieces.append(argNames.begin() + 1, argNames.end());
  auto result = ObjCSelector(ctx, selectorPieces.size(), selectorPieces);

  // Cache the name in the 'objc' attribute. We don't want to perform
  // string manipulation again.
  if (objc && didStringManipulation)
    const_cast<ObjCAttr *>(objc)->setName(result, /*implicit=*/true);

  return result;
}

Type ConstructorDecl::getInitializerInterfaceType() {
  if (!InitializerInterfaceType) {
    assert((!InitializerType || !InitializerType->is<PolymorphicFunctionType>())
           && "polymorphic function type is invalid interface type");
    
    // Don't cache type variable types.
    if (InitializerType->hasTypeVariable())
      return InitializerType;
    
    InitializerInterfaceType = InitializerType;
  }
  
  return InitializerInterfaceType;
}

void ConstructorDecl::setInitializerInterfaceType(Type t) {
  assert(!t->is<PolymorphicFunctionType>()
         && "polymorphic function type is invalid interface type");
  InitializerInterfaceType = t;
}

ConstructorDecl::BodyInitKind
ConstructorDecl::getDelegatingOrChainedInitKind(DiagnosticEngine *diags,
                                                ApplyExpr **init) {
  assert(hasBody() && "Constructor does not have a definition");

  if (init)
    *init = nullptr;

  // If we already computed the result, return it.
  if (ConstructorDeclBits.ComputedBodyInitKind) {
    return static_cast<BodyInitKind>(
             ConstructorDeclBits.ComputedBodyInitKind - 1);
  }


  struct FindReferenceToInitializer : ASTWalker {
    BodyInitKind Kind = BodyInitKind::None;
    ApplyExpr *InitExpr = nullptr;
    DiagnosticEngine *Diags;

    FindReferenceToInitializer(DiagnosticEngine *diags) : Diags(diags) { }

    std::pair<bool, Expr*> walkToExprPre(Expr *E) override {
      // Don't walk into closures.
      if (isa<ClosureExpr>(E))
        return { false, E };

      // Look for calls of a constructor.
      auto apply = dyn_cast<ApplyExpr>(E);
      if (!apply)
        return { true, E };

      auto Callee = apply->getFn()->getSemanticsProvidingExpr();
      
      BodyInitKind myKind;

      if (isa<OtherConstructorDeclRefExpr>(Callee)) {
        if (apply->getArg()->isSuperExpr())
          myKind = BodyInitKind::Chained;
        else
          myKind = BodyInitKind::Delegating;
      } else if (auto *UCE = dyn_cast<UnresolvedConstructorExpr>(Callee)) {
        if (UCE->getSubExpr()->isSuperExpr())
          myKind = BodyInitKind::Chained;
        else
          myKind = BodyInitKind::Delegating;
      } else {
        // Not a constructor call.
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
  } finder(diags);
  getBody()->walk(finder);

  // get the kind out of the finder.
  auto Kind = finder.Kind;


  // If we didn't find any delegating or chained initializers, check whether
  // the initializer was explicitly marked 'convenience'.
  if (Kind == BodyInitKind::None && getAttrs().hasAttribute<ConvenienceAttr>())
    Kind = BodyInitKind::Delegating;

  // If wes till don't know, check whether we have a class with a superclass: it
  // gets an implicit chained initializer.
  if (Kind == BodyInitKind::None) {
    if (auto classDecl = getDeclContext()->getDeclaredTypeInContext()
                           ->getClassOrBoundGenericClass()) {
      if (classDecl->getSuperclass())
        Kind = BodyInitKind::ImplicitChained;
    }
  }

  // Cache the result if it is trustworthy.
  if (diags) {
    ConstructorDeclBits.ComputedBodyInitKind = static_cast<unsigned>(Kind) + 1;
    if (init)
      *init = finder.InitExpr;
  }

  return Kind;
}

ObjCSelector DestructorDecl::getObjCSelector() const {
  auto &ctx = getASTContext();
  return ObjCSelector(ctx, 0, ctx.Id_dealloc);
}

SourceRange DestructorDecl::getSourceRange() const {
  if (getBodyKind() == BodyKind::Unparsed ||
      getBodyKind() == BodyKind::Skipped)
    return { getDestructorLoc(), BodyRange.End };

  if (getBodyKind() == BodyKind::None)
    return getDestructorLoc();

  return { getDestructorLoc(), getBody()->getEndLoc() };
}
