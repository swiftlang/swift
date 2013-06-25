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
#include "swift/AST/AST.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

/// getASTContext - Return the ASTContext for a specified DeclContetx by
/// walking up to the translation unit and returning its ASTContext.
ASTContext &DeclContext::getASTContext() {
  if (Module *M = dyn_cast<Module>(this))
    return M->Ctx;
  
  return getParent()->getASTContext();
}

Type DeclContext::getDeclaredTypeOfContext() const {
  switch (getContextKind()) {
  case DeclContextKind::BuiltinModule:
  case DeclContextKind::CapturingExpr:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::TranslationUnit:
  case DeclContextKind::SerializedModule:
  case DeclContextKind::ClangModule:
  case DeclContextKind::ConstructorDecl:
  case DeclContextKind::DestructorDecl:
    return Type();
    
  case DeclContextKind::ExtensionDecl:
    return cast<ExtensionDecl>(this)->getExtendedType();
    
  case DeclContextKind::NominalTypeDecl:
    return cast<NominalTypeDecl>(this)->getDeclaredType();
  }
}

Type DeclContext::getDeclaredTypeInContext() {
  switch (getContextKind()) {
    case DeclContextKind::BuiltinModule:
    case DeclContextKind::CapturingExpr:
    case DeclContextKind::TopLevelCodeDecl:
    case DeclContextKind::TranslationUnit:
    case DeclContextKind::SerializedModule:
    case DeclContextKind::ClangModule:
    case DeclContextKind::ConstructorDecl:
    case DeclContextKind::DestructorDecl:
      return Type();

    case DeclContextKind::ExtensionDecl: {
      auto ty = cast<ExtensionDecl>(this)->getExtendedType();
      if (auto unbound = ty->getAs<UnboundGenericType>())
        return unbound->getDecl()->getDeclaredTypeInContext();

      if (auto nominal = ty->getAs<NominalType>())
        return nominal->getDecl()->getDeclaredTypeInContext();

      return Type();
    }

    case DeclContextKind::NominalTypeDecl:
      return cast<NominalTypeDecl>(this)->getDeclaredTypeInContext();
  }

}

GenericParamList *DeclContext::getGenericParamsOfContext() const {
  switch (getContextKind()) {
    case DeclContextKind::BuiltinModule:
    case DeclContextKind::TopLevelCodeDecl:
    case DeclContextKind::TranslationUnit:
    case DeclContextKind::SerializedModule:
    case DeclContextKind::ClangModule:
      return nullptr;

    case DeclContextKind::CapturingExpr: {
      if (auto funcE = dyn_cast<FuncExpr>(this)) {
        if (auto funcD = funcE->getDecl()) {
          if (auto gp = funcD->getGenericParams()) {
            return gp;
          }

          return funcD->getDeclContext()->getGenericParamsOfContext();
        }
      }

      return nullptr;
    }
      
    case DeclContextKind::ConstructorDecl: {
      auto constructor = cast<ConstructorDecl>(this);
      if (auto gp = constructor->getGenericParams())
        return gp;

      return constructor->getDeclContext()->getGenericParamsOfContext();
    }

    case DeclContextKind::DestructorDecl:
      return cast<DestructorDecl>(this)->getDeclContext()
               ->getGenericParamsOfContext();

    case DeclContextKind::NominalTypeDecl: {
      auto nominal = cast<NominalTypeDecl>(this);
      if (auto gp = nominal->getGenericParams())
        return gp;

      return nominal->getDeclContext()->getGenericParamsOfContext();
    }

    case DeclContextKind::ExtensionDecl: {
      auto extension = cast<ExtensionDecl>(this);
      auto extendedType = extension->getExtendedType();
      if (auto unbound = extendedType->getAs<UnboundGenericType>()) {
        return unbound->getDecl()->getGenericParams();
      }
      if (auto nominalTy = extendedType->getAs<NominalType>()) {
        auto nominalDecl = nominalTy->getDecl();
        if (auto gp = nominalDecl->getGenericParams())
          return gp;
        return nominalDecl->getDeclContext()->getGenericParamsOfContext();
      }

      // FIXME: Eventually, extensions will be able to have their own
      // generic parameters.

      return nullptr;
    }
  }

  llvm_unreachable("Unhandled declaration context kind");
}

// Only allow allocation of Decls using the allocator in ASTContext.
void *Decl::operator new(size_t Bytes, ASTContext &C,
                         unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

// Only allow allocation of DeclContext using the allocator in ASTContext.
void *DeclContext::operator new(size_t Bytes, ASTContext &C,
                                unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

// Only allow allocation of Modules using the allocator in ASTContext.
void *Module::operator new(size_t Bytes, ASTContext &C,
                           unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

Module *Decl::getModuleContext() const {
  DeclContext *dc = getDeclContext();
  while (!dc->isModuleContext())
    dc = dc->getParent();
  return cast<Module>(dc);
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

ClangNode Decl::getClangNodeSlow() {
  return getASTContext().getClangNode(this);
}

void Decl::setClangNode(ClangNode node) {
  DeclBits.FromClang = true;
  getASTContext().setClangNode(this, node);
}

GenericParamList::GenericParamList(SourceLoc LAngleLoc,
                                   ArrayRef<GenericParam> Params,
                                   SourceLoc RequiresLoc,
                                   MutableArrayRef<Requirement> Requirements,
                                   SourceLoc RAngleLoc)
  : Brackets(LAngleLoc, RAngleLoc), NumParams(Params.size()),
    RequiresLoc(RequiresLoc), Requirements(Requirements),
    OuterParameters(nullptr)
{
  memcpy(this + 1, Params.data(), NumParams * sizeof(GenericParam));
}

GenericParamList *GenericParamList::create(ASTContext &Context,
                                           SourceLoc LAngleLoc,
                                           ArrayRef<GenericParam> Params,
                                           SourceLoc RAngleLoc) {
  unsigned Size = sizeof(GenericParamList)
                + sizeof(GenericParam) * Params.size();
  void *Mem = Context.Allocate(Size, alignof(GenericParamList));
  return new (Mem) GenericParamList(LAngleLoc, Params, SourceLoc(),
                                    MutableArrayRef<Requirement>(),
                                    RAngleLoc);
}

GenericParamList *
GenericParamList::create(ASTContext &Context,
                         SourceLoc LAngleLoc,
                         ArrayRef<GenericParam> Params,
                         SourceLoc RequiresLoc,
                         MutableArrayRef<Requirement> Requirements,
                         SourceLoc RAngleLoc) {
  unsigned Size = sizeof(GenericParamList)
                + sizeof(GenericParam) * Params.size();
  void *Mem = Context.Allocate(Size, alignof(GenericParamList));
  return new (Mem) GenericParamList(LAngleLoc, Params,
                                    RequiresLoc,
                                    Context.AllocateCopy(Requirements),
                                    RAngleLoc);
}

ImportDecl *ImportDecl::create(ASTContext &Ctx, DeclContext *DC,
                               SourceLoc ImportLoc,
                               ArrayRef<AccessPathElement> Path) {
  void *buffer = Ctx.Allocate(sizeof(ImportDecl) +
                              Path.size() * sizeof(AccessPathElement),
                              alignof(ImportDecl));
  return new (buffer) ImportDecl(DC, ImportLoc, Path);
}

ImportDecl::ImportDecl(DeclContext *DC, SourceLoc ImportLoc,
                       ArrayRef<AccessPathElement> Path)
  : Decl(DeclKind::Import, DC), ImportLoc(ImportLoc),
    NumPathElements(Path.size()) {
  memcpy(getPathBuffer(), Path.data(), Path.size() * sizeof(AccessPathElement));
}

/// \brief Gather all of the protocols from the set of inherited types.
static void gatherProtocols(ArrayRef<TypeLoc> inherited,
                            SmallVectorImpl<ProtocolDecl *> &allProtocols) {
  llvm::SmallPtrSet<ProtocolDecl *, 4> knownProtocols;

  for (auto inherited : inherited) {
    SmallVector<ProtocolDecl *, 4> protocols;
    if (inherited.getType()->isExistentialType(protocols)) {
      for (auto proto : protocols) {
        if (knownProtocols.insert(proto))
          allProtocols.push_back(proto);
      }
    }
  }
}

ArrayRef<ProtocolDecl *> ExtensionDecl::getProtocols() {
  if (!Protocols.empty())
    return Protocols;

  // Gather the complete set of protocols.
  // FIXME: Would be nice to also gather the protocol from extensions of this
  // type...
  SmallVector<ProtocolDecl *, 4> foundProtocols;
  gatherProtocols(Inherited, foundProtocols);
  if (foundProtocols.empty())
    return Protocols;

  // Copy the set of protocols to the heap so we don't compute it again.
  Protocols = getASTContext().AllocateCopy(foundProtocols);
  return Protocols;
}

SourceRange PatternBindingDecl::getSourceRange() const {
  if (Init && !Init->isImplicit())
    return { VarLoc, Init->getSourceRange().End };
  return { VarLoc, Pat->getSourceRange().End };
}

SourceLoc TopLevelCodeDecl::getStartLoc() const {
  return Body->getStartLoc();
}

SourceRange TopLevelCodeDecl::getSourceRange() const {
  return Body->getSourceRange();
}

/// getTypeOfReference - Return the full type judgement for a non-member
/// reference to this value.
Type ValueDecl::getTypeOfReference() const {
  // TODO: when the old type checker dies, set the NonSettable bit here instead
  // of in TypeCheckConstraints.cpp.
  if (isReferencedAsLValue()) {
    if (LValueType *LVT = Ty->getAs<LValueType>())
      return LValueType::get(LVT->getObjectType(),
                             LValueType::Qual::DefaultForVar, getASTContext());
    return LValueType::get(Ty,
                           LValueType::Qual::DefaultForVar, getASTContext());
  }

  return Ty;
}

/// isDefinition - Return true if this is a definition of a decl, not a
/// forward declaration (e.g. of a function) that is implemented outside of
/// the swift code.
bool ValueDecl::isDefinition() const {
  switch (getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::Subscript:
  case DeclKind::TopLevelCode:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    llvm_unreachable("non-value decls shouldn't get here");
      
  case DeclKind::Func:
    return cast<FuncDecl>(this)->getBody() != 0;

  case DeclKind::Var:
  case DeclKind::OneOf:
  case DeclKind::OneOfElement:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::TypeAlias:
  case DeclKind::Protocol:
    return true;
  }
}

bool ValueDecl::isInstanceMember() const {
  DeclContext *DC = getDeclContext();
  if (!DC->isTypeContext())
    return false;

  switch (getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    llvm_unreachable("Not a ValueDecl");

  case DeclKind::Class:
  case DeclKind::OneOf:
  case DeclKind::Protocol:
  case DeclKind::Struct:
  case DeclKind::TypeAlias:
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

  case DeclKind::OneOfElement:
    // oneof elements are not instance members.
    return false;

  case DeclKind::Subscript:
    // Subscripts are always instance members.
    return true;

  case DeclKind::Var:
    // Variables are always instance variables.
    // FIXME: Until we get static variables.
    return true;
  }
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

Type TypeDecl::getDeclaredType() const {
  if (auto TAD = dyn_cast<TypeAliasDecl>(this))
    return TAD->getAliasType();
  return cast<NominalTypeDecl>(this)->getDeclaredType();
}

ArrayRef<ProtocolDecl *> TypeDecl::getProtocols() {
  if (!Protocols.empty())
    return Protocols;

  // Gather the complete set of protocols.
  // FIXME: Would be nice to also gather the protocol from extensions of this
  // type...
  SmallVector<ProtocolDecl *, 4> foundProtocols;
  gatherProtocols(Inherited, foundProtocols);
  if (foundProtocols.empty())
    return Protocols;

  // Copy the set of protocols to the heap so we don't compute it again.
  Protocols = getASTContext().AllocateCopy(foundProtocols);
  return Protocols;
}

Type NominalTypeDecl::getDeclaredTypeInContext() {
  if (DeclaredTyInContext)
    return DeclaredTyInContext;
  
  Type Ty = getDeclaredType();
  if (UnboundGenericType *UGT = Ty->getAs<UnboundGenericType>()) {
    // If we have an unbound generic type, bind the type to the archetypes
    // in the type's definition.
    NominalTypeDecl *D = UGT->getDecl();
    SmallVector<Type, 4> GenericArgs;
    for (auto Param : *D->getGenericParams())
      GenericArgs.push_back(Param.getAsTypeParam()->getDeclaredType());
    Ty = BoundGenericType::get(D, getDeclContext()->getDeclaredTypeInContext(),
                               GenericArgs);
  }
  DeclaredTyInContext = Ty;
  return DeclaredTyInContext;
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
  assert(!extension->NextExtension && "Already added extension");

  // First extension; set both first and last.
  if (!FirstExtension) {
    FirstExtension = extension;
    LastExtension = extension;
    return;
  }

  // Add to the end of the list.
  LastExtension->NextExtension = extension;
  LastExtension = extension;
}

TypeAliasDecl::TypeAliasDecl(SourceLoc TypeAliasLoc, Identifier Name,
                             SourceLoc NameLoc, TypeLoc UnderlyingTy,
                             DeclContext *DC,
                             MutableArrayRef<TypeLoc> Inherited)
  : TypeDecl(DeclKind::TypeAlias, DC, Name, Inherited, Type()),
    TypeAliasLoc(TypeAliasLoc), NameLoc(NameLoc),
    UnderlyingTy(UnderlyingTy)
{
  // Set the type of the TypeAlias to the right MetaTypeType.
  ASTContext &Ctx = getASTContext();
  AliasTy = new (Ctx, AllocationArena::Permanent) NameAliasType(this);
  setType(MetaTypeType::get(AliasTy, Ctx));

  TypeAliasDeclBits.GenericParameter = false;
}

SourceRange TypeAliasDecl::getSourceRange() const {
  if (UnderlyingTy.hasLocation())
    return { TypeAliasLoc, UnderlyingTy.getSourceRange().End };
  // FIXME: Inherits clauses
  return { TypeAliasLoc, NameLoc };
}

OneOfDecl::OneOfDecl(SourceLoc OneOfLoc, Identifier Name, SourceLoc NameLoc,
                     MutableArrayRef<TypeLoc> Inherited,
                     GenericParamList *GenericParams, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::OneOf, Parent, Name, Inherited, GenericParams),
    OneOfLoc(OneOfLoc), NameLoc(NameLoc) {
  // Compute the associated type for this OneOfDecl.
  ASTContext &Ctx = Parent->getASTContext();
  if (!GenericParams)
    DeclaredTy = OneOfType::get(this, Parent->getDeclaredTypeInContext(), Ctx);
  else
    DeclaredTy = UnboundGenericType::get(this,
                                         Parent->getDeclaredTypeInContext(),
                                         Ctx);
  // Set the type of the OneOfDecl to the right MetaTypeType.
  setType(MetaTypeType::get(DeclaredTy, Ctx));
}

StructDecl::StructDecl(SourceLoc StructLoc, Identifier Name, SourceLoc NameLoc,
                       MutableArrayRef<TypeLoc> Inherited,
                       GenericParamList *GenericParams, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::Struct, Parent, Name, Inherited, GenericParams),
    StructLoc(StructLoc), NameLoc(NameLoc){
  // Compute the associated type for this StructDecl.
  ASTContext &Ctx = Parent->getASTContext();
  if (!GenericParams)
    DeclaredTy = StructType::get(this, Parent->getDeclaredTypeInContext(), Ctx);
  else
    DeclaredTy = UnboundGenericType::get(this,
                                         Parent->getDeclaredTypeInContext(),
                                         Ctx);
  // Set the type of the StructDecl to the right MetaTypeType.
  setType(MetaTypeType::get(DeclaredTy, Ctx));
}

ClassDecl::ClassDecl(SourceLoc ClassLoc, Identifier Name, SourceLoc NameLoc,
                     MutableArrayRef<TypeLoc> Inherited,
                     GenericParamList *GenericParams, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::Class, Parent, Name, Inherited, GenericParams),
    ClassLoc(ClassLoc), NameLoc(NameLoc) {
  // Compute the associated type for this ClassDecl.
  ASTContext &Ctx = Parent->getASTContext();
  if (!GenericParams)
    DeclaredTy = ClassType::get(this, Parent->getDeclaredTypeInContext(), Ctx);
  else
    DeclaredTy = UnboundGenericType::get(this,
                                         Parent->getDeclaredTypeInContext(),
                                         Ctx);
  // Set the type of the ClassDecl to the right MetaTypeType.
  setType(MetaTypeType::get(DeclaredTy, Ctx));
}


OneOfElementDecl *OneOfDecl::getElement(Identifier Name) const {
  // FIXME: Linear search is not great for large oneof decls.
  for (Decl *D : getMembers())
    if (OneOfElementDecl *Elt = dyn_cast<OneOfElementDecl>(D))
      if (Elt->getName() == Name)
        return Elt;
  return 0;
}

ProtocolDecl::ProtocolDecl(DeclContext *DC, SourceLoc ProtocolLoc,
                           SourceLoc NameLoc, Identifier Name,
                           MutableArrayRef<TypeLoc> Inherited)
  : NominalTypeDecl(DeclKind::Protocol, DC, Name, Inherited, nullptr),
    ProtocolLoc(ProtocolLoc), NameLoc(NameLoc)
{
  // Compute the associated type for this ClassDecl.
  ASTContext &Ctx = DC->getASTContext();
  DeclaredTy = new (Ctx, AllocationArena::Permanent) ProtocolType(this, Ctx);
  // Set the type of the ProtocolDecl to the right MetaTypeType.
  setType(MetaTypeType::get(DeclaredTy, Ctx));
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
    
    for (auto Inherited : Current->getInherited()) {
      SmallVector<ProtocolDecl *, 4> InheritedDecls;
      if (Inherited.getType()->isExistentialType(InheritedDecls)) {
        for (auto InheritedProto : InheritedDecls) {
          if (InheritedProto == Super)
            return true;
            
          else if (Visited.insert(InheritedProto))
            Stack.push_back(InheritedProto);
        }
      }
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
    
    for (auto IType : Current->getInherited()) {
      SmallVector<ProtocolDecl *, 4> InheritedDecls;
      if (IType.getType()->isExistentialType(InheritedDecls)) {
        for (auto InheritedProto : InheritedDecls) {
          if (Inherited.insert(InheritedProto))
            Stack.push_back(InheritedProto);
        }
      }
    }
  }
}

bool ProtocolDecl::requiresClass() {
  return RequiresClass.cache([&] {
    // If we have the [class_protocol] attribute, we're trivially a class
    // protocol.
    if (getAttrs().isClassProtocol())
      return true;
    
    // Check inherited protocols for class-ness.
    for (TypeLoc inherited : getInherited()) {
      SmallVector<ProtocolDecl*, 2> inheritedProtos;
      if (inherited.getType()->isExistentialType(inheritedProtos))
        for (auto *proto : inheritedProtos)
          if (proto->requiresClass())
            return true;
    }
    
    return false;    
  });
}

TypeAliasDecl *ProtocolDecl::getThis() const {
  for (auto member : getMembers()) {
    if (auto assocType = dyn_cast<TypeAliasDecl>(member))
      if (assocType->getName().str() == "This")
        return assocType;
  }
  llvm_unreachable("No 'This' associated type?");
}

void VarDecl::setProperty(ASTContext &Context, SourceLoc LBraceLoc,
                          FuncDecl *Get, FuncDecl *Set, SourceLoc RBraceLoc) {
  assert(!GetSet && "Variable is already a property?");
  void *Mem = Context.Allocate(sizeof(GetSetRecord), alignof(GetSetRecord));
  GetSet = new (Mem) GetSetRecord;
  GetSet->Braces = SourceRange(LBraceLoc, RBraceLoc);
  GetSet->Get = Get;
  GetSet->Set = Set;
  
  if (Get)
    Get->makeGetter(this);
  if (Set)
    Set->makeSetter(this);
}

VarDecl *FuncDecl::getImplicitThisDecl() const {
  return Body->getImplicitThisDecl();
}

/// getNaturalArgumentCount - Returns the "natural" number of
/// argument clauses taken by this function.
unsigned FuncDecl::getNaturalArgumentCount() const {
  return getBody()->getNaturalArgumentCount();
}

ArrayRef<ValueDecl*> FuncDecl::getCaptures() const {
  if (Body)
    return Body->getCaptures();
  else
    return {};
}

/// getExtensionType - If this is a method in a type extension for some type,
/// return that type, otherwise return Type().
Type FuncDecl::getExtensionType() const {
  DeclContext *DC = getDeclContext();
  switch (DC->getContextKind()) {
  case DeclContextKind::TranslationUnit:
  case DeclContextKind::BuiltinModule:
  case DeclContextKind::SerializedModule:
  case DeclContextKind::ClangModule:
  case DeclContextKind::CapturingExpr:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::ConstructorDecl:
  case DeclContextKind::DestructorDecl:
    return Type();

  case DeclContextKind::NominalTypeDecl:
    return cast<NominalTypeDecl>(DC)->getDeclaredType();
  case DeclContextKind::ExtensionDecl:
    return cast<ExtensionDecl>(DC)->getExtendedType();
  }
  llvm_unreachable("bad context kind");
}


/// computeThisType - If this is a method in a type extension for some type,
/// compute and return the type to be used for the 'this' argument of the
/// type (which varies based on whether the extended type is a reference type
/// or not), or an empty Type() if no 'this' argument should exist.  This can
/// only be used after name binding has resolved types.
Type FuncDecl::computeThisType(GenericParamList **OuterGenericParams) const {
  if (OuterGenericParams)
    *OuterGenericParams = nullptr;
  
  Type ContainerType = getExtensionType();
  if (ContainerType.isNull()) return ContainerType;

  // For a protocol, the type of 'this' is the associated type 'This', not
  // the protocol itself.
  if (auto Protocol = ContainerType->getAs<ProtocolType>()) {
    TypeAliasDecl *This = 0;
    for (auto Member : Protocol->getDecl()->getMembers()) {
      This = dyn_cast<TypeAliasDecl>(Member);
      if (!This)
        continue;

      // FIXME: Sane way to identify 'This'?
      if (This->getName().str() == "This")
        break;

      This = nullptr;
    }

    assert(This && "Missing 'This' associated type in protocol");
    ContainerType = This->getDeclaredType();
  }

  if (UnboundGenericType *UGT = ContainerType->getAs<UnboundGenericType>()) {
    // If we have an unbound generic type, bind the type to the archetypes
    // in the type's definition.
    NominalTypeDecl *D = UGT->getDecl();
    ContainerType = getDeclContext()->getDeclaredTypeInContext();

    if (OuterGenericParams)
      *OuterGenericParams = D->getGenericParams();
  } else if (OuterGenericParams) {
    *OuterGenericParams = getDeclContext()->getGenericParamsOfContext();
  }

  // 'static' functions have 'this' of type metatype<T>.
  if (isStatic())
    return MetaTypeType::get(ContainerType, getASTContext());

  if (ContainerType->hasReferenceSemantics())
    return ContainerType;

  // 'this' is accepts implicit l-values.
  return LValueType::get(ContainerType, LValueType::Qual::DefaultForVar,
                         getASTContext());
}

bool FuncDecl::isUnaryOperator() const {
  if (!isOperator())
    return false;
  
  unsigned opArgIndex = isa<ProtocolDecl>(getDeclContext()) ? 1 : 0;
  
  auto *argTuple
    = dyn_cast<TuplePattern>(getBody()->getArgParamPatterns()[opArgIndex]);
  if (!argTuple)
    return true;

  return argTuple->getNumFields() == 1
    && !argTuple->getFields()[0].isVararg();
}

bool FuncDecl::isBinaryOperator() const {
  if (!isOperator())
    return false;
  
  unsigned opArgIndex = isa<ProtocolDecl>(getDeclContext()) ? 1 : 0;
  
  auto *argTuple
    = dyn_cast<TuplePattern>(getBody()->getArgParamPatterns()[opArgIndex]);
  if (!argTuple)
    return false;
  
  return argTuple->getNumFields() == 2
    || (argTuple->getNumFields() == 1 && argTuple->getFields()[0].isVararg());
}

StringRef VarDecl::getObjCGetterSelector(llvm::SmallVectorImpl<char> &buffer)
  const
{
  llvm::raw_svector_ostream out(buffer);

  // The getter selector is the property name itself.
  // FIXME: 'is' prefix for boolean properties?
  out << getName().str();
  return out.str();
}
  
StringRef VarDecl::getObjCSetterSelector(llvm::SmallVectorImpl<char> &buffer)
  const
{
  llvm::raw_svector_ostream out(buffer);

  // The setter selector for, e.g., 'fooBar' is 'setFooBar:', with the
  // property name capitalized and preceded by 'set'.
  StringRef name = getName().str();
  assert(name.size() >= 1 && "empty var name?!");
    
  out << "set" << char(toupper(name[0])) << name.slice(1, name.size()) << ':';
  return out.str();
}

/// Produce the selector for this "Objective-C method" in the given buffer.
StringRef FuncDecl::getObjCSelector(llvm::SmallVectorImpl<char> &buffer) const {
  assert(buffer.empty());
  
  // Property accessors should go through a different path.
  assert(!isGetterOrSetter());

  llvm::raw_svector_ostream out(buffer);

  // Start with the method name.
  out << getName().str();

  // We should always have exactly two levels of argument pattern.
  auto argPatterns = getBody()->getArgParamPatterns();
  assert(argPatterns.size() == 2);
  const Pattern *pattern = argPatterns[1];
  auto tuple = dyn_cast<TuplePattern>(pattern);

  // If it's an empty tuple pattern, it's a nullary selector.
  if (tuple && tuple->getNumFields() == 0)
    return StringRef(buffer.data(), buffer.size());

  // Otherwise, it's at least a unary selector.
  out << ':';

  // If it's a unary selector, we're done.
  if (!tuple) {
    return out.str();
  }

  // For every element except the first, add a selector component.
  for (auto &elt : tuple->getFields().slice(1)) {
    auto eltPattern = elt.getPattern()->getSemanticsProvidingPattern();

    // Add a label to the selector component if there's a tag.
    if (auto named = dyn_cast<NamedPattern>(eltPattern)) {
      out << named->getBoundName().str();
    }

    // Add the colon regardless.  Yes, this can sometimes create a
    // component that's just a colon, and that's actually a legal
    // selector.
    out << ':';
  }

  return out.str();
}

SourceRange FuncDecl::getSourceRange() const {
  return Body->getSourceRange();
}

SourceRange OneOfElementDecl::getSourceRange() const {
  if (ArgumentType.hasLocation())
    return { IdentifierLoc, ArgumentType.getSourceRange().End };
  return IdentifierLoc;
}

SourceLoc SubscriptDecl::getLoc() const {
  return Indices->getStartLoc();
}

SourceRange SubscriptDecl::getSourceRange() const {
  if (Braces.isValid())
    return { SubscriptLoc, Braces.End };
  return { SubscriptLoc, ElementTy.getSourceRange().End };
}

SourceLoc ConstructorDecl::getLoc() const {
  return Arguments->getStartLoc();
}

SourceRange ConstructorDecl::getSourceRange() const {
  if (!Body || !Body->getEndLoc().isValid()) {
    const DeclContext *DC = getDeclContext();
    switch (DC->getContextKind()) {
    case DeclContextKind::ExtensionDecl:
      return cast<ExtensionDecl>(DC)->getLoc();
    case DeclContextKind::NominalTypeDecl:
      return cast<NominalTypeDecl>(DC)->getLoc();
    default:
      llvm_unreachable("Unhandled decl kind");
    }
  }
  return { ConstructorLoc, Body->getEndLoc() };
}

Type
ConstructorDecl::computeThisType(GenericParamList **OuterGenericParams) const {
  Type ContainerType = getDeclContext()->getDeclaredTypeOfContext();

  if (UnboundGenericType *UGT = ContainerType->getAs<UnboundGenericType>()) {
    // If we have an unbound generic type, bind the type to the archetypes
    // in the type's definition.
    NominalTypeDecl *D = UGT->getDecl();
    ContainerType = getDeclContext()->getDeclaredTypeInContext();

    if (OuterGenericParams)
      *OuterGenericParams = D->getGenericParams();
  } else if (OuterGenericParams) {
    *OuterGenericParams = getDeclContext()->getGenericParamsOfContext();
  }

  return ContainerType;
}

Type ConstructorDecl::getArgumentType() const {
  Type ArgTy = getType();
  ArgTy = ArgTy->castTo<AnyFunctionType>()->getResult();
  ArgTy = ArgTy->castTo<AnyFunctionType>()->getInput();
  return ArgTy;
}

/// Produce the selector for this "Objective-C method" in the given buffer.
StringRef
ConstructorDecl::getObjCSelector(llvm::SmallVectorImpl<char> &buffer) const {
  assert(buffer.empty());

  llvm::raw_svector_ostream out(buffer);


  // If it's an empty tuple pattern, it's the nullary selector "init".
  // FIXME: This leaves us without a way to describe "new".
  auto tuple = dyn_cast<TuplePattern>(Arguments);
  if (tuple && tuple->getNumFields() == 0) {
    out << "init";
    out.flush();
    return StringRef(buffer.data(), buffer.size());
  }

  // If it's not a tuple at all, it's the unary selector "init:".
  // FIXME: Diagnose this?
  if (!tuple) {
    out << "init:";
    out.flush();
    return StringRef(buffer.data(), buffer.size());
  }

  // For every element, add a selector component.
  for (auto &elt : tuple->getFields()) {
    auto eltPattern = elt.getPattern()->getSemanticsProvidingPattern();

    // Add a label to the selector component if there's a tag.
    if (auto named = dyn_cast<NamedPattern>(eltPattern)) {
      out << named->getBoundName().str();
    }

    // Add the colon regardless.  Yes, this can sometimes create a
    // component that's just a colon, and that's actually a legal
    // selector.
    out << ':';
  }

  return StringRef(buffer.data(), buffer.size());
}

Type
DestructorDecl::computeThisType(GenericParamList **OuterGenericParams) const {
  Type ContainerType = getDeclContext()->getDeclaredTypeOfContext();

  if (UnboundGenericType *UGT = ContainerType->getAs<UnboundGenericType>()) {
    // If we have an unbound generic type, bind the type to the archetypes
    // in the type's definition.
    NominalTypeDecl *D = UGT->getDecl();
    ContainerType = getDeclContext()->getDeclaredTypeInContext();

    if (OuterGenericParams)
      *OuterGenericParams = D->getGenericParams();
  } else if (OuterGenericParams) {
    *OuterGenericParams = getDeclContext()->getGenericParamsOfContext();
  }

  return ContainerType;
}

SourceRange DestructorDecl::getSourceRange() const {
  return { DestructorLoc, Body->getEndLoc() };
}
