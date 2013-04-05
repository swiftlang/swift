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
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Support/Process.h"
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

#include "llvm/Support/raw_ostream.h"

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
  if (Init)
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

/// getExtensionType - If this is a method in a type extension for some type,
/// return that type, otherwise return Type().
Type FuncDecl::getExtensionType() const {
  DeclContext *DC = getDeclContext();
  switch (DC->getContextKind()) {
  case DeclContextKind::TranslationUnit:
  case DeclContextKind::BuiltinModule:
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

  // 'this' is accepts implicit l-values and doesn't force them to the heap.
  return LValueType::get(ContainerType, LValueType::Qual::NonHeap,
                         getASTContext());
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
  Pattern *pattern = argPatterns[1];
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
  if (!Body)
    return cast<NominalTypeDecl>(getDeclContext())->getLoc();
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

//===----------------------------------------------------------------------===//
//  Decl printing.
//===----------------------------------------------------------------------===//

#define DEF_COLOR(NAME, COLOR)\
llvm::raw_ostream::Colors NAME##Color = llvm::raw_ostream::COLOR;

DEF_COLOR(Func, YELLOW)
DEF_COLOR(Extension, MAGENTA)

#undef DEF_COLOR

namespace {
  class PrintPattern : public PatternVisitor<PrintPattern> {
  public:
    raw_ostream &OS;
    PrintPattern(raw_ostream &os) : OS(os) {}

    void visitParenPattern(ParenPattern *P) {
      OS << '(';
      visit(P->getSubPattern());
      OS << ')';
    }
    void visitTuplePattern(TuplePattern *P) {
      OS << '(';
      for (unsigned i = 0, e = P->getNumFields(); i != e; ++i) {
        visit(P->getFields()[i].getPattern());
        if (i + 1 != e)
          OS << ", ";
      }
      OS << ')';
    }
    void visitNamedPattern(NamedPattern *P) {
      OS << P->getBoundName().str();
    }
    void visitAnyPattern(AnyPattern *P) {
      OS << '_';
    }
    void visitTypedPattern(TypedPattern *P) {
      visit(P->getSubPattern());
      OS << " : ";
      if (!P->hasType())
        OS << "<no type yet>";
      else
        P->getType()->print(OS);
    }
  };

  /// PrintDecl - Visitor implementation of Decl::print.
  class PrintDecl : public DeclVisitor<PrintDecl> {
  public:
    raw_ostream &OS;
    unsigned Indent;
    bool ShowColors;
    
    PrintDecl(raw_ostream &os, unsigned indent)
      : OS(os), Indent(indent), ShowColors(false) {
      if (&os == &llvm::errs() || &os == &llvm::outs())
	ShowColors = llvm::errs().is_displayed() && llvm::outs().is_displayed();
    }
    
    void printRec(Decl *D) { PrintDecl(OS, Indent + 2).visit(D); }
    void printRec(Expr *E) { E->print(OS, Indent+2); }
    void printRec(Stmt *S) { S->print(OS, Indent+2); }

    void printGenericParameters(GenericParamList *Params) {
      if (!Params)
        return;

      OS << '<';
      bool First = true;
      for (auto P : *Params) {
        if (First) {
          First = false;
        } else {
          OS << ", ";
        }
        OS << P.getDecl()->getName();
        if (!P.getAsTypeParam()->getInherited().empty()) {
          OS << " : ";
          P.getAsTypeParam()->getInherited()[0].getType()->print(OS);
        }
      }
      OS << '>';
    }

    void printCommon(Decl *D, const char *Name,
                     llvm::Optional<llvm::raw_ostream::Colors> Color =
                      llvm::Optional<llvm::raw_ostream::Colors>()) {
      OS.indent(Indent) << '(';

      // Support optional color output.
      if (ShowColors && Color.hasValue()) {
        if (const char *CStr =
            llvm::sys::Process::OutputColor(Color.getValue(), false, false)) {
          OS << CStr;
        }
      }

      OS << Name;

      if (ShowColors)
        OS << llvm::sys::Process::ResetColor();

      if (auto value = dyn_cast<ValueDecl>(D)) {
        if (!value->getName().empty()) {
          OS << " \"" << value->getName().str() << "\"";
        }
      }
    }

    void printInherited(ArrayRef<TypeLoc> Inherited) {
      if (Inherited.empty())
        return;
      OS << " inherits: ";
      bool First = true;
      for (auto Super : Inherited) {
        if (First)
          First = false;
        else
          OS << ", ";
        
        Super.getType()->print(OS);
      }
    }
    
    void visitImportDecl(ImportDecl *ID) {
      printCommon(ID, "import_decl");
      OS << " '" << ID->getAccessPath()[0].first;
      for (unsigned i = 1, e = ID->getAccessPath().size(); i != e; ++i)
        OS << "." << ID->getAccessPath()[i].first;
      OS << "')";
    }

    void visitExtensionDecl(ExtensionDecl *ED) {
      printCommon(ED, "extension_decl", ExtensionColor);
      OS << ' ';
      ED->getExtendedType()->print(OS);
      printInherited(ED->getInherited());
      for (Decl *Member : ED->getMembers()) {
        OS << '\n';
        printRec(Member);
      }
      OS << ")";
    }

    void printDeclName(ValueDecl *D) {
      if (D->getName().get())
        OS << '\'' << D->getName() << '\'';
      else
        OS << "'anonname=" << (const void*)D << '\'';
    }
    
    void visitTypeAliasDecl(TypeAliasDecl *TAD) {
      printCommon(TAD, "typealias");
      OS << " type='";
      if (TAD->hasUnderlyingType())
        TAD->getUnderlyingType()->print(OS);
      else
        OS << "<<<unresolved>>>";
      printInherited(TAD->getInherited());
      OS << "')";
    }

    void visitProtocolDecl(ProtocolDecl *PD) {
      printCommon(PD, "protocol");
      printInherited(PD->getInherited());
      for (auto VD : PD->getMembers()) {
        OS << '\n';
        printRec(VD);
      }
      OS << ")";
    }
    
    void printCommon(ValueDecl *VD, const char *Name) {
      printCommon((Decl*)VD, Name);
      OS << ' ';
      printDeclName(VD);
      if (FuncDecl *FD = dyn_cast<FuncDecl>(VD))
        printGenericParameters(FD->getGenericParams());
      if (ConstructorDecl *CD = dyn_cast<ConstructorDecl>(VD))
        printGenericParameters(CD->getGenericParams());
      if (NominalTypeDecl *NTD = dyn_cast<NominalTypeDecl>(VD))
        printGenericParameters(NTD->getGenericParams());

      OS << " type='";
      if (VD->hasType())
        VD->getType()->print(OS);
      else
        OS << "<null type>";
      OS << '\'';

      if (VD->hasFixedLifetime()) OS << " hasFixedLifetime=true";
      if (VD->isNeverUsedAsLValue()) OS << " neverUsedAsLValue=true";
    }

    void visitTranslationUnit(const TranslationUnit *TU) {
      OS.indent(Indent) << "(translation_unit";
      for (Decl *D : TU->Decls) {
        OS << '\n';
        printRec(D);
      }
      OS << ')';
    }

    void visitVarDecl(VarDecl *VD) {
      printCommon(VD, "var_decl");
      if (VD->isProperty()) {
        if (FuncDecl *Get = VD->getGetter()) {
          OS << "\n";
          OS.indent(Indent + 2);
          OS << "get =";
          printRec(Get);
        }
        if (FuncDecl *Set = VD->getSetter()) {
          OS << "\n";
          OS.indent(Indent + 2);
          OS << "set =";
          printRec(Set);
        }
      }
      OS << ')';
    }
    
    void visitFuncDecl(FuncDecl *FD) {
      printCommon(FD, "func_decl", FuncColor);
      if (FD->isGetterOrSetter()) {
        
        if (FD->getGetterDecl()) {
          OS << " getter";
        } else {
          assert(FD->getSetterDecl() && "no getter or setter!");
          OS << " setter";
        }
        
        if (ValueDecl *vd = dyn_cast<ValueDecl>(FD->getGetterOrSetterDecl())) {
          OS << "_for=" << vd->getName();
        }
      }
      OS << '\n';
      printRec(FD->getBody());
      OS << ')';
    }

    void visitOneOfDecl(OneOfDecl *OOD) {
      printCommon(OOD, "oneof_decl");
      printInherited(OOD->getInherited());
      for (Decl *D : OOD->getMembers()) {
        OS << '\n';
        printRec(D);
      }
      OS << ')';
    }

    void visitOneOfElementDecl(OneOfElementDecl *OOED) {
      printCommon(OOED, "oneof_element_decl");
      OS << ')';
    }

    void visitStructDecl(StructDecl *SD) {
      printCommon(SD, "struct_decl");
      printInherited(SD->getInherited());
      for (Decl *D : SD->getMembers()) {
        OS << '\n';
        printRec(D);
      }
      OS << ")";
    }

    void visitClassDecl(ClassDecl *CD) {
      printCommon(CD, "class_decl");
      printInherited(CD->getInherited());
      for (Decl *D : CD->getMembers()) {
        OS << '\n';
        printRec(D);
      }
      OS << ")";
    }

    void visitPatternBindingDecl(PatternBindingDecl *PBD) {
      printCommon(PBD, "pattern_binding_decl");
      OS << " pattern='";
      PrintPattern(OS).visit(PBD->getPattern());
      OS << '\'';
      if (PBD->getInit()) {
        OS << '\n';
        printRec(PBD->getInit());
      }
      OS << ')';
    }

    void visitSubscriptDecl(SubscriptDecl *SD) {
      printCommon(SD, "subscript_decl");
      if (FuncDecl *Get = SD->getGetter()) {
        OS << "\n";
        OS.indent(Indent + 2);
        OS << "get = ";
        printRec(Get);
      }
      if (FuncDecl *Set = SD->getSetter()) {
        OS << "\n";
        OS.indent(Indent + 2);
        OS << "set = ";
        printRec(Set);
      }
      OS << ')';
    }

    void visitConstructorDecl(ConstructorDecl *CD) {
      printCommon(CD, "constructor_decl", FuncColor);
      if (CD->getAllocThisExpr()) {
        OS << "\n";
        OS.indent(Indent+2);
        OS << "this = ";
        CD->getAllocThisExpr()->print(OS, 0);
      }
      if (CD->getBody()) {
        OS << '\n';
        printRec(CD->getBody());
      }
      OS << ')';
    }

    void visitDestructorDecl(DestructorDecl *DD) {
      printCommon(DD, "destructor_decl");
      OS << '\n';
      printRec(DD->getBody());
      OS << ')';
    }

    void visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
      printCommon(TLCD, "top_level_code_decl");
      if (TLCD->getBody()) {
        OS << "\n";
        printRec(TLCD->getBody());
      }
    }

  };
} // end anonymous namespace.

void Decl::dump() const {
  print(llvm::errs());
  PrintDecl(llvm::errs(), 0).visit(const_cast<Decl *>(this));
  llvm::errs() << '\n';
}

void Decl::dump(unsigned Indent) const {
  print(llvm::errs());
  PrintDecl(llvm::errs(), Indent).visit(const_cast<Decl *>(this));
  llvm::errs() << '\n';
}

void TranslationUnit::dump() const {
  PrintDecl(llvm::errs(), 0).visitTranslationUnit(this);
  llvm::errs() << '\n';
}

void Pattern::print(llvm::raw_ostream &OS) const {
  PrintPattern(OS).visit(const_cast<Pattern*>(this));
}

void Pattern::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
