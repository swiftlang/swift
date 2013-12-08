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
#include "swift/AST/Expr.h"
#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

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

ClangNode Decl::getClangNodeSlow() const {
  return getASTContext().getClangNode(this);
}

void Decl::setClangNode(ClangNode node) {
  DeclBits.FromClang = true;
  getASTContext().setClangNode(this, node);
}

bool Decl::isTransparent() const {
  // Check if the declaration had the attribute.
  if (getAttrs().isTransparent())
    return true;

  // Check if this is a function declaration which is within a transparent
  // extension.
  if (const AbstractFunctionDecl *FD = dyn_cast<AbstractFunctionDecl>(this)) {
    if (const ExtensionDecl *ED = dyn_cast<ExtensionDecl>(FD->getParent()))
      return ED->isTransparent();
  }

  // If this is a getter or a seter, check if the transparent attribute was set
  // on the value decl.
  if (const FuncDecl *FD = dyn_cast<FuncDecl>(this)) {
    if (ValueDecl *VD = FD->getGetterOrSetterDecl())
      return VD->isTransparent();
  }

  return false;
}

GenericParamList::GenericParamList(SourceLoc LAngleLoc,
                                   ArrayRef<GenericParam> Params,
                                   SourceLoc WhereLoc,
                                   MutableArrayRef<RequirementRepr> Requirements,
                                   SourceLoc RAngleLoc)
  : Brackets(LAngleLoc, RAngleLoc), NumParams(Params.size()),
    WhereLoc(WhereLoc), Requirements(Requirements),
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
                                    MutableArrayRef<RequirementRepr>(),
                                    RAngleLoc);
}

GenericParamList *
GenericParamList::create(const ASTContext &Context,
                         SourceLoc LAngleLoc,
                         ArrayRef<GenericParam> Params,
                         SourceLoc WhereLoc,
                         MutableArrayRef<RequirementRepr> Requirements,
                         SourceLoc RAngleLoc) {
  unsigned Size = sizeof(GenericParamList)
                + sizeof(GenericParam) * Params.size();
  void *Mem = Context.Allocate(Size, alignof(GenericParamList));
  return new (Mem) GenericParamList(LAngleLoc, Params,
                                    WhereLoc,
                                    Context.AllocateCopy(Requirements),
                                    RAngleLoc);
}

ImportDecl *ImportDecl::create(ASTContext &Ctx, DeclContext *DC,
                               SourceLoc ImportLoc, ImportKind Kind,
                               SourceLoc KindLoc, bool Exported,
                               ArrayRef<AccessPathElement> Path) {
  assert(!Path.empty());
  assert(Kind == ImportKind::Module || Path.size() > 1);
  void *buffer = Ctx.Allocate(sizeof(ImportDecl) +
                              Path.size() * sizeof(AccessPathElement),
                              alignof(ImportDecl));
  return new (buffer) ImportDecl(DC, ImportLoc, Kind, KindLoc, Exported, Path);
}

ImportDecl::ImportDecl(DeclContext *DC, SourceLoc ImportLoc, ImportKind K,
                       SourceLoc KindLoc, bool Exported,
                       ArrayRef<AccessPathElement> Path)
  : Decl(DeclKind::Import, DC), ImportLoc(ImportLoc), KindLoc(KindLoc),
    NumPathElements(Path.size()) {
  ImportDeclBits.ImportKind = static_cast<unsigned>(K);
  assert(getImportKind() == K && "not enough bits for ImportKind");
  ImportDeclBits.IsExported = Exported;
  std::uninitialized_copy(Path.begin(), Path.end(), getPathBuffer());
}

/// Associates the decls that are conforming, to the protocol decls.
static void associateConformingValueDecls(ProtocolConformance *Conformance,
                                          ASTContext &Ctx) {
  if (!Conformance)
    return;

  for (auto Witness : Conformance->getWitnesses()) {
    if (Witness.second)
      Ctx.recordConformingDecl(Witness.second.getDecl(), Witness.first);
  }
  
  for (auto InheritedConf: Conformance->getInheritedConformances())
    associateConformingValueDecls(InheritedConf.second, Ctx);
}

static void associateConformingValueDecls(
    ArrayRef<ProtocolConformance *> Conformances, ASTContext &Ctx) {
  for (auto Conf: Conformances) {
    associateConformingValueDecls(Conf, Ctx);
  }
}

void ExtensionDecl::setConformances(ArrayRef<ProtocolConformance *> c) {
  Conformances = c;
  associateConformingValueDecls(c, getASTContext());
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

SourceLoc TopLevelCodeDecl::getStartLoc() const {
  return Body->getStartLoc();
}

SourceRange TopLevelCodeDecl::getSourceRange() const {
  return Body->getSourceRange();
}

bool ValueDecl::isSettableOnBase(Type baseType) const {
  if (!isSettable()) return false;
  if (!baseType) return true;
  if (!isInstanceMember() && baseType->is<MetaTypeType>()) return true;
  return (baseType->isSettableLValue() ||
          baseType->getRValueType()->hasReferenceSemantics());
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
    llvm_unreachable("non-value decls shouldn't get here");

  case DeclKind::Func:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
    return cast<AbstractFunctionDecl>(this)->getBodyKind() !=
        AbstractFunctionDecl::BodyKind::None;

  case DeclKind::Var:
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
    // enum elements are not instance members.
    return false;

  case DeclKind::Subscript:
    // Subscripts are always instance members.
    return true;

  case DeclKind::Var:
    // Non-static variables are instance members.
    return !cast<VarDecl>(this)->isStatic();
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

ValueDecl *ValueDecl::getOverriddenDecl() const {
  if (auto fd = dyn_cast<FuncDecl>(this)) {
    return fd->getOverriddenDecl();
  }
  if (auto vd = dyn_cast<VarDecl>(this)) {
    return vd->getOverriddenDecl();
  }
  if (auto sd = dyn_cast<SubscriptDecl>(this)) {
    return sd->getOverriddenDecl();
  }
  return nullptr;
}

bool ValueDecl::canBeAccessedByDynamicLookup() const {
  if (getName().empty())
    return false;

  // Dynamic lookup can only find [objc] members.
  if (!isObjC())
    return false;

  // Dynamic lookup can only find class and protocol members, or extensions of
  // classes.
  auto nominalDC =getDeclContext()->getDeclaredTypeOfContext()->getAnyNominal();
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
  assert(Ty.isNull() && "changing type of declaration");
  Ty = T;
  if (!T.isNull() && T->is<ErrorType>())
    setInvalid();
}

/// Overwrite the type of this declaration.
void ValueDecl::overwriteType(Type T) {
  Ty = T;
  if (!T.isNull() && T->is<ErrorType>())
    setInvalid();
}

DeclContext *ValueDecl::getPotentialGenericDeclContext() {
  if (auto func = dyn_cast<AbstractFunctionDecl>(this))
    return func;

  return getDeclContext();
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
    InterfaceTy = MetaTypeType::get(InterfaceTy, ctx);
    return InterfaceTy;
  }

  if (!hasType())
    return Type();

  // If the type involves a type variable, don't cache it.
  auto type = getType();
  if (type->hasTypeVariable())
    return type;

  InterfaceTy = type;
  return InterfaceTy;
}

void ValueDecl::setInterfaceType(Type type) {
  assert((type.isNull() || !type->hasTypeVariable()) &&
         "Type variable in interface type");
  InterfaceTy = type;
}

Type TypeDecl::getDeclaredType() const {
  if (auto TAD = dyn_cast<TypeAliasDecl>(this))
    return TAD->getAliasType();
  if (auto typeParam = dyn_cast<AbstractTypeParamDecl>(this))
    return typeParam->getType()->castTo<MetaTypeType>()->getInstanceType();
  return cast<NominalTypeDecl>(this)->getDeclaredType();
}

Type TypeDecl::getDeclaredInterfaceType() const {
  return getInterfaceType()->castTo<MetaTypeType>()->getInstanceType();
}

bool TypeDecl::derivesProtocolConformance(ProtocolDecl *protocol) const {
  // Enums with raw types can derive their RawRepresentable conformance.
  if (auto *enumDecl = dyn_cast<EnumDecl>(this)) {
    auto rawRepresentable
      = getASTContext().getProtocol(KnownProtocolKind::RawRepresentable);
    return enumDecl->hasRawType() && protocol == rawRepresentable;
  }
  return false;
}

void TypeDecl::setConformances(ArrayRef<ProtocolConformance *> c) {
  Conformances = c;
  // A typealias should not affect the conformance bit of members of the
  // original; mark conformances only if this is a nominal type decl.
  if (isa<NominalTypeDecl>(this))
    associateConformingValueDecls(c, getASTContext());
}

GenericSignature::GenericSignature(ArrayRef<GenericTypeParamType *> params,
                                   ArrayRef<Requirement> requirements)
  : NumGenericParams(params.size()), NumRequirements(requirements.size())
{
  std::copy(params.begin(), params.end(),
            getGenericParamsBuffer().data());
  std::copy(requirements.begin(), requirements.end(),
            getRequirementsBuffer().data());
}

GenericSignature *GenericSignature::get(ArrayRef<GenericTypeParamType *> params,
                                        ArrayRef<Requirement> requirements,
                                        ASTContext &ctx) {
  // Allocate storage for the object.
  size_t bytes = sizeof(GenericSignature)
               + sizeof(GenericTypeParamType *) * params.size()
               + sizeof(Requirement) * requirements.size();
  void *mem = ctx.Allocate(bytes, alignof(GenericSignature));
  return new (mem) GenericSignature(params, requirements);
}

void NominalTypeDecl::setGenericSignature(
                        ArrayRef<GenericTypeParamType *> params,
                        ArrayRef<Requirement> requirements) {
  assert(!GenericSig && "Already have generic signature");
  GenericSig = GenericSignature::get(params, requirements, getASTContext());
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
  setType(MetaTypeType::get(DeclaredTy, ctx));

  // A protocol has an implicit generic parameter list consisting of a single
  // generic parameter, Self, that conforms to the protocol itself. This
  // parameter is always implicitly bound.
  //
  // If this protocol has been deserialized, it already has generic parameters.
  // Don't add them again.
  if (!getGenericParams()) {
    if (auto proto = dyn_cast<ProtocolDecl>(this)) {
      // The generic parameter 'Self'.
      auto selfId = ctx.getIdentifier("Self");
      auto selfDecl = new (ctx) GenericTypeParamDecl(proto, selfId,
                                                     proto->getLoc(), 0, 0);
      IdentTypeRepr::Component protoRef(proto->getLoc(), proto->getName(), { });
      protoRef.setValue(proto);
      TypeLoc selfInherited[1] = {
        TypeLoc(IdentTypeRepr::create(ctx, protoRef))
      };
      selfInherited[0].setType(DeclaredTy);
      selfDecl->setInherited(ctx.AllocateCopy(selfInherited));
      selfDecl->setImplicit();
      
      // The generic parameter list itself.
      GenericParams = GenericParamList::create(ctx, SourceLoc(),
                                               GenericParam(selfDecl),
                                               SourceLoc());
    }
  }
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
      GenericArgs.push_back(Param.getAsTypeParam()->getArchetype());
    Ty = BoundGenericType::get(D, getDeclContext()->getDeclaredTypeInContext(),
                               GenericArgs);
  }
  DeclaredTyInContext = Ty;
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
      genericArgs.push_back(param.getAsTypeParam()->getDeclaredType());

    type = BoundGenericType::get(const_cast<NominalTypeDecl *>(this),
                                 parentType, genericArgs);
  } else {
    type = NominalType::get(const_cast<NominalTypeDecl *>(this), parentType,
                            getASTContext());
  }

  InterfaceTy = MetaTypeType::get(type, getASTContext());
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
  // If this is a class, it conforms to the DynamicLookup protocol.
  if (isa<ClassDecl>(this)) {
    if (auto dynamicLookup
          = getASTContext().getProtocol(KnownProtocolKind::DynamicLookup)) {
      protocols.push_back(dynamicLookup);
    }
  }
}

TypeAliasDecl::TypeAliasDecl(SourceLoc TypeAliasLoc, Identifier Name,
                             SourceLoc NameLoc, TypeLoc UnderlyingTy,
                             DeclContext *DC,
                             MutableArrayRef<TypeLoc> Inherited)
  : TypeDecl(DeclKind::TypeAlias, DC, Name, NameLoc, Inherited),
    TypeAliasLoc(TypeAliasLoc),
    UnderlyingTy(UnderlyingTy)
{
  // Set the type of the TypeAlias to the right MetaTypeType.
  ASTContext &Ctx = getASTContext();
  AliasTy = new (Ctx, AllocationArena::Permanent) NameAliasType(this);
  setType(MetaTypeType::get(AliasTy, Ctx));
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
  setType(MetaTypeType::get(type, ctx));
}

SourceRange GenericTypeParamDecl::getSourceRange() const {
  SourceLoc endLoc = getNameLoc();

  if (!getInherited().empty()) {
    endLoc = getInherited().back().getSourceRange().End;
  }
  return SourceRange(getNameLoc(), endLoc);
}

AssociatedTypeDecl::AssociatedTypeDecl(DeclContext *dc, SourceLoc keywordLoc,
                                       Identifier name, SourceLoc nameLoc)
  : AbstractTypeParamDecl(DeclKind::AssociatedType, dc, name, nameLoc),
    KeywordLoc(keywordLoc)
{
  auto &ctx = dc->getASTContext();
  auto type = new (ctx, AllocationArena::Permanent) AssociatedTypeType(this);
  setType(MetaTypeType::get(type, ctx));
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
    StructLoc(StructLoc) { }

ClassDecl::ClassDecl(SourceLoc ClassLoc, Identifier Name, SourceLoc NameLoc,
                     MutableArrayRef<TypeLoc> Inherited,
                     GenericParamList *GenericParams, DeclContext *Parent)
  : NominalTypeDecl(DeclKind::Class, Parent, Name, NameLoc, Inherited,
                    GenericParams),
    ClassLoc(ClassLoc) {
  ClassDeclBits.Circularity
    = static_cast<unsigned>(CircularityCheck::Unchecked);
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
  for (Decl *D : getMembers())
    if (EnumElementDecl *Elt = dyn_cast<EnumElementDecl>(D))
      if (Elt->getName() == Name)
        return Elt;
  return 0;
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

      if (Visited.insert(InheritedProto))
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
      if (Inherited.insert(InheritedProto))
        Stack.push_back(InheritedProto);
    }
  }
}

bool ProtocolDecl::requiresClassSlow() {
  ProtocolDeclBits.RequiresClass = false;

  if (isProtocolsValid()) {
    // Only cache the result if it can not change in future.
    ProtocolDeclBits.RequiresClassValid = true;
  }

  if (getAttrs().isClassProtocol()) {
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
  return getGenericParams()->getParams()[0].getAsTypeParam();
}

SourceRange VarDecl::getTypeSourceRangeForDiagnostics() const {
  if (!getParentPattern())
    return getSourceRange();

  auto *Pat = getParentPattern()->getPattern();
  if (auto *TP = dyn_cast<TypedPattern>(Pat)) {
    return TP->getTypeLoc().getTypeRepr()->getSourceRange();
  }
  return getSourceRange();
}

void VarDecl::setComputedAccessors(ASTContext &Context, SourceLoc LBraceLoc,
                                   FuncDecl *Get, FuncDecl *Set,
                                   SourceLoc RBraceLoc) {
  assert(!GetSet && "Variable already has accessors?");
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

Type VarDecl::getGetterType() const {
  // If we have a getter, use its type.
  if (auto getter = getGetter())
    return getter->getType();

  // Otherwise, compute the type.
  GenericParamList *outerParams = nullptr;
  auto selfTy = getDeclContext()->getSelfTypeInContext(/*isStatic=*/isStatic(),
                                                       /*isConstructor=*/false,
                                                       &outerParams);

  // Form the getter type.
  auto &ctx = getASTContext();
  Type getterTy = FunctionType::get(TupleType::getEmpty(ctx), getType(), ctx);

  // Add 'self', if we have one.
  if (selfTy) {
    if (outerParams)
      getterTy = PolymorphicFunctionType::get(selfTy, getterTy, outerParams,
                                              ctx);
    else
      getterTy = FunctionType::get(selfTy, getterTy, ctx);
  }

  return getterTy;
}

Type VarDecl::getGetterInterfaceType() const {
  // If we have a getter, use its type.
  if (auto getter = getGetter())
    return getter->getInterfaceType();

  // Otherwise, compute the type.
  auto selfTy = getDeclContext()->getInterfaceSelfType(/*isStatic=*/isStatic(),
                                                       /*isConstructor=*/false);

  // Form the getter type.
  auto &ctx = getASTContext();
  Type getterTy = FunctionType::get(TupleType::getEmpty(ctx),
                                    getInterfaceType(), ctx);

  // Add 'self', if we have one.
  if (selfTy) {
    ArrayRef<GenericTypeParamType*> genericParams;
    ArrayRef<Requirement> requirements;
    std::tie(genericParams, requirements)
      = getDeclContext()->getGenericSignatureOfContext();
    
    if (genericParams.empty() && requirements.empty())
      getterTy = FunctionType::get(selfTy, getterTy, ctx);
    else
      getterTy = GenericFunctionType::get(genericParams, requirements,
                                          selfTy, getterTy,
                                          AnyFunctionType::ExtInfo(), ctx);
  }

  return getterTy;
}

Type VarDecl::getSetterType() const {
  // If we have a setter, use its type.
  if (auto setter = getSetter())
    return setter->getType();

  // Otherwise, compute the type.
  GenericParamList *outerParams = nullptr;
  auto selfTy = getDeclContext()->getSelfTypeInContext(/*isStatic=*/isStatic(),
                                                       /*isConstructor=*/false,
                                                       &outerParams);

  // Form the element -> () function type.
  auto &ctx = getASTContext();
  TupleTypeElt valueElt(getType(), ctx.getIdentifier("value"));
  Type setterTy = FunctionType::get(TupleType::get(valueElt, ctx), 
                                    TupleType::getEmpty(ctx),
                                    ctx);

  // Add the 'self' type, if we have one.
  if (selfTy) {
    if (outerParams)
      setterTy = PolymorphicFunctionType::get(selfTy, setterTy, outerParams,
                                              ctx);
    else
      setterTy = FunctionType::get(selfTy, setterTy, ctx);
  }

  return setterTy;
}

Type VarDecl::getSetterInterfaceType() const {
  // If we have a getter, use its type.
  if (auto setter = getSetter())
    return setter->getInterfaceType();
  
  // Otherwise, compute the type.
  auto selfTy = getDeclContext()->getInterfaceSelfType(/*isStatic=*/isStatic(),
                                                       /*isConstructor=*/false);
  
  // Form the element -> () function type.
  auto &ctx = getASTContext();
  TupleTypeElt valueElt(getInterfaceType(), ctx.getIdentifier("value"));
  Type setterTy = FunctionType::get(TupleType::get(valueElt, ctx),
                                    TupleType::getEmpty(ctx),
                                    ctx);
  
  // Add the 'self' type, if we have one.
  if (selfTy) {
    ArrayRef<GenericTypeParamType*> genericParams;
    ArrayRef<Requirement> requirements;
    std::tie(genericParams, requirements)
      = getDeclContext()->getGenericSignatureOfContext();
    
    if (genericParams.empty() && requirements.empty())
      setterTy = FunctionType::get(selfTy, setterTy, ctx);
    else
      setterTy = GenericFunctionType::get(genericParams, requirements,
                                          selfTy, setterTy,
                                          AnyFunctionType::ExtInfo(), ctx);
  }
  
  return setterTy;
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

Type AbstractFunctionDecl::computeSelfType(
                             GenericParamList **outerGenericParams) {
  bool isStatic = isa<FuncDecl>(this) && cast<FuncDecl>(this)->isStatic();
  return getDeclContext()->getSelfTypeInContext(isStatic,
                                                isa<ConstructorDecl>(this),
                                                outerGenericParams);
}

VarDecl *AbstractFunctionDecl::getImplicitSelfDeclSlow() const {
  if (auto FD = dyn_cast<FuncDecl>(this)) {
    VarDecl *SelfDecl = FD->getImplicitSelfDeclImpl();
    ImplicitSelfDeclAndIsCached.setPointerAndInt(SelfDecl, true);
    return SelfDecl;
  }
  ImplicitSelfDeclAndIsCached.setPointerAndInt(nullptr, true);
  return nullptr;
}

Type AbstractFunctionDecl::getExtensionType() const {
  return getDeclContext()->getDeclaredTypeInContext();
}

std::pair<DefaultArgumentKind, Type>
AbstractFunctionDecl::getDefaultArg(unsigned Index) const {
  ArrayRef<const Pattern *> Patterns = getArgParamPatterns();

  if (isa<FuncDecl>(this) && getImplicitSelfDecl()) {
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

VarDecl *FuncDecl::getImplicitSelfDeclImpl() const {
  ArrayRef<const Pattern *> ArgParamPatterns = getArgParamPatterns();
  if (ArgParamPatterns.empty())
    return nullptr;

  // "self" is represented as (typed_pattern (named_pattern (var_decl 'self')).
  auto TP = dyn_cast<TypedPattern>(ArgParamPatterns[0]);
  if (!TP)
    return nullptr;

  // The decl should be named 'self' and be implicit.
  auto NP = dyn_cast<NamedPattern>(TP->getSubPattern());
  if (NP && NP->getBoundName().str() == "self" && NP->isImplicit())
    return NP->getDecl();
  return nullptr;
}

FuncDecl *FuncDecl::createDeserialized(ASTContext &Context,
                                       SourceLoc StaticLoc, SourceLoc FuncLoc,
                                       Identifier Name, SourceLoc NameLoc,
                                       GenericParamList *GenericParams,
                                       Type Ty, unsigned NumParamPatterns,
                                       DeclContext *Parent) {
  assert(NumParamPatterns > 0);
  void *Mem = Context.Allocate(
      sizeof(FuncDecl) + 2 * NumParamPatterns * sizeof(Pattern *),
      alignof(FuncDecl));
  return ::new (Mem)
      FuncDecl(StaticLoc, FuncLoc, Name, NameLoc, NumParamPatterns,
               GenericParams, Ty, Parent);
}

FuncDecl *FuncDecl::create(ASTContext &Context, SourceLoc StaticLoc,
                           SourceLoc FuncLoc, Identifier Name,
                           SourceLoc NameLoc, GenericParamList *GenericParams,
                           Type Ty, ArrayRef<Pattern *> ArgParams,
                           ArrayRef<Pattern *> BodyParams,
                           TypeLoc FnRetType, DeclContext *Parent) {
  assert(ArgParams.size() == BodyParams.size());
  const unsigned NumParamPatterns = ArgParams.size();
  auto *FD = FuncDecl::createDeserialized(
      Context, StaticLoc, FuncLoc, Name, NameLoc, GenericParams, Ty,
      NumParamPatterns, Parent);
  FD->setDeserializedSignature(ArgParams, BodyParams, FnRetType);
  return FD;
}

void FuncDecl::setDeserializedSignature(ArrayRef<Pattern *> ArgParams,
                                        ArrayRef<Pattern *> BodyParams,
                                        TypeLoc FnRetType) {
  MutableArrayRef<Pattern *> ArgParamsRef = getArgParamPatterns();
  MutableArrayRef<Pattern *> BodyParamsRef = getBodyParamPatterns();
  const unsigned NumParamPatterns = ArgParamsRef.size();

  assert(ArgParams.size() == BodyParams.size());
  assert(NumParamPatterns == ArgParams.size());

  for (unsigned i = 0; i != NumParamPatterns; ++i)
    ArgParamsRef[i] = ArgParams[i];
  for (unsigned i = 0; i != NumParamPatterns; ++i)
    BodyParamsRef[i] = BodyParams[i];

  this->FnRetType = FnRetType;
}

Type FuncDecl::getResultType() const {
  Type resultTy = getType();
  if (!resultTy || resultTy->is<ErrorType>())
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
  
  auto *argTuple = dyn_cast<TuplePattern>(getArgParamPatterns()[opArgIndex]);
  if (!argTuple)
    return true;

  return argTuple->getNumFields() == 1 && !argTuple->hasVararg();
}

bool FuncDecl::isBinaryOperator() const {
  if (!isOperator())
    return false;
  
  unsigned opArgIndex = isa<ProtocolDecl>(getDeclContext()) ? 1 : 0;
  
  auto *argTuple = dyn_cast<TuplePattern>(getArgParamPatterns()[opArgIndex]);
  if (!argTuple)
    return false;
  
  return argTuple->getNumFields() == 2
    || (argTuple->getNumFields() == 1 && argTuple->hasVararg());
}

StringRef VarDecl::getObjCGetterSelector(SmallVectorImpl<char> &buffer) const {
  llvm::raw_svector_ostream out(buffer);

  // The getter selector is the property name itself.
  // FIXME: 'is' prefix for boolean properties?
  out << getName().str();
  return out.str();
}

StringRef VarDecl::getObjCSetterSelector(SmallVectorImpl<char> &buffer) const {
  llvm::raw_svector_ostream out(buffer);

  // The setter selector for, e.g., 'fooBar' is 'setFooBar:', with the
  // property name capitalized and preceded by 'set'.
  StringRef name = getName().str();
  assert(name.size() >= 1 && "empty var name?!");
    
  out << "set" << char(toupper(name[0])) << name.slice(1, name.size()) << ':';
  return out.str();
}

/// Produce the selector for this "Objective-C method" in the given buffer.
StringRef FuncDecl::getObjCSelector(SmallVectorImpl<char> &buffer) const {
  assert(buffer.empty());
  
  // Property accessors should go through a different path.
  assert(!isGetterOrSetter());

  llvm::raw_svector_ostream out(buffer);

  // Start with the method name.
  out << getName().str();

  // We should always have exactly two levels of argument pattern.
  auto argPatterns = getArgParamPatterns();
  assert(argPatterns.size() == 2);
  const Pattern *pattern = argPatterns[1];
  auto tuple = dyn_cast<TuplePattern>(pattern);

  // If it's an empty tuple pattern, it's a nullary selector.
  if (tuple && tuple->getNumFields() == 0)
    return out.str();

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
  if (getBodyKind() == BodyKind::Unparsed ||
      getBodyKind() == BodyKind::Skipped)
    return { FuncLoc, BodyEndLoc };

  if (auto *B = getBody())
    return { FuncLoc, B->getEndLoc() };
  if (getBodyResultTypeLoc().hasLocation())
    return { FuncLoc, getBodyResultTypeLoc().getSourceRange().End };
  const Pattern *LastPat = getArgParamPatterns().back();
  return { FuncLoc, LastPat->getEndLoc() };
}

/// Determine whether the given type is (or bridges to) an
/// Objective-C object type.
static bool isObjCObjectOrBridgedType(Type type) {
  // FIXME: Bridged types info should be available here in the AST
  // library, rather than hard-coding them.
  if (auto structTy = type->getAs<StructType>()) {
    auto structDecl = structTy->getDecl();
    const DeclContext *DC = structDecl->getDeclContext();
    if (DC->isModuleScopeContext() && DC->getParentModule()->isStdlibModule()) {
      if (structDecl->getName().str() == "String")
        return true;
    }
   
    return false;
  }

  // Unwrap metatypes for remaining checks.
  if (auto metaTy = type->getAs<MetaTypeType>())
    type = metaTy->getInstanceType();

  // Class types are Objective-C object types.
  if (type->is<ClassType>())
    return true;

  // [objc] protocols
  if (auto protoTy = type->getAs<ProtocolType>()) {
    auto proto = protoTy->getDecl();
    return proto->requiresClass() && proto->isObjC();
  }

  return false;
}

/// Determine whether the given Swift type is an integral type, i.e.,
/// a type that wraps a builtin integer.
static bool isIntegralType(Type type) {
  // Consider structs in the "swift" module that wrap a builtin
  // integer type to be integral types.
  if (auto structTy = type->getAs<StructType>()) {
    auto structDecl = structTy->getDecl();
    const DeclContext *DC = structDecl->getDeclContext();
    if (!DC->isModuleScopeContext() || !DC->getParentModule()->isStdlibModule())
      return false;

    // Find the single ivar.
    VarDecl *singleVar = nullptr;
    for (auto member : structDecl->getMembers()) {
      auto var = dyn_cast<VarDecl>(member);
      if (!var || var->isComputed())
        continue;

      if (singleVar)
        return false;

      singleVar = var;
    }

    if (!singleVar)
      return false;

    // Check whether it has integer type.
    return singleVar->getType()->is<BuiltinIntegerType>();
  }

  return false;
}

Type SubscriptDecl::getGetterType() const {
  // If we have a getter, use its type.
  if (auto getter = getGetter())
    return getter->getType();

  // Otherwise, compute the type.
  GenericParamList *outerParams = nullptr;
  auto selfTy = getDeclContext()->getSelfTypeInContext(/*isStatic=*/false,
                                                       /*isConstructor=*/false,
                                                       &outerParams);

  // Form the () -> element function type.
  auto &ctx = getASTContext();
  Type getterTy = FunctionType::get(TupleType::getEmpty(ctx), getElementType(),
                                    ctx);

  // Prepend the indices.
  getterTy = FunctionType::get(getIndices()->getType(), getterTy, ctx);
  
  // Prepend the 'self' type.
  if (outerParams)
    getterTy = PolymorphicFunctionType::get(selfTy, getterTy, outerParams, ctx);
  else
    getterTy = FunctionType::get(selfTy, getterTy, ctx);

  return getterTy;
}

Type SubscriptDecl::getGetterInterfaceType() const {
  // If we have a getter, use its type.
  if (auto getter = getGetter())
    return getter->getInterfaceType();

  // Otherwise, compute the type.
  auto selfTy = getDeclContext()->getInterfaceSelfType(/*isStatic=*/false,
                                                       /*isConstructor=*/false);

  auto interfaceTy = getInterfaceType()->castTo<AnyFunctionType>();
  auto indicesTy = interfaceTy->getInput();
  auto elementTy = interfaceTy->getResult();
  
  // Form the () -> element function type.
  auto &ctx = getASTContext();
  Type getterTy = FunctionType::get(TupleType::getEmpty(ctx), elementTy,
                                    ctx);

  // Prepend the indices.
  getterTy = FunctionType::get(indicesTy, getterTy, ctx);
  
  // Prepend the 'self' type.
  ArrayRef<GenericTypeParamType*> genericParams;
  ArrayRef<Requirement> requirements;
  std::tie(genericParams, requirements)
    = getDeclContext()->getGenericSignatureOfContext();
  
  if (genericParams.empty() && requirements.empty())
    getterTy = FunctionType::get(selfTy, getterTy, ctx);
  else
    getterTy = GenericFunctionType::get(genericParams, requirements,
                                        selfTy, getterTy,
                                        AnyFunctionType::ExtInfo(), ctx);

  return getterTy;
}

Type SubscriptDecl::getSetterType() const {
  // If we have a setter, use its type.
  if (auto setter = getSetter())
    return setter->getType();

  // Otherwise, compute the type.
  GenericParamList *outerParams = nullptr;
  auto selfTy = getDeclContext()->getSelfTypeInContext(/*isStatic=*/false,
                                                       /*isConstructor=*/false,
                                                       &outerParams);

  // Form the element -> () function type.
  auto &ctx = getASTContext();
  TupleTypeElt valueElt(getElementType(), ctx.getIdentifier("value"));
  Type setterTy = FunctionType::get(TupleType::get(valueElt, ctx), 
                                    TupleType::getEmpty(ctx),
                                    ctx);

  // Prepend the indices.
  setterTy = FunctionType::get(getIndices()->getType(), setterTy, ctx);

  // Prepend the 'self' type.
  if (outerParams)
    setterTy = PolymorphicFunctionType::get(selfTy, setterTy, outerParams, ctx);
  else
    setterTy = FunctionType::get(selfTy, setterTy, ctx);

  return setterTy;
}

Type SubscriptDecl::getSetterInterfaceType() const {
  // If we have a setter, use its type.
  if (auto setter = getSetter())
    return setter->getInterfaceType();

  // Otherwise, compute the type.
  auto selfTy = getDeclContext()->getInterfaceSelfType(/*isStatic=*/false,
                                                       /*isConstructor=*/false);

  auto interfaceTy = getInterfaceType()->castTo<AnyFunctionType>();
  auto indicesTy = interfaceTy->getInput();
  auto elementTy = interfaceTy->getResult();

  // Form the element -> () function type.
  auto &ctx = getASTContext();
  TupleTypeElt valueElt(elementTy, ctx.getIdentifier("value"));
  Type setterTy = FunctionType::get(TupleType::get(valueElt, ctx), 
                                    TupleType::getEmpty(ctx),
                                    ctx);

  // Prepend the indices.
  setterTy = FunctionType::get(indicesTy, setterTy, ctx);

  // Prepend the 'self' type.
  ArrayRef<GenericTypeParamType*> genericParams;
  ArrayRef<Requirement> requirements;
  std::tie(genericParams, requirements)
    = getDeclContext()->getGenericSignatureOfContext();
  
  if (genericParams.empty() && requirements.empty())
    setterTy = FunctionType::get(selfTy, setterTy, ctx);
  else
    setterTy = GenericFunctionType::get(genericParams, requirements,
                                        selfTy, setterTy,
                                        AnyFunctionType::ExtInfo(), ctx);

  return setterTy;
}

ObjCSubscriptKind SubscriptDecl::getObjCSubscriptKind() const {
  auto indexTy = getIndices()->getType();

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
  if (isObjCObjectOrBridgedType(indexTy))
    return ObjCSubscriptKind::Keyed;

  return ObjCSubscriptKind::None;
}

StringRef SubscriptDecl::getObjCGetterSelector() const {
  switch (getObjCSubscriptKind()) {
  case ObjCSubscriptKind::None:
    llvm_unreachable("Not an Objective-C subscript");
   
  case ObjCSubscriptKind::Indexed:
    return "objectAtIndexedSubscript:";

  case ObjCSubscriptKind::Keyed:
    return "objectForKeyedSubscript:";
  }
}

StringRef SubscriptDecl::getObjCSetterSelector() const {
  switch (getObjCSubscriptKind()) {
  case ObjCSubscriptKind::None:
    llvm_unreachable("Not an Objective-C subscript");
   
  case ObjCSubscriptKind::Indexed:
    return "setObject:atIndexedSubscript:";

  case ObjCSubscriptKind::Keyed:
    return "setObject:forKeyedSubscript:";
  }
}

SourceRange EnumElementDecl::getSourceRange() const {
  if (RawValueExpr && !RawValueExpr->isImplicit())
    return {getStartLoc(), RawValueExpr->getEndLoc()};
  if (ArgumentType.hasLocation())
    return {getStartLoc(), ArgumentType.getSourceRange().End};
  return {getStartLoc(), getNameLoc()};
}

SourceRange SubscriptDecl::getSourceRange() const {
  if (Braces.isValid())
    return { getSubscriptLoc(), Braces.End };
  return { getSubscriptLoc(), ElementTy.getSourceRange().End };
}

SourceRange ConstructorDecl::getSourceRange() const {
  if (getBodyKind() == BodyKind::Unparsed ||
      getBodyKind() == BodyKind::Skipped)
    return { getConstructorLoc(), BodyEndLoc };

  if (!Body || !Body->getEndLoc().isValid()) {
    const DeclContext *DC = getDeclContext();
    switch (DC->getContextKind()) {
    case DeclContextKind::ExtensionDecl:
      return cast<ExtensionDecl>(DC)->getLoc();
    case DeclContextKind::NominalTypeDecl:
      return cast<NominalTypeDecl>(DC)->getLoc();
    default:
      if (isInvalid())
        return getConstructorLoc();
      llvm_unreachable("Unhandled decl kind");
    }
  }
  return { getConstructorLoc(), Body->getEndLoc() };
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

/// Produce the selector for this "Objective-C method" in the given buffer.
StringRef
ConstructorDecl::getObjCSelector(SmallVectorImpl<char> &buffer) const {
  assert(buffer.empty());

  llvm::raw_svector_ostream out(buffer);

  // In the beginning, there was 'init'.
  out << "init";

  // If there are no parameters, this is just 'init'.
  auto tuple = cast<TuplePattern>(getArgParams());
  if (tuple->getNumFields() == 0) {
    return out.str();
  }

  // The first field is special: we uppercase the name.
  const auto &firstElt = tuple->getFields()[0];
  auto firstPattern = firstElt.getPattern()->getSemanticsProvidingPattern();
  if (auto firstNamed = dyn_cast<NamedPattern>(firstPattern)) {
    if (!firstNamed->getBoundName().empty()) {
      auto nameStr = firstNamed->getBoundName().str();
      out << (char)toupper(nameStr[0]);
      out << nameStr.substr(1);
    }

    // If there is only a single parameter and its type is the empty tuple
    // type, we're done: don't add the trailing colon.
    if (tuple->getNumFields() == 1) {
      auto emptyTupleTy = TupleType::getEmpty(getASTContext());
      if (!firstPattern->getType()->isEqual(emptyTupleTy))
        out << ':';
      return out.str();
    }

    // Continue with the remaining selectors.
    out << ':';
  }

  // For every remaining element, add a selector component.
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

SourceRange DestructorDecl::getSourceRange() const {
  if (getBodyKind() == BodyKind::Unparsed ||
      getBodyKind() == BodyKind::Skipped)
    return { getDestructorLoc(), BodyEndLoc };

  if (getBodyKind() == BodyKind::None)
    return getDestructorLoc();

  return { getDestructorLoc(), Body->getEndLoc() };
}
