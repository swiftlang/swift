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
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Expr.h"
#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/raw_ostream.h"
#include "swift/Basic/Range.h"

#include "clang/AST/DeclObjC.h"

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

  // If this is an accessor, check if the transparent attribute was set
  // on the value decl.
  if (const FuncDecl *FD = dyn_cast<FuncDecl>(this)) {
    if (auto *ASD = FD->getAccessorStorageDecl())
      return ASD->isTransparent();
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
  if (auto arch = t->getAs<ArchetypeType>())
    return arch->getAsDependentType(archetypeMap);
  return t;
}

// Helper to translate a RequirementRepr into a canonical Requirement expressed
// in terms of dependent types.
static void
addRequirementForRepr(SmallVectorImpl<Requirement> &requirements,
                      const RequirementRepr &repr,
                     const llvm::DenseMap<ArchetypeType*, Type> &archetypeMap) {
  switch (repr.getKind()) {
  case RequirementKind::Conformance: {
    // Primary conformance declarations would have already been gathered as
    // conformance requirements off the archetype.
    if (auto arch = repr.getSubject()->getAs<ArchetypeType>()) {
      if (!arch->getParent())
        return;
    }
    Requirement reqt(RequirementKind::Conformance,
                     getAsDependentType(repr.getSubject(), archetypeMap),
                     getAsDependentType(repr.getConstraint(), archetypeMap));
    requirements.push_back(reqt);
    return;
  }
  case RequirementKind::SameType: {
    // FIXME: ArchetypeBuilder doesn't preserve the distinction between the
    // matched archetypes, so this ends up producing useless '$T == $T'
    // requirements.
    /*
    Requirement reqt(RequirementKind::SameType,
                     getAsDependentType(repr.getFirstType(), archetypeMap),
                     getAsDependentType(repr.getSecondType(), archetypeMap));
    requirements.push_back(reqt);
     */
    return;
  }
  case RequirementKind::WitnessMarker:
    llvm_unreachable("should not exist after typechecking (?)");
  }
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
    
    GenericTypeParamDecl *typeParam = param.getAsTypeParam();
    auto typeParamTy = typeParam->getDeclaredType()
      ->castTo<GenericTypeParamType>();

    // Make sure we didn't visit this param already in the parent.
    auto found = archetypeMap.find(typeParam->getArchetype());
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
  }
  
  // Collect requirements from the 'where' clause.
  for (const auto &repr : getRequirements()) {
    addRequirementForRepr(requirements, repr, archetypeMap);
  }
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
}

Optional<ImportKind>
ImportDecl::findBestImportKind(ArrayRef<ValueDecl *> Decls) {
  assert(!Decls.empty());
  ImportKind FirstKind = ImportDecl::getBestImportKind(Decls.front());

  // Only functions can be overloaded.
  if (Decls.size() == 1)
    return FirstKind;
  if (FirstKind != ImportKind::Func)
    return Nothing;

  for (auto NextDecl : Decls.slice(1)) {
    if (ImportDecl::getBestImportKind(NextDecl) != FirstKind)
      return Nothing;
  }

  return FirstKind;
}

void ExtensionDecl::setConformances(ArrayRef<ProtocolConformance *> c) {
  Conformances = c;
}

GenericParamList *ExtensionDecl::getGenericParams() const {
  auto extendedType = getExtendedType();
  if (auto nominalDecl = extendedType->getNominalOrBoundGenericNominal()) {
    return nominalDecl->getGenericParamsOfContext();
  }
  return nullptr;
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
      if (isa<ClassDecl>(NTD) || isa<ProtocolDecl>(NTD))
        StaticSpelling = StaticSpellingKind::KeywordClass;
    }
  }
  return StaticSpelling;
}

StaticSpellingKind PatternBindingDecl::getCorrectStaticSpelling() const {
  return getCorrectStaticSpellingForDecl(this);
}

SourceLoc TopLevelCodeDecl::getStartLoc() const {
  return Body->getStartLoc();
}

SourceRange TopLevelCodeDecl::getSourceRange() const {
  return Body->getSourceRange();
}

SourceRange IfConfigDecl::getSourceRange() const {
  return SourceRange(IfLoc, EndLoc);
}

/// Return true if a DeclRefExpr or MemberRefExpr use of this value is
/// "direct" when being used in the specified context.
bool ValueDecl::isUseFromContextDirect(const DeclContext *UseDC) const {
  // Observing member are accessed directly from within their didSet/willSet
  // specifiers.  This prevents assignments from becoming infinite loops.
  if (auto *var = dyn_cast<AbstractStorageDecl>(this))
    if (auto *UseFD = dyn_cast<FuncDecl>(UseDC))
      if (var->hasStorage() && var->hasAccessorFunctions() &&
          UseFD->getAccessorStorageDecl() == var)
        return true;
  
  return false;
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
  if (auto fd = dyn_cast<FuncDecl>(this))
    return fd->getOverriddenDecl();
  if (auto sdd = dyn_cast<AbstractStorageDecl>(this))
    return sdd->getOverriddenDecl();
  if (auto cd = dyn_cast<ConstructorDecl>(this))
    return cd->getOverriddenDecl();
  return nullptr;
}

bool ValueDecl::canBeAccessedByDynamicLookup() const {
  if (!hasName())
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
  if (auto TAD = dyn_cast<TypeAliasDecl>(this))
    return TAD->getAliasType();
  if (auto typeParam = dyn_cast<AbstractTypeParamDecl>(this))
    return typeParam->getType()->castTo<MetatypeType>()->getInstanceType();
  return cast<NominalTypeDecl>(this)->getDeclaredType();
}

Type TypeDecl::getDeclaredInterfaceType() const {
  return getInterfaceType()->castTo<MetatypeType>()->getInstanceType();
}

/// Provide the set of parameters to a generic type, or null if
/// this function is not generic.
void NominalTypeDecl::setGenericParams(GenericParamList *params) {
  assert(!GenericParams && "Already has generic parameters");
  GenericParams = params;
  
  if (params)
    for (auto Param : *params)
      Param.setDeclContext(this);
}


bool NominalTypeDecl::derivesProtocolConformance(ProtocolDecl *protocol) const {
  // Enums with raw types can derive their RawRepresentable conformance.
  if (auto *enumDecl = dyn_cast<EnumDecl>(this)) {
    auto rawRepresentable
      = getASTContext().getProtocol(KnownProtocolKind::RawRepresentable);
    return enumDecl->hasRawType() && protocol == rawRepresentable;
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
  // If this is a class, it conforms to the DynamicLookup protocol.
  if (isa<ClassDecl>(this)) {
    if (auto dynamicLookup
          = getASTContext().getProtocol(KnownProtocolKind::DynamicLookup)) {
      protocols.push_back(dynamicLookup);
    }
  }
}

OptionalTypeKind NominalTypeDecl::classifyAsOptionalType() const {
  const ASTContext &ctx = getASTContext();
  if (this == ctx.getOptionalDecl()) {
    return OTK_Optional;
  } else if (this == ctx.getUncheckedOptionalDecl()) {
    return OTK_UncheckedOptional;
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
  ClassDeclBits.RequiresStoredPropertyInits = 0;
}

DestructorDecl *ClassDecl::getDestructor() {
  auto name = getASTContext().Id_destructor;
  auto results = lookupDirect(name);
  assert(!results.empty() && "Class without destructor?");
  assert(results.size() == 1 && "More than one destructor?");
  return cast<DestructorDecl>(results.front());
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

void AbstractStorageDecl::makeComputed(SourceLoc LBraceLoc,
                                       FuncDecl *Get, FuncDecl *Set,
                                       SourceLoc RBraceLoc) {
  assert(getStorageKind() == Stored && "VarDecl StorageKind already set");
  auto &Context = getASTContext();
  void *Mem = Context.Allocate(sizeof(GetSetRecord), alignof(GetSetRecord));
  GetSetInfo = new (Mem) GetSetRecord();
  GetSetInfo->Braces = SourceRange(LBraceLoc, RBraceLoc);
  GetSetInfo->Get = Get;
  GetSetInfo->Set = Set;
  
  if (Get)
    Get->makeAccessor(this, AccessorKind::IsGetter);
  if (Set)
    Set->makeAccessor(this, AccessorKind::IsSetter);
  
  // Mark that this is a computed property.
  setStorageKind(Computed);
}

/// \brief Turn this into a StoredWithTrivialAccessors var, specifying the
/// accessors (getter and setter) that go with it.
void AbstractStorageDecl::makeStoredWithTrivialAccessors(FuncDecl *Get,
                                                         FuncDecl *Set) {
  assert(getStorageKind() == Stored && "VarDecl StorageKind already set");
  assert(Get);
  auto &Context = getASTContext();
  void *Mem = Context.Allocate(sizeof(GetSetRecord), alignof(GetSetRecord));
  GetSetInfo = new (Mem) GetSetRecord();
  GetSetInfo->Braces = SourceRange();
  GetSetInfo->Get = Get;
  GetSetInfo->Set = Set;
  
  Get->makeAccessor(this, AccessorKind::IsGetter);
  if (Set) Set->makeAccessor(this, AccessorKind::IsSetter);
  
  // Mark that this is a StoredWithTrivialAccessors property.
  setStorageKind(StoredWithTrivialAccessors);
}

void AbstractStorageDecl::makeObserving(SourceLoc LBraceLoc,
                                            FuncDecl *WillSet, FuncDecl *DidSet,
                                            SourceLoc RBraceLoc) {
  assert(getStorageKind() == Stored && "VarDecl StorageKind already set");
  assert((WillSet || DidSet) &&
         "Can't be Observing without one or the other");
  auto &Context = getASTContext();
  void *Mem = Context.Allocate(sizeof(ObservingRecord),
                               alignof(ObservingRecord));
  GetSetInfo = new (Mem) ObservingRecord;
  GetSetInfo->Braces = SourceRange(LBraceLoc, RBraceLoc);

  // Mark that this is a Observing property.
  setStorageKind(Observing);

  getDidSetInfo().WillSet = WillSet;
  getDidSetInfo().DidSet = DidSet;
  
  if (WillSet) WillSet->makeAccessor(this, AccessorKind::IsWillSet);
  if (DidSet) DidSet->makeAccessor(this, AccessorKind::IsDidSet);
}

/// \brief Specify the synthesized get/set functions for a Observing var.
/// This is used by Sema.
void AbstractStorageDecl::setObservingAccessors(FuncDecl *Get,
                                                    FuncDecl *Set) {
  assert(getStorageKind() == Observing && "VarDecl is wrong type");
  assert(!getGetter() && !getSetter() && "getter and setter already set");
  assert(Get && Set && "Must specify getter and setter");

  GetSetInfo->Get = Get;
  GetSetInfo->Set = Set;

  Get->makeAccessor(this, AccessorKind::IsGetter);
  Set->makeAccessor(this, AccessorKind::IsSetter);
}

StringRef AbstractStorageDecl::
getObjCGetterSelector(SmallVectorImpl<char> &buffer) const {
  // If we override a property, use its getter selector.
  if (auto overridden = getOverriddenDecl())
    return overridden->getObjCGetterSelector(buffer);

  llvm::raw_svector_ostream out(buffer);

  // Subscripts use a specific selector.
  if (auto *SD = dyn_cast<SubscriptDecl>(this)) {
    switch (SD->getObjCSubscriptKind()) {
    case ObjCSubscriptKind::None:
      llvm_unreachable("Not an Objective-C subscript");
    case ObjCSubscriptKind::Indexed:
      out << "objectAtIndexedSubscript:";
      break;
    case ObjCSubscriptKind::Keyed:
      out << "objectForKeyedSubscript:";
      break;
    }

    return out.str();
  }

  // If there is an Objective-C @property declaration, use its getter
  // name.
  if (auto objc = dyn_cast_or_null<clang::ObjCPropertyDecl>(getClangDecl())) {
    auto selector = objc->getGetterName();
    if (!selector.isNull()) {
      selector.print(out);
      return out.str();
    }
  }

  // The getter selector is the property name itself.
  // FIXME: 'is' prefix for boolean properties?
  out << getName().str();
  return out.str();
}


StringRef AbstractStorageDecl::getObjCSetterSelector(SmallVectorImpl<char> &buffer) const {
  // If we override a property, use its setter selector.
  if (auto overridden = getOverriddenDecl())
    return overridden->getObjCSetterSelector(buffer);

  llvm::raw_svector_ostream out(buffer);

  // Subscripts use a specific selector.
  if (auto *SD = dyn_cast<SubscriptDecl>(this)) {
    switch (SD->getObjCSubscriptKind()) {
    case ObjCSubscriptKind::None:
      llvm_unreachable("Not an Objective-C subscript");

    case ObjCSubscriptKind::Indexed:
      out << "setObject:atIndexedSubscript:";
      break;
    case ObjCSubscriptKind::Keyed:
      out << "setObject:forKeyedSubscript:";
      break;
    }

    return out.str();
  }
  

  // If there is an Objective-C @property declaration, use its setter
  // name.
  if (auto objc = dyn_cast_or_null<clang::ObjCPropertyDecl>(getClangDecl())) {
    auto selector = objc->getSetterName();
    if (!selector.isNull()) {
      selector.print(out);
      return out.str();
    }
  }

  // The setter selector for, e.g., 'fooBar' is 'setFooBar:', with the
  // property name capitalized and preceded by 'set'.
  StringRef name = getName().str();
  assert(name.size() >= 1 && "empty var name?!");
  
  out << "set" << char(toupper(name[0])) << name.slice(1, name.size()) << ':';
  return out.str();
}


/// \brief Returns whether the var is settable in the specified context: this
/// is either because it is a stored var, because it has a custom setter, or
/// is a let member in an initializer.
bool VarDecl::isSettable(DeclContext *UseDC) const {
  // 'val' properties are generally immutable, unless they are a 'val' ivar
  // and we are in the init() for the type that holds the ivar.
  if (isVal()) {
    if (auto *CD = dyn_cast_or_null<ConstructorDecl>(UseDC))
      if (CD->getDeclContext() == getDeclContext())
        return true;

    return false;
  }
  
  // Observing properties are not mutable in their willset accessor, since any
  // value stored in the willSet will be immediately overwritten by the store
  // in progress.
  if (getStorageKind() == Observing && getWillSetFunc() == UseDC)
    return false;
  

  // vars are settable unless they are computed and have no setter.
  return hasStorage() || getSetter();
}

SourceRange VarDecl::getTypeSourceRangeForDiagnostics() const {
  if (!getParentPattern())
    return getSourceRange();

  auto *Pat = getParentPattern()->getPattern();
  if (auto *VP = dyn_cast<VarPattern>(Pat))
    Pat = VP->getSubPattern();
  if (auto *TP = dyn_cast<TypedPattern>(Pat))
    return TP->getTypeLoc().getTypeRepr()->getSourceRange();
  return getSourceRange();
}

/// Return true if this stored property needs to be accessed with getters and
/// setters for Objective-C.
bool AbstractStorageDecl::usesObjCGetterAndSetter() const {
  // We don't export generic methods or subclasses to IRGen yet.
  auto *DC = getDeclContext();
  if (DC->getDeclaredTypeInContext() &&
      DC->getDeclaredTypeInContext()->is<BoundGenericType>() &&
      !isa<ProtocolDecl>(DC))
    return false;

  if (auto override = getOverriddenDecl())
    return override->usesObjCGetterAndSetter();

  if (!isObjC())
    return false;

  // Don't expose objc properties for variables with function type. We can't
  // autorelease them, and eventually we want to map them back to blocks.
  if (isa<VarDecl>(this) && getType()->is<AnyFunctionType>())
    return false;

  return true;
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
  return getCorrectStaticSpellingForDecl(this);
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
  if (auto metaTy = type->getAs<MetatypeType>())
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
  if(selfTy->is<ErrorType>())
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
  //
  // With non-mutating methods on value types, we generally pass the value
  // directly in at +1.  The exception is for protocol methods, which we pass
  // inout at +0. We handle the abstraction difference in the witness thunk for
  // the received method, where we know the concrete receiver type.  We do this
  // by having existential_member_ref and archetype_member_ref take the 'self'
  // base object as an rvalue for @!mutating protocol members, even though that
  // doesn't match the type of the protocol requirement.
  if (isMutating || isa<ProtocolDecl>(dc))
    return InOutType::get(selfTy);
  
  // Non-mutating methods on structs and enums pass the receiver by value.
  return selfTy;
}

void AbstractFunctionDecl::setGenericParams(GenericParamList *GP) {
  // Set the specified generic parameters onto this abstract function, setting
  // the parameters' context to the function along the way.
  GenericParams = GP;
  if (GP)
    for (auto Param : *GP)
      Param.setDeclContext(this);
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
  if (NP && NP->isImplicit() && NP->getBoundName() == getASTContext().Id_self)
    return NP->getDecl();
  return nullptr;
}

Type AbstractFunctionDecl::getExtensionType() const {
  return getDeclContext()->getDeclaredTypeInContext();
}

std::pair<DefaultArgumentKind, Type>
AbstractFunctionDecl::getDefaultArg(unsigned Index) const {
  ArrayRef<const Pattern *> Patterns = getArgParamPatterns();

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

SourceRange AbstractFunctionDecl::getBodySourceRange() const {
  switch (getBodyKind()) {
  case BodyKind::None:
    return SourceRange();

  case BodyKind::Parsed:
    if (auto body = getBody())
      return body->getSourceRange();

    return SourceRange();

  case BodyKind::Skipped:
  case BodyKind::Unparsed:
    return BodyRange;
  }
}

StringRef AbstractFunctionDecl::getObjCSelector(
            SmallVectorImpl<char> &buffer) const {
  if (auto func = dyn_cast<FuncDecl>(this))
    return func->getObjCSelector(buffer);
  if (auto ctor = dyn_cast<ConstructorDecl>(this))
    return ctor->getObjCSelector(buffer);
  if (auto dtor = dyn_cast<DestructorDecl>(this))
    return dtor->getObjCSelector(buffer);
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
    VD->setDeclContext(DC);
  });
}

FuncDecl *FuncDecl::createDeserialized(ASTContext &Context,
                                       SourceLoc StaticLoc,
                                       StaticSpellingKind StaticSpelling,
                                       SourceLoc FuncLoc,
                                       Identifier Name, SourceLoc NameLoc,
                                       GenericParamList *GenericParams,
                                       Type Ty, unsigned NumParamPatterns,
                                       DeclContext *Parent) {
  assert(NumParamPatterns > 0);
  void *Mem = Context.Allocate(
      sizeof(FuncDecl) + 2 * NumParamPatterns * sizeof(Pattern *),
      alignof(FuncDecl));
  return ::new (Mem)
      FuncDecl(StaticLoc, StaticSpelling, FuncLoc, Name, NameLoc,
               NumParamPatterns, GenericParams, Ty, Parent);
}

FuncDecl *FuncDecl::create(ASTContext &Context, SourceLoc StaticLoc,
                           StaticSpellingKind StaticSpelling,
                           SourceLoc FuncLoc, Identifier Name,
                           SourceLoc NameLoc, GenericParamList *GenericParams,
                           Type Ty, ArrayRef<Pattern *> ArgParams,
                           ArrayRef<Pattern *> BodyParams,
                           TypeLoc FnRetType, DeclContext *Parent) {
  assert(ArgParams.size() == BodyParams.size());
  const unsigned NumParamPatterns = ArgParams.size();
  auto *FD = FuncDecl::createDeserialized(
      Context, StaticLoc, StaticSpelling, FuncLoc, Name, NameLoc,
      GenericParams, Ty, NumParamPatterns, Parent);
  FD->setDeserializedSignature(ArgParams, BodyParams, FnRetType);
  return FD;
}

StaticSpellingKind FuncDecl::getCorrectStaticSpelling() const {
  return getCorrectStaticSpellingForDecl(this);
}

void FuncDecl::setDeserializedSignature(ArrayRef<Pattern *> ArgParams,
                                        ArrayRef<Pattern *> BodyParams,
                                        TypeLoc FnRetType) {
  MutableArrayRef<Pattern *> ArgParamsRef = getArgParamPatterns();
  MutableArrayRef<Pattern *> BodyParamsRef = getBodyParamPatterns();
  unsigned NumParamPatterns = ArgParamsRef.size();

  assert(ArgParams.size() == BodyParams.size());
  assert(NumParamPatterns == ArgParams.size());

  for (unsigned i = 0; i != NumParamPatterns; ++i)
    ArgParamsRef[i] = ArgParams[i];
  for (unsigned i = 0; i != NumParamPatterns; ++i)
    BodyParamsRef[i] = BodyParams[i];

  // Set the decl context of any vardecls to this FuncDecl.
  for (auto P : ArgParams)
    setDeclContextOfPatternVars(P, this);

  if (BodyParams != ArgParams) {
    for (auto P : BodyParams)
      setDeclContextOfPatternVars(P, this);
  }

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

ConstructorDecl::ConstructorDecl(Identifier NameHack, SourceLoc ConstructorLoc,
                                 Pattern *SelfArgParam, Pattern *ArgParams,
                                 Pattern *SelfBodyParam, Pattern *BodyParams,
                                 GenericParamList *GenericParams,
                                 DeclContext *Parent)
  : AbstractFunctionDecl(DeclKind::Constructor, Parent, NameHack,
                         ConstructorLoc, 2, GenericParams) {
  setArgParams(SelfArgParam, ArgParams);
  setBodyParams(SelfBodyParam, BodyParams);
  
  ConstructorDeclBits.ComputedBodyInitKind = 0;
  ConstructorDeclBits.Abstract = 0;
}

void ConstructorDecl::setArgParams(Pattern *selfPattern, Pattern *argParams) {
  ArgParams[0] = selfPattern;
  ArgParams[1] = argParams;
  setDeclContextOfPatternVars(selfPattern, this);
  setDeclContextOfPatternVars(argParams, this);
}

void ConstructorDecl::setBodyParams(Pattern *selfPattern, Pattern *bodyParams) {
  BodyParams[0] = selfPattern;
  BodyParams[1] = bodyParams;
  setDeclContextOfPatternVars(selfPattern, this);
  setDeclContextOfPatternVars(bodyParams, this);
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
StringRef FuncDecl::getObjCSelector(SmallVectorImpl<char> &buffer) const {
  // For a getter or setter, go through the variable or subscript decl.
  if (isGetterOrSetter()) {
    auto asd = cast<AbstractStorageDecl>(getAccessorStorageDecl());
    return isGetter() ? asd->getObjCGetterSelector(buffer)
                      : asd->getObjCSetterSelector(buffer);
  }

  assert(buffer.empty());
  
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
  SourceLoc StartLoc = getStartLoc();
  if (StartLoc.isInvalid()) return SourceRange();

  if (getBodyKind() == BodyKind::Unparsed ||
      getBodyKind() == BodyKind::Skipped)
    return { StartLoc, BodyRange.End };

  if (auto *B = getBody())
    return { StartLoc, B->getEndLoc() };
  if (getBodyResultTypeLoc().hasLocation())
    return { StartLoc, getBodyResultTypeLoc().getSourceRange().End };
  const Pattern *LastPat = getArgParamPatterns().back();
  return { StartLoc, LastPat->getEndLoc() };
}

SourceRange EnumElementDecl::getSourceRange() const {
  if (RawValueExpr && !RawValueExpr->isImplicit())
    return {getStartLoc(), RawValueExpr->getEndLoc()};
  if (ArgumentType.hasLocation())
    return {getStartLoc(), ArgumentType.getSourceRange().End};
  return {getStartLoc(), getNameLoc()};
}
SourceRange ConstructorDecl::getSourceRange() const {
  if (getBodyKind() == BodyKind::Unparsed ||
      getBodyKind() == BodyKind::Skipped)
    return { getConstructorLoc(), BodyRange.End };

  if (!Body || !Body->getEndLoc().isValid()) {
    const DeclContext *DC = getDeclContext();
    switch (DC->getContextKind()) {
    case DeclContextKind::ExtensionDecl:
      return cast<ExtensionDecl>(DC)->getSourceRange();
    case DeclContextKind::NominalTypeDecl:
      return cast<NominalTypeDecl>(DC)->getSourceRange();
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

  // If there are no parameters, this is just 'init()'.
  auto tuple = cast<TuplePattern>(getArgParamPatterns()[1]);
  if (tuple->getNumFields() == 0)
    return out.str();

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

ConstructorDecl::BodyInitKind
ConstructorDecl::getDelegatingOrChainedInitKind(DiagnosticEngine *diags,
                                                Expr **init) {
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
    Expr *InitExpr = nullptr;
    DiagnosticEngine *Diags;

    FindReferenceToInitializer(DiagnosticEngine *diags) : Diags(diags) { }

    std::pair<bool, Expr*> walkToExprPre(Expr *E) override {
      if (auto apply = dyn_cast<ApplyExpr>(E)) {
        if (isa<OtherConstructorDeclRefExpr>(
              apply->getFn()->getSemanticsProvidingExpr())) {
          BodyInitKind myKind;
          if (isa<SuperRefExpr>(apply->getArg()->getSemanticsProvidingExpr()))
            myKind = BodyInitKind::Chained;
          else
            myKind = BodyInitKind::Delegating;

          if (Kind == BodyInitKind::None) {
            Kind = myKind;

            // If we're not emitting diagnostics, we're done.
            if (!Diags) {
              return { false, nullptr };
            }

            InitExpr = E;
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
      }

      // Don't walk into closures.
      if (isa<ClosureExpr>(E))
        return { false, E };

      return { true, E };
    }
  } finder(diags);
  getBody()->walk(finder);

  // If we didn't find any delegating or chained initializers, check whether
  // we have a class with a superclass: it gets an implicit chained initializer.
  if (finder.Kind == BodyInitKind::None) {
    if (auto classDecl = getDeclContext()->getDeclaredTypeInContext()
                           ->getClassOrBoundGenericClass()) {
      if (classDecl->getSuperclass())
        finder.Kind = BodyInitKind::ImplicitChained;
    }
  }

  // Cache the result.
  ConstructorDeclBits.ComputedBodyInitKind
    = static_cast<unsigned>(finder.Kind) + 1;
  if (init)
    *init = finder.InitExpr;

  return finder.Kind;
}

StringRef DestructorDecl::getObjCSelector(SmallVectorImpl<char> &buffer) const {
  return "dealloc";
}

SourceRange DestructorDecl::getSourceRange() const {
  if (getBodyKind() == BodyKind::Unparsed ||
      getBodyKind() == BodyKind::Skipped)
    return { getDestructorLoc(), BodyRange.End };

  if (getBodyKind() == BodyKind::None)
    return getDestructorLoc();

  return { getDestructorLoc(), Body->getEndLoc() };
}
