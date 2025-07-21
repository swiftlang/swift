//===--- Type.cpp - Swift Language Type ASTs ------------------------------===//
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
//  This file implements the Type class and subclasses.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "ast-types"

#include "clang/AST/Type.h"
#include "ForeignRepresentationInfo.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/Concurrency.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/PackConformance.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ReferenceCounting.h"
#include "swift/AST/SILLayout.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeLoc.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/TypeTransform.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/TypeWalker.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Compiler.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <functional>
#include <iterator>
using namespace swift;

#define TYPE(Id, _) \
  static_assert(IsTriviallyDestructible<Id##Type>::value, \
                "Types are BumpPtrAllocated; the destructor is never called");
#include "swift/AST/TypeNodes.def"

void TypeLoc::setType(Type Ty) {
  assert(!Ty || !Ty->hasTypeVariable() || !Ty->hasPlaceholder());
  this->Ty = Ty;
}

bool TypeLoc::isError() const {
  assert(wasValidated() && "Type not yet validated");
  return getType()->hasError();
}

SourceRange TypeLoc::getSourceRange() const {
  if (TyR)
    return TyR->getSourceRange();
  return SourceRange();
}

SourceLoc TypeLoc::getLoc() const {
  if (TyR) return TyR->getLoc();
  return SourceLoc();
}

NominalTypeDecl *CanType::getAnyNominal() const {
  return dyn_cast_or_null<NominalTypeDecl>(getAnyGeneric());
}

GenericTypeDecl *CanType::getAnyGeneric() const {
  // FIXME: Remove checking for existential types. `getAnyGeneric` should return
  // the GenericTypeDecl the type is directly bound to.
  if (auto existential = dyn_cast<ExistentialType>(*this))
    return existential->getConstraintType()->getAnyGeneric();
  if (auto ppt = dyn_cast<ParameterizedProtocolType>(*this))
    return ppt->getBaseType()->getDecl();
  if (auto Ty = dyn_cast<AnyGenericType>(*this))
    return Ty->getDecl();
  return nullptr;
}

//===----------------------------------------------------------------------===//
// Various Type Methods.
//===----------------------------------------------------------------------===//

/// isEqual - Return true if these two types are equal, ignoring sugar.
bool TypeBase::isEqual(Type Other) const {
  return getCanonicalType() == Other.getPointer()->getCanonicalType();
}

/// hasReferenceSemantics - Does this type have reference semantics?
bool TypeBase::hasReferenceSemantics() {
  return getCanonicalType().hasReferenceSemantics();
}

bool TypeBase::isUninhabited() {
  // Empty enum declarations are uninhabited
  if (auto nominalDecl = getAnyNominal())
    if (auto enumDecl = dyn_cast<EnumDecl>(nominalDecl))
      // Objective-C enums may be allowed to hold any value representable by
      // the underlying type, but only if they come from clang.
      if (enumDecl->getAllElements().empty() &&
          !(enumDecl->isObjC() && enumDecl->hasClangNode()))
        return true;
  return false;
}

bool TypeBase::isStructurallyUninhabited() {
  if (isUninhabited()) return true;
  
  // Tuples of uninhabited types are uninhabited
  if (auto *TTy = getAs<TupleType>())
    for (auto eltTy : TTy->getElementTypes())
      if (eltTy->isStructurallyUninhabited())
        return true;
  return false;
}

bool TypeBase::isAny() {
  Type constraint = this;
  if (auto existential = constraint->getAs<ExistentialType>())
    constraint = existential->getConstraintType();
  return constraint->isEqual(getASTContext().TheAnyType);
}

bool TypeBase::isMarkerExistential() {
  Type constraint = this;
  if (auto existential = constraint->getAs<ExistentialType>())
    constraint = existential->getConstraintType();

  if (!constraint->isConstraintType())
    return false;

  auto layout = constraint->getExistentialLayout();
  if (layout.hasExplicitAnyObject ||
      layout.explicitSuperclass) {
    return false;
  }

  for (auto *proto : layout.getProtocols()) {
    if (!proto->isMarkerProtocol())
      return false;
  }

  return true;
}

bool TypeBase::isSendableExistential() {
  Type constraint = this;
  if (auto existential = constraint->getAs<ExistentialType>())
    constraint = existential->getConstraintType();

  if (!constraint->isConstraintType())
    return false;

  return constraint->getKnownProtocol() == KnownProtocolKind::Sendable;
}

bool TypeBase::isPlaceholder() {
  return is<PlaceholderType>();
}

bool TypeBase::isAnyClassReferenceType() {
  return getCanonicalType().isAnyClassReferenceType();
}

bool CanType::isReferenceTypeImpl(CanType type, const GenericSignatureImpl *sig,
                                  bool functionsCount) {
  switch (type->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("sugared canonical type?");

  // These types are always class references.
  case TypeKind::BuiltinNativeObject:
  case TypeKind::BuiltinBridgeObject:
  case TypeKind::Class:
  case TypeKind::BoundGenericClass:
  case TypeKind::SILBox:
    return true;

  // For Self types, recur on the underlying type.
  case TypeKind::DynamicSelf:
    return isReferenceTypeImpl(cast<DynamicSelfType>(type).getSelfType(),
                               sig, functionsCount);
  case TypeKind::SILMoveOnlyWrapped:
    return isReferenceTypeImpl(
        cast<SILMoveOnlyWrappedType>(type)->getInnerType(), sig,
        functionsCount);

  // Archetypes and existentials are only class references if class-bounded.
  case TypeKind::PrimaryArchetype:
  case TypeKind::ExistentialArchetype:
  case TypeKind::OpaqueTypeArchetype:
  case TypeKind::PackArchetype:
  case TypeKind::ElementArchetype:
    return cast<ArchetypeType>(type)->requiresClass();
  case TypeKind::Protocol:
    return cast<ProtocolType>(type)->requiresClass();
  case TypeKind::ProtocolComposition:
    return cast<ProtocolCompositionType>(type)->requiresClass();
  case TypeKind::ParameterizedProtocol:
    return cast<ParameterizedProtocolType>(type)->getBaseType()->requiresClass();
  case TypeKind::Existential:
    return isReferenceTypeImpl(cast<ExistentialType>(type).getConstraintType(),
                               sig, functionsCount);

  case TypeKind::UnboundGeneric:
    return isa<ClassDecl>(cast<UnboundGenericType>(type)->getDecl());

  // Functions have reference semantics, but are not class references.
  case TypeKind::Function:
  case TypeKind::GenericFunction:
  case TypeKind::SILFunction:
    return functionsCount;

  // Nothing else is statically just a class reference.
  case TypeKind::SILBlockStorage:
  case TypeKind::Error:
  case TypeKind::Unresolved:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinIntegerLiteral:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinRawUnsafeContinuation:
  case TypeKind::BuiltinJob:
  case TypeKind::BuiltinExecutor:
  case TypeKind::BuiltinDefaultActorStorage:
  case TypeKind::BuiltinNonDefaultDistributedActorStorage:
  case TypeKind::BuiltinPackIndex:
  case TypeKind::BuiltinUnsafeValueBuffer:
  case TypeKind::BuiltinVector:
  case TypeKind::Tuple:
  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Metatype:
  case TypeKind::ExistentialMetatype:
  case TypeKind::Module:
  case TypeKind::LValue:
  case TypeKind::InOut:
  case TypeKind::TypeVariable:
  case TypeKind::Placeholder:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct:
  case TypeKind::SILToken:
  case TypeKind::Pack:
  case TypeKind::PackExpansion:
  case TypeKind::PackElement:
  case TypeKind::SILPack:
  case TypeKind::BuiltinTuple:
  case TypeKind::ErrorUnion:
  case TypeKind::Integer:
  case TypeKind::BuiltinUnboundGeneric:
  case TypeKind::BuiltinFixedArray:
#define REF_STORAGE(Name, ...) \
  case TypeKind::Name##Storage:
#include "swift/AST/ReferenceStorage.def"
    return false;

  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
    assert(sig && "dependent types can't answer reference semantics query");
    return sig->requiresClass(type);
  }

  llvm_unreachable("Unhandled type kind!");
}

/// Are variables of this type permitted to have
/// ownership attributes?
///
/// This includes:
///   - class types, generic or not
///   - archetypes with class or class protocol bounds
///   - existentials with class or class protocol bounds
/// But not:
///   - function types
bool TypeBase::allowsOwnership(const GenericSignatureImpl *sig) {
  return getCanonicalType().allowsOwnership(sig);
}

static void expandDefaults(SmallVectorImpl<ProtocolDecl *> &protocols,
                           InvertibleProtocolSet inverses,
                           ASTContext &ctx) {
  for (auto ip : InvertibleProtocolSet::allKnown()) {
    if (!inverses.contains(ip)) {
      auto *proto = ctx.getProtocol(getKnownProtocolKind(ip));
      protocols.push_back(proto);
    }
  }

  ProtocolType::canonicalizeProtocols(protocols);
}

ExistentialLayout::ExistentialLayout(CanProtocolType type) {
  auto *protoDecl = type->getDecl();

  hasExplicitAnyObject = false;
  containsObjCProtocol = protoDecl->isObjC();
  containsSwiftProtocol = (!protoDecl->isObjC() &&
                           !protoDecl->isMarkerProtocol());
  representsAnyObject = false;

  inverses = InvertibleProtocolSet();

  protocols.push_back(protoDecl);
  expandDefaults(protocols, InvertibleProtocolSet(), type->getASTContext());
}

ExistentialLayout::ExistentialLayout(CanProtocolCompositionType type) {
  hasExplicitAnyObject = type->hasExplicitAnyObject();
  containsObjCProtocol = false;
  containsSwiftProtocol = false;

  auto members = type.getMembers();
  if (!members.empty() &&
      (members[0].getClassOrBoundGenericClass() ||
       isa<UnboundGenericType>(members[0]))) {
    explicitSuperclass = members[0];
    members = members.slice(1);
  }

  for (auto member : members) {
    ProtocolDecl *protoDecl;
    if (auto protocolType = dyn_cast<ProtocolType>(member)) {
      protoDecl = protocolType->getDecl();
    } else {
      auto *parameterizedType = member->castTo<ParameterizedProtocolType>();
      protoDecl = parameterizedType->getProtocol();
      parameterized.push_back(parameterizedType);
    }
    if (protoDecl->isObjC())
      containsObjCProtocol = true;
    else if (!protoDecl->isMarkerProtocol())
      containsSwiftProtocol = true;
    protocols.push_back(protoDecl);
  }

  inverses = type->getInverses();
  expandDefaults(protocols, inverses, type->getASTContext());

  representsAnyObject = [&]() {
    if (!hasExplicitAnyObject)
      return false;

    if (explicitSuperclass)
      return false;

    if (!inverses.empty())
      return false;

    for (auto *proto : protocols) {
      if (!proto->getInvertibleProtocolKind())
        return false;
    }

    return true;
  }();
}

ExistentialLayout::ExistentialLayout(CanParameterizedProtocolType type)
    : ExistentialLayout(type.getBaseType()) {
  parameterized.push_back(type);
}

ExistentialLayout TypeBase::getExistentialLayout() {
  return getCanonicalType().getExistentialLayout();
}

ExistentialLayout CanType::getExistentialLayout() {
  CanType ty = *this;

  // Always remove one layer of move only ness.
  if (auto mv = dyn_cast<SILMoveOnlyWrappedType>(ty))
    ty = mv->getInnerType();

  if (auto existential = dyn_cast<ExistentialType>(ty))
    return existential->getConstraintType()->getExistentialLayout();

  if (auto metatype = dyn_cast<ExistentialMetatypeType>(ty))
    return metatype->getInstanceType()->getExistentialLayout();

  if (auto proto = dyn_cast<ProtocolType>(ty))
    return ExistentialLayout(proto);

  if (auto param = dyn_cast<ParameterizedProtocolType>(ty))
    return ExistentialLayout(param);

  auto comp = cast<ProtocolCompositionType>(ty);
  return ExistentialLayout(comp);
}

bool ExistentialLayout::requiresClass() const {
  if (hasExplicitAnyObject || explicitSuperclass)
    return true;

  for (auto proto : getProtocols()) {
    if (proto->requiresClass())
      return true;
  }

  return false;
}

Type ExistentialLayout::getSuperclass() const {
  if (explicitSuperclass)
    return explicitSuperclass;

  for (auto protoDecl : getProtocols()) {
    auto genericSig = protoDecl->getGenericSignature();
    if (auto superclass = genericSig->getSuperclassBound(
          protoDecl->getSelfInterfaceType()))
      return superclass;
  }

  return Type();
}

bool ExistentialLayout::needsExtendedShape(bool allowInverses) const {
  if (!getParameterizedProtocols().empty())
    return true;

  if (allowInverses && hasInverses())
    return true;

  return false;
}

bool TypeBase::isObjCExistentialType() {
  return getCanonicalType().isObjCExistentialType();
}

bool TypeBase::isTypeErasedGenericClassType() {
  return getCanonicalType().isTypeErasedGenericClassType();
}

bool CanType::isObjCExistentialTypeImpl(CanType type) {
  if (!type.isExistentialType())
    return false;

  return type.getExistentialLayout().isObjC();
}

bool CanType::isTypeErasedGenericClassTypeImpl(CanType type) {
  if (auto nom = type->getAnyNominal())
    return nom->isTypeErasedGenericClass();
  return false;
}

NominalTypeDecl *TypeBase::getAnyActor() {
  // Nominal types: check whether the declaration is an actor.
  if (auto nominal = getAnyNominal()) {
    if (nominal->isAnyActor())
      return nominal;
  }

  // Archetypes check for conformance to Actor.
  if (auto archetype = getAs<ArchetypeType>()) {
    for (auto proto : archetype->getConformsTo()) {
      if (proto->isAnyActor())
        return proto;
    }

    return nullptr;
  }

  if (auto self = getAs<DynamicSelfType>()) {
    return self->getSelfType()->getAnyActor();
  }

  // Existential types: check for Actor protocol.
  if (isExistentialType()) {
    auto layout = getExistentialLayout();
    if (auto superclass = layout.getSuperclass()) {
      if (auto actor = superclass->getAnyActor())
        return actor;
    }

    for (auto proto : layout.getProtocols()) {
      if (proto->isAnyActor())
        return proto;
    }

    return nullptr;
  }

  return nullptr;
}

bool TypeBase::isActorType() {
  if (auto actor = getAnyActor())
    return actor->isActor();
  return false;
}

bool TypeBase::isAnyActorType() {
  if (auto actor = getAnyActor())
    return actor->isAnyActor();
  return false;
}

bool TypeBase::isDistributedActor() {
  if (auto actor = getAnyActor())
    return actor->isDistributedActor();
  return false;
}

std::optional<KnownProtocolKind> TypeBase::getKnownProtocol() {
  if (auto protoTy = this->getAs<ProtocolType>())
    if (auto protoDecl = protoTy->getDecl())
      return protoDecl->getKnownProtocolKind();

  return std::nullopt;
}

bool TypeBase::isSpecialized() {
  Type t = getCanonicalType();

  for (;;) {
    if (!t || !t->getAnyNominal())
      return false;
    if (t->is<BoundGenericType>())
      return true;
    t = t->getNominalParent();
  }

  return false;
}

bool TypeBase::hasLocalArchetypeFromEnvironment(
    GenericEnvironment *env) const {
  if (!hasLocalArchetype())
    return false;

  return getCanonicalType().findIf([&](Type type) -> bool {
    auto *local = dyn_cast<LocalArchetypeType>(type.getPointer());
    return local && local->getGenericEnvironment() == env;
  });
}

Type TypeBase::addCurriedSelfType(const DeclContext *dc) {
  if (!dc->isTypeContext())
    return this;

  auto *type = this;

  GenericSignature sig = dc->getGenericSignatureOfContext();
  if (auto *genericFn = type->getAs<GenericFunctionType>()) {
    sig = genericFn->getGenericSignature();
    type = FunctionType::get(genericFn->getParams(),
                             genericFn->getResult(),
                             genericFn->getExtInfo());
  }

  auto selfTy = dc->getSelfInterfaceType();
  auto selfParam = AnyFunctionType::Param(selfTy);
  // FIXME: Verify ExtInfo state is correct, not working by accident.
  if (sig) {
    GenericFunctionType::ExtInfo info;
    return GenericFunctionType::get(sig, {selfParam}, type, info);
  }
  FunctionType::ExtInfo info;
  return FunctionType::get({selfParam}, type, info);
}

void TypeBase::getTypeVariables(
    SmallPtrSetImpl<TypeVariableType *> &typeVariables) {
  // If we know we don't have any type variables, we're done.
  if (!hasTypeVariable())
    return;

  class Walker : public TypeWalker {
    SmallPtrSetImpl<TypeVariableType *> &typeVariables;

  public:
    explicit Walker(SmallPtrSetImpl<TypeVariableType *> &typeVariables)
        : typeVariables(typeVariables) {}

    Action walkToTypePre(Type ty) override {
      // Skip children that don't contain type variables.
      if (!ty->hasTypeVariable())
        return Action::SkipNode;

      if (auto tv = dyn_cast<TypeVariableType>(ty.getPointer())) {
        typeVariables.insert(tv);
      }

      return Action::Continue;
    }
  };

  Walker walker(typeVariables);
  Type(this).walk(walker);

  assert((!typeVariables.empty() || hasError()) &&
         "Did not find type variables!");
}

Type TypeBase::getDependentMemberRoot() {
  Type t(this);

  while (auto *dmt = t->getAs<DependentMemberType>())
    t = dmt->getBase();

  return t;
}

bool TypeBase::isTypeVariableOrMember() {
  return getDependentMemberRoot()->is<TypeVariableType>();
}

bool TypeBase::canBeExistential() {
  if (isAnyExistentialType())
    return true;

  Type ty(this);
  // Unwrap (potentially multiple levels of) metatypes.
  while (auto *mt = ty->getAs<MetatypeType>())
    ty = mt->getInstanceType();

  if (auto *archeTy = ty->getAs<ArchetypeType>()) {
    // Only if all conformances are self-conforming protocols, the archetype
    // may be an existential.
    for (auto *proto : archeTy->getConformsTo()) {
      if (!proto->existentialConformsToSelf())
        return false;
    }
    // If there are no requirements on the archetype at all (`getConformsTo`
    // is empty), the archetype can still be `Any` and we have to return true.
    return true;
  }
  return false;
}

bool TypeBase::isTypeParameter() {
  return getDependentMemberRoot()->is<GenericTypeParamType>();
}

GenericTypeParamType *TypeBase::getRootGenericParam() {
  return getDependentMemberRoot()->castTo<GenericTypeParamType>();
}

static bool isLegalSILType(CanType type);

static bool isLegalSILTypeOrPackExpansion(CanType type) {
  // Pack expansions aren't legal in arbitrary positions in SIL;
  // for example, we should never see a function parameter or result
  // of pack-expansion type.  But they're allowed within SILPackTypes
  // and SIL TupleTypes as long as their pattern types are legal.
  // The count type should always be an AST type.
  if (auto packExpansionType = dyn_cast<PackExpansionType>(type)) {
    return isLegalSILType(packExpansionType.getPatternType());
  }

  return isLegalSILType(type);
}

static bool isLegalSILType(CanType type) {
  // L-values and inouts are not legal.
  if (!type->isMaterializable()) return false;

  // Function types must be lowered.
  if (isa<AnyFunctionType>(type)) return false;

  // Metatypes must have a representation.
  if (auto meta = dyn_cast<AnyMetatypeType>(type))
    return meta->hasRepresentation();

  // Tuples are legal if all their elements are legal.
  if (auto tupleType = dyn_cast<TupleType>(type)) {
    for (auto eltType : tupleType.getElementTypes()) {
      if (!isLegalSILTypeOrPackExpansion(eltType)) return false;
    }
    return true;
  }

  // Packs must be lowered.
  if (isa<PackType>(type)) return false;

  // Optionals are legal if their object type is legal.
  if (auto objectType = type.getOptionalObjectType()) {
    return isLegalSILType(objectType);
  }

  // Reference storage types are legal if their object type is legal.
  if (auto refType = dyn_cast<ReferenceStorageType>(type))
    return isLegalSILType(refType.getReferentType());

  return true;
}

bool TypeBase::isLegalSILType() {
  return ::isLegalSILType(getCanonicalType());
}

static bool isLegalFormalType(CanType type) {
  // L-values and inouts are not formal types.
  if (!type->isMaterializable()) return false;

  // Function types must not be lowered.
  if (isa<SILFunctionType>(type)) return false;

  // Pack types must not be lowered.
  if (isa<SILPackType>(type)) return false;

  // Reference storage types are not formal types.
  if (isa<ReferenceStorageType>(type)) return false;

  // Metatypes must not have a representation.
  if (auto meta = dyn_cast<AnyMetatypeType>(type))
    return !meta->hasRepresentation();

  // Tuples are legal if all their elements are legal.
  if (auto tupleType = dyn_cast<TupleType>(type)) {
    for (auto eltType : tupleType.getElementTypes()) {
      if (!isLegalFormalType(eltType)) return false;
    }
    return true;
  }

  // Optionals are legal if their object type is legal.
  if (auto objectType = type.getOptionalObjectType()) {
    return isLegalFormalType(objectType);
  }

  // Expansions are legal if their pattern type is legal.
  if (auto expansionType = dyn_cast<PackExpansionType>(type)) {
    return isLegalFormalType(expansionType.getPatternType());
  }

  return true;
}

bool TypeBase::isLegalFormalType() {
  return ::isLegalFormalType(getCanonicalType());
}

bool TypeBase::hasTypeRepr() const {
  // A type has a source-printable representation if none of its sub-pieces do
  // /not/ have a source-printable representation.
  return !Type(const_cast<TypeBase *>(this)).findIf([](Type subTy) -> bool {
    switch (subTy->getKind()) {
    case TypeKind::Error:
    case TypeKind::Unresolved:
    case TypeKind::TypeVariable:
      return true;

    case TypeKind::ExistentialArchetype:
    case TypeKind::OpaqueTypeArchetype:
    case TypeKind::GenericFunction:
    case TypeKind::LValue:
      return true;

#define REF_STORAGE(Name, ...) \
    case TypeKind::Name##Storage: return true;
#include "swift/AST/ReferenceStorage.def"

    case TypeKind::SILFunction:
    case TypeKind::SILBlockStorage:
    case TypeKind::SILBox:
    case TypeKind::SILToken:
      return true;

    default:
      return false;
    }
  });
}

bool TypeBase::isVoid() {
  if (auto TT = getAs<TupleType>())
    return TT->getNumElements() == 0;
  return false;
}

#define KNOWN_STDLIB_TYPE_DECL(NAME, DECL_CLASS, NUM_GENERIC_PARAMS) \
/** Check if this type is equal to Swift.NAME. */ \
bool TypeBase::is##NAME() { \
  if (auto generic = getAnyGeneric()) { \
    if (isa<DECL_CLASS>(generic)) { \
      return getASTContext().get##NAME##Decl() == generic; \
    } \
  } \
  return false; \
}
#include "swift/AST/KnownStdlibTypes.def"

Type TypeBase::getRValueType() {
  // If the type is not an lvalue, this is a no-op.
  if (!hasLValueType())
    return this;

  return Type(this).transformRec([](TypeBase *t) -> std::optional<Type> {
      if (auto *lvalueTy = dyn_cast<LValueType>(t))
        return lvalueTy->getObjectType();
      return std::nullopt;
    });
}

Type TypeBase::getOptionalObjectType() {
  if (auto boundTy = getAs<BoundGenericEnumType>())
    if (boundTy->getDecl()->isOptionalDecl())
      return boundTy->getGenericArgs()[0];
  return Type();
}

CanType CanType::getOptionalObjectTypeImpl(CanType type) {
  if (auto boundTy = dyn_cast<BoundGenericEnumType>(type))
    if (boundTy->getDecl()->isOptionalDecl())
      return boundTy.getGenericArgs()[0];
  return CanType();
}

Type TypeBase::wrapInOptionalType() const {
  auto &ctx = getASTContext();
  auto *result = BoundGenericEnumType::get(ctx.getOptionalDecl(), Type(),
                                           { getCanonicalType() });
  return result;
}

CanType CanType::wrapInOptionalTypeImpl(CanType type) {
  return type->wrapInOptionalType()->getCanonicalType();
}

Type TypeBase::getArrayElementType() {
  if (!isArray())
    return Type();

  if (!is<BoundGenericStructType>())
    return Type();

  // Array<T>
  auto boundStruct = castTo<BoundGenericStructType>();
  return boundStruct->getGenericArgs()[0];
}

Type TypeBase::getInlineArrayElementType() {
  if (!isInlineArray())
    return Type();

  if (!is<BoundGenericStructType>())
    return Type();

  // InlineArray<n, T>
  auto boundStruct = castTo<BoundGenericStructType>();
  return boundStruct->getGenericArgs()[1];
}

Type TypeBase::getAnyPointerElementType(PointerTypeKind &PTK) {
  auto &C = getASTContext();
  if (isUnsafeMutableRawPointer()) {
    PTK = PTK_UnsafeMutableRawPointer;
    return C.TheEmptyTupleType;
  }
  if (isUnsafeRawPointer()) {
    PTK = PTK_UnsafeRawPointer;
    return C.TheEmptyTupleType;
  }
  if (auto boundTy = getAs<BoundGenericType>()) {
    if (boundTy->isUnsafeMutablePointer()) {
      PTK = PTK_UnsafeMutablePointer;
    } else if (boundTy->isUnsafePointer()) {
      PTK = PTK_UnsafePointer;
    } else if (boundTy->isAutoreleasingUnsafeMutablePointer()) {
      PTK = PTK_AutoreleasingUnsafeMutablePointer;
    } else {
      return Type();
    }
    return boundTy->getGenericArgs()[0];
  }
  return Type();
}

Type TypeBase::wrapInPointer(PointerTypeKind kind) {
  ASTContext &ctx = getASTContext();
  NominalTypeDecl *pointerDecl = ([&ctx, kind] {
    switch (kind) {
    case PTK_UnsafeMutableRawPointer:
    case PTK_UnsafeRawPointer:
      // these pointer types don't take arguments.
      return (NominalTypeDecl*)nullptr;
    case PTK_UnsafePointer:
      return ctx.getUnsafePointerDecl();
    case PTK_UnsafeMutablePointer:
      return ctx.getUnsafeMutablePointerDecl();
    case PTK_AutoreleasingUnsafeMutablePointer:
      return ctx.getAutoreleasingUnsafeMutablePointerDecl();
    }
    llvm_unreachable("bad kind");
  }());

  assert(pointerDecl);
  // Don't fail hard on null pointerDecl.
  if (!pointerDecl) {
    return Type();
  }
  return BoundGenericType::get(pointerDecl, /*parent*/nullptr, Type(this));
}

Type TypeBase::getAnyBufferPointerElementType(BufferPointerTypeKind &BPTK) {
  auto &C = getASTContext();
  if (isUnsafeMutableRawBufferPointer()) {
    BPTK = BPTK_UnsafeMutableRawBufferPointer;
    return C.TheEmptyTupleType;
  }
  if (isUnsafeRawBufferPointer()) {
    BPTK = BPTK_UnsafeRawBufferPointer;
    return C.TheEmptyTupleType;
  }
  if (auto boundTy = getAs<BoundGenericType>()) {
    if (boundTy->isUnsafeMutableBufferPointer()) {
      BPTK = BPTK_UnsafeMutableBufferPointer;
    } else if (boundTy->isUnsafeBufferPointer()) {
      BPTK = BPTK_UnsafeBufferPointer;
    } else {
      return Type();
    }
    return boundTy->getGenericArgs()[0];
  }
  return Type();
}

Type TypeBase::lookThroughSingleOptionalType() {
  Type type(this);
  if (auto objType = type->getOptionalObjectType())
    type = objType;
  return type;
}

Type TypeBase::lookThroughAllOptionalTypes() {
  Type type(this);
  while (auto objType = type->getOptionalObjectType())
    type = objType;

  return type;
}

Type TypeBase::lookThroughAllOptionalTypes(SmallVectorImpl<Type> &optionals){
  Type type(this);
  while (auto objType = type->getOptionalObjectType()) {
    optionals.push_back(type);
    type = objType;
  }

  return type;
}

unsigned int TypeBase::getOptionalityDepth() {
  Type type(this);
  unsigned int depth = 0;
  while (auto objType = type->getOptionalObjectType()) {
    type = objType;
    ++depth;
  }
  return depth;
}

Type TypeBase::stripConcurrency(bool recurse, bool dropGlobalActor) {
  // Look through optionals.
  if (Type optionalObject = getOptionalObjectType()) {
    Type newOptionalObject =
        optionalObject->stripConcurrency(recurse, dropGlobalActor);
    if (optionalObject->isEqual(newOptionalObject))
      return Type(this);

    return OptionalType::get(newOptionalObject);
  }

  // Function types.
  if (auto fnType = getAs<AnyFunctionType>()) {
    // Strip off Sendable and (possibly) the global actor.
    ASTExtInfo extInfo =
        fnType->hasExtInfo() ? fnType->getExtInfo() : ASTExtInfo();
    extInfo = extInfo.withSendable(false);
    if (dropGlobalActor)
      extInfo = extInfo.withoutIsolation();

    ArrayRef<AnyFunctionType::Param> params = fnType->getParams();
    Type resultType = fnType->getResult();

    SmallVector<AnyFunctionType::Param, 4> newParams;
    if (recurse) {
      for (unsigned paramIdx : indices(params)) {
        const auto &param = params[paramIdx];
        Type newParamType = param.getPlainType()->stripConcurrency(
            recurse, dropGlobalActor);

        if (!newParams.empty()) {
          newParams.push_back(param.withType(newParamType));
          continue;
        }

        if (newParamType->isEqual(param.getPlainType()))
          continue;

        newParams.append(params.begin(), params.begin() + paramIdx);
        newParams.push_back(param.withType(newParamType));
      }

      if (!newParams.empty())
        params = newParams;

      resultType = resultType->stripConcurrency(recurse, dropGlobalActor);
    }

    // Drop Sendable requirements.
    GenericSignature genericSig;
    if (auto genericFnType = dyn_cast<GenericFunctionType>(fnType)) {
      auto requirements = genericFnType->getRequirements();
      SmallVector<Requirement, 4> newRequirements;
      for (unsigned reqIdx : indices(requirements)) {
        // If it's a Sendable requirement, skip it.
        const auto &req = requirements[reqIdx];
        if (req.getKind() == RequirementKind::Conformance &&
            req.getProtocolDecl()
              ->isSpecificProtocol(KnownProtocolKind::Sendable))
          continue;

        newRequirements.push_back(req);
      }

      if (newRequirements.size() == requirements.size()) {
        genericSig = genericFnType->getGenericSignature();
      } else {
        genericSig = GenericSignature::get(
            genericFnType->getGenericParams(), newRequirements);
      }
    }

    Type newFnType;
    if (genericSig) {
      newFnType = GenericFunctionType::get(
          genericSig, params, resultType, extInfo);
    } else {
      newFnType = FunctionType::get(params, resultType, extInfo);
    }
    if (newFnType->isEqual(this))
      return Type(this);

    return newFnType;
  }

  if (auto existentialType = getAs<ExistentialType>()) {
    auto newConstraintType = existentialType->getConstraintType()
        ->stripConcurrency(recurse, dropGlobalActor);
    if (newConstraintType.getPointer() ==
            existentialType->getConstraintType().getPointer())
      return Type(this);

    if (newConstraintType->getClassOrBoundGenericClass())
      return newConstraintType;

    return ExistentialType::get(newConstraintType);
  }

  if (auto protocolType = getAs<ProtocolType>()) {
    if (protocolType->getDecl()->isSpecificProtocol(
            KnownProtocolKind::Sendable))
      return getASTContext().TheAnyType;

    return Type(this);
  }

  if (auto protocolCompositionType = getAs<ProtocolCompositionType>()) {
    SmallVector<Type, 4> newMembers;
    auto members = protocolCompositionType->getMembers();
    for (unsigned i : indices(members)) {
      auto memberType = members[i];
      auto newMemberType =
          memberType->stripConcurrency(recurse, dropGlobalActor);
      if (!newMembers.empty()) {
        newMembers.push_back(newMemberType);
        continue;
      }

      if (memberType.getPointer() != newMemberType.getPointer()) {
        newMembers.append(members.begin(), members.begin() + i);
        newMembers.push_back(newMemberType);
        continue;
      }
    }

    if (!newMembers.empty()) {
      return ProtocolCompositionType::get(
          getASTContext(), newMembers,
          protocolCompositionType->getInverses(),
          protocolCompositionType->hasExplicitAnyObject());
    }

    return Type(this);
  }

  if (auto existentialMetatype = getAs<ExistentialMetatypeType>()) {
    auto instanceType = existentialMetatype->getExistentialInstanceType();
    auto newInstanceType =
        instanceType->stripConcurrency(recurse, dropGlobalActor);
    if (instanceType.getPointer() != newInstanceType.getPointer()) {
      std::optional<MetatypeRepresentation> repr;
      if (existentialMetatype->hasRepresentation())
        repr = existentialMetatype->getRepresentation();
      return ExistentialMetatypeType::get(
          newInstanceType, repr, getASTContext());
    }

    return Type(this);
  }

  if (auto *BGT = getAs<BoundGenericType>()) {
    if (!recurse)
      return Type(this);

    bool anyChanged = false;
    SmallVector<Type, 2> genericArgs;
    llvm::transform(BGT->getGenericArgs(), std::back_inserter(genericArgs),
                    [&](Type argTy) {
                      auto newArgTy =
                          argTy->stripConcurrency(recurse, dropGlobalActor);
                      anyChanged |= !newArgTy->isEqual(argTy);
                      return newArgTy;
                    });

    return anyChanged ? BoundGenericType::get(BGT->getDecl(), BGT->getParent(),
                                              genericArgs)
                      : Type(this);
  }

  if (auto *tuple = getAs<TupleType>()) {
    if (!recurse)
      return Type(this);

    bool anyChanged = false;
    SmallVector<TupleTypeElt, 2> elts;
    llvm::transform(
        tuple->getElements(), std::back_inserter(elts), [&](const auto &elt) {
          auto eltTy = elt.getType();
          auto strippedTy = eltTy->stripConcurrency(recurse, dropGlobalActor);
          anyChanged |= !strippedTy->isEqual(eltTy);
          return elt.getWithType(strippedTy);
        });

    return anyChanged ? TupleType::get(elts, getASTContext()) : Type(this);
  }

  if (auto *arrayTy = dyn_cast<ArraySliceType>(this)) {
    auto newBaseTy =
        arrayTy->getBaseType()->stripConcurrency(recurse, dropGlobalActor);
    return newBaseTy->isEqual(arrayTy->getBaseType())
               ? Type(this)
               : ArraySliceType::get(newBaseTy);
  }

  if (auto *dictTy = dyn_cast<DictionaryType>(this)) {
    auto keyTy = dictTy->getKeyType();
    auto strippedKeyTy = keyTy->stripConcurrency(recurse, dropGlobalActor);
    auto valueTy = dictTy->getValueType();
    auto strippedValueTy = valueTy->stripConcurrency(recurse, dropGlobalActor);

    return keyTy->isEqual(strippedKeyTy) && valueTy->isEqual(strippedValueTy)
               ? Type(this)
               : DictionaryType::get(strippedKeyTy, strippedValueTy);
  }

  return Type(this);
}

bool TypeBase::isAnyObject() {
  auto canTy = getCanonicalType();

  if (!canTy.isExistentialType() || canTy.isForeignReferenceType())
    return false;

  return canTy.getExistentialLayout().isAnyObject();
}

bool ExistentialLayout::isErrorExistential() const {
  auto protocols = getProtocols();
  return (!hasExplicitAnyObject &&
          !explicitSuperclass &&
          protocols.size() == 1 &&
          protocols[0]->isSpecificProtocol(KnownProtocolKind::Error));
}

bool ExistentialLayout::isExistentialWithError(ASTContext &ctx) const {
  auto errorProto = ctx.getProtocol(KnownProtocolKind::Error);
  if (!errorProto) return false;

  for (auto protoDecl : getProtocols()) {
    if (protoDecl == errorProto || protoDecl->inheritsFrom(errorProto))
      return true;
  }

  return false;
}

bool ExistentialLayout::containsNonMarkerProtocols() const {
  for (auto proto : getProtocols()) {
    if (!proto->isMarkerProtocol())
      return true;
  }

  return false;
}

LayoutConstraint ExistentialLayout::getLayoutConstraint() const {
  if (hasExplicitAnyObject) {
    return LayoutConstraint::getLayoutConstraint(
      LayoutConstraintKind::Class);
  }

  return LayoutConstraint();
}

bool TypeBase::isExistentialWithError() {
  auto canTy = getCanonicalType();

  if (!canTy.isExistentialType()) return false;

  // FIXME: Compute this as a bit in TypeBase so this operation isn't
  // overly expensive.
  auto layout = canTy.getExistentialLayout();
  return layout.isExistentialWithError(getASTContext());
}

bool TypeBase::isOpenedExistentialWithError() {
  if (auto archetype = getAs<ExistentialArchetypeType>()) {
    auto errorProto = getASTContext().getErrorDecl();
    if (!errorProto) return false;

    for (auto protoDecl : archetype->getConformsTo()) {
      if (protoDecl == errorProto || protoDecl->inheritsFrom(errorProto))
        return true;
    }
  }
  return false;
}

bool TypeBase::isStdlibType() {
  if (auto *NTD = getAnyNominal()) {
    auto *DC = NTD->getDeclContext();
    return DC->isModuleScopeContext() &&
           DC->getParentModule()->isStdlibModule();
  }
  return false;
}

bool TypeBase::isCGFloat() {
  auto *NTD = getAnyNominal();
  if (!NTD)
    return false;

  auto *DC = NTD->getDeclContext();
  if (!DC->isModuleScopeContext())
    return false;

  auto *module = DC->getParentModule();
  // On macOS `CGFloat` is part of a `CoreGraphics` module,
  // but on Linux it could be found in `Foundation`.
  return (module->getName().is("CoreGraphics") ||
          module->getName().is("Foundation")   ||
          module->getName().is("CoreFoundation")) &&
         NTD->getName().is("CGFloat");
}

bool TypeBase::isKnownStdlibCollectionType() {
  if (isArray() || isDictionary() || isSet()) {
    return true;
  }

  return false;
}

/// Remove argument labels from the function type.
Type TypeBase::removeArgumentLabels(unsigned numArgumentLabels) {
  // If there is nothing to remove, don't.
  if (numArgumentLabels == 0) return Type(this);

  auto fnType = castTo<AnyFunctionType>();

  // Drop argument labels from the input type.
  llvm::SmallVector<AnyFunctionType::Param, 8> unlabeledParams;
  unlabeledParams.reserve(fnType->getNumParams());
  for (const auto &param : fnType->getParams())
    unlabeledParams.push_back(param.getWithoutLabels());

  auto result = fnType->getResult()
                      ->removeArgumentLabels(numArgumentLabels - 1);

  if (auto *genericFnType = dyn_cast<GenericFunctionType>(fnType)) {
    return GenericFunctionType::get(genericFnType->getGenericSignature(),
                                    unlabeledParams, result,
                                    fnType->getExtInfo());
  }

  return FunctionType::get(unlabeledParams, result, fnType->getExtInfo());
}

Type TypeBase::replaceCovariantResultType(Type newResultType,
                                          unsigned uncurryLevel) {
  if (uncurryLevel == 0) {
    bool isLValue = is<LValueType>();

    auto loadedTy = getWithoutSpecifierType();
    if (auto objectType = loadedTy->getOptionalObjectType()) {
      newResultType = OptionalType::get(
          objectType->replaceCovariantResultType(newResultType, uncurryLevel));
    }

    return isLValue ? LValueType::get(newResultType) : newResultType;
  }

  // Determine the input and result types of this function.
  auto fnType = this->castTo<AnyFunctionType>();
  auto inputType = fnType->getParams();
  Type resultType =
    fnType->getResult()->replaceCovariantResultType(newResultType,
                                                    uncurryLevel - 1);

  // Produce the resulting function type.
  if (auto genericFn = dyn_cast<GenericFunctionType>(fnType)) {
    return GenericFunctionType::get(genericFn->getGenericSignature(),
                                    inputType, resultType,
                                    fnType->getExtInfo());
  }
  
  return FunctionType::get(inputType, resultType, fnType->getExtInfo());
}

/// Whether this parameter accepts an unlabeled trailing closure argument
/// using the more-restrictive forward-scan rule.
static bool allowsUnlabeledTrailingClosureParameter(const ParamDecl *param) {
  // inout parameters never allow an unlabeled trailing closure.
  if (param->isInOut())
    return false;

  Type paramType = param->isVariadic() ? param->getVarargBaseTy()
                                       : param->getInterfaceType();
  paramType = paramType->getRValueType()->lookThroughAllOptionalTypes();

  // For autoclosure parameters, look through the autoclosure result type
  // to get the actual argument type.
  if (param->isAutoClosure()) {
    auto fnType = paramType->getAs<AnyFunctionType>();
    if (!fnType)
      return false;

    paramType = fnType->getResult()->lookThroughAllOptionalTypes();
  }

  // After lookup through all optional types, this parameter allows an
  // unlabeled trailing closure if it is (structurally) a function type.
  return paramType->is<AnyFunctionType>();
}

ParameterListInfo::ParameterListInfo(
    ArrayRef<AnyFunctionType::Param> params,
    const ValueDecl *paramOwner,
    bool skipCurriedSelf) {
  defaultArguments.resize(params.size());
  propertyWrappers.resize(params.size());
  implicitSelfCapture.resize(params.size());
  inheritActorContext.resize(params.size());
  alwaysInheritActorContext.resize(params.size());
  variadicGenerics.resize(params.size());
  sendingParameters.resize(params.size());

  // No parameter owner means no parameter list means no default arguments
  // - hand back the zeroed bitvector.
  //
  // FIXME: We ought to not request parameter list info in this case.
  if (!paramOwner)
    return;

  // If the decl has a curried self, but we're not allowed to skip it, return.
  if (paramOwner->hasCurriedSelf() && !skipCurriedSelf)
    return;

  // Find the corresponding parameter list.
  auto *paramList = paramOwner->getParameterList();

  // No parameter list means no default arguments - hand back the zeroed
  // bitvector.
  if (!paramList) {
    return;
  }

  if (params.empty())
    return;

  // Arguments and parameters are not guaranteed to always line-up
  // perfectly, e.g. failure diagnostics tries to match argument type
  // to different "candidate" parameters.
  if (params.size() != paramList->size())
    return;

  // Now we have enough information to determine which parameters accept
  // unlabeled trailing closures.
  acceptsUnlabeledTrailingClosures.resize(params.size());

  // Note which parameters have default arguments and/or accept unlabeled
  // trailing closure arguments with the forward-scan rule.
  for (auto i : range(0, params.size())) {
    auto param = paramList->get(i);
    if (param->isDefaultArgument()) {
      defaultArguments.set(i);
    }

    if (allowsUnlabeledTrailingClosureParameter(param)) {
      acceptsUnlabeledTrailingClosures.set(i);
    }

    if (param->hasExternalPropertyWrapper()) {
      propertyWrappers.set(i);
    }

    if (param->getAttrs().hasAttribute<ImplicitSelfCaptureAttr>()) {
      implicitSelfCapture.set(i);
    }

    if (auto *attr =
            param->getAttrs().getAttribute<InheritActorContextAttr>()) {
      if (attr->isAlways()) {
        alwaysInheritActorContext.set(i);
      } else {
        inheritActorContext.set(i);
      }
    }

    if (param->getInterfaceType()->is<PackExpansionType>()) {
      variadicGenerics.set(i);
    }

    if (param->isSending()) {
      sendingParameters.set(i);
    }
  }
}

bool ParameterListInfo::hasDefaultArgument(unsigned paramIdx) const {
  return paramIdx < defaultArguments.size() ? defaultArguments[paramIdx]
      : false;
}

bool ParameterListInfo::acceptsUnlabeledTrailingClosureArgument(
    unsigned paramIdx) const {
  return paramIdx >= acceptsUnlabeledTrailingClosures.size() ||
      acceptsUnlabeledTrailingClosures[paramIdx];
}

bool ParameterListInfo::hasExternalPropertyWrapper(unsigned paramIdx) const {
  return paramIdx < propertyWrappers.size() ? propertyWrappers[paramIdx] : false;
}

bool ParameterListInfo::isImplicitSelfCapture(unsigned paramIdx) const {
  return paramIdx < implicitSelfCapture.size()
      ? implicitSelfCapture[paramIdx]
      : false;
}

std::pair<bool, InheritActorContextModifier>
ParameterListInfo::inheritsActorContext(unsigned paramIdx) const {
  if (paramIdx >= inheritActorContext.size())
    return std::make_pair(false, InheritActorContextModifier::None);

  if (inheritActorContext[paramIdx])
    return std::make_pair(true, InheritActorContextModifier::None);

  if (alwaysInheritActorContext[paramIdx])
    return std::make_pair(true, InheritActorContextModifier::Always);

  return std::make_pair(false, InheritActorContextModifier::None);
}

bool ParameterListInfo::isVariadicGenericParameter(unsigned paramIdx) const {
  return paramIdx < variadicGenerics.size()
      ? variadicGenerics[paramIdx]
      : false;
}

bool ParameterListInfo::isSendingParameter(unsigned paramIdx) const {
  return paramIdx < sendingParameters.size() ? sendingParameters[paramIdx]
                                             : false;
}

/// Turn a param list into a symbolic and printable representation that does not
/// include the types, something like (_:, b:, c:)
std::string swift::getParamListAsString(ArrayRef<AnyFunctionType::Param> params) {
  std::string result = "(";

  interleave(params,
             [&](const AnyFunctionType::Param &param) {
               if (!param.getLabel().empty())
                 result += param.getLabel().str();
               else
                 result += "_";
               result += ":";
             },
             [&] { result += ", "; });

  result += ')';
  return result;
}

/// Rebuilds the given 'self' type using the given object type as the
/// replacement for the object type of self.
static AnyFunctionType::Param
rebuildSelfTypeWithObjectType(AnyFunctionType::Param selfParam,
                              Type objectTy) {
  if (selfParam.getPlainType()->getAs<MetatypeType>())
    objectTy = MetatypeType::get(objectTy);

  return selfParam.withType(objectTy);
}

/// Returns a new function type exactly like this one but with the self
/// parameter replaced. Only makes sense for members of classes.
Type TypeBase::replaceSelfParameterType(Type newSelf) {
  auto fnTy = castTo<AnyFunctionType>();

  auto params = fnTy->getParams();
  assert(params.size() == 1);
  auto selfParam = rebuildSelfTypeWithObjectType(params[0], newSelf);

  if (auto genericFnTy = getAs<GenericFunctionType>()) {
    return GenericFunctionType::get(genericFnTy->getGenericSignature(),
                                    {selfParam},
                                    fnTy->getResult(),
                                    fnTy->getExtInfo());
  }

  return FunctionType::get({selfParam},
                           fnTy->getResult(),
                           fnTy->getExtInfo());
}

/// Look through a metatype, or just return the original type if it is
/// not a metatype.
Type TypeBase::getMetatypeInstanceType() {
  if (auto existentialMetaType = getAs<ExistentialMetatypeType>())
    return existentialMetaType->getExistentialInstanceType();

  if (auto metaTy = getAs<AnyMetatypeType>())
    return metaTy->getInstanceType();

  return this;
}

using ParameterizedProtocolMap =
  llvm::DenseMap<ProtocolDecl *, ParameterizedProtocolType *>;

/// Collect the protocols in the existential type T into the given
/// vector.
static void addProtocols(Type T,
                         SmallVectorImpl<ProtocolDecl *> &Protocols,
                         ParameterizedProtocolMap &Parameterized,
                         Type &Superclass,
                         InvertibleProtocolSet &Inverses,
                         bool &HasExplicitAnyObject) {
  if (auto Proto = T->getAs<ProtocolType>()) {
    Protocols.push_back(Proto->getDecl());
    return;
  }

  if (auto PC = T->getAs<ProtocolCompositionType>()) {
    Inverses.insertAll(PC->getInverses());
    HasExplicitAnyObject |= PC->hasExplicitAnyObject();
    for (auto P : PC->getMembers()) {
      addProtocols(P, Protocols, Parameterized, Superclass, Inverses,
                   HasExplicitAnyObject);
    }
    return;
  }

  if (auto PP = T->getAs<ParameterizedProtocolType>()) {
    Parameterized.insert({PP->getProtocol(), PP});
    Protocols.push_back(PP->getProtocol());
    return;
  }

  assert(isa<ClassDecl>(T->getAnyNominal()) && "Non-class, non-protocol "
         "member in protocol composition");
  assert((!Superclass || Superclass->isEqual(T)) &&
         "Should have diagnosed multiple superclasses by now");
  Superclass = T;
}

static void canonicalizeProtocols(SmallVectorImpl<ProtocolDecl *> &protocols,
                                  ParameterizedProtocolMap *parameterized) {
  // Skip a bunch of useless work.
  if (protocols.size() <= 1)
    return;

  llvm::SmallDenseMap<ProtocolDecl *, unsigned> known;
  bool zappedAny = false;

  // Seed the stack with the protocol declarations in the original list.
  // Zap any obvious duplicates along the way.
  for (unsigned i : indices(protocols)) {
    // If we have not seen this protocol before, record its index.
    if (known.count(protocols[i]) == 0) {
      known[protocols[i]] = i;
      continue;
    }

    // We have seen this protocol before; zap this occurrence.
    protocols[i] = nullptr;
    zappedAny = true;
  }
  
  // Walk the inheritance hierarchies of all of the protocols. If we run into
  // one of the known protocols, zap it from the original list.
  for (unsigned i : indices(protocols)) {
    auto *proto = protocols[i];
    if (proto == nullptr)
      continue;

    // The below algorithm assumes the inheritance graph is acyclic. Just skip
    // it if we have invalid code.
    if (proto->hasCircularInheritedProtocols())
      continue;

    // Add the protocols we inherited.
    auto allInherited = proto->getAllInheritedProtocols();
    for (auto *inherited : allInherited) {
      auto found = known.find(inherited);
      if (found != known.end()) {
        // Don't zap protocols associated with parameterized types.
        if (parameterized && parameterized->count(inherited))
          return;

        protocols[found->second] = nullptr;
        zappedAny = true;
      }
    }
  }
  
  if (zappedAny) {
    protocols.erase(std::remove(protocols.begin(), protocols.end(), nullptr),
                    protocols.end());
  }

  // Sort the set of protocols by module + name, to give a stable
  // ordering.
  llvm::array_pod_sort(protocols.begin(), protocols.end(), TypeDecl::compare);
}

void ProtocolType::canonicalizeProtocols(
       SmallVectorImpl<ProtocolDecl *> &protocols) {
  return ::canonicalizeProtocols(protocols, nullptr);
}

static void
getCanonicalParams(AnyFunctionType *funcType,
                   CanGenericSignature genericSig,
                   SmallVectorImpl<AnyFunctionType::Param> &canParams) {
  auto origParams = funcType->getParams();
  for (auto param : origParams) {
    canParams.emplace_back(param.getCanonical(genericSig));
  }
}

AnyFunctionType::Param
AnyFunctionType::Param::getCanonical(CanGenericSignature genericSig) const {
  // Canonicalize the type and drop the internal label to canonicalize the
  // Param.
  return Param(getPlainType()->getReducedType(genericSig),
               getLabel(), getParameterFlags(),
               /*InternalLabel=*/Identifier());
}

CanType TypeBase::computeCanonicalType() {
  assert(!hasCanonicalTypeComputed() && "called unnecessarily");

  TypeBase *Result = nullptr;
  switch (getKind()) {
#define ALWAYS_CANONICAL_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::Error:
  case TypeKind::Unresolved:
  case TypeKind::TypeVariable:
  case TypeKind::Placeholder:
  case TypeKind::BuiltinTuple:
    llvm_unreachable("these types are always canonical");

#define SUGARED_TYPE(id, parent) \
  case TypeKind::id: \
    Result = cast<id##Type>(this)-> \
             getSinglyDesugaredType()->getCanonicalType().getPointer(); \
    break;
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"

  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::Protocol: {
    auto nominalTy = cast<NominalType>(this);
    auto parentTy = nominalTy->getParent()->getCanonicalType();
    Result = NominalType::get(nominalTy->getDecl(), parentTy,
                              parentTy->getASTContext());
    break;
  }

  case TypeKind::Pack: {
    PackType *PT = cast<PackType>(this);
    assert(PT->getNumElements() != 0 && "Empty packs are always canonical");

    SmallVector<Type, 8> CanTys;
    CanTys.reserve(PT->getNumElements());
    for (Type field : PT->getElementTypes()) {
      CanTys.push_back(field->getCanonicalType());
    }

    const ASTContext &C = CanTys[0]->getASTContext();
    Result = PackType::get(C, CanTys)->castTo<PackType>();
    break;
  }

  case TypeKind::PackExpansion: {
    auto *expansion = cast<PackExpansionType>(this);
    auto patternType = expansion->getPatternType()->getCanonicalType();
    auto countType = expansion->getCountType()->getCanonicalType();
    if (auto packArchetype = dyn_cast<PackArchetypeType>(countType))
      countType = packArchetype->getReducedShape();
    Result = PackExpansionType::get(patternType, countType);
    break;
  }

  case TypeKind::PackElement: {
    auto *element = cast<PackElementType>(this);
    auto packType = element->getPackType()->getCanonicalType();
    Result = PackElementType::get(packType, element->getLevel());
    break;
  }

  case TypeKind::Tuple: {
    TupleType *TT = cast<TupleType>(this);
    assert(TT->getNumElements() != 0 && "Empty tuples are always canonical");

    SmallVector<TupleTypeElt, 8> CanElts;
    CanElts.reserve(TT->getNumElements());
    for (const TupleTypeElt &field : TT->getElements()) {
      assert(!field.getType().isNull() &&
             "Cannot get canonical type of un-typechecked TupleType!");
      CanElts.push_back(field.getWithType(field.getType()->getCanonicalType()));
    }

    const ASTContext &C = CanElts[0].getType()->getASTContext();
    Result = TupleType::get(CanElts, C)->castTo<TupleType>();
    break;
  }

  case TypeKind::GenericTypeParam: {
    GenericTypeParamType *gp = cast<GenericTypeParamType>(this);
    auto gpDecl = gp->getDecl();
    auto &C = gpDecl->getASTContext();
    Result =
        GenericTypeParamType::get(gp->getParamKind(), gp->getDepth(),
                                  gp->getIndex(), gp->getWeight(),
                                  gp->getValueType(),
                                  C);
    break;
  }

  case TypeKind::DependentMember: {
    auto dependent = cast<DependentMemberType>(this);
    auto base = dependent->getBase()->getCanonicalType();
    if (auto assocType = dependent->getAssocType())
      Result = DependentMemberType::get(base, assocType);
    else
      Result = DependentMemberType::get(base, dependent->getName());
    break;
  }

#define REF_STORAGE(Name, ...) \
  case TypeKind::Name##Storage:
#include "swift/AST/ReferenceStorage.def"
  {
    auto ref = cast<ReferenceStorageType>(this);
    Type referentType = ref->getReferentType()->getCanonicalType();
    Result = ReferenceStorageType::get(referentType, ref->getOwnership(),
                                       referentType->getASTContext());
    break;
  }
  case TypeKind::LValue:
    Result = LValueType::get(cast<LValueType>(this)->getObjectType()
                               ->getCanonicalType());
    break;
  case TypeKind::InOut:
    Result = InOutType::get(getInOutObjectType()->getCanonicalType());
    break;
  case TypeKind::Function:
  case TypeKind::GenericFunction: {
    AnyFunctionType *funcTy = cast<AnyFunctionType>(this);

    PrettyStackTraceType trace(funcTy->getResult()->getASTContext(),
                               "computing canonical type for ", this);

    CanGenericSignature genericSig;
    if (auto *genericFnTy = dyn_cast<GenericFunctionType>(this))
      genericSig = genericFnTy->getGenericSignature().getCanonicalSignature();

    // Transform the parameter and result types.
    SmallVector<AnyFunctionType::Param, 8> canParams;
    getCanonicalParams(funcTy, genericSig, canParams);
    auto resultTy = funcTy->getResult()->getReducedType(genericSig);

    std::optional<ASTExtInfo> extInfo = std::nullopt;
    if (funcTy->hasExtInfo())
      extInfo = funcTy->getCanonicalExtInfo(useClangTypes(resultTy));
    if (genericSig) {
      Result = GenericFunctionType::get(genericSig, canParams, resultTy,
                                        extInfo);
    } else {
      Result = FunctionType::get(canParams, resultTy, extInfo);
    }
    assert(Result->isCanonical());
    break;
  }

  case TypeKind::SILBlockStorage:
  case TypeKind::SILBox:
  case TypeKind::SILFunction:
  case TypeKind::SILToken:
  case TypeKind::SILMoveOnlyWrapped:
    llvm_unreachable("SIL-only types are always canonical!");

  case TypeKind::ProtocolComposition: {
    auto *PCT = cast<ProtocolCompositionType>(this);
    SmallVector<Type, 4> CanProtos;
    for (Type t : PCT->getMembers())
      CanProtos.push_back(t->getCanonicalType());
    assert(!CanProtos.empty() && "Non-canonical empty composition?");
    const ASTContext &C = CanProtos[0]->getASTContext();
    Type Composition = ProtocolCompositionType::get(C, CanProtos,
                                                    PCT->getInverses(),
                                                    PCT->hasExplicitAnyObject());
    Result = Composition.getPointer();
    break;
  }
  case TypeKind::ParameterizedProtocol: {
    auto *PPT = cast<ParameterizedProtocolType>(this);
    auto Base = cast<ProtocolType>(PPT->getBaseType()->getCanonicalType());
    SmallVector<Type, 1> CanArgs;
    for (Type t : PPT->getArgs())
      CanArgs.push_back(t->getCanonicalType());
    auto &C = Base->getASTContext();
    Result = ParameterizedProtocolType::get(C, Base, CanArgs);
    break;
  }
  case TypeKind::Existential: {
    auto *existential = cast<ExistentialType>(this);
    auto constraint = existential->getConstraintType()->getCanonicalType();
    Result = ExistentialType::get(constraint).getPointer();
    break;
  }
  case TypeKind::ExistentialMetatype: {
    auto metatype = cast<ExistentialMetatypeType>(this);
    auto instanceType = metatype->getInstanceType()->getCanonicalType();
    if (metatype->hasRepresentation())
      Result = ExistentialMetatypeType::get(instanceType,
                                            metatype->getRepresentation());
    else
      Result = ExistentialMetatypeType::get(instanceType);
    break;
  }
  case TypeKind::Metatype: {
    MetatypeType *MT = cast<MetatypeType>(this);
    Type InstanceTy = MT->getInstanceType()->getCanonicalType();
    if (MT->hasRepresentation())
      Result = MetatypeType::get(InstanceTy, MT->getRepresentation());
    else
      Result = MetatypeType::get(InstanceTy);
    break;
  }
  case TypeKind::DynamicSelf: {
    DynamicSelfType *DST = cast<DynamicSelfType>(this);
    Type SelfTy = DST->getSelfType()->getCanonicalType();
    Result = DynamicSelfType::get(SelfTy, SelfTy->getASTContext());
    break;
  }
  case TypeKind::UnboundGeneric: {
    auto unbound = cast<UnboundGenericType>(this);
    Type parentTy = unbound->getParent()->getCanonicalType();
    Result = UnboundGenericType::get(unbound->getDecl(), parentTy,
                                     parentTy->getASTContext());
    break;
  }
  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct: {
    BoundGenericType *BGT = cast<BoundGenericType>(this);
    Type parentTy;
    if (BGT->getParent())
      parentTy = BGT->getParent()->getCanonicalType();
    SmallVector<Type, 4> CanGenericArgs;
    for (Type Arg : BGT->getGenericArgs())
      CanGenericArgs.push_back(Arg->getCanonicalType());
    Result = BoundGenericType::get(BGT->getDecl(), parentTy, CanGenericArgs);
    break;
  }
  case TypeKind::ErrorUnion: {
    SmallVector<Type, 2> newTerms;
    for (auto term : cast<ErrorUnionType>(this)->getTerms()) {
      newTerms.push_back(term->getCanonicalType());
    }
    ASTContext &ctx = newTerms[0]->getASTContext();
    Result = ErrorUnionType::get(ctx, newTerms).getPointer();
    break;
  }
  case TypeKind::Integer: {
    auto intTy = cast<IntegerType>(this);
    APInt value = intTy->getValue();
    if (intTy->isNegative()) {
      value = -value;
    }
    SmallString<20> canonicalText;
    value.toStringUnsigned(canonicalText);
    Result = IntegerType::get(canonicalText, intTy->isNegative(),
                              intTy->getASTContext());
    break;
  }
  }

  // Cache the canonical type for future queries.
  assert(Result && "Case not implemented!");
  return CanonicalType = CanType(Result);
}

CanType TypeBase::getReducedType(GenericSignature sig) {
  return sig.getReducedType(this);
}

CanType TypeBase::getMinimalCanonicalType() const {
  const auto MinimalTy = getCanonicalType().transformRec(
      [](TypeBase *Ty) -> std::optional<Type> {
    const CanType CanTy = CanType(Ty);

    if (const auto ET = dyn_cast<ExistentialType>(CanTy)) {
      const auto PCT =
          dyn_cast<ProtocolCompositionType>(ET.getConstraintType());
      if (!PCT) {
        return CanTy;
      }

      const auto MinimalTy = PCT->getMinimalCanonicalType();
      if (MinimalTy->getClassOrBoundGenericClass()) {
        return MinimalTy;
      }

      return ExistentialType::get(MinimalTy);
    }

    if (const auto EM = dyn_cast<ExistentialMetatypeType>(CanTy)) {
      const auto PCT = dyn_cast<ProtocolCompositionType>(EM.getInstanceType());
      if (!PCT) {
        return CanTy;
      }

      const auto MinimalTy = PCT->getMinimalCanonicalType();
      if (MinimalTy->getClassOrBoundGenericClass()) {
        return MetatypeType::get(MinimalTy);
      }

      return ExistentialMetatypeType::get(MinimalTy);
    }

    if (const auto Composition = dyn_cast<ProtocolCompositionType>(CanTy)) {
      return Composition->getMinimalCanonicalType();
    }

    return std::nullopt;
  });

  return CanType(MinimalTy);
}

TypeBase *TypeBase::reconstituteSugar(bool Recursive) {
  auto Func = [Recursive](TypeBase *Ty) -> std::optional<Type> {
    if (auto boundGeneric = dyn_cast<BoundGenericType>(Ty)) {

      auto getGenericArg = [&](unsigned i) -> Type {
        auto arg = boundGeneric->getGenericArgs()[i];
        if (Recursive)
          arg = arg->reconstituteSugar(Recursive);
        return arg;
      };

      if (boundGeneric->isArray())
        return Type(ArraySliceType::get(getGenericArg(0)));
      if (boundGeneric->isDictionary())
        return Type(DictionaryType::get(getGenericArg(0), getGenericArg(1)));
      if (boundGeneric->isOptional())
        return Type(OptionalType::get(getGenericArg(0)));
    }
    return std::nullopt;
  };
  if (Recursive)
    return Type(this).transformRec(Func).getPointer();

  if (auto result = Func(this))
    return result->getPointer();

  return this;
}

TypeBase *TypeBase::getWithoutSyntaxSugar() {
  auto Func = [](TypeBase *Ty) -> std::optional<Type> {
    if (auto *syntaxSugarType = dyn_cast<SyntaxSugarType>(Ty))
      return syntaxSugarType->getSinglyDesugaredType()->getWithoutSyntaxSugar();
    return std::nullopt;
  };
  return Type(this).transformRec(Func).getPointer();
}

#define TYPE(Id, Parent)
#define SUGARED_TYPE(Id, Parent) \
  static_assert(std::is_base_of<SugarType, Id##Type>::value, "Sugar mismatch");
#include "swift/AST/TypeNodes.def"

Type SugarType::getSinglyDesugaredTypeSlow() {
  // Find the generic type that implements this syntactic sugar type.
  NominalTypeDecl *implDecl;

  // XXX -- If the Decl and Type class hierarchies agreed on spelling, then
  // we could handle the entire switch statement via macros.
  switch (getKind()) {
#define TYPE(Id, Parent) \
  case TypeKind::Id: llvm_unreachable("non-sugared type?");
#define SUGARED_TYPE(Id, Parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::TypeAlias:
    llvm_unreachable("bound type alias types always have an underlying type");
  case TypeKind::Locatable:
    llvm_unreachable("locatable types always have an underlying type");
  case TypeKind::ArraySlice:
  case TypeKind::VariadicSequence:
    implDecl = Context->getArrayDecl();
    break;
  case TypeKind::InlineArray:
    implDecl = Context->getInlineArrayDecl();
    break;
  case TypeKind::Optional:
    implDecl = Context->getOptionalDecl();
    break;
  case TypeKind::Dictionary:
    implDecl = Context->getDictionaryDecl();
    break;
  }
  if (!implDecl) {
    return ErrorType::get(*Context);
  }

  Bits.SugarType.HasCachedType = true;
  if (auto Ty = dyn_cast<UnarySyntaxSugarType>(this)) {
    UnderlyingType = BoundGenericType::get(implDecl, Type(), Ty->getBaseType());
  } else if (auto Ty = dyn_cast<DictionaryType>(this)) {
    UnderlyingType = BoundGenericType::get(
        implDecl, Type(), {Ty->getKeyType(), Ty->getValueType()});
  } else if (auto Ty = dyn_cast<InlineArrayType>(this)) {
    UnderlyingType = BoundGenericType::get(
        implDecl, Type(), {Ty->getCountType(), Ty->getElementType()});
  } else {
    llvm_unreachable("Not UnarySyntaxSugarType or DictionaryType?");
  }

  // Record the implementation type.
  return UnderlyingType;
}

GenericSignature TypeAliasType::getGenericSignature() const {
  return typealias->getGenericSignature();
}

SubstitutionMap TypeAliasType::getSubstitutionMap() const {
  auto genericSig = typealias->getGenericSignature();
  if (!genericSig)
    return SubstitutionMap();

  SubstitutionMap parentSubMap;
  DeclContext *dc = typealias->getDeclContext();

  if (dc->isLocalContext()) {
    if (auto parentSig = dc->getGenericSignatureOfContext())
      parentSubMap = parentSig->getIdentitySubstitutionMap();
  } else if (auto parent = getParent()) {
    parentSubMap = parent->getContextSubstitutionMap(dc);
  }

  SmallVector<Type, 4> replacements(
      parentSubMap.getReplacementTypes().begin(),
      parentSubMap.getReplacementTypes().end());
  for (auto arg : getDirectGenericArgs())
    replacements.push_back(arg);

  return SubstitutionMap::get(genericSig, replacements,
                              LookUpConformanceInModule());
}

GenericTypeParamType::GenericTypeParamType(GenericTypeParamDecl *param,
                                           RecursiveTypeProperties props)
  : SubstitutableType(TypeKind::GenericTypeParam, nullptr, props),
    Decl(param) {
  ASSERT(param->getDepth() != GenericTypeParamDecl::InvalidDepth);
  IsDecl = true;
  Depth = param->getDepth();
  Weight = 0;
  Index = param->getIndex();
  ParamKind = param->getParamKind();
  ValueType = param->getValueType();
}

GenericTypeParamType::GenericTypeParamType(Identifier name,
                                           GenericTypeParamType *canType,
                                           const ASTContext &ctx)
    : SubstitutableType(TypeKind::GenericTypeParam, nullptr,
                        canType->getRecursiveProperties()),
      Decl(nullptr) {
  Name = name;
  IsDecl = false;
  Depth = canType->getDepth();
  Weight = canType->getWeight();
  Index = canType->getIndex();
  ParamKind = canType->getParamKind();
  ValueType = canType->getValueType();

  setCanonicalType(CanType(canType));
}

GenericTypeParamType::GenericTypeParamType(GenericTypeParamKind paramKind,
                                           unsigned depth, unsigned index,
                                           unsigned weight, Type valueType,
                                           RecursiveTypeProperties props,
                                           const ASTContext &ctx)
    : SubstitutableType(TypeKind::GenericTypeParam, &ctx, props),
      Decl(nullptr) {
  ASSERT(!(paramKind == GenericTypeParamKind::Value && !valueType) &&
         "Value generic parameter must have type");
  IsDecl = false;
  Depth = depth;
  Weight = weight;
  Index = index;
  ParamKind = paramKind;
  ValueType = valueType;
}

GenericTypeParamDecl *GenericTypeParamType::getOpaqueDecl() const {
  auto *decl = getDecl();
  if (decl && decl->isOpaqueType())
    return decl;
  return nullptr;
}

Identifier GenericTypeParamType::getName() const {
  // Use the declaration name if we still have that sugar.
  if (auto decl = getDecl())
    return decl->getName();

  if (!isCanonical())
    return Name;

  // Otherwise, we're canonical. Produce an anonymous '<tau>_n_n' name.

  // getASTContext() doesn't actually mutate an already-canonical type.
  auto &C = const_cast<GenericTypeParamType*>(this)->getASTContext();
  auto &names = C.CanonicalGenericTypeParamTypeNames;

  auto key = (getDepth() << 16) | getIndex();

  auto cached = names.find(key);
  if (cached != names.end())
    return cached->second;
  
  llvm::SmallString<10> nameBuf;
  llvm::raw_svector_ostream os(nameBuf);

  static const char *tau = SWIFT_UTF8("\u03C4_");

  os << tau << getDepth() << '_' << getIndex();
  Identifier name = C.getIdentifier(os.str());
  names.insert({key, name});
  return name;
}

Type GenericTypeParamType::getValueType() const {
  if (getDecl())
    return getDecl()->getValueType();

  return ValueType;
}

GenericTypeParamType *GenericTypeParamType::withDepth(unsigned depth) const {
  return GenericTypeParamType::get(getParamKind(),
                                   depth,
                                   getIndex(),
                                   getWeight(),
                                   getValueType(),
                                   getASTContext());
}

const llvm::fltSemantics &BuiltinFloatType::getAPFloatSemantics() const {
  switch (getFPKind()) {
  case BuiltinFloatType::IEEE16:  return APFloat::IEEEhalf();
  case BuiltinFloatType::IEEE32:  return APFloat::IEEEsingle();
  case BuiltinFloatType::IEEE64:  return APFloat::IEEEdouble();
  case BuiltinFloatType::IEEE80:  return APFloat::x87DoubleExtended();
  case BuiltinFloatType::IEEE128: return APFloat::IEEEquad();
  case BuiltinFloatType::PPC128:  return APFloat::PPCDoubleDouble();
  }
  llvm::report_fatal_error("Unknown FP semantics");
}

bool TypeBase::mayBeCallable(DeclContext *dc) {
  if (is<AnyFunctionType>())
    return true;

  // Callable for construction.
  if (is<AnyMetatypeType>())
    return true;

  // Unresolved types that could potentially be callable.
  if (isPlaceholder() || is<UnresolvedType>() ||
      isTypeParameter() || isTypeVariableOrMember()) {
    return true;
  }
  // Callable nominal types.
  if (isCallAsFunctionType(dc) || hasDynamicCallableAttribute())
    return true;

  return false;
}

bool TypeBase::mayHaveSuperclass() {
  if (getClassOrBoundGenericClass())
    return true;

  if (auto archetype = getAs<ArchetypeType>())
    return (bool)archetype->requiresClass();

  return is<DynamicSelfType>();
}

bool TypeBase::satisfiesClassConstraint() {
  return mayHaveSuperclass() || isObjCExistentialType();
}

Type TypeBase::getSuperclass(bool useArchetypes) {
  auto *nominalDecl = getAnyNominal();
  auto *classDecl = dyn_cast_or_null<ClassDecl>(nominalDecl);

  // Handle some special non-class types here.
  if (!classDecl) {
    if (auto archetype = getAs<ArchetypeType>())
      return archetype->getSuperclass();

    if (auto dynamicSelfTy = getAs<DynamicSelfType>())
      return dynamicSelfTy->getSelfType();

    if (isExistentialType())
      return getExistentialLayout().getSuperclass();

    // No other types have superclasses.
    return Type();
  }

  // We have a class, so get the superclass type.
  //
  // If the derived class is generic, the superclass type may contain
  // generic type parameters from the signature of the derived class.
  Type superclassTy = classDecl->getSuperclass();

  // If there's no superclass, or it is fully concrete, we're done.
  if (!superclassTy || !superclassTy->hasTypeParameter())
    return superclassTy;

  auto hasUnboundGenericType = [&]() {
    Type t(this);
    while (t) {
      if (t->is<UnboundGenericType>())
        return true;
      t = t->getNominalParent();
    }
    return false;
  };

  // If we started with an UnboundGenericType, we cannot apply the
  // context substitution map. Return the unbound form of the superclass.
  if (hasUnboundGenericType())
    return superclassTy->getAnyNominal()->getDeclaredType();

  // Gather substitutions from the self type, and apply them to the original
  // superclass type to form the substituted superclass type.
  auto subMap = getContextSubstitutionMap(classDecl,
                                          (useArchetypes
                                           ? classDecl->getGenericEnvironment()
                                           : nullptr));
  return superclassTy.subst(subMap);
}

Type TypeBase::getRootClass(bool useArchetypes) {
  Type iterator = this;
  assert(iterator);

  while (auto superclass = iterator->getSuperclass(useArchetypes)) {
    iterator = superclass;
  }

  return iterator;
}

bool TypeBase::isExactSuperclassOf(Type ty) {
  // For there to be a superclass relationship, we must be a class, and
  // the potential subtype must be a class, superclass-bounded archetype,
  // or subclass existential involving an imported class and @objc
  // protocol.
  if (!getClassOrBoundGenericClass() ||
      !(ty->mayHaveSuperclass() ||
        (ty->isObjCExistentialType() &&
         ty->getSuperclass() &&
         ty->getSuperclass()->getAnyNominal()->hasClangNode())))
    return false;

  SmallPtrSet<ClassDecl *, 8> seen;
  do {
    if (auto *classDecl = ty->getClassOrBoundGenericClass())
      if (!seen.insert(classDecl).second)
        return false;
    if (ty->isEqual(this))
      return true;
  } while ((ty = ty->getSuperclass()));
  return false;
}

bool TypeBase::isValueParameter() {
  Type t(this);

  return t->is<GenericTypeParamType>() &&
         t->castTo<GenericTypeParamType>()->isValue();
}

namespace {
class IsBindableVisitor : public TypeVisitor<IsBindableVisitor, CanType, CanType> {
public:
  using VisitBindingCallback =
    llvm::function_ref<CanType (ArchetypeType *, CanType)>;
    
  VisitBindingCallback VisitBinding;
  
  IsBindableVisitor(VisitBindingCallback visit) : VisitBinding(visit) {}

  CanType visitArchetypeType(ArchetypeType *orig, CanType subst) {
    if (!subst->isTypeParameter()) {
      // Check that the archetype isn't constrained in a way that makes the
      // binding impossible.
      // For instance, if the archetype is class-constrained, and the binding
      // is not a class, it can never be bound.
      if (orig->requiresClass()
          && !subst->satisfiesClassConstraint())
        return CanType();

      if (auto superclass = orig->getSuperclass())
        if (!superclass->isBindableToSuperclassOf(subst))
          return CanType();

      // TODO: If the archetype has a superclass constraint, check that the
      // substitution is a subclass.
      
      // TODO: For private types or protocols, we might be able to definitively
      // deny bindings.
      
      // Otherwise, there may be an external retroactive conformance that
      // allows the binding.
    }
    // Let the binding succeed.
    return VisitBinding(orig, subst);
  }
  
  CanType visitType(TypeBase *orig, CanType subst) {
    if (CanType(orig) == subst)
      return subst;
    
    return CanType();
  }
  
  CanType visitDynamicSelfType(DynamicSelfType *orig, CanType subst) {
    // A "dynamic self" type can be bound to another dynamic self type, or the
    // non-dynamic base class type.
    if (auto dynSubst = dyn_cast<DynamicSelfType>(subst)) {
      if (auto newBase = visit(orig->getSelfType(), dynSubst.getSelfType())) {
        return CanDynamicSelfType::get(newBase, orig->getASTContext())
                                 ->getCanonicalType();
      }
      return CanType();
    }
    
    if (auto newNonDynBase = visit(orig->getSelfType(), subst)) {
      return newNonDynBase;
    }
    return CanType();
  }
  
  /// Handle a nominal type with generic parameters anywhere in its context.
  /// \c origType and \c substType must already have been established to be
  /// instantiations of the same \c NominalTypeDecl.
  CanType handleGenericNominalType(NominalTypeDecl *decl,
                                   CanType origType,
                                   CanType substType) {
    assert(origType->getAnyNominal() == decl
           && substType->getAnyNominal() == decl);
    
    LLVM_DEBUG(llvm::dbgs() << "\n---\nTesting bindability of:\n";
               origType->print(llvm::dbgs());
               llvm::dbgs() << "\nto subst type:\n";
               substType->print(llvm::dbgs()););
    
    auto origSubMap = origType->getContextSubstitutionMap(
        decl, decl->getGenericEnvironment());
    auto substSubMap = substType->getContextSubstitutionMap(
        decl, decl->getGenericEnvironment());

    auto genericSig = decl->getGenericSignature();
    
    SmallVector<Type, 4> newParams;
    llvm::DenseMap<SubstitutableType *, Type> newParamsMap;
    bool didChange = false;
    
    LLVM_DEBUG(llvm::dbgs() << "\nNominal type generic signature:\n";
               genericSig.print(llvm::dbgs()));
    
    for (auto gpTy : genericSig.getGenericParams()) {
      auto gp = gpTy->getCanonicalType();
      
      auto orig = gp.subst(origSubMap)->getCanonicalType();
      auto subst = gp.subst(substSubMap)->getCanonicalType();
      
      auto newParam = visit(orig, subst);
      if (!newParam)
        return CanType();
      
      newParams.push_back(newParam);
      newParamsMap.insert({gp->castTo<GenericTypeParamType>(), newParam});
      didChange |= (newParam != subst);
    }
    
    SmallVector<ProtocolConformanceRef, 4> newConformances;

    // Collect conformances for the new substitutions, and verify that they don't
    // invalidate the binding to the original type.
    for (const auto &req : genericSig.getRequirements()) {
      if (req.getKind() != RequirementKind::Conformance) continue;

      auto canTy = req.getFirstType()->getCanonicalType();
      
      // Verify the generic requirements, if the subst type is bound to
      // concrete type.
      auto *proto = req.getProtocolDecl();
      if (!canTy.subst(substSubMap)->isTypeParameter()) {
        auto origConf = origSubMap.lookupConformance(canTy, proto);
        auto substConf = substSubMap.lookupConformance(canTy, proto);

        if (origConf.isConcrete()) {
          if (!substConf.isConcrete())
            return CanType();
          if (origConf.getConcrete()->getRootConformance()
                != substConf.getConcrete()->getRootConformance())
            return CanType();
        }
      }
      
      // Gather the conformances for the new binding type, if the type changed.
      if (didChange) {
        auto newSubstTy = req.getFirstType().subst(
          QueryTypeSubstitutionMap{newParamsMap},
          LookUpConformanceInModule());
        
        auto newConformance
          = lookupConformance(newSubstTy, proto, /*allowMissing=*/true);
        if (!newConformance)
          return CanType();
        newConformances.push_back(newConformance);
      }
    }

    if (!didChange)
      return substType;
    
    // Build the new substituted generic type.
    auto newSubMap = SubstitutionMap::get(genericSig,
                                          newParams,
                                          newConformances);
    return decl->getDeclaredInterfaceType().subst(newSubMap)
               ->getCanonicalType();
  }
  
  CanType visitNominalType(NominalType *nom, CanType subst) {
    if (auto substNom = dyn_cast<NominalType>(subst)) {
      auto nomDecl = nom->getDecl();
      if (nom->getDecl() != substNom->getDecl())
        return CanType();
      
      // If the type is generic (because it's a nested type in a generic context),
      // process the generic type bindings.
      if (!isa<ProtocolDecl>(nomDecl) && nomDecl->isGenericContext()) {
        return handleGenericNominalType(nomDecl, CanType(nom), subst);
      }
      // Otherwise, the nongeneric nominal types trivially match.
      return subst;
    }
    return CanType();
  }
  
  CanType visitAnyMetatypeType(AnyMetatypeType *meta, CanType subst) {
    if (auto substMeta = dyn_cast<AnyMetatypeType>(subst)) {
      if (substMeta->getKind() != meta->getKind())
        return CanType();
      
      auto substInstance = visit(meta->getInstanceType()->getCanonicalType(),
                               substMeta->getInstanceType()->getCanonicalType());
      if (!substInstance)
        return CanType();
      
      if (substInstance == substMeta.getInstanceType())
        return subst;
      
      return isa<ExistentialMetatypeType>(substMeta)
        ? CanType(CanExistentialMetatypeType::get(substInstance))
        : CanType(CanMetatypeType::get(substInstance));
    }
    return CanType();
  }
  
  CanType visitTupleType(TupleType *tuple, CanType subst) {
    if (auto substTuple = dyn_cast<TupleType>(subst)) {
      // Tuple elements must match.
      if (tuple->getNumElements() != substTuple->getNumElements())
        return CanType();
      // TODO: Label reordering?
      SmallVector<TupleTypeElt, 4> newElements;
      bool didChange = false;
      for (unsigned i : indices(tuple->getElements())) {
        auto elt = tuple->getElements()[i],
             substElt = substTuple->getElements()[i];
        if (elt.getName() != substElt.getName())
          return CanType();
        auto newElt = visit(elt.getType(),
                            substElt.getType()->getCanonicalType());
        if (!newElt)
          return CanType();
        newElements.push_back(substElt.getWithType(newElt));
        didChange = didChange | (newElt != CanType(substElt.getType()));
      }
      if (!didChange)
        return subst;
      return TupleType::get(newElements, subst->getASTContext())
        ->getCanonicalType();
    }
    return CanType();
  }
  
  CanType visitDependentMemberType(DependentMemberType *dt, CanType subst) {
    return subst;
  }
  CanType visitGenericTypeParamType(GenericTypeParamType *dt, CanType subst) {
    return subst;
  }
  
  CanType visitFunctionType(FunctionType *func, CanType subst) {
    if (auto substFunc = dyn_cast<FunctionType>(subst)) {
      if (!func->hasSameExtInfoAs(substFunc))
        return CanType();
      
      if (func->getParams().size() != substFunc->getParams().size())
        return CanType();

      SmallVector<AnyFunctionType::Param, 4> newParams;
      bool didChange = false;
      for (unsigned i : indices(func->getParams())) {
        auto param = func->getParams()[i];
        auto substParam = substFunc.getParams()[i];
        if (param.getParameterFlags() != substParam.getParameterFlags())
          return CanType();
        
        auto newParamTy =
          visit(param.getPlainType(), substParam.getPlainType());
        if (!newParamTy)
          return CanType();
        
        newParams.push_back(substParam.withType(newParamTy));
        didChange = didChange | (newParamTy != substParam.getPlainType());
      }
      
      auto newReturn = visit(func->getResult()->getCanonicalType(),
                             substFunc->getResult()->getCanonicalType());
      if (!newReturn)
        return CanType();
      if (!didChange && newReturn == substFunc.getResult())
        return subst;
      return FunctionType::get(newParams, newReturn, func->getExtInfo())
        ->getCanonicalType();
    }
    return CanType();
  }
  
  CanType visitSILFunctionType(SILFunctionType *func, CanType subst) {
    if (auto substFunc = dyn_cast<SILFunctionType>(subst)) {
      if (!func->hasSameExtInfoAs(substFunc))
        return CanType();

      if (func->getInvocationGenericSignature()
          || substFunc->getInvocationGenericSignature()) {
        auto sig = func->getInvocationGenericSignature();
        if (sig != substFunc->getInvocationGenericSignature())
          return CanType();

        auto origSubs = func->getPatternSubstitutions();
        auto substSubs = substFunc->getPatternSubstitutions();

        if ((bool) origSubs != (bool) substSubs)
          return CanType();

        for (unsigned i : indices(origSubs.getReplacementTypes())) {
          auto origType =
            origSubs.getReplacementTypes()[i]->getReducedType(sig);
          auto substType =
            substSubs.getReplacementTypes()[i]->getReducedType(sig);

          auto newType = visit(origType, substType);

          if (!newType)
            return CanType();

          // We can test SILFunctionTypes for bindability, but we can't
          // transform them.
          assert(newType == substType
                 && "cannot transform SILFunctionTypes");
        }
      }

      // Compare substituted function types.
      if (func->getPatternGenericSignature()
          || substFunc->getPatternGenericSignature()) {
        if (func->getPatternGenericSignature()
              != substFunc->getPatternGenericSignature())
          return CanType();
        
        auto sig = func->getPatternGenericSignature();
        
        auto origSubs = func->getPatternSubstitutions();
        auto substSubs = substFunc->getPatternSubstitutions();
        
        for (unsigned i : indices(origSubs.getReplacementTypes())) {
          auto origType =
            origSubs.getReplacementTypes()[i]->getReducedType(sig);
          auto substType =
            substSubs.getReplacementTypes()[i]->getReducedType(sig);
          
          auto newType = visit(origType, substType);
          
          if (!newType)
            return CanType();
          
          // We can test SILFunctionTypes for bindability, but we can't
          // transform them.
          assert(newType == substType
                 && "cannot transform SILFunctionTypes");
        }
        
        return subst;
      }
      
      if (func->getParameters().size() != substFunc->getParameters().size())
        return CanType();
      if (func->getResults().size() != substFunc->getResults().size())
        return CanType();
      
      for (unsigned i : indices(func->getParameters())) {
        if (func->getParameters()[i].getConvention()
              != substFunc->getParameters()[i].getConvention())
          return CanType();
        
        auto origParam = func->getParameters()[i].getInterfaceType();
        auto substParam = substFunc->getParameters()[i].getInterfaceType();
        auto newParam = visit(origParam, substParam);
        if (!newParam)
          return CanType();
        
        // We can test SILFunctionTypes for bindability, but we can't
        // transform them.
        assert(newParam == substParam
               && "cannot transform SILFunctionTypes");
      }

      for (unsigned i : indices(func->getResults())) {
        if (func->getResults()[i].getConvention()
            != substFunc->getResults()[i].getConvention())
          return CanType();

        auto origResult = func->getResults()[i].getInterfaceType();
        auto substResult = substFunc->getResults()[i].getInterfaceType();
        auto newResult = visit(origResult, substResult);
        if (!newResult)
          return CanType();

        // We can test SILFunctionTypes for bindability, but we can't
        // transform them.
        assert(newResult == substResult
               && "cannot transform SILFunctionTypes");
      }
      
      return subst;
    }
    
    return CanType();
  }
  
  CanType visitBoundGenericType(BoundGenericType *bgt, CanType subst) {
    auto substBGT = dyn_cast<BoundGenericType>(subst);
    if (!substBGT)
      return CanType();
    
    if (bgt->getDecl() != substBGT->getDecl())
      return CanType();

    auto *decl = bgt->getDecl();

    return handleGenericNominalType(decl, CanType(bgt), subst);
  }
};
}

CanType TypeBase::substituteBindingsTo(Type ty,
                             IsBindableVisitor::VisitBindingCallback substFn) {
  return IsBindableVisitor(substFn)
    .visit(getCanonicalType(), ty->getCanonicalType());
}

bool TypeBase::isBindableTo(Type ty) {
  // Keep a mapping of archetype bindings, so we reject types that try to bind
  // different types to the same type parameter, e.g.
  // `Foo<T,T>`.isBindableTo(`Foo<Int, String>`).
  llvm::DenseMap<ArchetypeType *, CanType> Bindings;
  
  return !substituteBindingsTo(ty,
    [&](ArchetypeType *archetype, CanType binding) -> CanType {
      // If we already bound this archetype, make sure the new binding candidate
      // is the same type.
      auto bound = Bindings.find(archetype);
      if (bound != Bindings.end()) {
        if (!bound->second->isEqual(binding))
          return CanType();
      } else {
        Bindings.insert({archetype, binding});
      }
      return binding;
    })
    .isNull();
}

bool TypeBase::isBindableToSuperclassOf(Type ty) {
  // Do an exact match if no archetypes are involved.
  if (!hasArchetype())
    return isExactSuperclassOf(ty);
  
  // For there to be a superclass relationship,
  // the potential subtype must be a class or superclass-bounded archetype.
  if (!ty->mayHaveSuperclass())
    return false;

  SmallPtrSet<ClassDecl *, 8> seen;
  do {
    if (auto *classDecl = ty->getClassOrBoundGenericClass())
      if (!seen.insert(classDecl).second)
        return false;
    if (isBindableTo(ty))
      return true;
  } while ((ty = ty->getSuperclass()));
  return false;
}

static bool isBridgeableObjectType(CanType type) {
  // Metatypes aren't always trivially bridgeable unless they've been
  // SIL-lowered to have an @objc representation.
  if (auto metaTy = dyn_cast<AnyMetatypeType>(type)) {
    if (!metaTy->hasRepresentation())
      return false;

    if (metaTy->getRepresentation() != MetatypeRepresentation::ObjC)
      return false;

    if (auto metatype = dyn_cast<MetatypeType>(type)) {
      CanType instanceType = metatype.getInstanceType();
      return instanceType->mayHaveSuperclass();
    }

    // @objc protocol metatypes.
    if (auto metatype = dyn_cast<ExistentialMetatypeType>(type)) {
      return metatype.getInstanceType()->isObjCExistentialType();
    }
  }

  // Classes and class-constrained archetypes.
  if (type->mayHaveSuperclass())
    return true;

  // Pure-ObjC existential types.
  if (type.isObjCExistentialType()) {
    return true;
  }

  // Blocks.
  if (auto fnType = dyn_cast<AnyFunctionType>(type)) {
    return fnType->getRepresentation()
      == AnyFunctionType::Representation::Block;
  } else if (auto fnType = dyn_cast<SILFunctionType>(type)) {
    return fnType->getRepresentation()
      == SILFunctionType::Representation::Block;
  }

  return false;
}

static bool hasRetainablePointerRepresentation(CanType type) {
  // Look through one level of Optional<> or ImplicitlyUnwrappedOptional<>.
  if (auto objType = type.getOptionalObjectType()) {
    type = objType;
  }

  // C++ imported `SWIFT_SHARED_REFERENCE` classes are not compatible with
  // Swift's retain/release runtime functions.
  if (type.isForeignReferenceType())
    return false;

  return isBridgeableObjectType(type);
}

bool TypeBase::hasRetainablePointerRepresentation() {
  return ::hasRetainablePointerRepresentation(getCanonicalType());
}

bool TypeBase::isBridgeableObjectType() {
  return ::isBridgeableObjectType(getCanonicalType());
}

bool TypeBase::isPotentiallyBridgedValueType() {
  // struct and enum types
  if (auto nominal = getAnyNominal()) {
    if (isa<StructDecl>(nominal) || isa<EnumDecl>(nominal))
      return true;
  }

  // Error existentials.
  if (isExistentialWithError()) return true;

  // Archetypes that aren't class-constrained.
  if (auto archetype = getAs<ArchetypeType>())
    return !archetype->requiresClass();

  return false;
}

/// Determine whether this is a representable Objective-C object type.
static ForeignRepresentableKind
getObjCObjectRepresentable(Type type, const DeclContext *dc) {
  // @objc metatypes are representable when their instance type is.
  if (auto metatype = type->getAs<AnyMetatypeType>()) {
    auto instanceType = metatype->getInstanceType();

    // Consider metatype of any existential type as not Objective-C
    // representable.
    if (metatype->is<MetatypeType>() && instanceType->isAnyExistentialType())
      return ForeignRepresentableKind::None;

    // If the instance type is not representable verbatim, the metatype is not
    // representable.
    if (getObjCObjectRepresentable(instanceType, dc)
          != ForeignRepresentableKind::Object)
      return ForeignRepresentableKind::None;

    // Objective-C metatypes are trivially representable.
    if (metatype->hasRepresentation() &&
        metatype->getRepresentation() == MetatypeRepresentation::ObjC)
      return ForeignRepresentableKind::Object;

    // All other metatypes are bridged.
    return ForeignRepresentableKind::Bridged;
  }

  // DynamicSelfType is always representable in Objective-C, even if
  // the class is not @objc, allowing Self-returning methods to witness
  // @objc protocol requirements.
  if (type->is<DynamicSelfType>())
    return ForeignRepresentableKind::Object;

  // @objc classes.
  if (auto classDecl = type->getClassOrBoundGenericClass()) {
    if (classDecl->isObjC())
      return ForeignRepresentableKind::Object;
  }

  // Objective-C existential types are trivially representable if
  // they don't have a superclass constraint, or if the superclass
  // constraint is an @objc class.
  if (type->isExistentialType()) {
    auto layout = type->getExistentialLayout();
    if (layout.isObjC() &&
        (!layout.explicitSuperclass ||
         getObjCObjectRepresentable(layout.explicitSuperclass, dc) ==
           ForeignRepresentableKind::Object))
      return ForeignRepresentableKind::Object;
  }

  // Existentials consisting of only marker protocols can be bridged to id.
  if (type->isMarkerExistential()) {
    return ForeignRepresentableKind::Bridged;
  }

  if (auto tyContext = dc->getInnermostTypeContext()) {
    // Class-constrained generic parameters, from ObjC generic classes.
    if (auto *classDecl = tyContext->getSelfClassDecl())
      if (classDecl->hasClangNode())
        if (auto archetype = type->getAs<ArchetypeType>())
          if (archetype->requiresClass())
            return ForeignRepresentableKind::Object;

    // The 'Self' parameter in a protocol is representable in Objective-C.
    if (auto *protoDecl = dyn_cast<ProtocolDecl>(tyContext))
      if (type->isEqual(dc->mapTypeIntoContext(protoDecl->getSelfInterfaceType())))
        return ForeignRepresentableKind::Object;
  }

  return ForeignRepresentableKind::None;
}

/// Determine the foreign representation of this type.
///
/// This function determines when and how a particular type is mapped
/// into a foreign language. Any changes to the logic here also need
/// to be reflected in PrintAsClang, so that the Swift type will be
/// properly printed for (Objective-)C and in SIL's bridging logic.
static std::pair<ForeignRepresentableKind, ProtocolConformance *>
getForeignRepresentable(Type type, ForeignLanguage language,
                        const DeclContext *dc) {
  // Local function that simply produces a failing result.
  auto failure = []() -> std::pair<ForeignRepresentableKind,
                                   ProtocolConformance *> {
    return { ForeignRepresentableKind::None, nullptr };
  };
  
  // If type has an error let's fail early.
  if (type->hasError())
    return failure();

  // Look through one level of optional type, but remember that we did.
  bool wasOptional = false;
  if (auto valueType = type->getOptionalObjectType()) {
    type = valueType;
    wasOptional = true;
  }

  if (auto existential = type->getAs<ExistentialType>())
    type = existential->getConstraintType();

  // Objective-C object types, including metatypes.
  if (language == ForeignLanguage::ObjectiveC) {
    auto representable = getObjCObjectRepresentable(type, dc);
    if (representable != ForeignRepresentableKind::None)
      return { representable, nullptr };
  }

  // Function types.
  if (auto functionType = type->getAs<FunctionType>()) {
    // Cannot handle async or throwing functions.
    if (functionType->getExtInfo().isAsync() ||
        functionType->getExtInfo().isThrowing())
      return failure();

    // Whether we have found any types that are bridged.
    bool anyBridged = false;
    bool anyStaticBridged = false;

    // Local function to combine the result of a recursive invocation.
    //
    // Returns true on failure.
    auto recurse = [&](Type componentType) -> bool {
      switch (componentType->getForeignRepresentableIn(language, dc).first) {
      case ForeignRepresentableKind::None:
        return true;

      case ForeignRepresentableKind::Trivial:
      case ForeignRepresentableKind::Object:
        return false;

      case ForeignRepresentableKind::Bridged:
      case ForeignRepresentableKind::BridgedError:
        anyBridged = true;
        return false;

      case ForeignRepresentableKind::StaticBridged:
        anyStaticBridged = true;
        return false;
      }

      llvm_unreachable("Unhandled ForeignRepresentableKind in switch.");
    };

    // Check the representation of the function type.
    bool isBlock = false;
    switch (functionType->getRepresentation()) {
    case AnyFunctionType::Representation::Thin:
      return failure();

    case AnyFunctionType::Representation::Swift:
      anyStaticBridged = true;
      break;

    case AnyFunctionType::Representation::Block:
      isBlock = true;
      break;

    case AnyFunctionType::Representation::CFunctionPointer:
      break;
    }

    auto success = [](bool anyStaticBridged,
                      bool anyBridged,
                      bool isBlock) -> std::pair<ForeignRepresentableKind,
                                                 ProtocolConformance *> {
      // We have something representable; check how it is representable.
      return { anyStaticBridged ? ForeignRepresentableKind::StaticBridged
                   : anyBridged ? ForeignRepresentableKind::Bridged
                   : isBlock    ? ForeignRepresentableKind::Object
                   : ForeignRepresentableKind::Trivial,
                   nullptr };
    };

    // Look at the result type.
    Type resultType = functionType->getResult();
    if (!resultType->isVoid() && recurse(resultType))
      return failure();

    // Look at the input params.
    for (const auto &param : functionType->getParams()) {
      if (param.isVariadic())
        return failure();
      if (recurse(param.getOldType()))
        return failure();
    }

    return success(anyStaticBridged, anyBridged, isBlock);
  }

  // Give special dispensation to builtin types for testing purposes.
  if (type->is<BuiltinType>())
    return { ForeignRepresentableKind::Trivial, nullptr };

  auto nominal = type->getAnyNominal();
  if (!nominal) return failure();

  ASTContext &ctx = nominal->getASTContext();

  // Unmanaged<T> can be trivially represented in Objective-C if T
  // is trivially represented in Objective-C.
  if (language == ForeignLanguage::ObjectiveC && type->isUnmanaged()) {
    auto boundGenericType = type->getAs<BoundGenericType>();

    // Note: works around a broken Unmanaged<> definition.
    if (!boundGenericType || boundGenericType->getGenericArgs().size() != 1)
      return failure();
    
    auto typeArgument = boundGenericType->getGenericArgs()[0];
    if (typeArgument->isTriviallyRepresentableIn(language, dc))
      return { ForeignRepresentableKind::Trivial, nullptr };

    return failure();
  }

  // If the type was imported from Clang, check whether it is
  // representable in the requested language.
  if (nominal->hasClangNode() || nominal->isObjC()) {
    switch (language) {
    case ForeignLanguage::C:
      // Imported classes and protocols are not representable in C.
      if (isa<ClassDecl>(nominal) || isa<ProtocolDecl>(nominal))
        return failure();

      // @objc enums are not representable in C, @cdecl ones and imported ones
      // are ok.
      if (!nominal->hasClangNode())
        return failure();

      LLVM_FALLTHROUGH;

    case ForeignLanguage::ObjectiveC:
      if (isa<StructDecl>(nominal) || isa<EnumDecl>(nominal)) {
        // Non-trivial C++ classes and structures are not
        // supported by @objc attribute, even though they can
        // be represented in Objective-C++.
        if (auto *cxxRec = dyn_cast_or_null<clang::CXXRecordDecl>(
                nominal->getClangDecl())) {
          if (cxxRec->hasNonTrivialCopyConstructor() ||
              cxxRec->hasNonTrivialMoveConstructor() ||
              cxxRec->hasNonTrivialDestructor())
            return failure();
        }

        // Optional structs are not representable in (Objective-)C if they
        // originally came from C, whether or not they are bridged, unless they
        // came from swift_newtype. If they are defined in Swift, they are only
        // representable if they are bridged (checked below).
        if (wasOptional) {
          if (nominal->hasClangNode()) {
            Type underlyingType =
                nominal->getDeclaredInterfaceType()
                       ->getSwiftNewtypeUnderlyingType();
            if (underlyingType) {
              return getForeignRepresentable(OptionalType::get(underlyingType),
                                             language, dc);
            }
            return failure();
          }
          break;
        }
      }

      return { ForeignRepresentableKind::Trivial, nullptr };
    }
  }

  // @cdecl enums are representable in C and Objective-C.
  if (nominal->getAttrs().getAttribute<CDeclAttr>()) {
    return { ForeignRepresentableKind::Trivial, nullptr };
  }

  // Pointers may be representable in ObjC.
  PointerTypeKind pointerKind;
  if (auto pointerElt = type->getAnyPointerElementType(pointerKind)) {
    switch (pointerKind) {
    case PTK_UnsafeMutableRawPointer:
    case PTK_UnsafeRawPointer:
    case PTK_UnsafeMutablePointer:
    case PTK_UnsafePointer:
      // An UnsafeMutablePointer<T> or UnsafePointer<T> is
      // representable if T is trivially representable or Void.
      if (pointerElt->isVoid() ||
          pointerElt->isTriviallyRepresentableIn(language, dc))
        return { ForeignRepresentableKind::Trivial, nullptr };

      return failure();

    case PTK_AutoreleasingUnsafeMutablePointer:
      // An AutoreleasingUnsafeMutablePointer<T> is representable in
      // Objective-C if T is a representable object type in
      // Objective-C.

      // Allow one level of optionality.
      if (auto objectType = pointerElt->getOptionalObjectType())
        pointerElt = objectType;

      if (language == ForeignLanguage::ObjectiveC &&
          getObjCObjectRepresentable(pointerElt, dc)
            != ForeignRepresentableKind::None)
        return { ForeignRepresentableKind::Trivial, nullptr };

      return failure();
    }
  }

  // Determine whether this nominal type is known to be representable
  // in this foreign language.
  auto result = ctx.getForeignRepresentationInfo(nominal, language, dc);
  if (result.getKind() == ForeignRepresentableKind::None) return failure();

  if (wasOptional && !result.isRepresentableAsOptional())
    return failure();
  
  // If our nominal type has type arguments, make sure they are
  // representable as well. Because type arguments are not actually
  // translated separately, whether they are trivially representable
  // or bridged representable doesn't impact our final result.
  if (auto boundGenericType = type->getAs<BoundGenericType>()) {
    for (auto typeArg : boundGenericType->getGenericArgs()) {
      // Type arguments cannot be optional.
      if (typeArg->getOptionalObjectType())
        return failure();

      // A type parameter can appear here when we're looking at an
      // extension of an @objc imported class.
      //
      // FIXME: Make this more principled.
      if (typeArg->isTypeParameter())
        continue;

      // And must be representable either an object or bridged.
      switch (typeArg->getForeignRepresentableIn(language, dc).first) {
      case ForeignRepresentableKind::None:
      case ForeignRepresentableKind::StaticBridged:
        return failure();

      case ForeignRepresentableKind::Trivial:
        // FIXME: We allow trivially-representable cases that also
        // conform to _ObjectiveCBridgeable. This may not be desirable
        // and should be re-evaluated.
        if (auto nominal = typeArg->getAnyNominal()) {
          if (auto objcBridgeable
                = ctx.getProtocol(KnownProtocolKind::ObjectiveCBridgeable)) {
            SmallVector<ProtocolConformance *, 1> conformances;
            if (nominal->lookupConformance(objcBridgeable, conformances))
              break;
          }
        }
        
        return failure();
        
      case ForeignRepresentableKind::Object:
      case ForeignRepresentableKind::Bridged:
      case ForeignRepresentableKind::BridgedError:
        break;
      }
    }
    
    // Specialize the conformance we were given for the type we're testing.
    if (result.getKind() == ForeignRepresentableKind::Bridged
        && !result.getConformance()->getType()->isEqual(type)) {
      auto specialized = type->getASTContext()
        .getSpecializedConformance(type,
             cast<NormalProtocolConformance>(result.getConformance()),
             boundGenericType->getContextSubstitutionMap());
      result = ForeignRepresentationInfo::forBridged(specialized);
    }
  }

  return { result.getKind(), result.getConformance() };
}

std::pair<ForeignRepresentableKind, ProtocolConformance *>
TypeBase::getForeignRepresentableIn(ForeignLanguage language,
                                    const DeclContext *dc) {
  return getForeignRepresentable(Type(this), language, dc);
}

bool TypeBase::isRepresentableIn(ForeignLanguage language,
                                 const DeclContext *dc) {
  switch (getForeignRepresentableIn(language, dc).first) {
  case ForeignRepresentableKind::None:
    return false;

  case ForeignRepresentableKind::Trivial:
  case ForeignRepresentableKind::Object:
  case ForeignRepresentableKind::Bridged:
  case ForeignRepresentableKind::BridgedError:
  case ForeignRepresentableKind::StaticBridged:
    return true;
  }

  llvm_unreachable("Unhandled ForeignRepresentableKind in switch.");
}

bool TypeBase::isTriviallyRepresentableIn(ForeignLanguage language,
                                          const DeclContext *dc) {
  switch (getForeignRepresentableIn(language, dc).first) {
  case ForeignRepresentableKind::None:
  case ForeignRepresentableKind::Bridged:
  case ForeignRepresentableKind::BridgedError:
  case ForeignRepresentableKind::StaticBridged:
    return false;

  case ForeignRepresentableKind::Trivial:
  case ForeignRepresentableKind::Object:
    return true;
  }

  llvm_unreachable("Unhandled ForeignRepresentableKind in switch.");
}

static bool isABICompatibleEvenAddingOptional(CanType t1, CanType t2) {
  // Classes, class-constrained archetypes, and pure-ObjC existential
  // types all have single retainable pointer representation; optionality
  // change is allowed.
  // NOTE: This doesn't use isAnyClassReferenceType because we want it to
  // return a conservative answer for dependent types. There's probably
  // a better answer here, though.
  if ((t1->mayHaveSuperclass() || t1->isObjCExistentialType()) &&
      (t2->mayHaveSuperclass() || t2->isObjCExistentialType())) {
    return true;
  }

  // Class metatypes are ABI-compatible even under optionality change.
  if (auto metaTy1 = dyn_cast<MetatypeType>(t1)) {
    if (auto metaTy2 = dyn_cast<MetatypeType>(t2)) {
      if (metaTy1.getInstanceType().getClassOrBoundGenericClass() &&
          metaTy2.getInstanceType().getClassOrBoundGenericClass()) {
        return true;
      }
    }
  }

  return false;
}

namespace {
  enum class ParameterPosition {
    NotParameter,
    Parameter,
    ParameterTupleElement
  };

  enum class OptionalUnwrapping {
    None,
    OptionalToOptional,
    ValueToOptional,
    OptionalToValue
  };
} // end anonymous namespace

static bool matchesFunctionType(CanAnyFunctionType fn1, CanAnyFunctionType fn2,
                                TypeMatchOptions matchMode,
                                OptionalUnwrapping insideOptional,
                                llvm::function_ref<bool()> paramsAndResultMatch) {
  // FIXME: Handle generic functions in non-ABI matches.
  if (!matchMode.contains(TypeMatchFlags::AllowABICompatible)) {
    if (!isa<FunctionType>(fn1) || !isa<FunctionType>(fn2))
      return false;
  }

  // When checking overrides, allow the base type to be throwing even if the
  // overriding type isn't.
  auto ext1 = fn1->getExtInfo();
  auto ext2 = fn2->getExtInfo();
  if (matchMode.contains(TypeMatchFlags::AllowOverride)) {
    // Removing 'throwing' is ABI-compatible for synchronous functions, but
    // not for async ones.
    if (ext2.isThrowing() && !ext1.isThrowing() &&
        ext2.getThrownError().isNull() &&
        !(ext2.isAsync() &&
          matchMode.contains(TypeMatchFlags::AllowABICompatible))) {
      ext1 = ext1.withThrows(true, ext2.getThrownError());
    }

    // Removing '@Sendable' is ABI-compatible because there's nothing wrong with
    // a function being sendable when it doesn't need to be.
    if (!ext2.isSendable())
      ext1 = ext1.withSendable(false);
  }

  if (matchMode.contains(TypeMatchFlags::IgnoreFunctionSendability)) {
    ext1 = ext1.withSendable(false);
    ext2 = ext2.withSendable(false);
  }

  if (matchMode.contains(TypeMatchFlags::IgnoreFunctionGlobalActorIsolation)) {
    if (ext1.getGlobalActor())
      ext1 = ext1.withoutIsolation();
    if (ext2.getGlobalActor())
      ext2 = ext2.withoutIsolation();
  }

  // If specified, allow an escaping function parameter to override a
  // non-escaping function parameter when the parameter is optional.
  // Note that this is checking 'ext2' rather than 'ext1' because parameters
  // must be contravariant for the containing function to be covariant.
  if (matchMode.contains(
          TypeMatchFlags::IgnoreNonEscapingForOptionalFunctionParam) &&
      insideOptional == OptionalUnwrapping::OptionalToOptional) {
    if (!ext2.isNoEscape())
      ext1 = ext1.withNoEscape(false);
  }
  if (!ext1.isEqualTo(ext2, useClangTypes(fn1)))
    return false;

  return paramsAndResultMatch();
}

static bool matches(CanType t1, CanType t2, TypeMatchOptions matchMode,
                    ParameterPosition paramPosition,
                    OptionalUnwrapping insideOptional) {
  if (t1 == t2) return true;

  // First try unwrapping optionals.
  // Make sure we only unwrap at most one layer of optional.
  if (insideOptional == OptionalUnwrapping::None) {
    // Value-to-optional and optional-to-optional.
    if (auto obj2 = t2.getOptionalObjectType()) {
      // Optional-to-optional.
      if (auto obj1 = t1.getOptionalObjectType()) {
        // Allow T? and T! to freely match one another.
        return matches(obj1, obj2, matchMode, ParameterPosition::NotParameter,
                       OptionalUnwrapping::OptionalToOptional);
      }

      // Value-to-optional.
      if (matchMode.contains(TypeMatchFlags::AllowABICompatible)) {
        if (isABICompatibleEvenAddingOptional(t1, obj2))
          return true;
      }
      if (matchMode.contains(TypeMatchFlags::AllowOverride) ||
          matchMode.contains(TypeMatchFlags::AllowTopLevelOptionalMismatch)) {
        return matches(t1, obj2, matchMode, ParameterPosition::NotParameter,
                       OptionalUnwrapping::ValueToOptional);
      }

    } else if (matchMode.contains(
                 TypeMatchFlags::AllowTopLevelOptionalMismatch)) {
      // Optional-to-value, normally disallowed.
      if (auto obj1 = t1.getOptionalObjectType()) {
        return matches(obj1, t2, matchMode, ParameterPosition::NotParameter,
                       OptionalUnwrapping::OptionalToValue);
      }
    }
  }

  // Scalar-to-tuple and tuple-to-tuple.
  if (auto tuple2 = dyn_cast<TupleType>(t2)) {
    // We only ever look into singleton tuples on the RHS if we're
    // certain that the LHS isn't also a singleton tuple.
    ParameterPosition elementPosition;
    switch (paramPosition) {
    case ParameterPosition::NotParameter:
    case ParameterPosition::ParameterTupleElement:
      elementPosition = ParameterPosition::NotParameter;
      break;
    case ParameterPosition::Parameter:
      elementPosition = ParameterPosition::ParameterTupleElement;
      break;
    }

    auto tuple1 = dyn_cast<TupleType>(t1);
    if (!tuple1 || tuple1->getNumElements() != tuple2->getNumElements()) {
      if (tuple2->getNumElements() == 1) {
        return matches(t1, tuple2.getElementType(0), matchMode, elementPosition,
                       OptionalUnwrapping::None);
      }
      return false;
    }

    for (auto i : indices(tuple1.getElementTypes())) {
      if (!matches(tuple1.getElementType(i), tuple2.getElementType(i),
                   matchMode, elementPosition, OptionalUnwrapping::None)) {
        return false;
      }
    }
    return true;
  }

  // Function-to-function.
  if (auto fn2 = dyn_cast<AnyFunctionType>(t2)) {
    auto fn1 = dyn_cast<AnyFunctionType>(t1);
    if (!fn1)
      return false;

    auto paramsAndResultMatch = [&]() {
      auto fn2Params = fn2.getParams();
      auto fn1Params = fn1.getParams();
      if (fn2Params.size() != fn1Params.size()) {
        return false;
      }

      // Inputs are contravariant.
      for (auto i : indices(fn2.getParams())) {
        if (!matches(fn2Params[i].getOldType(), fn1Params[i].getOldType(),
                     matchMode, ParameterPosition::ParameterTupleElement,
                     OptionalUnwrapping::None)) {
          return false;
        }
      }

      // Results are covariant.
      return (matches(fn1.getResult(), fn2.getResult(), matchMode,
                      ParameterPosition::NotParameter,
                      OptionalUnwrapping::None));
    };

    return matchesFunctionType(fn1, fn2, matchMode, insideOptional,
                               paramsAndResultMatch);
  }

  // Class-to-class.
  if (matchMode.contains(TypeMatchFlags::AllowOverride))
    if (t2->isExactSuperclassOf(t1))
      return true;

  if (matchMode.contains(TypeMatchFlags::AllowABICompatible))
    if (isABICompatibleEvenAddingOptional(t1, t2))
      return true;

  if (matchMode.contains(TypeMatchFlags::AllowCompatibleOpaqueTypeArchetypes))
    if (auto opaque1 = t1->getAs<OpaqueTypeArchetypeType>())
      if (auto opaque2 = t2->getAs<OpaqueTypeArchetypeType>())
        return opaque1->getDecl()->getOpaqueInterfaceGenericSignature()
                 .getCanonicalSignature() ==
               opaque2->getDecl()->getOpaqueInterfaceGenericSignature()
                  .getCanonicalSignature() &&
               opaque1->getSubstitutions().getCanonical() ==
                 opaque2->getSubstitutions().getCanonical() &&
               opaque1->getInterfaceType()->getCanonicalType()->matches(
                   opaque2->getInterfaceType()->getCanonicalType(), matchMode);

  if (matchMode.contains(TypeMatchFlags::IgnoreSendability)) {
    // Support `any Sendable` -> `Any` matching inside generic types
    // e.g. collections and optionals (i.e. `[String: (any Sendable)?]`).
    if (auto *generic1 = t1->getAs<BoundGenericType>()) {
      if (auto *generic2 = t2->getAs<BoundGenericType>()) {
        if (generic1->getDecl() == generic2->getDecl()) {
          auto genericArgs1 = generic1->getGenericArgs();
          auto genericArgs2 = generic2->getGenericArgs();

          if (genericArgs1.size() == genericArgs2.size() &&
              llvm::all_of(llvm::zip_equal(genericArgs1, genericArgs2),
                           [&](const auto &elt) -> bool {
                             return matches(
                                 std::get<0>(elt)->getCanonicalType(),
                                 std::get<1>(elt)->getCanonicalType(),
                                 matchMode, ParameterPosition::NotParameter,
                                 OptionalUnwrapping::None);
                           }))
            return true;
        }
      }
    }

    // Attempting to match `any Sendable` by `Any` is allowed in this mode.
    if (t1->isAny()) {
      auto *PD = dyn_cast_or_null<ProtocolDecl>(t2->getAnyNominal());
      if (PD && PD->isSpecificProtocol(KnownProtocolKind::Sendable))
        return true;
    }
  }

  return false;
}

bool TypeBase::matches(Type other, TypeMatchOptions matchMode) {
  return ::matches(getCanonicalType(), other->getCanonicalType(), matchMode,
                   ParameterPosition::NotParameter, OptionalUnwrapping::None);
}

bool TypeBase::matchesParameter(Type other, TypeMatchOptions matchMode) {
  return ::matches(getCanonicalType(), other->getCanonicalType(), matchMode,
                   ParameterPosition::Parameter, OptionalUnwrapping::None);
}

bool TypeBase::matchesFunctionType(Type other, TypeMatchOptions matchMode,
                                   llvm::function_ref<bool()> paramsAndResultMatch) {
  auto thisFnTy = dyn_cast<AnyFunctionType>(getCanonicalType());
  auto otherFnTy = dyn_cast<AnyFunctionType>(other->getCanonicalType());

  assert(thisFnTy && otherFnTy);
  return ::matchesFunctionType(thisFnTy, otherFnTy, matchMode,
                               OptionalUnwrapping::None, paramsAndResultMatch);
}

/// getNamedElementId - If this tuple has a field with the specified name,
/// return the field index, otherwise return -1.
int TupleType::getNamedElementId(Identifier I) const {
  for (unsigned i = 0, e = Bits.TupleType.Count; i != e; ++i) {
    if (getTrailingObjects<TupleTypeElt>()[i].getName() == I)
      return i;
  }

  // Otherwise, name not found.
  return -1;
}

ArchetypeType::ArchetypeType(TypeKind Kind,
                             const ASTContext &Ctx,
                             RecursiveTypeProperties properties,
                             Type InterfaceType,
                             ArrayRef<ProtocolDecl *> ConformsTo,
                             Type Superclass, LayoutConstraint Layout,
                             GenericEnvironment *Environment)
  : SubstitutableType(Kind, &Ctx, properties),
    InterfaceType(InterfaceType),
    Environment(Environment)
{
  // Set up the bits we need for trailing objects to work.
  Bits.ArchetypeType.HasSuperclass = static_cast<bool>(Superclass);
  Bits.ArchetypeType.HasLayoutConstraint = static_cast<bool>(Layout);
  Bits.ArchetypeType.NumProtocols = ConformsTo.size();

  // Record the superclass.
  if (Superclass)
    *getSubclassTrailingObjects<Type>() = Superclass;

  // Record the layout constraint.
  if (Layout)
    *getSubclassTrailingObjects<LayoutConstraint>() = Layout;

  // Copy the protocols.
  std::uninitialized_copy(ConformsTo.begin(), ConformsTo.end(),
                          getSubclassTrailingObjects<ProtocolDecl *>());
}

bool ArchetypeType::isRoot() const {
  return getInterfaceType()->is<GenericTypeParamType>();
}

Type ArchetypeType::getExistentialType() const {
  auto *genericEnv = getGenericEnvironment();

  // Opened types hold this directly.
  if (auto *opened = dyn_cast<ExistentialArchetypeType>(this)) {
    if (opened->isRoot()) {
      return genericEnv->maybeApplyOuterContextSubstitutions(
          genericEnv->getOpenedExistentialType());
    }
  }

  // Otherwise we compute it via a generic signature query.
  auto interfaceType = getInterfaceType();
  auto genericSig = genericEnv->getGenericSignature();

  auto existentialType = genericSig->getExistentialType(interfaceType);
  return genericEnv->maybeApplyOuterContextSubstitutions(existentialType);
}

bool ArchetypeType::requiresClass() const {
  if (auto layout = getLayoutConstraint())
    return layout->isClass();
  return false;
}

Type ArchetypeType::getSuperclass() const {
  if (!Bits.ArchetypeType.HasSuperclass) return Type();

  auto *genericEnv = getGenericEnvironment();
  return genericEnv->mapTypeIntoContext(
      *getSubclassTrailingObjects<Type>());
}

Type ArchetypeType::getValueType() const {
  if (auto gp = getInterfaceType()->getAs<GenericTypeParamType>())
    return gp->getValueType();

  return Type();
}

Type ArchetypeType::getNestedType(AssociatedTypeDecl *assocType) {
  Type interfaceType = getInterfaceType();
  Type memberInterfaceType =
      DependentMemberType::get(interfaceType, assocType);
  auto genericSig = getGenericEnvironment()->getGenericSignature();
  if (genericSig->isValidTypeParameter(memberInterfaceType)) {
    return getGenericEnvironment()->getOrCreateArchetypeFromInterfaceType(
        memberInterfaceType);
  }

  return Type();
}

Type ArchetypeType::getNestedTypeByName(Identifier name) {
  Type interfaceType = getInterfaceType();
  Type memberInterfaceType = DependentMemberType::get(interfaceType, name);
  auto genericSig = getGenericEnvironment()->getGenericSignature();
  if (genericSig->isValidTypeParameter(memberInterfaceType)) {
    return getGenericEnvironment()->getOrCreateArchetypeFromInterfaceType(
        memberInterfaceType);
  }

  return Type();
}

Identifier ArchetypeType::getName() const {
  assert(InterfaceType);
  if (auto depMemTy = InterfaceType->getAs<DependentMemberType>())
    return depMemTy->getName();
  return InterfaceType->castTo<GenericTypeParamType>()->getName();
}

std::string ArchetypeType::getFullName() const {
  return InterfaceType.getString();
}

/// Determine the recursive type properties for an archetype.
RecursiveTypeProperties ArchetypeType::archetypeProperties(
    RecursiveTypeProperties properties,
    ArrayRef<ProtocolDecl *> conformsTo,
    Type superclass,
    SubstitutionMap subs
) {
  properties |= subs.getRecursiveProperties();

  for (auto proto : conformsTo) {
    if (proto->getExplicitSafety() == ExplicitSafety::Unsafe) {
      properties |= RecursiveTypeProperties::IsUnsafe;
      break;
    }
  }

  if (superclass) {
    auto superclassProps = superclass->getRecursiveProperties();
    superclassProps.removeHasTypeParameter();
    superclassProps.removeHasDependentMember();
    properties |= superclassProps;
  }

  return properties;
}

PrimaryArchetypeType::PrimaryArchetypeType(const ASTContext &Ctx,
                                     GenericEnvironment *GenericEnv,
                                     Type InterfaceType,
                                     ArrayRef<ProtocolDecl *> ConformsTo,
                                     Type Superclass, LayoutConstraint Layout,
                                     RecursiveTypeProperties Properties)
  : ArchetypeType(TypeKind::PrimaryArchetype, Ctx, Properties,
                  InterfaceType, ConformsTo, Superclass, Layout, GenericEnv)
{
  assert(!InterfaceType->isParameterPack());
}

CanPrimaryArchetypeType
PrimaryArchetypeType::getNew(const ASTContext &Ctx,
                      GenericEnvironment *GenericEnv,
                      Type InterfaceType,
                      SmallVectorImpl<ProtocolDecl *> &ConformsTo,
                      Type Superclass,
                      LayoutConstraint Layout) {
  assert(!Superclass || Superclass->getClassOrBoundGenericClass());
  assert(GenericEnv && "missing generic environment for archetype");

  // Gather the set of protocol declarations to which this archetype conforms.
  ProtocolType::canonicalizeProtocols(ConformsTo);

  RecursiveTypeProperties Properties = archetypeProperties(
    RecursiveTypeProperties::HasPrimaryArchetype,
    ConformsTo, Superclass, SubstitutionMap());
  assert(!Properties.hasTypeVariable());

  auto arena = AllocationArena::Permanent;
  void *mem = Ctx.Allocate(
    PrimaryArchetypeType::totalSizeToAlloc<ProtocolDecl *, Type, LayoutConstraint>(
          ConformsTo.size(), Superclass ? 1 : 0, Layout ? 1 : 0),
      alignof(PrimaryArchetypeType), arena);

  return CanPrimaryArchetypeType(::new (mem) PrimaryArchetypeType(
      Ctx, GenericEnv, InterfaceType, ConformsTo, Superclass, Layout,
      Properties));
}

OpaqueTypeArchetypeType::OpaqueTypeArchetypeType(
    GenericEnvironment *environment,
    RecursiveTypeProperties properties,
    Type interfaceType,
    ArrayRef<ProtocolDecl*> conformsTo,
    Type superclass, LayoutConstraint layout)
  : ArchetypeType(TypeKind::OpaqueTypeArchetype, interfaceType->getASTContext(),
                  properties, interfaceType, conformsTo, superclass, layout,
                  environment)
{
  assert(!interfaceType->isParameterPack());
}

OpaqueTypeDecl *OpaqueTypeArchetypeType::getDecl() const {
  return Environment->getOpaqueTypeDecl();
}

SubstitutionMap OpaqueTypeArchetypeType::getSubstitutions() const {
  return Environment->getOuterSubstitutions();
}

ExistentialArchetypeType::ExistentialArchetypeType(
    GenericEnvironment *environment, Type interfaceType,
    ArrayRef<ProtocolDecl *> conformsTo, Type superclass,
    LayoutConstraint layout, RecursiveTypeProperties properties)
  : LocalArchetypeType(TypeKind::ExistentialArchetype,
                       interfaceType->getASTContext(), properties,
                       interfaceType, conformsTo, superclass, layout,
                       environment)
{
  assert(!interfaceType->isParameterPack());
}

PackArchetypeType::PackArchetypeType(
    const ASTContext &Ctx, GenericEnvironment *GenericEnv, Type InterfaceType,
    ArrayRef<ProtocolDecl *> ConformsTo, Type Superclass,
    LayoutConstraint Layout, PackShape Shape,
    RecursiveTypeProperties Properties)
  : ArchetypeType(TypeKind::PackArchetype, Ctx, Properties,
                    InterfaceType, ConformsTo, Superclass, Layout, GenericEnv) {
  assert(InterfaceType->isParameterPack());
  *getTrailingObjects<PackShape>() = Shape;
}

CanPackArchetypeType
PackArchetypeType::get(const ASTContext &Ctx,
                       GenericEnvironment *GenericEnv,
                       Type InterfaceType, Type ShapeType,
                       SmallVectorImpl<ProtocolDecl *> &ConformsTo,
                       Type Superclass, LayoutConstraint Layout) {
  assert(!Superclass || Superclass->getClassOrBoundGenericClass());
  assert(GenericEnv && "missing generic environment for archetype");

  // Gather the set of protocol declarations to which this archetype conforms.
  ProtocolType::canonicalizeProtocols(ConformsTo);

  RecursiveTypeProperties properties = archetypeProperties(
    (RecursiveTypeProperties::HasPrimaryArchetype |
     RecursiveTypeProperties::HasPackArchetype),
    ConformsTo, Superclass, SubstitutionMap());
  assert(!properties.hasTypeVariable());

  auto arena = AllocationArena::Permanent;
  void *mem =
      Ctx.Allocate(PackArchetypeType::totalSizeToAlloc<ProtocolDecl *, Type,
                                                       LayoutConstraint, PackShape>(
                       ConformsTo.size(), Superclass ? 1 : 0, Layout ? 1 : 0,
                       /*shapeSize*/1),
                   alignof(PackArchetypeType), arena);

  return CanPackArchetypeType(::new (mem) PackArchetypeType(
      Ctx, GenericEnv, InterfaceType, ConformsTo, Superclass, Layout,
      {ShapeType}, properties));
}

CanType PackArchetypeType::getReducedShape() {
  // mapTypeIntoContext() also calls getReducedShape() via
  // PackExpansionType::get(), so avoid that by short-circuiting
  // the case where the pack archetype represents its own
  // shape class.
  auto shapeType = getTrailingObjects<PackShape>()->shapeType;
  if (shapeType->isEqual(getInterfaceType()))
    return CanType(this);

  return getGenericEnvironment()
      ->mapTypeIntoContext(shapeType)
      ->getCanonicalType();
}

ElementArchetypeType::ElementArchetypeType(
    const ASTContext &Ctx, GenericEnvironment *GenericEnv, Type InterfaceType,
    ArrayRef<ProtocolDecl *> ConformsTo, Type Superclass,
    LayoutConstraint Layout)
    : LocalArchetypeType(TypeKind::ElementArchetype, Ctx,
                         RecursiveTypeProperties::HasElementArchetype,
                         InterfaceType,
                         ConformsTo, Superclass, Layout, GenericEnv) {
}

CanTypeWrapper<ElementArchetypeType> ElementArchetypeType::getNew(
    GenericEnvironment *environment, Type interfaceType,
    ArrayRef<ProtocolDecl *> conformsTo, Type superclass,
    LayoutConstraint layout) {
  assert(!interfaceType->isParameterPack());
  assert((!superclass || !superclass->hasArchetype())
         && "superclass must be interface type");
  auto arena = AllocationArena::Permanent;
  ASTContext &ctx = interfaceType->getASTContext();
  void *mem = ctx.Allocate(
    ElementArchetypeType::totalSizeToAlloc<ProtocolDecl *,Type,LayoutConstraint>(
      conformsTo.size(),
      superclass ? 1 : 0,
      layout ? 1 : 0),
      alignof(ElementArchetypeType), arena);

  return CanElementArchetypeType(::new (mem) ElementArchetypeType(
      ctx, environment, interfaceType, conformsTo, superclass, layout));
}

UUID ElementArchetypeType::getOpenedElementID() const {
  return getGenericEnvironment()->getOpenedElementUUID();
}

CanExistentialType CanExistentialType::get(CanType constraint) {
  assert(!(constraint->isAny() || constraint->isAnyObject()) &&
         "Any(Object) may not apppear as canonical constraint type");
  assert(!constraint->is<ExistentialMetatypeType>() &&
         "Existential metatype may not apppear as canonical constraint type");
  return CanExistentialType(
      ExistentialType::get(constraint)->castTo<ExistentialType>());
}

void ProtocolCompositionType::Profile(llvm::FoldingSetNodeID &ID,
                                      ArrayRef<Type> Members,
                                      InvertibleProtocolSet Inverses,
                                      bool HasExplicitAnyObject) {
  ID.AddBoolean(HasExplicitAnyObject);
  for (auto T : Members)
    ID.AddPointer(T.getPointer());
  for (auto IP : Inverses)
    ID.AddInteger((uint8_t)IP);
}

ParameterizedProtocolType::ParameterizedProtocolType(
    const ASTContext *ctx,
    ProtocolType *base, ArrayRef<Type> args,
    RecursiveTypeProperties properties)
  : TypeBase(TypeKind::ParameterizedProtocol, /*Context=*/ctx, properties),
    Base(base) {
  assert(args.size() > 0);
  Bits.ParameterizedProtocolType.ArgCount = args.size();
  for (unsigned i : indices(args))
    getTrailingObjects<Type>()[i] = args[i];
}

void ParameterizedProtocolType::Profile(llvm::FoldingSetNodeID &ID,
                                        ProtocolType *baseTy,
                                        ArrayRef<Type> args) {
  ID.AddPointer(baseTy);
  for (auto arg : args)
    ID.AddPointer(arg.getPointer());
}

void ParameterizedProtocolType::getRequirements(
    Type baseType, SmallVectorImpl<Requirement> &reqs) const {
  auto *protoDecl = getProtocol();

  auto assocTypes = protoDecl->getPrimaryAssociatedTypes();
  auto argTypes = getArgs();
  assert(argTypes.size() <= assocTypes.size());

  auto conformance = lookupConformance(baseType, protoDecl);
  auto subMap = SubstitutionMap::getProtocolSubstitutions(
      protoDecl, baseType, conformance);

  for (unsigned i : indices(argTypes)) {
    auto argType = argTypes[i];
    auto *assocType = assocTypes[i];

    Type subjectType;
    if (baseType->isTypeParameter()) {
      // Fast path.
      subjectType = DependentMemberType::get(baseType, assocType);
    } else {
      // Do a general type substitution here because the associated type might be
      // from an inherited protocol, in which case we will evaluate a non-trivial
      // conformance path.
      subjectType = assocType->getDeclaredInterfaceType().subst(subMap);
    }

    reqs.emplace_back(RequirementKind::SameType, subjectType, argType);
  }
}

bool ProtocolType::requiresClass() {
  return getDecl()->requiresClass();
}

bool ProtocolCompositionType::requiresClass() {
  return getExistentialLayout().requiresClass();
}

/// Constructs a protocol composition corresponding to the `Any` type.
Type ProtocolCompositionType::theAnyType(const ASTContext &C) {
  return ProtocolCompositionType::get(C, {}, /*Inverses=*/{},
                                      /*HasExplicitAnyObject=*/false);
}

/// Constructs a protocol composition corresponding to the `any ~Copyable &
/// ~Escapable` type.
///
/// Note: This includes the inverse of all current invertible protocols.
Type ProtocolCompositionType::theUnconstrainedAnyType(const ASTContext &C) {
  return ProtocolCompositionType::get(C, {}, InvertibleProtocolSet::allKnown(),
                                      /*HasExplicitAnyObject=*/false);
}

/// Constructs a protocol composition containing the `AnyObject` constraint.
Type ProtocolCompositionType::theAnyObjectType(const ASTContext &C) {
  return ProtocolCompositionType::get(C, {}, /*Inverses=*/{},
                                      /*HasExplicitAnyObject=*/true);
}

Type ProtocolCompositionType::getInverseOf(const ASTContext &C,
                                           InvertibleProtocolKind IP) {
  return ProtocolCompositionType::get(C, {}, /*Inverses=*/{IP},
                                      /*HasExplicitAnyObject=*/false);
}

Type ProtocolCompositionType::withoutMarkerProtocols() const {
  SmallVector<Type, 4> newMembers;
  llvm::copy_if(getMembers(), std::back_inserter(newMembers), [](Type member) {
    auto *P = member->getAs<ProtocolType>();
    return !(P && P->getDecl()->isMarkerProtocol());
  });

  if (newMembers.size() == getMembers().size())
    return Type(const_cast<ProtocolCompositionType *>(this));

  return ProtocolCompositionType::get(getASTContext(), newMembers,
                                      getInverses(), hasExplicitAnyObject());
}

Type ProtocolCompositionType::get(const ASTContext &C,
                                  ArrayRef<Type> Members,
                                  InvertibleProtocolSet Inverses,
                                  bool HasExplicitAnyObject) {
  // Fast path for 'AnyObject', 'Any', and '~Copyable'.
  if (Members.empty()) {
    return build(C, Members, Inverses, HasExplicitAnyObject);
  }

  // Whether this composition has an `AnyObject` or protocol-inverse member
  // that is not reflected in the Members array.
  auto haveExtraMember = [&]{
    return HasExplicitAnyObject || !Inverses.empty();
  };

  // If there's a single member and no layout constraint or inverses,
  // return that type.
  if (Members.size() == 1 && !haveExtraMember()) {
    return Members.front();
  }

  for (Type t : Members) {
    if (!t->isCanonical())
      return build(C, Members, Inverses, HasExplicitAnyObject);
  }

  Type Superclass;
  SmallVector<ProtocolDecl *, 4> Protocols;
  ParameterizedProtocolMap Parameterized;
  for (Type t : Members) {
    addProtocols(t, Protocols, Parameterized, Superclass,
                 Inverses, HasExplicitAnyObject);
  }

  // Form the set of canonical component types.
  SmallVector<Type, 4> CanTypes;

  // The presence of a superclass constraint makes AnyObject redundant.
  if (Superclass) {
    HasExplicitAnyObject = false;
    CanTypes.push_back(Superclass->getCanonicalType());
  }

  // If there are any parameterized protocols, the canonicalization
  // algorithm gets more complex.
  canonicalizeProtocols(Protocols, &Parameterized);

  for (auto proto : Protocols) {
    // If we have a parameterized type for this protocol, use the
    // canonical type of that.  Sema should prevent us from building
    // compositions with the same protocol and conflicting constraints.
    if (!Parameterized.empty()) {
      auto it = Parameterized.find(proto);
      if (it != Parameterized.end()) {
        CanTypes.push_back(it->second->getCanonicalType());
        continue;
      }
    }

    CanTypes.push_back(proto->getDeclaredInterfaceType());
  }

  // If one member remains with no extra members, return that type.
  if (CanTypes.size() == 1 && !haveExtraMember())
    return CanTypes.front();

  return build(C, CanTypes, Inverses, HasExplicitAnyObject);
}

CanType ProtocolCompositionType::getMinimalCanonicalType() const {
  auto &Ctx = getASTContext();

  auto existentialSig = Ctx.getOpenedExistentialSignature(getCanonicalType());
  auto result = existentialSig.OpenedSig->getUpperBound(
      existentialSig.SelfType,
      /*forExistentialSelf=*/true,
      /*includeParameterizedProtocols=*/true);
  return result.subst(existentialSig.Generalization)->getCanonicalType();
}

ClangTypeInfo AnyFunctionType::getClangTypeInfo() const {
  switch (getKind()) {
  case TypeKind::Function:
    return cast<FunctionType>(this)->getClangTypeInfo();
  case TypeKind::GenericFunction:
    // Generic functions do not have C types.
    return ClangTypeInfo();
  default:
    llvm_unreachable("Illegal type kind for AnyFunctionType.");
  }
}

Type AnyFunctionType::getThrownError() const {
  switch (getKind()) {
  case TypeKind::Function:
    return cast<FunctionType>(this)->getThrownError();
  case TypeKind::GenericFunction:
    return cast<GenericFunctionType>(this)->getThrownError();
  default:
    llvm_unreachable("Illegal type kind for AnyFunctionType.");
  }
}

bool AnyFunctionType::isSendable() const {
  return getExtInfo().isSendable();
}

Type AnyFunctionType::getGlobalActor() const {
  switch (getKind()) {
  case TypeKind::Function:
    return cast<FunctionType>(this)->getGlobalActor();
  case TypeKind::GenericFunction:
    return cast<GenericFunctionType>(this)->getGlobalActor();
  default:
    llvm_unreachable("Illegal type kind for AnyFunctionType.");
  }
}

llvm::ArrayRef<LifetimeDependenceInfo>
AnyFunctionType::getLifetimeDependencies() const {
  switch (getKind()) {
  case TypeKind::Function:
    return cast<FunctionType>(this)->getLifetimeDependencies();
  case TypeKind::GenericFunction:
    return cast<GenericFunctionType>(this)->getLifetimeDependencies();

  default:
    llvm_unreachable("Illegal type kind for AnyFunctionType.");
  }
}

std::optional<LifetimeDependenceInfo>
AnyFunctionType::getLifetimeDependenceFor(unsigned targetIndex) const {
  switch (getKind()) {
  case TypeKind::Function:
    return cast<FunctionType>(this)->getLifetimeDependenceFor(targetIndex);
  case TypeKind::GenericFunction:
    return cast<GenericFunctionType>(this)->getLifetimeDependenceFor(
        targetIndex);

  default:
    llvm_unreachable("Illegal type kind for AnyFunctionType.");
  }
}

std::optional<LifetimeDependenceInfo>
AnyFunctionType::getLifetimeDependenceForResult(const ValueDecl *decl) const {
  auto resultIndex =
      decl->hasCurriedSelf() ? getNumParams() + 1 : getNumParams();
  return getLifetimeDependenceFor(resultIndex);
}

ClangTypeInfo AnyFunctionType::getCanonicalClangTypeInfo() const {
  return getClangTypeInfo().getCanonical();
}

ASTExtInfo
AnyFunctionType::getCanonicalExtInfo(bool useClangFunctionType) const {
  assert(hasExtInfo());

  CanGenericSignature genericSig;
  if (auto *genericFnTy = dyn_cast<GenericFunctionType>(this))
    genericSig = genericFnTy->getGenericSignature().getCanonicalSignature();

  Type globalActor = getGlobalActor();
  if (globalActor)
    globalActor = globalActor->getReducedType(genericSig);

  // When there is an explicitly-specified thrown error, canonicalize it's type.
  auto bits = Bits.AnyFunctionType.ExtInfoBits;
  Type thrownError = getThrownError();
  if (thrownError) {
    thrownError = thrownError->getReducedType(genericSig);

    //   - If the thrown error is `any Error`, the function throws and we
    //     drop the thrown error.
    if (thrownError->isEqual(
            thrownError->getASTContext().getErrorExistentialType())) {
      thrownError = Type();

      //   - If the thrown error is `Never`, the function does not throw and
      //     we drop the thrown error.
    } else if (thrownError->isNever()) {
      thrownError = Type();
      bits = bits & ~ASTExtInfoBuilder::ThrowsMask;
    }
  }

  return ExtInfo(bits,
                 useClangFunctionType ? getCanonicalClangTypeInfo()
                                      : ClangTypeInfo(),
                 globalActor, thrownError, getLifetimeDependencies());
}

bool AnyFunctionType::hasNonDerivableClangType() {
  auto clangTypeInfo = getClangTypeInfo();
  if (clangTypeInfo.empty())
    return false;
  auto computedClangType = getASTContext().getClangFunctionType(
      getParams(), getResult(), getRepresentation());
  assert(computedClangType && "Failed to compute Clang type.");
  return clangTypeInfo != ClangTypeInfo(computedClangType);
}

bool AnyFunctionType::hasSameExtInfoAs(const AnyFunctionType *otherFn) {
  return getExtInfo().isEqualTo(otherFn->getExtInfo(), useClangTypes(this));
}

bool swift::hasIsolatedParameter(ArrayRef<AnyFunctionType::Param> params) {
  for (auto &param : params) {
    if (param.isIsolated())
      return true;
  }
  return false;
}

ClangTypeInfo SILFunctionType::getClangTypeInfo() const {
  if (!Bits.SILFunctionType.HasClangTypeInfo)
    return ClangTypeInfo();
  auto *info = getTrailingObjects<ClangTypeInfo>();
  assert(!info->empty() &&
         "If the ClangTypeInfo was empty, we shouldn't have stored it.");
  return *info;
}

bool SILFunctionType::hasNonDerivableClangType() {
  auto clangTypeInfo = getClangTypeInfo();
  if (clangTypeInfo.empty())
    return false;
  auto results = getResults();
  auto computedClangType = getASTContext().getCanonicalClangFunctionType(
      getParameters(),
      results.empty() ? std::nullopt : std::optional<SILResultInfo>(results[0]),
      getRepresentation());
  assert(computedClangType && "Failed to compute Clang type.");
  return clangTypeInfo != ClangTypeInfo(computedClangType);
}

bool SILFunctionType::hasSameExtInfoAs(const SILFunctionType *otherFn) {
  return getExtInfo().isEqualTo(otherFn->getExtInfo(), useClangTypes(this));
}

DependentMemberType *TypeBase::findUnresolvedDependentMemberType() {
  if (!hasTypeParameter()) return nullptr;

  DependentMemberType *unresolvedDepMemTy = nullptr;
  Type(this).findIf([&](Type type) -> bool {
    if (auto depMemTy = type->getAs<DependentMemberType>()) {
      if (depMemTy->getAssocType() == nullptr) {
        unresolvedDepMemTy = depMemTy;
        return true;
      }
    }
    return false;
  });

  return unresolvedDepMemTy;
}

bool TypeBase::isNoEscape() const {
  auto type = getCanonicalType();

  if (auto silFuncTy = dyn_cast<SILFunctionType>(type))
    return silFuncTy->isNoEscape();

  if (auto funcTy = dyn_cast<FunctionType>(type))
    return funcTy->isNoEscape();

  if (auto tupleTy = dyn_cast<TupleType>(type)) {
    for (auto eltTy : tupleTy.getElementTypes())
      if (eltTy->isNoEscape())
        return true;
  }

  return false;
}

Identifier DependentMemberType::getName() const {
  if (NameOrAssocType.is<Identifier>())
    return NameOrAssocType.get<Identifier>();

  return NameOrAssocType.get<AssociatedTypeDecl *>()->getName();
}

Type Type::transformRec(
    llvm::function_ref<std::optional<Type>(TypeBase *)> fn) const {
  class Transform : public TypeTransform<Transform> {
    llvm::function_ref<std::optional<Type>(TypeBase *)> fn;
  public:
    explicit Transform(llvm::function_ref<std::optional<Type>(TypeBase *)> fn,
                       ASTContext &ctx) : TypeTransform(ctx), fn(fn) {}

    std::optional<Type> transform(TypeBase *type, TypePosition position) {
      return fn(type);
    }
  };

  return Transform(fn, (*this)->getASTContext()).doIt(*this, TypePosition::Invariant);
}

Type Type::transformWithPosition(
    TypePosition pos,
    llvm::function_ref<std::optional<Type>(TypeBase *, TypePosition)> fn)
    const {
  class Transform : public TypeTransform<Transform> {
    llvm::function_ref<std::optional<Type>(TypeBase *, TypePosition)> fn;
  public:
    explicit Transform(llvm::function_ref<std::optional<Type>(TypeBase *, TypePosition)> fn,
                       ASTContext &ctx) : TypeTransform(ctx), fn(fn) {}

    std::optional<Type> transform(TypeBase *type, TypePosition position) {
      return fn(type, position);
    }
  };

  return Transform(fn, (*this)->getASTContext()).doIt(*this, pos);
}

bool Type::findIf(llvm::function_ref<bool(Type)> pred) const {
  class Walker : public TypeWalker {
    llvm::function_ref<bool(Type)> Pred;
  public:
    explicit Walker(llvm::function_ref<bool(Type)> pred) : Pred(pred) {}

    Action walkToTypePre(Type ty) override {
      if (Pred(ty))
        return Action::Stop;
      return Action::Continue;
    }
  };

  return walk(Walker(pred));
}

TypeTraitResult TypeBase::canBeClass() {
  // Any bridgeable object type can be a class.
  if (isBridgeableObjectType())
    return TypeTraitResult::Is;

  CanType self = getCanonicalType();

  // Archetypes with a trivial layout constraint can never
  // represent a class.
  if (auto Archetype = dyn_cast<ArchetypeType>(self)) {
    if (auto Layout = Archetype->getLayoutConstraint()) {
      if (Layout->isTrivial())
        return TypeTraitResult::IsNot;
      if (Layout->isClass())
        return TypeTraitResult::Is;
    }
  }

  // Dependent types might be bound to classes.
  if (isa<SubstitutableType>(self))
    return TypeTraitResult::CanBe;
  if (isa<DependentMemberType>(self))
    return TypeTraitResult::CanBe;
  
  return TypeTraitResult::IsNot;
}

bool Type::isPrivateSystemType(bool treatNonBuiltinProtocolsAsPublic) const {
  Type Ty = *this;
  if (!Ty)
    return false;

  if (auto existential = dyn_cast<ExistentialType>(Ty.getPointer()))
    return existential->getConstraintType().isPrivateSystemType(
        treatNonBuiltinProtocolsAsPublic);

  // A 'public' typealias can have an 'internal' type.
  if (auto *NAT = dyn_cast<TypeAliasType>(Ty.getPointer())) {
    auto *AliasDecl = NAT->getDecl();
    if (auto parent = NAT->getParent()) {
      if (parent.isPrivateSystemType(treatNonBuiltinProtocolsAsPublic))
        return true;
    }

    if (AliasDecl->isPrivateSystemDecl(treatNonBuiltinProtocolsAsPublic))
      return true;

    return Type(NAT->getSinglyDesugaredType())
        .isPrivateSystemType(treatNonBuiltinProtocolsAsPublic);
  }

  if (Type Unwrapped = Ty->getOptionalObjectType())
    return Unwrapped.isPrivateSystemType(treatNonBuiltinProtocolsAsPublic);

  if (auto TyD = Ty->getAnyNominal())
    if (TyD->isPrivateSystemDecl(treatNonBuiltinProtocolsAsPublic))
      return true;

  return false;
}

// NOTE: If you add a new SOMETIMES_LOADABLE_CHECKED_REF_STORAGE type, then you
// may or may not want to emulate what 'unowned' does.
bool UnownedStorageType::isLoadable(ResilienceExpansion resilience) const {
  auto ty = getReferentType();
  if (auto underlyingTy = ty->getOptionalObjectType())
    ty = underlyingTy;
  return ty->getReferenceCounting() == ReferenceCounting::Native;
}

ReferenceCounting TypeBase::getReferenceCounting() {
  CanType type = getCanonicalType();
  ASTContext &ctx = type->getASTContext();

  if (isForeignReferenceType())
    return lookThroughAllOptionalTypes()
                   ->getClassOrBoundGenericClass()
                   ->hasRefCountingAnnotations()
               ? ReferenceCounting::Custom
               : ReferenceCounting::None;

  // In the absence of Objective-C interoperability, everything uses native
  // reference counting or is the builtin BridgeObject.
  if (!ctx.LangOpts.EnableObjCInterop) {
    return type->getKind() == TypeKind::BuiltinBridgeObject
             ? ReferenceCounting::Bridge
             : ReferenceCounting::Native;
  }

  switch (type->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("sugared canonical type?");

  case TypeKind::BuiltinNativeObject:
  case TypeKind::SILBox:
    return ReferenceCounting::Native;

  case TypeKind::BuiltinBridgeObject:
    return ReferenceCounting::Bridge;

  case TypeKind::Class:
    return cast<ClassType>(type)->getDecl()->getObjectModel();
  case TypeKind::BoundGenericClass:
    return cast<BoundGenericClassType>(type)->getDecl()->getObjectModel();
  case TypeKind::UnboundGeneric:
    return cast<ClassDecl>(cast<UnboundGenericType>(type)->getDecl())
        ->getObjectModel();

  case TypeKind::DynamicSelf:
    return cast<DynamicSelfType>(type).getSelfType()
        ->getReferenceCounting();
  case TypeKind::SILMoveOnlyWrapped:
    return cast<SILMoveOnlyWrappedType>(type)
        ->getInnerType()
        ->getReferenceCounting();

  case TypeKind::PrimaryArchetype:
  case TypeKind::ExistentialArchetype:
  case TypeKind::OpaqueTypeArchetype:
  case TypeKind::PackArchetype:
  case TypeKind::ElementArchetype: {
    auto archetype = cast<ArchetypeType>(type);
    auto layout = archetype->getLayoutConstraint();
    (void)layout;
    assert(layout && layout->isRefCounted());
    if (auto supertype = archetype->getSuperclass())
      return supertype->getReferenceCounting();
    return ReferenceCounting::Unknown;
  }

  case TypeKind::Protocol:
  case TypeKind::ProtocolComposition: {
    auto layout = type->getExistentialLayout();
    assert(layout.requiresClass() && "Opaque existentials don't use refcounting");
    if (auto superclass = layout.getSuperclass())
      return superclass->getReferenceCounting();
    return ReferenceCounting::Unknown;
  }

  case TypeKind::ParameterizedProtocol: {
    return cast<ParameterizedProtocolType>(this)
      ->getBaseType()
      ->getReferenceCounting();
  }

  case TypeKind::Existential:
    return cast<ExistentialType>(type)->getConstraintType()
        ->getReferenceCounting();

  case TypeKind::Function:
  case TypeKind::GenericFunction:
  case TypeKind::SILFunction:
  case TypeKind::SILBlockStorage:
  case TypeKind::Error:
  case TypeKind::Unresolved:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinIntegerLiteral:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinRawUnsafeContinuation:
  case TypeKind::BuiltinJob:
  case TypeKind::BuiltinExecutor:
  case TypeKind::BuiltinDefaultActorStorage:
  case TypeKind::BuiltinNonDefaultDistributedActorStorage:
  case TypeKind::BuiltinPackIndex:
  case TypeKind::BuiltinUnsafeValueBuffer:
  case TypeKind::BuiltinVector:
  case TypeKind::Tuple:
  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Metatype:
  case TypeKind::ExistentialMetatype:
  case TypeKind::Module:
  case TypeKind::LValue:
  case TypeKind::InOut:
  case TypeKind::TypeVariable:
  case TypeKind::Placeholder:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct:
  case TypeKind::SILToken:
  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
  case TypeKind::Pack:
  case TypeKind::PackExpansion:
  case TypeKind::PackElement:
  case TypeKind::SILPack:
  case TypeKind::BuiltinTuple:
  case TypeKind::ErrorUnion:
  case TypeKind::Integer:
  case TypeKind::BuiltinUnboundGeneric:
  case TypeKind::BuiltinFixedArray:
#define REF_STORAGE(Name, ...) \
  case TypeKind::Name##Storage:
#include "swift/AST/ReferenceStorage.def"
    llvm_unreachable("type is not a class reference");
  }

  llvm_unreachable("Unhandled type kind!");
}

//
// SILBoxType implementation
//

void SILBoxType::Profile(llvm::FoldingSetNodeID &id, SILLayout *Layout,
                         SubstitutionMap Substitutions) {
  id.AddPointer(Layout);
  Substitutions.profile(id);
}

static RecursiveTypeProperties getBoxRecursiveProperties(
    SILLayout *Layout, SubstitutionMap subMap) {
  RecursiveTypeProperties props;
  for (auto &field : Layout->getFields()) {
    auto fieldProps = field.getLoweredType()->getRecursiveProperties();
    fieldProps.removeHasTypeParameter();
    fieldProps.removeHasDependentMember();
    props |= fieldProps;
  }
  for (auto replacementType : subMap.getReplacementTypes()) {
    if (replacementType) props |= replacementType->getRecursiveProperties();
  }
  return props;
}

SILBoxType::SILBoxType(ASTContext &C,
                       SILLayout *Layout, SubstitutionMap Substitutions)
  : TypeBase(TypeKind::SILBox, &C,
             getBoxRecursiveProperties(Layout, Substitutions)),
    Layout(Layout), Substitutions(Substitutions) {
  assert(Substitutions.isCanonical());
}

AnyFunctionType *AnyFunctionType::getWithoutDifferentiability() const {
  SmallVector<Param, 8> newParams;
  for (auto &param : getParams()) {
    Param newParam(param.getPlainType(), param.getLabel(),
                   param.getParameterFlags().withNoDerivative(false));
    newParams.push_back(newParam);
  }
  auto nonDiffExtInfo =
      getExtInfo()
          .intoBuilder()
          .withDifferentiabilityKind(DifferentiabilityKind::NonDifferentiable)
          .build();
  if (isa<FunctionType>(this))
    return FunctionType::get(newParams, getResult(), nonDiffExtInfo);
  assert(isa<GenericFunctionType>(this));
  return GenericFunctionType::get(getOptGenericSignature(), newParams,
                                  getResult(), nonDiffExtInfo);
}

AnyFunctionType *AnyFunctionType::getWithoutThrowing() const {
  auto info = getExtInfo().intoBuilder().withThrows(false, Type()).build();
  return withExtInfo(info);
}

AnyFunctionType *
AnyFunctionType::withIsolation(FunctionTypeIsolation isolation) const {
  auto info = getExtInfo().intoBuilder().withIsolation(isolation).build();
  return withExtInfo(info);
}

AnyFunctionType *AnyFunctionType::withSendable(bool newValue) const {
  auto info = getExtInfo().intoBuilder().withSendable(newValue).build();
  return withExtInfo(info);
}

std::optional<Type> AnyFunctionType::getEffectiveThrownErrorType() const {
  // A non-throwing function... has no thrown interface type.
  if (!isThrowing())
    return std::nullopt;

  // If there is no specified thrown error type, it throws "any Error".
  Type thrownError = getThrownError();
  if (!thrownError)
    return getASTContext().getErrorExistentialType();

  // If the thrown interface type is "Never", this function does not throw.
  if (thrownError->isNever())
    return std::nullopt;

  // Otherwise, return the typed error.
  return thrownError;
}

Type AnyFunctionType::getEffectiveThrownErrorTypeOrNever() const {
  if (auto thrown = getEffectiveThrownErrorType())
    return *thrown;

  return getASTContext().getNeverType();
}

std::optional<TangentSpace>
TypeBase::getAutoDiffTangentSpace(LookupConformanceFn lookupConformance) {
  assert(lookupConformance);
  auto &ctx = getASTContext();

  Type cacheKey = this;
  auto lookup = ctx.AutoDiffTangentSpaces.find(cacheKey);
  if (lookup != ctx.AutoDiffTangentSpaces.end())
    return lookup->getSecond();
  auto cache = [&](std::optional<TangentSpace> tangentSpace) {
    ctx.AutoDiffTangentSpaces.insert({cacheKey, tangentSpace});
    return tangentSpace;
  };

  // For tuple types: the tangent space is a tuple of the elements' tangent
  // space types, for the elements that have a tangent space.
  if (auto *tupleTy = getAs<TupleType>()) {
    SmallVector<TupleTypeElt, 8> newElts;
    for (auto elt : tupleTy->getElements()) {
      auto eltSpace = elt.getType()->getAutoDiffTangentSpace(lookupConformance);
      if (!eltSpace)
        continue;
      newElts.push_back(elt.getWithType(eltSpace->getType()));
    }
    if (newElts.empty())
      return cache(
          TangentSpace::getTuple(ctx.TheEmptyTupleType->castTo<TupleType>()));
    if (newElts.size() == 1)
      return cache(TangentSpace::getTangentVector(newElts.front().getType()));
    auto *tupleType = TupleType::get(newElts, ctx);
    return cache(TangentSpace::getTuple(tupleType));
  }

  if (auto *expansionType = getAs<PackExpansionType>()) {
    auto patternTypeSpace =
      expansionType->getPatternType()->getAutoDiffTangentSpace(lookupConformance);
    if (!patternTypeSpace)
      return cache(std::nullopt);
    auto countType = expansionType->getCountType();
    auto *packExpansionTanType =
      PackExpansionType::get(patternTypeSpace->getType(), countType);
    return cache(TangentSpace::getPackExpansion(packExpansionTanType));
  }
  
  if (auto *silPackType = getAs<SILPackType>()) {
    SmallVector<CanType, 4> newEltTypes;
    for (auto eltTy : silPackType->getElementTypes()) {
      auto eltSpace = eltTy->getAutoDiffTangentSpace(lookupConformance);
      if (!eltSpace)
        continue;
      newEltTypes.push_back(eltSpace->getCanonicalType());
    }
    auto silPackTanType = SILPackType::get(ctx, silPackType->getExtInfo(),
                                           newEltTypes);
    return cache(TangentSpace::getSILPack(silPackTanType));
  }
  
  // For `Differentiable`-conforming types: the tangent space is the
  // `TangentVector` associated type.
  auto *differentiableProtocol =
      ctx.getProtocol(KnownProtocolKind::Differentiable);
  if (!differentiableProtocol)
    return cache(std::nullopt);
  auto associatedTypeLookup =
      differentiableProtocol->lookupDirect(ctx.Id_TangentVector);
  assert(associatedTypeLookup.size() == 1);
  auto *assocDecl = cast<AssociatedTypeDecl>(associatedTypeLookup[0]);

  // Try to get the `TangentVector` associated type of `base`.
  // Return the associated type if it is valid.
  auto conformance = swift::lookupConformance(this, differentiableProtocol);
  auto assocTy = conformance.getTypeWitness(assocDecl);
  if (!assocTy->hasError())
    return cache(TangentSpace::getTangentVector(assocTy));

  // Otherwise, there is no associated tangent space. Return `None`.
  return cache(std::nullopt);
}

bool TypeBase::isForeignReferenceType() {
  if (auto *classDecl = lookThroughAllOptionalTypes()->getClassOrBoundGenericClass())
    return classDecl->isForeignReferenceType();
  return false;
}

bool TypeBase::hasSimpleTypeRepr() const {
  // NOTE: Please keep this logic in sync with TypeRepr::isSimple().
  switch (getKind()) {
  case TypeKind::Function:
  case TypeKind::GenericFunction:
    return false;

  case TypeKind::Metatype:
    return !cast<const AnyMetatypeType>(this)->hasRepresentation();

  case TypeKind::ExistentialMetatype:
  case TypeKind::Existential:
    return false;

  case TypeKind::OpaqueTypeArchetype:
  case TypeKind::ExistentialArchetype:
    return false;

  case TypeKind::PrimaryArchetype: {
    auto archetype = cast<const PrimaryArchetypeType>(this);
    auto interface = archetype->getInterfaceType();
    return interface->hasSimpleTypeRepr();
  }

  case TypeKind::ProtocolComposition: {
    auto composition = cast<const ProtocolCompositionType>(this);

    // A protocol composition is simple if its syntactic representation does not
    // involve `&`. This is true if we have 'Any', 'AnyObject', or a single
    // inverse requirement like `~Copyable`.

    // All other protocol compositions contain at least two `&`-separated terms.

    // Add each logical member Foo.
    auto memberCount = composition->getMembers().size();

    // And each inverse requirement ~Foo.
    for (auto ip : composition->getInverses()) {
      (void) ip;
      ++memberCount;
    }

    // And finally, AnyObject.
    if (composition->hasExplicitAnyObject())
      ++memberCount;

    // Almost always, this will be > 1.
    return memberCount <= 1;
  }

  case TypeKind::GenericTypeParam: {
    if (cast<const GenericTypeParamType>(this)->getOpaqueDecl())
      return false;
    return true;
  }

  default:
    return true;
  }
}

bool CanType::isErrorExistentialType() const {
  if (!isExistentialTypeImpl(*this))
    return false;

  return const_cast<CanType *>(this)->getExistentialLayout().isErrorExistential();
}

bool CanType::isForeignReferenceType() {
  if (auto *classDecl = getPointer()->lookThroughAllOptionalTypes()->getClassOrBoundGenericClass())
    return classDecl->isForeignReferenceType();
  return false;
}

// Creates an `AnyFunctionType` from the given parameters, result type,
// generic signature, and `ExtInfo`.
static AnyFunctionType *
makeFunctionType(ArrayRef<AnyFunctionType::Param> parameters, Type resultType,
                 GenericSignature genericSignature,
                 AnyFunctionType::ExtInfo extInfo) {
  if (genericSignature)
    return GenericFunctionType::get(genericSignature, parameters, resultType,
                                    extInfo);
  return FunctionType::get(parameters, resultType, extInfo);
}

AnyFunctionType *AnyFunctionType::getAutoDiffDerivativeFunctionType(
    IndexSubset *parameterIndices, AutoDiffDerivativeFunctionKind kind,
    LookupConformanceFn lookupConformance, GenericSignature derivativeGenSig,
    bool makeSelfParamFirst) {
  assert(!parameterIndices->isEmpty() &&
         "Expected at least one differentiability parameter");
  auto &ctx = getASTContext();

  // If `derivativeGenSig` is not defined, use the current function's type
  // generic signature.
  if (!derivativeGenSig)
    derivativeGenSig = getOptGenericSignature();

  // Unwrap curry levels. At most, two parameter lists are necessary, for
  // curried method types with a `(Self)` parameter list.
  // TODO(TF-874): Simplify curry level logic.
  SmallVector<AnyFunctionType *, 2> curryLevels;
  auto *currentLevel = castTo<AnyFunctionType>();
  for (unsigned i : range(2)) {
    (void)i;
    if (currentLevel == nullptr)
      break;
    curryLevels.push_back(currentLevel);
    currentLevel = currentLevel->getResult()->getAs<AnyFunctionType>();
  }

  auto originalResult = curryLevels.back()->getResult();

  auto linearMapTypeExpected = getAutoDiffDerivativeFunctionLinearMapType(
      parameterIndices, kind.getLinearMapKind(), lookupConformance,
      makeSelfParamFirst);
  assert(linearMapTypeExpected && "Linear map type is invalid");
  Type linearMapType = linearMapTypeExpected.get();

  // Build the full derivative function type: `(T...) -> (R, LinearMapType)`.
  SmallVector<TupleTypeElt, 2> retElts;
  retElts.push_back(originalResult);
  retElts.push_back(linearMapType);
  auto retTy = TupleType::get(retElts, ctx);
  auto *derivativeFunctionType =
      makeFunctionType(curryLevels.back()->getParams(), retTy,
                       curryLevels.size() == 1 ? derivativeGenSig : nullptr,
                       curryLevels.back()->getExtInfo());

  // Wrap the derivative function type in additional curry levels.
  auto curryLevelsWithoutLast =
      ArrayRef<AnyFunctionType *>(curryLevels).drop_back(1);
  for (auto pair : enumerate(llvm::reverse(curryLevelsWithoutLast))) {
    unsigned i = pair.index();
    auto *curryLevel = pair.value();
    derivativeFunctionType = makeFunctionType(
        curryLevel->getParams(), derivativeFunctionType,
        i == curryLevelsWithoutLast.size() - 1 ? derivativeGenSig : nullptr,
        curryLevel->getExtInfo());
  }

  return derivativeFunctionType;
}

llvm::Expected<AnyFunctionType *>
AnyFunctionType::getAutoDiffDerivativeFunctionLinearMapType(
    IndexSubset *parameterIndices, AutoDiffLinearMapKind kind,
    LookupConformanceFn lookupConformance, bool makeSelfParamFirst) {
  auto &ctx = getASTContext();
  // Error if differentiability parameter indices are empty.
  if (parameterIndices->isEmpty())
    return llvm::make_error<DerivativeFunctionTypeError>(
        this, DerivativeFunctionTypeError::Kind::NoDifferentiabilityParameters);

  // Get differentiability parameters.
  SmallVector<AnyFunctionType::Param, 8> diffParams;
  getSubsetParameters(parameterIndices, diffParams,
                      /*reverseCurryLevels*/ !makeSelfParamFirst);

  // Get the original non-inout semantic result types.
  SmallVector<AutoDiffSemanticFunctionResultType, 1> originalResults;
  autodiff::getFunctionSemanticResults(this, parameterIndices, originalResults);
  // Error if no original semantic results.
  if (originalResults.empty())
    return llvm::make_error<DerivativeFunctionTypeError>(
        this, DerivativeFunctionTypeError::Kind::NoSemanticResults);

  // Accumulate non-semantic result tangent spaces.
  SmallVector<Type, 1> resultTanTypes, inoutTanTypes;
  for (auto i : range(originalResults.size())) {
    auto originalResult = originalResults[i];
    auto originalResultType = originalResult.type;

    // Voids currently have a defined tangent vector, so ignore them.
    if (originalResultType->isVoid())
      continue;

    // Get the original semantic result type's `TangentVector` associated type.
    // Error if a semantic result has no tangent space.
    auto resultTan =
        originalResultType->getAutoDiffTangentSpace(lookupConformance);
    if (!resultTan)
      return llvm::make_error<DerivativeFunctionTypeError>(
        this, DerivativeFunctionTypeError::Kind::NonDifferentiableResult,
        DerivativeFunctionTypeError::TypeAndIndex(
          originalResultType, unsigned(originalResult.index)));

    if (!originalResult.isSemanticResultParameter)
      resultTanTypes.push_back(resultTan->getType());
  }

  // Compute the result linear map function type.
  FunctionType *linearMapType;
  switch (kind) {
  case AutoDiffLinearMapKind::Differential: {
    // Compute the differential type, returned by JVP functions.
    //
    // Case 1: original function has no `inout` parameters.
    // - Original:     `(T0, T1, ...) -> R`
    // - Differential: `(T0.Tan, T1.Tan, ...) -> R.Tan`
    //
    // Case 2: original function has a wrt `inout` parameter.
    // - Original:      `(T0, inout T1, ...) -> Void`
    // - Differential:  `(T0.Tan, inout T1.Tan, ...) -> Void`
    SmallVector<AnyFunctionType::Param, 4> differentialParams;
    for (auto i : range(diffParams.size())) {
      auto diffParam = diffParams[i];
      auto paramType = diffParam.getPlainType();
      auto paramTan = paramType->getAutoDiffTangentSpace(lookupConformance);
      // Error if parameter has no tangent space.
      if (!paramTan)
        return llvm::make_error<DerivativeFunctionTypeError>(
            this,
            DerivativeFunctionTypeError::Kind::
                NonDifferentiableDifferentiabilityParameter,
            DerivativeFunctionTypeError::TypeAndIndex(paramType, i));

      differentialParams.push_back(AnyFunctionType::Param(
          paramTan->getType(), Identifier(), diffParam.getParameterFlags()));
    }
    Type differentialResult;
    if (resultTanTypes.empty()) {
      differentialResult = ctx.TheEmptyTupleType;
    } else if (resultTanTypes.size() == 1) {
      differentialResult = resultTanTypes.front();
    } else {
      SmallVector<TupleTypeElt, 2> differentialResults;
      for (auto i : range(resultTanTypes.size())) {
        auto resultTanType = resultTanTypes[i];
        differentialResults.push_back(
            TupleTypeElt(resultTanType, Identifier()));
      }
      differentialResult = TupleType::get(differentialResults, ctx);
    }

    // FIXME: Verify ExtInfo state is correct, not working by accident.
    FunctionType::ExtInfo info;
    linearMapType =
        FunctionType::get(differentialParams, differentialResult, info);
    break;
  }
  case AutoDiffLinearMapKind::Pullback: {
    // Compute the pullback type, returned by VJP functions.
    //
    // Case 1: original function has no `inout` parameters.
    // - Original: `(T0, T1, ...) -> R`
    // - Pullback: `R.Tan -> (T0.Tan, T1.Tan, ...)`
    //
    // Case 2: original function has wrt `inout` parameters.
    // - Original: `(T0, inout T1, ...) -> R`
    // - Pullback: `(R.Tan, inout T1.Tan) -> (T0.Tan, ...)`
    SmallVector<TupleTypeElt, 4> pullbackResults;
    SmallVector<AnyFunctionType::Param, 2> semanticResultParams;
    for (auto i : range(diffParams.size())) {
      auto diffParam = diffParams[i];
      auto paramType = diffParam.getPlainType();
      auto paramTan = paramType->getAutoDiffTangentSpace(lookupConformance);
      // Error if parameter has no tangent space.
      if (!paramTan)
        return llvm::make_error<DerivativeFunctionTypeError>(
            this,
            DerivativeFunctionTypeError::Kind::
                NonDifferentiableDifferentiabilityParameter,
            DerivativeFunctionTypeError::TypeAndIndex(paramType, i));

      if (diffParam.isAutoDiffSemanticResult()) {
        if (paramType->isVoid())
          continue;
        semanticResultParams.push_back(diffParam);
        continue;
      }
      pullbackResults.emplace_back(paramTan->getType());
    }
    Type pullbackResult;
    if (pullbackResults.empty()) {
      pullbackResult = ctx.TheEmptyTupleType;
    } else if (pullbackResults.size() == 1 && !containsPackExpansionParam()) {
      pullbackResult = pullbackResults.front().getType();
    } else {
      pullbackResult = TupleType::get(pullbackResults, ctx);
    }
    // First accumulate non-inout results as pullback parameters.
    SmallVector<FunctionType::Param, 2> pullbackParams;
    for (auto i : range(resultTanTypes.size())) {
      auto resultTanType = resultTanTypes[i];
      auto flags = ParameterTypeFlags().withInOut(false);
      pullbackParams.push_back(AnyFunctionType::Param(
          resultTanType, Identifier(), flags));
    }
    // Then append semantic result parameters.
    for (auto i : range(semanticResultParams.size())) {
      auto semanticResultParam = semanticResultParams[i];
      auto semanticResultParamType = semanticResultParam.getPlainType();
      auto semanticResultParamTan =
          semanticResultParamType->getAutoDiffTangentSpace(lookupConformance);
      auto flags = ParameterTypeFlags().withInOut(true);
      pullbackParams.push_back(AnyFunctionType::Param(
          semanticResultParamTan->getType(), Identifier(), flags));
    }
    // FIXME: Verify ExtInfo state is correct, not working by accident.
    FunctionType::ExtInfo info;
    linearMapType = FunctionType::get(pullbackParams, pullbackResult, info);
    break;
  }
  }

  assert(linearMapType && "Expected linear map type");
  return linearMapType;
}

CanSILFunctionType
SILFunctionType::withInvocationSubstitutions(SubstitutionMap subs) const {
  subs = subs.getCanonical();
  if (subs == getInvocationSubstitutions())
    return CanSILFunctionType(const_cast<SILFunctionType*>(this));

  assert(!subs || CanGenericSignature(subs.getGenericSignature())
                    == getInvocationGenericSignature());
  return SILFunctionType::get(getInvocationGenericSignature(),
                          getExtInfo(), getCoroutineKind(),
                          getCalleeConvention(),
                          getParameters(), getYields(), getResults(),
                          getOptionalErrorResult(),
                          getPatternSubstitutions(), subs,
                          const_cast<SILFunctionType*>(this)->getASTContext(),
                          getWitnessMethodConformanceOrInvalid());
}

CanSILFunctionType
SILFunctionType::withPatternSubstitutions(SubstitutionMap subs) const {
  subs = subs.getCanonical();
  if (subs == getPatternSubstitutions())
    return CanSILFunctionType(const_cast<SILFunctionType*>(this));

  assert(!subs || CanGenericSignature(subs.getGenericSignature())
                    == getPatternGenericSignature());
  return SILFunctionType::get(getInvocationGenericSignature(),
                          getExtInfo(), getCoroutineKind(),
                          getCalleeConvention(),
                          getParameters(), getYields(), getResults(),
                          getOptionalErrorResult(),
                          subs, getInvocationSubstitutions(),
                          const_cast<SILFunctionType*>(this)->getASTContext(),
                          getWitnessMethodConformanceOrInvalid());
}

CanSILFunctionType
SILFunctionType::withPatternSpecialization(CanGenericSignature sig,
                                           SubstitutionMap subs,
                                           ProtocolConformanceRef
                                             witnessConformance) const {
  assert(!hasInvocationSubstitutions());
  subs = subs.getCanonical();
  assert(!subs || CanGenericSignature(subs.getGenericSignature())
                    == getSubstGenericSignature());
  return SILFunctionType::get(sig,
                          getExtInfo(), getCoroutineKind(),
                          getCalleeConvention(),
                          getParameters(), getYields(), getResults(),
                          getOptionalErrorResult(),
                          subs, SubstitutionMap(),
                          const_cast<SILFunctionType*>(this)->getASTContext(),
                          witnessConformance);
}

CanSILFunctionType SILFunctionType::withSendable(bool newValue) const {
  return withExtInfo(getExtInfo().withSendable(newValue));
}

CanSILFunctionType SILFunctionType::withExtInfo(ExtInfo newExtInfo) const {
  return SILFunctionType::get(
      getInvocationGenericSignature(), newExtInfo, getCoroutineKind(),
      getCalleeConvention(), getParameters(), getYields(), getResults(),
      getOptionalErrorResult(), getPatternSubstitutions(),
      getInvocationSubstitutions(),
      const_cast<SILFunctionType *>(this)->getASTContext(),
      getWitnessMethodConformanceOrInvalid());
}

APInt IntegerType::getValue() const {
  return BuiltinIntegerWidth::arbitrary().parse(getDigitsText(), /*radix*/ 0,
                                                isNegative());
}

SourceLoc swift::extractNearestSourceLoc(Type ty) {
  if (auto nominal = ty->getAnyNominal())
    return extractNearestSourceLoc(nominal);

  return SourceLoc();
}

StringRef swift::getNameForParamSpecifier(ParamSpecifier specifier) {
  switch (specifier) {
  case ParamSpecifier::Default:
    return "default";
  case ParamSpecifier::InOut:
    return "inout";
  case ParamSpecifier::Borrowing:
    return "borrowing";
  case ParamSpecifier::Consuming:
    return "consuming";
  case ParamSpecifier::LegacyShared:
    return "__shared";
  case ParamSpecifier::LegacyOwned:
    return "__owned";
  case ParamSpecifier::ImplicitlyCopyableConsuming:
    return "implicitly_copyable_consuming";
  }
  llvm_unreachable("bad ParamSpecifier");
}

static std::optional<DiagnosticBehavior>
getConcurrencyDiagnosticBehaviorLimitRec(
    Type type, DeclContext *declCtx,
    llvm::SmallPtrSetImpl<NominalTypeDecl *> &visited) {
  if (auto *nomDecl = type->getNominalOrBoundGenericNominal()) {
    // If we have already seen this type, treat it as having no limit.
    if (!visited.insert(nomDecl).second)
      return std::nullopt;

    // First try to just grab the exact concurrency diagnostic behavior.
    if (auto result =
            swift::getConcurrencyDiagnosticBehaviorLimit(nomDecl, declCtx)) {
      return result;
    }

    // But if we get nothing, see if we can come up with diagnostic behavior by
    // merging our fields if we have a struct.
    if (auto *structDecl = dyn_cast<StructDecl>(nomDecl)) {
      std::optional<DiagnosticBehavior> diagnosticBehavior;
      auto substMap = type->getContextSubstitutionMap();
      for (auto storedProperty : structDecl->getStoredProperties()) {
        auto lhs = diagnosticBehavior.value_or(DiagnosticBehavior::Unspecified);
        auto astType = storedProperty->getInterfaceType().subst(substMap);
        auto rhs = getConcurrencyDiagnosticBehaviorLimitRec(astType, declCtx,
                                                            visited);
        auto result = lhs.merge(rhs.value_or(DiagnosticBehavior::Unspecified));
        if (result != DiagnosticBehavior::Unspecified)
          diagnosticBehavior = result;
      }
      return diagnosticBehavior;
    }
  }

  // When attempting to determine the diagnostic behavior limit of a tuple, just
  // merge for each of the elements.
  if (auto *tupleType = type->getAs<TupleType>()) {
    std::optional<DiagnosticBehavior> diagnosticBehavior;
    for (auto tupleType : tupleType->getElements()) {
      auto lhs = diagnosticBehavior.value_or(DiagnosticBehavior::Unspecified);

      auto type = tupleType.getType()->getCanonicalType();
      auto rhs = getConcurrencyDiagnosticBehaviorLimitRec(type, declCtx,
                                                          visited);
      auto result = lhs.merge(rhs.value_or(DiagnosticBehavior::Unspecified));
      if (result != DiagnosticBehavior::Unspecified)
        diagnosticBehavior = result;
    }
    return diagnosticBehavior;
  }

  // Metatypes that aren't Sendable were introduced in Swift 6.2, so downgrade
  // them to warnings prior to Swift 7.
  if (type->is<AnyMetatypeType>()) {
    if (!type->getASTContext().LangOpts.isSwiftVersionAtLeast(7))
      return DiagnosticBehavior::Warning;
  }

  return std::nullopt;
}

std::optional<DiagnosticBehavior>
TypeBase::getConcurrencyDiagnosticBehaviorLimit(DeclContext *declCtx) const {
  auto *self = const_cast<TypeBase *>(this);
  llvm::SmallPtrSet<NominalTypeDecl *, 16> visited;
  return getConcurrencyDiagnosticBehaviorLimitRec(Type(self), declCtx, visited);
}

GenericTypeParamKind
TypeBase::getMatchingParamKind() {
  if (auto gtpt = dyn_cast<GenericTypeParamType>(this)) {
    return gtpt->getParamKind();
  }
  
  if (auto arch = dyn_cast<ArchetypeType>(this)) {
    return arch->mapTypeOutOfContext()->getMatchingParamKind();
  }
  
  if (isa<IntegerType>(this)) {
    return GenericTypeParamKind::Value;
  }
  
  if (isa<PackType>(this)) {
    return GenericTypeParamKind::Pack;
  }
  
  return GenericTypeParamKind::Type;
}
