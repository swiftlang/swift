//===--- Type.cpp - Swift Language Type ASTs ------------------------------===//
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
//  This file implements the Type class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "ForeignRepresentationInfo.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/TypeWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/TypeLoc.h"
#include "swift/AST/TypeRepr.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <functional>
#include <iterator>
using namespace swift;

Type QueryTypeSubstitutionMap::operator()(SubstitutableType *type) const {
  auto key = type->getCanonicalType()->castTo<SubstitutableType>();
  auto known = substitutions.find(key);
  if (known != substitutions.end() && known->second)
    return known->second;

  // Not known.
  return Type();
}

Type
QueryTypeSubstitutionMapOrIdentity::operator()(SubstitutableType *type) const {
  auto key = type->getCanonicalType()->castTo<SubstitutableType>();
  auto known = substitutions.find(key);
  if (known != substitutions.end() && known->second)
    return known->second;
  
  return type;
}

Type QuerySubstitutionMap::operator()(SubstitutableType *type) const {
  auto key = cast<SubstitutableType>(type->getCanonicalType());
  return subMap.lookupSubstitution(key);
}

void TypeLoc::setType(Type Ty, bool validated) {
  assert(!Ty || !Ty->hasTypeVariable());
  TAndValidBit.setPointerAndInt(Ty, validated);
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

TypeLoc TypeLoc::clone(ASTContext &ctx) const {
  if (TyR) {
    TypeLoc result(TyR->clone(ctx));
    result.TAndValidBit = this->TAndValidBit;
    return result;
  }
  return *this;
}

SourceLoc TypeLoc::getLoc() const {
  if (TyR) return TyR->getLoc();
  return SourceLoc();
}

// Only allow allocation of Types using the allocator in ASTContext.
void *TypeBase::operator new(size_t bytes, const ASTContext &ctx,
                             AllocationArena arena, unsigned alignment) {
  return ctx.Allocate(bytes, alignment, arena);
}

bool CanType::isActuallyCanonicalOrNull() const {
  return getPointer() == nullptr ||
         getPointer() == llvm::DenseMapInfo<TypeBase *>::getTombstoneKey() ||
         getPointer()->isCanonical();
}

NominalTypeDecl *CanType::getAnyNominal() const {
  return dyn_cast_or_null<NominalTypeDecl>(getAnyGeneric());
}

GenericTypeDecl *CanType::getAnyGeneric() const {
  if (auto Ty = dyn_cast<AnyGenericType>(*this))
    return Ty->getDecl();
  return nullptr;
}

//===----------------------------------------------------------------------===//
// Various Type Methods.
//===----------------------------------------------------------------------===//

/// isEqual - Return true if these two types are equal, ignoring sugar.
bool TypeBase::isEqual(Type Other) {
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
      if (enumDecl->getAllElements().empty())
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
  return isEqual(getASTContext().TheAnyType);
}

bool TypeBase::isAnyClassReferenceType() {
  return getCanonicalType().isAnyClassReferenceType();
}

bool CanType::isReferenceTypeImpl(CanType type, bool functionsCount) {
  switch (type->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("sugared canonical type?");

  // These types are always class references.
  case TypeKind::BuiltinUnknownObject:
  case TypeKind::BuiltinNativeObject:
  case TypeKind::BuiltinBridgeObject:
  case TypeKind::Class:
  case TypeKind::BoundGenericClass:
  case TypeKind::SILBox:
    return true;

  // For Self types, recur on the underlying type.
  case TypeKind::DynamicSelf:
    return isReferenceTypeImpl(cast<DynamicSelfType>(type).getSelfType(),
                               functionsCount);

  // Archetypes and existentials are only class references if class-bounded.
  case TypeKind::Archetype:
    return cast<ArchetypeType>(type)->requiresClass();
  case TypeKind::Protocol:
    return cast<ProtocolType>(type)->requiresClass();
  case TypeKind::ProtocolComposition:
    return cast<ProtocolCompositionType>(type)->requiresClass();

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
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinRawPointer:
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
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct:
  case TypeKind::SILToken:
  case TypeKind::UnownedStorage:
  case TypeKind::UnmanagedStorage:
  case TypeKind::WeakStorage:
    return false;

  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
    llvm_unreachable("Dependent types can't answer reference-semantics query");
  }

  llvm_unreachable("Unhandled type kind!");
}

/// hasOwnership - Are variables of this type permitted to have
/// ownership attributes?
///
/// This includes:
///   - class types, generic or not
///   - archetypes with class or class protocol bounds
///   - existentials with class or class protocol bounds
/// But not:
///   - function types
bool TypeBase::allowsOwnership() {
  return getCanonicalType().isAnyClassReferenceType();
}

ExistentialLayout::ExistentialLayout(ProtocolType *type) {
  assert(type->isCanonical());

  auto *protoDecl = type->getDecl();

  hasExplicitAnyObject = false;
  containsNonObjCProtocol = !protoDecl->isObjC();

  singleProtocol = type;
}

ExistentialLayout::ExistentialLayout(ProtocolCompositionType *type) {
  assert(type->isCanonical());

  hasExplicitAnyObject = type->hasExplicitAnyObject();
  containsNonObjCProtocol = false;

  auto members = type->getMembers();
  if (!members.empty() &&
      isa<ClassDecl>(members[0]->getAnyNominal())) {
    superclass = members[0];
    members = members.slice(1);
  }

  for (auto member : members) {
    auto *protoDecl = member->castTo<ProtocolType>()->getDecl();
    containsNonObjCProtocol |= !protoDecl->isObjC();
  }

  singleProtocol = nullptr;
  protocols = { members.data(), members.size() };
}


ExistentialLayout TypeBase::getExistentialLayout() {
  return getCanonicalType().getExistentialLayout();
}

ExistentialLayout CanType::getExistentialLayout() {
  if (auto proto = dyn_cast<ProtocolType>(*this))
    return ExistentialLayout(proto);

  auto comp = cast<ProtocolCompositionType>(*this);
  return ExistentialLayout(comp);
}

bool ExistentialLayout::requiresClass() const {
  if (hasExplicitAnyObject || superclass)
    return true;

  for (auto proto : getProtocols()) {
    if (proto->requiresClass())
      return true;
  }

  return false;
}

bool ExistentialLayout::isAnyObject() const {
  return (hasExplicitAnyObject && !superclass && getProtocols().empty());
}

bool TypeBase::isObjCExistentialType() {
  return getCanonicalType().isObjCExistentialType();
}

bool CanType::isObjCExistentialTypeImpl(CanType type) {
  if (!type.isExistentialType())
    return false;

  return type.getExistentialLayout().isObjC();
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

bool TypeBase::hasOpenedExistential(ArchetypeType *opened) {
  assert(opened->getOpenedExistentialType() &&
         "not an opened existential type");

  if (!hasOpenedExistential())
    return false;

  return getCanonicalType().findIf([&](Type type) -> bool {
    return opened == dyn_cast<ArchetypeType>(type.getPointer());
  });
}

void TypeBase::getOpenedExistentials(
       SmallVectorImpl<ArchetypeType *> &opened) {
  if (!hasOpenedExistential())
    return;

  SmallPtrSet<ArchetypeType *, 4> known;
  getCanonicalType().findIf([&](Type type) -> bool {
    auto archetype = dyn_cast<ArchetypeType>(type.getPointer());
    if (!archetype)
      return false;

    if (!archetype->getOpenedExistentialType())
      return false;

    if (known.insert(archetype).second)
      opened.push_back(archetype);

    return false;
  });
}

Type TypeBase::eraseOpenedExistential(ArchetypeType *opened) {
  assert(opened->getOpenedExistentialType() &&
         "Not an opened existential type?");

  if (!hasOpenedExistential())
    return Type(this);

  auto existentialType = opened->getOpenedExistentialType();

  return Type(this).transform([&](Type t) -> Type {
    // A metatype with an opened existential type becomes an
    // existential metatype.
    if (auto *metatypeType = dyn_cast<MetatypeType>(t.getPointer())) {
      auto instanceType = metatypeType->getInstanceType();
      if (instanceType->hasOpenedExistential()) {
        instanceType = instanceType->eraseOpenedExistential(opened);
        return ExistentialMetatypeType::get(instanceType);
      }
    }

    // @opened P => P
    if (auto *archetypeType = dyn_cast<ArchetypeType>(t.getPointer())) {
      if (archetypeType == opened)
        return existentialType;
    }

    return t;
  });
}

Type TypeBase::eraseDynamicSelfType() {
  if (!hasDynamicSelfType())
    return this;

  return Type(this).transform([](Type t) -> Type {
    if (auto *selfTy = dyn_cast<DynamicSelfType>(t.getPointer()))
      return selfTy->getSelfType();
    return t;
  });
}

Type TypeBase::addCurriedSelfType(const DeclContext *dc) {
  if (!dc->isTypeContext())
    return this;

  auto *type = this;

  GenericSignature *sig = dc->getGenericSignatureOfContext();
  if (auto *genericFn = type->getAs<GenericFunctionType>()) {
    sig = genericFn->getGenericSignature();
    type = FunctionType::get(genericFn->getInput(),
                             genericFn->getResult(),
                             genericFn->getExtInfo());
  }

  auto selfTy = dc->getDeclaredInterfaceType();
  if (sig)
    return GenericFunctionType::get(sig, selfTy, type,
                                    AnyFunctionType::ExtInfo());
  return FunctionType::get(selfTy, type);
}

void
TypeBase::getTypeVariables(SmallVectorImpl<TypeVariableType *> &typeVariables) {
  // If we know we don't have any type variables, we're done.
  if (hasTypeVariable()) {
    auto addTypeVariables = [&](Type type) -> bool {
      if (auto tv = dyn_cast<TypeVariableType>(type.getPointer())) {
        typeVariables.push_back(tv);
      }

      return false;
    };

    // Use Type::findIf() to walk the types, finding type variables along the
    // way.
    getCanonicalType().findIf(addTypeVariables);
    Type(this).findIf(addTypeVariables);
    assert((!typeVariables.empty() || hasError()) &&
           "Did not find type variables!");
  }
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
      if (!isLegalSILType(eltType)) return false;
    }
    return true;
  }

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

  return true;
}

bool TypeBase::isLegalFormalType() {
  return ::isLegalFormalType(getCanonicalType());
}

bool TypeBase::isVoid() {
  if (auto TT = getAs<TupleType>())
    return TT->getNumElements() == 0;
  return false;
}

/// \brief Check if this type is equal to Swift.Bool.
bool TypeBase::isBool() {
  if (auto NTD = getAnyNominal())
    if (isa<StructDecl>(NTD))
      return getASTContext().getBoolDecl() == NTD;
  return false;
}


bool TypeBase::isAssignableType() {
  if (hasLValueType()) return true;
  if (auto tuple = getAs<TupleType>()) {
    for (auto eltType : tuple->getElementTypes()) {
      if (!eltType->isAssignableType())
        return false;
    }
    return true;
  }
  return false;
}

Type TypeBase::getRValueType() {
  // If the type is not an lvalue, this is a no-op.
  if (!hasLValueType())
    return this;

  return Type(this).transform([](Type t) -> Type {
      if (auto *lvalueTy = dyn_cast<LValueType>(t.getPointer()))
        return lvalueTy->getObjectType();
      return t;
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

Type TypeBase::getAnyPointerElementType(PointerTypeKind &PTK) {
  auto &C = getASTContext();
  if (auto nominalTy = getAs<NominalType>()) {
    if (nominalTy->getDecl() == C.getUnsafeMutableRawPointerDecl()) {
      PTK = PTK_UnsafeMutableRawPointer;
      return C.TheEmptyTupleType;
    }
    if (nominalTy->getDecl() == C.getUnsafeRawPointerDecl()) {
      PTK = PTK_UnsafeRawPointer;
      return C.TheEmptyTupleType;
    }
  }
  if (auto boundTy = getAs<BoundGenericType>()) {
    if (boundTy->getDecl() == C.getUnsafeMutablePointerDecl()) {
      PTK = PTK_UnsafeMutablePointer;
    } else if (boundTy->getDecl() == C.getUnsafePointerDecl()) {
      PTK = PTK_UnsafePointer;
    } else if (
      boundTy->getDecl() == C.getAutoreleasingUnsafeMutablePointerDecl()
    ) {
      PTK = PTK_AutoreleasingUnsafeMutablePointer;
    } else {
      return Type();
    }
    return boundTy->getGenericArgs()[0];
  }
  return Type();
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

bool TypeBase::isAnyObject() {
  auto canTy = getCanonicalType();

  if (!canTy.isExistentialType())
    return false;

  return canTy.getExistentialLayout().isAnyObject();
}

bool ExistentialLayout::isErrorExistential() const {
  auto protocols = getProtocols();
  return (!hasExplicitAnyObject &&
          !superclass &&
          protocols.size() == 1 &&
          protocols[0]->getDecl()->isSpecificProtocol(KnownProtocolKind::Error));
}

bool ExistentialLayout::isExistentialWithError(ASTContext &ctx) const {
  auto errorProto = ctx.getProtocol(KnownProtocolKind::Error);
  if (!errorProto) return false;

  for (auto proto : getProtocols()) {
    auto *protoDecl = proto->getDecl();
    if (protoDecl == errorProto || protoDecl->inheritsFrom(errorProto))
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


static Type getStrippedType(const ASTContext &context, Type type,
                            bool stripLabels) {
  return type.transform([&](Type type) -> Type {
    auto *tuple = dyn_cast<TupleType>(type.getPointer());
    if (!tuple)
      return type;

    SmallVector<TupleTypeElt, 4> elements;
    bool anyChanged = false;
    unsigned idx = 0;
    for (const auto &elt : tuple->getElements()) {
      Type eltTy = getStrippedType(context, elt.getRawType(),
                                   stripLabels);
      if (anyChanged || eltTy.getPointer() != elt.getRawType().getPointer() ||
          (elt.hasName() && stripLabels)) {
        if (!anyChanged) {
          elements.reserve(tuple->getNumElements());
          for (unsigned i = 0; i != idx; ++i) {
            const TupleTypeElt &elt = tuple->getElement(i);
            Identifier newName = stripLabels? Identifier() : elt.getName();
            elements.push_back(elt.getWithName(newName));
          }
          anyChanged = true;
        }

        Identifier newName = stripLabels? Identifier() : elt.getName();
        elements.emplace_back(eltTy, newName, elt.getParameterFlags());
      }
      ++idx;
    }

    if (!anyChanged)
      return type;

    return TupleType::get(elements, context);
  });
}

Type TypeBase::getUnlabeledType(ASTContext &Context) {
  return getStrippedType(Context, Type(this), /*stripLabels=*/true);
}

Type TypeBase::getWithoutParens() {
  Type Ty = this;
  while (auto ParenTy = dyn_cast<ParenType>(Ty.getPointer()))
    Ty = ParenTy->getUnderlyingType();
  return Ty;
}

Type TypeBase::getWithoutImmediateLabel() {
  Type Ty = this;
  if (auto tupleTy = dyn_cast<TupleType>(Ty.getPointer())) {
    if (tupleTy->hasParenSema(/*allowName*/true))
      Ty = tupleTy->getElementType(0);
  }
  return Ty;
}

Type TypeBase::replaceCovariantResultType(Type newResultType,
                                          unsigned uncurryLevel) {
  if (uncurryLevel == 0) {
    if (auto objectType = getOptionalObjectType()) {
      assert(!newResultType->getOptionalObjectType());
      return OptionalType::get(
          objectType->replaceCovariantResultType(newResultType, uncurryLevel));
    }

    return newResultType;
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

SmallVector<AnyFunctionType::Param, 4>
swift::decomposeArgType(Type type, ArrayRef<Identifier> argumentLabels) {
  SmallVector<AnyFunctionType::Param, 4> result;
  switch (type->getKind()) {
  case TypeKind::Tuple: {
    auto tupleTy = cast<TupleType>(type.getPointer());
    
    // If we have one argument label but a tuple argument with > 1 element,
    // put the whole tuple into the argument.
    // FIXME: This horribleness is due to the mis-modeling of arguments as
    // ParenType or TupleType.
    if (argumentLabels.size() == 1 && tupleTy->getNumElements() > 1) {
      // Break out to do the default thing below.
      break;
    }
    
    for (auto i : range(0, tupleTy->getNumElements())) {
      const auto &elt = tupleTy->getElement(i);
      assert(!(elt.getParameterFlags().isAutoClosure() ||
              elt.getParameterFlags().isVariadic()) &&
             "Vararg or autoclosure argument tuple doesn't make sense");
      result.push_back(AnyFunctionType::Param(elt.getRawType(),
                                              argumentLabels[i],
                                              elt.getParameterFlags()));
    }
    return result;
  }
    
  case TypeKind::Paren: {
    auto parenTy = cast<ParenType>(type.getPointer());
    result.push_back(AnyFunctionType::Param(parenTy->getUnderlyingType()->getInOutObjectType(),
                                            Identifier(),
                                            parenTy->getParameterFlags()));
    return result;
  }
    
  default:
    // Default behavior below; inject the argument as the sole parameter.
    break;
  }
  
  // Just inject this parameter.
  assert(result.empty() && (argumentLabels.size() == 1));
  result.push_back(AnyFunctionType::Param(type->getInOutObjectType(), argumentLabels[0],
                                          ParameterTypeFlags().withInOut(type->is<InOutType>())));
  return result;
}

void swift::computeDefaultMap(ArrayRef<AnyFunctionType::Param> params,
                              const ValueDecl *paramOwner, unsigned level,
                              SmallVectorImpl<bool> &outDefaultMap) {
  // Find the corresponding parameter list.
  const ParameterList *paramList = nullptr;
  if (paramOwner) {
    if (auto *func = dyn_cast<AbstractFunctionDecl>(paramOwner)) {
      if (level < func->getNumParameterLists())
        paramList = func->getParameterList(level);
    } else if (auto *subscript = dyn_cast<SubscriptDecl>(paramOwner)) {
      if (level == 1)
        paramList = subscript->getIndices();
    } else if (auto *enumElement = dyn_cast<EnumElementDecl>(paramOwner)) {
      if (level == 1)
        paramList = enumElement->getParameterList();
    }
  }

  switch (params.size()) {
  case 0:
    break;

  case 1:
    outDefaultMap.push_back(paramList && paramList->size() == 1 &&
                            paramList->get(0)->isDefaultArgument());
    break;

  default:
    // Arguments and parameters are not guaranteed to always line-up
    // perfectly, e.g. failure diagnostics tries to match argument type
    // to different "candidate" parameters.
    if (paramList && params.size() != paramList->size())
      paramList = nullptr;

    for (auto i : range(0, params.size())) {
      outDefaultMap.push_back(paramList &&
                              paramList->get(i)->isDefaultArgument());
    }
    break;
  }
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
static Type rebuildSelfTypeWithObjectType(Type selfTy, Type objectTy) {
  auto existingObjectTy = selfTy->getRValueInstanceType();
  return selfTy.transform([=](Type type) -> Type {
    if (type->isEqual(existingObjectTy))
      return objectTy;
    return type;
  });
}

/// Returns a new function type exactly like this one but with the self
/// parameter replaced. Only makes sense for members of types.
Type TypeBase::replaceSelfParameterType(Type newSelf) {
  auto fnTy = castTo<AnyFunctionType>();
  Type input = rebuildSelfTypeWithObjectType(fnTy->getInput(), newSelf);

  if (auto genericFnTy = getAs<GenericFunctionType>()) {
    return GenericFunctionType::get(genericFnTy->getGenericSignature(),
                                    input,
                                    fnTy->getResult(),
                                    fnTy->getExtInfo());
  }

  return FunctionType::get(input,
                           fnTy->getResult(),
                           fnTy->getExtInfo());
}

/// Retrieve the object type for a 'self' parameter, digging into one-element
/// tuples, inout types, and metatypes.
Type TypeBase::getRValueInstanceType() {
  Type type = this;
  
  // Look through argument list tuples.
  if (auto tupleTy = type->getAs<TupleType>()) {
    if (tupleTy->hasParenSema(/*allowName*/true))
      type = tupleTy->getElementType(0);
  }
  
  if (auto metaTy = type->getAs<AnyMetatypeType>())
    return metaTy->getInstanceType();

  // For mutable value type methods, we need to dig through inout types.
  return type->getInOutObjectType();
}

/// \brief Collect the protocols in the existential type T into the given
/// vector.
static void addProtocols(Type T,
                         SmallVectorImpl<ProtocolDecl *> &Protocols,
                         Type &Superclass,
                         bool &HasExplicitAnyObject) {
  if (auto Proto = T->getAs<ProtocolType>()) {
    Protocols.push_back(Proto->getDecl());
    return;
  }

  if (auto PC = T->getAs<ProtocolCompositionType>()) {
    if (PC->hasExplicitAnyObject())
      HasExplicitAnyObject = true;
    for (auto P : PC->getMembers())
      addProtocols(P, Protocols, Superclass, HasExplicitAnyObject);
    return;
  }

  assert(isa<ClassDecl>(T->getAnyNominal()) && "Non-class, non-protocol "
         "member in protocol composition");
  assert((!Superclass || Superclass->isEqual(T)) &&
         "Should have diagnosed multiple superclasses by now");
  Superclass = T;
}

/// \brief Add the protocol (or protocols) in the type T to the stack of
/// protocols, checking whether any of the protocols had already been seen and
/// zapping those in the original list that we find again.
static void addMinimumProtocols(Type T,
                                SmallVectorImpl<ProtocolDecl *> &Protocols,
                           llvm::SmallDenseMap<ProtocolDecl *, unsigned> &Known,
                                llvm::SmallPtrSet<ProtocolDecl *, 16> &Visited,
                                SmallVector<ProtocolDecl *, 16> &Stack,
                                bool &ZappedAny) {
  if (auto Proto = T->getAs<ProtocolType>()) {
    auto KnownPos = Known.find(Proto->getDecl());
    if (KnownPos != Known.end()) {
      // We've come across a protocol that is in our original list. Zap it.
      Protocols[KnownPos->second] = nullptr;
      ZappedAny = true;
    }

    if (Visited.insert(Proto->getDecl()).second) {
      Stack.push_back(Proto->getDecl());
      for (auto Inherited : Proto->getDecl()->getInheritedProtocols())
        addMinimumProtocols(Inherited->getDeclaredType(), Protocols, Known,
                            Visited, Stack, ZappedAny);
    }
    return;
  }
  
  if (auto PC = T->getAs<ProtocolCompositionType>()) {
    for (auto C : PC->getMembers()) {
      addMinimumProtocols(C, Protocols, Known, Visited, Stack, ZappedAny);
    }
  }
}

bool ProtocolType::visitAllProtocols(
                                 ArrayRef<ProtocolDecl *> protocols,
                                 llvm::function_ref<bool(ProtocolDecl *)> fn) {
  SmallVector<ProtocolDecl *, 4> stack;
  SmallPtrSet<ProtocolDecl *, 4> knownProtocols;

  // Prepopulate the stack.
  for (auto proto : protocols) {
    if (knownProtocols.insert(proto).second)
      stack.push_back(proto);
  }
  std::reverse(stack.begin(), stack.end());

  while (!stack.empty()) {
    auto proto = stack.back();
    stack.pop_back();

    // Visit this protocol.
    if (fn(proto))
      return true;

    // Add inherited protocols that we haven't seen already.
    for (auto inherited : proto->getInheritedProtocols()) {
      if (knownProtocols.insert(inherited).second)
        stack.push_back(inherited);
    }
  }

  return false;
}

void ProtocolType::canonicalizeProtocols(
       SmallVectorImpl<ProtocolDecl *> &protocols) {
  llvm::SmallDenseMap<ProtocolDecl *, unsigned> known;
  llvm::SmallPtrSet<ProtocolDecl *, 16> visited;
  SmallVector<ProtocolDecl *, 16> stack;
  bool zappedAny = false;

  // Seed the stack with the protocol declarations in the original list.
  // Zap any obvious duplicates along the way.
  for (unsigned I = 0, N = protocols.size(); I != N; ++I) {
    // Check whether we've seen this protocol before.
    auto knownPos = known.find(protocols[I]);
    
    // If we have not seen this protocol before, record its index.
    if (knownPos == known.end()) {
      known[protocols[I]] = I;
      stack.push_back(protocols[I]);
      continue;
    }
    
    // We have seen this protocol before; zap this occurrence.
    protocols[I] = nullptr;
    zappedAny = true;
  }
  
  // Walk the inheritance hierarchies of all of the protocols. If we run into
  // one of the known protocols, zap it from the original list.
  while (!stack.empty()) {
    ProtocolDecl *Current = stack.back();
    stack.pop_back();
    
    // Add the protocols we inherited.
    for (auto Inherited : Current->getInheritedProtocols()) {
      addMinimumProtocols(Inherited->getDeclaredType(), protocols, known,
                          visited, stack, zappedAny);
    }
  }
  
  if (zappedAny)
    protocols.erase(std::remove(protocols.begin(), protocols.end(), nullptr),
                    protocols.end());

  // Sort the set of protocols by module + name, to give a stable
  // ordering.
  llvm::array_pod_sort(protocols.begin(), protocols.end(), TypeDecl::compare);
}

static Type
getCanonicalInputType(AnyFunctionType *funcType,
                      llvm::function_ref<CanType(Type)> getCanonicalType) {
  auto origInputType = funcType->getInput();
  bool isParen = origInputType->hasParenSugar();
  Type inputType = getCanonicalType(origInputType);

  if (!isParen && AnyFunctionType::isCanonicalFunctionInputType(inputType))
    return inputType;

  auto flags = ParameterTypeFlags().withInOut(inputType->is<InOutType>());
  if (auto *parenTy = dyn_cast<ParenType>(origInputType.getPointer())) {
    auto parenFlags = parenTy->getParameterFlags();
    flags =
        flags.withShared(parenFlags.isShared()).withOwned(parenFlags.isOwned());
  }

  inputType = ParenType::get(inputType->getASTContext(),
                             inputType->getInOutObjectType(), flags);
  assert(AnyFunctionType::isCanonicalFunctionInputType(inputType));

  return inputType;
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
    assert(gpDecl->getDepth() != GenericTypeParamDecl::InvalidDepth &&
           "parameter hasn't been validated");
    Result = GenericTypeParamType::get(gpDecl->getDepth(), gpDecl->getIndex(),
                                       gpDecl->getASTContext());
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

  case TypeKind::UnownedStorage:
  case TypeKind::UnmanagedStorage:
  case TypeKind::WeakStorage: {
    auto ref = cast<ReferenceStorageType>(this);
    Type referentType = ref->getReferentType()->getCanonicalType();
    Result = ReferenceStorageType::get(referentType, ref->getOwnership(),
                                       referentType->getASTContext());
    break;
  }
  case TypeKind::LValue:
    Result = LValueType::get(getRValueType()->getCanonicalType());
    break;
  case TypeKind::InOut:
    Result = InOutType::get(getInOutObjectType()->getCanonicalType());
    break;
  case TypeKind::GenericFunction: {
    GenericFunctionType *function = cast<GenericFunctionType>(this);

    // Canonicalize the signature.
    GenericSignature *sig = function->getGenericSignature()
      ->getCanonicalSignature();
    
    // Transform the input and result types.
    auto inputTy = getCanonicalInputType(function, [&](Type type) -> CanType {
      return type->getCanonicalType(sig);
    });
    auto resultTy = function->getResult()->getCanonicalType(sig);
    Result = GenericFunctionType::get(sig, inputTy, resultTy,
                                      function->getExtInfo());
    assert(Result->isCanonical());
    break;
  }
      
  case TypeKind::SILBlockStorage:
  case TypeKind::SILBox:
  case TypeKind::SILFunction:
  case TypeKind::SILToken:
    llvm_unreachable("SIL-only types are always canonical!");

  case TypeKind::Function: {
    FunctionType *FT = cast<FunctionType>(this);
    auto In = getCanonicalInputType(
        FT, [](Type type) -> CanType { return type->getCanonicalType(); });
    Type Out = FT->getResult()->getCanonicalType();
    Result = FunctionType::get(In, Out, FT->getExtInfo());
    break;
  }
  case TypeKind::ProtocolComposition: {
    auto *PCT = cast<ProtocolCompositionType>(this);
    SmallVector<Type, 4> CanProtos;
    for (Type t : PCT->getMembers())
      CanProtos.push_back(t->getCanonicalType());
    assert(!CanProtos.empty() && "Non-canonical empty composition?");
    const ASTContext &C = CanProtos[0]->getASTContext();
    Type Composition = ProtocolCompositionType::get(C, CanProtos,
                                                    PCT->hasExplicitAnyObject());
    Result = Composition.getPointer();
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
  }

  // Cache the canonical type for future queries.
  assert(Result && "Case not implemented!");
  CanonicalType = Result;
  return CanType(Result);
}

CanType TypeBase::getCanonicalType(GenericSignature *sig) {
  if (!sig)
    return getCanonicalType();

  return sig->getCanonicalTypeInContext(this);
}

TypeBase *TypeBase::reconstituteSugar(bool Recursive) {
  auto Func = [](Type Ty) -> Type {
    if (auto boundGeneric = dyn_cast<BoundGenericType>(Ty.getPointer())) {
      auto &ctx = boundGeneric->getASTContext();
      if (boundGeneric->getDecl() == ctx.getArrayDecl())
        return ArraySliceType::get(boundGeneric->getGenericArgs()[0]);
      if (boundGeneric->getDecl() == ctx.getDictionaryDecl())
        return DictionaryType::get(boundGeneric->getGenericArgs()[0],
                                   boundGeneric->getGenericArgs()[1]);
      if (boundGeneric->getDecl() == ctx.getOptionalDecl())
        return OptionalType::get(boundGeneric->getGenericArgs()[0]);
    }
    return Ty;
  };
  if (Recursive)
    return Type(this).transform(Func).getPointer();
  else
    return Func(this).getPointer();
}

#define TYPE(Id, Parent)
#define SUGARED_TYPE(Id, Parent) \
  static_assert(std::is_base_of<SugarType, Id##Type>::value, "Sugar mismatch");
#include "swift/AST/TypeNodes.def"

ParenType::ParenType(Type baseType, RecursiveTypeProperties properties,
                     ParameterTypeFlags flags)
  : SugarType(TypeKind::Paren,
              flags.isInOut() ? InOutType::get(baseType) : baseType,
              properties) {
  Bits.ParenType.Flags = flags.toRaw();
  if (flags.isInOut())
    assert(!baseType->is<InOutType>() && "caller did not pass a base type");
  if (baseType->is<InOutType>())
    assert(flags.isInOut() && "caller did not set flags correctly");
}


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
  case TypeKind::Paren:
    llvm_unreachable("parenthesis are sugar, but not syntax sugar");
  case TypeKind::NameAlias:
    llvm_unreachable("bound name alias types always have an underlying type");
  case TypeKind::ArraySlice:
    implDecl = Context->getArrayDecl();
    break;
  case TypeKind::Optional:
    implDecl = Context->getOptionalDecl();
    break;
  case TypeKind::Dictionary:
    implDecl = Context->getDictionaryDecl();
    break;
  }
  assert(implDecl && "Type has not been set yet");

  Bits.SugarType.HasCachedType = true;
  if (auto Ty = dyn_cast<UnarySyntaxSugarType>(this)) {
    UnderlyingType = BoundGenericType::get(implDecl, Type(), Ty->getBaseType());
  } else if (auto Ty = dyn_cast<DictionaryType>(this)) {
    UnderlyingType = BoundGenericType::get(implDecl, Type(),
                                      { Ty->getKeyType(), Ty->getValueType() });
  } else {
    llvm_unreachable("Not UnarySyntaxSugarType or DictionaryType?");
  }

  // Record the implementation type.
  return UnderlyingType;
}

SubstitutionMap NameAliasType::getSubstitutionMap() const {
  if (auto genericSig = getGenericSignature())
    return genericSig->getSubstitutionMap(getSubstitutionList());

  return SubstitutionMap();
}

SmallVector<Type, 2> NameAliasType::getInnermostGenericArgs() const {
  SmallVector<Type, 2> result;

  // If the typealias is not generic, there are no generic arguments
  if (!typealias->isGeneric()) return result;

  auto genericSig = typealias->getGenericSignature();
  if (!genericSig) return result;

  // If the substitution list was empty, bail out.
  if (getSubstitutionList().empty()) return result;

  // Retrieve the substitutions for the generic parameters (only).
  unsigned numAllGenericParams = genericSig->getGenericParams().size();
  auto genericArgSubs = getSubstitutionList().slice(0, numAllGenericParams);

  // Copy the replacement types for the innermost generic arguments.
  unsigned numMyGenericParams = typealias->getGenericParams()->size();
  for (const auto &sub : genericArgSubs.take_back(numMyGenericParams)) {
    result.push_back(sub.getReplacement());
  }
  return result;
}

unsigned GenericTypeParamType::getDepth() const {
  if (auto param = getDecl()) {
    return param->getDepth();
  }

  auto fixedNum = ParamOrDepthIndex.get<DepthIndexTy>();
  return fixedNum >> 16;
}

unsigned GenericTypeParamType::getIndex() const {
  if (auto param = getDecl()) {
    return param->getIndex();
  }

  auto fixedNum = ParamOrDepthIndex.get<DepthIndexTy>();
  return fixedNum & 0xFFFF;
}

Identifier GenericTypeParamType::getName() const {
  // Use the declaration name if we still have that sugar.
  if (auto decl = getDecl())
    return decl->getName();
  
  // Otherwise, we're canonical. Produce an anonymous '<tau>_n_n' name.
  assert(isCanonical());
  // getASTContext() doesn't actually mutate an already-canonical type.
  auto &C = const_cast<GenericTypeParamType*>(this)->getASTContext();
  auto &names = C.CanonicalGenericTypeParamTypeNames;
  unsigned depthIndex = ParamOrDepthIndex.get<DepthIndexTy>();
  auto cached = names.find(depthIndex);
  if (cached != names.end())
    return cached->second;
  
  llvm::SmallString<10> nameBuf;
  llvm::raw_svector_ostream os(nameBuf);

  static const char *tau = u8"\u03C4_";
  
  os << tau << getDepth() << '_' << getIndex();
  Identifier name = C.getIdentifier(os.str());
  names.insert({depthIndex, name});
  return name;
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

Type TypeBase::getSuperclass() {
  auto *nominalDecl = getAnyNominal();
  auto *classDecl = dyn_cast_or_null<ClassDecl>(nominalDecl);

  // Handle some special non-class types here.
  if (!classDecl) {
    if (auto archetype = getAs<ArchetypeType>())
      return archetype->getSuperclass();

    if (auto dynamicSelfTy = getAs<DynamicSelfType>())
      return dynamicSelfTy->getSelfType();

    if (auto compositionTy = getAs<ProtocolCompositionType>())
      return compositionTy->getExistentialLayout().superclass;

    // No other types have superclasses.
    return Type();
  }

  // We have a class, so get the superclass type.
  //
  // If the derived class is generic, the superclass type may contain
  // generic type parameters from the signature of the derived class.
  Type superclassTy = classDecl->getSuperclass();

  // If there's no superclass, or it is fully concrete, we're done.
  if (!superclassTy || !superclassTy->hasTypeParameter() ||
      hasUnboundGenericType())
    return superclassTy;

  // Gather substitutions from the self type, and apply them to the original
  // superclass type to form the substituted superclass type.
  ModuleDecl *module = classDecl->getModuleContext();
  auto subMap = getContextSubstitutionMap(module,
                                          classDecl,
                                          classDecl->getGenericEnvironment());
  return superclassTy.subst(subMap);
}

bool TypeBase::isExactSuperclassOf(Type ty) {
  // For there to be a superclass relationship, we must be a superclass, and
  // the potential subtype must be a class or superclass-bounded archetype.
  if (!getClassOrBoundGenericClass() || !ty->mayHaveSuperclass())
    return false;

  do {
    if (ty->isEqual(this))
      return true;
    if (ty->getAnyNominal() && ty->getAnyNominal()->isInvalid())
      return false;
  } while ((ty = ty->getSuperclass()));
  return false;
}

/// Returns true if type `a` has archetypes that can be bound to form `b`.
bool TypeBase::isBindableTo(Type b) {
  class IsBindableVisitor : public TypeVisitor<IsBindableVisitor, bool, CanType>
  {
    llvm::DenseMap<ArchetypeType *, CanType> Bindings;

  public:
    IsBindableVisitor() {}
  
    bool visitArchetypeType(ArchetypeType *orig, CanType subst) {
      // If we already bound this archetype, make sure the new binding candidate
      // is the same type.
      auto bound = Bindings.find(orig);
      if (bound != Bindings.end()) {
        return bound->second->isEqual(subst);
      }

      // Check that the archetype isn't constrained in a way that makes the
      // binding impossible.
      // For instance, if the archetype is class-constrained, and the binding
      // is not a class, it can never be bound.
      if (orig->requiresClass() && !subst->satisfiesClassConstraint())
        return false;

      // TODO: If the archetype has a superclass constraint, check that the
      // substitution is a subclass.
      
      // TODO: For private types or protocols, we might be able to definitively
      // deny bindings.
      
      // Otherwise, there may be an external retroactive conformance that
      // allows the binding.
      
      // Remember the binding, and succeed.
      Bindings.insert({orig, subst});
      return true;
    }
    
    bool visitType(TypeBase *orig, CanType subst) {
      if (CanType(orig) == subst)
        return true;
      
      llvm_unreachable("not a valid canonical type substitution");
    }
    
    bool visitNominalType(NominalType *nom, CanType subst) {
      if (auto substNom = dyn_cast<NominalType>(subst)) {
        if (nom->getDecl() != substNom->getDecl())
          return false;
        
        if (nom->getDecl()->isInvalid())
          return false;
        
        // Same decl should always either have or not have a parent.
        assert((bool)nom->getParent() == (bool)substNom->getParent());
        
        if (nom->getParent())
          return visit(nom->getParent()->getCanonicalType(),
                       substNom->getParent()->getCanonicalType());
        return true;
      }
      return false;
    }
    
    bool visitAnyMetatypeType(AnyMetatypeType *meta, CanType subst) {
      if (auto substMeta = dyn_cast<AnyMetatypeType>(subst)) {
        if (substMeta->getKind() != meta->getKind())
          return false;
        return visit(meta->getInstanceType()->getCanonicalType(),
                     substMeta->getInstanceType()->getCanonicalType());
      }
      return false;
    }
    
    bool visitTupleType(TupleType *tuple, CanType subst) {
      if (auto substTuple = dyn_cast<TupleType>(subst)) {
        // Tuple elements must match.
        if (tuple->getNumElements() != substTuple->getNumElements())
          return false;
        // TODO: Label reordering?
        for (unsigned i : indices(tuple->getElements())) {
          auto elt = tuple->getElements()[i],
               substElt = substTuple->getElements()[i];
          if (elt.getName() != substElt.getName())
            return false;
          if (!visit(elt.getType(), substElt.getType()->getCanonicalType()))
            return false;
        }
        return true;
      }
      return false;
    }
    
    bool visitDependentMemberType(DependentMemberType *dt, CanType subst) {
      llvm_unreachable("can't visit dependent types");
    }
    bool visitGenericTypeParamType(GenericTypeParamType *dt, CanType subst) {
      llvm_unreachable("can't visit dependent types");
    }
    
    bool visitFunctionType(FunctionType *func, CanType subst) {
      if (auto substFunc = dyn_cast<FunctionType>(subst)) {
        if (func->getExtInfo() != substFunc->getExtInfo())
          return false;
        
        if (!visit(func->getInput()->getCanonicalType(),
                   substFunc->getInput()->getCanonicalType()))
          return false;
        
        return visit(func->getResult()->getCanonicalType(),
                     substFunc->getResult()->getCanonicalType());
      }
      return false;
    }
    
    bool visitSILFunctionType(SILFunctionType *func,
                              CanType subst) {
      if (auto substFunc = dyn_cast<SILFunctionType>(subst)) {
        if (func->getExtInfo() != substFunc->getExtInfo())
          return false;
        
        // TODO: Generic signatures
        if (func->getGenericSignature() || substFunc->getGenericSignature())
          return false;
        
        if (func->getParameters().size() != substFunc->getParameters().size())
          return false;
        if (func->getResults().size() != substFunc->getResults().size())
          return false;
        
        for (unsigned i : indices(func->getParameters())) {
          if (func->getParameters()[i].getConvention()
                != substFunc->getParameters()[i].getConvention())
            return false;
          if (!visit(func->getParameters()[i].getType(),
                     substFunc->getParameters()[i].getType()))
            return false;
        }

        for (unsigned i : indices(func->getResults())) {
          if (func->getResults()[i].getConvention()
              != substFunc->getResults()[i].getConvention())
            return false;

          if (!visit(func->getResults()[i].getType(),
                     substFunc->getResults()[i].getType()))
            return false;
        }
        
        return true;
      }
      
      return false;
    }
    
    bool visitBoundGenericType(BoundGenericType *bgt, CanType subst) {
      if (auto substBGT = dyn_cast<BoundGenericType>(subst)) {
        if (bgt->getDecl() != substBGT->getDecl())
          return false;

        auto *decl = bgt->getDecl();
        if (decl->isInvalid())
          return false;

        auto *moduleDecl = decl->getParentModule();
        auto origSubMap = bgt->getContextSubstitutionMap(
            moduleDecl, decl, decl->getGenericEnvironment());
        auto substSubMap = substBGT->getContextSubstitutionMap(
            moduleDecl, decl, decl->getGenericEnvironment());

        auto *genericSig = decl->getGenericSignature();
        auto result = genericSig->enumeratePairedRequirements(
          [&](Type t, ArrayRef<Requirement> reqts) -> bool {
            auto orig = t.subst(origSubMap)->getCanonicalType();
            auto subst = t.subst(substSubMap)->getCanonicalType();
            if (!visit(orig, subst))
              return true;

            auto canTy = t->getCanonicalType();
            for (auto reqt : reqts) {
              auto *proto = reqt.getSecondType()->castTo<ProtocolType>()
                ->getDecl();
              auto origConf = *origSubMap.lookupConformance(canTy, proto);
              auto substConf = *substSubMap.lookupConformance(canTy, proto);

              if (origConf.isConcrete()) {
                if (!substConf.isConcrete())
                  return true;
                if (origConf.getConcrete()->getRootNormalConformance()
                    != substConf.getConcrete()->getRootNormalConformance())
                  return true;
              }
            }
            return false;
          });

        if (result)
          return false;

        // Same decl should always either have or not have a parent.
        assert((bool)bgt->getParent() == (bool)substBGT->getParent());
        if (bgt->getParent())
          return visit(bgt->getParent()->getCanonicalType(),
                       substBGT->getParent()->getCanonicalType());
        return true;
      }
      return false;
    }
  };
  
  return IsBindableVisitor().visit(getCanonicalType(),
                                   b->getCanonicalType());
}

bool TypeBase::isBindableToSuperclassOf(Type ty) {
  // Do an exact match if no archetypes are involved.
  if (!hasArchetype())
    return isExactSuperclassOf(ty);
  
  // For there to be a superclass relationship,
  // the potential subtype must be a class or superclass-bounded archetype.
  if (!ty->mayHaveSuperclass())
    return false;

  // If the type is itself an archetype, we could always potentially bind it
  // to the superclass (via external retroactive conformance, even if the
  // type isn't statically known to conform).
  //
  // We could theoretically reject cases where the set of conformances is known
  // (say the protocol or classes are private or internal).
  if (is<ArchetypeType>())
    return true;
  
  do {
    if (isBindableTo(ty))
      return true;
    if (ty->getAnyNominal() && ty->getAnyNominal()->isInvalid())
      return false;
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

  // Look through DynamicSelfType.
  if (auto dynSelf = type->getAs<DynamicSelfType>())
    type = dynSelf->getSelfType();

  // @objc classes.
  if (auto classDecl = type->getClassOrBoundGenericClass()) {
    auto &ctx = classDecl->getASTContext();
    if (auto resolver = ctx.getLazyResolver())
      resolver->resolveDeclSignature(classDecl);

    if (classDecl->isObjC())
      return ForeignRepresentableKind::Object;
  }

  // Objective-C existential types are trivially representable if
  // they don't have a superclass constraint, or if the superclass
  // constraint is an @objc class.
  if (type->isExistentialType()) {
    auto layout = type->getExistentialLayout();
    if (layout.isObjC() &&
        (!layout.superclass ||
         getObjCObjectRepresentable(layout.superclass, dc) ==
           ForeignRepresentableKind::Object))
      return ForeignRepresentableKind::Object;
  }

  // Any can be bridged to id.
  if (type->isAny()) {
    return ForeignRepresentableKind::Bridged;
  }

  // Class-constrained generic parameters, from ObjC generic classes.
  if (auto tyContext = dc->getInnermostTypeContext())
    if (auto clas = tyContext->getAsClassOrClassExtensionContext())
      if (clas->hasClangNode())
        if (auto archetype = type->getAs<ArchetypeType>())
          if (archetype->requiresClass())
            return ForeignRepresentableKind::Object;

  return ForeignRepresentableKind::None;
}

/// Determine the foreign representation of this type.
///
/// This function determines when and how a particular type is mapped
/// into a foreign language. Any changes to the logic here also need
/// to be reflected in PrintAsObjC, so that the Swift type will be
/// properly printed for (Objective-)C and in SIL's bridging logic.
static std::pair<ForeignRepresentableKind, ProtocolConformance *>
getForeignRepresentable(Type type, ForeignLanguage language,
                        const DeclContext *dc) {
  // Look through one level of optional type, but remember that we did.
  bool wasOptional = false;
  if (auto valueType = type->getOptionalObjectType()) {
    type = valueType;
    wasOptional = true;
  }

  // Objective-C object types, including metatypes.
  if (language == ForeignLanguage::ObjectiveC) {
    auto representable = getObjCObjectRepresentable(type, dc);
    if (representable != ForeignRepresentableKind::None)
      return { representable, nullptr };
  }

  // Local function that simply produces a failing result.
  auto failure = []() -> std::pair<ForeignRepresentableKind,
                                   ProtocolConformance *> {
    return { ForeignRepresentableKind::None, nullptr };
  };

  // Function types.
  if (auto functionType = type->getAs<FunctionType>()) {
    // Cannot handle throwing functions.
    if (functionType->getExtInfo().throws())
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

    // Look at the result type.
    Type resultType = functionType->getResult();
    if (!resultType->isVoid() && recurse(resultType))
      return failure();

    // Look at the input types.
    Type inputType = functionType->getInput();
    if (auto inputTuple = inputType->getAs<TupleType>()) {
      for (const auto &elt : inputTuple->getElements()) {
        if (elt.isVararg())
          return failure();
        if (recurse(elt.getType()))
          return failure();
      }
    } else if (recurse(inputType)) {
      return failure();
    }

    // We have something representable; check how it is representable.
    return { anyStaticBridged ? ForeignRepresentableKind::StaticBridged
                 : anyBridged ? ForeignRepresentableKind::Bridged
                 : isBlock    ? ForeignRepresentableKind::Object
                 : ForeignRepresentableKind::Trivial,
             nullptr };
  }

  // Give special dispensation to builtin types for testing purposes.
  if (type->is<BuiltinType>())
    return { ForeignRepresentableKind::Trivial, nullptr };

  auto nominal = type->getAnyNominal();
  if (!nominal) return failure();

  ASTContext &ctx = nominal->getASTContext();

  // Unmanaged<T> can be trivially represented in Objective-C if T
  // is trivially represented in Objective-C.
  if (language == ForeignLanguage::ObjectiveC &&
      nominal == ctx.getUnmanagedDecl()) {
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
      LLVM_FALLTHROUGH;

    case ForeignLanguage::ObjectiveC:
      if (isa<StructDecl>(nominal) || isa<EnumDecl>(nominal)) {
        // Optional structs are not representable in (Objective-)C if they
        // originally came from C, whether or not they are bridged, unless they
        // came from swift_newtype. If they are defined in Swift, they are only
        // representable if they are bridged (checked below).
        if (wasOptional) {
          if (nominal->hasClangNode()) {
            Type underlyingType =
                nominal->getDeclaredType()->getSwiftNewtypeUnderlyingType();
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
            if (nominal->lookupConformance(dc->getParentModule(),
                                           objcBridgeable,
                                           conformances))
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
                                std::function<bool()> paramsAndResultMatch) {
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
    if (ext2.throws()) {
      ext1 = ext1.withThrows(true);
    }
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
  if (ext1 != ext2)
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

    std::function<bool()> paramsAndResultMatch = [=]() {
      // Inputs are contravariant, results are covariant.
      return (matches(fn2.getInput(), fn1.getInput(), matchMode,
                      ParameterPosition::Parameter, OptionalUnwrapping::None) &&
              matches(fn1.getResult(), fn2.getResult(), matchMode,
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
                                   std::function<bool()> paramsAndResultMatch) {
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

/// getElementForScalarInit - If a tuple of this type can be initialized with a
/// scalar, return the field number that the scalar is assigned to.  If not,
/// return -1.
int TupleType::getElementForScalarInit() const {
  if (Bits.TupleType.Count == 0) return -1;
  
  int FieldWithoutDefault = -1;
  for (unsigned i = 0, e = Bits.TupleType.Count; i != e; ++i) {
    // If we already saw a non-vararg field missing a default value, then we
    // cannot assign a scalar to this tuple.
    if (FieldWithoutDefault != -1) {
      // Vararg fields are okay; they'll just end up being empty.
      if (getTrailingObjects<TupleTypeElt>()[i].isVararg())
        continue;
    
      return -1;
    }
    
    // Otherwise, remember this field number.
    FieldWithoutDefault = i;    
  }
  
  // If all the elements have default values, the scalar initializes the first
  // value in the tuple.
  return FieldWithoutDefault == -1 ? 0 : FieldWithoutDefault;
}

/// If this tuple has a varargs element to it, return the base type of the
/// varargs element (i.e., if it is "Int...", this returns Int, not [Int]).
/// Otherwise, this returns Type().
Type TupleType::getVarArgsBaseType() const {
  for (unsigned i = 0, e = Bits.TupleType.Count; i != e; ++i) {
    if (getTrailingObjects<TupleTypeElt>()[i].isVararg())
      return getTrailingObjects<TupleTypeElt>()[i].getVarargBaseTy();
  }
  
  return Type();
}


ArchetypeType::ArchetypeType(
  const ASTContext &Ctx,
  llvm::PointerUnion<ArchetypeType *, GenericEnvironment *> ParentOrGenericEnv,
  Type InterfaceType,
  ArrayRef<ProtocolDecl *> ConformsTo,
  Type Superclass, LayoutConstraint Layout)
    : SubstitutableType(TypeKind::Archetype, &Ctx,
                        RecursiveTypeProperties::HasArchetype),
      InterfaceType(InterfaceType) {
  // Record the parent/generic environment.
  if (auto parent = ParentOrGenericEnv.dyn_cast<ArchetypeType *>()) {
    ParentOrOpenedOrEnvironment = parent;
  } else {
    ParentOrOpenedOrEnvironment =
      ParentOrGenericEnv.get<GenericEnvironment *>();
  }

  // Set up the bits we need for trailing objects to work.
  Bits.ArchetypeType.ExpandedNestedTypes = false;
  Bits.ArchetypeType.HasSuperclass = static_cast<bool>(Superclass);
  Bits.ArchetypeType.HasLayoutConstraint = static_cast<bool>(Layout);
  Bits.ArchetypeType.NumProtocols = ConformsTo.size();

  // Record the superclass.
  if (Superclass)
    *getTrailingObjects<Type>() = Superclass;

  // Record the layout constraint.
  if (Layout)
    *getTrailingObjects<LayoutConstraint>() = Layout;

  // Copy the protocols.
  std::uninitialized_copy(ConformsTo.begin(), ConformsTo.end(),
                          getTrailingObjects<ProtocolDecl *>());
}

ArchetypeType::ArchetypeType(const ASTContext &Ctx, Type Existential,
                             ArrayRef<ProtocolDecl *> ConformsTo,
                             Type Superclass, LayoutConstraint Layout,
                             UUID uuid)
  : SubstitutableType(TypeKind::Archetype, &Ctx,
                      RecursiveTypeProperties(
                        RecursiveTypeProperties::HasArchetype |
                        RecursiveTypeProperties::HasOpenedExistential)),
    ParentOrOpenedOrEnvironment(Existential.getPointer()) {
  // Set up the bits we need for trailing objects to work.
  Bits.ArchetypeType.ExpandedNestedTypes = false;
  Bits.ArchetypeType.HasSuperclass = static_cast<bool>(Superclass);
  Bits.ArchetypeType.HasLayoutConstraint = static_cast<bool>(Layout);
  Bits.ArchetypeType.NumProtocols = ConformsTo.size();

  // Record the superclass.
  if (Superclass)
    *getTrailingObjects<Type>() = Superclass;

  // Record the layout constraint.
  if (Layout)
    *getTrailingObjects<LayoutConstraint>() = Layout;

  // Copy the protocols.
  std::uninitialized_copy(ConformsTo.begin(), ConformsTo.end(),
                          getTrailingObjects<ProtocolDecl *>());

  // Record the UUID.
  *getTrailingObjects<UUID>() = uuid;
}

CanArchetypeType ArchetypeType::getNew(
                                   const ASTContext &Ctx,
                                   ArchetypeType *Parent,
                                   DependentMemberType *InterfaceType,
                                   SmallVectorImpl<ProtocolDecl *> &ConformsTo,
                                   Type Superclass,
                                   LayoutConstraint Layout) {
  assert(!Superclass || Superclass->getClassOrBoundGenericClass());

  // Gather the set of protocol declarations to which this archetype conforms.
  ProtocolType::canonicalizeProtocols(ConformsTo);

  auto arena = AllocationArena::Permanent;
  void *mem = Ctx.Allocate(
      totalSizeToAlloc<ProtocolDecl *, Type, LayoutConstraint, UUID>(
          ConformsTo.size(), Superclass ? 1 : 0, Layout ? 1 : 0, 0),
      alignof(ArchetypeType), arena);

  return CanArchetypeType(new (mem) ArchetypeType(
      Ctx, Parent, InterfaceType, ConformsTo, Superclass, Layout));
}

CanArchetypeType
ArchetypeType::getNew(const ASTContext &Ctx,
                      GenericEnvironment *GenericEnv,
                      GenericTypeParamType *InterfaceType,
                      SmallVectorImpl<ProtocolDecl *> &ConformsTo,
                      Type Superclass,
                      LayoutConstraint Layout) {
  assert(!Superclass || Superclass->getClassOrBoundGenericClass());
  assert(GenericEnv && "missing generic environment for archetype");

  // Gather the set of protocol declarations to which this archetype conforms.
  ProtocolType::canonicalizeProtocols(ConformsTo);

  auto arena = AllocationArena::Permanent;
  void *mem = Ctx.Allocate(
      totalSizeToAlloc<ProtocolDecl *, Type, LayoutConstraint, UUID>(
          ConformsTo.size(), Superclass ? 1 : 0, Layout ? 1 : 0, 0),
      alignof(ArchetypeType), arena);

  return CanArchetypeType(new (mem) ArchetypeType(
      Ctx, GenericEnv, InterfaceType, ConformsTo, Superclass, Layout));
}

bool ArchetypeType::requiresClass() const {
  if (Bits.ArchetypeType.HasSuperclass)
    return true;
  if (auto layout = getLayoutConstraint())
    if (layout->isClass())
      return true;
  for (ProtocolDecl *conformed : getConformsTo())
    if (conformed->requiresClass())
      return true;
  return false;
}

namespace {
  /// \brief Function object that orders archetypes by name.
  struct OrderArchetypeByName {
    bool operator()(std::pair<Identifier, Type> X,
                    std::pair<Identifier, Type> Y) const {
      return X.first.str() < Y.first.str();
    }

    bool operator()(std::pair<Identifier, Type> X,
                    Identifier Y) const {
      return X.first.str() < Y.str();
    }

    bool operator()(Identifier X,
                    std::pair<Identifier, Type> Y) const {
      return X.str() < Y.first.str();
    }

    bool operator()(Identifier X, Identifier Y) const {
      return X.str() < Y.str();
    }
  };
} // end anonymous namespace

void ArchetypeType::populateNestedTypes() const {
  if (Bits.ArchetypeType.ExpandedNestedTypes) return;

  // Collect the set of nested types of this archetype.
  SmallVector<std::pair<Identifier, Type>, 4> nestedTypes;
  llvm::SmallPtrSet<Identifier, 4> knownNestedTypes;
  ProtocolType::visitAllProtocols(getConformsTo(),
                                  [&](ProtocolDecl *proto) -> bool {
    for (auto member : proto->getMembers()) {
      if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
        if (knownNestedTypes.insert(assocType->getName()).second)
          nestedTypes.push_back({ assocType->getName(), Type() });
      }
    }

    return false;
  });

  // Record the nested types.
  auto mutableThis = const_cast<ArchetypeType *>(this);
  mutableThis->setNestedTypes(mutableThis->getASTContext(), nestedTypes);
}

Type ArchetypeType::getNestedType(Identifier Name) const {
  populateNestedTypes();

  auto Pos = std::lower_bound(NestedTypes.begin(), NestedTypes.end(), Name,
                              OrderArchetypeByName());
  if (Pos == NestedTypes.end() || Pos->first != Name) {
    return ErrorType::get(const_cast<ArchetypeType *>(this)->getASTContext());
  }

  // If the type is null, lazily resolve it. 
  if (!Pos->second) {
    resolveNestedType(*Pos);
  }

  return Pos->second;
}

Optional<Type> ArchetypeType::getNestedTypeIfKnown(Identifier Name) const {
  populateNestedTypes();

  auto Pos = std::lower_bound(NestedTypes.begin(), NestedTypes.end(), Name,
                              OrderArchetypeByName());
  if (Pos == NestedTypes.end() || Pos->first != Name || !Pos->second)
    return None;

  return Pos->second;
}

bool ArchetypeType::hasNestedType(Identifier Name) const {
  populateNestedTypes();

  auto Pos = std::lower_bound(NestedTypes.begin(), NestedTypes.end(), Name,
                              OrderArchetypeByName());
  return Pos != NestedTypes.end() && Pos->first == Name;
}

ArrayRef<std::pair<Identifier, Type>>
ArchetypeType::getAllNestedTypes(bool resolveTypes) const {
  populateNestedTypes();

  if (resolveTypes) {
    for (auto &nested : NestedTypes) {
      if (!nested.second)
        resolveNestedType(nested);
    }
  }

  return NestedTypes;
}

void ArchetypeType::setNestedTypes(
                                 ASTContext &Ctx,
                                 ArrayRef<std::pair<Identifier, Type>> Nested) {
  assert(!Bits.ArchetypeType.ExpandedNestedTypes && "Already expanded");
  NestedTypes = Ctx.AllocateCopy(Nested);
  std::sort(NestedTypes.begin(), NestedTypes.end(), OrderArchetypeByName());
  Bits.ArchetypeType.ExpandedNestedTypes = true;
}

void ArchetypeType::registerNestedType(Identifier name, Type nested) {
  populateNestedTypes();

  auto found = std::lower_bound(NestedTypes.begin(), NestedTypes.end(), name,
                                OrderArchetypeByName());
  assert(found != NestedTypes.end() && found->first == name &&
         "Unable to find nested type?");
  assert(!found->second ||
         found->second->isEqual(nested) ||
         (found->second->hasError() && nested->hasError()));
  found->second = nested;
}

static void collectFullName(const ArchetypeType *Archetype,
                            SmallVectorImpl<char> &Result) {
  if (auto Parent = Archetype->getParent()) {
    collectFullName(Parent, Result);
    Result.push_back('.');
  }
  Result.append(Archetype->getName().str().begin(),
                Archetype->getName().str().end());
}

AssociatedTypeDecl *ArchetypeType::getAssocType() const {
  assert(!getOpenedExistentialType());
  if (auto *depMemTy = InterfaceType->getAs<DependentMemberType>())
    return depMemTy->getAssocType();
  return nullptr;
}

Identifier ArchetypeType::getName() const {
  assert(!getOpenedExistentialType());
  if (auto assocType = getAssocType())
    return assocType->getName();

  return InterfaceType->castTo<GenericTypeParamType>()->getName();
}

std::string ArchetypeType::getFullName() const {
  llvm::SmallString<64> Result;
  collectFullName(this, Result);
  return Result.str().str();
}

GenericEnvironment *ArchetypeType::getGenericEnvironment() const {
  if (auto parent = getParent())
    return parent->getGenericEnvironment();

  return ParentOrOpenedOrEnvironment.dyn_cast<GenericEnvironment *>();
}

void ProtocolCompositionType::Profile(llvm::FoldingSetNodeID &ID,
                                      ArrayRef<Type> Members,
                                      bool HasExplicitAnyObject) {
  ID.AddInteger(HasExplicitAnyObject);
  for (auto T : Members)
    ID.AddPointer(T.getPointer());
}

bool ProtocolType::requiresClass() {
  return getDecl()->requiresClass();
}

bool ProtocolCompositionType::requiresClass() {
  return getExistentialLayout().requiresClass();
}

Type ProtocolCompositionType::get(const ASTContext &C,
                                  ArrayRef<Type> Members,
                                  bool HasExplicitAnyObject) {
  for (Type t : Members) {
    if (!t->isCanonical())
      return build(C, Members, HasExplicitAnyObject);
  }
    
  Type Superclass;
  SmallVector<ProtocolDecl *, 4> Protocols;
  for (Type t : Members) {
    addProtocols(t, Protocols, Superclass, HasExplicitAnyObject);
  }
  
  // Minimize the set of protocols composed together.
  ProtocolType::canonicalizeProtocols(Protocols);

  // The presence of a superclass constraint makes AnyObject redundant.
  if (Superclass)
    HasExplicitAnyObject = false;

  // If one protocol remains with no further constraints, its nominal
  // type is the canonical type.
  if (Protocols.size() == 1 && !Superclass && !HasExplicitAnyObject)
    return Protocols.front()->getDeclaredType();

  // Form the set of canonical protocol types from the protocol
  // declarations, and use that to build the canonical composition type.
  SmallVector<Type, 4> CanTypes;
  if (Superclass)
    CanTypes.push_back(Superclass->getCanonicalType());
  std::transform(Protocols.begin(), Protocols.end(),
                 std::back_inserter(CanTypes),
                 [](ProtocolDecl *Proto) {
                   return Proto->getDeclaredType();
                 });

  // TODO: Canonicalize away HasExplicitAnyObject if it is implied
  // by one of our member protocols.
  return build(C, CanTypes, HasExplicitAnyObject);
}

bool AnyFunctionType::isCanonicalFunctionInputType(Type input) {
  // Canonically, we should have a tuple type or parenthesized type.
  if (auto tupleTy = dyn_cast<TupleType>(input.getPointer()))
    return tupleTy->isCanonical();
  if (auto parenTy = dyn_cast<ParenType>(input.getPointer()))
    return parenTy->getUnderlyingType()->isCanonical();

  // FIXME: Still required for the constraint solver.
  return isa<TypeVariableType>(input.getPointer());
}

FunctionType *
GenericFunctionType::substGenericArgs(SubstitutionList args) {
  return substGenericArgs(getGenericSignature()->getSubstitutionMap(args));
}

FunctionType *
GenericFunctionType::substGenericArgs(const SubstitutionMap &subs) {
  Type input = getInput().subst(subs);
  Type result = getResult().subst(subs);
  return FunctionType::get(input, result, getExtInfo());
}

FunctionType *
GenericFunctionType::substGenericArgs(TypeSubstitutionFn subs,
                                      LookupConformanceFn conformances) {
  Type input = getInput().subst(subs, conformances);
  Type result = getResult().subst(subs, conformances);
  return FunctionType::get(input, result, getExtInfo());
}

static Type getMemberForBaseType(LookupConformanceFn lookupConformances,
                                 Type origBase,
                                 Type substBase,
                                 AssociatedTypeDecl *assocType,
                                 Identifier name,
                                 SubstOptions options) {
  // Produce a dependent member type for the given base type.
  auto getDependentMemberType = [&](Type baseType) {
    if (assocType)
      return DependentMemberType::get(baseType, assocType);

    return DependentMemberType::get(baseType, name);
  };

  // Produce a failed result.
  auto failed = [&]() -> Type {
    if (!options.contains(SubstFlags::UseErrorType)) return Type();

    Type baseType = ErrorType::get(substBase ? substBase : origBase);
    if (assocType)
      return DependentMemberType::get(baseType, assocType);

    return DependentMemberType::get(baseType, name);
  };

  // If we don't have a substituted base type, fail.
  if (!substBase) return failed();

  // Error recovery path.
  // FIXME: Generalized existentials will look here.
  if (substBase->isOpenedExistential())
    return failed();

  // If the parent is an archetype, extract the child archetype with the
  // given name.
  if (auto archetypeParent = substBase->getAs<ArchetypeType>()) {
    if (archetypeParent->hasNestedType(name))
      return archetypeParent->getNestedType(name);

    // If looking for an associated type and the archetype is constrained to a
    // class, continue to the default associated type lookup
    if (!assocType || !archetypeParent->getSuperclass())
      return failed();
  }

  // If the parent is a type variable or a member rooted in a type variable,
  // we're done.
  if (substBase->isTypeVariableOrMember())
    return getDependentMemberType(substBase);

  // Retrieve the member type with the given name.

  // Tuples don't have member types.
  if (substBase->is<TupleType>())
    return failed();

  // If the parent is dependent, create a dependent member type.
  if (substBase->isTypeParameter())
    return getDependentMemberType(substBase);

  // If we know the associated type, look in the witness table.
  LazyResolver *resolver = substBase->getASTContext().getLazyResolver();
  if (assocType) {
    auto proto = assocType->getProtocol();
    Optional<ProtocolConformanceRef> conformance
      = lookupConformances(origBase->getCanonicalType(),
                           substBase,
                           proto->getDeclaredType());

    if (!conformance) return failed();
    if (!conformance->isConcrete()) return failed();

    // Retrieve the type witness.
    auto witness =
      conformance->getConcrete()->getTypeWitness(assocType, resolver, options);
    if (!witness)
      return failed();

    // This is a hacky feature allowing code completion to migrate to
    // using Type::subst() without changing output.
    if (options & SubstFlags::DesugarMemberTypes) {
      if (auto *aliasType =
                   dyn_cast<NameAliasType>(witness.getPointer())) {
        if (!aliasType->is<ErrorType>())
          witness = aliasType->getSinglyDesugaredType();
      }
    }

    if (witness->is<ErrorType>())
      return failed();

    return witness;
  }

  return failed();
}

Optional<ProtocolConformanceRef>
LookUpConformanceInModule::operator()(CanType dependentType,
                                      Type conformingReplacementType,
                                      ProtocolType *conformedProtocol) const {
  if (conformingReplacementType->isTypeParameter())
    return ProtocolConformanceRef(conformedProtocol->getDecl());

  return M->lookupConformance(conformingReplacementType,
                              conformedProtocol->getDecl());
}

Optional<ProtocolConformanceRef>
LookUpConformanceInSubstitutionMap::operator()(CanType dependentType,
                                       Type conformingReplacementType,
                                       ProtocolType *conformedProtocol) const {
  return Subs.lookupConformance(dependentType, conformedProtocol->getDecl());
}

Optional<ProtocolConformanceRef>
MakeAbstractConformanceForGenericType::operator()(CanType dependentType,
                                       Type conformingReplacementType,
                                       ProtocolType *conformedProtocol) const {
  assert((conformingReplacementType->is<SubstitutableType>()
          || conformingReplacementType->is<DependentMemberType>())
         && "replacement requires looking up a concrete conformance");
  return ProtocolConformanceRef(conformedProtocol->getDecl());
}

Optional<ProtocolConformanceRef>
LookUpConformanceInSignature::operator()(CanType dependentType,
                                         Type conformingReplacementType,
                                         ProtocolType *conformedProtocol) const {
  // FIXME: Should pass dependentType instead, once
  // GenericSignature::lookupConformance() does the right thing
  return Sig.lookupConformance(conformingReplacementType->getCanonicalType(),
                               conformedProtocol->getDecl());
}

Type DependentMemberType::substBaseType(ModuleDecl *module,
                                        Type substBase,
                                        LazyResolver *resolver) {
  return substBaseType(substBase, LookUpConformanceInModule(module));
}

Type DependentMemberType::substBaseType(Type substBase,
                                        LookupConformanceFn lookupConformance) {
  if (substBase.getPointer() == getBase().getPointer() &&
      substBase->hasTypeParameter())
    return this;

  return getMemberForBaseType(lookupConformance, getBase(), substBase,
                              getAssocType(), getName(), None);
}

static Type substGenericFunctionType(GenericFunctionType *genericFnType,
                                     TypeSubstitutionFn substitutions,
                                     LookupConformanceFn lookupConformances,
                                     SubstOptions options) {
  // Substitute into the function type (without generic signature).
  auto *bareFnType = FunctionType::get(genericFnType->getInput(),
                                       genericFnType->getResult(),
                                       genericFnType->getExtInfo());
  Type result =
    Type(bareFnType).subst(substitutions, lookupConformances, options);
  if (!result || result->is<ErrorType>()) return result;

  auto *fnType = result->castTo<FunctionType>();
  // Substitute generic parameters.
  bool anySemanticChanges = false;
  SmallVector<GenericTypeParamType *, 4> genericParams;
  for (auto param : genericFnType->getGenericParams()) {
    Type paramTy =
      Type(param).subst(substitutions, lookupConformances, options);
    if (!paramTy)
      return Type();

    if (auto newParam = paramTy->getAs<GenericTypeParamType>()) {
      if (!newParam->isEqual(param))
        anySemanticChanges = true;

      genericParams.push_back(newParam);
    } else {
      anySemanticChanges = true;
    }
  }

  // If no generic parameters remain, this is a non-generic function type.
  if (genericParams.empty())
    return result;

  // Transform requirements.
  SmallVector<Requirement, 4> requirements;
  for (const auto &req : genericFnType->getRequirements()) {
    // Substitute into the requirement.
    auto substReqt = req.subst(substitutions, lookupConformances, options);
    if (!substReqt) {
      anySemanticChanges = true;
      continue;
    }

    // Did anything change?
    if (!anySemanticChanges &&
        (!req.getFirstType()->isEqual(substReqt->getFirstType()) ||
         (req.getKind() != RequirementKind::Layout &&
          !req.getSecondType()->isEqual(substReqt->getSecondType())))) {
      anySemanticChanges = true;
    }

    // Skip any erroneous requirements.
    if (substReqt->getFirstType()->hasError() ||
        (substReqt->getKind() != RequirementKind::Layout &&
         substReqt->getSecondType()->hasError()))
      continue;

    requirements.push_back(*substReqt);
  }

  GenericSignature *genericSig = nullptr;
  if (anySemanticChanges) {
    // If there were semantic changes, we need to build a new generic
    // signature.
    GenericSignatureBuilder builder(genericFnType->getASTContext());

    // Add the generic parameters to the builder.
    for (auto gp : genericParams)
      builder.addGenericParameter(gp);

    // Add the requirements to the builder.
    auto source =
      GenericSignatureBuilder::FloatingRequirementSource::forAbstract();
    for (const auto &req : requirements)
      builder.addRequirement(req, source, /*inferForModule=*/nullptr);

    // Form the generic signature.
    genericSig = std::move(builder).computeGenericSignature(SourceLoc());
  } else {
    // Use the mapped generic signature.
    genericSig = GenericSignature::get(genericParams, requirements);
  }

  // Produce the new generic function type.
  return GenericFunctionType::get(genericSig, fnType->getInput(),
                                  fnType->getResult(), fnType->getExtInfo());
}

static Type substType(Type derivedType,
                      TypeSubstitutionFn substitutions,
                      LookupConformanceFn lookupConformances,
                      SubstOptions options) {
  // Handle substitutions into generic function types.
  if (auto genericFnType = derivedType->getAs<GenericFunctionType>()) {
    return substGenericFunctionType(genericFnType, substitutions,
                                    lookupConformances, options);
  }

  // FIXME: Change getTypeOfMember() to not pass GenericFunctionType here
  if (!derivedType->hasArchetype() &&
      !derivedType->hasTypeParameter())
    return derivedType;

  return derivedType.transformRec([&](TypeBase *type) -> Optional<Type> {
    // FIXME: Add SIL versions of mapTypeIntoContext() and
    // mapTypeOutOfContext() and use them appropriately
    assert((options.contains(SubstFlags::AllowLoweredTypes) ||
            !isa<SILFunctionType>(type)) &&
           "should not be doing AST type-substitution on a lowered SIL type;"
           "use SILType::subst");

    // Special-case handle SILBoxTypes; we want to structurally substitute the
    // substitutions.
    if (auto boxTy = dyn_cast<SILBoxType>(type)) {
      if (boxTy->getGenericArgs().empty())
        return Type(boxTy);
      
      auto subMap = boxTy->getLayout()->getGenericSignature()
        ->getSubstitutionMap(boxTy->getGenericArgs());
      subMap = subMap.subst(substitutions, lookupConformances);

      SmallVector<Substitution, 4> newSubs;
      boxTy->getLayout()->getGenericSignature()
        ->getSubstitutions(subMap, newSubs);
      for (auto &arg : newSubs) {
        arg = Substitution(arg.getReplacement()->getCanonicalType(),
                           arg.getConformances());
      }
      return SILBoxType::get(boxTy->getASTContext(),
                             boxTy->getLayout(),
                             newSubs);
    }
    
    // We only substitute for substitutable types and dependent member types.
    
    // For dependent member types, we may need to look up the member if the
    // base is resolved to a non-dependent type.
    if (auto depMemTy = dyn_cast<DependentMemberType>(type)) {
      auto newBase = substType(depMemTy->getBase(),
                               substitutions, lookupConformances, options);
      if (!newBase)
        return Type();
      
      return getMemberForBaseType(lookupConformances,
                                  depMemTy->getBase(), newBase,
                                  depMemTy->getAssocType(),
                                  depMemTy->getName(), options);
    }
    
    auto substOrig = dyn_cast<SubstitutableType>(type);
    if (!substOrig)
      return None;

    // If we have a substitution for this type, use it.
    if (auto known = substitutions(substOrig))
      return known;

    // If we failed to substitute a generic type parameter, give up.
    if (isa<GenericTypeParamType>(substOrig)) {
      if (options.contains(SubstFlags::UseErrorType))
        return ErrorType::get(type);
      return Type();
    }

    auto archetype = cast<ArchetypeType>(substOrig);

    // Opened existentials cannot be substituted in this manner,
    // but if they appear in the original type this is not an
    // error.
    if (archetype->isOpenedExistential())
      return Type(type);

    // For archetypes, we can substitute the parent (if present).
    auto parent = archetype->getParent();
    if (!parent) {
      if (options.contains(SubstFlags::UseErrorType))
        return ErrorType::get(type);
      return Type();
    }

    // Substitute into the parent type.
    Type substParent = substType(parent, substitutions,
                                 lookupConformances, options);

    // If the parent didn't change, we won't change.
    if (substParent.getPointer() == parent)
      return Type(type);

    // Get the associated type reference from a child archetype.
    AssociatedTypeDecl *assocType = archetype->getAssocType();

    return getMemberForBaseType(lookupConformances, parent, substParent,
                                assocType, archetype->getName(),
                                options);
  });
}

Type Type::subst(const SubstitutionMap &substitutions,
                 SubstOptions options) const {
  return substType(*this,
                   QuerySubstitutionMap{substitutions},
                   LookUpConformanceInSubstitutionMap(substitutions),
                   options);
}

Type Type::subst(TypeSubstitutionFn substitutions,
                 LookupConformanceFn conformances,
                 SubstOptions options) const {
  return substType(*this, substitutions, conformances, options);
}

Type Type::substDependentTypesWithErrorTypes() const {
  return substType(*this,
                   [](SubstitutableType *t) -> Type { return Type(); },
                   MakeAbstractConformanceForGenericType(),
                   (SubstFlags::AllowLoweredTypes |
                    SubstFlags::UseErrorType));
}

const DependentMemberType *TypeBase::findUnresolvedDependentMemberType() {
  if (!hasTypeParameter()) return nullptr;

  const DependentMemberType *unresolvedDepMemTy = nullptr;
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

static Type getConcreteTypeForSuperclassTraversing(Type t) {
  if (!t->getAnyNominal()) {
    if (auto archetype = t->getAs<ArchetypeType>()) {
      return archetype->getSuperclass();
    } else if (auto dynamicSelfTy = t->getAs<DynamicSelfType>()) {
      return dynamicSelfTy->getSelfType();
    } else if (auto compositionTy = t->getAs<ProtocolCompositionType>()) {
      return compositionTy->getExistentialLayout().superclass;
    }
  }
  return t;
}

Type TypeBase::getSuperclassForDecl(const ClassDecl *baseClass) {
  Type t = getConcreteTypeForSuperclassTraversing(this);

  while (t) {
    // If we have a class-constrained archetype or class-constrained
    // existential, get the underlying superclass constraint.
    auto *nominalDecl = t->getAnyNominal();
    assert(nominalDecl && "expected nominal type here");
    assert(isa<ClassDecl>(nominalDecl) && "expected a class here");

    if (nominalDecl == baseClass)
      return t;

    t = t->getSuperclass();
  }
  llvm_unreachable("no inheritance relationship between given classes");
}

Type TypeBase::getGenericAncestor() {
  Type t = getConcreteTypeForSuperclassTraversing(this);

  while (t && !t->hasError()) {
    auto NTD = t->getAnyNominal();
    assert(NTD && "expected nominal type in NTD");
    if (!NTD)
      return Type();

    if (NTD->isGenericContext())
      return t;

    t = t->getSuperclass();
  }

  return Type();
}

TypeSubstitutionMap
TypeBase::getContextSubstitutions(const DeclContext *dc,
                                  GenericEnvironment *genericEnv) {
  assert(dc->isTypeContext());
  Type baseTy(this);

  assert(!baseTy->hasLValueType() &&
         !baseTy->is<AnyMetatypeType>() &&
         !baseTy->is<ErrorType>());

  // The resulting set of substitutions. Always use this to ensure we
  // don't miss out on NRVO anywhere.
  TypeSubstitutionMap substitutions;

  // If the member is part of a protocol or extension thereof, we need
  // to substitute in the type of Self.
  if (dc->getAsProtocolOrProtocolExtensionContext()) {
    // FIXME: This feels painfully inefficient. We're creating a dense map
    // for a single substitution.
    substitutions[dc->getSelfInterfaceType()
                    ->getCanonicalType()->castTo<GenericTypeParamType>()]
      = baseTy;
    return substitutions;
  }

  // Find the superclass type with the context matching that of the member.
  auto *ownerNominal = dc->getAsNominalTypeOrNominalTypeExtensionContext();
  if (auto *ownerClass = dyn_cast<ClassDecl>(ownerNominal))
    baseTy = baseTy->getSuperclassForDecl(ownerClass);

  assert(ownerNominal == baseTy->getAnyNominal());

  // Gather all of the substitutions for all levels of generic arguments.
  GenericParamList *curGenericParams = dc->getGenericParamsOfContext();
  if (!curGenericParams)
    return substitutions;

  while (baseTy && curGenericParams) {
    // For a bound generic type, gather the generic parameter -> generic
    // argument substitutions.
    if (auto boundGeneric = baseTy->getAs<BoundGenericType>()) {
      auto params = curGenericParams->getParams();
      auto args = boundGeneric->getGenericArgs();
      for (unsigned i = 0, n = args.size(); i != n; ++i) {
        substitutions[params[i]->getDeclaredInterfaceType()->getCanonicalType()
                        ->castTo<GenericTypeParamType>()] = args[i];
      }

      // Continue looking into the parent.
      baseTy = boundGeneric->getParent();
      curGenericParams = curGenericParams->getOuterParameters();
      continue;
    }

    // Continue looking into the parent.
    if (auto protocolTy = baseTy->getAs<ProtocolType>()) {
      baseTy = protocolTy->getParent();
      curGenericParams = curGenericParams->getOuterParameters();
      continue;
    }

    // Continue looking into the parent.
    if (auto nominalTy = baseTy->getAs<NominalType>()) {
      baseTy = nominalTy->getParent();
      continue;
    }

    llvm_unreachable("Bad base type");
  }

  if (genericEnv) {
    auto *parentDC = dc;
    while (parentDC->isTypeContext())
      parentDC = parentDC->getParent();
    if (auto *outerSig = parentDC->getGenericSignatureOfContext()) {
      for (auto gp : outerSig->getGenericParams()) {
        auto result = substitutions.insert(
          {gp->getCanonicalType()->castTo<GenericTypeParamType>(),
           genericEnv->mapTypeIntoContext(gp)});
        assert(result.second);
        (void) result;
      }
    }
  }

  return substitutions;
}

SubstitutionMap TypeBase::getContextSubstitutionMap(
    ModuleDecl *module, const DeclContext *dc,
    GenericEnvironment *genericEnv) {
  auto *genericSig = dc->getGenericSignatureOfContext();
  if (genericSig == nullptr)
    return SubstitutionMap();
  return genericSig->getSubstitutionMap(
      QueryTypeSubstitutionMap{getContextSubstitutions(dc, genericEnv)},
      LookUpConformanceInModule(module));
}

TypeSubstitutionMap TypeBase::getMemberSubstitutions(
    const ValueDecl *member,
    GenericEnvironment *genericEnv) {
  auto *memberDC = member->getDeclContext();

  TypeSubstitutionMap substitutions;

  // Compute the set of member substitutions to apply.
  if (memberDC->isTypeContext())
    substitutions = getContextSubstitutions(memberDC, genericEnv);

  // If the member itself is generic, preserve its generic parameters.
  // We need this since code completion and diagnostics want to be able
  // to call getTypeOfMember() with functions and nested types.
  if (isa<AbstractFunctionDecl>(member) ||
      isa<GenericTypeDecl>(member) ||
      isa<SubscriptDecl>(member)) {
    auto *innerDC = member->getInnermostDeclContext();
    if (innerDC->isInnermostContextGeneric()) {
      auto *sig = innerDC->getGenericSignatureOfContext();
      for (auto param : sig->getInnermostGenericParams()) {
        auto *genericParam = param->getCanonicalType()
            ->castTo<GenericTypeParamType>();
        substitutions[genericParam] =
          (genericEnv
           ? genericEnv->mapTypeIntoContext(param)
           : param);
      }
    }
  }

  return substitutions;
}

SubstitutionMap TypeBase::getMemberSubstitutionMap(
    ModuleDecl *module, const ValueDecl *member,
    GenericEnvironment *genericEnv) {
  auto *genericSig = member->getInnermostDeclContext()
      ->getGenericSignatureOfContext();
  if (genericSig == nullptr)
    return SubstitutionMap();
  auto subs = getMemberSubstitutions(member, genericEnv);
  return genericSig->getSubstitutionMap(
      QueryTypeSubstitutionMap{subs},
      LookUpConformanceInModule(module));
}

Type TypeBase::getTypeOfMember(ModuleDecl *module, const ValueDecl *member,
                               Type memberType) {
  // If no member type was provided, use the member's type.
  if (!memberType)
    memberType = member->getInterfaceType();

  assert(memberType);

  // Perform the substitution.
  auto substitutions = getMemberSubstitutionMap(module, member);
  return memberType.subst(substitutions, SubstFlags::UseErrorType);
}

Type TypeBase::adjustSuperclassMemberDeclType(const ValueDecl *baseDecl,
                                              const ValueDecl *derivedDecl,
                                              Type memberType) {
  auto subs = SubstitutionMap::getOverrideSubstitutions(
      baseDecl, derivedDecl, /*derivedSubs=*/None);

  if (auto *genericMemberType = memberType->getAs<GenericFunctionType>()) {
    memberType = FunctionType::get(genericMemberType->getInput(),
                                   genericMemberType->getResult(),
                                   genericMemberType->getExtInfo());
  }

  auto type = memberType.subst(subs, SubstFlags::UseErrorType);

  if (isa<AbstractFunctionDecl>(baseDecl)) {
    type = type->replaceSelfParameterType(this);
    if (auto func = dyn_cast<FuncDecl>(baseDecl)) {
      if (func->hasDynamicSelf()) {
        type = type->replaceCovariantResultType(this,
                                                func->getNumParameterLists());
      }
    } else if (isa<ConstructorDecl>(baseDecl)) {
      type = type->replaceCovariantResultType(this, /*uncurryLevel=*/2);
    }
  }

  return type;
}

Identifier DependentMemberType::getName() const {
  if (NameOrAssocType.is<Identifier>())
    return NameOrAssocType.get<Identifier>();

  return NameOrAssocType.get<AssociatedTypeDecl *>()->getName();
}

static bool transformSILResult(
                           SILResultInfo &result, bool &changed,
                           llvm::function_ref<Optional<Type>(TypeBase *)> fn) {
  Type transType = result.getType().transformRec(fn);
  if (!transType) return true;

  CanType canTransType = transType->getCanonicalType();
  if (canTransType != result.getType()) {
    changed = true;
    result = result.getWithType(canTransType);
  }
  return false;
}

static bool transformSILYield(
                            SILYieldInfo &yield, bool &changed,
                            llvm::function_ref<Optional<Type>(TypeBase *)> fn) {
  Type transType = yield.getType().transformRec(fn);
  if (!transType) return true;

  CanType canTransType = transType->getCanonicalType();
  if (canTransType != yield.getType()) {
    changed = true;
    yield = yield.getWithType(canTransType);
  }
  return false;
}

static bool transformSILParameter(
                            SILParameterInfo &param, bool &changed,
                            llvm::function_ref<Optional<Type>(TypeBase *)> fn) {
  Type transType = param.getType().transformRec(fn);
  if (!transType) return true;

  CanType canTransType = transType->getCanonicalType();
  if (canTransType != param.getType()) {
    changed = true;
    param = param.getWithType(canTransType);
  }
  return false;
}

Type Type::transform(llvm::function_ref<Type(Type)> fn) const {
  return transformRec([&fn](TypeBase *type) -> Optional<Type> {
    Type transformed = fn(Type(type));
    if (!transformed)
      return Type();

    // If the function didn't change the type at all, let transformRec()
    // recurse.
    if (transformed.getPointer() == type)
      return None;

    return transformed;
  });
}

Type Type::transformRec(
                    llvm::function_ref<Optional<Type>(TypeBase *)> fn) const {
  if (!isa<ParenType>(getPointer())) {
    // Transform this type node.
    if (Optional<Type> transformed = fn(getPointer()))
      return *transformed;

    // Recurse.
  }

  // Recursive into children of this type.
  TypeBase *base = getPointer();
  switch (base->getKind()) {
#define ALWAYS_CANONICAL_TYPE(Id, Parent) \
case TypeKind::Id:
#define TYPE(Id, Parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::Error:
  case TypeKind::Unresolved:
  case TypeKind::TypeVariable:
  case TypeKind::GenericTypeParam:
  case TypeKind::SILToken:
    return *this;

  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::Protocol: {
    auto nominalTy = cast<NominalType>(base);
    if (auto parentTy = nominalTy->getParent()) {
      parentTy = parentTy.transformRec(fn);
      if (!parentTy)
        return Type();

      if (parentTy.getPointer() == nominalTy->getParent().getPointer())
        return *this;

      return NominalType::get(nominalTy->getDecl(), parentTy,
                              Ptr->getASTContext());
    }

    return *this;
  }
      
  case TypeKind::SILBlockStorage: {
    auto storageTy = cast<SILBlockStorageType>(base);
    Type transCap = storageTy->getCaptureType().transformRec(fn);
    if (!transCap)
      return Type();
    CanType canTransCap = transCap->getCanonicalType();
    if (canTransCap != storageTy->getCaptureType())
      return SILBlockStorageType::get(canTransCap);
    return storageTy;
  }

  case TypeKind::SILBox: {
#ifndef NDEBUG
    // This interface isn't suitable for updating the substitution map in a
    // generic SILBox.
    auto boxTy = cast<SILBoxType>(base);
    for (auto &arg : boxTy->getGenericArgs())
      assert(arg.getReplacement()->isEqual(arg.getReplacement().transformRec(fn))
             && "SILBoxType can't be transformed");
#endif
    return base;
  }
  
  case TypeKind::SILFunction: {
    auto fnTy = cast<SILFunctionType>(base);
    bool changed = false;

    SmallVector<SILParameterInfo, 8> transInterfaceParams;
    for (SILParameterInfo param : fnTy->getParameters()) {
      if (transformSILParameter(param, changed, fn)) return Type();
      transInterfaceParams.push_back(param);
    }

    SmallVector<SILYieldInfo, 8> transInterfaceYields;
    for (SILYieldInfo yield : fnTy->getYields()) {
      if (transformSILYield(yield, changed, fn)) return Type();
      transInterfaceYields.push_back(yield);
    }

    SmallVector<SILResultInfo, 8> transInterfaceResults;
    for (SILResultInfo result : fnTy->getResults()) {
      if (transformSILResult(result, changed, fn)) return Type();
      transInterfaceResults.push_back(result);
    }

    Optional<SILResultInfo> transErrorResult;
    if (fnTy->hasErrorResult()) {
      SILResultInfo result = fnTy->getErrorResult();
      if (transformSILResult(result, changed, fn)) return Type();
      transErrorResult = result;
    }

    if (!changed) return *this;

    return SILFunctionType::get(fnTy->getGenericSignature(),
                                fnTy->getExtInfo(),
                                fnTy->getCoroutineKind(),
                                fnTy->getCalleeConvention(),
                                transInterfaceParams,
                                transInterfaceYields,
                                transInterfaceResults,
                                transErrorResult,
                                Ptr->getASTContext(),
                                fnTy->getWitnessMethodConformanceOrNone());
  }

  case TypeKind::UnownedStorage:
  case TypeKind::UnmanagedStorage:
  case TypeKind::WeakStorage: {
    auto storageTy = cast<ReferenceStorageType>(base);
    Type refTy = storageTy->getReferentType();
    Type substRefTy = refTy.transformRec(fn);
    if (!substRefTy)
      return Type();

    if (substRefTy.getPointer() == refTy.getPointer())
      return *this;

    return ReferenceStorageType::get(substRefTy, storageTy->getOwnership(),
                                     Ptr->getASTContext());
  }

  case TypeKind::UnboundGeneric: {
    auto unbound = cast<UnboundGenericType>(base);
    Type substParentTy;
    if (auto parentTy = unbound->getParent()) {
      substParentTy = parentTy.transformRec(fn);
      if (!substParentTy)
        return Type();

      if (substParentTy.getPointer() == parentTy.getPointer())
        return *this;

      return UnboundGenericType::get(unbound->getDecl(), substParentTy,
                                     Ptr->getASTContext());
    }

    return *this;
  }

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct: {
    auto bound = cast<BoundGenericType>(base);
    SmallVector<Type, 4> substArgs;
    bool anyChanged = false;
    Type substParentTy;
    if (auto parentTy = bound->getParent()) {
      substParentTy = parentTy.transformRec(fn);
      if (!substParentTy)
        return Type();

      if (substParentTy.getPointer() != parentTy.getPointer())
        anyChanged = true;
    }

    for (auto arg : bound->getGenericArgs()) {
      Type substArg = arg.transformRec(fn);
      if (!substArg)
        return Type();
      substArgs.push_back(substArg);
      if (substArg.getPointer() != arg.getPointer())
        anyChanged = true;
    }

    if (!anyChanged)
      return *this;

    return BoundGenericType::get(bound->getDecl(), substParentTy, substArgs);
  }

  case TypeKind::ExistentialMetatype: {
    auto meta = cast<ExistentialMetatypeType>(base);
    auto instanceTy = meta->getInstanceType().transformRec(fn);
    if (!instanceTy)
      return Type();

    if (instanceTy.getPointer() == meta->getInstanceType().getPointer())
      return *this;

    if (meta->hasRepresentation())
      return ExistentialMetatypeType::get(instanceTy,
                                          meta->getRepresentation());
    return ExistentialMetatypeType::get(instanceTy);
  }

  case TypeKind::Metatype: {
    auto meta = cast<MetatypeType>(base);
    auto instanceTy = meta->getInstanceType().transformRec(fn);
    if (!instanceTy)
      return Type();

    if (instanceTy.getPointer() == meta->getInstanceType().getPointer())
      return *this;

    if (meta->hasRepresentation())
      return MetatypeType::get(instanceTy, meta->getRepresentation());
    return MetatypeType::get(instanceTy);
  }

  case TypeKind::DynamicSelf: {
    auto dynamicSelf = cast<DynamicSelfType>(base);
    auto selfTy = dynamicSelf->getSelfType().transformRec(fn);
    if (!selfTy)
      return Type();

    if (selfTy.getPointer() == dynamicSelf->getSelfType().getPointer())
      return *this;

    return DynamicSelfType::get(selfTy, selfTy->getASTContext());
  }

  case TypeKind::NameAlias: {
    auto alias = cast<NameAliasType>(base);
    Type oldUnderlyingType = Type(alias->getSinglyDesugaredType());
    Type newUnderlyingType = oldUnderlyingType.transformRec(fn);
    if (!newUnderlyingType) return Type();

    Type oldParentType = alias->getParent();
    Type newParentType;
    if (oldParentType && !oldParentType->hasTypeParameter() &&
        !oldParentType->hasArchetype()) {
      newParentType = oldParentType.transformRec(fn);
      if (!newParentType) return newUnderlyingType;
    }

    auto subMap = alias->getSubstitutionMap();
    if (auto genericSig = subMap.getGenericSignature()) {
      for (Type gp : genericSig->getGenericParams()) {
        Type oldReplacementType = gp.subst(subMap);
        if (!oldReplacementType)
          return newUnderlyingType;

        if (oldReplacementType->hasTypeParameter() ||
            oldReplacementType->hasArchetype())
          return newUnderlyingType;

        Type newReplacementType = oldReplacementType.transformRec(fn);
        if (!newReplacementType)
          return newUnderlyingType;

        // If anything changed with the replacement type, we lose the sugar.
        // FIXME: This is really unfortunate.
        if (!newReplacementType->isEqual(oldReplacementType))
          return newUnderlyingType;
      }
    }

    if (oldUnderlyingType.getPointer() == newUnderlyingType.getPointer())
      return *this;

    return NameAliasType::get(alias->getDecl(), newParentType, subMap,
                                   newUnderlyingType);
  }

  case TypeKind::Paren: {
    auto paren = cast<ParenType>(base);
    Type underlying = paren->getUnderlyingType().transformRec(fn);
    if (!underlying)
      return Type();

    if (underlying.getPointer() == paren->getUnderlyingType().getPointer())
      return *this;

    auto otherFlags = paren->getParameterFlags().withInOut(underlying->is<InOutType>());
    return ParenType::get(Ptr->getASTContext(), underlying->getInOutObjectType(), otherFlags);
  }

  case TypeKind::Tuple: {
    auto tuple = cast<TupleType>(base);
    bool anyChanged = false;
    SmallVector<TupleTypeElt, 4> elements;
    unsigned Index = 0;
    for (const auto &elt : tuple->getElements()) {
      Type eltTy = elt.getType().transformRec(fn);
      if (!eltTy)
        return Type();

      // If nothing has changed, just keep going.
      if (!anyChanged && eltTy.getPointer() == elt.getType().getPointer()) {
        ++Index;
        continue;
      }

      // If this is the first change we've seen, copy all of the previous
      // elements.
      if (!anyChanged) {
        // Copy all of the previous elements.
        elements.append(tuple->getElements().begin(),
                        tuple->getElements().begin() + Index);
        anyChanged = true;
      }

      // Add the new tuple element, with the new type, no initializer,
      elements.push_back(elt.getWithType(eltTy));
      ++Index;
    }

    if (!anyChanged)
      return *this;

    return TupleType::get(elements, Ptr->getASTContext());
  }


  case TypeKind::DependentMember: {
    auto dependent = cast<DependentMemberType>(base);
    auto dependentBase = dependent->getBase().transformRec(fn);
    if (!dependentBase)
      return Type();

    if (dependentBase.getPointer() == dependent->getBase().getPointer())
      return *this;

    if (auto assocType = dependent->getAssocType())
      return DependentMemberType::get(dependentBase, assocType);

    return DependentMemberType::get(dependentBase, dependent->getName());
  }

  case TypeKind::GenericFunction:
  case TypeKind::Function: {
    auto function = cast<AnyFunctionType>(base);
    auto inputTy = function->getInput().transformRec(fn);
    if (!inputTy)
      return Type();
    auto resultTy = function->getResult().transformRec(fn);
    if (!resultTy)
      return Type();

    bool isUnchanged =
        inputTy.getPointer() == function->getInput().getPointer() &&
        resultTy.getPointer() == function->getResult().getPointer();

    if (auto genericFnType = dyn_cast<GenericFunctionType>(base)) {

#ifndef NDEBUG
      // Check that generic parameters won't be trasnformed.
      // Transform generic parameters.
      for (auto param : genericFnType->getGenericParams()) {
        assert(Type(param).transformRec(fn).getPointer() == param &&
               "GenericFunctionType transform() changes type parameter");
      }
#endif
      if (isUnchanged) return *this;
      
      auto genericSig = genericFnType->getGenericSignature();
      return GenericFunctionType::get(genericSig, inputTy, resultTy,
                                      function->getExtInfo());
    }

    if (isUnchanged) return *this;

    return FunctionType::get(inputTy, resultTy,
                             function->getExtInfo());
  }

  case TypeKind::ArraySlice: {
    auto slice = cast<ArraySliceType>(base);
    auto baseTy = slice->getBaseType().transformRec(fn);
    if (!baseTy)
      return Type();

    if (baseTy.getPointer() == slice->getBaseType().getPointer())
      return *this;

    return ArraySliceType::get(baseTy);
  }

  case TypeKind::Optional: {
    auto optional = cast<OptionalType>(base);
    auto baseTy = optional->getBaseType().transformRec(fn);
    if (!baseTy)
      return Type();

    if (baseTy.getPointer() == optional->getBaseType().getPointer())
      return *this;

    return OptionalType::get(baseTy);
  }

  case TypeKind::Dictionary: {
    auto dict = cast<DictionaryType>(base);
    auto keyTy = dict->getKeyType().transformRec(fn);
    if (!keyTy)
      return Type();

    auto valueTy = dict->getValueType().transformRec(fn);
    if (!valueTy)
      return Type();

    if (keyTy.getPointer() == dict->getKeyType().getPointer() &&
        valueTy.getPointer() == dict->getValueType().getPointer())
      return *this;

    return DictionaryType::get(keyTy, valueTy);
  }

  case TypeKind::LValue: {
    auto lvalue = cast<LValueType>(base);
    auto objectTy = lvalue->getObjectType().transformRec(fn);
    if (!objectTy || objectTy->hasError())
      return objectTy;

    return objectTy.getPointer() == lvalue->getObjectType().getPointer() ?
      *this : LValueType::get(objectTy);
  }

  case TypeKind::InOut: {
    auto inout = cast<InOutType>(base);
    auto objectTy = inout->getObjectType().transformRec(fn);
    if (!objectTy || objectTy->hasError())
      return objectTy;
    
    return objectTy.getPointer() == inout->getObjectType().getPointer() ?
      *this : InOutType::get(objectTy);
  }

  case TypeKind::ProtocolComposition: {
    auto pc = cast<ProtocolCompositionType>(base);
    SmallVector<Type, 4> substMembers;
    auto members = pc->getMembers();
    bool anyChanged = false;
    unsigned index = 0;
    for (auto member : members) {
      auto substMember = member.transformRec(fn);
      if (!substMember)
        return Type();
      
      if (anyChanged) {
        substMembers.push_back(substMember);
        ++index;
        continue;
      }
      
      if (substMember.getPointer() != member.getPointer()) {
        anyChanged = true;
        substMembers.append(members.begin(), members.begin() + index);
        substMembers.push_back(substMember);
      }
      
      ++index;
    }
    
    if (!anyChanged)
      return *this;
    
    return ProtocolCompositionType::get(Ptr->getASTContext(),
                                        substMembers,
                                        pc->hasExplicitAnyObject());
  }
  }
  
  llvm_unreachable("Unhandled type in transformation");
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

bool Type::isPrivateStdlibType(bool treatNonBuiltinProtocolsAsPublic) const {
  Type Ty = *this;
  if (!Ty)
    return false;

  // A 'public' typealias can have an 'internal' type.
  if (auto *NAT = dyn_cast<NameAliasType>(Ty.getPointer())) {
    auto *AliasDecl = NAT->getDecl();
    if (auto parent = NAT->getParent()) {
      if (parent.isPrivateStdlibType(treatNonBuiltinProtocolsAsPublic))
        return true;
    }

    if (AliasDecl->isPrivateStdlibDecl(treatNonBuiltinProtocolsAsPublic))
      return true;

    return Type(NAT->getSinglyDesugaredType()).isPrivateStdlibType(
                                            treatNonBuiltinProtocolsAsPublic);
  }

  if (auto Paren = dyn_cast<ParenType>(Ty.getPointer())) {
    Type Underlying = Paren->getUnderlyingType();
    return Underlying.isPrivateStdlibType(treatNonBuiltinProtocolsAsPublic);
  }

  if (Type Unwrapped = Ty->getOptionalObjectType())
    return Unwrapped.isPrivateStdlibType(treatNonBuiltinProtocolsAsPublic);

  if (auto TyD = Ty->getAnyNominal())
    if (TyD->isPrivateStdlibDecl(treatNonBuiltinProtocolsAsPublic))
      return true;

  return false;
}

bool UnownedStorageType::isLoadable(ResilienceExpansion resilience) const {
  return getReferentType()->usesNativeReferenceCounting(resilience);
}

static bool doesOpaqueClassUseNativeReferenceCounting(const ASTContext &ctx) {
  return !ctx.LangOpts.EnableObjCInterop;
}

static bool usesNativeReferenceCounting(ClassDecl *theClass,
                                        ResilienceExpansion resilience) {
  // TODO: Resilience? there might be some legal avenue of changing this.
  while (Type supertype = theClass->getSuperclass()) {
    theClass = supertype->getClassOrBoundGenericClass();
    assert(theClass);
  }
  return !theClass->hasClangNode();
}

bool TypeBase::usesNativeReferenceCounting(ResilienceExpansion resilience) {
  CanType type = getCanonicalType();
  switch (type->getKind()) {
#define SUGARED_TYPE(id, parent) case TypeKind::id:
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("sugared canonical type?");

  case TypeKind::BuiltinNativeObject:
  case TypeKind::SILBox:
    return true;

  case TypeKind::BuiltinUnknownObject:
  case TypeKind::BuiltinBridgeObject:
    return ::doesOpaqueClassUseNativeReferenceCounting(type->getASTContext());

  case TypeKind::Class:
    return ::usesNativeReferenceCounting(cast<ClassType>(type)->getDecl(),
                                         resilience);
  case TypeKind::BoundGenericClass:
    return ::usesNativeReferenceCounting(
                                  cast<BoundGenericClassType>(type)->getDecl(),
                                         resilience);
  case TypeKind::UnboundGeneric:
    return ::usesNativeReferenceCounting(
                    cast<ClassDecl>(cast<UnboundGenericType>(type)->getDecl()),
                                         resilience);

  case TypeKind::DynamicSelf:
    return cast<DynamicSelfType>(type).getSelfType()
             ->usesNativeReferenceCounting(resilience);

  case TypeKind::Archetype: {
    auto archetype = cast<ArchetypeType>(type);
    auto layout = archetype->getLayoutConstraint();
    (void)layout;
    assert(archetype->requiresClass() ||
           (layout && layout->isRefCounted()));
    if (auto supertype = archetype->getSuperclass())
      return supertype->usesNativeReferenceCounting(resilience);
    return ::doesOpaqueClassUseNativeReferenceCounting(type->getASTContext());
  }

  case TypeKind::Protocol:
  case TypeKind::ProtocolComposition: {
    auto layout = getExistentialLayout();
    assert(layout.requiresClass() && "Opaque existentials don't use refcounting");
    if (layout.superclass)
      return layout.superclass->usesNativeReferenceCounting(resilience);
    return ::doesOpaqueClassUseNativeReferenceCounting(type->getASTContext());
  }

  case TypeKind::Function:
  case TypeKind::GenericFunction:
  case TypeKind::SILFunction:
  case TypeKind::SILBlockStorage:
  case TypeKind::Error:
  case TypeKind::Unresolved:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinRawPointer:
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
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct:
  case TypeKind::SILToken:
  case TypeKind::UnownedStorage:
  case TypeKind::UnmanagedStorage:
  case TypeKind::WeakStorage:
  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
    llvm_unreachable("type is not a class reference");
  }

  llvm_unreachable("Unhandled type kind!");
}

//
// SILBoxType implementation
//

void SILBoxType::Profile(llvm::FoldingSetNodeID &id, SILLayout *Layout,
                         SubstitutionList Args) {
  id.AddPointer(Layout);
  profileSubstitutionList(id, Args);
}

SILBoxType::SILBoxType(ASTContext &C,
                       SILLayout *Layout, SubstitutionList Args)
: TypeBase(TypeKind::SILBox, &C,
           getRecursivePropertiesFromSubstitutions(Args)), Layout(Layout) {
  Bits.SILBoxType.NumGenericArgs = Args.size();
#ifndef NDEBUG
  // Check that the generic args are reasonable for the box's signature.
  if (Layout->getGenericSignature())
    (void)Layout->getGenericSignature()->getSubstitutionMap(Args);
  for (auto &arg : Args)
    assert(arg.getReplacement()->isCanonical() &&
           "box arguments must be canonical types!");
#endif
  std::uninitialized_copy(Args.begin(), Args.end(),
                          getTrailingObjects<Substitution>());
}

RecursiveTypeProperties SILBoxType::
getRecursivePropertiesFromSubstitutions(SubstitutionList Params) {
  RecursiveTypeProperties props;
  for (auto &param : Params) {
    props |= param.getReplacement()->getRecursiveProperties();
  }
  return props;
}

Type TypeBase::openAnyExistentialType(ArchetypeType *&opened) {
  assert(isAnyExistentialType());
  if (auto metaty = getAs<ExistentialMetatypeType>()) {
    opened = ArchetypeType::getOpened(metaty->getInstanceType());
    if (metaty->hasRepresentation())
      return MetatypeType::get(opened, metaty->getRepresentation());
    else
      return MetatypeType::get(opened);
  }
  opened = ArchetypeType::getOpened(this);
  return opened;
}
