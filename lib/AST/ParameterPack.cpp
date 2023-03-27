//===--- ParameterPack.cpp - Utilities for variadic generics --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements utilities for substituting type parameter packs
// appearing in pack expansion types.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;

namespace {

/// Collects all unique pack type parameters referenced from the pattern type,
/// skipping those captured by nested pack expansion types.
struct PackTypeParameterCollector: TypeWalker {
  llvm::SetVector<Type> typeParams;

  Action walkToTypePre(Type t) override {
    if (t->is<PackExpansionType>())
      return Action::SkipChildren;

    if (auto *boundGenericType = dyn_cast<BoundGenericType>(t.getPointer())) {
      if (auto parentType = boundGenericType->getParent())
        parentType.walk(*this);

      Type(boundGenericType->getExpandedGenericArgsPack()).walk(*this);
      return Action::SkipChildren;
    }

    if (auto *typeAliasType = dyn_cast<TypeAliasType>(t.getPointer())) {
      if (typeAliasType->getDecl()->isGeneric()) {
        if (auto parentType = typeAliasType->getParent())
          parentType.walk(*this);

        Type(typeAliasType->getExpandedGenericArgsPack()).walk(*this);
        return Action::SkipChildren;
      }
    }

    if (auto *paramTy = t->getAs<GenericTypeParamType>()) {
      if (paramTy->isParameterPack())
        typeParams.insert(paramTy);
    } else if (auto *archetypeTy = t->getAs<PackArchetypeType>()) {
      typeParams.insert(archetypeTy->getRoot());
    }

    return Action::Continue;
  }
};

}

void TypeBase::getTypeParameterPacks(
    SmallVectorImpl<Type> &rootParameterPacks) {
  PackTypeParameterCollector collector;
  Type(this).walk(collector);

  rootParameterPacks.append(collector.typeParams.begin(),
                            collector.typeParams.end());
}

bool GenericTypeParamType::isParameterPack() const {
  if (auto param = getDecl()) {
    return param->isParameterPack();
  }

  auto fixedNum = ParamOrDepthIndex.get<DepthIndexTy>();
  return (fixedNum & GenericTypeParamType::TYPE_SEQUENCE_BIT) ==
         GenericTypeParamType::TYPE_SEQUENCE_BIT;
}

PackType *TypeBase::getPackSubstitutionAsPackType() {
  if (auto pack = getAs<PackType>()) {
    return pack;
  } else {
    return PackType::getSingletonPackExpansion(this);
  }
}

CanType PackExpansionType::getReducedShape() {
  auto reducedShape = countType->getReducedShape();
  if (reducedShape == getASTContext().TheEmptyTupleType)
    return reducedShape;

  return CanType(PackExpansionType::get(reducedShape, reducedShape));
}

bool TupleType::containsPackExpansionType() const {
  for (auto elt : getElements()) {
    if (elt.getType()->is<PackExpansionType>())
      return true;
  }

  return false;
}

bool CanTupleType::containsPackExpansionTypeImpl(CanTupleType tuple) {
  for (auto eltType : tuple.getElementTypes()) {
    if (isa<PackExpansionType>(eltType))
      return true;
  }

  return false;
}

bool AnyFunctionType::containsPackExpansionType(ArrayRef<Param> params) {
  for (auto param : params) {
    if (param.getPlainType()->is<PackExpansionType>())
      return true;
  }

  return false;
}

bool PackType::containsPackExpansionType() const {
  for (auto type : getElementTypes()) {
    if (type->is<PackExpansionType>())
      return true;
  }

  return false;
}

template <class T>
static CanPackType getReducedShapeOfPack(const ASTContext &ctx,
                                         const T &elementTypes) {
  SmallVector<Type, 4> elts;
  elts.reserve(elementTypes.size());

  for (auto elt : elementTypes) {
    // T... => shape(T)...
    if (auto *packExpansionType = elt->template getAs<PackExpansionType>()) {
      elts.push_back(packExpansionType->getReducedShape());
      continue;
    }

    // Use () as a placeholder for scalar shape.
    assert(!elt->template is<PackArchetypeType>() &&
           "Pack archetype outside of a pack expansion");
    elts.push_back(ctx.TheEmptyTupleType);
  }

  return CanPackType(PackType::get(ctx, elts));
}

CanPackType PackType::getReducedShape() {
  return getReducedShapeOfPack(getASTContext(), getElementTypes());
}

CanPackType SILPackType::getReducedShape() const {
  return getReducedShapeOfPack(getASTContext(), getElementTypes());
}

CanType TypeBase::getReducedShape() {
  if (isTypeParameter()) {
    auto *genericParam = getRootGenericParam();
    if (genericParam->isParameterPack())
      return genericParam->getCanonicalType();

    // Use () as a placeholder for scalar shape.
    return getASTContext().TheEmptyTupleType;
  }

  if (auto *packArchetype = getAs<PackArchetypeType>())
     return packArchetype->getReducedShape();

  if (auto *packType = getAs<PackType>())
    return packType->getReducedShape();

  if (auto *expansionType = getAs<PackExpansionType>())
    return expansionType->getReducedShape();

  if (auto *silPackType = getAs<SILPackType>()) {
    auto can = cast<SILPackType>(silPackType->getCanonicalType());
    return can->getReducedShape();
  }

  SmallVector<Type, 2> rootParameterPacks;
  getTypeParameterPacks(rootParameterPacks);

  if (!rootParameterPacks.empty())
    return rootParameterPacks.front()->getReducedShape();

  assert(!isTypeVariableOrMember());
  assert(!hasTypeParameter());

  // Use () as a placeholder for scalar shape.
  return getASTContext().TheEmptyTupleType;
}

unsigned ParameterList::getOrigParamIndex(SubstitutionMap subMap,
                                          unsigned substIndex) const {
  unsigned remappedIndex = substIndex;

  for (unsigned i = 0, e = size(); i < e; ++i) {
    auto *param = get(i);
    auto paramType = param->getInterfaceType();

    unsigned substCount = 1;
    if (auto *packExpansionType = paramType->getAs<PackExpansionType>()) {
      auto replacementType = packExpansionType->getCountType().subst(subMap);
      if (auto *packType = replacementType->getAs<PackType>()) {
        substCount = packType->getNumElements();
      }
    }

    if (remappedIndex < substCount)
      return i;

    remappedIndex -= substCount;
  }

  llvm::errs() << "Invalid substituted argument index: " << substIndex << "\n";
  subMap.dump(llvm::errs());
  dump(llvm::errs());
  abort();
}

/// <T...> Foo<T, Pack{Int, String}> => Pack{T..., Int, String}
PackType *BoundGenericType::getExpandedGenericArgsPack() {
  // It would be nicer to use genericSig.getInnermostGenericParams() here,
  // but that triggers a request cycle if we're in the middle of computing
  // the generic signature already.
  SmallVector<Type, 2> params;
  for (auto *paramDecl : getDecl()->getGenericParams()->getParams()) {
    params.push_back(paramDecl->getDeclaredInterfaceType());
  }

  return PackType::get(getASTContext(),
                       TypeArrayView<GenericTypeParamType>(params),
                       getGenericArgs());
}

/// <T...> Foo<T, Pack{Int, String}> => Pack{T..., Int, String}
PackType *TypeAliasType::getExpandedGenericArgsPack() {
  if (!getDecl()->isGeneric())
    return nullptr;

  auto genericSig = getGenericSignature();
  return PackType::get(getASTContext(),
                       genericSig.getInnermostGenericParams(),
                       getDirectGenericArgs());
}

/// <T...> Pack{T, Pack{Int, String}} => Pack{T..., Int, String}
PackType *PackType::get(const ASTContext &C,
                        TypeArrayView<GenericTypeParamType> params,
                        ArrayRef<Type> args) {
  SmallVector<Type, 2> wrappedArgs;

  assert(params.size() == args.size());
  for (unsigned i = 0, e = params.size(); i < e; ++i) {
    auto arg = args[i];

    if (params[i]->isParameterPack()) {
      auto argPackElements = arg->castTo<PackType>()->getElementTypes();
      wrappedArgs.append(argPackElements.begin(), argPackElements.end());
      continue;
    }

    wrappedArgs.push_back(arg);
  }

  return get(C, wrappedArgs);
}

PackType *PackType::getSingletonPackExpansion(Type param) {
  assert(param->isParameterPack() || param->is<PackArchetypeType>());
  return get(param->getASTContext(), {PackExpansionType::get(param, param)});
}

CanPackType CanPackType::getSingletonPackExpansion(CanType param) {
  return CanPackType(PackType::getSingletonPackExpansion(param));
}

bool SILPackType::containsPackExpansionType() const {
  for (auto type : getElementTypes()) {
    if (isa<PackExpansionType>(type))
      return true;
  }

  return false;
}

CanPackType
CanTupleType::getInducedPackTypeImpl(CanTupleType tuple) {
  return getInducedPackTypeImpl(tuple, 0, tuple->getNumElements());
}

CanPackType
CanTupleType::getInducedPackTypeImpl(CanTupleType tuple, unsigned start, unsigned count) {
  assert(start + count <= tuple->getNumElements() && "range out of range");
  auto &ctx = tuple->getASTContext();
  return CanPackType::get(ctx, tuple.getElementTypes().slice(start, count));
}

static CanType getApproximateFormalElementType(const ASTContext &ctx,
                                               CanType loweredEltType) {
  CanType formalEltType = TupleType::getEmpty(ctx);
  if (auto expansion = dyn_cast<PackExpansionType>(loweredEltType))
    formalEltType = CanPackExpansionType::get(formalEltType,
                                              expansion.getCountType());
  return formalEltType;
}

template <class Collection>
static CanPackType getApproximateFormalPackType(const ASTContext &ctx,
                                                Collection loweredEltTypes) {
  // Build an array of formal element types, but be lazy about it:
  // use the original array unless we see an element type that doesn't
  // work as a legal format type.
  Optional<SmallVector<CanType, 4>> formalEltTypes;
  for (auto i : indices(loweredEltTypes)) {
    auto loweredEltType = loweredEltTypes[i];
    bool isLegal = loweredEltType->isLegalFormalType();

    // If the type isn't legal as a formal type, substitute the empty
    // tuple type (or an invariant expansion of it over the count type).
    CanType formalEltType = loweredEltType;
    if (!isLegal) {
      formalEltType = getApproximateFormalElementType(ctx, loweredEltType);
    }

    // If we're already building an array, unconditionally append to it.
    // Otherwise, if the type isn't legal, build the array up to this
    // point and then append.  Otherwise, we're still being lazy.
    if (formalEltTypes) {
      formalEltTypes->push_back(formalEltType);
    } else if (!isLegal) {
      formalEltTypes.emplace();
      formalEltTypes->reserve(loweredEltTypes.size());
      formalEltTypes->append(loweredEltTypes.begin(),
                             loweredEltTypes.begin() + i);
      formalEltTypes->push_back(formalEltType);
    }

    assert(isLegal || formalEltTypes.hasValue());
  }

  // Use the array we built if we made one (if we ever saw a non-legal
  // element type).
  if (formalEltTypes) {
    return CanPackType::get(ctx, *formalEltTypes);
  } else {
    return CanPackType::get(ctx, loweredEltTypes);
  }
}

CanPackType SILPackType::getApproximateFormalPackType() const {
  return ::getApproximateFormalPackType(getASTContext(), getElementTypes());
}

CanPackType
CanTupleType::getInducedApproximateFormalPackTypeImpl(CanTupleType tuple) {
  return ::getApproximateFormalPackType(tuple->getASTContext(),
                                        tuple.getElementTypes());
}
