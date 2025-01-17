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
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;

/// FV(PackExpansionType(Pattern, Count), N) = FV(Pattern, N+1)
/// FV(PackElementType(Param, M), N) = FV(Param, 0) if M >= N, {} otherwise
/// FV(Param, N) = {Param}
static Type transformTypeParameterPacksRec(
    Type t, llvm::function_ref<std::optional<Type>(SubstitutableType *)> fn,
    unsigned expansionLevel) {
  return t.transformWithPosition(
      TypePosition::Invariant,
      [&](TypeBase *t, TypePosition p) -> std::optional<Type> {
        // If we're already inside N levels of PackExpansionType,  and we're
        // walking into another PackExpansionType, a type parameter pack
        // reference now needs level (N+1) to be free.
        if (auto *expansionType = dyn_cast<PackExpansionType>(t)) {
          auto countType = expansionType->getCountType();
          auto patternType = expansionType->getPatternType();
          auto newPatternType = transformTypeParameterPacksRec(
              patternType, fn, expansionLevel + 1);
          if (patternType.getPointer() != newPatternType.getPointer())
            return Type(PackExpansionType::get(patternType, countType));

          return Type(expansionType);
        }

        // A PackElementType with level N reaches past N levels of
        // nested PackExpansionType. So a type parameter pack reference
        // therein is free if N is greater than or equal to our current
        // expansion level.
        if (auto *eltType = dyn_cast<PackElementType>(t)) {
          if (eltType->getLevel() >= expansionLevel) {
            return transformTypeParameterPacksRec(eltType->getPackType(), fn,
                                                  /*expansionLevel=*/0);
          }

          return Type(eltType);
        }

        // A bare type parameter pack is like a PackElementType with level 0.
        if (auto *paramType = dyn_cast<SubstitutableType>(t)) {
          if (expansionLevel == 0 && (isa<PackArchetypeType>(paramType) ||
                                      paramType->isRootParameterPack())) {
            return fn(paramType);
          }

          return Type(paramType);
        }

        return std::nullopt;
      });
}

Type Type::transformTypeParameterPacks(
    llvm::function_ref<std::optional<Type>(SubstitutableType *)> fn) const {
  return transformTypeParameterPacksRec(*this, fn, /*expansionLevel=*/0);
}

namespace {

/// Collects all unique pack type parameters referenced from the pattern type,
/// skipping those captured by nested pack expansion types.
struct PackReferenceCollector: TypeWalker {
  llvm::function_ref<bool (Type)> fn;
  unsigned expansionLevel;
  SmallVector<unsigned, 2> elementLevel;

  PackReferenceCollector(llvm::function_ref<bool (Type)> fn)
    : fn(fn), expansionLevel(0) {
    elementLevel.push_back(0);
  }

  Action walkToTypePre(Type t) override {
    if (t->is<PackExpansionType>()) {
      ++expansionLevel;
      return Action::Continue;
    }

    if (auto *boundGenericType = dyn_cast<BoundGenericType>(t.getPointer())) {
      if (auto parentType = boundGenericType->getParent())
        parentType.walk(*this);

      for (auto type : boundGenericType->getExpandedGenericArgs())
        type.walk(*this);

      return Action::SkipNode;
    }

    if (auto *typeAliasType = dyn_cast<TypeAliasType>(t.getPointer())) {
      if (typeAliasType->getDecl()->isGeneric()) {
        if (auto parentType = typeAliasType->getParent())
          parentType.walk(*this);

        for (auto type : typeAliasType->getExpandedGenericArgs())
          type.walk(*this);

        return Action::SkipNode;
      }
    }

    if (auto *eltType = t->getAs<PackElementType>()) {
      elementLevel.push_back(eltType->getLevel());
      return Action::Continue;
    }

    if (elementLevel.back() == expansionLevel) {
      if (fn(t))
        return Action::Stop;
    }

    return Action::Continue;
  }

  Action walkToTypePost(Type t) override {
    if (t->is<PackExpansionType>())
      --expansionLevel;

    if (t->is<PackElementType>())
      elementLevel.pop_back();

    return Action::Continue;
  }
};

}

void TypeBase::walkPackReferences(
    llvm::function_ref<bool (Type)> fn) {
  Type(this).walk(PackReferenceCollector(fn));
}

void TypeBase::getTypeParameterPacks(
    SmallVectorImpl<Type> &rootParameterPacks) {
  walkPackReferences([&](Type t) {
    if (auto *paramTy = t->getAs<GenericTypeParamType>()) {
      if (paramTy->isParameterPack())
        rootParameterPacks.push_back(paramTy);
    } else if (auto *archetypeTy = t->getAs<PackArchetypeType>()) {
      if (archetypeTy->isRoot()) {
        rootParameterPacks.push_back(archetypeTy);
      } else {
        auto *genericEnv = archetypeTy->getGenericEnvironment();
        auto paramTy = archetypeTy->getInterfaceType()->getRootGenericParam();
        rootParameterPacks.push_back(
            genericEnv->mapTypeIntoContext(paramTy));
      }
    }

    return false;
  });
}

bool TypeBase::isParameterPack() {
  return getDependentMemberRoot()->isRootParameterPack();
}

bool TypeBase::isRootParameterPack() {
  Type t(this);

  return t->is<GenericTypeParamType>() &&
         t->castTo<GenericTypeParamType>()->isParameterPack();
}

PackType *TypeBase::getPackSubstitutionAsPackType() {
  if (auto pack = getAs<PackType>()) {
    return pack;
  } else {
    return PackType::getSingletonPackExpansion(this);
  }
}

static Type increasePackElementLevelImpl(
    Type type, unsigned level, unsigned outerLevel) {
  assert(level > 0);

  return type.transformRec([&](TypeBase *t) -> std::optional<Type> {
    if (auto *elementType = dyn_cast<PackElementType>(t)) {
      if (elementType->getLevel() >= outerLevel) {
        elementType = PackElementType::get(elementType->getPackType(),
                                           elementType->getLevel() + level);
      }

      return Type(elementType);
    }

    if (auto *expansionType = dyn_cast<PackExpansionType>(t)) {
      return Type(PackExpansionType::get(
          increasePackElementLevelImpl(expansionType->getPatternType(),
                                       level, outerLevel + 1),
          expansionType->getCountType()));
    }

    if (t->isParameterPack() || isa<PackArchetypeType>(t)) {
      if (outerLevel == 0)
        return Type(PackElementType::get(t, level));

      return Type(t);
    }

    return std::nullopt;
  });
}

Type TypeBase::increasePackElementLevel(unsigned level) {
  if (level == 0)
    return Type(this);

  return increasePackElementLevelImpl(Type(this), level, 0);
}

CanType PackExpansionType::getReducedShape() {
  auto reducedShape = countType->getReducedShape();
  if (reducedShape == getASTContext().TheEmptyTupleType)
    return reducedShape;

  return CanType(PackExpansionType::get(reducedShape, reducedShape));
}

unsigned TupleType::getNumScalarElements() const {
  unsigned n = 0;
  for (auto elt : getElements()) {
    if (!elt.getType()->is<PackExpansionType>())
      ++n;
  }

  return n;
}

bool TupleType::containsPackExpansionType() const {
  assert(!hasTypeVariable());
  for (auto elt : getElements()) {
    auto eltTy = elt.getType();
    assert(!eltTy->hasTypeVariable());
    if (eltTy->is<PackExpansionType>())
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
    auto paramTy = param.getPlainType();
    assert(!paramTy->hasTypeVariable());
    if (paramTy->is<PackExpansionType>())
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
SmallVector<Type, 2> BoundGenericType::getExpandedGenericArgs() {
  // It would be nicer to use genericSig.getInnermostGenericParams() here,
  // but that triggers a request cycle if we're in the middle of computing
  // the generic signature already.
  SmallVector<GenericTypeParamType *, 2> params;
  for (auto *paramDecl : getDecl()->getGenericParams()->getParams()) {
    params.push_back(paramDecl->getDeclaredInterfaceType()
                         ->castTo<GenericTypeParamType>());
  }

  return PackType::getExpandedGenericArgs(params, getGenericArgs());
}

/// <T...> Foo<T, Pack{Int, String}> => Pack{T..., Int, String}
SmallVector<Type, 2> TypeAliasType::getExpandedGenericArgs() {
  if (!getDecl()->isGeneric())
    return SmallVector<Type, 2>();

  auto genericSig = getGenericSignature();
  return PackType::getExpandedGenericArgs(
                       genericSig.getInnermostGenericParams(),
                       getDirectGenericArgs());
}

/// <T...> Pack{T, Pack{Int, String}} => {T..., Int, String}
SmallVector<Type, 2>
PackType::getExpandedGenericArgs(ArrayRef<GenericTypeParamType *> params,
                                 ArrayRef<Type> args) {
  SmallVector<Type, 2> wrappedArgs;

  assert(params.size() == args.size());
  for (unsigned i = 0, e = params.size(); i < e; ++i) {
    auto arg = args[i];

    if (params[i]->isParameterPack()) {
      // FIXME: A temporary fix to make it possible to debug expressions
      // with partially resolved variadic generic types. The issue stems
      // from the fact that `BoundGenericType` is allowed to have pack
      // parameters directly represented by type variables, as soon as
      // that is no longer the case this check should be removed.
      if (arg->is<TypeVariableType>()) {
        wrappedArgs.push_back(arg);
      } else {
        auto argPackElements = arg->castTo<PackType>()->getElementTypes();
        wrappedArgs.append(argPackElements.begin(), argPackElements.end());
      }
      continue;
    }

    wrappedArgs.push_back(arg);
  }

  return wrappedArgs;
}

PackType *PackType::getSingletonPackExpansion(Type param) {
  SmallVector<Type, 2> rootParameterPacks;
  param->getTypeParameterPacks(rootParameterPacks);
  assert(rootParameterPacks.size() >= 1);
  auto count = rootParameterPacks[0];
  return get(param->getASTContext(), {PackExpansionType::get(param, count)});
}

CanPackType CanPackType::getSingletonPackExpansion(CanType param) {
  // Note: You can't just wrap the result in CanPackType() here because
  // PackExpansionType has the additional requirement that the count type
  // must be a reduced shape.
  return cast<PackType>(
    PackType::getSingletonPackExpansion(param)
      ->getCanonicalType());
}

PackExpansionType *PackType::unwrapSingletonPackExpansion() const {
  if (getNumElements() == 1) {
    if (auto expansion = getElementTypes()[0]->getAs<PackExpansionType>()) {
      auto pattern = expansion->getPatternType();
      if (pattern->isParameterPack() || pattern->is<PackArchetypeType>())
        return expansion;
    }
  }

  return nullptr;
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
  // work as a legal formal type.
  std::optional<SmallVector<CanType, 4>> formalEltTypes;
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

    assert(isLegal || formalEltTypes.has_value());
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
