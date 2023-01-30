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

/// G<{X1, ..., Xn}, {Y1, ..., Yn}>... => {G<X1, Y1>, ..., G<Xn, Yn>}...
PackExpansionType *PackExpansionType::expand() {
  auto countType = getCountType();
  auto *countPack = countType->getAs<PackType>();
  if (countPack == nullptr)
    return this;

  auto patternType = getPatternType();
  if (patternType->is<PackType>())
    return this;

  unsigned j = 0;
  SmallVector<Type, 4> expandedTypes;
  for (auto type : countPack->getElementTypes()) {
    Type expandedCount;
    if (auto *expansion = type->getAs<PackExpansionType>())
      expandedCount = expansion->getCountType();

    auto expandedPattern = patternType.transformRec(
      [&](Type t) -> Optional<Type> {
        if (t->is<PackExpansionType>())
          return t;

        if (auto *nestedPack = t->getAs<PackType>()) {
          auto nestedPackElts = nestedPack->getElementTypes();
          if (j < nestedPackElts.size()) {
            if (expandedCount) {
              if (auto *expansion = nestedPackElts[j]->getAs<PackExpansionType>())
                return expansion->getPatternType();
            } else {
              return nestedPackElts[j];
            }
          }

          return ErrorType::get(t->getASTContext());
        }

        return None;
      });

    if (expandedCount) {
      expandedTypes.push_back(PackExpansionType::get(expandedPattern,
                                                     expandedCount));
    } else {
      expandedTypes.push_back(expandedPattern);
    }

    ++j;
  }

  auto *packType = PackType::get(getASTContext(), expandedTypes);
  return PackExpansionType::get(packType, countType);
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

/// (W, {X, Y}..., Z) => (W, X, Y, Z)
TupleType *TupleType::flattenPackTypes() {
  bool anyChanged = false;
  SmallVector<TupleTypeElt, 4> elts;

  for (unsigned i = 0, e = getNumElements(); i < e; ++i) {
    auto elt = getElement(i);

    if (auto *expansionType = elt.getType()->getAs<PackExpansionType>()) {
      if (auto *packType = expansionType->getPatternType()->getAs<PackType>()) {
        if (!anyChanged) {
          elts.append(getElements().begin(), getElements().begin() + i);
          anyChanged = true;
        }

        bool first = true;
        for (auto packElt : packType->getElementTypes()) {
          if (first) {
            elts.push_back(TupleTypeElt(packElt, elt.getName()));
            first = false;
            continue;
          }
          elts.push_back(TupleTypeElt(packElt));
        }

        continue;
      }
    }

    if (anyChanged)
      elts.push_back(elt);
  }

  if (!anyChanged)
    return this;

  return TupleType::get(elts, getASTContext());
}

bool AnyFunctionType::containsPackExpansionType(ArrayRef<Param> params) {
  for (auto param : params) {
    if (param.getPlainType()->is<PackExpansionType>())
      return true;
  }

  return false;
}

/// (W, {X, Y}..., Z) -> T => (W, X, Y, Z) -> T
AnyFunctionType *AnyFunctionType::flattenPackTypes() {
  bool anyChanged = false;
  SmallVector<AnyFunctionType::Param, 4> params;

  for (unsigned i = 0, e = getParams().size(); i < e; ++i) {
    auto param = getParams()[i];

    if (auto *expansionType = param.getPlainType()->getAs<PackExpansionType>()) {
      if (auto *packType = expansionType->getPatternType()->getAs<PackType>()) {
        if (!anyChanged) {
          params.append(getParams().begin(), getParams().begin() + i);
          anyChanged = true;
        }

        bool first = true;
        for (auto packElt : packType->getElementTypes()) {
          if (first) {
            params.push_back(param.withType(packElt));
            first = false;
            continue;
          }
          params.push_back(param.withType(packElt).getWithoutLabels());
        }

        continue;
      }
    }

    if (anyChanged)
      params.push_back(param);
  }

  if (!anyChanged)
    return this;

  if (auto *genericFuncType = getAs<GenericFunctionType>()) {
    return GenericFunctionType::get(genericFuncType->getGenericSignature(),
                                    params, getResult(), getExtInfo());
  } else {
    return FunctionType::get(params, getResult(), getExtInfo());
  }
}

bool PackType::containsPackExpansionType() const {
  for (auto type : getElementTypes()) {
    if (type->is<PackExpansionType>())
      return true;
  }

  return false;
}

/// {W, {X, Y}..., Z} => {W, X, Y, Z}
PackType *PackType::flattenPackTypes() {
  bool anyChanged = false;
  SmallVector<Type, 4> elts;

  for (unsigned i = 0, e = getNumElements(); i < e; ++i) {
    auto elt = getElementType(i);

    if (auto *expansionType = elt->getAs<PackExpansionType>()) {
      if (auto *packType = expansionType->getPatternType()->getAs<PackType>()) {
        if (!anyChanged) {
          elts.append(getElementTypes().begin(), getElementTypes().begin() + i);
          anyChanged = true;
        }

        for (auto packElt : packType->getElementTypes()) {
          elts.push_back(packElt);
        }

        continue;
      }
    }

    if (anyChanged)
      elts.push_back(elt);
  }

  if (!anyChanged)
    return this;

  return PackType::get(getASTContext(), elts);
}

CanPackType PackType::getReducedShape() {
  SmallVector<Type, 4> elts;

  auto &ctx = getASTContext();

  for (auto elt : getElementTypes()) {
    // T... => shape(T)...
    if (auto *packExpansionType = elt->getAs<PackExpansionType>()) {
      elts.push_back(packExpansionType->getReducedShape());
      continue;
    }

    // Use () as a placeholder for scalar shape.
    assert(!elt->is<PackArchetypeType>() &&
           "Pack archetype outside of a pack expansion");
    elts.push_back(ctx.TheEmptyTupleType);
  }

  return CanPackType(PackType::get(ctx, elts));
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
      wrappedArgs.push_back(PackExpansionType::get(
          arg, arg->getReducedShape()));
      continue;
    }

    wrappedArgs.push_back(arg);
  }

  return get(C, wrappedArgs)->flattenPackTypes();
}

CanPackType PackArchetypeType::getSingletonPackType() {
  SmallVector<Type, 1> types;
  types.push_back(PackExpansionType::get(this, getReducedShape()));
  return CanPackType(PackType::get(getASTContext(), types));
}
