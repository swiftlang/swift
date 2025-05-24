//===--- TypeTransform.h - Recursive visitor to replace types ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the visitor used by Type::transformRec() and
// Type::transformWithPosition().
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_TYPETRANSFORM_H
#define SWIFT_AST_TYPETRANSFORM_H

#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/SILLayout.h"
#include "swift/AST/LifetimeDependence.h"

namespace swift {

template<typename Derived>
class TypeTransform {
public:
  ASTContext &ctx;

  TypeTransform(ASTContext &ctx) : ctx(ctx) {}

private:
  Derived &asDerived() { return *static_cast<Derived *>(this); }

  /// \param pos The variance position of the result type.
  bool transformSILResult(
      TypePosition pos, SILResultInfo &result, bool &changed) {
    Type transType = doIt(result.getInterfaceType(), pos);
    if (!transType) return true;

    CanType canTransType = transType->getCanonicalType();
    if (canTransType != result.getInterfaceType()) {
      changed = true;
      result = result.getWithInterfaceType(canTransType);
    }
    return false;
  }

  /// \param pos The variance position of the yield type.
  bool transformSILYield(
      TypePosition pos, SILYieldInfo &yield, bool &changed) {
    Type transType = doIt(yield.getInterfaceType(), pos);
    if (!transType) return true;

    CanType canTransType = transType->getCanonicalType();
    if (canTransType != yield.getInterfaceType()) {
      changed = true;
      yield = yield.getWithInterfaceType(canTransType);
    }
    return false;
  }

  /// \param pos The variance position of the parameter type.
  bool transformSILParameter(
      TypePosition pos, SILParameterInfo &param, bool &changed) {
    Type transType = doIt(param.getInterfaceType(), pos);
    if (!transType) return true;

    CanType canTransType = transType->getCanonicalType();
    if (canTransType != param.getInterfaceType()) {
      changed = true;
      param = param.getWithInterfaceType(canTransType);
    }
    return false;
  }

  PackType *getTransformedPack(Type substType) {
    if (auto pack = substType->getAs<PackType>()) {
      return pack;
    }

    // The pack matchers like to make expansions out of packs, and
    // these types then propagate out into transforms.  Make sure we
    // flatten them exactly if they were the underlying pack.
    // FIXME: stop doing this and make PackExpansionType::get assert
    // that we never construct these types
    if (auto expansion = substType->getAs<PackExpansionType>()) {
      return expansion->getPatternType()->getAs<PackType>();
    }

    return nullptr;
  }

public:
  Type doIt(Type t, TypePosition pos) {
    // Transform this type node.
    if (std::optional<Type> transformed =
            asDerived().transform(t.getPointer(), pos))
      return *transformed;

    // Recur into children of this type.
    TypeBase *base = t.getPointer();

    switch (base->getKind()) {
#define BUILTIN_CONCRETE_TYPE(Id, Parent) \
case TypeKind::Id:
#define TYPE(Id, Parent)
#include "swift/AST/TypeNodes.def"
    case TypeKind::Error:
    case TypeKind::Unresolved:
    case TypeKind::TypeVariable:
    case TypeKind::Placeholder:
    case TypeKind::SILToken:
    case TypeKind::Module:
    case TypeKind::BuiltinTuple:
    case TypeKind::Integer:
      return t;

    case TypeKind::BuiltinFixedArray: {
      auto bfaTy = cast<BuiltinFixedArrayType>(base);
      
      Type transSize = doIt(bfaTy->getSize(),
                            TypePosition::Invariant);
      if (!transSize) {
        return Type();
      }
      
      Type transElement = doIt(bfaTy->getElementType(),
                               TypePosition::Invariant);
      if (!transElement) {
        return Type();
      }
      
      CanType canTransSize = transSize->getCanonicalType();
      CanType canTransElement = transElement->getCanonicalType();
      if (canTransSize != bfaTy->getSize()
          || canTransElement != bfaTy->getElementType()) {
        return BuiltinFixedArrayType::get(canTransSize, canTransElement);
      }
      
      return bfaTy;
    }

    case TypeKind::PrimaryArchetype:
    case TypeKind::PackArchetype: {
      auto *archetype = cast<ArchetypeType>(base);
      return asDerived().transformPrimaryArchetypeType(archetype, pos);
    }

    case TypeKind::OpaqueTypeArchetype: {
      auto *opaque = cast<OpaqueTypeArchetypeType>(base);
      if (auto result = asDerived().transformOpaqueTypeArchetypeType(opaque, pos))
        return *result;

      auto subMap = opaque->getSubstitutions();
      auto newSubMap = asDerived().transformSubstitutionMap(subMap);
      if (newSubMap == subMap)
        return t;
      if (!newSubMap)
        return Type();

      return OpaqueTypeArchetypeType::get(opaque->getDecl(),
                                          opaque->getInterfaceType(),
                                          newSubMap);
    }

    case TypeKind::ExistentialArchetype: {
      auto *local = cast<LocalArchetypeType>(base);
      if (auto result = asDerived().transformLocalArchetypeType(local, pos))
        return *result;

      auto *env = local->getGenericEnvironment();

      auto genericSig = env->getGenericSignature();
      auto existentialTy = env->getOpenedExistentialType();
      auto subMap = env->getOuterSubstitutions();
      auto uuid = env->getOpenedExistentialUUID();

      auto newSubMap = asDerived().transformSubstitutionMap(subMap);
      if (newSubMap == subMap)
        return t;
      if (!newSubMap)
        return Type();

      auto *newEnv = GenericEnvironment::forOpenedExistential(
          genericSig, existentialTy, newSubMap, uuid);
      return newEnv->mapTypeIntoContext(local->getInterfaceType());
    }

    case TypeKind::ElementArchetype: {
      auto *local = cast<LocalArchetypeType>(base);
      if (auto result = asDerived().transformLocalArchetypeType(local, pos))
        return *result;

      return local;
    }

    case TypeKind::GenericTypeParam: {
      auto *param = cast<GenericTypeParamType>(base);
      return asDerived().transformGenericTypeParamType(param, pos);
    }

    case TypeKind::Enum:
    case TypeKind::Struct:
    case TypeKind::Class:
    case TypeKind::Protocol: {
      auto nominalTy = cast<NominalType>(base);
      if (auto parentTy = nominalTy->getParent()) {
        parentTy = doIt(parentTy, pos);
        if (!parentTy)
          return Type();

        if (parentTy.getPointer() == nominalTy->getParent().getPointer())
          return t;

        return NominalType::get(nominalTy->getDecl(), parentTy, ctx);
      }

      return t;
    }

    case TypeKind::SILBlockStorage: {
      auto storageTy = cast<SILBlockStorageType>(base);
      Type transCap = doIt(storageTy->getCaptureType(),
                           TypePosition::Invariant);
      if (!transCap)
        return Type();
      CanType canTransCap = transCap->getCanonicalType();
      if (canTransCap != storageTy->getCaptureType())
        return SILBlockStorageType::get(canTransCap);
      return storageTy;
    }

    case TypeKind::SILMoveOnlyWrapped: {
      auto *storageTy = cast<SILMoveOnlyWrappedType>(base);
      Type transCap = doIt(storageTy->getInnerType(),
                           TypePosition::Invariant);
      if (!transCap)
        return Type();
      CanType canTransCap = transCap->getCanonicalType();
      if (canTransCap != storageTy->getInnerType())
        return SILMoveOnlyWrappedType::get(canTransCap);
      return storageTy;
    }

    case TypeKind::SILBox: {
      bool changed = false;
      auto boxTy = cast<SILBoxType>(base);

      SmallVector<SILField, 4> newFields;
      auto *l = boxTy->getLayout();
      for (auto f : l->getFields()) {
        auto fieldTy = f.getLoweredType();
        auto transformed = asDerived().transformSILField(
          fieldTy, TypePosition::Invariant);
        changed |= fieldTy != transformed;
        newFields.push_back(SILField(transformed, f.isMutable()));
      }

      auto oldSubMap = boxTy->getSubstitutions();
      auto newSubMap = asDerived().transformSubstitutionMap(oldSubMap);
      if (oldSubMap && !newSubMap)
        return Type();
      changed |= (oldSubMap != newSubMap);
      if (!changed)
        return t;
      boxTy = SILBoxType::get(ctx,
                              SILLayout::get(ctx,
                                             l->getGenericSignature(),
                                             newFields,
                                             l->capturesGenericEnvironment()),
                              newSubMap);
      return boxTy;
    }

    case TypeKind::SILFunction: {
      auto fnTy = cast<SILFunctionType>(base);

      if (fnTy->isPolymorphic())
        return fnTy;

      auto updateSubs = [&](SubstitutionMap &subs) -> bool {
        auto newSubs = asDerived().transformSubstitutionMap(subs);
        if (subs && !newSubs)
          return false;
        if (subs == newSubs)
          return false;

        subs = newSubs;
        return true;
      };

      if (auto subs = fnTy->getInvocationSubstitutions()) {
        if (updateSubs(subs)) {
          return fnTy->withInvocationSubstitutions(subs);
        }
        return fnTy;
      }

      if (auto subs = fnTy->getPatternSubstitutions()) {
        if (updateSubs(subs)) {
          return fnTy->withPatternSubstitutions(subs);
        }
        return fnTy;
      }

      bool changed = false;

      SmallVector<SILParameterInfo, 8> transInterfaceParams;
      for (SILParameterInfo param : fnTy->getParameters()) {
        if (transformSILParameter(pos.flipped(), param, changed))
          return Type();
        transInterfaceParams.push_back(param);
      }

      SmallVector<SILYieldInfo, 8> transInterfaceYields;
      for (SILYieldInfo yield : fnTy->getYields()) {
        if (transformSILYield(pos, yield, changed)) return Type();
        transInterfaceYields.push_back(yield);
      }

      SmallVector<SILResultInfo, 8> transInterfaceResults;
      for (SILResultInfo result : fnTy->getResults()) {
        if (transformSILResult(pos, result, changed)) return Type();
        transInterfaceResults.push_back(result);
      }

      std::optional<SILResultInfo> transErrorResult;
      if (fnTy->hasErrorResult()) {
        SILResultInfo result = fnTy->getErrorResult();
        if (transformSILResult(pos, result, changed)) return Type();
        transErrorResult = result;
      }

      if (!changed) {
        return t;
      }
      
      // Lifetime dependencies get eliminated if their target type was
      // substituted with an escapable type.
      auto extInfo = fnTy->getExtInfo();
      if (!extInfo.getLifetimeDependencies().empty()) {
        SmallVector<LifetimeDependenceInfo, 2> substDependenceInfos;
        bool didRemoveLifetimeDependencies
          = filterEscapableLifetimeDependencies(GenericSignature(),
                                              extInfo.getLifetimeDependencies(),
                                              substDependenceInfos,
                                              [&](unsigned targetIndex) {
            if (targetIndex >= transInterfaceParams.size()) {
              // Target is a return type.
              return transInterfaceResults[targetIndex - transInterfaceParams.size()]
                .getInterfaceType();
            } else {
              // Target is a parameter.
              return transInterfaceParams[targetIndex].getInterfaceType();
            }
          });
        if (didRemoveLifetimeDependencies) {
          extInfo = extInfo.withLifetimeDependencies(
              ctx.AllocateCopy(substDependenceInfos));
        }
      }

      return SILFunctionType::get(
          fnTy->getInvocationGenericSignature(),
          extInfo,
          fnTy->getCoroutineKind(),
          fnTy->getCalleeConvention(),
          transInterfaceParams,
          transInterfaceYields,
          transInterfaceResults,
          transErrorResult,
          SubstitutionMap(),
          SubstitutionMap(),
          ctx,
          fnTy->getWitnessMethodConformanceOrInvalid());
    }

  #define REF_STORAGE(Name, ...) \
    case TypeKind::Name##Storage:
  #include "swift/AST/ReferenceStorage.def"
    {
      auto storageTy = cast<ReferenceStorageType>(base);
      Type refTy = storageTy->getReferentType();
      Type substRefTy = doIt(refTy, pos);
      if (!substRefTy)
        return Type();

      if (substRefTy.getPointer() == refTy.getPointer())
        return t;

      return ReferenceStorageType::get(substRefTy, storageTy->getOwnership(),
                                       ctx);
    }

    case TypeKind::UnboundGeneric: {
      auto unbound = cast<UnboundGenericType>(base);
      Type substParentTy;
      if (auto parentTy = unbound->getParent()) {
        substParentTy = doIt(parentTy, pos);
        if (!substParentTy)
          return Type();

        if (substParentTy.getPointer() == parentTy.getPointer())
          return t;

        return UnboundGenericType::get(unbound->getDecl(), substParentTy,
                                       ctx);
      }

      return t;
    }

    case TypeKind::BoundGenericClass:
    case TypeKind::BoundGenericEnum:
    case TypeKind::BoundGenericStruct: {
      auto bound = cast<BoundGenericType>(base);
      SmallVector<Type, 4> substArgs;
      bool anyChanged = false;
      Type substParentTy;
      if (auto parentTy = bound->getParent()) {
        substParentTy = doIt(parentTy, pos);
        if (!substParentTy)
          return Type();

        if (substParentTy.getPointer() != parentTy.getPointer())
          anyChanged = true;
      }

      const auto transformGenArg = [&](Type arg, TypePosition p) -> bool {
        Type substArg = doIt(arg, p);
        if (!substArg)
          return true;
        substArgs.push_back(substArg);
        if (substArg.getPointer() != arg.getPointer())
          anyChanged = true;

        return false;
      };

      if (bound->isArray() || bound->isOptional()) {
        // Swift.Array preserves variance in its 'Value' type.
        // Swift.Optional preserves variance in its 'Wrapped' type.
        if (transformGenArg(bound->getGenericArgs().front(), pos))
          return Type();
      } else if (bound->isDictionary()) {
        // Swift.Dictionary preserves variance in its 'Element' type.
        if (transformGenArg(bound->getGenericArgs().front(),
                            TypePosition::Invariant) ||
            transformGenArg(bound->getGenericArgs().back(), pos))
          return Type();
      } else {
        for (auto arg : bound->getGenericArgs()) {
          if (transformGenArg(arg, TypePosition::Invariant))
            return Type();
        }
      }

      if (!anyChanged)
        return t;

      return BoundGenericType::get(bound->getDecl(), substParentTy, substArgs);
    }

    case TypeKind::ExistentialMetatype: {
      auto meta = cast<ExistentialMetatypeType>(base);
      auto instanceTy = doIt(meta->getInstanceType(), pos);
      if (!instanceTy)
        return Type();

      if (instanceTy.getPointer() == meta->getInstanceType().getPointer())
        return t;

      if (meta->hasRepresentation())
        return ExistentialMetatypeType::get(instanceTy,
                                            meta->getRepresentation());
      return ExistentialMetatypeType::get(instanceTy);
    }

    case TypeKind::Metatype: {
      auto meta = cast<MetatypeType>(base);
      auto instanceTy = doIt(meta->getInstanceType(), pos);
      if (!instanceTy)
        return Type();

      if (instanceTy.getPointer() == meta->getInstanceType().getPointer())
        return t;

      if (meta->hasRepresentation())
        return MetatypeType::get(instanceTy, meta->getRepresentation());
      return MetatypeType::get(instanceTy);
    }

    case TypeKind::DynamicSelf: {
      auto dynamicSelf = cast<DynamicSelfType>(base);
      auto selfTy = doIt(dynamicSelf->getSelfType(), pos);
      if (!selfTy)
        return Type();

      if (selfTy.getPointer() == dynamicSelf->getSelfType().getPointer())
        return t;

      return DynamicSelfType::get(selfTy, ctx);
    }

    case TypeKind::TypeAlias: {
      auto alias = cast<TypeAliasType>(base);
      Type oldUnderlyingTy = Type(alias->getSinglyDesugaredType());
      Type newUnderlyingTy = doIt(oldUnderlyingTy, pos);
      if (!newUnderlyingTy) return Type();

      Type oldParentType = alias->getParent();
      Type newParentType;
      if (oldParentType) {
        newParentType = doIt(oldParentType, pos);
        if (!newParentType) return newUnderlyingTy;
      }

      if (newParentType && newParentType->isExistentialType())
        return newUnderlyingTy;

      RecursiveTypeProperties substProps;
      if (newParentType)
        substProps = newParentType->getRecursiveProperties();

      SmallVector<Type, 4> substArgs;
      bool anyChanged = false;

      const auto transformGenArg = [&](Type arg) -> bool {
        Type substArg = doIt(arg, TypePosition::Invariant);
        if (!substArg)
          return true;
        substProps |= substArg->getRecursiveProperties();
        substArgs.push_back(substArg);
        if (substArg.getPointer() != arg.getPointer())
          anyChanged = true;

        return false;
      };

      for (auto arg : alias->getDirectGenericArgs()) {
        if (transformGenArg(arg))
          return Type();
      }

      if (oldParentType.getPointer() == newParentType.getPointer() &&
          oldUnderlyingTy.getPointer() == newUnderlyingTy.getPointer() &&
          !anyChanged)
        return t;

      // We leave the old behavior behind for ConstraintSystem::openType(), where
      // preserving sugar introduces a performance penalty.
      if (asDerived().shouldDesugarTypeAliases())
        return newUnderlyingTy;

      // Don't leave local archetypes and type variables behind in sugar
      // if they don't appear in the underlying type, to avoid confusion.
      if (substProps.hasLocalArchetype() != newUnderlyingTy->hasLocalArchetype())
        return newUnderlyingTy;
      if (substProps.hasTypeVariable() != newUnderlyingTy->hasTypeVariable())
        return newUnderlyingTy;

      return TypeAliasType::get(alias->getDecl(), newParentType, substArgs,
                                newUnderlyingTy);
    }

    case TypeKind::Locatable: {
      auto locatable = cast<LocatableType>(base);
      Type oldUnderlyingTy = Type(locatable->getSinglyDesugaredType());
      Type newUnderlyingTy = doIt(oldUnderlyingTy, pos);
      if (!newUnderlyingTy)
        return Type();

      if (oldUnderlyingTy.getPointer() == newUnderlyingTy.getPointer())
        return t;

      return LocatableType::get(locatable->getLoc(), newUnderlyingTy);
    }

    case TypeKind::ErrorUnion: {
      auto errorUnion = cast<ErrorUnionType>(base);
      bool anyChanged = false;
      SmallVector<Type, 4> terms;
      unsigned Index = 0;
      for (Type term : errorUnion->getTerms()) {
        Type transformedTerm = doIt(term, TypePosition::Invariant);
        if (!transformedTerm)
          return Type();

        // If nothing has changed, just keep going.
        if (!anyChanged &&
            transformedTerm.getPointer() == term.getPointer()) {
          ++Index;
          continue;
        }

        // If this is the first change we've seen, copy all of the previous
        // elements.
        if (!anyChanged) {
          // Copy all of the previous elements.
          terms.append(errorUnion->getTerms().begin(),
                       errorUnion->getTerms().begin() + Index);
          anyChanged = true;
        }

        // If the transformed type is a pack, immediately expand it.
        if (auto termPack = getTransformedPack(transformedTerm)) {
          auto termElements = termPack->getElementTypes();
          terms.append(termElements.begin(), termElements.end());
        } else {
          terms.push_back(transformedTerm);
        }
      }

      if (!anyChanged)
        return t;

      return ErrorUnionType::get(ctx, terms);
    }

    case TypeKind::Pack: {
      auto pack = cast<PackType>(base);
      bool anyChanged = false;
      SmallVector<Type, 4> elements;
      unsigned Index = 0;
      for (Type eltTy : pack->getElementTypes()) {
        Type transformedEltTy = doIt(eltTy, TypePosition::Invariant);
        if (!transformedEltTy)
          return Type();

        // If nothing has changed, just keep going.
        if (!anyChanged &&
            transformedEltTy.getPointer() == eltTy.getPointer()) {
          ++Index;
          continue;
        }

        // If this is the first change we've seen, copy all of the previous
        // elements.
        if (!anyChanged) {
          // Copy all of the previous elements.
          elements.append(pack->getElementTypes().begin(),
                          pack->getElementTypes().begin() + Index);
          anyChanged = true;
        }

        // If the transformed type is a pack, immediately expand it.
        if (auto eltPack = getTransformedPack(transformedEltTy)) {
          auto eltElements = eltPack->getElementTypes();
          elements.append(eltElements.begin(), eltElements.end());
        } else {
          elements.push_back(transformedEltTy);
        }
      }

      if (!anyChanged)
        return t;

      return PackType::get(ctx, elements);
    }

    case TypeKind::SILPack: {
      auto pack = cast<SILPackType>(base);
      bool anyChanged = false;
      SmallVector<CanType, 4> elements;
      unsigned Index = 0;
      for (Type eltTy : pack->getElementTypes()) {
        Type transformedEltTy = doIt(eltTy, TypePosition::Invariant);
        if (!transformedEltTy)
          return Type();

        // If nothing has changed, just keep going.
        if (!anyChanged &&
            transformedEltTy.getPointer() == eltTy.getPointer()) {
          ++Index;
          continue;
        }

        // If this is the first change we've seen, copy all of the previous
        // elements.
        if (!anyChanged) {
          // Copy all of the previous elements.
          elements.append(pack->getElementTypes().begin(),
                          pack->getElementTypes().begin() + Index);
          anyChanged = true;
        }

        auto transformedEltCanTy = transformedEltTy->getCanonicalType();

        // Flatten immediately.
        if (auto transformedEltPack =
              dyn_cast<SILPackType>(transformedEltCanTy)) {
          auto elementElements = transformedEltPack->getElementTypes();
          elements.append(elementElements.begin(), elementElements.end());
        } else {
          assert(!isa<PackType>(transformedEltCanTy));
          elements.push_back(transformedEltCanTy);
        }
      }

      if (!anyChanged)
        return t;

      return SILPackType::get(ctx, pack->getExtInfo(), elements);
    }

    case TypeKind::PackExpansion: {
      auto *expand = cast<PackExpansionType>(base);
      return asDerived().transformPackExpansionType(expand, pos);
    }

    case TypeKind::PackElement: {
      auto element = cast<PackElementType>(base);
      return asDerived().transformPackElementType(element, pos);
    }

    case TypeKind::Tuple: {
      auto tuple = cast<TupleType>(base);
      bool anyChanged = false;
      SmallVector<TupleTypeElt, 4> elements;
      unsigned Index = 0;
      for (const auto &elt : tuple->getElements()) {
        Type eltTy = elt.getType();
        Type transformedEltTy = doIt(eltTy, pos);
        if (!transformedEltTy)
          return Type();

        // If nothing has changed, just keep going.
        if (!anyChanged &&
            transformedEltTy.getPointer() == elt.getType().getPointer()) {
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

        // Add the new tuple element, with the transformed type.
        // Expand packs immediately.
        if (auto eltPack = getTransformedPack(transformedEltTy)) {
          bool first = true;
          for (auto eltElement : eltPack->getElementTypes()) {
            if (first) {
              elements.push_back(elt.getWithType(eltElement));
              first = false;
            } else {
              elements.push_back(TupleTypeElt(eltElement));
            }
          }
        } else {
          elements.push_back(elt.getWithType(transformedEltTy));
        }
      }

      if (!anyChanged)
        return t;

      if (asDerived().shouldUnwrapVanishingTuples()) {
        // Handle vanishing tuples -- If the transform would yield a singleton
        // tuple, and we didn't start with one, flatten to produce the
        // element type.
        if (elements.size() == 1 &&
            !elements[0].getType()->is<PackExpansionType>() &&
            !(tuple->getNumElements() == 1 &&
              !tuple->getElementType(0)->is<PackExpansionType>())) {
          return elements[0].getType();
        }
      }

      return TupleType::get(elements, ctx);
    }


    case TypeKind::DependentMember: {
      auto dependent = cast<DependentMemberType>(base);
      return asDerived().transformDependentMemberType(dependent, pos);
    }

    case TypeKind::GenericFunction:
    case TypeKind::Function: {
      auto function = cast<AnyFunctionType>(base);

      bool isUnchanged = true;

      // Transform function parameter types.
      SmallVector<AnyFunctionType::Param, 8> substParams;
      for (auto param : function->getParams()) {
        auto type = param.getPlainType();
        auto label = param.getLabel();
        auto flags = param.getParameterFlags();
        auto internalLabel = param.getInternalLabel();

        TypePosition paramPos = pos.flipped();
        if (param.isInOut())
          paramPos = TypePosition::Invariant;

        Type substType = doIt(type, paramPos);
        if (!substType)
          return Type();

        if (type.getPointer() != substType.getPointer())
          isUnchanged = false;

        // FIXME: Remove this once we get rid of TVO_CanBindToInOut;
        // the only time we end up here is when the constraint solver
        // simplifies a type containing a type variable fixed to an
        // InOutType.
        if (substType->is<InOutType>()) {
          assert(flags.getValueOwnership() == ValueOwnership::Default);
          substType = substType->getInOutObjectType();
          flags = flags.withInOut(true);
        }

        if (auto substPack = getTransformedPack(substType)) {
          bool first = true;
          for (auto substEltType : substPack->getElementTypes()) {
            if (first) {
              substParams.emplace_back(substEltType, label, flags,
                                       internalLabel);
              first = false;
            } else {
              substParams.emplace_back(substEltType, Identifier(), flags,
                                       Identifier());
            }
          }
        } else {
          substParams.emplace_back(substType, label, flags, internalLabel);
        }
      }

      // Transform result type.
      Type resultTy = doIt(function->getResult(), pos);
      if (!resultTy)
        return Type();

      if (resultTy.getPointer() != function->getResult().getPointer())
        isUnchanged = false;

      // Transform the extended info.
      std::optional<ASTExtInfo> extInfo;
      if (function->hasExtInfo()) {
        auto origExtInfo = function->getExtInfo();
        extInfo = origExtInfo;

        // Transform the thrown error.
        if (Type origThrownError = origExtInfo.getThrownError()) {
          Type thrownError = doIt(origThrownError, pos);
          if (!thrownError)
            return Type();

          if (thrownError.getPointer() != origThrownError.getPointer())
            isUnchanged = false;

          extInfo = extInfo->withThrows(true, thrownError);

          // If there was a generic thrown error and it substituted with
          // 'any Error' or 'Never', map to 'throws' or non-throwing rather than
          // maintaining the sugar.
          if (origThrownError->isTypeParameter() ||
              origThrownError->isTypeVariableOrMember()) {
            // 'any Error'
            if (thrownError->isEqual(ctx.getErrorExistentialType())) {
              extInfo = extInfo->withThrows(true, Type());
            } else if (thrownError->isNever()) {
              extInfo = extInfo->withThrows(false, Type());
            }
          }
        }

        // Transform the global actor.
        if (Type origGlobalActorType = origExtInfo.getGlobalActor()) {
          Type globalActorType = doIt(origGlobalActorType, TypePosition::Invariant);
          if (!globalActorType)
            return Type();

          if (globalActorType.getPointer() != origGlobalActorType.getPointer())
            isUnchanged = false;

          extInfo = extInfo->withGlobalActor(globalActorType);
        }
      }

      if (auto genericFnType = dyn_cast<GenericFunctionType>(base)) {
  #ifndef NDEBUG
        // Check that generic parameters won't be transformed.
        // Transform generic parameters.
        for (auto param : genericFnType->getGenericParams()) {
          assert(doIt(Type(param), TypePosition::Invariant)->isEqual(param) &&
                 "GenericFunctionType transform() changes type parameter");
        }
  #endif

        if (isUnchanged) return t;

        auto genericSig = genericFnType->getGenericSignature();
        return GenericFunctionType::get(
            genericSig, substParams, resultTy, extInfo);
      }
      
      if (isUnchanged) {
        return t;
      }

      // Substitution may have replaced parameter or return types that had
      // lifetime dependencies with Escapable types, which render those
      // dependencies inactive.
      if (extInfo && !extInfo->getLifetimeDependencies().empty()) {
        SmallVector<LifetimeDependenceInfo, 2> substDependenceInfos;
        bool didRemoveLifetimeDependencies
          = filterEscapableLifetimeDependencies(GenericSignature(),
                                                extInfo->getLifetimeDependencies(),
                                                substDependenceInfos,
                                                [&](unsigned targetIndex) {
            // Traverse potentially-curried function types.
            ArrayRef<AnyFunctionType::Param> params = substParams;
            auto result = resultTy;
            while (targetIndex >= params.size()) {
              if (auto curriedTy = result->getAs<AnyFunctionType>()) {
                targetIndex -= params.size();
                params = curriedTy->getParams();
                result = curriedTy->getResult();
                continue;
              } else {
                // The last lifetime dependency targets the result at the end
                // of the curried chain.
                ASSERT(targetIndex == params.size()
                       && "invalid lifetime dependence target");
                return result;
              }
            }
            return params[targetIndex].getParameterType();
          });

        if (didRemoveLifetimeDependencies) {
          extInfo = extInfo->withLifetimeDependencies(
              ctx.AllocateCopy(substDependenceInfos));
        }
      }

      return FunctionType::get(substParams, resultTy, extInfo);
    }

    case TypeKind::ArraySlice: {
      auto slice = cast<ArraySliceType>(base);
      auto baseTy = doIt(slice->getBaseType(), pos);
      if (!baseTy)
        return Type();

      if (baseTy.getPointer() == slice->getBaseType().getPointer())
        return t;

      return ArraySliceType::get(baseTy);
    }

    case TypeKind::InlineArray: {
      auto ty = cast<InlineArrayType>(base);
      auto countTy = doIt(ty->getCountType(), TypePosition::Invariant);
      if (!countTy)
        return Type();

      // Currently the element type is invariant for InlineArray.
      // FIXME: Should we allow covariance?
      auto eltTy = doIt(ty->getElementType(), TypePosition::Invariant);
      if (!eltTy)
        return Type();

      if (countTy.getPointer() == ty->getCountType().getPointer() &&
          eltTy.getPointer() == ty->getElementType().getPointer())
        return t;

      return InlineArrayType::get(countTy, eltTy);
    }

    case TypeKind::Optional: {
      auto optional = cast<OptionalType>(base);
      auto baseTy = doIt(optional->getBaseType(), pos);
      if (!baseTy)
        return Type();

      if (baseTy.getPointer() == optional->getBaseType().getPointer())
        return t;

      return OptionalType::get(baseTy);
    }

    case TypeKind::VariadicSequence: {
      auto seq = cast<VariadicSequenceType>(base);
      auto baseTy = doIt(seq->getBaseType(), pos);
      if (!baseTy)
        return Type();

      if (baseTy.getPointer() == seq->getBaseType().getPointer())
        return t;

      return VariadicSequenceType::get(baseTy);
    }

    case TypeKind::Dictionary: {
      auto dict = cast<DictionaryType>(base);
      auto keyTy = doIt(dict->getKeyType(), TypePosition::Invariant);
      if (!keyTy)
        return Type();

      auto valueTy = doIt(dict->getValueType(), pos);
      if (!valueTy)
        return Type();

      if (keyTy.getPointer() == dict->getKeyType().getPointer() &&
          valueTy.getPointer() == dict->getValueType().getPointer())
        return t;

      return DictionaryType::get(keyTy, valueTy);
    }

    case TypeKind::LValue: {
      auto lvalue = cast<LValueType>(base);
      auto objectTy = doIt(lvalue->getObjectType(), TypePosition::Invariant);
      if (!objectTy || objectTy->hasError())
        return objectTy;

      return objectTy.getPointer() == lvalue->getObjectType().getPointer() ?
        t : LValueType::get(objectTy);
    }

    case TypeKind::InOut: {
      auto inout = cast<InOutType>(base);
      auto objectTy = doIt(inout->getObjectType(), TypePosition::Invariant);
      if (!objectTy || objectTy->hasError())
        return objectTy;

      return objectTy.getPointer() == inout->getObjectType().getPointer() ?
        t : InOutType::get(objectTy);
    }

    case TypeKind::Existential: {
      auto *existential = cast<ExistentialType>(base);
      auto constraint = doIt(existential->getConstraintType(), pos);
      if (!constraint || constraint->hasError())
        return constraint;

      if (constraint.getPointer() ==
          existential->getConstraintType().getPointer())
        return t;

      return ExistentialType::get(constraint);
    }

    case TypeKind::ProtocolComposition: {
      auto pc = cast<ProtocolCompositionType>(base);
      SmallVector<Type, 4> substMembers;
      auto members = pc->getMembers();
      bool anyChanged = false;
      for (auto member : members) {
        auto substMember = doIt(member, pos);
        if (!substMember)
          return Type();

        substMembers.push_back(substMember);

        if (substMember.getPointer() != member.getPointer())
          anyChanged = true;
      }

      if (!anyChanged)
        return t;

      return ProtocolCompositionType::get(ctx,
                                          substMembers,
                                          pc->getInverses(),
                                          pc->hasExplicitAnyObject());
    }

    case TypeKind::ParameterizedProtocol: {
      auto *ppt = cast<ParameterizedProtocolType>(base);
      Type base = ppt->getBaseType();

      bool anyChanged = false;

      Type substBase = doIt(base, pos);
      if (!substBase)
        return Type();

      if (substBase.getPointer() != base.getPointer())
        anyChanged = true;

      SmallVector<Type, 2> substArgs;
      for (auto arg : ppt->getArgs()) {
        auto substArg = doIt(arg, TypePosition::Invariant);
        if (!substArg)
          return Type();

        substArgs.push_back(substArg);

        if (substArg.getPointer() != arg.getPointer())
          anyChanged = true;
      }

      if (!anyChanged)
        return t;

      return ParameterizedProtocolType::get(
          ctx,
          substBase->castTo<ProtocolType>(),
          substArgs);
    }
    }

    llvm_unreachable("Unhandled type in transformation");
  }

  // If original was non-empty and transformed is empty, we're
  // signaling failure, that is, a Type() return from doIt().
  SubstitutionMap transformSubstitutionMap(SubstitutionMap subs) {
    if (subs.empty())
      return subs;

    SmallVector<Type, 4> newSubs;
    bool anyChanged = false;
    for (auto replacement : subs.getReplacementTypes()) {
      Type newReplacement = doIt(replacement, TypePosition::Invariant);
      if (!newReplacement)
        return SubstitutionMap();
      newSubs.push_back(newReplacement);
      if (replacement.getPointer() != newReplacement.getPointer())
        anyChanged = true;
    }

    if (!anyChanged)
      return subs;

    auto sig = subs.getGenericSignature();
    return SubstitutionMap::get(sig, newSubs, LookUpConformanceInModule());
  }

  bool shouldUnwrapVanishingTuples() const { return true; }

  bool shouldDesugarTypeAliases() const { return false; }

  CanType transformSILField(CanType fieldTy, TypePosition pos) {
    return doIt(fieldTy, pos)->getCanonicalType();
  }

  Type transformGenericTypeParamType(GenericTypeParamType *param, TypePosition pos) {
    return param;
  }

  Type transformPackExpansionType(PackExpansionType *expand, TypePosition pos) {
    // Substitution completely replaces this.

    Type transformedPat = doIt(expand->getPatternType(), pos);
    if (!transformedPat)
      return Type();

    Type transformedCount = doIt(expand->getCountType(), TypePosition::Shape);
    if (!transformedCount)
      return Type();

    if (transformedPat.getPointer() == expand->getPatternType().getPointer() &&
        transformedCount.getPointer() == expand->getCountType().getPointer())
      return expand;

    return PackExpansionType::get(transformedPat, transformedCount);
  }

  Type transformPackElementType(PackElementType *element, TypePosition pos) {
    Type transformedPack = doIt(element->getPackType(), pos);
    if (!transformedPack)
      return Type();

    if (transformedPack.getPointer() == element->getPackType().getPointer())
      return element;

    return PackElementType::get(transformedPack, element->getLevel());
  }

  Type transformDependentMemberType(DependentMemberType *dependent, TypePosition pos) {
    auto dependentBase = doIt(dependent->getBase(), pos);
    if (!dependentBase)
      return Type();

    if (dependentBase.getPointer() == dependent->getBase().getPointer())
      return dependent;

    if (auto assocType = dependent->getAssocType())
      return DependentMemberType::get(dependentBase, assocType);

    return DependentMemberType::get(dependentBase, dependent->getName());
  }

  Type transformPrimaryArchetypeType(ArchetypeType *primary,
                                     TypePosition pos) {
    return primary;
  }

  std::optional<Type> transformOpaqueTypeArchetypeType(OpaqueTypeArchetypeType *opaque,
                                                       TypePosition pos) {
    return std::nullopt;
  }

  std::optional<Type> transformLocalArchetypeType(LocalArchetypeType *opaque,
                                                  TypePosition pos) {
    return std::nullopt;
  }
};

}

#endif
