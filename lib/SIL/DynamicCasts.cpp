//===--- DynamicCasts.cpp - Utilities for dynamic casts -------------------===//
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

#include "swift/AST/Types.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;

static unsigned getAnyMetatypeDepth(CanType type) {
  unsigned depth = 0;
  while (auto metatype = dyn_cast<AnyMetatypeType>(type)) {
    type = metatype.getInstanceType();
    depth++;
  }
  return depth;
}

static bool
mayBridgeToObjectiveC(ModuleDecl *M, CanType T) {
  // FIXME: Disable when we don't support Objective-C interoperability?
  return true;
}

static bool
mustBridgeToSwiftValueBox(ModuleDecl *M, CanType T) {
  // If the target type is either an unknown dynamic type, or statically
  // known to bridge, the cast may succeed.
  if (T->hasArchetype())
    return false;

  if (T->isAnyExistentialType())
    return false;

  // getBridgedToObjC() might return a null-type for some types
  // whose bridging implementation is allowed to live elsewhere. Exclude this
  // case here.
  if (auto N = T->getAnyNominal())
    if (M->getASTContext().isTypeBridgedInExternalModule(N))
      return false;

  return !M->getASTContext().getBridgedToObjC(M, T);
}

static bool canClassOrSuperclassesHaveExtensions(ClassDecl *CD,
                                                 bool isWholeModuleOpts) {
  while (CD) {
    // Open classes can always be extended
    if (CD->getEffectiveAccess() == Accessibility::Open)
      return true;

    // Internal and public classes can be extended, if we are not in
    // whole-module-optimization mode.
    if (CD->getEffectiveAccess() >= Accessibility::Internal &&
        !isWholeModuleOpts)
      return true;

    if (!CD->hasSuperclass())
      break;

    CD = CD->getSuperclass()->getClassOrBoundGenericClass();
  }

  return false;
}

/// Try to classify a conversion from non-existential type
/// into an existential type by performing a static check
/// of protocol conformances if it is possible.
static DynamicCastFeasibility
classifyDynamicCastToProtocol(CanType source,
                              CanType target,
                              bool isWholeModuleOpts) {
  assert(target.isExistentialType() &&
         "target should be an existential type");

  if (source == target)
    return DynamicCastFeasibility::WillSucceed;

  auto *SourceNominalTy = source.getAnyNominal();

  if (!SourceNominalTy)
    return DynamicCastFeasibility::MaySucceed;

  auto *TargetProtocol = target.getAnyNominal();
  if (!TargetProtocol)
    return DynamicCastFeasibility::MaySucceed;

  auto SourceProtocols = SourceNominalTy->getAllProtocols();

  // Check all protocols implemented by the type.
  for (auto *Protocol : SourceProtocols) {
    if (Protocol == TargetProtocol)
      return DynamicCastFeasibility::WillSucceed;
  }

  // If we are casting a protocol, then the cast will fail
  // as we have not found any conformances and protocols cannot
  // be extended currently.
  // NOTE: If we allow protocol extensions in the future, this
  // conditional statement should be removed.
  if (isa<ProtocolType>(source)) {
    return DynamicCastFeasibility::WillFail;
  }

  // If it is a class and it can be proven that this class and its
  // superclasses cannot be extended, then it is safe to proceed.
  // No need to check this for structs, as they do not have any
  // superclasses.
  if (auto *CD = source.getClassOrBoundGenericClass()) {
    if (canClassOrSuperclassesHaveExtensions(CD, isWholeModuleOpts))
      return DynamicCastFeasibility::MaySucceed;
    // Derived types may conform to the protocol.
    if (!CD->isFinal()) {
      // TODO: If it is a private type or internal type and we
      // can prove that there are no derived types conforming to a
      // protocol, then we can still return WillFail.
      return DynamicCastFeasibility::MaySucceed;
    }
  }

  // If the source type is file-private or target protocol is file-private,
  // then conformances cannot be changed at run-time, because only this
  // file could have implemented them, but no conformances were found.
  // Therefore it is safe to make a negative decision at compile-time.
  if (SourceNominalTy->getEffectiveAccess() <= Accessibility::FilePrivate ||
      TargetProtocol->getEffectiveAccess() <= Accessibility::FilePrivate) {
    // This cast is always false. Replace it with a branch to the
    // failure block.
    return DynamicCastFeasibility::WillFail;
  }

  // If we are in a whole-module compilation and
  // if the source type is internal or target protocol is internal,
  // then conformances cannot be changed at run-time, because only this
  // module could have implemented them, but no conformances were found.
  // Therefore it is safe to make a negative decision at compile-time.
  if (isWholeModuleOpts &&
      (SourceNominalTy->getEffectiveAccess() <= Accessibility::Internal ||
       TargetProtocol->getEffectiveAccess() <= Accessibility::Internal)) {
    return DynamicCastFeasibility::WillFail;
  }

  return DynamicCastFeasibility::MaySucceed;
}

static DynamicCastFeasibility
classifyDynamicCastFromProtocol(ModuleDecl *M, CanType source, CanType target,
                                bool isWholeModuleOpts) {
  assert(source.isExistentialType() &&
         "source should be an existential type");

  if (source == target)
    return DynamicCastFeasibility::WillSucceed;

  // Casts from class existential into a non-class can never succeed.
  if (source->isClassExistentialType() &&
      !target.isAnyExistentialType() &&
      !target.getClassOrBoundGenericClass() &&
      !isa<ArchetypeType>(target) &&
      !mayBridgeToObjectiveC(M, target)) {
    assert((target.getEnumOrBoundGenericEnum() ||
            target.getStructOrBoundGenericStruct() ||
            isa<TupleType>(target) ||
            isa<SILFunctionType>(target) ||
            isa<FunctionType>(target) ||
            isa<MetatypeType>(target)) &&
           "Target should be an enum, struct, tuple, metatype or function type");
    return DynamicCastFeasibility::WillFail;
  }

  // TODO: maybe prove that certain conformances are impossible?

  return DynamicCastFeasibility::MaySucceed;
}

/// Returns the existential type associated with the Hashable
/// protocol, if it can be found.
static CanType getHashableExistentialType(ModuleDecl *M) {
  auto hashable =
    M->getASTContext().getProtocol(KnownProtocolKind::Hashable);
  if (!hashable) return CanType();
  return hashable->getDeclaredType()->getCanonicalType();
}

/// Check if a given type conforms to _BridgedToObjectiveC protocol.
bool swift::isObjectiveCBridgeable(ModuleDecl *M, CanType Ty) {
  // Retrieve the _BridgedToObjectiveC protocol.
  auto bridgedProto =
      M->getASTContext().getProtocol(KnownProtocolKind::ObjectiveCBridgeable);

  if (bridgedProto) {
    // Find the conformance of the value type to _BridgedToObjectiveC.
    // Check whether the type conforms to _BridgedToObjectiveC.
    auto conformance = M->lookupConformance(Ty, bridgedProto, nullptr);
    return conformance.hasValue();
  }
  return false;
}

/// Check if a given type conforms to _Error protocol.
bool swift::isError(ModuleDecl *M, CanType Ty) {
  // Retrieve the Error protocol.
  auto errorTypeProto =
      M->getASTContext().getProtocol(KnownProtocolKind::Error);

  if (errorTypeProto) {
    // Find the conformance of the value type to _BridgedToObjectiveC.
    // Check whether the type conforms to _BridgedToObjectiveC.
    auto conformance = M->lookupConformance(Ty, errorTypeProto, nullptr);
    return conformance.hasValue();
  }
  return false;
}

/// Given that a type is not statically known to be an optional type, check whether
/// it might dynamically be an optional type.
static bool canDynamicallyBeOptionalType(CanType type) {
  assert(!type.getAnyOptionalObjectType());
  return (isa<ArchetypeType>(type) || type.isExistentialType())
      && !type.isAnyClassReferenceType();
}

/// Given two class types, check whether there's a hierarchy relationship
/// between them.
static DynamicCastFeasibility
classifyClassHierarchyCast(CanType source, CanType target) {
  // Upcast: if the target type statically matches a type in the
  // source type's hierarchy, this is a static upcast and the cast
  // will always succeed.
  if (target->isExactSuperclassOf(source, nullptr))
    return DynamicCastFeasibility::WillSucceed;

  // Upcast: if the target type might dynamically match a type in the
  // source type's hierarchy, this might be an upcast, in which
  // case the cast might succeed.
  if (target->isBindableToSuperclassOf(source, nullptr))
    return DynamicCastFeasibility::MaySucceed;

  // Downcast: if the source type might dynamically match a type in the
  // target type's hierarchy, this might be a downcast, in which case
  // the cast might succeed.  Note that this also covers the case where
  // the source type statically matches a type in the target type's
  // hierarchy; since it's a downcast, the cast still at best might succeed.
  if (source->isBindableToSuperclassOf(target, nullptr))
    return DynamicCastFeasibility::MaySucceed;

  // Otherwise, the classes are unrelated and the cast will fail (at least
  // on these grounds).
  return DynamicCastFeasibility::WillFail;
}

CanType swift::getNSBridgedClassOfCFClass(ModuleDecl *M, CanType type) {
  if (auto classDecl = type->getClassOrBoundGenericClass()) {
    if (classDecl->getForeignClassKind() == ClassDecl::ForeignKind::CFType) {
      if (auto bridgedAttr =
            classDecl->getAttrs().getAttribute<ObjCBridgedAttr>()) {
        auto bridgedClass = bridgedAttr->getObjCClass();
        // TODO: this should handle generic classes properly.
        if (!bridgedClass->isGenericContext()) {
          return bridgedClass->getDeclaredInterfaceType()->getCanonicalType();
        }
      }
    }
  }
  return CanType();
}

static bool isCFBridgingConversion(ModuleDecl *M, SILType sourceType,
                                   SILType targetType) {
  return (sourceType.getSwiftRValueType() ==
            getNSBridgedClassOfCFClass(M, targetType.getSwiftRValueType()) ||
          targetType.getSwiftRValueType() ==
            getNSBridgedClassOfCFClass(M, sourceType.getSwiftRValueType()));
}

/// Try to classify the dynamic-cast relationship between two types.
DynamicCastFeasibility
swift::classifyDynamicCast(ModuleDecl *M,
                           CanType source,
                           CanType target,
                           bool isSourceTypeExact,
                           bool isWholeModuleOpts) {
  if (source == target) return DynamicCastFeasibility::WillSucceed;

  auto sourceObject = source.getAnyOptionalObjectType();
  auto targetObject = target.getAnyOptionalObjectType();

  // A common level of optionality doesn't affect the feasibility,
  // except that we can't fold things to failure because nil inhabits
  // both types.
  if (sourceObject && targetObject) {
    return atWorst(classifyDynamicCast(M, sourceObject, targetObject),
                   DynamicCastFeasibility::MaySucceed);

  // Casting to a more optional type follows the same rule unless we
  // know that the source cannot dynamically be an optional value,
  // in which case we'll always just cast and inject into an optional.
  } else if (targetObject) {
    auto result = classifyDynamicCast(M, source, targetObject,
                                      /* isSourceTypeExact */ false,
                                      isWholeModuleOpts);
    if (canDynamicallyBeOptionalType(source))
      result = atWorst(result, DynamicCastFeasibility::MaySucceed);
    return result;

  // Casting to a less-optional type can always fail.
  } else if (sourceObject) {
    return atBest(classifyDynamicCast(M, sourceObject, target,
                                      /* isSourceTypeExact */ false,
                                      isWholeModuleOpts),
                  DynamicCastFeasibility::MaySucceed);
  }
  assert(!sourceObject && !targetObject);

  // Assume that casts to or from existential types or involving
  // dependent types can always succeed.  This is over-conservative.
  if (source->hasArchetype() || source.isExistentialType() ||
      target->hasArchetype() || target.isExistentialType()) {

    auto *SourceNominalTy = source.getAnyNominal();

    // Check conversions from non-protocol types into protocol types.
    if (!source.isExistentialType() &&
        SourceNominalTy &&
        target.isExistentialType())
      return classifyDynamicCastToProtocol(source, target, isWholeModuleOpts);

    // Check conversions from protocol types to non-protocol types.
    if (source.isExistentialType() &&
        !target.isExistentialType())
      return classifyDynamicCastFromProtocol(M, source, target,
                                             isWholeModuleOpts);

    return DynamicCastFeasibility::MaySucceed;
  }

  // Casts from AnyHashable.
  if (auto sourceStruct = dyn_cast<StructType>(source)) {
    if (sourceStruct->getDecl() == M->getASTContext().getAnyHashableDecl()) {
      if (auto hashable = getHashableExistentialType(M)) {
        // Succeeds if Hashable can be cast to the target type.
        return classifyDynamicCastFromProtocol(M, hashable, target,
                                               isWholeModuleOpts);
      }
    }
  }

  // Casts to AnyHashable.
  if (auto targetStruct = dyn_cast<StructType>(target)) {
    if (targetStruct->getDecl() == M->getASTContext().getAnyHashableDecl()) {
      // Succeeds if the source type can be dynamically cast to Hashable.
      // Hashable is not actually a legal existential type right now, but
      // the check doesn't care about that.
      if (auto hashable = getHashableExistentialType(M)) {
        return classifyDynamicCastToProtocol(source, hashable,
                                             isWholeModuleOpts);
      }
    }
  }

  // Metatype casts.
  if (auto sourceMetatype = dyn_cast<AnyMetatypeType>(source)) {
    auto targetMetatype = dyn_cast<AnyMetatypeType>(target);
    if (!targetMetatype) return DynamicCastFeasibility::WillFail;

    source = sourceMetatype.getInstanceType();
    target = targetMetatype.getInstanceType();

    if (source == target &&
        targetMetatype.isAnyExistentialType() ==
            sourceMetatype.isAnyExistentialType())
      return DynamicCastFeasibility::WillSucceed;

    if (targetMetatype.isAnyExistentialType() &&
        (isa<ProtocolType>(target) || isa<ProtocolCompositionType>(target))) {
      auto Feasibility = classifyDynamicCastToProtocol(source,
                                                       target,
                                                       isWholeModuleOpts);
      // Cast from existential metatype to existential metatype may still
      // succeed, even if we cannot prove anything statically.
      if (Feasibility != DynamicCastFeasibility::WillFail ||
          !sourceMetatype.isAnyExistentialType())
        return Feasibility;
    }

    // If isSourceTypeExact is true, we know we are casting the result of a
    // MetatypeInst instruction.
    if (isSourceTypeExact) {
      // If source or target are existentials, then it can be cast
      // successfully only into itself.
      if ((target.isAnyExistentialType() || source.isAnyExistentialType()) &&
          target != source)
        return DynamicCastFeasibility::WillFail;
    }

    // Casts from class existential metatype into a concrete non-class metatype
    // can never succeed.
    if (source->isClassExistentialType() &&
        !target.isAnyExistentialType() &&
        !target.getClassOrBoundGenericClass())
      return DynamicCastFeasibility::WillFail;

    // TODO: prove that some conversions to existential metatype will
    // obviously succeed/fail.
    // TODO: prove that some conversions from class existential metatype
    // to a concrete non-class metatype will obviously fail.
    // TODO: class metatype to/from AnyObject
    // TODO: protocol concrete metatype to/from ObjCProtocol
    if (isa<ExistentialMetatypeType>(sourceMetatype) ||
        isa<ExistentialMetatypeType>(targetMetatype))
      return (getAnyMetatypeDepth(source) == getAnyMetatypeDepth(target)
              ? DynamicCastFeasibility::MaySucceed
              : DynamicCastFeasibility::WillFail);

    // If both metatypes are class metatypes, check if classes can be
    // cast.
    if (source.getClassOrBoundGenericClass() &&
        target.getClassOrBoundGenericClass())
      return classifyClassHierarchyCast(source, target);

    // Different structs cannot be cast to each other.
    if (source.getStructOrBoundGenericStruct() &&
        target.getStructOrBoundGenericStruct() &&
        source != target)
      return DynamicCastFeasibility::WillFail;

    // Different enums cannot be cast to each other.
    if (source.getEnumOrBoundGenericEnum() &&
        target.getEnumOrBoundGenericEnum() &&
        source != target)
      return DynamicCastFeasibility::WillFail;

    // If we don't know any better, assume that the cast may succeed.
    return DynamicCastFeasibility::MaySucceed;
  }

  // Function casts.
  if (auto sourceFunction = dyn_cast<FunctionType>(source)) {
    if (auto targetFunction = dyn_cast<FunctionType>(target)) {
      // A function cast can succeed if the function types can be identical,
      // or if the target type is throwier than the original.

      // A non-throwing source function can be cast to a throwing target type,
      // but not vice versa.
      if (sourceFunction->throws() && !targetFunction->throws())
        return DynamicCastFeasibility::WillFail;
      
      // The cast can't change the representation at runtime.
      if (targetFunction->getRepresentation()
            != sourceFunction->getRepresentation())
        return DynamicCastFeasibility::WillFail;
      
      if (sourceFunction.getInput() == targetFunction.getInput()
          && sourceFunction.getResult() == targetFunction.getResult())
        return DynamicCastFeasibility::WillSucceed;

      auto isSubstitutable = [](CanType a, CanType b) -> bool {
        // FIXME: Unnecessarily conservative; should structurally check for
        // substitutability.
        return a == b || a->hasArchetype() || b->hasArchetype();
      };
    
      if (isSubstitutable(sourceFunction.getInput(), targetFunction.getInput())
          && isSubstitutable(targetFunction.getInput(),
                             targetFunction.getResult()))
        return DynamicCastFeasibility::MaySucceed;
      
      return DynamicCastFeasibility::WillFail;
    }
  }

  // Tuple casts.
  if (auto sourceTuple = dyn_cast<TupleType>(source)) {
    if (auto targetTuple = dyn_cast<TupleType>(target)) {
      // # of elements must coincide.
      if (sourceTuple->getNumElements() != targetTuple->getNumElements())
        return DynamicCastFeasibility::WillFail;

      DynamicCastFeasibility result = DynamicCastFeasibility::WillSucceed;
      for (unsigned i : range(sourceTuple->getNumElements())) {
        const auto &sourceElt = sourceTuple->getElement(i);
        const auto &targetElt = targetTuple->getElement(i);

        // If both have names and the names mismatch, the cast will fail.
        if (sourceElt.hasName() && targetElt.hasName() &&
            sourceElt.getName() != targetElt.getName())
          return DynamicCastFeasibility::WillFail;

        // Combine the result of prior elements with this element type.
        result = std::max(result,
                          classifyDynamicCast(M,
                            sourceElt.getType()->getCanonicalType(),
                            targetElt.getType()->getCanonicalType(),
                            isSourceTypeExact,
                            isWholeModuleOpts));

        // If this element failed, we're done.
        if (result == DynamicCastFeasibility::WillFail)
          break;
      }

      return result;
    }
  }

  // Class casts.
  auto sourceClass = source.getClassOrBoundGenericClass();
  auto targetClass = target.getClassOrBoundGenericClass();
  if (sourceClass) {
    if (targetClass) {
      // Imported Objective-C generics don't check the generic parameters, which
      // are lost at runtime.
      if (sourceClass->usesObjCGenericsModel()) {
      
        if (sourceClass == targetClass)
          return DynamicCastFeasibility::WillSucceed;
        
        if (targetClass->usesObjCGenericsModel()) {
          // If both classes are ObjC generics, the cast may succeed if the
          // classes are related, irrespective of their generic parameters.
          auto isDeclSuperclass = [&](ClassDecl *proposedSuper,
                                      ClassDecl *proposedSub) -> bool {
            do {
              if (proposedSuper == proposedSub)
                return true;
            } while ((proposedSub = proposedSub->getSuperclassDecl()));
            
            return false;
          };
          
          if (isDeclSuperclass(sourceClass, targetClass))
            return DynamicCastFeasibility::MaySucceed;
          
          if (isDeclSuperclass(targetClass, sourceClass)) {
            return DynamicCastFeasibility::WillSucceed;
          }          
          return DynamicCastFeasibility::WillFail;
        }
      }

      // Try a hierarchy cast.  If that isn't failure, we can report it.
      auto hierarchyResult = classifyClassHierarchyCast(source, target);
      if (hierarchyResult != DynamicCastFeasibility::WillFail)
        return hierarchyResult;

      // As a backup, consider whether either type is a CF class type
      // with an NS bridged equivalent.
      CanType bridgedSource = getNSBridgedClassOfCFClass(M, source);
      CanType bridgedTarget = getNSBridgedClassOfCFClass(M, target);

      // If neither type qualifies, we're done.
      if (!bridgedSource && !bridgedTarget)
        return DynamicCastFeasibility::WillFail;

      // Otherwise, map over to the bridged types and try to answer the
      // question there.
      if (bridgedSource) source = bridgedSource;
      if (bridgedTarget) target = bridgedTarget;
      return classifyDynamicCast(M, source, target, false, isWholeModuleOpts);
    }

    // Casts from a class into a non-class can never succeed if the target must
    // be bridged to a SwiftValueBox. You would need an AnyObject source for
    // that.
    if (!target.isAnyExistentialType() &&
        !target.getClassOrBoundGenericClass() &&
        !isa<ArchetypeType>(target) &&
        mustBridgeToSwiftValueBox(M, target)) {
      assert((target.getEnumOrBoundGenericEnum() ||
              target.getStructOrBoundGenericStruct() ||
              isa<TupleType>(target) ||
              isa<SILFunctionType>(target) ||
              isa<FunctionType>(target) ||
              isa<MetatypeType>(target)) &&
             "Target should be an enum, struct, tuple, metatype or function type");
      return DynamicCastFeasibility::WillFail;
    }


    // In the Objective-C runtime, class metatypes are also class instances.
    // The cast may succeed if the target type can be inhabited by a class
    // metatype.
    // TODO: Narrow this to the sourceClass being exactly NSObject.
    if (M->getASTContext().LangOpts.EnableObjCInterop) {
      if (auto targetMeta = dyn_cast<MetatypeType>(target)) {
        if (isa<ArchetypeType>(targetMeta.getInstanceType())
            || targetMeta.getInstanceType()->mayHaveSuperclass())
          return DynamicCastFeasibility::MaySucceed;
      } else if (isa<ExistentialMetatypeType>(target)) {
        return DynamicCastFeasibility::MaySucceed;
      }
    }
  }

  // If the source is not existential, an archetype, or (under the ObjC runtime)
  // a class, and the destination is a metatype, there is no way the cast can
  // succeed.
  if (target->is<AnyMetatypeType>()) return DynamicCastFeasibility::WillFail;

  // FIXME: Be more careful with bridging conversions from
  // NSArray, NSDictionary and NSSet as they may fail?

  // We know that a cast from Int -> class foobar will fail.
  if (targetClass &&
      !source.isAnyExistentialType() &&
      !source.getClassOrBoundGenericClass() &&
      !isa<ArchetypeType>(source) &&
      mustBridgeToSwiftValueBox(M, source)) {
      assert((source.getEnumOrBoundGenericEnum() ||
              source.getStructOrBoundGenericStruct() ||
              isa<TupleType>(source) ||
              isa<SILFunctionType>(source) ||
              isa<FunctionType>(source) ||
              isa<MetatypeType>(source)) &&
             "Source should be an enum, struct, tuple, metatype or function type");
    return DynamicCastFeasibility::WillFail;
  }

  // Check if there might be a bridging conversion.
  if (source->isBridgeableObjectType() && mayBridgeToObjectiveC(M, target)) {
    // Try to get the ObjC type which is bridged to target type.
    assert(!target.isAnyExistentialType());
    if (Type ObjCTy = M->getASTContext().getBridgedToObjC(M, target)) {
      // If the bridged ObjC type is known, check if
      // source type can be cast into it.
      return classifyDynamicCast(M, source,
          ObjCTy->getCanonicalType(),
          /* isSourceTypeExact */ false, isWholeModuleOpts);
    }
    return DynamicCastFeasibility::MaySucceed;
  }
  
  if (target->isBridgeableObjectType() && mayBridgeToObjectiveC(M, source)) {
    // Try to get the ObjC type which is bridged to source type.
    assert(!source.isAnyExistentialType());
    if (Type ObjCTy = M->getASTContext().getBridgedToObjC(M, source)) {
      // If the bridged ObjC type is known, check if
      // this type can be cast into target type.
      return classifyDynamicCast(M,
          ObjCTy->getCanonicalType(),
          target,
          /* isSourceTypeExact */ false, isWholeModuleOpts);
    }
    return DynamicCastFeasibility::MaySucceed;
  }

  // Check if it is a cast between bridged error types.
  if (isError(M, source) && isError(M, target)) {
    // TODO: Cast to NSError succeeds always.
    return DynamicCastFeasibility::MaySucceed;
  }

  // Check for a viable collection cast.
  if (auto sourceStruct = dyn_cast<BoundGenericStructType>(source)) {
    if (auto targetStruct = dyn_cast<BoundGenericStructType>(target)) {
      // Both types have to be the same kind of collection.
      auto typeDecl = sourceStruct->getDecl();
      if (typeDecl == targetStruct->getDecl()) {
        auto sourceArgs = sourceStruct.getGenericArgs();
        auto targetArgs = targetStruct.getGenericArgs();

        // Note that we can never say that a collection cast is impossible:
        // a cast can always succeed on an empty collection.

        // Arrays and sets.
        if (typeDecl == M->getASTContext().getArrayDecl() ||
            typeDecl == M->getASTContext().getSetDecl()) {
          auto valueFeasibility =
            classifyDynamicCast(M, sourceArgs[0], targetArgs[0]);
          return atWorst(valueFeasibility,
                         DynamicCastFeasibility::MaySucceed);

        // Dictionaries.
        } else if (typeDecl == M->getASTContext().getDictionaryDecl()) {
          auto keyFeasibility =
            classifyDynamicCast(M, sourceArgs[0], targetArgs[0]);
          auto valueFeasibility =
            classifyDynamicCast(M, sourceArgs[1], targetArgs[1]);
          return atWorst(atBest(keyFeasibility, valueFeasibility),
                         DynamicCastFeasibility::MaySucceed);
        }
      }
    }
  }

  return DynamicCastFeasibility::WillFail;
}

static unsigned getOptionalDepth(CanType type) {
  unsigned depth = 0;
  while (CanType objectType = type.getAnyOptionalObjectType()) {
    depth++;
    type = objectType;
  }
  return depth;
}

namespace {
  struct Source {
    SILValue Value;
    CanType FormalType;
    CastConsumptionKind Consumption;

    bool isAddress() const { return Value->getType().isAddress(); }
    IsTake_t shouldTake() const {
      return shouldTakeOnSuccess(Consumption);
    }

    Source() = default;
    Source(SILValue value, CanType formalType, CastConsumptionKind consumption)
      : Value(value), FormalType(formalType), Consumption(consumption) {}
  };

  struct Target {
    SILValue Address;
    SILType LoweredType;
    CanType FormalType;

    bool isAddress() const { return (bool) Address; }

    Source asAddressSource() const {
      assert(isAddress());
      return { Address, FormalType, CastConsumptionKind::TakeAlways };
    }
    Source asScalarSource(SILValue value) const {
      assert(!isAddress());
      assert(!value->getType().isAddress());
      return { value, FormalType, CastConsumptionKind::TakeAlways };
    }

    Target() = default;
    Target(SILValue address, CanType formalType)
      : Address(address), LoweredType(address->getType()),
        FormalType(formalType) {
      assert(LoweredType.isAddress());
    }
    Target(SILType loweredType, CanType formalType)
      : Address(), LoweredType(loweredType), FormalType(formalType) {
      assert(!loweredType.isAddress());
    }
  };

  class CastEmitter {
    SILBuilder &B;
    SILModule &M;
    ASTContext &Ctx;
    SILLocation Loc;
    ModuleDecl *SwiftModule;
  public:
    CastEmitter(SILBuilder &B, ModuleDecl *swiftModule, SILLocation loc)
      : B(B), M(B.getModule()), Ctx(M.getASTContext()), Loc(loc),
        SwiftModule(swiftModule) {}

    Source emitTopLevel(Source source, Target target) {
      unsigned sourceOptDepth = getOptionalDepth(source.FormalType);
      unsigned targetOptDepth = getOptionalDepth(target.FormalType);      

      assert(sourceOptDepth <= targetOptDepth);
      return emitAndInjectIntoOptionals(source, target,
                                        targetOptDepth - sourceOptDepth);
    }

  private:
    const TypeLowering &getTypeLowering(SILType type) {
      return M.Types.getTypeLowering(type);
    }

    SILValue getOwnedScalar(Source source, const TypeLowering &srcTL) {
      assert(!source.isAddress());
      if (!source.shouldTake())
        srcTL.emitCopyValue(B, Loc, source.Value);
      return source.Value;
    }

    Source putOwnedScalar(SILValue scalar, Target target) {
      assert(scalar->getType() == target.LoweredType.getObjectType());
      if (!target.isAddress())
        return target.asScalarSource(scalar);

      auto &targetTL = getTypeLowering(target.LoweredType);
      targetTL.emitStoreOfCopy(B, Loc, scalar, target.Address,
                               IsInitialization);
      return target.asAddressSource();
    }

    Source emitSameType(Source source, Target target) {
      assert(source.FormalType == target.FormalType);

      auto &srcTL = getTypeLowering(source.Value->getType());

      // The destination always wants a +1 value, so make the source
      // +1 if it's a scalar.
      if (!source.isAddress()) {
        source.Value = getOwnedScalar(source, srcTL);
        source.Consumption = CastConsumptionKind::TakeAlways;
      }

      // If we've got a scalar and want a scalar, the source is
      // exactly right.
      if (!target.isAddress() && !source.isAddress())
        return source;

      // If the destination wants a non-address value, load
      if (!target.isAddress()) {
        SILValue value = srcTL.emitLoadOfCopy(B, Loc, source.Value,
                                              source.shouldTake());
        return target.asScalarSource(value);
      }

      if (source.isAddress()) {
        srcTL.emitCopyInto(B, Loc, source.Value, target.Address,
                           source.shouldTake(), IsInitialization);
      } else {
        srcTL.emitStoreOfCopy(B, Loc, source.Value, target.Address,
                              IsInitialization);
      }
      return target.asAddressSource();
    }

    Source emit(Source source, Target target) {
      if (source.FormalType == target.FormalType)
        return emitSameType(source, target);

      // Handle subtype conversions involving optionals.
      OptionalTypeKind sourceOptKind;
      if (auto sourceObjectType =
            source.FormalType.getAnyOptionalObjectType(sourceOptKind)) {
        return emitOptionalToOptional(source, sourceOptKind, sourceObjectType,
                                      target);
      }
      assert(!target.FormalType.getAnyOptionalObjectType());

      // The only other things we return WillSucceed for currently is
      // an upcast or CF/NS toll-free-bridging conversion.
      // FIXME: Upcasts between existential metatypes are not handled yet.
      // We should generate for it:
      // %openedSrcMetatype = open_existential srcMetatype
      // init_existential dstMetatype, %openedSrcMetatype
      auto &srcTL = getTypeLowering(source.Value->getType());
      SILValue value;
      if (source.isAddress()) {
        value = srcTL.emitLoadOfCopy(B, Loc, source.Value, source.shouldTake());
      } else {
        value = getOwnedScalar(source, srcTL);
      }
      auto targetTy = target.LoweredType;
      if (isCFBridgingConversion(SwiftModule, targetTy, value->getType())) {
        value = B.createUncheckedRefCast(Loc, value, targetTy.getObjectType());
      } else {
        value = B.createUpcast(Loc, value, targetTy.getObjectType());
      }
      return putOwnedScalar(value, target);
    }

    Source emitAndInjectIntoOptionals(Source source, Target target,
                                      unsigned depth) {
      if (depth == 0)
        return emit(source, target);

      // Recurse.
      EmitSomeState state;
      Target objectTarget = prepareForEmitSome(target, state);
      Source objectSource =
        emitAndInjectIntoOptionals(source, objectTarget, depth - 1);
      return emitSome(objectSource, target, state);
    }

    Source emitOptionalToOptional(Source source,
                                  OptionalTypeKind sourceOptKind,
                                  CanType sourceObjectType,
                                  Target target) {
      // Switch on the incoming value.
      SILBasicBlock *contBB = B.splitBlockForFallthrough();
      SILBasicBlock *noneBB = B.splitBlockForFallthrough();
      SILBasicBlock *someBB = B.splitBlockForFallthrough();

      // Emit the switch.
      std::pair<EnumElementDecl*, SILBasicBlock*> cases[] = {
        { Ctx.getOptionalSomeDecl(sourceOptKind), someBB },
        { Ctx.getOptionalNoneDecl(sourceOptKind), noneBB },
      };
      if (source.isAddress()) {
        B.createSwitchEnumAddr(Loc, source.Value, /*default*/ nullptr, cases);
      } else {
        B.createSwitchEnum(Loc, source.Value, /*default*/ nullptr, cases);
      }

      // Create the Some block, which recurses.
      B.setInsertionPoint(someBB);
      {
        auto sourceSomeDecl = Ctx.getOptionalSomeDecl(sourceOptKind);

        SILType loweredSourceObjectType =
          source.Value->getType().getEnumElementType(sourceSomeDecl, M);

        // Form the target for the optional object.
        EmitSomeState state;
        Target objectTarget = prepareForEmitSome(target, state);

        // Form the source value.
        AllocStackInst *sourceTemp = nullptr;
        Source objectSource;
        if (source.isAddress()) {
          // TODO: add an instruction for non-destructively getting a
          // specific element's data.
          SILValue sourceAddr = source.Value;
          if (!source.shouldTake()) {
            sourceTemp = B.createAllocStack(Loc,
                                       sourceAddr->getType().getObjectType());
            sourceAddr = sourceTemp;
            B.createCopyAddr(Loc, source.Value, sourceAddr, IsNotTake,
                             IsInitialization);
          }
          sourceAddr = B.createUncheckedTakeEnumDataAddr(Loc, sourceAddr,
                                    sourceSomeDecl, loweredSourceObjectType);
          objectSource = Source(sourceAddr, sourceObjectType,
                                CastConsumptionKind::TakeAlways);
        } else {
          // switch enum always start as @owned.
          SILValue sourceObjectValue = someBB->createPHIArgument(
              loweredSourceObjectType, ValueOwnershipKind::Owned);
          objectSource = Source(sourceObjectValue, sourceObjectType,
                                source.Consumption);
        }

        Source resultObject = emit(objectSource, objectTarget);

        // Deallocate the source temporary if we needed one.
        if (sourceTemp) {
          B.createDeallocStack(Loc, sourceTemp);
        }

        Source result = emitSome(resultObject, target, state);
        assert(result.isAddress() == target.isAddress());
        if (target.isAddress()) {
          B.createBranch(Loc, contBB);
        } else {
          B.createBranch(Loc, contBB, { result.Value });
        }
      }

      // Create the None block.
      B.setInsertionPoint(noneBB);
      {
        Source result = emitNone(target);
        assert(result.isAddress() == target.isAddress());
        if (target.isAddress()) {
          B.createBranch(Loc, contBB);
        } else {
          B.createBranch(Loc, contBB, { result.Value });
        }
      }

      // Continuation block.
      B.setInsertionPoint(contBB);
      if (target.isAddress()) {
        return target.asAddressSource();
      } else {
        SILValue result = contBB->createPHIArgument(target.LoweredType,
                                                    ValueOwnershipKind::Owned);
        return target.asScalarSource(result);
      }
    }

    struct EmitSomeState {
      EnumElementDecl *SomeDecl;
    };

    Target prepareForEmitSome(Target target, EmitSomeState &state) {
      auto objectType = target.FormalType.getAnyOptionalObjectType();
      assert(objectType && "emitting Some into non-optional type");

      auto someDecl = Ctx.getOptionalSomeDecl();
      state.SomeDecl = someDecl;

      SILType loweredObjectType =
        target.LoweredType.getEnumElementType(someDecl, M);

      if (target.isAddress()) {
        SILValue objectAddr =
          B.createInitEnumDataAddr(Loc, target.Address, someDecl,
                                   loweredObjectType);
        return { objectAddr, objectType };
      } else {
        return { loweredObjectType, objectType };
      }
    }

    Source emitSome(Source source, Target target, EmitSomeState &state) {
      // If our target is an address, prepareForEmitSome should have set this
      // up so that we emitted directly into 
      if (target.isAddress()) {
        B.createInjectEnumAddr(Loc, target.Address, state.SomeDecl);
        return target.asAddressSource();
      } else {
        auto &srcTL = getTypeLowering(source.Value->getType());
        auto sourceObject = getOwnedScalar(source, srcTL);
        auto source = B.createEnum(Loc, sourceObject, state.SomeDecl,
                                   target.LoweredType);
        return target.asScalarSource(source);
      }
    }

    Source emitNone(Target target) {
      auto noneDecl = Ctx.getOptionalNoneDecl();
      
      if (target.isAddress()) {
        B.createInjectEnumAddr(Loc, target.Address, noneDecl);
        return target.asAddressSource();
      } else {
        SILValue res = B.createEnum(Loc, nullptr, noneDecl, target.LoweredType);
        return target.asScalarSource(res);
      }
    }
  };
} // end anonymous namespace

/// Emit an unconditional scalar cast that's known to succeed.
SILValue
swift::emitSuccessfulScalarUnconditionalCast(SILBuilder &B, ModuleDecl *M,
                                             SILLocation loc, SILValue value,
                                             SILType loweredTargetType,
                                             CanType sourceType,
                                             CanType targetType,
                                             SILInstruction *existingCast) {
  assert(classifyDynamicCast(M, sourceType, targetType)
           == DynamicCastFeasibility::WillSucceed);

  // Casts to/from existential types cannot be further improved.
  if (sourceType.isAnyExistentialType() ||
      targetType.isAnyExistentialType()) {
    if (existingCast)
      // Indicate that the existing cast cannot be further improved.
      return SILValue();

    llvm_unreachable("Casts to/from existentials are not supported yet");
  }

  // Fast path changes that don't change the type.
  if (sourceType == targetType)
    return value;

  Source source(value, sourceType, CastConsumptionKind::TakeAlways);
  Target target(loweredTargetType, targetType);
  Source result = CastEmitter(B, M, loc).emitTopLevel(source, target);
  assert(!result.isAddress());
  assert(result.Value->getType() == loweredTargetType);
  assert(result.Consumption == CastConsumptionKind::TakeAlways);
  return result.Value;
}

bool swift::emitSuccessfulIndirectUnconditionalCast(SILBuilder &B, ModuleDecl *M,
                                                    SILLocation loc,
                                               CastConsumptionKind consumption,
                                                    SILValue src,
                                                    CanType sourceType,
                                                    SILValue dest,
                                                    CanType targetType,
                                                    SILInstruction *existingCast) {
  assert(classifyDynamicCast(M, sourceType, targetType)
           == DynamicCastFeasibility::WillSucceed);

  assert(src->getType().isAddress());
  assert(dest->getType().isAddress());

  // Casts between the same types can be always handled here.
  // Casts from non-existentials into existentials and
  // vice-versa cannot be improved yet.
  // Casts between a value type and a class cannot be optimized.
  // Therefore generate a simple unconditional_checked_cast_aadr.

  if (src->getType() != dest->getType())
  if (src->getType().isAnyExistentialType() !=
      dest->getType().isAnyExistentialType() ||
      !(src->getType().getClassOrBoundGenericClass() &&
       dest->getType().getClassOrBoundGenericClass())) {
    // If there is an existing cast with the same arguments,
    // indicate we cannot improve it.
    if (existingCast) {
      auto *UCCAI = dyn_cast<UnconditionalCheckedCastAddrInst>(existingCast);
      if (UCCAI && UCCAI->getSrc() == src && UCCAI->getDest() == dest &&
          UCCAI->getSourceType() == sourceType &&
          UCCAI->getTargetType() == targetType &&
          UCCAI->getConsumptionKind() == consumption) {
        // Indicate that the existing cast cannot be further improved.
        return false;
      }
    }

    B.createUnconditionalCheckedCastAddr(loc, consumption, src, sourceType,
                                         dest, targetType);
    return true;
  }

  Source source(src, sourceType, consumption);
  Target target(dest, targetType);
  Source result = CastEmitter(B, M, loc).emitTopLevel(source, target);
  assert(result.isAddress());
  assert(result.Value == dest);
  assert(result.Consumption == CastConsumptionKind::TakeAlways);
  (void) result;
  return true;
}

/// Can the given cast be performed by the scalar checked-cast
/// instructions?
///
/// CAUTION: if you introduce bridging conversions to the set of
/// things handleable by the scalar checked casts --- and that's not
/// totally unreasonable --- you will need to make the scalar checked
/// casts take a cast consumption kind.
bool swift::canUseScalarCheckedCastInstructions(SILModule &M,
                                                CanType sourceType,
                                                CanType targetType) {
  // Look through one level of optionality on the source.
  auto objectType = sourceType;
  if (auto type = objectType.getAnyOptionalObjectType())
    objectType = type;

  // Casting to NSError needs to go through the indirect-cast case,
  // since it may conform to Error and require Error-to-NSError
  // bridging, unless we can statically see that the source type inherits
  // NSError.
  
  // A class-constrained archetype may be bound to NSError, unless it has a
  // non-NSError superclass constraint. Casts to archetypes thus must always be
  // indirect.
  if (auto archetype = targetType->getAs<ArchetypeType>()) {
    auto super = archetype->getSuperclass();
    if (super.isNull())
      return false;

    // Only ever permit this if the source type is a reference type.
    if (!objectType.isAnyClassReferenceType())
      return false;

    // A base class constraint that isn't NSError rules out the archetype being
    // bound to NSError.
    if (auto nserror = M.Types.getNSErrorType())
      return !super->isEqual(nserror);
    // If NSError wasn't loaded, any base class constraint must not be NSError.
    return true;
  }
  
  if (targetType == M.Types.getNSErrorType()) {
    // If we statically know the source is an NSError subclass, then the cast
    // can go through the scalar path (and it's trivially true so can be
    // killed).
    return targetType->isExactSuperclassOf(objectType, nullptr);
  }
  
  // Three supported cases:
  // - metatype to metatype
  // - metatype to object
  // - object to object
  if ((objectType.isAnyClassReferenceType() || isa<AnyMetatypeType>(objectType))
      && targetType.isAnyClassReferenceType())
    return true;

  if (isa<AnyMetatypeType>(objectType) && isa<AnyMetatypeType>(targetType))
    return true;
  
  // Otherwise, we need to use the general indirect-cast functions.
  return false;
}

/// Carry out the operations required for an indirect conditional cast
/// using a scalar cast operation.
void swift::
emitIndirectConditionalCastWithScalar(SILBuilder &B, ModuleDecl *M,
                                      SILLocation loc,
                                      CastConsumptionKind consumption,
                                      SILValue src, CanType sourceType,
                                      SILValue dest, CanType targetType,
                                      SILBasicBlock *indirectSuccBB,
                                      SILBasicBlock *indirectFailBB) {
  assert(canUseScalarCheckedCastInstructions(B.getModule(),
                                             sourceType, targetType));

  // We only need a different failure block if the cast consumption
  // requires us to destroy the source value.
  SILBasicBlock *scalarFailBB;
  if (!shouldDestroyOnFailure(consumption)) {
    scalarFailBB = indirectFailBB;
  } else {
    scalarFailBB = B.splitBlockForFallthrough();
  }

  // We always need a different success block.
  SILBasicBlock *scalarSuccBB = B.splitBlockForFallthrough();

  auto &srcTL = B.getModule().Types.getTypeLowering(src->getType());

  // Always take; this works under an assumption that retaining the
  // result is equivalent to retaining the source.  That means that
  // these casts would not be appropriate for bridging-like conversions.
  SILValue srcValue = srcTL.emitLoadOfCopy(B, loc, src, IsTake);

  SILType targetValueType = dest->getType().getObjectType();
  B.createCheckedCastBranch(loc, /*exact*/ false, srcValue, targetValueType,
                            scalarSuccBB, scalarFailBB);

  // Emit the success block.
  B.setInsertionPoint(scalarSuccBB); {
    auto &targetTL = B.getModule().Types.getTypeLowering(targetValueType);
    SILValue succValue = scalarSuccBB->createPHIArgument(
        targetValueType, ValueOwnershipKind::Owned);
    if (!shouldTakeOnSuccess(consumption))
      targetTL.emitCopyValue(B, loc, succValue);
    targetTL.emitStoreOfCopy(B, loc, succValue, dest, IsInitialization);
    B.createBranch(loc, indirectSuccBB);
  }

  // Emit the failure block.
  if (shouldDestroyOnFailure(consumption)) {
    B.setInsertionPoint(scalarFailBB);
    srcTL.emitDestroyValue(B, loc, srcValue);
    B.createBranch(loc, indirectFailBB);
  }
}
