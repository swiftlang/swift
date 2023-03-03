//===--- TypeLowering.cpp - Type information for SILGen -------------------===//
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

#define DEBUG_TYPE "libsil"

#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/TypeDifferenceVisitor.h"
#include "swift/AST/Types.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "clang/AST/Type.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace Lowering;

// Necessary to straightforwardly write SIL tests that exercise
// OpaqueValueTypeLowering (and MoveOnlyOpaqueValueTypeLowering): the tests can
// be written as though opaque values were enabled to begin but have since been
// lowered out of.
llvm::cl::opt<bool> TypeLoweringForceOpaqueValueLowering(
    "type-lowering-force-opaque-value-lowering", llvm::cl::init(false),
    llvm::cl::desc("Force TypeLowering to behave as if building with opaque "
                   "values enabled"));

static bool shouldUseOpaqueValuesForAddressOnly(TypeConverter &TC) {
  return TC.Context.SILOpts.EnableSILOpaqueValues ||
         TypeLoweringForceOpaqueValueLowering;
}

namespace {
  /// A CRTP type visitor for deciding whether the metatype for a type
  /// is a singleton type, i.e. whether there can only ever be one
  /// such value.
  struct HasSingletonMetatype : CanTypeVisitor<HasSingletonMetatype, bool> {
    /// Class metatypes have non-trivial representation due to the
    /// possibility of subclassing.
    bool visitClassType(CanClassType type) {
      if (type->isForeignReferenceType())
        return true;
      return false;
    }
    bool visitBoundGenericClassType(CanBoundGenericClassType type) {
      return false;
    }
    bool visitDynamicSelfType(CanDynamicSelfType type) {
      return false;
    }

    /// Dependent types have non-trivial representation in case they
    /// instantiate to a class metatype.
    bool visitGenericTypeParamType(CanGenericTypeParamType type) {
      return false;
    }
    bool visitDependentMemberType(CanDependentMemberType type) {
      return false;
    }
    
    /// Archetype metatypes have non-trivial representation in case
    /// they instantiate to a class metatype.
    bool visitArchetypeType(CanArchetypeType type) {
      return false;
    }
    
    /// All levels of class metatypes support subtyping.
    bool visitMetatypeType(CanMetatypeType type) {
      return visit(type.getInstanceType());
    }

    /// Everything else is trivial.  Note that ordinary metatypes of
    /// existential types are still singleton.
    bool visitType(CanType type) {
      return true;
    }
  };
} // end anonymous namespace

/// Does the metatype for the given type have a known-singleton
/// representation?
static bool hasSingletonMetatype(CanType instanceType) {
  return HasSingletonMetatype().visit(instanceType);
}

CaptureKind TypeConverter::getDeclCaptureKind(CapturedValue capture,
                                              TypeExpansionContext expansion) {
  auto decl = capture.getDecl();
  auto *var = cast<VarDecl>(decl);
  assert(var->hasStorage() &&
         "should not have attempted to directly capture this variable");

  auto &lowering = getTypeLowering(
      var->getType(), TypeExpansionContext::noOpaqueTypeArchetypesSubstitution(
                          expansion.getResilienceExpansion()));

  // If this is a noncopyable 'let' constant that is not a shared paramdecl or
  // used by a noescape capture, then we know it is boxed and want to pass it in
  // its boxed form so we can obey Swift's capture reference semantics.
  if (!var->supportsMutation() && lowering.getLoweredType().isPureMoveOnly() &&
      !capture.isNoEscape()) {
      auto *param = dyn_cast<ParamDecl>(var);
      if (!param || param->getValueOwnership() != ValueOwnership::Shared) {
        return CaptureKind::ImmutableBox;
      }
  }

  // If this is a non-address-only stored 'let' constant, we can capture it
  // by value.  If it is address-only, then we can't load it, so capture it
  // by its address (like a var) instead.
  if (!var->supportsMutation() && !lowering.isAddressOnly())
      return CaptureKind::Constant;

  // In-out parameters are captured by address.
  if (auto *param = dyn_cast<ParamDecl>(var)) {
    if (param->isInOut())
      return CaptureKind::StorageAddress;
  }

  // Reference storage types can appear in a capture list, which means
  // we might allocate boxes to store the captures. However, those boxes
  // have the same lifetime as the closure itself, so we must capture
  // the box itself and not the payload, even if the closure is noescape,
  // otherwise they will be destroyed when the closure is formed.
  if (var->getType()->is<ReferenceStorageType>()) {
    return CaptureKind::Box;
  }

  // For 'let' constants
  if (!var->supportsMutation()) {
    assert(getTypeLowering(
               var->getType(),
               TypeExpansionContext::noOpaqueTypeArchetypesSubstitution(
                   expansion.getResilienceExpansion()))
               .isAddressOnly());
    return CaptureKind::Immutable;
  }

  // If we're capturing into a non-escaping closure, we can generally just
  // capture the address of the value as no-escape.
  return (capture.isNoEscape()
          ? CaptureKind::StorageAddress
          : CaptureKind::Box);
}

using RecursiveProperties = TypeLowering::RecursiveProperties;

namespace {

/// Should computing the recursive properties of the given type
/// consult the cache in the TypeConverter?  Generally, if computing
/// recursive properties is non-trivial (as it is for structs and
/// enums), we should consult the cache; but we need to suppress this
/// if we're actually computing the properties *for* the cache.
enum UseCache_t : bool {
  BypassCache = false,
  UseCache = true
};

/// Compute recursive type properties for a type.  Works on both lowered
/// and formal types.
///
/// The infinite-type check relies on us not re-entering this analysis
/// for anything that is not a directly-stored component in the type.
/// Computing a lowered type can *definitely* require that, so we need
/// make sure we never trigger arbitrary type lowering during this.
/// Fortunately, it's also much better for performance if we can avoid
/// that, and it lets us directly compute properties for formal types.
class ComputeTypeProperties
  : public CanTypeVisitor<ComputeTypeProperties,
                          RecursiveProperties,
                          UseCache_t> {
  TypeConverter &TC;
  TypeExpansionContext Expansion;

public:
  ComputeTypeProperties(TypeConverter &TC, TypeExpansionContext expansion)
    : TC(TC), Expansion(expansion) {}

  /// Can the given type be looked up in the TC's type-lowering cache?
  bool canUseCache(CanType type) {
    // We can't cache types with type parameters in them because
    // we'd potentially need to capture the generic signature.
    return !type->hasTypeParameter();
  }
  RecursiveProperties getFromCache(CanType type, bool isKnownLoweredType) {
    // TODO: add an intermediate level of caching that doesn't require
    // constructing a TypeLowering object.
    if (isKnownLoweredType) {
      return TC.getTypeLowering(SILType::getPrimitiveObjectType(type),
                                Expansion)
               .getRecursiveProperties();
    } else {
      return TC.getTypeLowering(type, Expansion).getRecursiveProperties();
    }
  }

  void handleLifetimeAnnotation(LifetimeAnnotation annotation,
                                RecursiveProperties &props) {
    switch (annotation) {
    case LifetimeAnnotation::None:
      break;
    case LifetimeAnnotation::Lexical:
      props.setLexical(IsLexical);
      break;
    case LifetimeAnnotation::EagerMove:
      props.setLexical(IsNotLexical);
      break;
    }
  }

#define SIMPLE(TYPE, KIND)                                               \
  RecursiveProperties visit##TYPE##Type(Can##TYPE##Type type,            \
                                        UseCache_t shouldUseCache) {     \
    return RecursiveProperties::for##KIND();                             \
  }
#define IMPOSSIBLE(TYPE)                                                 \
  RecursiveProperties visit##TYPE##Type(Can##TYPE##Type type,            \
                                        UseCache_t shouldUseCache) {     \
    llvm_unreachable(#TYPE "Type should not be seen here");              \
  }

  SIMPLE(BuiltinInteger, Trivial)
  SIMPLE(BuiltinIntegerLiteral, Trivial)
  SIMPLE(BuiltinFloat, Trivial)
  SIMPLE(BuiltinRawUnsafeContinuation, Trivial)
  SIMPLE(BuiltinJob, Trivial)
  SIMPLE(BuiltinExecutor, Trivial)
  SIMPLE(BuiltinPackIndex, Trivial)
  SIMPLE(BuiltinNativeObject, Reference)
  SIMPLE(BuiltinBridgeObject, Reference)
  SIMPLE(BuiltinRawPointer, RawPointer)
  SIMPLE(BuiltinVector, Trivial)
  SIMPLE(SILToken, Trivial)
  SIMPLE(AnyMetatype, Trivial)
  SIMPLE(Module, Trivial)
  SIMPLE(SILBox, Reference)

  IMPOSSIBLE(Pack)
  IMPOSSIBLE(LValue)
  IMPOSSIBLE(InOut)
  IMPOSSIBLE(Error)
  IMPOSSIBLE(BuiltinTuple)

#undef SIMPLE
#undef IMPOSSIBLE

  RecursiveProperties visitSILPackType(CanSILPackType type,
                                       UseCache_t shouldUseCache) {
    RecursiveProperties props;
    props.setAddressOnly();
    for (auto eltType : type.getElementTypes()) {
      props.addSubobject(visit(eltType, UseCache));
    }
    return props;
  }

  RecursiveProperties visitPackExpansionType(CanPackExpansionType type,
                                             UseCache_t shouldUseCache) {
    return visit(type.getPatternType(), UseCache);
  }

  RecursiveProperties visitBuiltinUnsafeValueBufferType(
                                     CanBuiltinUnsafeValueBufferType type,
                                     UseCache_t shouldUseCache) {
    return {IsNotTrivial, IsFixedABI, IsAddressOnly, IsNotResilient,
            IsNotTypeExpansionSensitive, DoesNotHaveRawPointer, IsLexical};
  }

  RecursiveProperties visitBuiltinDefaultActorStorageType(
                                       CanBuiltinDefaultActorStorageType type,
                                       UseCache_t shouldUseCache) {
    return {IsNotTrivial, IsFixedABI, IsAddressOnly, IsNotResilient,
            IsNotTypeExpansionSensitive, DoesNotHaveRawPointer, IsLexical};
  }

  RecursiveProperties visitAnyFunctionType(CanAnyFunctionType type,
                                           UseCache_t shouldUseCache) {
    // Differentiable functions are effectively just tuples of functions
    // and don't impact the recursive properties.

    if (type->hasExtInfo() &&
        isThinRepresentation(type->getRepresentation()))
      return RecursiveProperties::forTrivial();

    return RecursiveProperties::forReference();
  }

  RecursiveProperties visitSILFunctionType(CanSILFunctionType type,
                                           UseCache_t shouldUseCache) {
    // Differentiable functions are effectively just tuples of functions
    // and don't impact the recursive properties.

    if (type->getExtInfo().hasContext()) {
      // Nonescaping closures ultimately become trivial, but we give them
      // ownership semantics to do borrow checking on their captures, so we
      // lower them as reference types. Passes will eliminate ownership
      // operations on them.
      //
      // TODO: Nonescaping closures should also be move-only to ensure we
      // eliminate copies.
      return RecursiveProperties::forReference();
    }
    
    // Contextless function references are trivial types.
    return RecursiveProperties::forTrivial();
  }

  Type getConcreteReferenceStorageReferent(CanType type) {
    if (type->isTypeParameter()) {
      auto genericSig = TC.getCurGenericSignature();
      assert(genericSig);
      if (auto concreteType = genericSig->getConcreteType(type))
        return concreteType;
      if (auto superclassType = genericSig->getSuperclassBound(type))
        return superclassType;
      assert(genericSig->requiresClass(type));
      return TC.Context.getAnyObjectType();
    }

    return type;
  }

#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  RecursiveProperties visit##Name##StorageType(Can##Name##StorageType type, \
                                               UseCache_t shouldUseCache) { \
    return {IsNotTrivial, \
            IsFixedABI, \
            IsAddressOnly, \
            IsNotResilient, \
            IsNotTypeExpansionSensitive, \
            DoesNotHaveRawPointer, \
            IsLexical}; \
  }
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  RecursiveProperties visit##Name##StorageType(Can##Name##StorageType type, \
                                               UseCache_t shouldUseCache) { \
    return RecursiveProperties::forReference(); \
  }
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  RecursiveProperties \
  visitLoadable##Name##StorageType(Can##Name##StorageType type, \
                                   UseCache_t shouldUseCache) { \
    return RecursiveProperties::forReference(); \
  } \
  RecursiveProperties \
  visitAddressOnly##Name##StorageType(Can##Name##StorageType type, \
                                      UseCache_t shouldUseCache) { \
    return {IsNotTrivial, \
            IsFixedABI, \
            IsAddressOnly, \
            IsNotResilient, \
            IsNotTypeExpansionSensitive, \
            DoesNotHaveRawPointer, \
            IsLexical}; \
  } \
  RecursiveProperties visit##Name##StorageType(Can##Name##StorageType type, \
                                               UseCache_t shouldUseCache) { \
    auto referentType = type.getReferentType(); \
    if (auto objectType = referentType.getOptionalObjectType()) \
      referentType = objectType; \
    auto concreteType = getConcreteReferenceStorageReferent(referentType); \
    if (Name##StorageType::get(concreteType, TC.Context) \
          ->isLoadable(Expansion.getResilienceExpansion())) { \
      return visitLoadable##Name##StorageType(type, shouldUseCache); \
    } else { \
      return visitAddressOnly##Name##StorageType(type, shouldUseCache); \
    } \
  }
#define UNCHECKED_REF_STORAGE(Name, ...) \
  RecursiveProperties \
  visit##Name##StorageType(Can##Name##StorageType type, \
                           UseCache_t shouldUseCache) { \
    return RecursiveProperties::forTrivial(); \
  }
#include "swift/AST/ReferenceStorage.def"

  RecursiveProperties
  visitAbstractTypeParamType(CanType type, UseCache_t shouldUseCache) {
    assert(type->isTypeParameter());

    auto signature = TC.getCurGenericSignature();
    assert(signature && "lowering an interface type without having "
                        "entered a GenericContextRAII");

    auto requirements = signature->getLocalRequirements(type);

    // This should use the same logic as visitArchetypeType.

    if (requirements.concreteType) {
      return visit(requirements.concreteType->getCanonicalType(), UseCache);
    }

    if (requirements.superclass) {
      return visit(requirements.superclass->getCanonicalType(), UseCache);
    }

    return fromLayoutConstraint(requirements.layout);
  }

  RecursiveProperties visitGenericTypeParamType(CanGenericTypeParamType type,
                                                UseCache_t shouldUseCache) {
    return visitAbstractTypeParamType(type, shouldUseCache);
  }
  RecursiveProperties visitDependentMemberType(CanDependentMemberType type,
                                               UseCache_t shouldUseCache) {
    return visitAbstractTypeParamType(type, shouldUseCache);
  }
  RecursiveProperties
  visitOpaqueTypeArchetypeType(CanOpaqueTypeArchetypeType ty,
                               UseCache_t shouldUseCache) {
    auto props = [&] {
      auto replacedTy = substOpaqueTypesWithUnderlyingTypes(ty, Expansion);
      if (replacedTy == ty)
        return visitArchetypeType(ty, shouldUseCache);
      return visit(replacedTy, shouldUseCache);
    }();
    props.setTypeExpansionSensitive(IsTypeExpansionSensitive);
    return props;
  }

  RecursiveProperties visitArchetypeType(CanArchetypeType type,
                                         UseCache_t shouldUseCache) {
    // This should use the same logic as visitAbstractTypeParamType.

    if (auto superclass = type->getSuperclass()) {
      return visit(superclass->getCanonicalType(), UseCache);
    }

    return fromLayoutConstraint(type->getLayoutConstraint());
  }

  RecursiveProperties fromLayoutConstraint(LayoutConstraint layout) {
    if (!layout) {
      return RecursiveProperties::forOpaque();
    }

    if (layout->isFixedSizeTrivial()) {
      return RecursiveProperties::forTrivial();
    }

    if (layout->isAddressOnlyTrivial()) {
      auto properties = RecursiveProperties::forTrivial();
      properties.setAddressOnly();
      return properties;
    }

    if (layout->isRefCounted()) {
      return RecursiveProperties::forReference();
    }

    return RecursiveProperties::forOpaque();
  }

  RecursiveProperties visitExistentialType(CanType type,
                                           UseCache_t shouldUseCache) {
    switch (SILType::getPrimitiveObjectType(type)
              .getPreferredExistentialRepresentation()) {
    case ExistentialRepresentation::None:
      llvm_unreachable("not an existential type?!");
    // Opaque existentials are address-only.
    case ExistentialRepresentation::Opaque:
      return {IsNotTrivial,
              IsFixedABI,
              IsAddressOnly,
              IsNotResilient,
              IsNotTypeExpansionSensitive,
              DoesNotHaveRawPointer,
              IsLexical};
    // Class-constrained and boxed existentials are refcounted.
    case ExistentialRepresentation::Class:
    case ExistentialRepresentation::Boxed:
      return RecursiveProperties::forReference();
    // Existential metatypes are trivial.
    case ExistentialRepresentation::Metatype:
      return RecursiveProperties::forTrivial();
    }

    llvm_unreachable("Unhandled ExistentialRepresentation in switch.");
  }
  RecursiveProperties visitProtocolType(CanProtocolType type,
                                        UseCache_t shouldUseCache) {
    return visitExistentialType(type, shouldUseCache);
  }
  RecursiveProperties
  visitProtocolCompositionType(CanProtocolCompositionType type,
                               UseCache_t shouldUseCache) {
    return visitExistentialType(type, shouldUseCache);
  }
  RecursiveProperties
  visitParameterizedProtocolType(CanParameterizedProtocolType type,
                                 UseCache_t shouldUseCache) {
    return visitExistentialType(type, shouldUseCache);
  }

  bool handleResilience(NominalTypeDecl *D,
                        RecursiveProperties &properties) {
    if (D->isResilient()) {
      // If the type is resilient and defined in our module, make a note of
      // that, since our lowering now depends on the resilience expansion.
      bool sameModule = (D->getModuleContext() == &TC.M);
      if (sameModule)
        properties.addSubobject(RecursiveProperties::forResilient());

      // If the type is in a different module, or if we're using a minimal
      // expansion, the type is address only and completely opaque to us.
      //
      // Note: if the type is in a different module, the lowering does
      // not depend on the resilience expansion, so we do not need to set
      // the isResilient() flag above.
      if (!sameModule || Expansion.getResilienceExpansion() ==
                             ResilienceExpansion::Minimal) {
        properties.addSubobject(RecursiveProperties::forOpaque());
        return true;
      }
    }

    return false;
  }

  void handleInfinite(RecursiveProperties &properties) {
    properties.setAddressOnly();
    properties.setInfinite();
  }

  // Classes depend on their attributes.
  RecursiveProperties visitClassType(CanClassType type,
                                     UseCache_t shouldUseCache) {
    return visitAnyClassType(type, type->getDecl());
  }
  RecursiveProperties
  visitBoundGenericClassType(CanBoundGenericClassType type,
                             UseCache_t shouldUseCache) {
    return visitAnyClassType(type, type->getDecl());
  }
  RecursiveProperties
  visitAnyClassType(CanType type, ClassDecl *D) {
    // Foreign references can have trivial reference counting.
    if (D->isForeignReferenceType()) {
      if (type->getReferenceCounting() == ReferenceCounting::None)
        return RecursiveProperties::forTrivial();
    }

    auto properties = RecursiveProperties::forReference();
    if (D->getLifetimeAnnotation() == LifetimeAnnotation::EagerMove)
      properties.setLexical(IsNotLexical);
    return properties;
  }

  // Enums depend on their enumerators.
  RecursiveProperties visitEnumType(CanEnumType type,
                                    UseCache_t shouldUseCache) {
    return visitAnyEnumType(type, type->getDecl(), shouldUseCache);
  }
  RecursiveProperties visitBoundGenericEnumType(CanBoundGenericEnumType type,
                                                UseCache_t shouldUseCache) {
    return visitAnyEnumType(type, type->getDecl(), shouldUseCache);
  }
  RecursiveProperties visitAnyEnumType(CanType enumType, EnumDecl *D,
                                       UseCache_t shouldUseCache) {
    // We have to look through optionals here without grabbing the
    // type lowering because the way that optionals are reabstracted
    // can trip recursion checks if we try to build a lowered type.
    if (D->isOptionalDecl()) {
      return visit(enumType.getOptionalObjectType(), UseCache);
    }

    if (shouldUseCache && canUseCache(enumType))
      return getFromCache(enumType, /*known lowered*/ true);

    RecursiveProperties properties;

    // Bail out if the enum layout relies on itself.
    TypeConverter::LowerAggregateTypeRAII loweringEnum(TC, enumType);
    if (loweringEnum.IsInfinite) {
      handleInfinite(properties);
      return properties;
    }

    // Update for resilience, and stop if we have an opaque resilient type.
    if (handleResilience(D, properties)) {
      return properties;
    }

    if (D->isMoveOnly()) {
      properties.setNonTrivial();
      properties.setMoveOnly();
    }

    // If the whole enum is indirect, we lower it as if all payload
    // cases were indirect. This means a fixed-layout indirect enum
    // is always loadable and nontrivial. A resilient indirect enum
    // is still address only, because we don't know how many bits
    // are used for the discriminator, and new non-indirect cases
    // may be added resiliently later.
    if (D->isIndirect()) {
      properties.setNonTrivial();
      properties.setLexical(IsLexical);
      handleLifetimeAnnotation(D->getLifetimeAnnotation(), properties);
      return properties;
    }

    auto subMap = enumType->getContextSubstitutionMap(&TC.M, D);

    // Accumulate the properties of all payloads.
    for (auto elt : D->getAllElements()) {
      // No-payload elements do not affect any recursive properties.
      if (!elt->hasAssociatedValues())
        continue;

      RecursiveProperties eltProps;

      // Indirect elements are references no matter what the payload is.
      if (elt->isIndirect()) {
        eltProps = RecursiveProperties::forReference();

      // Otherwise, substitute the payload type.  We need to do this
      // without triggering type lowering, both for efficiency and in order
      // to avoid tripping the infinite-type check.
      } else {
        auto payloadEltType =
          elt->getArgumentInterfaceType().subst(subMap)->getCanonicalType();
        eltProps = visit(payloadEltType, UseCache);
      }

      handleLifetimeAnnotation(elt->getLifetimeAnnotation(), eltProps);
      properties.addSubobject(eltProps);
    }

    // Type-level annotations override inferred behavior.
    handleLifetimeAnnotation(D->getLifetimeAnnotation(), properties);

    return properties;
  }

  // Structs depend on their physical fields.
  RecursiveProperties visitStructType(CanStructType type,
                                      UseCache_t shouldUseCache) {
    return visitAnyStructType(type, type->getDecl(), shouldUseCache);
  }
  RecursiveProperties
  visitBoundGenericStructType(CanBoundGenericStructType type,
                              UseCache_t shouldUseCache) {
    return visitAnyStructType(type, type->getDecl(), shouldUseCache);
  }
  RecursiveProperties visitAnyStructType(CanType structType, StructDecl *D,
                                         UseCache_t shouldUseCache) {
    if (shouldUseCache && canUseCache(structType))
      return getFromCache(structType, /*known lowered*/ true);

    RecursiveProperties properties;

    // Bail out if the struct layout relies on itself.
    TypeConverter::LowerAggregateTypeRAII loweringStruct(TC, structType);
    if (loweringStruct.IsInfinite) {
      handleInfinite(properties);
      return properties;
    }

    // Update for resilience, and stop if we have an opaque resilient type.
    if (handleResilience(D, properties))
      return properties;

    if (D->isCxxNonTrivial()) {
      properties.setAddressOnly();
      properties.setNonTrivial();
      properties.setLexical(IsLexical);
    }

    auto subMap = structType->getContextSubstitutionMap(&TC.M, D);

    // Classify the type according to its stored properties.
    for (auto field : D->getStoredProperties()) {
      auto pointerAuthQual = field->getPointerAuthQualifier();
      if (pointerAuthQual.isPresent()) {
        properties.setAddressOnly();
      }

      // Substitute the field type.  We need to do this without triggering
      // type lowering, both for efficiency and in order to avoid tripping
      // the infinite-type check.
      auto fieldType =
        field->getInterfaceType().subst(subMap)->getCanonicalType();

      auto fieldProps = visit(fieldType, UseCache);
      handleLifetimeAnnotation(field->getLifetimeAnnotation(), fieldProps);
      properties.addSubobject(fieldProps);
    }

    // Type-level annotations override inferred behavior.
    handleLifetimeAnnotation(D->getLifetimeAnnotation(), properties);

    if (D->isMoveOnly()) {
      properties.setNonTrivial();
      properties.setMoveOnly();
    }

    return properties;
  }

  // Tuples depend on their elements.
  RecursiveProperties visitTupleType(CanTupleType type,
                                     UseCache_t shouldUseCache) {
    // Walking over a tuple is pretty painless, so only go through
    // the cache if there are a significant number of members.
    // 3 is an arbitrary but probably reasonable threshold.
    if (type->getNumElements() >= 3 && shouldUseCache && canUseCache(type))
      return getFromCache(type, /*known lowered*/ false);

    CanType lastEltType;
    RecursiveProperties props;
    for (auto eltType : type.getElementTypes()) {
      // Avoid doing redundant work when there are sequences of elements
      // with  the same type.  This is a significant optimization in
      // practice when working with the large homogeneous tuples that
      // the importer likes to produce for C array types.
      if (eltType == lastEltType) continue;
      lastEltType = eltType;

      props.addSubobject(visit(eltType, UseCache));
    }
    return props;
  }

  RecursiveProperties visitDynamicSelfType(CanDynamicSelfType type,
                                           UseCache_t shouldUseCache) {
    return visit(type.getSelfType(), UseCache);
  }

  RecursiveProperties
  visitSILBlockStorageType(CanSILBlockStorageType type,
                           UseCache_t shouldUseCache) {
    // Should not be loaded.
    return {IsNotTrivial,
            IsFixedABI,
            IsAddressOnly,
            IsNotResilient,
            IsNotTypeExpansionSensitive,
            DoesNotHaveRawPointer,
            IsLexical};
  }

  RecursiveProperties
  visitSILMoveOnlyWrappedType(CanSILMoveOnlyWrappedType type,
                              UseCache_t shouldUseCache) {
    CanType innerType = type->getInnerType();
    auto innerProps = visit(innerType, UseCache);
    if (innerProps.isAddressOnly()) {
      return RecursiveProperties::forOpaque();
    }

    return RecursiveProperties::forReference();
  }
};
} // end anonymous namespace

static RecursiveProperties
computeTypeProperties(TypeConverter &tc, CanType loweredType,
                      TypeExpansionContext expansion,
                      UseCache_t shouldUseCache) {
  return ComputeTypeProperties(tc, expansion)
           .visit(loweredType, shouldUseCache);
}

static CanType computeLoweredRValueType(TypeConverter &TC,
                                        AbstractionPattern origType,
                                        CanType substType,
                                        TypeExpansionContext forExpansion,
                                        UseCache_t shouldUseCache);

/// True if the type, or the referenced type of an address
/// type, is address-only.  For example, it could be a resilient struct or
/// something of unknown size.
bool SILType::isAddressOnly(CanType type,
                            TypeConverter &tc,
                            CanGenericSignature sig,
                            TypeExpansionContext expansion) {
  Optional<TypeConverter::GenericContextRAII> scope;
  if (sig) scope.emplace(tc, sig);

  return tc.getTypeProperties(type, expansion).isAddressOnly();
}

namespace {
  /// A class for types that can be loaded and stored in SIL.
  /// This always include loadable types, but can include address-only types if
  /// opaque values are passed by value.
  class LoadableTypeLowering : public TypeLowering {
  protected:
    LoadableTypeLowering(SILType type, RecursiveProperties properties,
                         IsReferenceCounted_t isRefCounted,
                         TypeExpansionContext forExpansion)
      : TypeLowering(type, properties, isRefCounted, forExpansion) {}

  public:
    void emitDestroyAddress(SILBuilder &B, SILLocation loc,
                            SILValue addr) const override {
      SILValue value = emitLoad(B, loc, addr, LoadOwnershipQualifier::Take);
      emitDestroyValue(B, loc, value);
    }

    void emitDestroyRValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      emitDestroyValue(B, loc, value);
    }

    void emitCopyInto(SILBuilder &B, SILLocation loc,
                      SILValue src, SILValue dest, IsTake_t isTake,
                      IsInitialization_t isInit) const override {
      SILValue value = emitLoadOfCopy(B, loc, src, isTake);
      emitStoreOfCopy(B, loc, value, dest, isInit);
    }
  };

  /// A class for trivial, fixed-layout, loadable types.
  class TrivialTypeLowering final : public LoadableTypeLowering {
  public:
    TrivialTypeLowering(SILType type, RecursiveProperties properties,
                        TypeExpansionContext forExpansion)
      : LoadableTypeLowering(type, properties, IsNotReferenceCounted,
                             forExpansion) {
      assert(properties.isFixedABI());
      assert(properties.isTrivial());
      assert(!properties.isAddressOnly());
    }

    SILValue emitLoadOfCopy(SILBuilder &B, SILLocation loc, SILValue addr,
                            IsTake_t isTake) const override {
      return emitLoad(B, loc, addr, LoadOwnershipQualifier::Trivial);
    }

    void emitStoreOfCopy(SILBuilder &B, SILLocation loc,
                         SILValue value, SILValue addr,
                         IsInitialization_t isInit) const override {
      emitStore(B, loc, value, addr, StoreOwnershipQualifier::Trivial);
    }

    void emitStore(SILBuilder &B, SILLocation loc, SILValue value,
                   SILValue addr, StoreOwnershipQualifier qual) const override {
      if (B.getFunction().hasOwnership()) {
        B.createStore(loc, value, addr, StoreOwnershipQualifier::Trivial);
        return;
      }
      B.createStore(loc, value, addr, StoreOwnershipQualifier::Unqualified);
    }

    SILValue emitLoad(SILBuilder &B, SILLocation loc, SILValue addr,
                      LoadOwnershipQualifier qual) const override {
      if (B.getFunction().hasOwnership())
        return B.createLoad(loc, addr, LoadOwnershipQualifier::Trivial);
      return B.createLoad(loc, addr, LoadOwnershipQualifier::Unqualified);
    }

    SILValue emitLoweredLoad(SILBuilder &B, SILLocation loc, SILValue addr,
                             LoadOwnershipQualifier qual,
                             TypeExpansionKind) const override {
      if (B.getFunction().hasOwnership())
        return B.createLoad(loc, addr, LoadOwnershipQualifier::Trivial);
      return B.createLoad(loc, addr, LoadOwnershipQualifier::Unqualified);
    }

    void emitLoweredStore(SILBuilder &B, SILLocation loc, SILValue value,
                          SILValue addr, StoreOwnershipQualifier qual,
                          Lowering::TypeLowering::TypeExpansionKind
                              expansionKind) const override {
      auto storeQual = [&]() -> StoreOwnershipQualifier {
        if (B.getFunction().hasOwnership())
          return StoreOwnershipQualifier::Trivial;
        return StoreOwnershipQualifier::Unqualified;
      }();
      B.createStore(loc, value, addr, storeQual);
    }

    void emitDestroyAddress(SILBuilder &B, SILLocation loc,
                            SILValue addr) const override {
      // Trivial
    }

    void
    emitLoweredDestroyValue(SILBuilder &B, SILLocation loc, SILValue value,
                            TypeExpansionKind loweringStyle) const override {
      // Trivial
    }

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  TypeExpansionKind style) const override {
      // Trivial
      return value;
    }

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      // Trivial
      return value;
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      // Trivial
    }
  };

  class NonTrivialLoadableTypeLowering : public LoadableTypeLowering {
  public:
    NonTrivialLoadableTypeLowering(SILType type,
                                   RecursiveProperties properties,
                                   IsReferenceCounted_t isRefCounted,
                                   TypeExpansionContext forExpansion)
      : LoadableTypeLowering(type, properties, isRefCounted, forExpansion) {
      assert(!properties.isTrivial());
    }

    SILValue emitLoadOfCopy(SILBuilder &B, SILLocation loc,
                            SILValue addr, IsTake_t isTake) const override {
      auto qual =
          isTake ? LoadOwnershipQualifier::Take : LoadOwnershipQualifier::Copy;
      return emitLoad(B, loc, addr, qual);
    }

    void emitStoreOfCopy(SILBuilder &B, SILLocation loc,
                         SILValue newValue, SILValue addr,
                         IsInitialization_t isInit) const override {
      auto qual = isInit ? StoreOwnershipQualifier::Init
                         : StoreOwnershipQualifier::Assign;
      emitStore(B, loc, newValue, addr, qual);
    }

    void emitStore(SILBuilder &B, SILLocation loc, SILValue value,
                   SILValue addr, StoreOwnershipQualifier qual) const override {
      if (B.getFunction().hasOwnership()) {
        B.createStore(loc, value, addr, qual);
        return;
      }

      if (qual != StoreOwnershipQualifier::Assign) {
        B.createStore(loc, value, addr, StoreOwnershipQualifier::Unqualified);
        return;
      }

      // If the ownership qualifier is [assign], then we need to eliminate the
      // old value.
      //
      // 1. Load old value.
      // 2. Store new value.
      // 3. Release old value.
      SILValue old =
          B.createLoad(loc, addr, LoadOwnershipQualifier::Unqualified);
      B.createStore(loc, value, addr, StoreOwnershipQualifier::Unqualified);
      B.emitDestroyValueOperation(loc, old);
    }

    SILValue emitLoad(SILBuilder &B, SILLocation loc, SILValue addr,
                      LoadOwnershipQualifier qual) const override {
      if (B.getFunction().hasOwnership())
        return B.createLoad(loc, addr, qual);

      SILValue loadValue =
          B.createLoad(loc, addr, LoadOwnershipQualifier::Unqualified);

      // If we do not have a copy, just return the value...
      if (qual != LoadOwnershipQualifier::Copy)
        return loadValue;

      // Otherwise, emit the copy value operation and return our original
      // value. This is a small non-ownership optimization to not destabilize
      // the optimizer pipeline.
      //
      // TODO: Once the pass pipeline is fixed, we should evaluate if we can do
      // this again.
      B.emitCopyValueOperation(loc, loadValue);
      return loadValue;
    }

    SILValue emitLoweredLoad(SILBuilder &B, SILLocation loc, SILValue addr,
                             LoadOwnershipQualifier qual,
                             TypeExpansionKind expansionKind) const override {
      if (B.getFunction().hasOwnership())
        return B.createLoad(loc, addr, qual);

      SILValue loadValue =
          B.createLoad(loc, addr, LoadOwnershipQualifier::Unqualified);

      // If we do not have a copy, just return the value...
      if (qual != LoadOwnershipQualifier::Copy)
        return loadValue;

      // Otherwise, emit the copy value operation.
      B.emitLoweredCopyValueOperation(loc, loadValue, expansionKind);

      // Otherwise, emit the copy value operation and return our original
      // value. This is a small non-ownership optimization to not destabilize
      // the optimizer pipeline.
      //
      // TODO: Once the pass pipeline is fixed, we should evaluate if we can do
      // this again.
      return loadValue;
    }

    void emitLoweredStore(SILBuilder &B, SILLocation loc, SILValue value,
                          SILValue addr, StoreOwnershipQualifier qual,
                          Lowering::TypeLowering::TypeExpansionKind
                              expansionKind) const override {
      if (B.getFunction().hasOwnership()) {
        B.createStore(loc, value, addr, qual);
        return;
      }

      if (qual == StoreOwnershipQualifier::Assign) {
        SILValue oldValue = B.emitLoadValueOperation(
            loc, addr, LoadOwnershipQualifier::Unqualified);
        B.emitLoweredDestroyValueOperation(loc, oldValue, expansionKind);
      }
      B.createStore(loc, value, addr, StoreOwnershipQualifier::Unqualified);
    }
  };

  /// A CRTP helper class for loadable but non-trivial aggregate types.
  template <class Impl, class IndexType>
  class LoadableAggTypeLowering : public NonTrivialLoadableTypeLowering {
  public:
    /// A child of this aggregate type.
    class Child {
      /// The index of this child, used to project it out.
      IndexType Index;

      /// The aggregate's type lowering.
      const TypeLowering *Lowering;
    public:
      Child(IndexType index, const TypeLowering &lowering)
        : Index(index), Lowering(&lowering) {}
      const TypeLowering &getLowering() const { return *Lowering; }
      IndexType getIndex() const { return Index; }
      bool isTrivial() const { return Lowering->isTrivial(); }
    };

  private:
    const Impl &asImpl() const { return static_cast<const Impl&>(*this); }
    Impl &asImpl() { return static_cast<Impl&>(*this); }

    // A reference to the lazily-allocated children vector.
    mutable ArrayRef<Child> Children = {};
    
  protected:
    virtual void lowerChildren(TypeConverter &TC, SmallVectorImpl<Child> &children)
      const = 0;
    
  public:
    LoadableAggTypeLowering(CanType type, RecursiveProperties properties,
                            TypeExpansionContext forExpansion)
      : NonTrivialLoadableTypeLowering(SILType::getPrimitiveObjectType(type),
                                       properties, IsNotReferenceCounted,
                                       forExpansion) {
    }

    /// CRTP Default implementation of destructuring an aggregate value.
    ///
    /// Uses getChildren() and emitRValueProject() to create projections for
    /// each child. Subclasses should override this to customize on how
    /// destructuring is done.
    ///
    /// NOTE: Due to the CRTP, this must always be called as
    /// asImpl().destructureAggregate() to ensure that one gets the proper
    /// implementation!
    void destructureAggregate(
        SILBuilder &B, SILLocation loc, SILValue aggValue, bool skipTrivial,
        function_ref<void(unsigned, SILValue, const TypeLowering &)> visitor)
        const {
      for (auto pair : llvm::enumerate(getChildren(B.getModule().Types))) {
        auto &child = pair.value();
        auto &childLowering = child.getLowering();
        // Skip trivial children.
        if (skipTrivial && childLowering.isTrivial())
          continue;
        auto childIndex = child.getIndex();
        auto childValue = asImpl().emitRValueProject(B, loc, aggValue,
                                                     childIndex, childLowering);
        visitor(pair.index(), childValue, childLowering);
      }
    }

    virtual SILValue rebuildAggregate(SILBuilder &B, SILLocation loc,
                                      ArrayRef<SILValue> values) const = 0;

    ArrayRef<Child> getChildren(TypeConverter &TC) const {
      if (Children.data() == nullptr) {
        SmallVector<Child, 4> children;
        lowerChildren(TC, children);
        auto buf = operator new(sizeof(Child) * children.size(), TC);
        memcpy(buf, children.data(), sizeof(Child) * children.size());
        Children = {reinterpret_cast<Child*>(buf), children.size()};
      }
      return Children;
    }

    template <class T>
    void forEachNonTrivialChild(SILBuilder &B, SILLocation loc,
                                SILValue aggValue,
                                const T &operation) const {
      asImpl().destructureAggregate(B, loc, aggValue, true /*skipTrivial*/,
                                    [&](unsigned, SILValue childValue,
                                        const TypeLowering &childLowering) {
                                      operation(B, loc, childValue,
                                                childLowering);
                                    });
    }

    using SimpleOperationTy = void (TypeLowering::*)(SILBuilder &B,
                                                     SILLocation loc,
                                                     SILValue value) const;
    void forEachNonTrivialChild(SILBuilder &B, SILLocation loc,
                                SILValue aggValue,
                                SimpleOperationTy operation) const {
      forEachNonTrivialChild(B, loc, aggValue,
                             [operation](SILBuilder &B, SILLocation loc,
                                         SILValue childValue,
                                         const TypeLowering &childLowering) {
                               (childLowering.*operation)(B, loc, childValue);
                             });
    }

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      if (B.getFunction().hasOwnership())
        return B.createCopyValue(loc, value);
      B.createRetainValue(loc, value, B.getDefaultAtomicity());
      return value;
    }

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue aggValue,
                                  TypeExpansionKind style) const override {
      if (style == TypeExpansionKind::None) {
        return emitCopyValue(B, loc, aggValue);
      }

      SmallVector<SILValue, 8> loweredChildValues;
      asImpl().destructureAggregate(
          B, loc, aggValue, false /*skipTrivial*/,
          [&](unsigned childIndex, SILValue childValue,
              const TypeLowering &childLowering) {
            if (!childLowering.isTrivial())
              childValue = childLowering.emitLoweredCopyChildValue(
                  B, loc, childValue, style);
            loweredChildValues.push_back(childValue);
          });

      // Without ownership, return our original value. This is a small
      // non-ownership optimization to not destabilize the optimizer pipeline.
      //
      // TODO: Once the pass pipeline is fixed, we should evaluate if we can do
      // this again.
      if (!B.hasOwnership())
        return aggValue;
      return rebuildAggregate(B, loc, loweredChildValues);
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue aggValue) const override {
      if (B.getFunction().hasOwnership()) {
        B.createDestroyValue(loc, aggValue);
        return;
      }

      B.createReleaseValue(loc, aggValue, B.getDefaultAtomicity());
    }

    void
    emitLoweredDestroyValue(SILBuilder &B, SILLocation loc, SILValue aggValue,
                            TypeExpansionKind loweringStyle) const override {
      SimpleOperationTy Fn;

      switch(loweringStyle) {
      case TypeExpansionKind::None:
        return emitDestroyValue(B, loc, aggValue);
      case TypeExpansionKind::DirectChildren:
        Fn = &TypeLowering::emitDestroyValue;
        break;
      case TypeExpansionKind::MostDerivedDescendents:
        Fn = &TypeLowering::emitLoweredDestroyValueMostDerivedDescendents;
        break;
      }

      forEachNonTrivialChild(B, loc, aggValue, Fn);
    }
  };

  /// A lowering for loadable but non-trivial tuple types.
  class LoadableTupleTypeLowering final
      : public LoadableAggTypeLowering<LoadableTupleTypeLowering, unsigned> {
    using Super = LoadableAggTypeLowering<LoadableTupleTypeLowering, unsigned>;

  public:
    LoadableTupleTypeLowering(CanType type, RecursiveProperties properties,
                              TypeExpansionContext forExpansion)
      : LoadableAggTypeLowering(type, properties, forExpansion) {}

    SILValue emitRValueProject(SILBuilder &B, SILLocation loc,
                               SILValue tupleValue, unsigned index,
                               const TypeLowering &eltLowering) const {
      assert(!B.hasOwnership() &&
             "Shouldn't call this when ownership is enabled?! Destructure "
             "non-trivial tuples instead");
      return B.createTupleExtract(loc, tupleValue, index,
                                  eltLowering.getLoweredType());
    }

    void destructureAggregate(
        SILBuilder &B, SILLocation loc, SILValue aggValue, bool skipTrivial,
        function_ref<void(unsigned childIndex, SILValue childValue,
                          const TypeLowering &childLowering)>
            visitor) const {
      // Without ownership, use our parent.
      if (!B.hasOwnership())
        return Super::destructureAggregate(B, loc, aggValue, skipTrivial,
                                           visitor);

      // Otherwise, emit a destructure tuple and do the loop.
      auto *dti = B.createDestructureTuple(loc, aggValue);
      for (auto pair : llvm::enumerate(dti->getResults())) {
        SILValue childValue = pair.value();
        auto &childLowering =
            B.getFunction().getTypeLowering(childValue->getType());
        if (skipTrivial && childLowering.isTrivial())
          continue;
        visitor(pair.index(), childValue, childLowering);
      }
    }

    SILValue rebuildAggregate(SILBuilder &B, SILLocation loc,
                              ArrayRef<SILValue> values) const override {
      return B.createTuple(loc, getLoweredType(), values);
    }
  
  private:
    void lowerChildren(TypeConverter &TC, SmallVectorImpl<Child> &children)
    const override {
      // The children are just the elements of the lowered tuple.
      auto silTy = getLoweredType();
      auto tupleTy = silTy.castTo<TupleType>();
      children.reserve(tupleTy->getNumElements());
      unsigned index = 0;
      for (auto elt : tupleTy.getElementTypes()) {
        auto silElt = SILType::getPrimitiveType(elt, silTy.getCategory());
        auto &eltTL = TC.getTypeLowering(silElt, getExpansionContext());
        children.push_back(Child{index, eltTL});
        ++index;
      }
    }
  };

  /// A lowering for loadable but non-trivial struct types.
  class LoadableStructTypeLowering final
      : public LoadableAggTypeLowering<LoadableStructTypeLowering, VarDecl *> {
    using Super =
        LoadableAggTypeLowering<LoadableStructTypeLowering, VarDecl *>;

  public:
    LoadableStructTypeLowering(CanType type, RecursiveProperties properties,
                               TypeExpansionContext forExpansion)
      : LoadableAggTypeLowering(type, properties, forExpansion) {}

    SILValue emitRValueProject(SILBuilder &B, SILLocation loc,
                               SILValue structValue, VarDecl *field,
                               const TypeLowering &fieldLowering) const {
      return B.createStructExtract(loc, structValue, field,
                                   fieldLowering.getLoweredType());
    }

    void destructureAggregate(
        SILBuilder &B, SILLocation loc, SILValue aggValue, bool skipTrivial,
        function_ref<void(unsigned childIndex, SILValue childValue,
                          const TypeLowering &childLowering)>
            visitor) const {
      if (!B.hasOwnership())
        return Super::destructureAggregate(B, loc, aggValue, skipTrivial,
                                           visitor);

      auto *dsi = B.createDestructureStruct(loc, aggValue);
      for (auto pair : llvm::enumerate(dsi->getResults())) {
        SILValue childValue = pair.value();
        auto &childLowering =
            B.getFunction().getTypeLowering(childValue->getType());
        if (skipTrivial && childLowering.isTrivial())
          continue;
        visitor(pair.index(), childValue, childLowering);
      }
    }

    SILValue rebuildAggregate(SILBuilder &B, SILLocation loc,
                              ArrayRef<SILValue> values) const override {
      return B.createStruct(loc, getLoweredType(), values);
    }
        
  private:
    void lowerChildren(TypeConverter &TC, SmallVectorImpl<Child> &children)
    const override {
      auto silTy = getLoweredType();
      auto structDecl = silTy.getStructOrBoundGenericStruct();
      assert(structDecl);
      
      for (auto prop : structDecl->getStoredProperties()) {
        SILType propTy = silTy.getFieldType(prop, TC, getExpansionContext());
        auto &propTL = TC.getTypeLowering(propTy, getExpansionContext());
        children.push_back(Child{prop, propTL});
      }
    }
  };

  /// A lowering for loadable but non-trivial enum types.
  class LoadableEnumTypeLowering final : public NonTrivialLoadableTypeLowering {
  public:
    LoadableEnumTypeLowering(CanType type, RecursiveProperties properties,
                             TypeExpansionContext forExpansion)
      : NonTrivialLoadableTypeLowering(SILType::getPrimitiveObjectType(type),
                                       properties,
                                       IsNotReferenceCounted,
                                       forExpansion) {}

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      if (B.getFunction().hasOwnership())
        return B.createCopyValue(loc, value);
      B.createRetainValue(loc, value, B.getDefaultAtomicity());
      return value;
    }

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  TypeExpansionKind style) const override {
      if (B.getFunction().hasOwnership())
        return B.createCopyValue(loc, value);
      B.createRetainValue(loc, value, B.getDefaultAtomicity());
      return value;
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      if (B.getFunction().hasOwnership()) {
        B.createDestroyValue(loc, value);
        return;
      }
      B.createReleaseValue(loc, value, B.getDefaultAtomicity());
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc, SILValue value,
                                 TypeExpansionKind style) const override {
      // Enums, we never want to expand.
      return emitDestroyValue(B, loc, value);
    }
  };

  /// A type lowering for `@differentiable` function types.
  class NormalDifferentiableSILFunctionTypeLowering final
      : public LoadableAggTypeLowering<
                   NormalDifferentiableSILFunctionTypeLowering,
                   NormalDifferentiableFunctionTypeComponent> {
  public:
    using LoadableAggTypeLowering::LoadableAggTypeLowering;

    SILValue emitRValueProject(
        SILBuilder &B, SILLocation loc, SILValue tupleValue,
        NormalDifferentiableFunctionTypeComponent extractee,
        const TypeLowering &eltLowering) const {
      return B.createDifferentiableFunctionExtract(
          loc, extractee, tupleValue);
    }

    SILValue rebuildAggregate(SILBuilder &B, SILLocation loc,
                              ArrayRef<SILValue> values) const override {
      assert(values.size() == 3);
      auto fnTy = getLoweredType().castTo<SILFunctionType>();
      auto *parameterIndices = fnTy->getDifferentiabilityParameterIndices();
      auto *resultIndices = fnTy->getDifferentiabilityResultIndices();
      return B.createDifferentiableFunction(
          loc, parameterIndices, resultIndices, values[0],
          std::make_pair(values[1], values[2]));
    }

    void lowerChildren(TypeConverter &TC,
                       SmallVectorImpl<Child> &children) const override {
      auto fnTy = getLoweredType().castTo<SILFunctionType>();
      auto numDerivativeFns = 2;
      children.reserve(numDerivativeFns + 1);
      auto origFnTy = fnTy->getWithoutDifferentiability();
      auto *paramIndices = fnTy->getDifferentiabilityParameterIndices();
      auto *resultIndices = fnTy->getDifferentiabilityResultIndices();
      children.push_back(Child{
        NormalDifferentiableFunctionTypeComponent::Original,
        TC.getTypeLowering(origFnTy, getExpansionContext())
      });
      for (AutoDiffDerivativeFunctionKind kind :
               {AutoDiffDerivativeFunctionKind::JVP,
                AutoDiffDerivativeFunctionKind::VJP}) {
        auto derivativeFnTy = origFnTy->getAutoDiffDerivativeFunctionType(
            paramIndices, resultIndices, kind, TC,
            LookUpConformanceInModule(&TC.M));
        auto silTy = SILType::getPrimitiveObjectType(derivativeFnTy);
        NormalDifferentiableFunctionTypeComponent extractee(kind);
        // Assert that we have the right extractee. A terrible bug in the past
        // was caused by implicit conversions from `unsigned` to
        // `NormalDifferentiableFunctionTypeComponent` which resulted into a
        // wrong extractee.
        assert(extractee.getAsDerivativeFunctionKind() == kind);
        children.push_back(Child{
            extractee, TC.getTypeLowering(silTy, getExpansionContext())});
      }
      assert(children.size() == 3);
    }
  };

  /// A type lowering for `@differentiable(_linear)` function types.
  class LinearDifferentiableSILFunctionTypeLowering final
      : public LoadableAggTypeLowering<
                   LinearDifferentiableSILFunctionTypeLowering,
                   LinearDifferentiableFunctionTypeComponent> {
  public:
    using LoadableAggTypeLowering::LoadableAggTypeLowering;

    SILValue emitRValueProject(
        SILBuilder &B, SILLocation loc, SILValue tupleValue,
        LinearDifferentiableFunctionTypeComponent component,
        const TypeLowering &eltLowering) const {
      return B.createLinearFunctionExtract(loc, component, tupleValue);
    }

    SILValue rebuildAggregate(SILBuilder &B, SILLocation loc,
                              ArrayRef<SILValue> values) const override {
      assert(values.size() == 2);
      auto fnTy = getLoweredType().castTo<SILFunctionType>();
      auto paramIndices = fnTy->getDifferentiabilityParameterIndices();
      return B.createLinearFunction(loc, paramIndices, values[0], values[1]);
    }

    void lowerChildren(TypeConverter &TC,
                       SmallVectorImpl<Child> &children) const override {
      auto fnTy = getLoweredType().castTo<SILFunctionType>();
      children.reserve(2);
      auto origFnTy = fnTy->getWithoutDifferentiability();
      auto paramIndices = fnTy->getDifferentiabilityParameterIndices();
      children.push_back(Child{
        LinearDifferentiableFunctionTypeComponent::Original,
        TC.getTypeLowering(origFnTy, getExpansionContext())
      });
      auto transposeFnTy = origFnTy->getAutoDiffTransposeFunctionType(
          paramIndices, TC, LookUpConformanceInModule(&TC.M));
      auto transposeSILFnTy = SILType::getPrimitiveObjectType(transposeFnTy);
      children.push_back(Child{
        LinearDifferentiableFunctionTypeComponent::Transpose,
        TC.getTypeLowering(transposeSILFnTy, getExpansionContext())
      });
      assert(children.size() == 2);
    }
  };

  class LeafLoadableTypeLowering : public NonTrivialLoadableTypeLowering {
  public:
    LeafLoadableTypeLowering(SILType type, RecursiveProperties properties,
                             IsReferenceCounted_t isRefCounted,
                             TypeExpansionContext forExpansion)
      : NonTrivialLoadableTypeLowering(type, properties, isRefCounted,
                                       forExpansion) {}

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  TypeExpansionKind style) const override {
      return emitCopyValue(B, loc, value);
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc, SILValue value,
                                 TypeExpansionKind style) const override {
      emitDestroyValue(B, loc, value);
    }
  };

  /// A class for reference types, which are all non-trivial but still
  /// loadable.
  class ReferenceTypeLowering : public LeafLoadableTypeLowering {
  public:
    ReferenceTypeLowering(SILType type, RecursiveProperties properties,
                          TypeExpansionContext forExpansion)
        : LeafLoadableTypeLowering(type, properties, IsReferenceCounted,
                                   forExpansion) {}

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      if (isa<FunctionRefInst>(value) || isa<DynamicFunctionRefInst>(value) ||
          isa<PreviousDynamicFunctionRefInst>(value))
        return value;

      if (B.getFunction().hasOwnership())
        return B.createCopyValue(loc, value);

      B.createStrongRetain(loc, value, B.getDefaultAtomicity());
      return value;
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      if (B.getFunction().hasOwnership()) {
        B.createDestroyValue(loc, value);
        return;
      }
      B.createStrongRelease(loc, value, B.getDefaultAtomicity());
    }
  };

  /// A class for move only types which are non-trivial and loadable
  class MoveOnlyReferenceTypeLowering : public LeafLoadableTypeLowering {
  public:
    MoveOnlyReferenceTypeLowering(SILType type, RecursiveProperties properties,
                                  TypeExpansionContext forExpansion)
        : LeafLoadableTypeLowering(type, properties, IsReferenceCounted,
                                   forExpansion) {}

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      if (isa<FunctionRefInst>(value) || isa<DynamicFunctionRefInst>(value) ||
          isa<PreviousDynamicFunctionRefInst>(value))
        return value;

      if (B.getFunction().hasOwnership())
        return B.createCopyValue(loc, value);

      B.createStrongRetain(loc, value, B.getDefaultAtomicity());
      return value;
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      if (B.getFunction().hasOwnership()) {
        B.createDestroyValue(loc, value);
        return;
      }
      B.createStrongRelease(loc, value, B.getDefaultAtomicity());
    }
  };

/// A type lowering for loadable @unowned types.
#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  class Loadable##Name##TypeLowering final : public LeafLoadableTypeLowering { \
  public: \
    Loadable##Name##TypeLowering(SILType type, \
                                 TypeExpansionContext forExpansion, \
                                 RecursiveProperties props) \
      : LeafLoadableTypeLowering(type, props, \
                                 IsReferenceCounted, \
                                 forExpansion) {} \
    SILValue emitCopyValue(SILBuilder &B, SILLocation loc, \
                           SILValue value) const override { \
      if (B.getFunction().hasOwnership()) \
        return B.createCopyValue(loc, value); \
      B.create##Name##Retain(loc, value, B.getDefaultAtomicity()); \
      return value; \
    } \
    void emitDestroyValue(SILBuilder &B, SILLocation loc, \
                          SILValue value) const override { \
      if (B.getFunction().hasOwnership()) { \
        B.createDestroyValue(loc, value); \
        return; \
      } \
      B.create##Name##Release(loc, value, B.getDefaultAtomicity()); \
    } \
  };
#include "swift/AST/ReferenceStorage.def"

  /// A class for non-trivial, address-only types.
  class AddressOnlyTypeLowering : public TypeLowering {
  public:
    AddressOnlyTypeLowering(SILType type, RecursiveProperties properties,
                            TypeExpansionContext forExpansion)
      : TypeLowering(type, properties, IsNotReferenceCounted,
                     forExpansion) {
      assert(properties.isAddressOnly());
    }

    void emitCopyInto(SILBuilder &B, SILLocation loc,
                      SILValue src, SILValue dest, IsTake_t isTake,
                      IsInitialization_t isInit) const override {
      B.createCopyAddr(loc, src, dest, isTake, isInit);
    }

    SILValue emitLoadOfCopy(SILBuilder &B, SILLocation loc,
                            SILValue addr, IsTake_t isTake) const override {
      llvm_unreachable("calling emitLoadOfCopy on non-loadable type");
    }

    void emitStoreOfCopy(SILBuilder &B, SILLocation loc,
                         SILValue newValue, SILValue addr,
                         IsInitialization_t isInit) const override {
      llvm_unreachable("calling emitStoreOfCopy on non-loadable type");
    }

    void emitStore(SILBuilder &B, SILLocation loc, SILValue value,
                   SILValue addr, StoreOwnershipQualifier qual) const override {
      llvm_unreachable("calling emitStore on non-loadable type");
    }

    SILValue emitLoad(SILBuilder &B, SILLocation loc, SILValue addr,
                      LoadOwnershipQualifier qual) const override {
      llvm_unreachable("calling emitLoad on non-loadable type");
    }

    SILValue emitLoweredLoad(SILBuilder &B, SILLocation loc, SILValue addr,
                             LoadOwnershipQualifier qual,
                             Lowering::TypeLowering::TypeExpansionKind
                                 expansionKind) const override {
      llvm_unreachable("calling emitLoweredLoad on non-loadable type?!");
    }

    void emitLoweredStore(SILBuilder &B, SILLocation loc, SILValue value,
                          SILValue addr, StoreOwnershipQualifier qual,
                          Lowering::TypeLowering::TypeExpansionKind
                              expansionKind) const override {
      llvm_unreachable("calling emitLoweredStore on non-loadable type?!");
    }

    void emitDestroyAddress(SILBuilder &B, SILLocation loc,
                            SILValue addr) const override {
      if (!isTrivial())
        B.createDestroyAddr(loc, addr);
    }

    void emitDestroyRValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      if (!isTrivial())
        B.createDestroyAddr(loc, value);
    }

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      llvm_unreachable("type is not loadable!");
    }

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  TypeExpansionKind style) const override {
      llvm_unreachable("type is not loadable!");
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      llvm_unreachable("type is not loadable!");
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc, SILValue value,
                                 TypeExpansionKind style) const override {
      llvm_unreachable("type is not loadable!");
    }
  };

  /// A class for non-trivial, address-only, move only types.
  class MoveOnlyAddressOnlyTypeLowering : public TypeLowering {
  public:
    MoveOnlyAddressOnlyTypeLowering(SILType type,
                                    RecursiveProperties properties,
                                    TypeExpansionContext forExpansion)
        : TypeLowering(type, properties, IsNotReferenceCounted, forExpansion) {
      assert(properties.isAddressOnly());
    }

    void emitCopyInto(SILBuilder &B, SILLocation loc, SILValue src,
                      SILValue dest, IsTake_t isTake,
                      IsInitialization_t isInit) const override {
      assert((B.getModule().getStage() == SILStage::Raw || isTake == true) &&
             "Can only copy move only values in Raw SIL");
      B.createCopyAddr(loc, src, dest, isTake, isInit);
    }

    SILValue emitLoadOfCopy(SILBuilder &B, SILLocation loc, SILValue addr,
                            IsTake_t isTake) const override {
      llvm_unreachable("calling emitLoadOfCopy on non-loadable type");
    }

    void emitStoreOfCopy(SILBuilder &B, SILLocation loc, SILValue newValue,
                         SILValue addr,
                         IsInitialization_t isInit) const override {
      llvm_unreachable("calling emitStoreOfCopy on non-loadable type");
    }

    void emitStore(SILBuilder &B, SILLocation loc, SILValue value,
                   SILValue addr, StoreOwnershipQualifier qual) const override {
      llvm_unreachable("calling emitStore on non-loadable type");
    }

    SILValue emitLoad(SILBuilder &B, SILLocation loc, SILValue addr,
                      LoadOwnershipQualifier qual) const override {
      llvm_unreachable("calling emitLoad on non-loadable type");
    }

    SILValue emitLoweredLoad(SILBuilder &B, SILLocation loc, SILValue addr,
                             LoadOwnershipQualifier qual,
                             Lowering::TypeLowering::TypeExpansionKind
                                 expansionKind) const override {
      llvm_unreachable("calling emitLoweredLoad on non-loadable type?!");
    }

    void emitLoweredStore(SILBuilder &B, SILLocation loc, SILValue value,
                          SILValue addr, StoreOwnershipQualifier qual,
                          Lowering::TypeLowering::TypeExpansionKind
                              expansionKind) const override {
      llvm_unreachable("calling emitLoweredStore on non-loadable type?!");
    }

    void emitDestroyAddress(SILBuilder &B, SILLocation loc,
                            SILValue addr) const override {
      if (!isTrivial())
        B.createDestroyAddr(loc, addr);
    }

    void emitDestroyRValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      if (!isTrivial())
        B.createDestroyAddr(loc, value);
    }

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      llvm_unreachable("type is not loadable!");
    }

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  TypeExpansionKind style) const override {
      llvm_unreachable("type is not loadable!");
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      llvm_unreachable("type is not loadable!");
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc, SILValue value,
                                 TypeExpansionKind style) const override {
      llvm_unreachable("type is not loadable!");
    }
  };

  /// A class for Builtin.UnsafeValueBuffer.  The only purpose here is
  /// to catch obviously broken attempts to copy or destroy the buffer.
  class UnsafeValueBufferTypeLowering : public AddressOnlyTypeLowering {
  public:
    UnsafeValueBufferTypeLowering(SILType type,
                                  TypeExpansionContext forExpansion,
                                  IsTypeExpansionSensitive_t isSensitive)
      : AddressOnlyTypeLowering(type,
                                {IsNotTrivial, IsFixedABI,
                                 IsAddressOnly, IsNotResilient, isSensitive,
                                 DoesNotHaveRawPointer, IsLexical},
                                forExpansion) {}

    void emitCopyInto(SILBuilder &B, SILLocation loc,
                      SILValue src, SILValue dest, IsTake_t isTake,
                      IsInitialization_t isInit) const override {
      llvm_unreachable("cannot copy an UnsafeValueBuffer!");
    }

    void emitDestroyAddress(SILBuilder &B, SILLocation loc,
                            SILValue addr) const override {
      llvm_unreachable("cannot destroy an UnsafeValueBuffer!");
    }

    void emitDestroyRValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      llvm_unreachable("cannot destroy an UnsafeValueBuffer!");
    }
  };

  /// Lower address only types as opaque values.
  class OpaqueValueTypeLowering : public LeafLoadableTypeLowering {
  public:
    void setLoweredAddresses() const override {
      LoweredType = LoweredType.getAddressType();
    }

    OpaqueValueTypeLowering(SILType type, RecursiveProperties properties,
                            TypeExpansionContext forExpansion)
      : LeafLoadableTypeLowering(type, properties, IsNotReferenceCounted,
                                 forExpansion) {}

    void emitCopyInto(SILBuilder &B, SILLocation loc,
                      SILValue src, SILValue dest, IsTake_t isTake,
                      IsInitialization_t isInit) const override {
      if (LoweredType.isAddress()) {
        B.createCopyAddr(loc, src, dest, isTake, isInit);
      } else {
        SILValue value = emitLoadOfCopy(B, loc, src, isTake);
        emitStoreOfCopy(B, loc, value, dest, isInit);
      }
    }

    // OpaqueValue store cannot be decoupled from a destroy because it is not
    // bitwise-movable.
    void emitStore(SILBuilder &B, SILLocation loc, SILValue value,
                   SILValue addr, StoreOwnershipQualifier qual) const override {
      B.createStore(loc, value, addr, qual);
    }

    // OpaqueValue load cannot be decoupled from a copy because it is not
    // bitwise-movable.
    SILValue emitLoad(SILBuilder &B, SILLocation loc, SILValue addr,
                      LoadOwnershipQualifier qual) const override {
      return B.createLoad(loc, addr, qual);
    }

    // --- Same as LeafLoadableTypeLowering.

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  TypeExpansionKind style) const override {
      llvm_unreachable("lowered copy");
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc, SILValue value,
                                 TypeExpansionKind style) const override {
      llvm_unreachable("destroy value");
    }

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      return B.createCopyValue(loc, value);
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      B.createDestroyValue(loc, value);
    }
  };

  /// Lower address only types as opaque values.
  ///
  /// Opaque values behave like loadable leaf types in SIL.
  ///
  /// FIXME: When you remove an unreachable, just delete the method.
  class MoveOnlyOpaqueValueTypeLowering : public LeafLoadableTypeLowering {
  public:
    MoveOnlyOpaqueValueTypeLowering(SILType type,
                                    RecursiveProperties properties,
                                    TypeExpansionContext forExpansion)
        : LeafLoadableTypeLowering(type, properties, IsNotReferenceCounted,
                                   forExpansion) {}

    void setLoweredAddresses() const override {
      LoweredType = LoweredType.getAddressType();
    }

    void emitCopyInto(SILBuilder &B, SILLocation loc, SILValue src,
                      SILValue dest, IsTake_t isTake,
                      IsInitialization_t isInit) const override {
      llvm_unreachable("copy into");
    }

    // --- Same as LeafLoadableTypeLowering.

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  TypeExpansionKind style) const override {
      llvm_unreachable("lowered copy");
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc, SILValue value,
                                 TypeExpansionKind style) const override {
      llvm_unreachable("destroy value");
    }

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      assert(B.getModule().getStage() == SILStage::Raw &&
             "Can not copy a move only value in non-Raw SIL");
      return B.createCopyValue(loc, value);
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      B.createDestroyValue(loc, value);
    }
  };

  /// Build the appropriate TypeLowering subclass for the given type,
  /// which is assumed to already have been lowered.
  class CreateTypeLowering
      : public CanTypeVisitor<CreateTypeLowering, TypeLowering *> {
    TypeConverter &TC;
    RecursiveProperties Properties;
    TypeExpansionContext Expansion;
    bool LoweredAddresses;

  public:
    CreateTypeLowering(TypeConverter &TC, TypeExpansionContext expansion,
                       bool loweredAddresses, RecursiveProperties properties)
        : TC(TC), Properties(properties), Expansion(expansion),
          LoweredAddresses(loweredAddresses) {}

    TypeLowering *handleTrivial(CanType type) {
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC) TrivialTypeLowering(silType, Properties, Expansion);
    }

    TypeLowering *handleReference(CanType type) {
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC) ReferenceTypeLowering(silType, Properties, Expansion);
    }

    TypeLowering *handleMoveOnly(CanType type) {
      if (!Properties.isAddressOnly()) {
        auto silType = SILType::getPrimitiveObjectType(type);
        return new (TC)
            MoveOnlyReferenceTypeLowering(silType, Properties, Expansion);
      }

      if (!shouldUseOpaqueValuesForAddressOnly(TC)) {
        auto silType = SILType::getPrimitiveAddressType(type);
        return new (TC)
            MoveOnlyAddressOnlyTypeLowering(silType, Properties, Expansion);
      }

      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC)
          MoveOnlyOpaqueValueTypeLowering(silType, Properties, Expansion);
    }

    TypeLowering *handleAddressOnly(CanType type) {
      if (!shouldUseOpaqueValuesForAddressOnly(TC)) {
        auto silType = SILType::getPrimitiveAddressType(type);
        return new (TC) AddressOnlyTypeLowering(silType, Properties, Expansion);
      }
      auto silType = SILType::getPrimitiveType(
          type, LoweredAddresses ? SILValueCategory::Address
                                 : SILValueCategory::Object);
      return new (TC) OpaqueValueTypeLowering(silType, Properties, Expansion);
    }

    // Generic visitor that invokes one of the general routines above.
    TypeLowering *visitType(CanType type) {
      // The highest-order bit is whether the type is move-only.
      if (Properties.isMoveOnly())
        return handleMoveOnly(type);

      // Next is whether the type is address-only.
      if (Properties.isAddressOnly())
        return handleAddressOnly(type);
      assert(!Properties.isInfinite() &&
             "all infinite types should be address-only");

      // Next is whether the type is trivial.
      if (Properties.isTrivial())
        return handleTrivial(type);

      // We treat all non-trivial types that get here as "references";
      // but note that a lot of types (including aggregates) don't
      // get here.
      return handleReference(type);
    }

    // Give some types special-case TypeLowering classes.

#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
    TypeLowering *visit##Name##StorageType(Can##Name##StorageType type) { \
      return new (TC) Loadable##Name##TypeLowering( \
                                  SILType::getPrimitiveObjectType(type), \
                                  Expansion, Properties); \
    }
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
    TypeLowering *visit##Name##StorageType(Can##Name##StorageType type) { \
      if (Properties.isAddressOnly()) \
        return handleAddressOnly(type); \
      return new (TC) Loadable##Name##TypeLowering( \
                                  SILType::getPrimitiveObjectType(type), \
                                  Expansion, Properties); \
    }
#include "swift/AST/ReferenceStorage.def"

    TypeLowering *
    visitBuiltinUnsafeValueBufferType(CanBuiltinUnsafeValueBufferType type) {
      auto silType = SILType::getPrimitiveAddressType(type);
      return new (TC)
          UnsafeValueBufferTypeLowering(silType, Expansion,
                                        Properties.isTypeExpansionSensitive());
    }

    TypeLowering *visitTupleType(CanTupleType tupleType) {
      return handleAggregate<LoadableTupleTypeLowering>(tupleType);
    }

    TypeLowering *visitStructType(CanType structType) {
      return visitAnyStructType(structType);
    }
    TypeLowering *visitBoundGenericStructType(CanType structType) {
      return visitAnyStructType(structType);
    }
    TypeLowering *visitAnyStructType(CanType structType) {
      return handleAggregate<LoadableStructTypeLowering>(structType);
    }

    TypeLowering *visitEnumType(CanType enumType) {
      return visitAnyEnumType(enumType);
    }
    TypeLowering *visitBoundGenericEnumType(CanType enumType) {
      return visitAnyEnumType(enumType);
    }
    TypeLowering *visitAnyEnumType(CanType enumType) {
      return handleAggregate<LoadableEnumTypeLowering>(enumType);
    }

    TypeLowering *visitSILFunctionType(CanSILFunctionType type) {
      if (Properties.isTrivial())
        return handleTrivial(type);

      // Give non-trivial differentiable functions a custom TypeLowering
      // class.  TODO: why are we doing this?
      switch (type->getDifferentiabilityKind()) {
      // TODO: Ban `Normal` and `Forward` cases.
      case DifferentiabilityKind::Normal:
      case DifferentiabilityKind::Forward:
      case DifferentiabilityKind::Reverse:
        return new (TC) NormalDifferentiableSILFunctionTypeLowering(type,
                          Properties, Expansion);
      case DifferentiabilityKind::Linear:
        return new (TC) LinearDifferentiableSILFunctionTypeLowering(type,
                          Properties, Expansion);
      case DifferentiabilityKind::NonDifferentiable:
        break;
      }

      // Use the default logic.
      return visitType(type);
    }

    /// Convenience function to make a TypeLowering for a kind of type
    /// that wants specific treatment for its loadable types but is
    /// fine with a generic treatment for address-only types.
    template <class LoadableLoweringClass>
    TypeLowering *handleAggregate(CanType type) {
      if (Properties.isAddressOnly()) {
        return handleAddressOnly(type);
      }
      assert(Properties.isFixedABI());
      if (Properties.isTrivial()) {
        return handleTrivial(type);
      }

      return new (TC) LoadableLoweringClass(type, Properties, Expansion);
    }
  };
} // end anonymous namespace

TypeConverter::TypeConverter(ModuleDecl &m, bool loweredAddresses)
    : LoweredAddresses(loweredAddresses), M(m), Context(m.getASTContext()) {}

TypeConverter::~TypeConverter() {
  // The bump pointer allocator destructor will deallocate but not destroy all
  // our independent TypeLowerings.
  for (auto &ti : LoweredTypes) {
    // Destroy only the unique entries.
    CanType srcType = ti.first.OrigType;
    if (!srcType) continue;
    CanType mappedType = ti.second->getLoweredType().getASTType();
    if (srcType == mappedType)
      ti.second->~TypeLowering();
  }
}

void *TypeLowering::operator new(size_t size, TypeConverter &tc) {
  return tc.TypeLoweringBPA.Allocate(size, alignof(TypeLowering&));
}

const TypeLowering *TypeConverter::find(const TypeKey &k) {
  if (!k.isCacheable()) return nullptr;

  auto ck = k.getCachingKey();
  auto found = LoweredTypes.find(ck);
  if (found == LoweredTypes.end())
    return nullptr;

  assert((found->second || k.expansionContext.isMinimal()) &&
         "type recursion not caught in Sema");
  return found->second;
}

#ifndef NDEBUG
void TypeConverter::removeNullEntry(const TypeKey &k) {
  if (!k.isCacheable())
    return;

  auto ck = k.getCachingKey();

  auto found = LoweredTypes.find(ck);
  if (found == LoweredTypes.end() || found->second != nullptr)
    return;

  LoweredTypes.erase(ck);
}
#endif

void TypeConverter::insert(const TypeKey &k, const TypeLowering *tl) {
  if (!k.isCacheable()) return;

  LoweredTypes[k.getCachingKey()] = tl;
}

/// Lower each of the elements of the substituted type according to
/// the abstraction pattern of the given original type.
static CanTupleType computeLoweredTupleType(TypeConverter &tc,
                                            TypeExpansionContext context,
                                            AbstractionPattern origType,
                                            CanTupleType substType) {
  assert(origType.matchesTuple(substType));

  // Does the lowered tuple type differ from the substituted type in
  // any interesting way?
  bool changed = false;
  SmallVector<TupleTypeElt, 4> loweredElts;
  loweredElts.reserve(substType->getNumElements());

  for (auto i : indices(substType->getElementTypes())) {
    auto origEltType = origType.getTupleElementType(i);
    auto substEltType = substType.getElementType(i);

    CanType loweredTy =
        tc.getLoweredRValueType(context, origEltType, substEltType);
    changed = (changed || substEltType != loweredTy);

    loweredElts.push_back(substType->getElement(i).getWithType(loweredTy));
  }

  if (!changed) return substType;

  return CanTupleType(TupleType::get(loweredElts, tc.Context));
}

/// Lower each of the elements of the substituted type according to
/// the abstraction pattern of the given original type.
static CanSILPackType computeLoweredPackType(TypeConverter &tc,
                                             TypeExpansionContext context,
                                             AbstractionPattern origType,
                                             CanPackType substType) {
  assert(origType.matchesPack(substType));

  SmallVector<CanType, 4> loweredElts;
  loweredElts.reserve(substType->getNumElements());

  for (auto i : indices(substType->getElementTypes())) {
    auto origEltType = origType.getPackElementType(i);
    auto substEltType = substType.getElementType(i);

    CanType loweredTy =
        tc.getLoweredRValueType(context, origEltType, substEltType);
    loweredElts.push_back(loweredTy);
  }

  bool elementIsAddress = true; // TODO
  SILPackType::ExtInfo extInfo(elementIsAddress);

  return SILPackType::get(tc.Context, extInfo, loweredElts);
}

static CanType computeLoweredOptionalType(TypeConverter &tc,
                                          TypeExpansionContext context,
                                          AbstractionPattern origType,
                                          CanType substType,
                                          CanType substObjectType) {
  assert(substType.getOptionalObjectType() == substObjectType);

  CanType loweredObjectType = tc.getLoweredRValueType(
      context, origType.getOptionalObjectType(), substObjectType);

  // If the object type didn't change, we don't have to rebuild anything.
  if (loweredObjectType == substObjectType) {
    return substType;
  }

  auto optDecl = tc.Context.getOptionalDecl();
  return CanType(BoundGenericEnumType::get(optDecl, Type(), loweredObjectType));
}

static CanType
computeLoweredReferenceStorageType(TypeConverter &tc,
                                   TypeExpansionContext context,
                                   AbstractionPattern origType,
                                   CanReferenceStorageType substType) {
  CanType loweredReferentType = tc.getLoweredRValueType(
      context, origType.getReferenceStorageReferentType(),
      substType.getReferentType());

  if (loweredReferentType == substType.getReferentType())
    return substType;

  return CanReferenceStorageType::get(loweredReferentType,
                                      substType->getOwnership());
}

CanSILFunctionType
TypeConverter::getSILFunctionType(TypeExpansionContext context,
                                  AbstractionPattern origType,
                                  CanFunctionType substType) {
  return cast<SILFunctionType>(
      getLoweredRValueType(context, origType, substType));
}

const TypeLowering &
TypeConverter::getTypeLowering(AbstractionPattern origType,
                               Type origSubstType,
                               TypeExpansionContext forExpansion) {
  CanType substType = origSubstType->getCanonicalType();
  bool origHasOpaqueArchetype = substType->hasOpaqueArchetype();
  // A type is type expansion sensitive if its lowering could depend on the type
  // expansion context:
  // - If the type has an opaque archetype
  //   Because depending on the type expansion context we might get a different
  //   SIL  type (Foo<some P> vs Foo<Int>).
  // - or if during type lowering we discover an opaque archetype that
  //   influences type lowering by type expansion context
  //   E.g a struct containing a field that is a opaque archetype will be
  //   loadable or not depending on the type expansion context. In a more
  //   permissive type expansion context we will look through the opaque
  //   archetype and could discover a loadable type making the whole aggregate
  //   loadable.
  auto isTypeExpansionSensitive = origHasOpaqueArchetype
                                      ? IsTypeExpansionSensitive
                                      : IsNotTypeExpansionSensitive;
  auto key = getTypeKey(origType, substType, forExpansion);
  assert(!substType->is<InOutType>());

  // Look to see if we've cached a minimal-expansion lowering for this
  // orig/subst pair.  If we have, we're done if either
  // - we're looking for the minimal expansion anyway (possible) or
  // - the minimal expansion says it's not expansion-sensitive (likely).
  auto *candidateLowering = find(key.getKeyForMinimalExpansion());
  auto *lowering = getTypeLoweringForExpansion(
      key, forExpansion, candidateLowering, IsNotTypeExpansionSensitive);
  if (lowering != nullptr)
    return *lowering;

#ifndef NDEBUG
  // Catch reentrancy bugs.
  if (candidateLowering == nullptr)
    insert(key.getKeyForMinimalExpansion(), nullptr);
#endif

  // Lower the type.
  auto loweredSubstType =
      computeLoweredRValueType(*this, origType, substType, forExpansion,
                               BypassCache);

  // If that didn't change the type and the key is cacheable, there's no
  // point in re-checking the table, so just construct a type lowering
  // and cache it.
  if (loweredSubstType == substType && key.isCacheable()) {
    auto properties =
      computeTypeProperties(*this, loweredSubstType, forExpansion,
                            BypassCache);
    lowering =
      CreateTypeLowering(*this, forExpansion, LoweredAddresses, properties)
            .visit(loweredSubstType);

  // Otherwise, check the table at a key that would be used by the
  // SILType-based lookup path for the type we just lowered to, then cache
  // that same result at this key if possible.
  } else {
    lowering = &getTypeLoweringForLoweredType(
        loweredSubstType, forExpansion, isTypeExpansionSensitive);
  }

  if (!lowering->isResilient() && !lowering->isTypeExpansionSensitive()) {
    insert(key.getKeyForMinimalExpansion(), lowering);
  } else {
    insert(key, lowering);
#ifndef NDEBUG
    removeNullEntry(key.getKeyForMinimalExpansion());
#endif
  }

#ifndef NDEBUG
  verifyLowering(*lowering, origType, loweredSubstType, forExpansion);
#endif

  return *lowering;
}

#ifndef NDEBUG
bool TypeConverter::visitAggregateLeaves(
    Lowering::AbstractionPattern origType, Type substType,
    TypeExpansionContext context,
    std::function<bool(Type, Lowering::AbstractionPattern, ValueDecl *,
                       Optional<unsigned>)>
        isLeafAggregate,
    std::function<bool(Type, Lowering::AbstractionPattern, ValueDecl *,
                       Optional<unsigned>)>
        visit) {
  llvm::SmallSet<std::tuple<TypeBase *, ValueDecl *, unsigned>, 16> visited;
  llvm::SmallVector<
      std::tuple<TypeBase *, AbstractionPattern, ValueDecl *, unsigned>, 16>
      worklist;
  auto insertIntoWorklist = [&visited,
                             &worklist](Type substTy, AbstractionPattern origTy,
                                        ValueDecl *field,
                                        Optional<unsigned> maybeIndex) -> bool {
    unsigned index = maybeIndex.value_or(UINT_MAX);
    if (!visited.insert({substTy.getPointer(), field, index}).second)
      return false;
    worklist.push_back({substTy.getPointer(), origTy, field, index});
    return true;
  };
  auto popFromWorklist = [&worklist]()
      -> std::tuple<Type, AbstractionPattern, ValueDecl *, Optional<unsigned>> {
    TypeBase *ty;
    AbstractionPattern origTy = AbstractionPattern::getOpaque();
    ValueDecl *field;
    unsigned index;
    std::tie(ty, origTy, field, index) = worklist.pop_back_val();
    Optional<unsigned> maybeIndex;
    if (index != UINT_MAX)
      maybeIndex = {index};
    return {ty->getCanonicalType(), origTy, field, index};
  };
  auto isAggregate = [](Type ty) {
    return ty->is<TupleType>() || ty->getEnumOrBoundGenericEnum() ||
           ty->getStructOrBoundGenericStruct();
  };
  insertIntoWorklist(substType, origType, nullptr, llvm::None);
  while (!worklist.empty()) {
    Type ty;
    AbstractionPattern origTy = AbstractionPattern::getOpaque();
    ValueDecl *field;
    Optional<unsigned> index;
    std::tie(ty, origTy, field, index) = popFromWorklist();
    if (isAggregate(ty) && !isLeafAggregate(ty, origTy, field, index)) {
      if (auto tupleTy = ty->getAs<TupleType>()) {
        for (unsigned tupleIndex = 0, num = tupleTy->getNumElements();
             tupleIndex < num; ++tupleIndex) {
          auto origElementTy = origTy.getTupleElementType(tupleIndex);
          auto substElementTy =
              tupleTy->getElementType(tupleIndex)->getCanonicalType();
          substElementTy =
              computeLoweredRValueType(*this, origElementTy, substElementTy,
                                       context, BypassCache);
          insertIntoWorklist(substElementTy, origElementTy, nullptr,
                             tupleIndex);
        }
      } else if (auto *decl = ty->getStructOrBoundGenericStruct()) {
        for (auto *structField : decl->getStoredProperties()) {
          auto subMap = ty->getContextSubstitutionMap(&M, decl);
          auto substFieldTy =
              structField->getInterfaceType().subst(subMap)->getCanonicalType();
          auto sig =
              structField->getDeclContext()->getGenericSignatureOfContext();
          auto interfaceTy =
              structField->getInterfaceType()->getReducedType(sig);
          auto origFieldType =
              origTy.unsafeGetSubstFieldType(structField, interfaceTy);
          insertIntoWorklist(substFieldTy, origFieldType, structField,
                             llvm::None);
        }
      } else if (auto *decl = ty->getEnumOrBoundGenericEnum()) {
        auto subMap = ty->getContextSubstitutionMap(&M, decl);
        for (auto *element : decl->getAllElements()) {
          if (!element->hasAssociatedValues())
            continue;
          // TODO: Callback for indirect elements.
          if (element->isIndirect())
            continue;
          auto substElementType = element->getArgumentInterfaceType()
                                      .subst(subMap)
                                      ->getCanonicalType();
          auto origElementTy = origTy.unsafeGetSubstFieldType(
              element, element->getArgumentInterfaceType()->getReducedType(
                           decl->getGenericSignature()));

          insertIntoWorklist(substElementType, origElementTy, element,
                             llvm::None);
        }
      } else {
        llvm_unreachable("unknown aggregate kind!");
      }
      continue;
    }

    // This type is a leaf.  Visit it.
    auto success = visit(ty, origTy, field, index);
    if (!success)
      return false;
  }
  return true;
}

void TypeConverter::verifyLowering(const TypeLowering &lowering,
                                   AbstractionPattern origType, Type substType,
                                   TypeExpansionContext forExpansion) {
  // Non-trivial lowerings should always be lexical unless all non-trivial
  // fields are eager move.
  if (!lowering.isTrivial() && !lowering.isLexical()) {
    if (lowering.getRecursiveProperties().isInfinite())
      return;
    auto getLifetimeAnnotation = [](Type ty) -> LifetimeAnnotation {
      NominalTypeDecl *nominal;
      if (!(nominal = ty->getAnyNominal()))
        return LifetimeAnnotation::None;
      return nominal->getLifetimeAnnotation();
    };
    bool hasNoNontrivialLexicalLeaf = visitAggregateLeaves(
        origType, substType, forExpansion,
        /*isLeaf=*/
        [&](auto ty, auto origTy, auto *field, auto index) -> bool {
          // The field's type is an aggregate.  Treat it as a leaf if it
          // has a lifetime annotation.

          // If it's a field of a tuple or the top-level type, there's no value
          // decl on which to look for an attribute.  It's a leaf iff the type
          // has a lifetime annotation.
          if (index || !field)
            return getLifetimeAnnotation(ty).isSome();

          // It's a field of a struct or an enum.  It's a leaf if the type
          // or the var decl has a lifetime annotation.
          return field->getLifetimeAnnotation().isSome() ||
                 getLifetimeAnnotation(ty);
        },
        /*visit=*/
        [&](auto ty, auto origTy, auto *field, auto index) -> bool {
          // Look at each leaf: if it is non-trivial, verify that it is
          // attributed @_eagerMove.

          // If the leaf is the whole type, verify that it is annotated
          // @_eagerMove.
          if (ty->getCanonicalType() == substType->getCanonicalType())
            return getLifetimeAnnotation(ty) == LifetimeAnnotation::EagerMove;

          auto &tyLowering = getTypeLowering(origTy, ty, forExpansion);

          // Leaves which are trivial aren't of interest.
          if (tyLowering.isTrivial())
            return true;

          // We're visiting a non-trival leaf of a type whose lowering is
          // not lexical.  The leaf must be annotated @_eagerMove.
          // Otherwise, the whole type would be lexical.

          if (index || !field) {
            // There is no field decl that might be annotated @_eagerMove.  The
            // field is @_eagerMove iff its type is annotated @_eagerMove.
            return getLifetimeAnnotation(ty) == LifetimeAnnotation::EagerMove;
          }

          // The field is non-trivial and the whole type is non-lexical.
          // That's fine as long as the field or its type is annotated
          // @_eagerMove.
          return field->getLifetimeAnnotation() ==
                     LifetimeAnnotation::EagerMove ||
                 getLifetimeAnnotation(ty) == LifetimeAnnotation::EagerMove;
        });
    assert(hasNoNontrivialLexicalLeaf &&
           "Found non-trivial lexical leaf in non-trivial non-lexical type?!");
  }
}
#endif

namespace {
class LoweredRValueTypeVisitor
    : public CanTypeVisitor<LoweredRValueTypeVisitor, CanType, UseCache_t> {
  TypeConverter &TC;
  TypeExpansionContext forExpansion;
  AbstractionPattern origType;

public:
  LoweredRValueTypeVisitor(TypeConverter &TC,
                           TypeExpansionContext forExpansion,
                           AbstractionPattern origType)
      : TC(TC), forExpansion(forExpansion), origType(origType) {}

  bool canUseCache(CanType substType) {
    return !substType->hasTypeParameter() &&
           origType.hasCachingKey();
  }
  CanType getFromCache(CanType substType) {
    // TODO: add an intermediate level of caching that doesn't rely on
    // creating TypeLowering objects.
    auto &tl = TC.getTypeLowering(origType, substType, forExpansion);
    return tl.getLoweredType().getRawASTType();
  }

  // AST function types are turned into SIL function types:
  //   - the type is uncurried as desired
  //   - types are turned into their unbridged equivalents, depending
  //     on the abstract CC
  //   - ownership conventions are deduced
  //   - a minimal substituted generic signature is extracted to represent
  //     possible ABI-compatible substitutions
  CanType visitAnyFunctionType(CanAnyFunctionType substFnType,
                               UseCache_t shouldUseCache) {
    if (shouldUseCache && canUseCache(substFnType))
      return getFromCache(substFnType);

    // If the formal type uses a C convention, it is not formally
    // abstractable, and it may be subject to implicit bridging.
    auto extInfo = substFnType->getExtInfo();
    auto rep = extInfo.getRepresentation();
    SILFunctionTypeRepresentation silRep = convertRepresentation(rep);
    if (getSILFunctionLanguage(silRep) == SILFunctionLanguage::C) {
      // The importer only applies fully-reversible bridging to the
      // component types of C function pointers.
      auto bridging = Bridgeability::Full;
      if (silRep == SILFunctionTypeRepresentation::CFunctionPointer)
        bridging = Bridgeability::None;

      // Bridge the parameters and result of the function type.
      auto bridgedFnType =
          TC.getBridgedFunctionType(origType, substFnType, bridging, silRep);
      substFnType = bridgedFnType;

      // Also rewrite the type of the abstraction pattern.
      auto signature = origType.getGenericSignatureOrNull();
      if (origType.isTypeParameter()) {
        origType = AbstractionPattern(signature, bridgedFnType);
      } else {
        origType.rewriteType(signature, bridgedFnType);
      }
    }

    AnyFunctionType::ExtInfo baseExtInfo;
    if (auto origFnType = origType.getAs<AnyFunctionType>()) {
      baseExtInfo = origFnType->getExtInfo();
    } else {
      baseExtInfo = substFnType->getExtInfo();
    }
    const clang::Type *clangType = baseExtInfo.getClangTypeInfo().getType();
    if (shouldStoreClangType(rep) && !clangType) {
      clangType = TC.Context.getClangFunctionType(
          substFnType->getParams(), substFnType->getResult(), rep);
    }
    auto silExtInfo =
        SILExtInfoBuilder(
            baseExtInfo.intoBuilder().withClangFunctionType(clangType), false)
            .build();

    return ::getNativeSILFunctionType(TC, forExpansion, origType, substFnType,
                                      silExtInfo, None, None, None, {});
  }

  // Ignore dynamic self types.
  CanType visitDynamicSelfType(CanDynamicSelfType selfType,
                               UseCache_t shouldUseCache) {
    return visit(selfType.getSelfType(), shouldUseCache);
  }

  // Static metatypes are unitary and can optimized to a "thin" empty
  // representation if the type also appears as a static metatype in the
  // original abstraction pattern.
  CanType visitMetatypeType(CanMetatypeType substMeta,
                            UseCache_t shouldUseCache) {
    // If the metatype has already been lowered, it will already carry its
    // representation.
    if (substMeta->hasRepresentation()) {
      assert(substMeta->isLegalSILType());
      return substOpaqueTypesWithUnderlyingTypes(substMeta, forExpansion);
    }

    MetatypeRepresentation repr;

    auto origMeta = origType.getAs<MetatypeType>();
    if (!origMeta) {
      // If the metatype matches a dependent type, it must be thick.
      assert(origType.isTypeParameterOrOpaqueArchetype());
      repr = MetatypeRepresentation::Thick;
    } else {
      // Otherwise, we're thin if the metatype is thinnable both
      // substituted and in the abstraction pattern.
      if (hasSingletonMetatype(substMeta.getInstanceType()) &&
          hasSingletonMetatype(origMeta.getInstanceType()))
        repr = MetatypeRepresentation::Thin;
      else
        repr = MetatypeRepresentation::Thick;
    }

    CanType instanceType = substOpaqueTypesWithUnderlyingTypes(
        substMeta.getInstanceType(), forExpansion);

    // Regardless of thinness, metatypes are always trivial.
    return CanMetatypeType::get(instanceType, repr);
  }

  // Give existential metatypes @thick representation by default.
  CanType
  visitExistentialMetatypeType(CanExistentialMetatypeType existMetatype,
                               UseCache_t shouldUseCache) {
    if (existMetatype->hasRepresentation()) {
      assert(existMetatype->isLegalSILType());
      return existMetatype;
    }

    return CanExistentialMetatypeType::get(existMetatype.getInstanceType(),
                                           MetatypeRepresentation::Thick);
  }

  // Lower pack element types.
  CanType visitPackType(CanPackType substPackType, UseCache_t shouldUseCache) {
    if (shouldUseCache && canUseCache(substPackType))
      return getFromCache(substPackType);

    return computeLoweredPackType(TC, forExpansion, origType,
                                  substPackType);
  }

  CanType visitPackExpansionType(CanPackExpansionType substPackExpansionType,
                                 UseCache_t shouldUseCache) {
    bool changed = false;

    CanType substPatternType = substPackExpansionType.getPatternType();
    CanType loweredSubstPatternType =
      TC.getLoweredRValueType(forExpansion,
                              origType.getPackExpansionPatternType(),
                              substPatternType);
    changed |= (loweredSubstPatternType != substPatternType);

    if (!changed)
      return substPackExpansionType;

    CanType substCountType = substPackExpansionType.getCountType();
    return CanType(PackExpansionType::get(loweredSubstPatternType,
                                          substCountType));
  }

  CanType visitBuiltinTupleType(CanBuiltinTupleType type,
                                UseCache_t shouldUseCache) {
    llvm_unreachable("BuiltinTupleType should not show up here");
  }

  // Lower tuple element types.
  CanType visitTupleType(CanTupleType substTupleType,
                         UseCache_t shouldUseCache) {
    // Lowering tuple types is usually pretty painless, and we can do
    // it without extra hash lookups, so only go through the cache if
    // there's a significant number of members.  3 is an arbitrary
    // but probably reasonable threshold.
    if (shouldUseCache && canUseCache(substTupleType) &&
        substTupleType->getNumElements() >= 3)
      return getFromCache(substTupleType);

    return computeLoweredTupleType(TC, forExpansion, origType,
                                   substTupleType);
  }

  // Lower the referent type of reference storage types.
  CanType visitReferenceStorageType(CanReferenceStorageType substRefType,
                                    UseCache_t shouldUseCache) {
    return computeLoweredReferenceStorageType(TC, forExpansion, origType,
                                              substRefType);
  }

  CanType visitSILFunctionType(CanSILFunctionType silFnTy,
                               UseCache_t shouldUseCache) {
    if (!silFnTy->hasOpaqueArchetype() ||
        !forExpansion.shouldLookThroughOpaqueTypeArchetypes())
      return silFnTy;
    return silFnTy->substituteOpaqueArchetypes(TC, forExpansion);
  }

  CanType visitGenericTypeParamType(CanGenericTypeParamType substType,
                                    UseCache_t shouldUseCache) {
    return visitAbstractTypeParamType(substType);
  }
  CanType visitDependentMemberType(CanDependentMemberType substType,
                                   UseCache_t shouldUseCache) {
    return visitAbstractTypeParamType(substType);
  }
  CanType visitAbstractTypeParamType(CanType substType) {
    auto signature = TC.getCurGenericSignature();
    assert(signature && "lowering interface type without using "
                        "GenericContextRAII");

    // Don't leave concretely-constrained type parameters in lowered
    // types.  It's fine for them to appear in nested positions where
    // they're still formal types, but in any position where lowering
    // is required, replace them with the concrete type (and then
    // recursively continue lowering).
    if (auto concreteType = signature->getConcreteType(substType)) {
      return visit(concreteType->getCanonicalType(), UseCache);
    }
    return substType;
  }

  CanType visitBoundGenericEnumType(CanBoundGenericEnumType enumType,
                                    UseCache_t shouldUseCache) {
    // Lower the object type of optional types.
    if (auto substObjectType = enumType.getOptionalObjectType()) {
      return computeLoweredOptionalType(TC, forExpansion, origType,
                                        enumType, substObjectType);
    }

    return visitType(enumType, shouldUseCache);
  }

  CanType visitType(CanType substType, UseCache_t shouldUseCache) {
    // In the generic fallback case, the lowered type is just the
    // formal type.

    // Make sure we look through opaque types.  (Maybe we should only
    // be doing this at the top level, though, like we do with
    // concretely-constrained type parameters?)
    auto underlyingTy =
        substOpaqueTypesWithUnderlyingTypes(substType, forExpansion,
                                            /*allowLoweredTypes*/ true);
    if (underlyingTy != substType) {
      underlyingTy = visit(underlyingTy, shouldUseCache);
    }

    return underlyingTy;
  }
};
} // end anonymouse namespace

static CanType computeLoweredRValueType(TypeConverter &TC,
                                        AbstractionPattern origType,
                                        CanType substType,
                                        TypeExpansionContext forExpansion,
                                        UseCache_t shouldUseCache) {
  LoweredRValueTypeVisitor visitor(TC, forExpansion, origType);
  return visitor.visit(substType, shouldUseCache);
}

CanType
TypeConverter::getLoweredRValueType(TypeExpansionContext forExpansion,
                                    Type t) {
  return getLoweredRValueType(forExpansion, AbstractionPattern(t), t);
}

CanType
TypeConverter::getLoweredRValueType(TypeExpansionContext forExpansion,
                                    AbstractionPattern origType,
                                    Type substType) {
  assert(!substType->hasTypeParameter() || getCurGenericSignature());
  return computeLoweredRValueType(*this, origType,
                                  substType->getCanonicalType(),
                                  forExpansion, UseCache);
}

RecursiveProperties
TypeConverter::getTypeProperties(SILType type,
                                 TypeExpansionContext forExpansion) {
  assert(!type.hasTypeParameter() || getCurGenericSignature());
  return computeTypeProperties(*this, type.getRawASTType(),
                               forExpansion, UseCache);
}

RecursiveProperties
TypeConverter::getTypeProperties(Type ncSubstType,
                                 TypeExpansionContext forExpansion) {
  auto substType = ncSubstType->getCanonicalType();
  assert(!substType->hasTypeParameter() || getCurGenericSignature());
  return computeTypeProperties(*this, substType, forExpansion, UseCache);
}

std::pair<SILType, RecursiveProperties>
TypeConverter::getLoweredTypeAndProperties(AbstractionPattern origType,
                                           CanType substType,
                                       TypeExpansionContext forExpansion) {
  assert(!substType->hasTypeParameter() || getCurGenericSignature());

  CanType loweredType =
    computeLoweredRValueType(*this, origType, substType, forExpansion,
                             UseCache);

  RecursiveProperties properties =
    computeTypeProperties(*this, loweredType, forExpansion, UseCache);

  // FIXME: it's really unfortunate that we have to compute type
  // properties just to set the address-ness of this type right, but
  // there's currently an expectation that getLoweredType always does
  // this.
  SILType silType = [&] {
    if (properties.isAddressOnly() &&
        !shouldUseOpaqueValuesForAddressOnly(*this))
      return SILType::getPrimitiveAddressType(loweredType);
    return SILType::getPrimitiveObjectType(loweredType);
  }();
  
  return std::make_pair(silType, properties);
}

SILType TypeConverter::getLoweredType(AbstractionPattern origType,
                                      Type substType,
                                      TypeExpansionContext forExpansion) {
  auto loweredTypeAndProperties =
    getLoweredTypeAndProperties(origType, substType->getCanonicalType(),
                                forExpansion);
  return loweredTypeAndProperties.first;
}

const TypeLowering &
TypeConverter::getTypeLowering(SILType type,
                               TypeExpansionContext forExpansion,
                               CanGenericSignature sig) {
  // The type lowering for a type parameter relies on its context.
  assert(sig || !type.getASTType()->hasTypeParameter());

  GenericContextRAII scope(*this, sig);

  // We use the Raw AST type to ensure that moveonlywrapped values use the move
  // only type lowering. This ensures that trivial moveonlywrapped values are
  // not trivial.
  auto loweredType = type.getRawASTType();
  return getTypeLoweringForLoweredType(loweredType, forExpansion);
}

const TypeLowering &
TypeConverter::getTypeLowering(SILType t, SILFunction &F) {
  return getTypeLowering(t, TypeExpansionContext(F),
                       F.getLoweredFunctionType()->getSubstGenericSignature());
}

const TypeLowering &TypeConverter::getTypeLoweringForLoweredType(
    CanType loweredType, TypeExpansionContext forExpansion) {
  auto isTypeExpansionSensitive = loweredType->hasOpaqueArchetype()
                                      ? IsTypeExpansionSensitive
                                      : IsNotTypeExpansionSensitive;
  return getTypeLoweringForLoweredType(loweredType, forExpansion,
                                       isTypeExpansionSensitive);
}

const TypeLowering &TypeConverter::getTypeLoweringForLoweredType(
    CanType loweredType, TypeExpansionContext forExpansion,
    IsTypeExpansionSensitive_t isTypeExpansionSensitive) {

  // For very large types (e.g. tuples with many elements), this assertion is
  // very expensive to execute, because the `isLegalSILType` status is not cached.
  // Therefore the assert is commented out and only here for documentation purposes.
  // assert(loweredType->isLegalSILType() && "type is not lowered!");

  // Cache the lowered type record for a contextualized type independent of the
  // abstraction pattern. Lowered type parameters can't be cached or looked up
  // without context.
  AbstractionPattern origTypeForCaching = loweredType->hasTypeParameter()
      ? AbstractionPattern::getInvalid()
      : AbstractionPattern(loweredType);
  auto key = getTypeKey(origTypeForCaching, loweredType, forExpansion);

  auto *candidateLowering = find(key.getKeyForMinimalExpansion());
  auto *lowering = getTypeLoweringForExpansion(
      key, forExpansion, candidateLowering, isTypeExpansionSensitive);
  if (lowering != nullptr)
    return *lowering;

#ifndef NDEBUG
  // Catch reentrancy bugs.
  if (candidateLowering == nullptr)
    insert(key.getKeyForMinimalExpansion(), nullptr);
#endif

  // We may be able to look through opaque archetypes in this
  // expansion context that we weren't able to when originally
  // building the lowered type.  If so, recurse.
  // TODO: should we really be doing this?  Maybe this should be
  // an explicit operation.
  if (forExpansion.shouldLookThroughOpaqueTypeArchetypes() &&
      loweredType->hasOpaqueArchetype()) {
    auto newLoweredType = computeLoweredRValueType(
        *this, AbstractionPattern(loweredType), loweredType,
        forExpansion, BypassCache);
    if (newLoweredType != loweredType) {
      lowering = &getTypeLoweringForLoweredType(newLoweredType, forExpansion);

      // The lowering is expansion-sensitive because of the type
      // transformation we just did, so we cannot cache under the
      // minimal key.  `lowering->isTypeExpansionSensitive()` is
      // technically wrong for the key we're inserting under, but
      // that's fine because we don't rely on that bit being set
      // properly for anything but minimal keys.
      insert(key, lowering);

#ifndef NDEBUG
      removeNullEntry(key.getKeyForMinimalExpansion());
#endif

      return *lowering;
    }
  }

  auto properties =
    computeTypeProperties(*this, loweredType, forExpansion, BypassCache);
  lowering = CreateTypeLowering(*this, forExpansion, LoweredAddresses,
                                properties)
                 .visit(loweredType);

  if (!lowering->isResilient() && !lowering->isTypeExpansionSensitive())
    insert(key.getKeyForMinimalExpansion(), lowering);
  else {
    insert(key, lowering);
#ifndef NDEBUG
    removeNullEntry(key.getKeyForMinimalExpansion());
#endif
  }

  return *lowering;
}

/// When we've found a type lowering for one resilience expansion,
/// check if its the one we want; if not, walk the list until we
/// find the right one, returning nullptr if the caller needs to
/// go ahead and lower the type with the correct expansion.
const TypeLowering *TypeConverter::getTypeLoweringForExpansion(
    TypeKey key, TypeExpansionContext forExpansion,
    const TypeLowering *minimalExpansionLowering,
    IsTypeExpansionSensitive_t isOrigTypeSensitive) {
  if (minimalExpansionLowering == nullptr)
    return nullptr;

  if (!minimalExpansionLowering->isResilient() &&
      !minimalExpansionLowering->isTypeExpansionSensitive() &&
      !isOrigTypeSensitive) {
    // Don't try to refine the lowering for other resilience expansions if
    // we don't expect to get a different lowering anyway. Similar if the
    // original type did not have opaque type archetypes.
    //
    // See handleResilience() for the gory details; we only set this
    // flag if the type is resilient *and* inside our module.
    return minimalExpansionLowering;
  }

  auto *exactLowering = find(key);
  if (exactLowering)
    return exactLowering;

  // We have to create a new one.
  return nullptr;
}

static GenericSignature 
getEffectiveGenericSignature(DeclContext *dc,
                             CaptureInfo captureInfo) {
  if (dc->getParent()->isLocalContext() &&
      !captureInfo.hasGenericParamCaptures())
    return nullptr;

  return dc->getGenericSignatureOfContext();
}

static GenericSignature 
getEffectiveGenericSignature(AnyFunctionRef fn,
                             CaptureInfo captureInfo) {
  return getEffectiveGenericSignature(fn.getAsDeclContext(), captureInfo);
}

static CanGenericSignature
getCanonicalSignatureOrNull(GenericSignature sig) {
  if (!sig || sig->areAllParamsConcrete())
    return nullptr;
  return sig.getCanonicalSignature();
}

/// Get the type of a global variable accessor function, () -> RawPointer.
static CanAnyFunctionType getGlobalAccessorType(CanType varType) {
  ASTContext &C = varType->getASTContext();
  // FIXME: Verify ExtInfo state is correct, not working by accident.
  CanFunctionType::ExtInfo info;
  return CanFunctionType::get({}, C.TheRawPointerType, info);
}

/// Removes @noescape from the given type if it's a function type. Otherwise,
/// returns the original type.
static CanType removeNoEscape(CanType resultType) {
  if (auto funTy = resultType->getAs<AnyFunctionType>()) {
    auto newExtInfo = funTy->getExtInfo().withNoEscape(false);
    return adjustFunctionType(cast<AnyFunctionType>(resultType), newExtInfo);
  }

  return resultType;
}

/// Get the type of a default argument generator, () -> T.
static CanAnyFunctionType getDefaultArgGeneratorInterfaceType(
                                                     TypeConverter &TC,
                                                     SILDeclRef c) {
  auto *vd = c.getDecl();
  auto *pd = getParameterAt(vd, c.defaultArgIndex);

  Type resultTy;

  if (auto type = pd->getTypeOfDefaultExpr()) {
    resultTy = type->mapTypeOutOfContext();
  } else {
    resultTy = pd->getInterfaceType();
  }

  assert(resultTy && "Didn't find default argument?");

  // The result type might be written in terms of type parameters
  // that have been made fully concrete.
  CanType canResultTy = resultTy->getReducedType(
                            vd->getInnermostDeclContext()
                              ->getGenericSignatureOfContext());

  // Remove @noescape from function return types. A @noescape
  // function return type is a contradiction.
  canResultTy = removeNoEscape(canResultTy);

  // Get the generic signature from the surrounding context.
  auto sig = TC.getConstantGenericSignature(c);

  // FIXME: Verify ExtInfo state is correct, not working by accident.
  CanAnyFunctionType::ExtInfo info;
  return CanAnyFunctionType::get(getCanonicalSignatureOrNull(sig), {},
                                 canResultTy, info);
}

/// Get the type of a stored property initializer, () -> T.
static CanAnyFunctionType getStoredPropertyInitializerInterfaceType(
                                                     VarDecl *VD) {
  auto *DC = VD->getDeclContext();
  CanType resultTy =
    VD->getParentPattern()->getType()->mapTypeOutOfContext()
          ->getCanonicalType();

  // If this is the backing storage for a property with an attached
  // wrapper that was initialized with '=', the stored property initializer
  // will be in terms of the original property's type.
  if (auto originalProperty = VD->getOriginalWrappedProperty()) {
    if (originalProperty->isPropertyMemberwiseInitializedWithWrappedType()) {
      resultTy = originalProperty->getPropertyWrapperInitValueInterfaceType()
                                     ->getCanonicalType();
      // Stored property initializers can't return @noescape functions
      resultTy = removeNoEscape(resultTy);
    }
  }

  auto sig = DC->getGenericSignatureOfContext();

  // FIXME: Verify ExtInfo state is correct, not working by accident.
  CanAnyFunctionType::ExtInfo info;
  return CanAnyFunctionType::get(getCanonicalSignatureOrNull(sig), {}, resultTy,
                                 info);
}

/// Type of runtime discoverable attribute generator is () -> <#AttrType#>
static CanAnyFunctionType
getRuntimeAttributeGeneratorInterfaceType(TypeConverter &TC, SILDeclRef c) {
  auto *attachedToDecl = c.getDecl();
  auto *attr = c.pointer.get<CustomAttr *>();
  auto *attrType = attachedToDecl->getRuntimeDiscoverableAttrTypeDecl(attr);
  auto generator =
      attachedToDecl->getRuntimeDiscoverableAttributeGenerator(attr);

  auto resultTy = generator.second->getCanonicalType();

  CanType canResultTy = resultTy->getReducedType(
      attrType->getInnermostDeclContext()->getGenericSignatureOfContext());

  // Remove @noescape from function return types. A @noescape
  // function return type is a contradiction.
  canResultTy = removeNoEscape(canResultTy);

  // FIXME: Verify ExtInfo state is correct, not working by accident.
  CanAnyFunctionType::ExtInfo info;
  return CanAnyFunctionType::get(/*genericSignature=*/nullptr,
                                 /*params=*/{}, canResultTy, info);
}

/// Get the type of a property wrapper backing initializer,
/// (property-type) -> backing-type.
static CanAnyFunctionType getPropertyWrapperBackingInitializerInterfaceType(
                                                     TypeConverter &TC,
                                                     SILDeclRef c) {
  auto *VD = cast<VarDecl>(c.getDecl());
  CanType resultType =
      VD->getPropertyWrapperBackingPropertyType()->getCanonicalType();

  CanType inputType;
  if (c.kind == SILDeclRef::Kind::PropertyWrapperBackingInitializer) {
    inputType = VD->getPropertyWrapperInitValueInterfaceType()->getCanonicalType();
  } else {
    Type interfaceType = VD->getPropertyWrapperProjectionVar()->getInterfaceType();
    inputType = interfaceType->getCanonicalType();
  }

  GenericSignature sig = TC.getConstantGenericSignature(c);

  AnyFunctionType::Param param(
      inputType, Identifier(),
      ParameterTypeFlags().withOwnershipSpecifier(ParamSpecifier::LegacyOwned));
  // FIXME: Verify ExtInfo state is correct, not working by accident.
  CanAnyFunctionType::ExtInfo info;
  return CanAnyFunctionType::get(getCanonicalSignatureOrNull(sig), {param},
                                 resultType, info);
}

/// Get the type of a destructor function.
static CanAnyFunctionType getDestructorInterfaceType(DestructorDecl *dd,
                                                     bool isDeallocating,
                                                     bool isForeign) {
  auto classType = dd->getDeclContext()->getDeclaredInterfaceType()
    ->getReducedType(dd->getGenericSignatureOfContext());

  assert((!isForeign || isDeallocating)
         && "There are no foreign destroying destructors");
  auto extInfoBuilder =
      AnyFunctionType::ExtInfoBuilder(FunctionType::Representation::Thin,
                                      /*throws*/ false);
  if (isForeign)
    extInfoBuilder = extInfoBuilder.withSILRepresentation(
        SILFunctionTypeRepresentation::ObjCMethod);
  else
    extInfoBuilder = extInfoBuilder.withSILRepresentation(
        SILFunctionTypeRepresentation::Method);
  auto extInfo = extInfoBuilder.build();

  auto &C = dd->getASTContext();
  CanType resultTy = (isDeallocating
                      ? TupleType::getEmpty(C)
                      : C.TheNativeObjectType);
  // FIXME: Verify ExtInfo state is correct, not working by accident.
  CanFunctionType::ExtInfo info;
  CanType methodTy = CanFunctionType::get({}, resultTy, info);

  auto sig = dd->getGenericSignatureOfContext();
  FunctionType::Param args[] = {FunctionType::Param(classType)};
  return CanAnyFunctionType::get(getCanonicalSignatureOrNull(sig),
                                 llvm::makeArrayRef(args),
                                 methodTy, extInfo);
}

/// Retrieve the type of the ivar initializer or destroyer method for
/// a class.
static CanAnyFunctionType getIVarInitDestroyerInterfaceType(ClassDecl *cd,
                                                            bool isObjC,
                                                            bool isDestroyer) {
  auto classType = cd->getDeclaredInterfaceType()
    ->getReducedType(cd->getGenericSignatureOfContext());

  auto resultType = (isDestroyer
                     ? TupleType::getEmpty(cd->getASTContext())
                     : classType);
  auto extInfoBuilder =
      AnyFunctionType::ExtInfoBuilder(FunctionType::Representation::Thin,
                                      /*throws*/ false);
  auto extInfo = extInfoBuilder
                     .withSILRepresentation(
                         isObjC ? SILFunctionTypeRepresentation::ObjCMethod
                                : SILFunctionTypeRepresentation::Method)
                     .build();

  resultType = CanFunctionType::get({}, resultType, extInfo);
  auto sig = cd->getGenericSignature();
  FunctionType::Param args[] = {FunctionType::Param(classType)};
  return CanAnyFunctionType::get(getCanonicalSignatureOrNull(sig),
                                 llvm::makeArrayRef(args),
                                 resultType, extInfo);
}

static CanAnyFunctionType
getFunctionInterfaceTypeWithCaptures(TypeConverter &TC,
                                     CanAnyFunctionType funcType,
                                     SILDeclRef constant) {
  // Get transitive closure of value captured by this function, and any
  // captured functions.
  auto captureInfo = TC.getLoweredLocalCaptures(constant);

  // Capture generic parameters from the enclosing context if necessary.
  auto closure = *constant.getAnyFunctionRef();
  auto genericSig = getEffectiveGenericSignature(closure, captureInfo);

  auto innerExtInfo =
      AnyFunctionType::ExtInfoBuilder(FunctionType::Representation::Thin,
                                      funcType->isThrowing())
          .withConcurrent(funcType->isSendable())
          .withAsync(funcType->isAsync())
          .build();

  return CanAnyFunctionType::get(
      getCanonicalSignatureOrNull(genericSig),
      funcType.getParams(), funcType.getResult(),
      innerExtInfo);
}

static CanAnyFunctionType getAsyncEntryPoint(ASTContext &C) {

  // @main struct Main {
  //    static func main() async throws {}
  //    static func $main() async throws { try await main() }
  //  }
  //
  // func @async_main() async -> Void {
  //   do {
  //      try await Main.$main()
  //      exit(0)
  //   } catch {
  //      _emitErrorInMain(error)
  //   }
  // }
  //
  // This generates the type signature for @async_main
  // TODO: 'Never' return type would be more accurate.

  CanType returnType = C.getVoidType()->getCanonicalType();
  FunctionType::ExtInfo extInfo =
      FunctionType::ExtInfoBuilder().withAsync(true).withThrows(false).build();
  return CanAnyFunctionType::get(/*genericSig*/ nullptr, {}, returnType,
                                 extInfo);
}

static CanAnyFunctionType getEntryPointInterfaceType(ASTContext &C) {
  // Use standard library types if we have them; otherwise, fall back to
  // builtins.
  CanType Int32Ty;
  if (auto Int32Decl = C.getInt32Decl()) {
    Int32Ty = Int32Decl->getDeclaredInterfaceType()->getCanonicalType();
  } else {
    Int32Ty = CanType(BuiltinIntegerType::get(32, C));
  }

  CanType PtrPtrInt8Ty = C.TheRawPointerType;
  if (auto PointerDecl = C.getUnsafeMutablePointerDecl()) {
    if (auto Int8Decl = C.getInt8Decl()) {
      Type Int8Ty = Int8Decl->getDeclaredInterfaceType();
      Type PointerInt8Ty = BoundGenericType::get(PointerDecl,
                                                 nullptr,
                                                 Int8Ty);
      Type OptPointerInt8Ty = OptionalType::get(PointerInt8Ty);
      PtrPtrInt8Ty = BoundGenericType::get(PointerDecl,
                                           nullptr,
                                           OptPointerInt8Ty)
        ->getCanonicalType();
    }
  }

  using Param = FunctionType::Param;
  Param params[] = {Param(Int32Ty), Param(PtrPtrInt8Ty)};

  auto rep = FunctionTypeRepresentation::CFunctionPointer;
  auto *clangTy = C.getClangFunctionType(params, Int32Ty, rep);
  auto extInfo = FunctionType::ExtInfoBuilder()
                     .withRepresentation(rep)
                     .withClangFunctionType(clangTy)
                     .build();

  return CanAnyFunctionType::get(/*genericSig*/ nullptr,
                                 llvm::makeArrayRef(params), Int32Ty, extInfo);
}

CanAnyFunctionType TypeConverter::makeConstantInterfaceType(SILDeclRef c) {
  if (auto *derivativeId = c.getDerivativeFunctionIdentifier()) {
    auto originalFnTy =
        makeConstantInterfaceType(c.asAutoDiffOriginalFunction());
    auto *derivativeFnTy = originalFnTy->getAutoDiffDerivativeFunctionType(
        derivativeId->getParameterIndices(), derivativeId->getKind(),
        LookUpConformanceInModule(&M),
        derivativeId->getDerivativeGenericSignature());
    return cast<AnyFunctionType>(derivativeFnTy->getCanonicalType());
  }

  auto *vd = c.loc.dyn_cast<ValueDecl *>();
  switch (c.kind) {
  case SILDeclRef::Kind::Func: {
    CanAnyFunctionType funcTy;
    if (auto *ACE = c.loc.dyn_cast<AbstractClosureExpr *>()) {
      // FIXME: Closures could have an interface type computed by Sema.
      funcTy = cast<AnyFunctionType>(
        ACE->getType()->mapTypeOutOfContext()->getCanonicalType());
    } else {
      funcTy = cast<AnyFunctionType>(
        vd->getInterfaceType()->getCanonicalType());
    }
    return getFunctionInterfaceTypeWithCaptures(*this, funcTy, c);
  }

  case SILDeclRef::Kind::EnumElement: {
    auto funcTy = cast<AnyFunctionType>(
      vd->getInterfaceType()->getCanonicalType());
    auto sig = vd->getDeclContext()->getGenericSignatureOfContext();
    return CanAnyFunctionType::get(getCanonicalSignatureOrNull(sig),
                                   funcTy->getParams(),
                                   funcTy.getResult(),
                                   funcTy->getExtInfo());
  }
  
  case SILDeclRef::Kind::Allocator: {
    auto *cd = cast<ConstructorDecl>(vd);
    auto funcTy = cast<AnyFunctionType>(
                                   cd->getInterfaceType()->getCanonicalType());
    return getFunctionInterfaceTypeWithCaptures(*this, funcTy, c);
  }

  case SILDeclRef::Kind::Initializer: {
    auto *cd = cast<ConstructorDecl>(vd);
    auto funcTy = cast<AnyFunctionType>(
                         cd->getInitializerInterfaceType()->getCanonicalType());
    return getFunctionInterfaceTypeWithCaptures(*this, funcTy, c);
  }

  case SILDeclRef::Kind::Destroyer:
  case SILDeclRef::Kind::Deallocator:
    return getDestructorInterfaceType(cast<DestructorDecl>(vd),
                                      c.kind == SILDeclRef::Kind::Deallocator,
                                      c.isForeign);
  
  case SILDeclRef::Kind::GlobalAccessor: {
    VarDecl *var = cast<VarDecl>(vd);
    assert(var->hasStorage() &&
           "constant ref to computed global var");
    return getGlobalAccessorType(var->getInterfaceType()->getCanonicalType());
  }
  case SILDeclRef::Kind::DefaultArgGenerator:
    return getDefaultArgGeneratorInterfaceType(*this, c);
  case SILDeclRef::Kind::StoredPropertyInitializer:
    return getStoredPropertyInitializerInterfaceType(cast<VarDecl>(vd));
  case SILDeclRef::Kind::PropertyWrapperBackingInitializer:
  case SILDeclRef::Kind::PropertyWrapperInitFromProjectedValue:
    return getPropertyWrapperBackingInitializerInterfaceType(*this, c);
  case SILDeclRef::Kind::IVarInitializer:
    return getIVarInitDestroyerInterfaceType(cast<ClassDecl>(vd),
                                             c.isForeign, false);
  case SILDeclRef::Kind::IVarDestroyer:
    return getIVarInitDestroyerInterfaceType(cast<ClassDecl>(vd),
                                             c.isForeign, true);
  case SILDeclRef::Kind::AsyncEntryPoint:
    return getAsyncEntryPoint(Context);
  case SILDeclRef::Kind::EntryPoint:
    return getEntryPointInterfaceType(Context);
  case SILDeclRef::Kind::RuntimeAttributeGenerator:
    return getRuntimeAttributeGeneratorInterfaceType(*this, c);
  }

  llvm_unreachable("Unhandled SILDeclRefKind in switch.");
}

GenericSignature 
TypeConverter::getConstantGenericSignature(SILDeclRef c) {
  auto *vd = c.loc.dyn_cast<ValueDecl *>();
  
  /// Get the function generic params, including outer params.
  switch (c.kind) {
  case SILDeclRef::Kind::Func:
  case SILDeclRef::Kind::Allocator:
  case SILDeclRef::Kind::Initializer:
  case SILDeclRef::Kind::Destroyer:
  case SILDeclRef::Kind::Deallocator: {
    auto captureInfo = getLoweredLocalCaptures(c);
    return getEffectiveGenericSignature(
      *c.getAnyFunctionRef(), captureInfo);
  }
  case SILDeclRef::Kind::IVarInitializer:
  case SILDeclRef::Kind::IVarDestroyer:
    return cast<ClassDecl>(vd)->getGenericSignature();
  case SILDeclRef::Kind::DefaultArgGenerator: {
    // Use the generic environment of the original function.
    auto captureInfo = getLoweredLocalCaptures(c);
    return getEffectiveGenericSignature(
      vd->getInnermostDeclContext(), captureInfo);
  }
  case SILDeclRef::Kind::PropertyWrapperBackingInitializer:
  case SILDeclRef::Kind::PropertyWrapperInitFromProjectedValue: {
    // FIXME: It might be better to compute lowered local captures of
    // the property wrapper generator directly and collapse this into the
    // above case. For now, take the generic signature of the enclosing
    // context.
    auto *dc = vd->getDeclContext();
    if (dc->isLocalContext()) {
      SILDeclRef enclosingDecl;
      if (auto *closure = dyn_cast<AbstractClosureExpr>(dc)) {
        enclosingDecl = SILDeclRef(closure);
      } else {
        enclosingDecl = SILDeclRef(cast<AbstractFunctionDecl>(dc));
      }
      return getConstantGenericSignature(enclosingDecl);
    }
    return dc->getGenericSignatureOfContext();
  }
  case SILDeclRef::Kind::EnumElement:
  case SILDeclRef::Kind::GlobalAccessor:
  case SILDeclRef::Kind::StoredPropertyInitializer:
    return vd->getDeclContext()->getGenericSignatureOfContext();
  case SILDeclRef::Kind::EntryPoint:
  case SILDeclRef::Kind::AsyncEntryPoint:
  case SILDeclRef::Kind::RuntimeAttributeGenerator:
    llvm_unreachable("Doesn't have generic signature");
  }

  llvm_unreachable("Unhandled SILDeclRefKind in switch.");
}

GenericEnvironment *
TypeConverter::getConstantGenericEnvironment(SILDeclRef c) {
  return getConstantGenericSignature(c).getGenericEnvironment();
}

SILType TypeConverter::getSubstitutedStorageType(TypeExpansionContext context,
                                                 AbstractStorageDecl *value,
                                                 Type lvalueType) {
  // The l-value type is the result of applying substitutions to
  // the type-of-reference.  Essentially, we want to apply those
  // same substitutions to value->getType().

  // Canonicalize and lower the l-value's object type.
  AbstractionPattern origType = getAbstractionPattern(value);
  CanType substType = lvalueType->getCanonicalType();

  assert(!isa<LValueType>(substType));

  // Look through reference storage on the original type.
  auto origRefType = origType.getAs<ReferenceStorageType>();
  if (origRefType) {
    origType = origType.getReferenceStorageReferentType();
    substType = substType.getReferenceStorageReferent();
  }

  CanType substLoweredType = getLoweredRValueType(context, origType, substType);

  // Type substitution preserves structural type structure, and the
  // type-of-reference is only different in the outermost structural
  // types.  So, basically, we just need to undo the changes made by
  // getTypeOfReference and then reapply them on the substituted type.

  // The only really significant manipulation there is with @weak and
  // @unowned.
  if (origRefType) {
    substLoweredType = CanReferenceStorageType::get(substType,
                                                    origRefType->getOwnership());
  }

  return SILType::getPrimitiveAddressType(substLoweredType);
}

ProtocolDispatchStrategy
TypeConverter::getProtocolDispatchStrategy(ProtocolDecl *P) {
  // ObjC protocols use ObjC method dispatch, and Swift protocols
  // use witness tables.
  if (P->isObjC())
    return ProtocolDispatchStrategy::ObjC;
  
  return ProtocolDispatchStrategy::Swift;
}

/// If a capture references a local function, return a reference to that
/// function.
static Optional<AnyFunctionRef>
getAnyFunctionRefFromCapture(CapturedValue capture) {
  if (auto *afd = dyn_cast<AbstractFunctionDecl>(capture.getDecl()))
    return AnyFunctionRef(afd);
  return None;
}

bool
TypeConverter::hasLoweredLocalCaptures(SILDeclRef fn) {
  return !getLoweredLocalCaptures(fn).getCaptures().empty();
}

CaptureInfo
TypeConverter::getLoweredLocalCaptures(SILDeclRef fn) {
  PrettyStackTraceSILLocation stack("getting lowered local captures",
                                    fn.getAsRegularLocation(), Context);
  // If we're guaranteed to never have local captures, bail out now.
  switch (fn.kind) {
  case SILDeclRef::Kind::StoredPropertyInitializer:
  case SILDeclRef::Kind::PropertyWrapperBackingInitializer:
  case SILDeclRef::Kind::PropertyWrapperInitFromProjectedValue:
    return CaptureInfo::empty();

  default:
    if (fn.hasDecl()) {
      if (!fn.getDecl()->isLocalCapture())
        return CaptureInfo::empty();
    }

    break;
  }

  fn.isForeign = 0;

  // See if we've cached the lowered capture list for this function.
  auto found = LoweredCaptures.find(fn);
  if (found != LoweredCaptures.end())
    return found->second;
  
  // Recursively collect transitive captures from captured local functions.
  llvm::DenseSet<AnyFunctionRef> visitedFunctions;
  llvm::MapVector<ValueDecl*,CapturedValue> captures;

  // If there is a capture of 'self' with dynamic 'Self' type, it goes last so
  // that IRGen can pass dynamic 'Self' metadata.
  Optional<CapturedValue> selfCapture;

  bool capturesGenericParams = false;
  DynamicSelfType *capturesDynamicSelf = nullptr;
  OpaqueValueExpr *capturesOpaqueValue = nullptr;

  std::function<void (CaptureInfo captureInfo, DeclContext *dc)> collectCaptures;
  std::function<void (AnyFunctionRef)> collectFunctionCaptures;
  std::function<void (SILDeclRef)> collectConstantCaptures;

  collectCaptures = [&](CaptureInfo captureInfo, DeclContext *dc) {
    assert(captureInfo.hasBeenComputed());

    if (captureInfo.hasGenericParamCaptures())
      capturesGenericParams = true;
    if (captureInfo.hasDynamicSelfCapture())
      capturesDynamicSelf = captureInfo.getDynamicSelfType();
    if (captureInfo.hasOpaqueValueCapture())
      capturesOpaqueValue = captureInfo.getOpaqueValue();

    SmallVector<CapturedValue, 4> localCaptures;
    captureInfo.getLocalCaptures(localCaptures);
    for (auto capture : localCaptures) {
      // If the capture is of another local function, grab its transitive
      // captures instead.
      if (auto capturedFn = getAnyFunctionRefFromCapture(capture)) {
        collectFunctionCaptures(*capturedFn);
        continue;
      }

      // If the capture is of a computed property, grab the transitive captures
      // of its accessors.
      if (auto capturedVar = dyn_cast<VarDecl>(capture.getDecl())) {
        auto collectAccessorCaptures = [&](AccessorKind kind) {
          if (auto *accessor = capturedVar->getParsedAccessor(kind)) {
            collectFunctionCaptures(accessor);
          } else if (capturedVar->hasAttachedPropertyWrapper() ||
                     capturedVar->getOriginalWrappedProperty(
                         PropertyWrapperSynthesizedPropertyKind::Projection)) {
            // Wrapped properties have synthesized accessors.
            if (auto *accessor = capturedVar->getSynthesizedAccessor(kind))
              collectFunctionCaptures(accessor);
          }
        };

        // 'Lazy' properties don't fit into the below categorization,
        // and they have a synthesized getter, not a parsed one.
        if (capturedVar->getAttrs().hasAttribute<LazyAttr>()) {
          if (auto *getter = capturedVar->getSynthesizedAccessor(
                AccessorKind::Get))
            collectFunctionCaptures(getter);
        }

        if (!capture.isDirect()) {
          auto impl = capturedVar->getImplInfo();

          switch (impl.getReadImpl()) {
          case ReadImplKind::Stored:
            // Will capture storage later.
            break;
          case ReadImplKind::Address:
            collectAccessorCaptures(AccessorKind::Address);
            break;
          case ReadImplKind::Get:
            collectAccessorCaptures(AccessorKind::Get);
            break;
          case ReadImplKind::Read:
            collectAccessorCaptures(AccessorKind::Read);
            break;
          case ReadImplKind::Inherited:
            llvm_unreachable("inherited local variable?");
          }

          switch (impl.getWriteImpl()) {
          case WriteImplKind::Immutable:
          case WriteImplKind::Stored:
            break;
          case WriteImplKind::StoredWithObservers:
            collectAccessorCaptures(AccessorKind::WillSet);
            collectAccessorCaptures(AccessorKind::DidSet);
            break;
          case WriteImplKind::Set:
            collectAccessorCaptures(AccessorKind::Set);
            break;
          case WriteImplKind::MutableAddress:
            collectAccessorCaptures(AccessorKind::MutableAddress);
            break;
          case WriteImplKind::Modify:
            collectAccessorCaptures(AccessorKind::Modify);
            break;
          case WriteImplKind::InheritedWithObservers:
            llvm_unreachable("inherited local variable");
          }

          switch (impl.getReadWriteImpl()) {
          case ReadWriteImplKind::Immutable:
          case ReadWriteImplKind::Stored:
            break;
          case ReadWriteImplKind::MaterializeToTemporary:
            // We've already processed the read and write operations.
            break;
          case ReadWriteImplKind::MutableAddress:
            collectAccessorCaptures(AccessorKind::MutableAddress);
            break;
          case ReadWriteImplKind::Modify:
            collectAccessorCaptures(AccessorKind::Modify);
            break;
          case ReadWriteImplKind::StoredWithDidSet:
            // We've already processed the didSet operation.
            break;
          case ReadWriteImplKind::InheritedWithDidSet:
            llvm_unreachable("inherited local variable");
          }
        }

        if (!capturedVar->hasStorage())
          continue;

        // We can always capture the storage in these cases.
        Type captureType = capturedVar->getType()->getMetatypeInstanceType();

        if (auto *selfType = captureType->getAs<DynamicSelfType>()) {
          captureType = selfType->getSelfType();

          // We're capturing a 'self' value with dynamic 'Self' type;
          // handle it specially.
          //
          // However, only do this if its a 'let'; if the capture is
          // mutable, we're going to be capturing a box or an address.
          if (captureType->getClassOrBoundGenericClass() &&
              capturedVar->isLet()) {
            // If we've already captured the same value already, just merge
            // flags.
            if (selfCapture && selfCapture->getDecl() == capture.getDecl()) {
              selfCapture = selfCapture->mergeFlags(capture);
              continue;

            // Otherwise, record the canonical self capture. It will appear
            // at the end of the capture list.
            } else if (!selfCapture) {
              selfCapture = capture;
              continue;
            }

            // If we end up here, we have multiple different captured values
            // with a dynamic 'Self' type. Handle this and any subsequent
            // captures via the normal code path below.
          }
        }

        // Fall through to capture the storage.
      }

      // Collect non-function captures.
      ValueDecl *value = capture.getDecl();
      auto existing = captures.find(value);
      if (existing != captures.end()) {
        existing->second = existing->second.mergeFlags(capture);
      } else {
        captures.insert(std::pair<ValueDecl *, CapturedValue>(value, capture));
      }
    }
  };

  collectFunctionCaptures = [&](AnyFunctionRef curFn) {
    if (!curFn.getBody())
      return;

    if (!visitedFunctions.insert(curFn).second)
      return;

    PrettyStackTraceAnyFunctionRef("lowering local captures", curFn);
    auto dc = curFn.getAsDeclContext();
    collectCaptures(curFn.getCaptureInfo(), dc);

    // A function's captures also include its default arguments, because
    // when we reference a function we don't track which default arguments
    // are referenced too.
    //
    // FIXME: This should be more fine-grained -- we should only need the
    // captures for default arguments that are actually referenced.
    if (auto *AFD = curFn.getAbstractFunctionDecl()) {
      for (auto *P : *AFD->getParameters()) {
        if (P->hasDefaultExpr())
          collectCaptures(P->getDefaultArgumentCaptureInfo(), dc);
      }
    }
  };

  collectConstantCaptures = [&](SILDeclRef curFn) {
    if (curFn.isDefaultArgGenerator()) {
      PrettyStackTraceSILLocation stack("lowering local captures",
                                        fn.getAsRegularLocation(), Context);
      
      if (auto *afd = dyn_cast<AbstractFunctionDecl>(curFn.getDecl())) {
        auto *param = getParameterAt(afd, curFn.defaultArgIndex);
        if (param->hasDefaultExpr()) {
          auto dc = afd->getInnermostDeclContext();
          collectCaptures(param->getDefaultArgumentCaptureInfo(), dc);
        }
        return;
      }

      if (curFn.getDecl()->getInnermostDeclContext()
            ->getGenericSignatureOfContext())
        capturesGenericParams = true;

      return;
    }

    collectFunctionCaptures(*curFn.getAnyFunctionRef());
  };

  collectConstantCaptures(fn);

  SmallVector<CapturedValue, 4> resultingCaptures;
  for (auto capturePair : captures) {
    resultingCaptures.push_back(capturePair.second);
  }

  // If we captured an opaque value, add it.
  if (capturesOpaqueValue) {
    resultingCaptures.push_back(CapturedValue(capturesOpaqueValue, 0));
  }

  // If we captured the dynamic 'Self' type and we have a 'self' value also,
  // add it as the final capture. Otherwise, add a fake hidden capture for
  // the dynamic 'Self' metatype.
  if (selfCapture.has_value()) {
    resultingCaptures.push_back(*selfCapture);
  } else if (capturesDynamicSelf) {
    selfCapture = CapturedValue::getDynamicSelfMetadata();
    resultingCaptures.push_back(*selfCapture);
  }

  // Cache the uniqued set of transitive captures.
  CaptureInfo info{Context, resultingCaptures, capturesDynamicSelf,
                   capturesOpaqueValue, capturesGenericParams};
  auto inserted = LoweredCaptures.insert({fn, info});
  assert(inserted.second && "already in map?!");
  (void)inserted;
  return info;
}

/// Given that type1 is known to be a subtype of type2, check if the two
/// types have the same calling convention representation.
TypeConverter::ABIDifference
TypeConverter::checkForABIDifferences(SILModule &M,
                                      SILType type1, SILType type2,
                                      bool thunkOptionals) {
  // Unwrap optionals, but remember that we did.
  bool type1WasOptional = false;
  bool type2WasOptional = false;
  if (auto object = type1.getOptionalObjectType()) {
    type1WasOptional = true;
    type1 = object;
  }
  if (auto object = type2.getOptionalObjectType()) {
    type2WasOptional = true;
    type2 = object;
  }

  bool optionalityChange;
  if (thunkOptionals) {
    // Forcing IUOs always requires a thunk.
    if (type1WasOptional && !type2WasOptional)
      return ABIDifference::NeedsThunk;
  
    // Except for the above case, we should not be making a value less optional.
    
    // If we're introducing a level of optionality, only certain types are
    // ABI-compatible -- check below.
    optionalityChange = (!type1WasOptional && type2WasOptional);
  } else {
    // We haven't implemented codegen for optional thunking at all levels
    // (particularly objc_blocks at depth). Just accept ABI compatibility
    // in either direction in these cases.
    optionalityChange = type1WasOptional != type2WasOptional;
  }

  // If the types are identical and there was no optionality change,
  // we're done.
  if (type1 == type2 && !optionalityChange)
    return ABIDifference::CompatibleRepresentation;
  
  // Classes, class-constrained archetypes, and pure-ObjC existential types
  // all have single retainable pointer representation; optionality change
  // is allowed.
  if (type1.getASTType()->satisfiesClassConstraint() &&
      type2.getASTType()->satisfiesClassConstraint())
    return ABIDifference::CompatibleRepresentation;

  // Function parameters are ABI compatible if their differences are
  // trivial.
  if (auto fnTy1 = type1.getAs<SILFunctionType>()) {
    if (auto fnTy2 = type2.getAs<SILFunctionType>()) {
      // Async/synchronous conversions always need a thunk.
      if (fnTy1->isAsync() != fnTy2->isAsync())
        return ABIDifference::NeedsThunk;
      // Usin an async function without an error result in place of an async
      // function that needs an error result is not ABI compatible.
      if (fnTy2->isAsync() && !fnTy1->hasErrorResult() &&
          fnTy2->hasErrorResult())
        return ABIDifference::NeedsThunk;

      // @convention(block) is a single retainable pointer so optionality
      // change is allowed.
      if (optionalityChange)
        if (fnTy1->getRepresentation() != fnTy2->getRepresentation() ||
            fnTy1->getRepresentation() != SILFunctionTypeRepresentation::Block)
          return ABIDifference::NeedsThunk;

      return checkFunctionForABIDifferences(M, fnTy1, fnTy2);
    }
  }
  
  // Metatypes are ABI-compatible if they have the same representation.
  if (auto meta1 = type1.getAs<MetatypeType>()) {
    if (auto meta2 = type2.getAs<MetatypeType>()) {
      if (meta1->getRepresentation() == meta2->getRepresentation() &&
          (!optionalityChange ||
           meta1->getRepresentation() == MetatypeRepresentation::Thick))
        return ABIDifference::CompatibleRepresentation;
    }
  }
  
  // Existential metatypes which are not identical are only ABI-compatible
  // in @objc representation.
  //
  // Optionality change is allowed since @objc existential metatypes have a
  // single retainable pointer representation.
  if (auto meta1 = type1.getAs<ExistentialMetatypeType>()) {
    if (auto meta2 = type2.getAs<ExistentialMetatypeType>()) {
      if (meta1->getRepresentation() == meta2->getRepresentation() &&
          meta1->getRepresentation() == MetatypeRepresentation::ObjC)
        return ABIDifference::CompatibleRepresentation;
    }
  }

  // Tuple types are ABI-compatible if their elements are.
  if (!optionalityChange) {
    if (auto tuple1 = type1.getAs<TupleType>()) {
      if (auto tuple2 = type2.getAs<TupleType>()) {
        if (tuple1->getNumElements() != tuple2->getNumElements())
          return ABIDifference::NeedsThunk;
        
        for (unsigned i = 0, e = tuple1->getNumElements(); i < e; i++) {
          if (checkForABIDifferences(M,
                                     type1.getTupleElementType(i),
                                     type2.getTupleElementType(i))
                != ABIDifference::CompatibleRepresentation)
            return ABIDifference::NeedsThunk;
        }

        // Tuple lengths and elements match
        return ABIDifference::CompatibleRepresentation;
      }
    }
  }

  // The types are different, or there was an optionality change resulting
  // in a change in representation.
  return ABIDifference::NeedsThunk;
}

namespace {
class HaveDifferentAbstractStructure
    : public CanTypeDifferenceVisitor<HaveDifferentAbstractStructure> {
public:
  // Treat any sort of abstract type as equivalent.
  static bool isAbstract(CanType type) {
    return (isa<SubstitutableType>(type) || isa<DependentMemberType>(type));
  };

  // We can fast-path some of these checks by providing these two overrides:
  bool visitSubstitutableType(CanSubstitutableType type1,
                              CanSubstitutableType type2) {
    return false;
  }
  bool visitDependentMemberType(CanDependentMemberType type1,
                                CanDependentMemberType type2) {
    return false;
  }

  // We also need to handle the general case where we have different
  // kinds of substitutable types.
  bool visitDifferentComponentTypes(CanType type1, CanType type2) {
    // This is a difference only if both types aren't abstract.
    return !(isAbstract(type1) && isAbstract(type2));
  }

  // Change the rules used for SIL function types to only consider
  // the basic structure, not any substitutions.
  bool visitSILFunctionType(CanSILFunctionType type1,
                            CanSILFunctionType type2) {
    return visitSILFunctionTypeStructure(type1, type2)
        || visitSILFunctionTypeComponents(type1, type2);
  }
};
}

static bool haveDifferentAbstractStructure(CanType type1, CanType type2) {
  return HaveDifferentAbstractStructure().visit(type1, type2);
}

static TypeConverter::ABIDifference
checkForABIDifferencesInYield(TypeConverter &TC, SILModule &M,
                              SILFunctionType *fnTy1, SILYieldInfo yield1,
                              SILFunctionType *fnTy2, SILYieldInfo yield2) {
  // Require the interface types to have the same basic abstract
  // structure, ignoring any substitutions from the function type.
  // This structure is what determines the signature of the continuation
  // function.
  if (haveDifferentAbstractStructure(yield1.getInterfaceType(),
                                     yield2.getInterfaceType()))
    return TypeConverter::ABIDifference::NeedsThunk;

  // Also make sure that the actual yield types match in ABI.
  return TC.checkForABIDifferences(
      M, yield1.getSILStorageType(M, fnTy1, TypeExpansionContext::minimal()),
      yield2.getSILStorageType(M, fnTy2, TypeExpansionContext::minimal()));
}

TypeConverter::ABIDifference
TypeConverter::checkFunctionForABIDifferences(SILModule &M,
                                              SILFunctionType *fnTy1,
                                              SILFunctionType *fnTy2) {
  // For now, only differentiate representation from calling convention when
  // staging in substituted function types.
  //
  // We might still want to conditionalize this behavior even after we commit
  // substituted function types, to avoid bloating
  // IR for platforms that don't differentiate function type representations.
  bool DifferentFunctionTypesHaveDifferentRepresentation = true;
  
  // TODO: For C language types we should consider the attached Clang types.
  if (fnTy1->getLanguage() == SILFunctionLanguage::C)
    DifferentFunctionTypesHaveDifferentRepresentation = false;
  
  // Fast path -- if both functions were unwrapped from a CanSILFunctionType,
  // we might have pointer equality here.
  if (fnTy1 == fnTy2)
    return ABIDifference::CompatibleRepresentation;

  if (fnTy1->getParameters().size() != fnTy2->getParameters().size())
    return ABIDifference::NeedsThunk;

  if (fnTy1->getNumResults() != fnTy2->getNumResults())
    return ABIDifference::NeedsThunk;

  if (fnTy1->getNumYields() != fnTy2->getNumYields())
    return ABIDifference::NeedsThunk;

  // If we don't have a context but the other type does, we'll return
  // ABIDifference::ThinToThick below.
  if (fnTy1->getExtInfo().hasContext() &&
      fnTy1->getCalleeConvention() != fnTy2->getCalleeConvention())
    return ABIDifference::NeedsThunk;

  for (unsigned i : indices(fnTy1->getResults())) {
    auto result1 = fnTy1->getResults()[i];
    auto result2 = fnTy2->getResults()[i];

    if (result1.getConvention() != result2.getConvention())
      return ABIDifference::NeedsThunk;

    if (checkForABIDifferences(M,
                               result1.getSILStorageType(
                                   M, fnTy1, TypeExpansionContext::minimal()),
                               result2.getSILStorageType(
                                   M, fnTy2, TypeExpansionContext::minimal()),
                               /*thunk iuos*/ fnTy1->getLanguage() ==
                                   SILFunctionLanguage::Swift) !=
        ABIDifference::CompatibleRepresentation)
      return ABIDifference::NeedsThunk;
  }

  for (unsigned i : indices(fnTy1->getYields())) {
    auto yield1 = fnTy1->getYields()[i];
    auto yield2 = fnTy2->getYields()[i];

    if (yield1.getConvention() != yield2.getConvention())
      return ABIDifference::NeedsThunk;

    if (checkForABIDifferencesInYield(*this, M, fnTy1, yield1, fnTy2, yield2)
        != ABIDifference::CompatibleRepresentation)
      return ABIDifference::NeedsThunk;
  }

  // If one type does not have an error result, we can still trivially cast
  // (casting away an error result is only safe if the function never throws,
  // of course).
  if (fnTy1->hasErrorResult() && fnTy2->hasErrorResult()) {
    auto error1 = fnTy1->getErrorResult(), error2 = fnTy2->getErrorResult();

    if (error1.getConvention() != error2.getConvention())
      return ABIDifference::NeedsThunk;

    if (checkForABIDifferences(
            M,
            error1.getSILStorageType(M, fnTy1, TypeExpansionContext::minimal()),
            error2.getSILStorageType(M, fnTy2, TypeExpansionContext::minimal()),
            /*thunk iuos*/ fnTy1->getLanguage() ==
                SILFunctionLanguage::Swift) !=
        ABIDifference::CompatibleRepresentation)
      return ABIDifference::NeedsThunk;
  }

  // Asynchronous functions require a thunk if they differ in whether they
  // have an error result.
  if (fnTy1->hasErrorResult() != fnTy2->hasErrorResult() &&
      (fnTy1->isAsync() || fnTy2->isAsync()))
    return ABIDifference::NeedsThunk;

  for (unsigned i = 0, e = fnTy1->getParameters().size(); i < e; ++i) {
    auto param1 = fnTy1->getParameters()[i], param2 = fnTy2->getParameters()[i];
    
    if (param1.getConvention() != param2.getConvention())
      return ABIDifference::NeedsThunk;

    // Parameters are contravariant and our relation is not symmetric, so
    // make sure to flip the relation around.
    if (checkForABIDifferences(
            M,
            param2.getSILStorageType(M, fnTy2, TypeExpansionContext::minimal()),
            param1.getSILStorageType(M, fnTy1, TypeExpansionContext::minimal()),
            /*thunk iuos*/ fnTy1->getLanguage() ==
                SILFunctionLanguage::Swift) !=
        ABIDifference::CompatibleRepresentation)
      return ABIDifference::NeedsThunk;
  }

  auto rep1 = fnTy1->getRepresentation(), rep2 = fnTy2->getRepresentation();
  if (rep1 != rep2) {
    if (rep1 == SILFunctionTypeRepresentation::Thin &&
        rep2 == SILFunctionTypeRepresentation::Thick) {
      if (DifferentFunctionTypesHaveDifferentRepresentation) {
        // FIXME: check whether the representations are compatible modulo
        // context
        return ABIDifference::CompatibleCallingConvention_ThinToThick;
      } else {
        return ABIDifference::CompatibleRepresentation_ThinToThick;
      }
    }

    return ABIDifference::NeedsThunk;
  }

  if (DifferentFunctionTypesHaveDifferentRepresentation)
    return ABIDifference::CompatibleCallingConvention;
  else
    return ABIDifference::CompatibleRepresentation;
}

CanSILBoxType
TypeConverter::getInterfaceBoxTypeForCapture(ValueDecl *captured,
                                             CanType loweredInterfaceType,
                                             bool isMutable) {
  auto &C = M.getASTContext();
  auto signature = getCanonicalSignatureOrNull(
      captured->getDeclContext()->getGenericSignatureOfContext());
  
  // If the type is not dependent at all, we can form a concrete box layout.
  // We don't need to capture the generic environment.
  if (!loweredInterfaceType->hasTypeParameter()) {
    auto layout = SILLayout::get(C, nullptr,
                                 SILField(loweredInterfaceType, isMutable),
                                 /*captures generics*/ false);
    return SILBoxType::get(C, layout, {});
  }
  
  // Otherwise, the layout needs to capture the generic environment of its
  // originating scope.
  // TODO: We could conceivably minimize the captured generic environment to
  // only the parts used by the captured variable.
  
  auto layout = SILLayout::get(C, signature,
                               SILField(loweredInterfaceType, isMutable),
                               /*captures generics*/ false);
  
  // Instantiate the layout with identity substitutions.
  auto subMap = signature->getIdentitySubstitutionMap();

  auto boxTy = SILBoxType::get(C, layout, subMap);
#ifndef NDEBUG
  auto loweredContextType = loweredInterfaceType;
  auto contextBoxTy = boxTy;
  if (signature) {
    auto env = signature.getGenericEnvironment();
    loweredContextType = env->mapTypeIntoContext(loweredContextType)
                            ->getCanonicalType();
    contextBoxTy = cast<SILBoxType>(
      env->mapTypeIntoContext(contextBoxTy)
         ->getCanonicalType());
  }
  assert(contextBoxTy->getLayout()->getFields().size() == 1 &&
         getSILBoxFieldType(TypeExpansionContext::minimal(), contextBoxTy,
                            *this, 0)
                 .getASTType() == loweredContextType &&
         "box field type doesn't match capture!");
#endif
  return boxTy;
}

CanSILBoxType
TypeConverter::getContextBoxTypeForCapture(ValueDecl *captured,
                                           CanType loweredContextType,
                                           GenericEnvironment *env,
                                           bool isMutable) {
  CanType loweredInterfaceType = loweredContextType;
  if (env) {
    auto homeSig = captured->getDeclContext()
        ->getGenericSignatureOfContext();
    loweredInterfaceType =
      loweredInterfaceType->mapTypeOutOfContext()
        ->getReducedType(homeSig);
  }
  
  auto boxType = getInterfaceBoxTypeForCapture(captured,
                                               loweredInterfaceType,
                                               isMutable);
  if (env)
    boxType = cast<SILBoxType>(
      env->mapTypeIntoContext(boxType)
         ->getCanonicalType());
  
  return boxType;
}

CanSILBoxType TypeConverter::getBoxTypeForEnumElement(
    TypeExpansionContext context, SILType enumType, EnumElementDecl *elt) {

  auto *enumDecl = enumType.getEnumOrBoundGenericEnum();

  assert(elt->getDeclContext() == enumDecl);
  assert(elt->isIndirect() || elt->getParentEnum()->isIndirect());

  auto &C = M.getASTContext();
  auto boxSignature = getCanonicalSignatureOrNull(
      enumDecl->getGenericSignature());

  if (boxSignature == CanGenericSignature()) {
    auto eltIntfTy = elt->getArgumentInterfaceType();
    auto boxVarTy = getLoweredRValueType(context, eltIntfTy);
    auto layout = SILLayout::get(C, nullptr, SILField(boxVarTy, true),
                                 /*captures generics*/ false);
    return SILBoxType::get(C, layout, {});
  }

  // Use the enum's signature for the box type.
  auto boundEnum = enumType.getASTType();

  // Lower the enum element's argument in the box's context.
  auto eltIntfTy = elt->getArgumentInterfaceType();

  auto boxVarTy = getLoweredRValueType(context,
                                       getAbstractionPattern(elt), eltIntfTy);
  auto layout = SILLayout::get(C, boxSignature, SILField(boxVarTy, true),
                               /*captures generics*/ false);

  // Instantiate the layout with enum's substitution list.
  auto subMap = boundEnum->getContextSubstitutionMap(
      &M, enumDecl, enumDecl->getGenericEnvironment());

  auto boxTy = SILBoxType::get(C, layout, subMap);
  return boxTy;
}

Optional<AbstractionPattern>
TypeConverter::getConstantAbstractionPattern(SILDeclRef constant) {
  if (auto closure = constant.getAbstractClosureExpr()) {
    // Using operator[] here creates an entry in the map if one doesn't exist
    // yet, marking the fact that the lack of abstraction pattern has been
    // established and cannot be overridden by `setAbstractionPattern` later.
    return ClosureAbstractionPatterns[closure];
  }
  return None;
}

TypeExpansionContext
TypeConverter::getCaptureTypeExpansionContext(SILDeclRef constant) {
  auto found = CaptureTypeExpansionContexts.find(constant);
  if (found != CaptureTypeExpansionContexts.end()) {
    return found->second;
  }
  // Insert a minimal type expansion context into the cache, so that further
  // attempts to change it raise an error.
  auto minimal = TypeExpansionContext::minimal();
  CaptureTypeExpansionContexts.insert({constant, minimal});
  return minimal;
}

void TypeConverter::setAbstractionPattern(AbstractClosureExpr *closure,
                                          AbstractionPattern pattern) {
  auto existing = ClosureAbstractionPatterns.find(closure);
  if (existing != ClosureAbstractionPatterns.end()) {
    assert(*existing->second == pattern
     && "closure shouldn't be emitted at different abstraction level contexts");
  } else {
    ClosureAbstractionPatterns[closure] = pattern;
  }
}

void TypeConverter::setCaptureTypeExpansionContext(SILDeclRef constant,
                                                   SILModule &M) {
  if (!hasLoweredLocalCaptures(constant)) {
    return;
  }
  
  TypeExpansionContext context = constant.isSerialized()
    ? TypeExpansionContext::minimal()
    : TypeExpansionContext::maximal(constant.getAnyFunctionRef()->getAsDeclContext(),
                                    M.isWholeModule());

  auto existing = CaptureTypeExpansionContexts.find(constant);
  if (existing != CaptureTypeExpansionContexts.end()) {
    assert(existing->second == context
     && "closure shouldn't be emitted with different capture type expansion contexts");
  } else {
    // Lower in the context of the closure. Since the set of captures is a
    // private contract between the closure and its enclosing context, we
    // don't need to keep its capture types opaque.
    // The exception is if it's inlinable, in which case it might get inlined into
    // some place we need to keep opaque types opaque.
    CaptureTypeExpansionContexts.insert({constant, context});
  }
}

void TypeConverter::setLoweredAddresses() {
  assert(!LoweredAddresses);
  for (auto &pair : this->LoweredTypes) {
    pair.getSecond()->setLoweredAddresses();
  }
  LoweredAddresses = true;
}

static void countNumberOfInnerFields(unsigned &fieldsCount, TypeConverter &TC,
                                     SILType Ty,
                                     TypeExpansionContext expansion) {
  if (auto *structDecl = Ty.getStructOrBoundGenericStruct()) {
    assert(
        !structDecl->isResilient(&TC.M, expansion.getResilienceExpansion()) &&
        " FSO should not be trying to explode resilient (ie address-only) "
        "types at all");
    for (auto *prop : structDecl->getStoredProperties()) {
      SILType propTy = Ty.getFieldType(prop, TC, expansion);
      unsigned fieldsCountBefore = fieldsCount;
      countNumberOfInnerFields(fieldsCount, TC, propTy, expansion);
      if (fieldsCount == fieldsCountBefore) {
        // size of Struct(BigStructType) == size of BigStructType()
        // prevent counting its size as BigStructType()+1
        ++fieldsCount;
      }
    }
    return;
  }
  if (auto tupleTy = Ty.getAs<TupleType>()) {
    for (auto elt : tupleTy.getElementTypes()) {
      auto silElt = SILType::getPrimitiveObjectType(elt);
      countNumberOfInnerFields(fieldsCount, TC, silElt, expansion);
    }
    return;
  }
  if (auto *enumDecl = Ty.getEnumOrBoundGenericEnum()) {
    if (enumDecl->isIndirect()) {
      return;
    }
    assert(!enumDecl->isResilient(&TC.M, expansion.getResilienceExpansion()) &&
           " FSO should not be trying to explode resilient (ie address-only) "
           "types at all");
    unsigned fieldsCountBefore = fieldsCount;
    unsigned maxEnumCount = 0;
    for (auto elt : enumDecl->getAllElements()) {
      if (!elt->hasAssociatedValues())
        continue;

      if (elt->isIndirect())
        continue;

      // Although one might assume enums have a fields count of 1
      // Which holds true for current uses of this code
      // (we shouldn't expand enums)
      // Number of fields > 1 as "future proof" for this heuristic:
      // In case it is used by a pass that tries to explode enums.
      auto payloadTy = Ty.getEnumElementType(elt, TC, expansion);
      fieldsCount = 0;
      countNumberOfInnerFields(fieldsCount, TC, payloadTy, expansion);
      if (fieldsCount > maxEnumCount) {
        maxEnumCount = fieldsCount;
      }
    }
    fieldsCount = fieldsCountBefore + maxEnumCount;
    return;
  }
}

unsigned TypeConverter::countNumberOfFields(SILType Ty,
                                            TypeExpansionContext expansion) {
  auto key = std::make_pair(Ty, unsigned(expansion.getResilienceExpansion()));
  auto Iter = TypeFields.find(key);
  if (Iter != TypeFields.end()) {
    return std::max(Iter->second, 1U);
  }
  unsigned fieldsCount = 0;
  countNumberOfInnerFields(fieldsCount, *this, Ty, expansion);
  TypeFields[key] = fieldsCount;
  return std::max(fieldsCount, 1U);
}

void TypeLowering::print(llvm::raw_ostream &os) const {
  auto BOOL = [&](bool b) -> StringRef {
    if (b)
      return "true";
    return "false";
  };
  os << "Type Lowering for lowered type: " << LoweredType << ".\n"
     << "Expansion: " << getResilienceExpansion() << "\n"
     << "isTrivial: " << BOOL(Properties.isTrivial()) << ".\n"
     << "isFixedABI: " << BOOL(Properties.isFixedABI()) << ".\n"
     << "isAddressOnly: " << BOOL(Properties.isAddressOnly()) << ".\n"
     << "isResilient: " << BOOL(Properties.isResilient()) << ".\n"
     << "\n";
}

void TypeLowering::dump() const {
  print(llvm::dbgs());
}
