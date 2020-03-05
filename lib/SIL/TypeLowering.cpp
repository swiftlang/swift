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

namespace {
  /// A CRTP type visitor for deciding whether the metatype for a type
  /// is a singleton type, i.e. whether there can only ever be one
  /// such value.
  struct HasSingletonMetatype : CanTypeVisitor<HasSingletonMetatype, bool> {
    /// Class metatypes have non-trivial representation due to the
    /// possibility of subclassing.
    bool visitClassType(CanClassType type) {
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

  // If this is a non-address-only stored 'let' constant, we can capture it
  // by value.  If it is address-only, then we can't load it, so capture it
  // by its address (like a var) instead.
  if (!var->supportsMutation() &&
      (Context.LangOpts.EnableSILOpaqueValues ||
       !getTypeLowering(
            var->getType(),
            TypeExpansionContext::noOpaqueTypeArchetypesSubstitution(
                expansion.getResilienceExpansion()))
            .isAddressOnly()))
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

  // If we're capturing into a non-escaping closure, we can generally just
  // capture the address of the value as no-escape.
  return (capture.isNoEscape()
          ? CaptureKind::StorageAddress
          : CaptureKind::Box);
}

using RecursiveProperties = TypeLowering::RecursiveProperties;

static RecursiveProperties
classifyType(AbstractionPattern origType, CanType type,
             TypeConverter &TC, TypeExpansionContext expansion);

namespace {
  /// A CRTP helper class for doing things that depends on type
  /// classification.
  template <class Impl, class RetTy>
  class TypeClassifierBase
    : public CanTypeVisitor<Impl, RetTy, AbstractionPattern>
  {
    Impl &asImpl() { return *static_cast<Impl*>(this); }
  protected:
    TypeConverter &TC;
    TypeExpansionContext Expansion;
    TypeClassifierBase(TypeConverter &TC, TypeExpansionContext Expansion)
      : TC(TC), Expansion(Expansion) {}

  public:
    // The subclass should implement:
    //   // Trivial, fixed-layout, and non-address-only.
    //   RetTy handleTrivial(CanType);
    //   RetTy handleTrivial(CanType. RecursiveProperties properties);
    //   // A reference type.
    //   RetTy handleReference(CanType);
    //   // Non-trivial and address-only.
    //   RetTy handleAddressOnly(CanType, RecursiveProperties properties);
    // and, if it doesn't override handleTupleType,
    //   // An aggregate type that's non-trivial.
    //   RetTy handleNonTrivialAggregate(CanType, RecursiveProperties properties);
    //
    // Alternatively, it can just implement:
    //   RetTy handle(CanType, RecursiveProperties properties);

    /// Handle a trivial, fixed-size, loadable type.
    RetTy handleTrivial(CanType type, RecursiveProperties properties) {
      return asImpl().handle(type, properties);
    }

    RetTy handleAddressOnly(CanType type, RecursiveProperties properties) {
      return asImpl().handle(type, properties);
    }

    RetTy handleNonTrivialAggregate(CanType type,
                                    RecursiveProperties properties) {
      return asImpl().handle(type, properties);
    }

    RetTy handleTrivial(CanType type) {
      return asImpl().handleTrivial(type, RecursiveProperties::forTrivial());
    }
    RetTy handleReference(CanType type) {
      return asImpl().handle(type, RecursiveProperties::forReference());
    }

#define IMPL(TYPE, LOWERING)                                                 \
    RetTy visit##TYPE##Type(Can##TYPE##Type type, AbstractionPattern orig) { \
      return asImpl().handle##LOWERING(type);                                \
    }

    IMPL(BuiltinInteger, Trivial)
    IMPL(BuiltinIntegerLiteral, Trivial)
    IMPL(BuiltinFloat, Trivial)
    IMPL(BuiltinRawPointer, Trivial)
    IMPL(BuiltinNativeObject, Reference)
    IMPL(BuiltinBridgeObject, Reference)
    IMPL(BuiltinVector, Trivial)
    IMPL(SILToken, Trivial)
    IMPL(Class, Reference)
    IMPL(BoundGenericClass, Reference)
    IMPL(AnyMetatype, Trivial)
    IMPL(Module, Trivial)

#undef IMPL

    RetTy visitBuiltinUnsafeValueBufferType(
                                         CanBuiltinUnsafeValueBufferType type,
                                         AbstractionPattern origType) {
      return asImpl().handleAddressOnly(type, {IsNotTrivial, IsFixedABI,
                                               IsAddressOnly, IsNotResilient});
    }

    RetTy visitAnyFunctionType(CanAnyFunctionType type,
                               AbstractionPattern origType) {
      switch (type->getRepresentation()) {
      case AnyFunctionType::Representation::Swift:
      case AnyFunctionType::Representation::Block:
        // SWIFT_ENABLE_TENSORFLOW
        // TODO: Are there cases where we have to lower this differently when it
        // is @differentiable?
        return asImpl().handleReference(type);

      case AnyFunctionType::Representation::CFunctionPointer:
      case AnyFunctionType::Representation::Thin:
        return asImpl().handleTrivial(type);
      }
      llvm_unreachable("bad function representation");
    }
    
    RetTy visitSILFunctionType(CanSILFunctionType type,
                               AbstractionPattern origType) {
      // SWIFT_ENABLE_TENSORFLOW
      switch (type->getDifferentiabilityKind()) {
      case DifferentiabilityKind::Normal:
        return asImpl().visitNormalDifferentiableSILFunctionType(
            type, getNormalDifferentiableSILFunctionTypeRecursiveProperties(
                      type, origType));
      case DifferentiabilityKind::Linear:
        return asImpl().visitLinearDifferentiableSILFunctionType(
            type, getLinearDifferentiableSILFunctionTypeRecursiveProperties(
                      type, origType));
      case DifferentiabilityKind::NonDifferentiable:
        break;
      }

      // Only escaping closures are references.
      bool isSwiftEscaping = type->getExtInfo().isNoEscape() &&
                             type->getExtInfo().getRepresentation() ==
                                 SILFunctionType::Representation::Thick;
      if (type->getExtInfo().hasContext() && !isSwiftEscaping)
        return asImpl().handleReference(type);
      // No escaping closures are trivial types.
      return asImpl().handleTrivial(type);
    }

    // SWIFT_ENABLE_TENSORFLOW
    RecursiveProperties
    getNormalDifferentiableSILFunctionTypeRecursiveProperties(
        CanSILFunctionType type, AbstractionPattern origType) {
      auto &M = TC.M;
      auto origTy = type->getWithoutDifferentiability();
      // Pass the `AbstractionPattern` generic signature to
      // `SILFunctionType:getAutoDiffDerivativeFunctionType` for correct type
      // lowering.
      auto jvpTy = origTy->getAutoDiffDerivativeFunctionType(
          type->getDifferentiabilityParameterIndices(), /*resultIndex*/ 0,
          AutoDiffDerivativeFunctionKind::JVP, TC,
          LookUpConformanceInModule(&M), origType.getGenericSignatureOrNull());
      auto vjpTy = origTy->getAutoDiffDerivativeFunctionType(
          type->getDifferentiabilityParameterIndices(), /*resultIndex*/ 0,
          AutoDiffDerivativeFunctionKind::VJP, TC,
          LookUpConformanceInModule(&M), origType.getGenericSignatureOrNull());
      RecursiveProperties props;
      props.addSubobject(classifyType(origType, origTy, TC, Expansion));
      props.addSubobject(classifyType(origType, jvpTy, TC, Expansion));
      props.addSubobject(classifyType(origType, vjpTy, TC, Expansion));
      return props;
    }

    RecursiveProperties
    getLinearDifferentiableSILFunctionTypeRecursiveProperties(
        CanSILFunctionType type, AbstractionPattern origType) {
      auto &M = TC.M;
      auto origTy = type->getWithoutDifferentiability();
      auto transTy = origTy->getAutoDiffTransposeFunctionType(
          type->getDifferentiabilityParameterIndices(), TC,
          LookUpConformanceInModule(&M), origType.getGenericSignatureOrNull());
      RecursiveProperties props;
      props.addSubobject(classifyType(origType, origTy, TC, Expansion));
      props.addSubobject(classifyType(origType, transTy, TC, Expansion));
      return props;
    }

    RetTy visitNormalDifferentiableSILFunctionType(
        CanSILFunctionType type, RecursiveProperties props) {
      return handleAggregateByProperties(type, props);
    }

    RetTy visitLinearDifferentiableSILFunctionType(
        CanSILFunctionType type, RecursiveProperties props) {
      return handleAggregateByProperties(type, props);
    }

    RetTy visitLValueType(CanLValueType type,
                          AbstractionPattern origType) {
      llvm_unreachable("shouldn't get an l-value type here");
    }
    RetTy visitInOutType(CanInOutType type,
                         AbstractionPattern origType) {
      llvm_unreachable("shouldn't get an inout type here");
    }
    RetTy visitErrorType(CanErrorType type,
                         AbstractionPattern origType) {
      return asImpl().handleTrivial(type);
    }

    // Dependent types can be lowered according to their corresponding
    // abstraction pattern.

    RetTy visitAbstractTypeParamType(CanType type,
                                     AbstractionPattern origType) {
      if (origType.isTypeParameterOrOpaqueArchetype()) {
        if (origType.requiresClass()) {
          return asImpl().handleReference(type);
        } else {
          return asImpl().handleAddressOnly(type,
                                            RecursiveProperties::forOpaque());
        }
      } else {
        // If the abstraction pattern provides a concrete type, lower as that
        // type. This can occur if the abstraction pattern provides a more
        // constrained generic signature with more same-type constraints than
        // the original declaration whose type we're lowering.
        return asImpl().visit(origType.getType(), origType);
      }
    }

    RetTy visitGenericTypeParamType(CanGenericTypeParamType type,
                                    AbstractionPattern origType) {
      return visitAbstractTypeParamType(type, origType);
    }

    RetTy visitDependentMemberType(CanDependentMemberType type,
                                   AbstractionPattern origType) {
      return visitAbstractTypeParamType(type, origType);
    }

    Type getConcreteReferenceStorageReferent(Type type) {
      if (type->isTypeParameter()) {
        return TC.Context.getAnyObjectType();
      }

      return type;
    }

#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
    RetTy visit##Name##StorageType(Can##Name##StorageType type, \
                                   AbstractionPattern origType) { \
      return asImpl().handleAddressOnly(type, {IsNotTrivial, \
                                               IsFixedABI, \
                                               IsAddressOnly, \
                                               IsNotResilient}); \
    }
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
    RetTy visit##Name##StorageType(Can##Name##StorageType type, \
                                   AbstractionPattern origType) { \
      return asImpl().handleReference(type); \
    }
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
    RetTy visitLoadable##Name##StorageType(Can##Name##StorageType type, \
                                           AbstractionPattern origType) { \
      return asImpl().handleReference(type); \
    } \
    RetTy visitAddressOnly##Name##StorageType(Can##Name##StorageType type, \
                                              AbstractionPattern origType) { \
      return asImpl().handleAddressOnly(type, {IsNotTrivial, \
                                               IsFixedABI, \
                                               IsAddressOnly, \
                                               IsNotResilient}); \
    } \
    RetTy visit##Name##StorageType(Can##Name##StorageType type, \
                                   AbstractionPattern origType) { \
      auto referentType = type->getReferentType(); \
      auto concreteType = getConcreteReferenceStorageReferent(referentType); \
      if (Name##StorageType::get(concreteType, TC.Context) \
            ->isLoadable(Expansion.getResilienceExpansion())) { \
        return asImpl().visitLoadable##Name##StorageType(type, origType); \
      } else { \
        return asImpl().visitAddressOnly##Name##StorageType(type, origType); \
      } \
    }
#define UNCHECKED_REF_STORAGE(Name, ...) \
    RetTy visit##Name##StorageType(Can##Name##StorageType type, \
                                   AbstractionPattern origType) { \
      return asImpl().handleTrivial(type); \
    }
#include "swift/AST/ReferenceStorage.def"

    RetTy visitOpaqueTypeArchetypeType(CanOpaqueTypeArchetypeType ty,
                                       AbstractionPattern origType) {
      auto replacedTy = substOpaqueTypesWithUnderlyingTypes(ty, Expansion);
      if (replacedTy == ty)
        return visitArchetypeType(ty, origType);
      return this->visit(replacedTy, origType);
    }

    RetTy visitArchetypeType(CanArchetypeType type,
                             AbstractionPattern origType) {
      if (type->requiresClass()) {
        return asImpl().handleReference(type);
      }

      auto LayoutInfo = type->getLayoutConstraint();
      if (LayoutInfo) {
        if (LayoutInfo->isFixedSizeTrivial()) {
          return asImpl().handleTrivial(type);
        }

        if (LayoutInfo->isAddressOnlyTrivial()) {
          auto properties = RecursiveProperties::forTrivial();
          properties.setAddressOnly();
          return asImpl().handleAddressOnly(type, properties);
        }

        if (LayoutInfo->isRefCounted())
          return asImpl().handleReference(type);
      }
      return asImpl().handleAddressOnly(type, RecursiveProperties::forOpaque());
    }

    RetTy visitExistentialType(CanType type,
                               AbstractionPattern origType) {
      switch (SILType::getPrimitiveObjectType(type)
                .getPreferredExistentialRepresentation()) {
      case ExistentialRepresentation::None:
        llvm_unreachable("not an existential type?!");
      // Opaque existentials are address-only.
      case ExistentialRepresentation::Opaque:
        return asImpl().handleAddressOnly(type, {IsNotTrivial,
                                                 IsFixedABI,
                                                 IsAddressOnly,
                                                 IsNotResilient});
      // Class-constrained and boxed existentials are refcounted.
      case ExistentialRepresentation::Class:
      case ExistentialRepresentation::Boxed:
        return asImpl().handleReference(type);
      // Existential metatypes are trivial.
      case ExistentialRepresentation::Metatype:
        return asImpl().handleTrivial(type);
      }

      llvm_unreachable("Unhandled ExistentialRepresentation in switch.");
    }
    RetTy visitProtocolType(CanProtocolType type,
                            AbstractionPattern origType) {
      return visitExistentialType(type, origType);
    }
    RetTy visitProtocolCompositionType(CanProtocolCompositionType type,
                                       AbstractionPattern origType) {
      return visitExistentialType(type, origType);
    }

    // Enums depend on their enumerators.
    RetTy visitEnumType(CanEnumType type,
                        AbstractionPattern origType) {
      return asImpl().visitAnyEnumType(type, origType, type->getDecl());
    }
    RetTy visitBoundGenericEnumType(CanBoundGenericEnumType type,
                                    AbstractionPattern origType) {
      return asImpl().visitAnyEnumType(type, origType, type->getDecl());
    }
    
    // Structs depend on their physical fields.
    RetTy visitStructType(CanStructType type,
                          AbstractionPattern origType) {
      return asImpl().visitAnyStructType(type, origType, type->getDecl());
    }
    RetTy visitBoundGenericStructType(CanBoundGenericStructType type,
                                      AbstractionPattern origType) {
      return asImpl().visitAnyStructType(type, origType, type->getDecl());
    }

    // Tuples depend on their elements.
    RetTy visitTupleType(CanTupleType type,
                         AbstractionPattern origType) {
      RecursiveProperties props;
      for (unsigned i = 0, e = type->getNumElements(); i < e; ++i) {
        props.addSubobject(classifyType(origType.getTupleElementType(i),
                                        type.getElementType(i),
                                        TC, Expansion));
      }
      return asImpl().handleAggregateByProperties(type, props);
    }

    RetTy visitDynamicSelfType(CanDynamicSelfType type,
                               AbstractionPattern origType) {
      return this->visit(type.getSelfType(), origType);
    }
    
    RetTy visitSILBlockStorageType(CanSILBlockStorageType type,
                                   AbstractionPattern origType) {
      // Should not be loaded.
      return asImpl().handleAddressOnly(type, {IsNotTrivial,
                                               IsFixedABI,
                                               IsAddressOnly,
                                               IsNotResilient});
    }

    RetTy visitSILBoxType(CanSILBoxType type,
                          AbstractionPattern origType) {
      // Should not be loaded.
      return asImpl().handleReference(type);
    }

    RetTy handleAggregateByProperties(CanType type, RecursiveProperties props) {
      if (props.isAddressOnly()) {
        return asImpl().handleAddressOnly(type, props);
      }
      assert(props.isFixedABI() && "unsupported combination for now");
      if (props.isTrivial()) {
        return asImpl().handleTrivial(type, props);
      }
      return asImpl().handleNonTrivialAggregate(type, props);
    }
  };

  class TypeClassifier :
      public TypeClassifierBase<TypeClassifier, RecursiveProperties> {
  public:
    TypeClassifier(TypeConverter &TC,
                   TypeExpansionContext Expansion)
        : TypeClassifierBase(TC, Expansion) {}

    RecursiveProperties handle(CanType type, RecursiveProperties properties) {
      return properties;
    }

    RecursiveProperties visitAnyEnumType(CanType type,
                                         AbstractionPattern origType,
                                         EnumDecl *D) {
      // We have to look through optionals here without grabbing the
      // type lowering because the way that optionals are reabstracted
      // can trip recursion checks if we try to build a lowered type.
      if (D->isOptionalDecl()) {
        return visit(type.getOptionalObjectType(),
                     origType.getOptionalObjectType());
      }

      // Consult the type lowering.
      auto &lowering = TC.getTypeLowering(origType, type, Expansion);
      return handleClassificationFromLowering(type, lowering);
    }

    RecursiveProperties visitAnyStructType(CanType type,
                                           AbstractionPattern origType,
                                           StructDecl *D) {
      // Consult the type lowering.
      auto &lowering = TC.getTypeLowering(origType, type, Expansion);
      return handleClassificationFromLowering(type, lowering);
    }

  private:
    RecursiveProperties handleClassificationFromLowering(CanType type,
                                           const TypeLowering &lowering) {
      return handle(type, lowering.getRecursiveProperties());
    }
  };
} // end anonymous namespace

static RecursiveProperties classifyType(AbstractionPattern origType,
                                        CanType type,
                                        TypeConverter &tc,
                                        TypeExpansionContext expansion) {
  return TypeClassifier(tc, expansion).visit(type, origType);
}

/// True if the type, or the referenced type of an address
/// type, is address-only.  For example, it could be a resilient struct or
/// something of unknown size.
bool SILType::isAddressOnly(CanType type,
                            TypeConverter &tc,
                            CanGenericSignature sig,
                            TypeExpansionContext expansion) {
  return classifyType(AbstractionPattern(sig, type),
                      type, tc, expansion).isAddressOnly();
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

      // Otherwise, emit the copy value operation.
      return B.emitCopyValueOperation(loc, loadValue);
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
      for (auto &child : getChildren(B.getModule().Types)) {
        auto &childLowering = child.getLowering();
        // Skip trivial children.
        if (childLowering.isTrivial())
          continue;
        auto childIndex = child.getIndex();
        auto childValue = asImpl().emitRValueProject(B, loc, aggValue,
                                                   childIndex, childLowering);
        operation(B, loc, childIndex, childValue, childLowering);
      }
    }

    using SimpleOperationTy = void (TypeLowering::*)(SILBuilder &B,
                                                     SILLocation loc,
                                                     SILValue value) const;
    void forEachNonTrivialChild(SILBuilder &B, SILLocation loc,
                                SILValue aggValue,
                                SimpleOperationTy operation) const {
      forEachNonTrivialChild(B, loc, aggValue,
        [operation](SILBuilder &B, SILLocation loc, IndexType index,
                     SILValue childValue, const TypeLowering &childLowering) {
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

      llvm::SmallVector<SILValue, 8> loweredChildValues;
      for (auto &child : getChildren(B.getModule().Types)) {
        auto &childLowering = child.getLowering();
        SILValue childValue = asImpl().emitRValueProject(B, loc, aggValue,
                                                         child.getIndex(),
                                                         childLowering);
        if (!childLowering.isTrivial()) {
          SILValue loweredChildValue = childLowering.emitLoweredCopyChildValue(
              B, loc, childValue, style);
          loweredChildValues.push_back(loweredChildValue);
        } else {
          loweredChildValues.push_back(childValue);
        }
      }

      return rebuildAggregate(B, loc, loweredChildValues);
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue aggValue) const override {
      if (B.getFunction().hasOwnership()) {
        B.createDestroyValue(loc, aggValue);
        return;
      }

      B.emitReleaseValueAndFold(loc, aggValue);
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

  // SWIFT_ENABLE_TENSORFLOW
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
      auto paramIndices = fnTy->getDifferentiabilityParameterIndices();
      return B.createDifferentiableFunction(
          loc, paramIndices, values[0], std::make_pair(values[1], values[2]));
    }

    void lowerChildren(TypeConverter &TC,
                       SmallVectorImpl<Child> &children) const override {
      auto fnTy = getLoweredType().castTo<SILFunctionType>();
      auto numDerivativeFns = 2;
      children.reserve(numDerivativeFns + 1);
      auto origFnTy = fnTy->getWithoutDifferentiability();
      auto paramIndices = fnTy->getDifferentiabilityParameterIndices();
      children.push_back(Child{
        NormalDifferentiableFunctionTypeComponent::Original,
        TC.getTypeLowering(origFnTy, getExpansionContext())
      });
      for (AutoDiffDerivativeFunctionKind kind :
               {AutoDiffDerivativeFunctionKind::JVP,
                AutoDiffDerivativeFunctionKind::VJP}) {
        auto derivativeFnTy = origFnTy->getAutoDiffDerivativeFunctionType(
            paramIndices, 0, kind, TC,
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

  /// A lowering for loadable but non-trivial tuple types.
  class LoadableTupleTypeLowering final
      : public LoadableAggTypeLowering<LoadableTupleTypeLowering, unsigned> {
  public:
    LoadableTupleTypeLowering(CanType type, RecursiveProperties properties,
                              TypeExpansionContext forExpansion)
      : LoadableAggTypeLowering(type, properties, forExpansion) {}

    SILValue emitRValueProject(SILBuilder &B, SILLocation loc,
                               SILValue tupleValue, unsigned index,
                               const TypeLowering &eltLowering) const {
      return B.createTupleExtract(loc, tupleValue, index,
                                  eltLowering.getLoweredType());
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
      : public LoadableAggTypeLowering<LoadableStructTypeLowering, VarDecl*> {
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
      B.emitReleaseValueAndFold(loc, value);
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc, SILValue value,
                                 TypeExpansionKind style) const override {
      // Enums, we never want to expand.
      return emitDestroyValue(B, loc, value);
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
    ReferenceTypeLowering(SILType type, TypeExpansionContext forExpansion)
      : LeafLoadableTypeLowering(type, RecursiveProperties::forReference(),
                                 IsReferenceCounted, forExpansion) {}

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
      B.emitStrongReleaseAndFold(loc, value);
    }
  };

/// A type lowering for loadable @unowned types.
#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  class Loadable##Name##TypeLowering final : public LeafLoadableTypeLowering { \
  public: \
    Loadable##Name##TypeLowering(SILType type, \
                                 TypeExpansionContext forExpansion) \
      : LeafLoadableTypeLowering(type, RecursiveProperties::forReference(), \
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

    void emitDestroyAddress(SILBuilder &B, SILLocation loc,
                            SILValue addr) const override {
      if (!isTrivial())
        B.emitDestroyAddrAndFold(loc, addr);
    }

    void emitDestroyRValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      if (!isTrivial())
        B.emitDestroyAddrAndFold(loc, value);
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
                                  TypeExpansionContext forExpansion)
      : AddressOnlyTypeLowering(type,
                                {IsNotTrivial, IsFixedABI,
                                 IsAddressOnly, IsNotResilient},
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
  ///
  /// Opaque values behave like loadable leaf types in SIL.
  ///
  /// FIXME: When you remove an unreachable, just delete the method.
  class OpaqueValueTypeLowering : public LeafLoadableTypeLowering {
  public:
    OpaqueValueTypeLowering(SILType type, RecursiveProperties properties,
                            TypeExpansionContext forExpansion)
      : LeafLoadableTypeLowering(type, properties, IsNotReferenceCounted,
                                 forExpansion) {}

    void emitCopyInto(SILBuilder &B, SILLocation loc,
                      SILValue src, SILValue dest, IsTake_t isTake,
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
      return B.createCopyValue(loc, value);
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      B.createDestroyValue(loc, value);
    }
  };

  /// Build the appropriate TypeLowering subclass for the given type,
  /// which is assumed to already have been lowered.
  class LowerType
    : public TypeClassifierBase<LowerType, TypeLowering *>
  {
  public:
    LowerType(TypeConverter &TC, TypeExpansionContext Expansion)
      : TypeClassifierBase(TC, Expansion) {}

    TypeLowering *handleTrivial(CanType type) {
      return handleTrivial(type, RecursiveProperties::forTrivial());
    }

    TypeLowering *handleTrivial(CanType type,
                                RecursiveProperties properties) {
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC) TrivialTypeLowering(silType, properties, Expansion);
    }

    TypeLowering *handleReference(CanType type) {
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC) ReferenceTypeLowering(silType, Expansion);
    }

    TypeLowering *handleAddressOnly(CanType type,
                                    RecursiveProperties properties) {
      if (!TC.Context.LangOpts.EnableSILOpaqueValues) {
        auto silType = SILType::getPrimitiveAddressType(type);
        return new (TC) AddressOnlyTypeLowering(silType, properties,
                                                           Expansion);
      }
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC) OpaqueValueTypeLowering(silType, properties, Expansion);
    }

#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
    TypeLowering * \
    visit##Name##StorageType(Can##Name##StorageType type, \
                             AbstractionPattern origType) { \
      return new (TC) Loadable##Name##TypeLowering( \
                                  SILType::getPrimitiveObjectType(type), \
                                  Expansion); \
    }
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
    TypeLowering * \
    visitLoadable##Name##StorageType(Can##Name##StorageType type, \
                                     AbstractionPattern origType) { \
      return new (TC) Loadable##Name##TypeLowering( \
                                  SILType::getPrimitiveObjectType(type), \
                                  Expansion); \
    }
#include "swift/AST/ReferenceStorage.def"

    TypeLowering *
    visitBuiltinUnsafeValueBufferType(CanBuiltinUnsafeValueBufferType type,
                                      AbstractionPattern origType) {
      auto silType = SILType::getPrimitiveAddressType(type);
      return new (TC) UnsafeValueBufferTypeLowering(silType, Expansion);
    }

    TypeLowering *visitTupleType(CanTupleType tupleType,
                                 AbstractionPattern origType) {
      RecursiveProperties properties;
      for (unsigned i = 0, e = tupleType->getNumElements(); i < e; ++i) {
        auto eltType = tupleType.getElementType(i);
        auto origEltType = origType.getTupleElementType(i);
        auto &lowering = TC.getTypeLowering(origEltType, eltType, Expansion);
        properties.addSubobject(lowering.getRecursiveProperties());
      }

      return handleAggregateByProperties<LoadableTupleTypeLowering>(tupleType,
                                                                    properties);
    }

    bool handleResilience(CanType type, NominalTypeDecl *D,
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
        // the isResilent() flag above.
        if (!sameModule || Expansion.getResilienceExpansion() ==
                               ResilienceExpansion::Minimal) {
          properties.addSubobject(RecursiveProperties::forOpaque());
          return true;
        }
      }

      return false;
    }

    TypeLowering *visitAnyStructType(CanType structType,
                                     AbstractionPattern origType,
                                     StructDecl *D) {
      RecursiveProperties properties;

      if (handleResilience(structType, D, properties))
        return handleAddressOnly(structType, properties);

      auto subMap = structType->getContextSubstitutionMap(&TC.M, D);

      // Classify the type according to its stored properties.
      for (auto field : D->getStoredProperties()) {
        auto substFieldType =
          field->getInterfaceType().subst(subMap)
               ->getCanonicalType(D->getGenericSignature());
        
        // We are determining the recursive properties of the struct here,
        // not the lowered types of the fields, so instead of lowering the
        // field type against the declaration's interface type as we normally
        // would, we use the substituted field type in order to accurately
        // preserve the properties of the aggregate.
        auto origFieldType = origType.unsafeGetSubstFieldType(field);
        
        properties.addSubobject(classifyType(origFieldType, substFieldType,
                                             TC, Expansion));
      }

      return handleAggregateByProperties<LoadableStructTypeLowering>(structType,
                                                                    properties);
    }
        
    TypeLowering *visitAnyEnumType(CanType enumType,
                                   AbstractionPattern origType,
                                   EnumDecl *D) {
      RecursiveProperties properties;

      if (handleResilience(enumType, D, properties))
        return handleAddressOnly(enumType, properties);

      // If the whole enum is indirect, we lower it as if all payload
      // cases were indirect. This means a fixed-layout indirect enum
      // is always loadable and nontrivial. A resilient indirect enum
      // is still address only, because we don't know how many bits
      // are used for the discriminator, and new non-indirect cases
      // may be added resiliently later.
      if (D->isIndirect()) {
        properties.setNonTrivial();
        return new (TC) LoadableEnumTypeLowering(enumType, properties,
                                                 Expansion);
      }

      auto subMap = enumType->getContextSubstitutionMap(&TC.M, D);

      // Accumulate the properties of all direct payloads.
      for (auto elt : D->getAllElements()) {
        // No-payload elements do not affect any recursive properties.
        if (!elt->hasAssociatedValues())
          continue;

        // Indirect elements only make the type nontrivial.
        if (elt->isIndirect()) {
          properties.setNonTrivial();
          continue;
        }
        
        auto substEltType =
          elt->getArgumentInterfaceType().subst(subMap)
             ->getCanonicalType(D->getGenericSignature());
        
        auto origEltType = origType.unsafeGetSubstFieldType(elt,
                              elt->getArgumentInterfaceType()
                                 ->getCanonicalType(D->getGenericSignature()));
        properties.addSubobject(classifyType(origEltType, substEltType,
                                             TC, Expansion));
      }

      return handleAggregateByProperties<LoadableEnumTypeLowering>(enumType,
                                                                   properties);
    }

    // SWIFT_ENABLE_TENSORFLOW
    TypeLowering *
    visitNormalDifferentiableSILFunctionType(CanSILFunctionType type,
                                             RecursiveProperties props) {
      return handleAggregateByProperties
          <NormalDifferentiableSILFunctionTypeLowering>(type, props);
    }

    TypeLowering *
    visitLinearDifferentiableSILFunctionType(CanSILFunctionType type,
                                             RecursiveProperties props) {
      return handleAggregateByProperties
          <LinearDifferentiableSILFunctionTypeLowering>(type, props);
    }

    template <class LoadableLoweringClass>
    TypeLowering *handleAggregateByProperties(CanType type,
                                              RecursiveProperties props) {
      if (props.isAddressOnly()) {
        return handleAddressOnly(type, props);
      }
      assert(props.isFixedABI());
      if (props.isTrivial()) {
        return handleTrivial(type, props);
      }
      return new (TC) LoadableLoweringClass(type, props, Expansion);
    }
  };
} // end anonymous namespace

TypeConverter::TypeConverter(ModuleDecl &m)
  : M(m), Context(m.getASTContext()) {
}

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

const TypeLowering *TypeConverter::find(TypeKey k) {
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
void TypeConverter::removeNullEntry(TypeKey k) {
  if (!k.isCacheable())
    return;

  auto ck = k.getCachingKey();

  auto found = LoweredTypes.find(ck);
  if (found == LoweredTypes.end() || found->second != nullptr)
    return;

  LoweredTypes.erase(ck);
}
#endif

void TypeConverter::insert(TypeKey k, const TypeLowering *tl) {
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

    auto &substElt = substType->getElement(i);

    // Make sure we don't have something non-materializable.
    auto Flags = substElt.getParameterFlags();
    assert(Flags.getValueOwnership() == ValueOwnership::Default);
    assert(!Flags.isVariadic());

    CanType loweredSubstEltType =
        tc.getLoweredRValueType(context, origEltType, substEltType);
    changed = (changed || substEltType != loweredSubstEltType ||
               !Flags.isNone());

    // Note: we drop @escaping and @autoclosure which can still appear on
    // materializable tuple types.
    //
    // FIXME: Replace this with an assertion that the original tuple element
    // did not have any flags.
    loweredElts.emplace_back(loweredSubstEltType,
                             substElt.getName(),
                             ParameterTypeFlags());
  }

  if (!changed) return substType;

  // The cast should succeed, because if we end up with a one-element
  // tuple type here, it must have a label.
  return cast<TupleType>(CanType(TupleType::get(loweredElts, tc.Context)));
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

bool TypeConverter::hasOpaqueArchetypeOrPropertiesOrCases(CanType ty) {
  if (ty->hasOpaqueArchetype())
    return true;

  auto it = opaqueArchetypeFields.find(ty);
  if (it == opaqueArchetypeFields.end()) {
    bool res = ty->hasOpaqueArchetypePropertiesOrCases();
    opaqueArchetypeFields[ty] = res;
    return res;
  }
  return it->second;
}

const TypeLowering &
TypeConverter::getTypeLowering(AbstractionPattern origType,
                               Type origSubstType,
                               TypeExpansionContext forExpansion) {
  CanType substType = origSubstType->getCanonicalType();
  auto origHadOpaqueTypeArchetype =
      hasOpaqueArchetypeOrPropertiesOrCases(origSubstType->getCanonicalType());
  auto key = getTypeKey(origType, substType, forExpansion);
  assert(!substType->is<InOutType>());

  auto *candidateLowering = find(key.getKeyForMinimalExpansion());
  auto *lowering = getTypeLoweringForExpansion(
      key, forExpansion, candidateLowering, origHadOpaqueTypeArchetype);
  if (lowering != nullptr)
    return *lowering;

#ifndef NDEBUG
  // Catch reentrancy bugs.
  if (candidateLowering == nullptr)
    insert(key.getKeyForMinimalExpansion(), nullptr);
#endif

  // Lower the type.
  auto loweredSubstType =
      computeLoweredRValueType(forExpansion, origType, substType);

  // If that didn't change the type and the key is cachable, there's no
  // point in re-checking the table, so just construct a type lowering
  // and cache it.
  if (loweredSubstType == substType && key.isCacheable()) {
    lowering = LowerType(*this, forExpansion)
      .visit(key.SubstType, key.OrigType);

  // Otherwise, check the table at a key that would be used by the
  // SILType-based lookup path for the type we just lowered to, then cache
  // that same result at this key if possible.
  } else {
    lowering = &getTypeLoweringForLoweredType(origType,
                                              loweredSubstType,
                                              forExpansion,
                                              origHadOpaqueTypeArchetype);
  }

  if (!lowering->isResilient() && !origHadOpaqueTypeArchetype) {
    insert(key.getKeyForMinimalExpansion(), lowering);
  } else {
    insert(key, lowering);
#ifndef NDEBUG
    removeNullEntry(key.getKeyForMinimalExpansion());
#endif
  }
  return *lowering;
}

CanType
TypeConverter::computeLoweredRValueType(TypeExpansionContext forExpansion,
                                        AbstractionPattern origType,
                                        CanType substType) {
  // AST function types are turned into SIL function types:
  //   - the type is uncurried as desired
  //   - types are turned into their unbridged equivalents, depending
  //     on the abstract CC
  //   - ownership conventions are deduced
  //   - a minimal substituted generic signature is extracted to represent
  //     possible ABI-compatible substitutions
  if (auto substFnType = dyn_cast<AnyFunctionType>(substType)) {
    // If the formal type uses a C convention, it is not formally
    // abstractable, and it may be subject to implicit bridging.
    auto extInfo = substFnType->getExtInfo();
    if (getSILFunctionLanguage(extInfo.getSILRepresentation())
          == SILFunctionLanguage::C) {
      // The importer only applies fully-reversible bridging to the
      // component types of C function pointers.
      auto bridging = Bridgeability::Full;
      if (extInfo.getSILRepresentation()
                        == SILFunctionTypeRepresentation::CFunctionPointer)
        bridging = Bridgeability::None;

      // Bridge the parameters and result of the function type.
      auto bridgedFnType = getBridgedFunctionType(origType, substFnType,
                                                  extInfo, bridging);
      substFnType = bridgedFnType;

      // Also rewrite the type of the abstraction pattern.
      auto signature = origType.getGenericSignatureOrNull();
      if (origType.isTypeParameter()) {
        origType = AbstractionPattern(signature, bridgedFnType);
      } else {
        origType.rewriteType(signature, bridgedFnType);
      }
    }

    return getNativeSILFunctionType(*this, forExpansion, origType, substFnType);
  }

  // Ignore dynamic self types.
  if (auto selfType = dyn_cast<DynamicSelfType>(substType)) {
    return getLoweredRValueType(forExpansion, origType, selfType.getSelfType());
  }

  // Static metatypes are unitary and can optimized to a "thin" empty
  // representation if the type also appears as a static metatype in the
  // original abstraction pattern.
  if (auto substMeta = dyn_cast<MetatypeType>(substType)) {
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
      assert(origType.isTypeParameter());
      repr = MetatypeRepresentation::Thick;
    } else {
      // Otherwise, we're thin if the metatype is thinnable both
      // substituted and in the abstraction pattern.
      if (hasSingletonMetatype(substMeta.getInstanceType())
          && hasSingletonMetatype(origMeta.getInstanceType()))
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
  if (auto existMetatype = dyn_cast<ExistentialMetatypeType>(substType)) {
    if (existMetatype->hasRepresentation()) {
      assert(existMetatype->isLegalSILType());
      return existMetatype;
    }

    return CanExistentialMetatypeType::get(existMetatype.getInstanceType(),
                                           MetatypeRepresentation::Thick);
  }

  // Lower tuple element types.
  if (auto substTupleType = dyn_cast<TupleType>(substType)) {
    return computeLoweredTupleType(*this, forExpansion, origType,
                                   substTupleType);
  }

  // Lower the referent type of reference storage types.
  if (auto substRefType = dyn_cast<ReferenceStorageType>(substType)) {
    return computeLoweredReferenceStorageType(*this, forExpansion, origType,
                                              substRefType);
  }

  // Lower the object type of optional types.
  if (auto substObjectType = substType.getOptionalObjectType()) {
    return computeLoweredOptionalType(*this, forExpansion, origType,
                                      substType, substObjectType);
  }

  if (auto silFnTy = dyn_cast<SILFunctionType>(substType)) {
    if (!substType->hasOpaqueArchetype() ||
        !forExpansion.shouldLookThroughOpaqueTypeArchetypes())
      return substType;
    return silFnTy->substituteOpaqueArchetypes(*this, forExpansion);
  }

  // The Swift type directly corresponds to the lowered type.
  auto underlyingTy =
      substOpaqueTypesWithUnderlyingTypes(substType, forExpansion,
                                          /*allowLoweredTypes*/ true);
  if (underlyingTy != substType) {
    underlyingTy = computeLoweredRValueType(
        forExpansion,
        origType,
        underlyingTy);
  }

  return underlyingTy;
}

const TypeLowering &
TypeConverter::getTypeLowering(SILType type,
                               TypeExpansionContext forExpansion,
                               CanGenericSignature sig) {
  // The type lowering for a type parameter relies on its context.
  if (!(sig || !type.getASTType()->hasTypeParameter())) {
    llvm::errs() << "TypeConverter::getTypeLowering\n";
    type.dump();
    type.getASTType()->dump();
  }
  assert(sig || !type.getASTType()->hasTypeParameter());
  auto loweredType = type.getASTType();
  auto origHadOpaqueTypeArchetype =
      hasOpaqueArchetypeOrPropertiesOrCases(loweredType);

  return getTypeLoweringForLoweredType(
                       AbstractionPattern(sig, loweredType),
                       loweredType, forExpansion,
                       origHadOpaqueTypeArchetype);
}

const TypeLowering &
TypeConverter::getTypeLowering(SILType t, SILFunction &F) {
  return getTypeLowering(t, TypeExpansionContext(F),
                       F.getLoweredFunctionType()->getSubstGenericSignature());
}

const TypeLowering &
TypeConverter::getTypeLoweringForLoweredType(AbstractionPattern origType,
                                             CanType loweredType,
                                             TypeExpansionContext forExpansion,
                                             bool origHadOpaqueTypeArchetype) {
  assert(loweredType->isLegalSILType() && "type is not lowered!");
  (void)loweredType;
  
  // Cache the lowered type record for a contextualized type independent of the
  // abstraction pattern. Lowered type parameters can't be cached or looked up
  // without context. (TODO: We could if they match the out-of-context
  // abstraction pattern.)
  AbstractionPattern origTypeForCaching = loweredType->hasTypeParameter()
      ? AbstractionPattern::getInvalid()
      : AbstractionPattern(loweredType);
  auto key = getTypeKey(origTypeForCaching, loweredType, forExpansion);

  auto *candidateLowering = find(key.getKeyForMinimalExpansion());
  auto *lowering = getTypeLoweringForExpansion(
      key, forExpansion, candidateLowering, origHadOpaqueTypeArchetype);
  if (lowering != nullptr)
    return *lowering;

#ifndef NDEBUG
  // Catch reentrancy bugs.
  if (candidateLowering == nullptr)
    insert(key.getKeyForMinimalExpansion(), nullptr);
#endif

  if (forExpansion.shouldLookThroughOpaqueTypeArchetypes() &&
      loweredType->hasOpaqueArchetype()) {
    loweredType = computeLoweredRValueType(
        forExpansion, origType, loweredType);
  }

  lowering =
      LowerType(*this, forExpansion)
        .visit(loweredType, origType);

  if (!lowering->isResilient() && !origHadOpaqueTypeArchetype)
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
const TypeLowering *TypeConverter::
getTypeLoweringForExpansion(TypeKey key,
                            TypeExpansionContext forExpansion,
                            const TypeLowering *lowering,
                            bool origHadOpaqueTypeArchetype) {
  if (lowering == nullptr)
    return nullptr;

  if (!lowering->isResilient() && !origHadOpaqueTypeArchetype) {
    // Don't try to refine the lowering for other resilience expansions if
    // we don't expect to get a different lowering anyway. Similar if the
    // original type did not have opaque type archetypes.
    //
    // See LowerType::handleResilience() for the gory details; we only
    // set this flag if the type is resilient *and* inside our module.
    return lowering;
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
  return CanFunctionType::get({}, C.TheRawPointerType);
}

/// Get the type of a default argument generator, () -> T.
static CanAnyFunctionType getDefaultArgGeneratorInterfaceType(
                                                     SILDeclRef c) {
  auto *vd = c.getDecl();
  auto resultTy = getParameterAt(vd,
                                 c.defaultArgIndex)->getInterfaceType();
  assert(resultTy && "Didn't find default argument?");

  // The result type might be written in terms of type parameters
  // that have been made fully concrete.
  CanType canResultTy = resultTy->getCanonicalType(
                            vd->getInnermostDeclContext()
                              ->getGenericSignatureOfContext());

  // Remove @noescape from function return types. A @noescape
  // function return type is a contradiction.
  if (auto funTy = canResultTy->getAs<AnyFunctionType>()) {
    auto newExtInfo = funTy->getExtInfo().withNoEscape(false);
    canResultTy =
        adjustFunctionType(cast<AnyFunctionType>(canResultTy), newExtInfo);
  }

  // Get the generic signature from the surrounding context.
  auto sig = vd->getInnermostDeclContext()->getGenericSignatureOfContext();
  if (auto *afd = dyn_cast<AbstractFunctionDecl>(vd)) {
    auto *param = getParameterAt(afd, c.defaultArgIndex);
    if (param->hasDefaultExpr()) {
      auto captureInfo = param->getDefaultArgumentCaptureInfo();
      sig = getEffectiveGenericSignature(afd, captureInfo);
    }
  }

  return CanAnyFunctionType::get(getCanonicalSignatureOrNull(sig),
                                 {}, canResultTy);
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
    if (originalProperty->isPropertyMemberwiseInitializedWithWrappedType())
      resultTy = originalProperty->getValueInterfaceType()->getCanonicalType();
  }

  auto sig = DC->getGenericSignatureOfContext();

  return CanAnyFunctionType::get(getCanonicalSignatureOrNull(sig),
                                 {}, resultTy);
}

/// Get the type of a property wrapper backing initializer,
/// (property-type) -> backing-type.
static CanAnyFunctionType getPropertyWrapperBackingInitializerInterfaceType(
                                                     TypeConverter &TC,
                                                     VarDecl *VD) {
  CanType resultType =
      VD->getPropertyWrapperBackingPropertyType()->getCanonicalType();

  auto *DC = VD->getInnermostDeclContext();
  CanType inputType =
    VD->getParentPattern()->getType()->mapTypeOutOfContext()
          ->getCanonicalType();

  auto sig = DC->getGenericSignatureOfContext();

  AnyFunctionType::Param param(
      inputType, Identifier(),
      ParameterTypeFlags().withValueOwnership(ValueOwnership::Owned));
  return CanAnyFunctionType::get(getCanonicalSignatureOrNull(sig), {param},
                                 resultType);
}
/// Get the type of a destructor function.
static CanAnyFunctionType getDestructorInterfaceType(DestructorDecl *dd,
                                                     bool isDeallocating,
                                                     bool isForeign) {
  auto classType = dd->getDeclContext()->getDeclaredInterfaceType()
    ->getCanonicalType(dd->getGenericSignatureOfContext());

  assert((!isForeign || isDeallocating)
         && "There are no foreign destroying destructors");
  auto extInfo =
            AnyFunctionType::ExtInfo(FunctionType::Representation::Thin,
                                     /*throws*/ false);
  if (isForeign)
    extInfo = extInfo
      .withSILRepresentation(SILFunctionTypeRepresentation::ObjCMethod);
  else
    extInfo = extInfo
      .withSILRepresentation(SILFunctionTypeRepresentation::Method);

  auto &C = dd->getASTContext();
  CanType resultTy = (isDeallocating
                      ? TupleType::getEmpty(C)
                      : C.TheNativeObjectType);
  CanType methodTy = CanFunctionType::get({}, resultTy);

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
    ->getCanonicalType(cd->getGenericSignatureOfContext());

  auto resultType = (isDestroyer
                     ? TupleType::getEmpty(cd->getASTContext())
                     : classType);
  auto extInfo = AnyFunctionType::ExtInfo(FunctionType::Representation::Thin,
                                          /*throws*/ false);
  extInfo = extInfo
    .withSILRepresentation(isObjC? SILFunctionTypeRepresentation::ObjCMethod
                           : SILFunctionTypeRepresentation::Method);

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

  auto innerExtInfo = AnyFunctionType::ExtInfo(FunctionType::Representation::Thin,
                                               funcType->throws());

  return CanAnyFunctionType::get(
      getCanonicalSignatureOrNull(genericSig),
      funcType.getParams(), funcType.getResult(),
      innerExtInfo);
}

CanAnyFunctionType TypeConverter::makeConstantInterfaceType(SILDeclRef c) {
  // SWIFT_ENABLE_TENSORFLOW
  if (auto *autoDiffFuncId = c.autoDiffDerivativeFunctionIdentifier) {
    auto originalFnTy =
        makeConstantInterfaceType(c.asAutoDiffOriginalFunction());
    auto *fnTy = originalFnTy->getAutoDiffDerivativeFunctionType(
        autoDiffFuncId->getParameterIndices(), autoDiffFuncId->getKind(),
        LookUpConformanceInModule(&M));
    return cast<AnyFunctionType>(fnTy->getCanonicalType());
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
    return getDefaultArgGeneratorInterfaceType(c);
  case SILDeclRef::Kind::StoredPropertyInitializer:
    return getStoredPropertyInitializerInterfaceType(cast<VarDecl>(vd));
  case SILDeclRef::Kind::PropertyWrapperBackingInitializer:
    return getPropertyWrapperBackingInitializerInterfaceType(*this,
                                                             cast<VarDecl>(vd));
  case SILDeclRef::Kind::IVarInitializer:
    return getIVarInitDestroyerInterfaceType(cast<ClassDecl>(vd),
                                             c.isForeign, false);
  case SILDeclRef::Kind::IVarDestroyer:
    return getIVarInitDestroyerInterfaceType(cast<ClassDecl>(vd),
                                             c.isForeign, true);
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
  case SILDeclRef::Kind::EnumElement:
  case SILDeclRef::Kind::GlobalAccessor:
  case SILDeclRef::Kind::StoredPropertyInitializer:
  case SILDeclRef::Kind::PropertyWrapperBackingInitializer:
    return vd->getDeclContext()->getGenericSignatureOfContext();
  }

  llvm_unreachable("Unhandled SILDeclRefKind in switch.");
}

GenericEnvironment *
TypeConverter::getConstantGenericEnvironment(SILDeclRef c) {
  if (auto sig = getConstantGenericSignature(c))
    return sig->getGenericEnvironment();
  return nullptr;
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
    return CaptureInfo::empty();

  default:
    if (fn.hasDecl()) {
      if (!fn.getDecl()->isLocalCapture())
        return CaptureInfo::empty();
    }

    break;
  }

  fn.isForeign = 0;
  fn.isCurried = 0;
  fn.isDirectReference = 0;

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
          if (auto *accessor = capturedVar->getParsedAccessor(kind))
            collectFunctionCaptures(accessor);
        };

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
  if (selfCapture.hasValue()) {
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
  bool DifferentFunctionTypesHaveDifferentRepresentation
    = Context.LangOpts.EnableSubstSILFunctionTypesForFunctionValues;
  
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
                               result1.getSILStorageType(M, fnTy1),
                               result2.getSILStorageType(M, fnTy2),
             /*thunk iuos*/ fnTy1->getLanguage() == SILFunctionLanguage::Swift)
        != ABIDifference::CompatibleRepresentation)
      return ABIDifference::NeedsThunk;
  }

  for (unsigned i : indices(fnTy1->getYields())) {
    auto yield1 = fnTy1->getYields()[i];
    auto yield2 = fnTy2->getYields()[i];

    if (yield1.getConvention() != yield2.getConvention())
      return ABIDifference::NeedsThunk;

    if (checkForABIDifferences(M,
                               yield1.getSILStorageType(M, fnTy1),
                               yield2.getSILStorageType(M, fnTy2),
             /*thunk iuos*/ fnTy1->getLanguage() == SILFunctionLanguage::Swift)
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

    if (checkForABIDifferences(M,
                               error1.getSILStorageType(M, fnTy1),
                               error2.getSILStorageType(M, fnTy2),
              /*thunk iuos*/ fnTy1->getLanguage() == SILFunctionLanguage::Swift)
        != ABIDifference::CompatibleRepresentation)
      return ABIDifference::NeedsThunk;
  }

  for (unsigned i = 0, e = fnTy1->getParameters().size(); i < e; ++i) {
    auto param1 = fnTy1->getParameters()[i], param2 = fnTy2->getParameters()[i];
    
    if (param1.getConvention() != param2.getConvention())
      return ABIDifference::NeedsThunk;

    // Parameters are contravariant and our relation is not symmetric, so
    // make sure to flip the relation around.
    if (checkForABIDifferences(M,
                               param2.getSILStorageType(M, fnTy2),
                               param1.getSILStorageType(M, fnTy1),
              /*thunk iuos*/ fnTy1->getLanguage() == SILFunctionLanguage::Swift)
        != ABIDifference::CompatibleRepresentation)
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
                                 SILField(loweredInterfaceType, isMutable));
    return SILBoxType::get(C, layout, {});
  }
  
  // Otherwise, the layout needs to capture the generic environment of its
  // originating scope.
  // TODO: We could conceivably minimize the captured generic environment to
  // only the parts used by the captured variable.
  
  auto layout = SILLayout::get(C, signature,
                               SILField(loweredInterfaceType, isMutable));
  
  // Instantiate the layout with identity substitutions.
  auto subMap = SubstitutionMap::get(
    signature,
    [&](SubstitutableType *type) -> Type {
      return signature->getCanonicalTypeInContext(type);
    },
    MakeAbstractConformanceForGenericType());

  auto boxTy = SILBoxType::get(C, layout, subMap);
#ifndef NDEBUG
  auto loweredContextType = loweredInterfaceType;
  auto contextBoxTy = boxTy;
  if (signature) {
    auto env = signature->getGenericEnvironment();
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
        ->getCanonicalType(homeSig);
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
    auto layout = SILLayout::get(C, nullptr, SILField(boxVarTy, true));
    return SILBoxType::get(C, layout, {});
  }

  // Use the enum's signature for the box type.
  auto boundEnum = enumType.getASTType();

  // Lower the enum element's argument in the box's context.
  auto eltIntfTy = elt->getArgumentInterfaceType();

  auto boxVarTy = getLoweredRValueType(context,
                                       getAbstractionPattern(elt), eltIntfTy);
  auto layout = SILLayout::get(C, boxSignature, SILField(boxVarTy, true));

  // Instantiate the layout with enum's substitution list.
  auto subMap = boundEnum->getContextSubstitutionMap(
      &M, enumDecl, enumDecl->getGenericEnvironment());

  auto boxTy = SILBoxType::get(C, layout, subMap);
  return boxTy;
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
