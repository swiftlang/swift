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
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "swift/ClangImporter/ClangModule.h"
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

CaptureKind TypeConverter::getDeclCaptureKind(CapturedValue capture) {
  auto decl = capture.getDecl();
  if (auto *var = dyn_cast<VarDecl>(decl)) {
    assert(var->hasStorage() &&
           "should not have attempted to directly capture this variable");

    // If this is a non-address-only stored 'let' constant, we can capture it
    // by value.  If it is address-only, then we can't load it, so capture it
    // by its address (like a var) instead.
    if (var->isImmutable() &&
        (!SILModuleConventions(M).useLoweredAddresses() ||
         !getTypeLowering(var->getType()).isAddressOnly()))
      return CaptureKind::Constant;

    // In-out parameters are captured by address.
    if (var->isInOut()) {
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
    return capture.isNoEscape() ?
      CaptureKind::StorageAddress : CaptureKind::Box;
  }
  
  // "Captured" local types require no context.
  if (isa<TypeAliasDecl>(decl) || isa<GenericTypeParamDecl>(decl) ||
      isa<AssociatedTypeDecl>(decl))
    return CaptureKind::None;
  
  llvm_unreachable("function-like captures should have been lowered away");
}

using RecursiveProperties = TypeLowering::RecursiveProperties;

static RecursiveProperties
classifyType(CanType type, SILModule &M, CanGenericSignature sig,
             ResilienceExpansion expansion);

namespace {
  /// A CRTP helper class for doing things that depends on type
  /// classification.
  template <class Impl, class RetTy>
  class TypeClassifierBase : public CanTypeVisitor<Impl, RetTy> {
    Impl &asImpl() { return *static_cast<Impl*>(this); }
  protected:
    SILModule &M;
    CanGenericSignature Sig;
    ResilienceExpansion Expansion;
    TypeClassifierBase(SILModule &M, CanGenericSignature Sig,
                       ResilienceExpansion Expansion)
      : M(M), Sig(Sig), Expansion(Expansion) {}

  public:
    // The subclass should implement:
    //   // Trivial, fixed-layout, and non-address-only.
    //   RetTy handleTrivial(CanType);
    //   // A reference type.
    //   RetTy handleReference(CanType);
    //   // Non-trivial and address-only.
    //   RetTy handleAddressOnly(CanType, RecursiveProperties properties);
    // and, if it doesn't override handleTupleType,
    //   // An aggregate type that's non-trivial.
    //   RetTy handleNonTrivialAggregate(CanType, IsFixedABI_t fixed);
    //
    // Alternatively, it can just implement:
    //   RetTy handle(CanType, RecursiveProperties properties);

    /// Handle a trivial, fixed-size, loadable type.
    RetTy handleTrivial(CanType type) {
      return asImpl().handle(type, RecursiveProperties());
    }

    RetTy handleReference(CanType type) {
      return asImpl().handle(type, RecursiveProperties::forReference());
    }

    RetTy handleAddressOnly(CanType type, RecursiveProperties properties) {
      return asImpl().handle(type, properties);
    }

    RetTy handleNonTrivialAggregate(CanType type,
                                    RecursiveProperties properties) {
      return asImpl().handle(type, properties);
    }

#define IMPL(TYPE, LOWERING)                            \
    RetTy visit##TYPE##Type(Can##TYPE##Type type) {     \
      return asImpl().handle##LOWERING(type);        \
    }

    IMPL(BuiltinInteger, Trivial)
    IMPL(BuiltinIntegerLiteral, Trivial)
    IMPL(BuiltinFloat, Trivial)
    IMPL(BuiltinRawPointer, Trivial)
    IMPL(BuiltinNativeObject, Reference)
    IMPL(BuiltinBridgeObject, Reference)
    IMPL(BuiltinUnknownObject, Reference)
    IMPL(BuiltinVector, Trivial)
    IMPL(SILToken, Trivial)
    IMPL(Class, Reference)
    IMPL(BoundGenericClass, Reference)
    IMPL(AnyMetatype, Trivial)
    IMPL(Module, Trivial)

#undef IMPL

    RetTy visitBuiltinUnsafeValueBufferType(
                                         CanBuiltinUnsafeValueBufferType type) {
      return asImpl().handleAddressOnly(type, {IsNotTrivial, IsFixedABI,
                                               IsAddressOnly});
    }

    RetTy visitAnyFunctionType(CanAnyFunctionType type) {
      switch (type->getRepresentation()) {
      case AnyFunctionType::Representation::Swift:
      case AnyFunctionType::Representation::Block:
        return asImpl().handleReference(type);

      // SWIFT_ENABLE_TENSORFLOW
      case AnyFunctionType::Representation::TensorFlow:
      case AnyFunctionType::Representation::CFunctionPointer:
      case AnyFunctionType::Representation::Thin:
        return asImpl().handleTrivial(type);
      }
      llvm_unreachable("bad function representation");
    }
    
    RetTy visitSILFunctionType(CanSILFunctionType type) {
      // Only escaping closures are references.
      bool isSwiftEscaping = type->getExtInfo().isNoEscape() &&
                             type->getExtInfo().getRepresentation() ==
                                 SILFunctionType::Representation::Thick;
      if (type->getExtInfo().hasContext() && !isSwiftEscaping)
        return asImpl().handleReference(type);
      // No escaping closures are trivial types.
      return asImpl().handleTrivial(type);
    }

    RetTy visitLValueType(CanLValueType type) {
      llvm_unreachable("shouldn't get an l-value type here");
    }
    RetTy visitInOutType(CanInOutType type) {
      llvm_unreachable("shouldn't get an inout type here");
    }
    RetTy visitErrorType(CanErrorType type) {
      llvm_unreachable("shouldn't get an error type here");
    }

    // Dependent types should be contextualized before visiting.

    CanGenericSignature getGenericSignature() {
      if (Sig)
        return Sig;
      return M.Types.getCurGenericContext();
    }

    RetTy visitAbstractTypeParamType(CanType type) {
      if (auto genericSig = getGenericSignature()) {
        if (genericSig->requiresClass(type)) {
          return asImpl().handleReference(type);
        } else if (genericSig->isConcreteType(type)) {
          return asImpl().visit(genericSig->getConcreteType(type)
                                    ->getCanonicalType());
        } else {
          return asImpl().handleAddressOnly(type,
                                            RecursiveProperties::forOpaque());
        }
      }
      llvm_unreachable("should have substituted dependent type into context");

    }

    RetTy visitGenericTypeParamType(CanGenericTypeParamType type) {
      return visitAbstractTypeParamType(type);
    }

    RetTy visitDependentMemberType(CanDependentMemberType type) {
      return visitAbstractTypeParamType(type);
    }

    Type getConcreteReferenceStorageReferent(Type type) {
      if (type->isTypeParameter()) {
        auto signature = getGenericSignature();
        assert(signature && "dependent type without generic signature?!");

        if (auto concreteType = signature->getConcreteType(type))
          return concreteType->getCanonicalType();

        assert(signature->requiresClass(type));

        // If we have a superclass bound, recurse on that.  This should
        // always terminate: even if we allow
        //   <T, U: T, V: U, ...>
        // at some point the type-checker should prove acyclic-ness.
        auto bound = signature->getSuperclassBound(type);
        if (bound) {
          return getConcreteReferenceStorageReferent(bound->getCanonicalType());
        }

        auto &ctx = M.getASTContext();
        return ctx.TheUnknownObjectType;
      }

      return type;
    }

#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
    RetTy visit##Name##StorageType(Can##Name##StorageType type) { \
      return asImpl().handleAddressOnly(type, {IsNotTrivial, \
                                               IsFixedABI, \
                                               IsAddressOnly}); \
    }
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
    RetTy visit##Name##StorageType(Can##Name##StorageType type) { \
      return asImpl().handleReference(type); \
    }
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
    RetTy visitLoadable##Name##StorageType(Can##Name##StorageType type) { \
      return asImpl().handleReference(type); \
    } \
    RetTy visitAddressOnly##Name##StorageType(Can##Name##StorageType type) { \
      return asImpl().handleAddressOnly(type, {IsNotTrivial, \
                                               IsFixedABI, \
                                               IsAddressOnly}); \
    } \
    RetTy visit##Name##StorageType(Can##Name##StorageType type) { \
      auto referentType = type->getReferentType(); \
      auto concreteType = getConcreteReferenceStorageReferent(referentType); \
      auto &ctx = M.getASTContext(); \
      if (Name##StorageType::get(concreteType, ctx) \
            ->isLoadable(ResilienceExpansion::Maximal)) {         \
        return asImpl().visitLoadable##Name##StorageType(type); \
      } else { \
        return asImpl().visitAddressOnly##Name##StorageType(type); \
      } \
    }
#define UNCHECKED_REF_STORAGE(Name, ...) \
    RetTy visit##Name##StorageType(Can##Name##StorageType type) { \
      return asImpl().handleTrivial(type); \
    }
#include "swift/AST/ReferenceStorage.def"

    RetTy visitArchetypeType(CanArchetypeType type) {
      if (type->requiresClass()) {
        return asImpl().handleReference(type);
      }

      auto LayoutInfo = type->getLayoutConstraint();
      if (LayoutInfo) {
        if (LayoutInfo->isFixedSizeTrivial()) {
          return asImpl().handleTrivial(type);
        }

        if (LayoutInfo->isAddressOnlyTrivial()) {
          return asImpl().handleAddressOnly(type,
                                     {IsTrivial, IsNotFixedABI, IsAddressOnly});
        }

        if (LayoutInfo->isRefCounted())
          return asImpl().handleReference(type);
      }
      return asImpl().handleAddressOnly(type, RecursiveProperties::forOpaque());
    }

    RetTy visitExistentialType(CanType type) {
      switch (SILType::getPrimitiveObjectType(type)
                .getPreferredExistentialRepresentation(M)) {
      case ExistentialRepresentation::None:
        llvm_unreachable("not an existential type?!");
      // Opaque existentials are address-only.
      case ExistentialRepresentation::Opaque:
        return asImpl().handleAddressOnly(type, {IsNotTrivial,
                                                 IsFixedABI,
                                                 IsAddressOnly});
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
    RetTy visitProtocolType(CanProtocolType type) {
      return visitExistentialType(type);
    }
    RetTy visitProtocolCompositionType(CanProtocolCompositionType type) {
      return visitExistentialType(type);
    }

    // Enums depend on their enumerators.
    RetTy visitEnumType(CanEnumType type) {
      return asImpl().visitAnyEnumType(type, type->getDecl());
    }
    RetTy visitBoundGenericEnumType(CanBoundGenericEnumType type) {
      return asImpl().visitAnyEnumType(type, type->getDecl());
    }
    
    // Structs depend on their physical fields.
    RetTy visitStructType(CanStructType type) {
      return asImpl().visitAnyStructType(type, type->getDecl());
    }
    RetTy visitBoundGenericStructType(CanBoundGenericStructType type) {
      return asImpl().visitAnyStructType(type, type->getDecl());
    }

    // Tuples depend on their elements.
    RetTy visitTupleType(CanTupleType type) {
      RecursiveProperties props;
      for (auto eltType : type.getElementTypes()) {
        props.addSubobject(classifyType(eltType, M, Sig, Expansion));
      }
      return asImpl().handleAggregateByProperties(type, props);
    }

    RetTy visitDynamicSelfType(CanDynamicSelfType type) {
      return this->visit(type.getSelfType());
    }
    
    RetTy visitSILBlockStorageType(CanSILBlockStorageType type) {
      // Should not be loaded.
      return asImpl().handleAddressOnly(type, {IsNotTrivial, IsFixedABI,
                                               IsAddressOnly});
    }

    RetTy visitSILBoxType(CanSILBoxType type) {
      // Should not be loaded.
      return asImpl().handleReference(type);
    }

    RetTy handleAggregateByProperties(CanType type, RecursiveProperties props) {
      if (props.isAddressOnly()) {
        return asImpl().handleAddressOnly(type, props);
      }
      assert(props.isFixedABI() && "unsupported combination for now");
      if (props.isTrivial()) {
        return asImpl().handleTrivial(type);
      }
      return asImpl().handleNonTrivialAggregate(type, props);
    }
  };

  class TypeClassifier :
      public TypeClassifierBase<TypeClassifier, RecursiveProperties> {
  public:
    TypeClassifier(SILModule &M, CanGenericSignature Sig,
                   ResilienceExpansion Expansion)
        : TypeClassifierBase(M, Sig, Expansion) {}

    RecursiveProperties handle(CanType type, RecursiveProperties properties) {
      return properties;
    }

    RecursiveProperties visitAnyEnumType(CanType type, EnumDecl *D) {
      // We have to look through optionals here without grabbing the
      // type lowering because the way that optionals are reabstracted
      // can trip recursion checks if we try to build a lowered type.
      if (D->isOptionalDecl()) {
        return visit(type.getOptionalObjectType());
      }

      // Consult the type lowering.
      type = getSubstitutedTypeForTypeLowering(type);
      auto &lowering = M.Types.getTypeLowering(type);
      return handleClassificationFromLowering(type, lowering);
    }

    RecursiveProperties visitAnyStructType(CanType type, StructDecl *D) {
      // Consult the type lowering.
      type = getSubstitutedTypeForTypeLowering(type);
      auto &lowering = M.Types.getTypeLowering(type);
      return handleClassificationFromLowering(type, lowering);
    }

  private:
    CanType getSubstitutedTypeForTypeLowering(CanType type) {
      // If we're using a generic signature different from
      // M.Types.getCurGenericContext(), we have to map the
      // type into context before asking for a type lowering
      // because the rest of type lowering doesn't have a generic
      // signature plumbed through.
      if (Sig && type->hasTypeParameter()) {
        type = Sig->getCanonicalSignature()
          .getGenericEnvironment()
          ->mapTypeIntoContext(type)
          ->getCanonicalType();
      }

      return type;
    }

    RecursiveProperties handleClassificationFromLowering(CanType type,
                                           const TypeLowering &lowering) {
      return handle(type, lowering.getRecursiveProperties());
    }
  };
} // end anonymous namespace

static RecursiveProperties classifyType(CanType type, SILModule &M,
                                        CanGenericSignature sig,
                                        ResilienceExpansion expansion) {
  assert(!type->hasError() &&
         "Error types should not appear in type-checked AST");

  return TypeClassifier(M, sig, expansion).visit(type);
}

/// True if the type, or the referenced type of an address
/// type, is address-only.  For example, it could be a resilient struct or
/// something of unknown size.
bool SILType::isAddressOnly(CanType type, SILModule &M,
                            CanGenericSignature sig,
                            ResilienceExpansion expansion) {
  return classifyType(type, M, sig, expansion).isAddressOnly();
}

namespace {
  /// A class for types that can be loaded and stored in SIL.
  /// This always include loadable types, but can include address-only types if
  /// opaque values are passed by value.
  class LoadableTypeLowering : public TypeLowering {
  protected:
    LoadableTypeLowering(SILType type, RecursiveProperties properties,
                         IsReferenceCounted_t isRefCounted)
      : TypeLowering(type, properties, isRefCounted) {}

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
    TrivialTypeLowering(SILType type)
      : LoadableTypeLowering(type, {IsTrivial, IsFixedABI, IsNotAddressOnly},
                             IsNotReferenceCounted) {}

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
      if (B.getFunction().hasQualifiedOwnership()) {
        B.createStore(loc, value, addr, StoreOwnershipQualifier::Trivial);
        return;
      }
      B.createStore(loc, value, addr, StoreOwnershipQualifier::Unqualified);
    }

    SILValue emitLoad(SILBuilder &B, SILLocation loc, SILValue addr,
                      LoadOwnershipQualifier qual) const override {
      if (B.getFunction().hasQualifiedOwnership())
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
                                   IsReferenceCounted_t isRefCounted)
      : NonTrivialLoadableTypeLowering(type,
                                   {IsNotTrivial, IsFixedABI, IsNotAddressOnly},
                                       isRefCounted) {}

    /// This constructor is necessary because of opaque-values.
    NonTrivialLoadableTypeLowering(SILType type,
                                   RecursiveProperties properties,
                                   IsReferenceCounted_t isRefCounted)
      : LoadableTypeLowering(type, properties, isRefCounted) {
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
      if (B.getFunction().hasQualifiedOwnership()) {
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
      if (B.getFunction().hasQualifiedOwnership())
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
    virtual void lowerChildren(SILModule &M, SmallVectorImpl<Child> &children)
      const = 0;
    
  public:
    LoadableAggTypeLowering(CanType type)
      : NonTrivialLoadableTypeLowering(SILType::getPrimitiveObjectType(type),
                                       IsNotReferenceCounted) {
    }

    virtual SILValue rebuildAggregate(SILBuilder &B, SILLocation loc,
                                      ArrayRef<SILValue> values) const = 0;

    ArrayRef<Child> getChildren(SILModule &M) const {
      if (Children.data() == nullptr) {
        SmallVector<Child, 4> children;
        lowerChildren(M, children);
        auto isDependent = IsDependent_t(getLoweredType().hasTypeParameter());
        auto buf = operator new(sizeof(Child) * children.size(), M.Types,
                                isDependent);
        memcpy(buf, children.data(), sizeof(Child) * children.size());
        Children = {reinterpret_cast<Child*>(buf), children.size()};
      }
      return Children;
    }

    template <class T>
    void forEachNonTrivialChild(SILBuilder &B, SILLocation loc,
                                SILValue aggValue,
                                const T &operation) const {
      for (auto &child : getChildren(B.getModule())) {
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
      if (B.getFunction().hasQualifiedOwnership())
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
      for (auto &child : getChildren(B.getModule())) {
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
      if (B.getFunction().hasQualifiedOwnership()) {
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

  /// A lowering for loadable but non-trivial tuple types.
  class LoadableTupleTypeLowering final
      : public LoadableAggTypeLowering<LoadableTupleTypeLowering, unsigned> {
  public:
    LoadableTupleTypeLowering(CanType type)
      : LoadableAggTypeLowering(type) {}

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
    void lowerChildren(SILModule &M, SmallVectorImpl<Child> &children)
    const override {
      // The children are just the elements of the lowered tuple.
      auto silTy = getLoweredType();
      auto tupleTy = silTy.castTo<TupleType>();
      children.reserve(tupleTy->getNumElements());
      unsigned index = 0;
      for (auto elt : tupleTy.getElementTypes()) {
        auto silElt = SILType::getPrimitiveType(elt, silTy.getCategory());
        children.push_back(Child{index, M.Types.getTypeLowering(silElt)});
        ++index;
      }
    }
  };

  /// A lowering for loadable but non-trivial struct types.
  class LoadableStructTypeLowering final
      : public LoadableAggTypeLowering<LoadableStructTypeLowering, VarDecl*> {
  public:
    LoadableStructTypeLowering(CanType type)
      : LoadableAggTypeLowering(type) {}

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
    void lowerChildren(SILModule &M, SmallVectorImpl<Child> &children)
    const override {
      auto silTy = getLoweredType();
      auto structDecl = silTy.getStructOrBoundGenericStruct();
      assert(structDecl);
      
      for (auto prop : structDecl->getStoredProperties()) {
        SILType propTy = silTy.getFieldType(prop, M);        
        children.push_back(Child{prop, M.Types.getTypeLowering(propTy)});
      }
    }
  };
  
  /// A lowering for loadable but non-trivial enum types.
  class LoadableEnumTypeLowering final : public NonTrivialLoadableTypeLowering {
  public:
    LoadableEnumTypeLowering(CanType type)
      : NonTrivialLoadableTypeLowering(SILType::getPrimitiveObjectType(type),
                                       IsNotReferenceCounted) {}

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      if (B.getFunction().hasQualifiedOwnership())
        return B.createCopyValue(loc, value);
      B.createRetainValue(loc, value, B.getDefaultAtomicity());
      return value;
    }

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  TypeExpansionKind style) const override {
      if (B.getFunction().hasQualifiedOwnership())
        return B.createCopyValue(loc, value);
      B.createRetainValue(loc, value, B.getDefaultAtomicity());
      return value;
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      if (B.getFunction().hasQualifiedOwnership()) {
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
                             IsReferenceCounted_t isRefCounted)
      : NonTrivialLoadableTypeLowering(type, properties, isRefCounted) {}

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
    ReferenceTypeLowering(SILType type)
      : LeafLoadableTypeLowering(type, RecursiveProperties::forReference(),
                                 IsReferenceCounted) {}

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      if (isa<FunctionRefInst>(value) || isa<DynamicFunctionRefInst>(value) ||
          isa<PreviousDynamicFunctionRefInst>(value))
        return value;

      if (B.getFunction().hasQualifiedOwnership())
        return B.createCopyValue(loc, value);

      B.createStrongRetain(loc, value, B.getDefaultAtomicity());
      return value;
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      if (B.getFunction().hasQualifiedOwnership()) {
        B.createDestroyValue(loc, value);
        return;
      }
      B.emitStrongReleaseAndFold(loc, value);
    }
  };

/// A type lowering for loadable @unowned types.
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  class Loadable##Name##TypeLowering final : public LeafLoadableTypeLowering { \
  public: \
    Loadable##Name##TypeLowering(SILType type) \
      : LeafLoadableTypeLowering(type, RecursiveProperties::forReference(), \
                                 IsReferenceCounted) {} \
    SILValue emitCopyValue(SILBuilder &B, SILLocation loc, \
                           SILValue value) const override { \
      if (B.getFunction().hasQualifiedOwnership()) \
        return B.createCopyValue(loc, value); \
      B.create##Name##Retain(loc, value, B.getDefaultAtomicity()); \
      return value; \
    } \
    void emitDestroyValue(SILBuilder &B, SILLocation loc, \
                          SILValue value) const override { \
      if (B.getFunction().hasQualifiedOwnership()) { \
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
    AddressOnlyTypeLowering(SILType type, RecursiveProperties properties)
      : TypeLowering(type, properties, IsNotReferenceCounted)
    {}

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
    UnsafeValueBufferTypeLowering(SILType type)
      : AddressOnlyTypeLowering(type,
                                {IsNotTrivial, IsFixedABI, IsAddressOnly}) {}

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
    OpaqueValueTypeLowering(SILType type, RecursiveProperties properties)
      : LeafLoadableTypeLowering(type, properties, IsNotReferenceCounted) {}

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
    : public TypeClassifierBase<LowerType, const TypeLowering *>
  {
    TypeConverter &TC;
    IsDependent_t Dependent;
  public:
    LowerType(TypeConverter &TC, CanGenericSignature Sig,
              ResilienceExpansion Expansion, IsDependent_t Dependent)
      : TypeClassifierBase(TC.M, Sig, Expansion),
        TC(TC), Dependent(Dependent) {}

    const TypeLowering *
    handleTrivial(CanType type) {
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC, Dependent) TrivialTypeLowering(silType);
    }

    const TypeLowering *handleReference(CanType type) {
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC, Dependent) ReferenceTypeLowering(silType);
    }

    const TypeLowering *handleAddressOnly(CanType type, 
                                          RecursiveProperties properties) {
      if (SILModuleConventions(M).useLoweredAddresses()) {
        auto silType = SILType::getPrimitiveAddressType(type);
        return new (TC, Dependent) AddressOnlyTypeLowering(silType, properties);
      }
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC, Dependent) OpaqueValueTypeLowering(silType, properties);
    }

#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
    const TypeLowering * \
    visitLoadable##Name##StorageType(Can##Name##StorageType type) { \
      return new (TC, Dependent) Loadable##Name##TypeLowering( \
                                  SILType::getPrimitiveObjectType(type)); \
    }
#include "swift/AST/ReferenceStorage.def"

    const TypeLowering *
    visitBuiltinUnsafeValueBufferType(CanBuiltinUnsafeValueBufferType type) {
      auto silType = SILType::getPrimitiveAddressType(type);
      return new (TC, Dependent) UnsafeValueBufferTypeLowering(silType);
    }

    const TypeLowering *visitTupleType(CanTupleType tupleType) {
      RecursiveProperties properties;
      for (auto eltType : tupleType.getElementTypes()) {
        auto &lowering = TC.getTypeLowering(eltType);
        properties.addSubobject(lowering.getRecursiveProperties());
      }

      return handleAggregateByProperties<LoadableTupleTypeLowering>(tupleType,
                                                                    properties);
    }

    const TypeLowering *visitAnyStructType(CanType structType, StructDecl *D) {

      // For now, if the type does not have a fixed layout in all resilience
      // domains, we will treat it as address-only in SIL.
      if (D->isResilient(M.getSwiftModule(), Expansion))
        return handleAddressOnly(structType,
                                 RecursiveProperties::forOpaque());

      // Classify the type according to its stored properties.
      RecursiveProperties properties;
      for (auto field : D->getStoredProperties()) {
        auto substFieldType =
          structType->getTypeOfMember(D->getModuleContext(), field, nullptr);
        properties.addSubobject(classifyType(substFieldType->getCanonicalType(),
                                             M, Sig, Expansion));
      }

      return handleAggregateByProperties<LoadableStructTypeLowering>(structType,
                                                                    properties);
    }
        
    const TypeLowering *visitAnyEnumType(CanType enumType, EnumDecl *D) {
      // For now, if the type does not have a fixed layout in all resilience
      // domains, we will treat it as address-only in SIL.
      if (D->isResilient(M.getSwiftModule(), Expansion))
        return handleAddressOnly(enumType, RecursiveProperties::forOpaque());

      // If the whole enum is indirect, we lower it as if all payload
      // cases were indirect. This means a fixed-layout indirect enum
      // is always loadable and nontrivial. A resilient indirect enum
      // is still address only, because we don't know how many bits
      // are used for the discriminator.
      if (D->isIndirect()) {
        return new (TC, Dependent) LoadableEnumTypeLowering(enumType);
      }

      // Accumulate the properties of all direct payloads.
      RecursiveProperties properties;
      for (auto elt : D->getAllElements()) {
        // No-payload elements do not affect any recursive properties.
        if (!elt->hasAssociatedValues())
          continue;

        // Indirect elements only make the type nontrivial.
        if (elt->isIndirect()) {
          properties.setNonTrivial();
          continue;
        }
        
        auto substEltType = enumType->getTypeOfMember(
                              D->getModuleContext(), elt,
                              elt->getArgumentInterfaceType())
          ->getCanonicalType();
        
        properties.addSubobject(classifyType(substEltType, M, Sig, Expansion));
      }

      return handleAggregateByProperties<LoadableEnumTypeLowering>(enumType,
                                                                   properties);
    }

    template <class LoadableLoweringClass>
    const TypeLowering *handleAggregateByProperties(CanType type,
                                                    RecursiveProperties props) {
      if (props.isAddressOnly()) {
        return handleAddressOnly(type, props);
      }
      assert(props.isFixedABI());
      if (props.isTrivial()) {
        return handleTrivial(type);
      }
      return new (TC, Dependent) LoadableLoweringClass(type);
    }
  };
} // end anonymous namespace

TypeConverter::TypeConverter(SILModule &m)
  : M(m), Context(m.getASTContext()) {
}

TypeConverter::~TypeConverter() {
  // The bump pointer allocator destructor will deallocate but not destroy all
  // our independent TypeLowerings.
  for (auto &ti : IndependentTypes) {
    // Destroy only the unique entries.
    CanType srcType = ti.first.OrigType;
    if (!srcType) continue;
    CanType mappedType = ti.second->getLoweredType().getASTType();
    if (srcType == mappedType)
      ti.second->~TypeLowering();
  }
}

void *TypeLowering::operator new(size_t size, TypeConverter &tc,
                                 IsDependent_t dependent) {
  if (dependent) {
    auto &state = tc.DependentTypes.back();
    return state.BPA.Allocate(size, alignof(TypeLowering&));
  }
  return tc.IndependentBPA.Allocate(size, alignof(TypeLowering&));
}

const TypeLowering *TypeConverter::find(TypeKey k) {
  if (!k.isCacheable()) return nullptr;

  auto ck = k.getCachingKey();

  llvm::DenseMap<CachingTypeKey, const TypeLowering *> *types;
  if (k.isDependent()) {
    auto &state = DependentTypes.back();
    types = &state.Map;
  } else {
    types = &IndependentTypes;
  }

  auto found = types->find(ck);
  if (found == types->end())
    return nullptr;

  assert(found->second && "type recursion not caught in Sema");
  return found->second;
}

void TypeConverter::insert(TypeKey k, const TypeLowering *tl) {
  if (!k.isCacheable()) return;

  llvm::DenseMap<CachingTypeKey, const TypeLowering *> *types;
  if (k.isDependent()) {
    auto &state = DependentTypes.back();
    types = &state.Map;
  } else {
    types = &IndependentTypes;
  }

  (*types)[k.getCachingKey()] = tl;
}

/// Lower each of the elements of the substituted type according to
/// the abstraction pattern of the given original type.
static CanTupleType getLoweredTupleType(TypeConverter &tc,
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

    SILType silType = tc.getLoweredType(origEltType, substEltType);
    CanType loweredSubstEltType = silType.getASTType();

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

static CanType getLoweredOptionalType(TypeConverter &tc,
                                      AbstractionPattern origType,
                                      CanType substType,
                                      CanType substObjectType) {
  assert(substType.getOptionalObjectType() == substObjectType);

  CanType loweredObjectType =
      tc.getLoweredType(origType.getOptionalObjectType(), substObjectType)
          .getASTType();

  // If the object type didn't change, we don't have to rebuild anything.
  if (loweredObjectType == substObjectType) {
    return substType;
  }

  auto optDecl = tc.Context.getOptionalDecl();
  return CanType(BoundGenericEnumType::get(optDecl, Type(), loweredObjectType));
}

static CanType getLoweredReferenceStorageType(TypeConverter &tc,
                                              AbstractionPattern origType,
                                           CanReferenceStorageType substType) {
  CanType loweredReferentType =
    tc.getLoweredType(origType.getReferenceStorageReferentType(),
                      substType.getReferentType())
      .getASTType();

  if (loweredReferentType == substType.getReferentType())
    return substType;

  return CanReferenceStorageType::get(loweredReferentType,
                                      substType->getOwnership());
}

CanSILFunctionType
TypeConverter::getSILFunctionType(AbstractionPattern origType,
                                  CanFunctionType substType) {
  return getLoweredType(origType, substType)
           .castTo<SILFunctionType>();
}

const TypeLowering &
TypeConverter::getTypeLowering(AbstractionPattern origType,
                               Type origSubstType) {
  CanType substType = origSubstType->getCanonicalType();
  auto key = getTypeKey(origType, substType);
  
  assert((!key.isDependent() || getCurGenericContext())
         && "dependent type outside of generic context?!");
  assert(!substType->is<InOutType>());

  if (auto existing = find(key))
    return *existing;

  // Lower the type.
  CanType loweredSubstType =
    getLoweredRValueType(origType, substType);

  // If that didn't change the type and the key is cacheable, there's no
  // point in re-checking the table, so just construct a type lowering
  // and cache it.
  if (loweredSubstType == substType && key.isCacheable()) {
    return getTypeLoweringForUncachedLoweredType(key);
  }

  // Otherwise, check the table at a key that would be used by the
  // SILType-based lookup path for the type we just lowered to, then cache
  // that same result at this key if possible.
  AbstractionPattern origTypeForCaching =
    AbstractionPattern(getCurGenericContext(), loweredSubstType);
  auto loweredKey = getTypeKey(origTypeForCaching, loweredSubstType);

  auto &lowering = getTypeLoweringForLoweredType(loweredKey);
  insert(key, &lowering);
  return lowering;
}

CanType TypeConverter::getLoweredRValueType(AbstractionPattern origType,
                                            CanType substType) {
  assert(!substType->hasError() &&
         "Error types should not appear in type-checked AST");

  // AST function types are turned into SIL function types:
  //   - the type is uncurried as desired
  //   - types are turned into their unbridged equivalents, depending
  //     on the abstract CC
  //   - ownership conventions are deduced
  if (auto substFnType = dyn_cast<AnyFunctionType>(substType)) {
    // If the formal type uses a C convention, it is not formally
    // abstractable, and it may be subject to implicit bridging.
    auto extInfo = substFnType->getExtInfo();
    if (getSILFunctionLanguage(extInfo.getSILRepresentation())
          == SILFunctionLanguage::C) {
      // Bridge the parameters and result of the function type.
      auto bridgedFnType = getBridgedFunctionType(origType, substFnType,
                                                  extInfo);
      substFnType = bridgedFnType;

      // Also rewrite the type of the abstraction pattern.
      auto signature = getCurGenericContext();
      if (origType.isTypeParameter()) {
        origType = AbstractionPattern(signature, bridgedFnType);
      } else {
        origType.rewriteType(signature, bridgedFnType);
      }
    }

    return getNativeSILFunctionType(M, origType, substFnType);
  }

  // Ignore dynamic self types.
  if (auto selfType = dyn_cast<DynamicSelfType>(substType)) {
    return selfType.getSelfType();
  }

  // Static metatypes are unitary and can optimized to a "thin" empty
  // representation if the type also appears as a static metatype in the
  // original abstraction pattern.
  if (auto substMeta = dyn_cast<MetatypeType>(substType)) {
    // If the metatype has already been lowered, it will already carry its
    // representation.
    if (substMeta->hasRepresentation()) {
      assert(substMeta->isLegalSILType());
      return substMeta;
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
    
    CanType instanceType = substMeta.getInstanceType();

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
    return getLoweredTupleType(*this, origType, substTupleType);
  }

  // Lower the referent type of reference storage types.
  if (auto substRefType = dyn_cast<ReferenceStorageType>(substType)) {
    return getLoweredReferenceStorageType(*this, origType, substRefType);
  }

  // Lower the object type of optional types.
  if (auto substObjectType = substType.getOptionalObjectType()) {
    return getLoweredOptionalType(*this, origType, substType, substObjectType);
  }

  // The Swift type directly corresponds to the lowered type.
  return substType;
}

const TypeLowering &TypeConverter::getTypeLowering(SILType type) {
  auto loweredType = type.getASTType();
  auto key = getTypeKey(AbstractionPattern(getCurGenericContext(), loweredType),
                        loweredType);

  return getTypeLoweringForLoweredType(key);
}

const TypeLowering &
TypeConverter::getTypeLoweringForLoweredType(TypeKey key) {
  auto type = key.SubstType;
  assert(type->isLegalSILType() && "type is not lowered!");
  (void)type;

  // Re-using uncurry level 0 is reasonable because our uncurrying
  // transforms are idempotent at this level.  This means we don't
  // need a ton of redundant entries in the map.
  if (auto existing = find(key))
    return *existing;

  return getTypeLoweringForUncachedLoweredType(key);
}

/// Do type-lowering for a lowered type which is not already in the cache,
/// then insert it into the cache.
const TypeLowering &
TypeConverter::getTypeLoweringForUncachedLoweredType(TypeKey key) {
  assert(!find(key) && "re-entrant or already cached");
  assert(key.SubstType->isLegalSILType() && "type is not already lowered");

#ifndef NDEBUG
  // Catch reentrancy bugs.
  insert(key, nullptr);
#endif

  // FIXME: Get expansion from SILFunction
  auto *theInfo = LowerType(*this,
                            CanGenericSignature(),
                            ResilienceExpansion::Minimal,
                            key.isDependent()).visit(key.SubstType);

  if (key.OrigType.isForeign()) {
    assert(theInfo->isLoadable() && "Cannot lower address-only type with "
           "foreign abstraction pattern");
  }

  insert(key, theInfo);
  return *theInfo;
}

/// Get the type of a global variable accessor function, () -> RawPointer.
static CanAnyFunctionType getGlobalAccessorType(CanType varType) {
  ASTContext &C = varType->getASTContext();
  return CanFunctionType::get({}, C.TheRawPointerType);
}

/// Get the type of a default argument generator, () -> T.
static CanAnyFunctionType getDefaultArgGeneratorInterfaceType(
                                                     TypeConverter &TC,
                                                     ValueDecl *VD,
                                                     DeclContext *DC,
                                                     unsigned DefaultArgIndex) {
  auto resultTy = getParameterAt(VD, DefaultArgIndex)->getInterfaceType();
  assert(resultTy && "Didn't find default argument?");

  // The result type might be written in terms of type parameters
  // that have been made fully concrete.
  CanType canResultTy = resultTy->getCanonicalType(
                            DC->getGenericSignatureOfContext());

  // Remove @noescape from function return types. A @noescape
  // function return type is a contradiction.
  if (auto funTy = canResultTy->getAs<AnyFunctionType>()) {
    auto newExtInfo = funTy->getExtInfo().withNoEscape(false);
    canResultTy =
        adjustFunctionType(cast<AnyFunctionType>(canResultTy), newExtInfo);
  }

  // Get the generic signature from the surrounding context.
  auto funcInfo = TC.getConstantInfo(SILDeclRef(VD));
  return CanAnyFunctionType::get(funcInfo.FormalType.getOptGenericSignature(),
                                 {}, canResultTy);
}

/// Get the type of a stored property initializer, () -> T.
static CanAnyFunctionType getStoredPropertyInitializerInterfaceType(
                                                     TypeConverter &TC,
                                                     VarDecl *VD) {
  auto *DC = VD->getDeclContext();
  CanType resultTy =
    VD->getParentPattern()->getType()->mapTypeOutOfContext()
          ->getCanonicalType();
  auto sig = TC.getEffectiveGenericSignature(DC);

  return CanAnyFunctionType::get(sig, {}, resultTy);
}

/// Get the type of a destructor function.
static CanAnyFunctionType getDestructorInterfaceType(TypeConverter &TC,
                                                     DestructorDecl *dd,
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

  auto &C = TC.Context;
  CanType resultTy = (isDeallocating
                      ? TupleType::getEmpty(C)
                      : C.TheNativeObjectType);
  CanType methodTy = CanFunctionType::get({}, resultTy);

  auto sig = TC.getEffectiveGenericSignature(dd);
  FunctionType::Param args[] = {FunctionType::Param(classType)};
  return CanAnyFunctionType::get(sig, llvm::makeArrayRef(args),
                                 methodTy, extInfo);
}

/// Retrieve the type of the ivar initializer or destroyer method for
/// a class.
static CanAnyFunctionType getIVarInitDestroyerInterfaceType(TypeConverter &TC,
                                                            ClassDecl *cd,
                                                            bool isObjC,
                                                            bool isDestroyer) {
  auto classType = cd->getDeclaredInterfaceType()
    ->getCanonicalType(cd->getGenericSignatureOfContext());

  auto resultType = (isDestroyer
                     ? TupleType::getEmpty(TC.Context)
                     : classType);
  auto extInfo = AnyFunctionType::ExtInfo(FunctionType::Representation::Thin,
                                          /*throws*/ false);
  extInfo = extInfo
    .withSILRepresentation(isObjC? SILFunctionTypeRepresentation::ObjCMethod
                           : SILFunctionTypeRepresentation::Method);

  resultType = CanFunctionType::get({}, resultType, extInfo);
  auto sig = TC.getEffectiveGenericSignature(cd);
  FunctionType::Param args[] = {FunctionType::Param(classType)};
  return CanAnyFunctionType::get(sig, llvm::makeArrayRef(args),
                                 resultType, extInfo);
}

GenericEnvironment *
TypeConverter::getEffectiveGenericEnvironment(AnyFunctionRef fn,
                                              CaptureInfo captureInfo) {
  auto dc = fn.getAsDeclContext();

  if (getEffectiveGenericSignature(fn, captureInfo))
    return dc->getGenericEnvironmentOfContext();

  return nullptr;
}

CanGenericSignature
TypeConverter::getEffectiveGenericSignature(DeclContext *dc) {
  if (auto sig = dc->getGenericSignatureOfContext()) {
    if (sig->areAllParamsConcrete())
      return nullptr;
    return sig->getCanonicalSignature();
  }

  return nullptr;
}

CanGenericSignature
TypeConverter::getEffectiveGenericSignature(AnyFunctionRef fn,
                                            CaptureInfo captureInfo) {
  auto dc = fn.getAsDeclContext();

  if (dc->getParent()->isLocalContext() &&
      !captureInfo.hasGenericParamCaptures())
    return nullptr;

  return getEffectiveGenericSignature(dc);
}

CanAnyFunctionType
TypeConverter::getFunctionInterfaceTypeWithCaptures(CanAnyFunctionType funcType,
                                                    AnyFunctionRef theClosure) {
  // Get transitive closure of value captured by this function, and any
  // captured functions.
  auto captureInfo = getLoweredLocalCaptures(theClosure);

  // Capture generic parameters from the enclosing context if necessary.
  CanGenericSignature genericSig = getEffectiveGenericSignature(theClosure,
                                                                captureInfo);

  auto innerExtInfo = AnyFunctionType::ExtInfo(FunctionType::Representation::Thin,
                                               funcType->throws());

  return CanAnyFunctionType::get(genericSig, funcType.getParams(),
                                 funcType.getResult(), innerExtInfo);
}

CanAnyFunctionType TypeConverter::makeConstantInterfaceType(SILDeclRef c) {
  auto *vd = c.loc.dyn_cast<ValueDecl *>();

  switch (c.kind) {
  case SILDeclRef::Kind::Func: {
    if (auto *ACE = c.loc.dyn_cast<AbstractClosureExpr *>()) {
      // FIXME: Closures could have an interface type computed by Sema.
      auto funcTy = cast<AnyFunctionType>(ACE->getType()->getCanonicalType());
      funcTy = cast<AnyFunctionType>(
          funcTy->mapTypeOutOfContext()
              ->getCanonicalType());
      return getFunctionInterfaceTypeWithCaptures(funcTy, ACE);
    }

    FuncDecl *func = cast<FuncDecl>(vd);
    auto funcTy = cast<AnyFunctionType>(
        func->getInterfaceType()->eraseDynamicSelfType()->getCanonicalType());
    return getFunctionInterfaceTypeWithCaptures(funcTy, func);
  }

  case SILDeclRef::Kind::EnumElement:
    return cast<AnyFunctionType>(vd->getInterfaceType()->getCanonicalType());
  
  case SILDeclRef::Kind::Allocator: {
    auto *cd = cast<ConstructorDecl>(vd);
    auto funcTy = cast<AnyFunctionType>(
                                   cd->getInterfaceType()->getCanonicalType());
    return getFunctionInterfaceTypeWithCaptures(funcTy, cd);
  }

  case SILDeclRef::Kind::Initializer: {
    auto *cd = cast<ConstructorDecl>(vd);
    auto funcTy = cast<AnyFunctionType>(
                         cd->getInitializerInterfaceType()->getCanonicalType());
    return getFunctionInterfaceTypeWithCaptures(funcTy, cd);
  }

  case SILDeclRef::Kind::Destroyer:
  case SILDeclRef::Kind::Deallocator:
    return getDestructorInterfaceType(*this,
                                      cast<DestructorDecl>(vd),
                                      c.kind == SILDeclRef::Kind::Deallocator,
                                      c.isForeign);
  
  case SILDeclRef::Kind::GlobalAccessor: {
    VarDecl *var = cast<VarDecl>(vd);
    assert(var->hasStorage() &&
           "constant ref to computed global var");
    return getGlobalAccessorType(var->getInterfaceType()->getCanonicalType());
  }
  case SILDeclRef::Kind::DefaultArgGenerator:
    return getDefaultArgGeneratorInterfaceType(*this, vd, vd->getDeclContext(),
                                               c.defaultArgIndex);
  case SILDeclRef::Kind::StoredPropertyInitializer:
    return getStoredPropertyInitializerInterfaceType(*this,
                                                     cast<VarDecl>(vd));
  case SILDeclRef::Kind::IVarInitializer:
    return getIVarInitDestroyerInterfaceType(*this,
                                             cast<ClassDecl>(vd),
                                             c.isForeign, false);
  case SILDeclRef::Kind::IVarDestroyer:
    return getIVarInitDestroyerInterfaceType(*this,
                                             cast<ClassDecl>(vd),
                                             c.isForeign, true);
  }

  llvm_unreachable("Unhandled SILDeclRefKind in switch.");
}

/// Get the generic environment for an entity.
GenericEnvironment *
TypeConverter::getConstantGenericEnvironment(SILDeclRef c) {
  auto *vd = c.loc.dyn_cast<ValueDecl *>();
  
  /// Get the function generic params, including outer params.
  switch (c.kind) {
  case SILDeclRef::Kind::Func: {
    if (auto *ACE = c.getAbstractClosureExpr()) {
      auto captureInfo = getLoweredLocalCaptures(ACE);

      return getEffectiveGenericEnvironment(ACE, captureInfo);
    }
    FuncDecl *func = cast<FuncDecl>(vd);
    auto captureInfo = getLoweredLocalCaptures(func);

    return getEffectiveGenericEnvironment(func, captureInfo);
  }
  case SILDeclRef::Kind::EnumElement: {
    auto eltDecl = cast<EnumElementDecl>(vd);
    return eltDecl->getDeclContext()->getGenericEnvironmentOfContext();
  }
  case SILDeclRef::Kind::Allocator:
  case SILDeclRef::Kind::Initializer:
  case SILDeclRef::Kind::Destroyer:
  case SILDeclRef::Kind::Deallocator: {
    auto *afd = cast<AbstractFunctionDecl>(vd);
    auto captureInfo = getLoweredLocalCaptures(afd);
    return getEffectiveGenericEnvironment(afd, captureInfo);
  }
  case SILDeclRef::Kind::GlobalAccessor:
    return vd->getDeclContext()->getGenericEnvironmentOfContext();
  case SILDeclRef::Kind::IVarInitializer:
  case SILDeclRef::Kind::IVarDestroyer:
    return cast<ClassDecl>(vd)->getGenericEnvironmentOfContext();
  case SILDeclRef::Kind::DefaultArgGenerator:
    // Use the generic environment of the original function.
    return getConstantGenericEnvironment(SILDeclRef(c.getDecl()));
  case SILDeclRef::Kind::StoredPropertyInitializer:
    // Use the generic environment of the containing type.
    return c.getDecl()->getDeclContext()->getGenericEnvironmentOfContext();
  }

  llvm_unreachable("Unhandled SILDeclRefKind in switch.");
}

SILType TypeConverter::getSubstitutedStorageType(AbstractStorageDecl *value,
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

  SILType silSubstType = getLoweredType(origType, substType).getAddressType();
  substType = silSubstType.getASTType();

  // Type substitution preserves structural type structure, and the
  // type-of-reference is only different in the outermost structural
  // types.  So, basically, we just need to undo the changes made by
  // getTypeOfReference and then reapply them on the substituted type.

  // The only really significant manipulation there is with @weak and
  // @unowned.
  if (origRefType) {
    substType = CanType(ReferenceStorageType::get(substType,
                                                  origRefType->getOwnership(),
                                                  Context));
    return SILType::getPrimitiveType(substType, SILValueCategory::Address);
  }

  return silSubstType;
}

void TypeConverter::pushGenericContext(CanGenericSignature sig) {
  // If the generic signature is empty, this is a no-op.
  if (!sig)
    return;

  DependentTypeState state(sig);
  DependentTypes.push_back(std::move(state));
}

void TypeConverter::popGenericContext(CanGenericSignature sig) {
  // If the generic signature is empty, this is a no-op.
  if (!sig)
    return;

  DependentTypeState &state = DependentTypes.back();
  assert(state.Sig == sig && "unpaired push/pop");
  
  // Erase our cached TypeLowering objects and associated mappings for dependent
  // types.
  // Resetting the DependentBPA will deallocate but not run the destructor of
  // the dependent TypeLowerings.
  for (auto &ti : state.Map) {
    // Destroy only the unique entries.
    CanType srcType = ti.first.OrigType;
    if (!srcType) continue;
    CanType mappedType = ti.second->getLoweredType().getASTType();
    if (srcType == mappedType)
      ti.second->~TypeLowering();
  }

  DependentTypes.pop_back();
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
TypeConverter::hasLoweredLocalCaptures(AnyFunctionRef fn) {
  return !getLoweredLocalCaptures(fn).getCaptures().empty();
}

CaptureInfo
TypeConverter::getLoweredLocalCaptures(AnyFunctionRef fn) {
  // First, bail out if there are no local captures at all.
  if (!fn.getCaptureInfo().hasLocalCaptures() &&
      !fn.getCaptureInfo().hasDynamicSelfCapture()) {
    CaptureInfo info;
    info.setGenericParamCaptures(
        fn.getCaptureInfo().hasGenericParamCaptures());
    return info;
  };

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
  
  std::function<void (AnyFunctionRef)> collectFunctionCaptures
  = [&](AnyFunctionRef curFn) {
    if (!visitedFunctions.insert(curFn).second)
      return;
  
    if (curFn.getCaptureInfo().hasGenericParamCaptures())
      capturesGenericParams = true;
    if (curFn.getCaptureInfo().hasDynamicSelfCapture())
      capturesDynamicSelf = curFn.getCaptureInfo().getDynamicSelfType();

    SmallVector<CapturedValue, 4> localCaptures;
    curFn.getCaptureInfo().getLocalCaptures(localCaptures);
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
        if (!capture.isDirect()) {
          auto impl = capturedVar->getImplInfo();

          switch (impl.getReadImpl()) {
          case ReadImplKind::Stored:
            // Will capture storage later.
            break;
          case ReadImplKind::Address:
            collectFunctionCaptures(capturedVar->getAddressor());
            break;
          case ReadImplKind::Get:
            collectFunctionCaptures(capturedVar->getGetter());
            break;
          case ReadImplKind::Read:
            collectFunctionCaptures(capturedVar->getReadCoroutine());
            break;
          case ReadImplKind::Inherited:
            llvm_unreachable("inherited local variable?");
          }

          switch (impl.getWriteImpl()) {
          case WriteImplKind::Immutable:
          case WriteImplKind::Stored:
            break;
          case WriteImplKind::StoredWithObservers:
          case WriteImplKind::Set:
            collectFunctionCaptures(capturedVar->getSetter());
            break;
          case WriteImplKind::MutableAddress:
            collectFunctionCaptures(capturedVar->getMutableAddressor());
            break;
          case WriteImplKind::Modify:
            collectFunctionCaptures(capturedVar->getModifyCoroutine());
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
            collectFunctionCaptures(capturedVar->getMutableAddressor());
            break;
          case ReadWriteImplKind::Modify:
            collectFunctionCaptures(capturedVar->getModifyCoroutine());
            break;
          }
        }

        if (!capturedVar->hasStorage())
          continue;

        // We can always capture the storage in these cases.
        Type captureType = capturedVar->getType();
        if (auto *metatypeType = captureType->getAs<MetatypeType>())
          captureType = metatypeType->getInstanceType();

        if (auto *selfType = captureType->getAs<DynamicSelfType>()) {
          captureType = selfType->getSelfType();

          // We're capturing a 'self' value with dynamic 'Self' type;
          // handle it specially.
          if (captureType->getClassOrBoundGenericClass()) {
            if (selfCapture)
              selfCapture = selfCapture->mergeFlags(capture);
            else
              selfCapture = capture;
            continue;
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
  collectFunctionCaptures(fn);

  SmallVector<CapturedValue, 4> resultingCaptures;
  for (auto capturePair : captures) {
    resultingCaptures.push_back(capturePair.second);
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
  auto inserted = LoweredCaptures.insert({fn, CaptureInfo()});
  assert(inserted.second && "already in map?!");
  auto &cachedCaptures = inserted.first->second;
  cachedCaptures.setGenericParamCaptures(capturesGenericParams);
  cachedCaptures.setDynamicSelfType(capturesDynamicSelf);
  cachedCaptures.setCaptures(Context.AllocateCopy(resultingCaptures));
  
  return cachedCaptures;
}

/// Given that type1 is known to be a subtype of type2, check if the two
/// types have the same calling convention representation.
TypeConverter::ABIDifference
TypeConverter::checkForABIDifferences(SILType type1, SILType type2,
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
    return ABIDifference::Trivial;
  
  // Classes, class-constrained archetypes, and pure-ObjC existential types
  // all have single retainable pointer representation; optionality change
  // is allowed.
  if (type1.getASTType()->satisfiesClassConstraint() &&
      type2.getASTType()->satisfiesClassConstraint())
    return ABIDifference::Trivial;

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

      return checkFunctionForABIDifferences(fnTy1, fnTy2);
    }
  }
  
  // Metatypes are ABI-compatible if they have the same representation.
  if (auto meta1 = type1.getAs<MetatypeType>()) {
    if (auto meta2 = type2.getAs<MetatypeType>()) {
      if (meta1->getRepresentation() == meta2->getRepresentation() &&
          (!optionalityChange ||
           meta1->getRepresentation() == MetatypeRepresentation::Thick))
        return ABIDifference::Trivial;
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
        return ABIDifference::Trivial;
    }
  }

  // Tuple types are ABI-compatible if their elements are.
  if (!optionalityChange) {
    if (auto tuple1 = type1.getAs<TupleType>()) {
      if (auto tuple2 = type2.getAs<TupleType>()) {
        if (tuple1->getNumElements() != tuple2->getNumElements())
          return ABIDifference::NeedsThunk;
        
        for (unsigned i = 0, e = tuple1->getNumElements(); i < e; i++) {
          if (checkForABIDifferences(type1.getTupleElementType(i),
                                     type2.getTupleElementType(i))
                != ABIDifference::Trivial)
            return ABIDifference::NeedsThunk;
        }

        // Tuple lengths and elements match
        return ABIDifference::Trivial;
      }
    }
  }

  // The types are different, or there was an optionality change resulting
  // in a change in representation.
  return ABIDifference::NeedsThunk;
}

TypeConverter::ABIDifference
TypeConverter::checkFunctionForABIDifferences(SILFunctionType *fnTy1,
                                              SILFunctionType *fnTy2) {
  // Fast path -- if both functions were unwrapped from a CanSILFunctionType,
  // we might have pointer equality here.
  if (fnTy1 == fnTy2)
    return ABIDifference::Trivial;

  if (fnTy1->getParameters().size() != fnTy2->getParameters().size())
    return ABIDifference::NeedsThunk;

  if (fnTy1->getNumResults() != fnTy2->getNumResults())
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

    if (checkForABIDifferences(result1.getSILStorageType(),
                             result2.getSILStorageType(),
              /*thunk iuos*/ fnTy1->getLanguage() == SILFunctionLanguage::Swift)
        != ABIDifference::Trivial)
      return ABIDifference::NeedsThunk;
  }

  // If one type does not have an error result, we can still trivially cast
  // (casting away an error result is only safe if the function never throws,
  // of course).
  if (fnTy1->hasErrorResult() && fnTy2->hasErrorResult()) {
    auto error1 = fnTy1->getErrorResult(), error2 = fnTy2->getErrorResult();

    if (error1.getConvention() != error2.getConvention())
      return ABIDifference::NeedsThunk;

    if (checkForABIDifferences(error1.getSILStorageType(),
                               error2.getSILStorageType(),
              /*thunk iuos*/ fnTy1->getLanguage() == SILFunctionLanguage::Swift)
        != ABIDifference::Trivial)
      return ABIDifference::NeedsThunk;
  }

  for (unsigned i = 0, e = fnTy1->getParameters().size(); i < e; ++i) {
    auto param1 = fnTy1->getParameters()[i], param2 = fnTy2->getParameters()[i];
    
    if (param1.getConvention() != param2.getConvention())
      return ABIDifference::NeedsThunk;

    // Parameters are contravariant and our relation is not symmetric, so
    // make sure to flip the relation around.
    std::swap(param1, param2);

    if (checkForABIDifferences(param1.getSILStorageType(),
                               param2.getSILStorageType(),
              /*thunk iuos*/ fnTy1->getLanguage() == SILFunctionLanguage::Swift)
        != ABIDifference::Trivial)
      return ABIDifference::NeedsThunk;
  }

  auto rep1 = fnTy1->getRepresentation(), rep2 = fnTy2->getRepresentation();
  if (rep1 != rep2) {
    if (rep1 == SILFunctionTypeRepresentation::Thin &&
        rep2 == SILFunctionTypeRepresentation::Thick)
      return ABIDifference::ThinToThick;

    return ABIDifference::NeedsThunk;
  }

  return ABIDifference::Trivial;
}

CanSILBoxType
TypeConverter::getInterfaceBoxTypeForCapture(ValueDecl *captured,
                                             CanType loweredInterfaceType,
                                             bool isMutable) {
  auto &C = M.getASTContext();
  auto signature = getEffectiveGenericSignature(captured->getDeclContext());
  
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
  // FIXME: Map the box type out of context when asserting the field so
  // we don't need to push a GenericContextScope (which really ought to die).
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
  assert(contextBoxTy->getLayout()->getFields().size() == 1
         && contextBoxTy->getFieldType(M, 0).getASTType()
             == loweredContextType
         && "box field type doesn't match capture!");
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

CanSILBoxType TypeConverter::getBoxTypeForEnumElement(SILType enumType,
                                                      EnumElementDecl *elt) {

  auto *enumDecl = enumType.getEnumOrBoundGenericEnum();

  assert(elt->getDeclContext() == enumDecl);
  assert(elt->isIndirect() || elt->getParentEnum()->isIndirect());

  auto &C = M.getASTContext();

  auto boxSignature = getEffectiveGenericSignature(enumDecl);

  if (boxSignature == CanGenericSignature()) {
    auto eltIntfTy = elt->getArgumentInterfaceType();
    auto boxVarTy = getLoweredType(eltIntfTy).getASTType();
    auto layout = SILLayout::get(C, nullptr, SILField(boxVarTy, true));
    return SILBoxType::get(C, layout, {});
  }

  // Use the enum's signature for the box type.
  auto boundEnum = enumType.getASTType();

  // Lower the enum element's argument in the box's context.
  auto eltIntfTy = elt->getArgumentInterfaceType();
  GenericContextScope scope(*this, boxSignature);
  auto boxVarTy = getLoweredType(getAbstractionPattern(elt), eltIntfTy)
                      .getASTType();
  auto layout = SILLayout::get(C, boxSignature, SILField(boxVarTy, true));

  // Instantiate the layout with enum's substitution list.
  auto subMap = boundEnum->getContextSubstitutionMap(
      M.getSwiftModule(), enumDecl, enumDecl->getGenericEnvironment());

  auto boxTy = SILBoxType::get(C, layout, subMap);
  return boxTy;
}

static void countNumberOfInnerFields(unsigned &fieldsCount, SILModule &Module,
                                     SILType Ty) {
  if (auto *structDecl = Ty.getStructOrBoundGenericStruct()) {
    assert(!structDecl->isResilient(Module.getSwiftModule(),
                                    ResilienceExpansion::Minimal) &&
           " FSO should not be trying to explode resilient (ie address-only) "
           "types at all");
    for (auto *prop : structDecl->getStoredProperties()) {
      SILType propTy = Ty.getFieldType(prop, Module);
      unsigned fieldsCountBefore = fieldsCount;
      countNumberOfInnerFields(fieldsCount, Module, propTy);
      if (fieldsCount == fieldsCountBefore) {
        // size of Struct(BigStructType) == size of BigStructType()
        // prevent counting its size as BigStructType()+1
        ++fieldsCount;
      }
    }
    return;
  }
  if (auto tupleTy = Ty.getAs<TupleType>()) {
    for (auto elt : tupleTy->getElementTypes()) {
      auto silElt = SILType::getPrimitiveObjectType(elt->getCanonicalType());
      countNumberOfInnerFields(fieldsCount, Module, silElt);
    }
    return;
  }
  if (auto *enumDecl = Ty.getEnumOrBoundGenericEnum()) {
    if (enumDecl->isIndirect()) {
      return;
    }
    assert(!enumDecl->isResilient(Module.getSwiftModule(),
                                  ResilienceExpansion::Minimal) &&
           " FSO should not be trying to explode resilient (ie address-only) "
           "types at all");
    unsigned fieldsCountBefore = fieldsCount;
    unsigned maxEnumCount = 0;
    for (auto elt : enumDecl->getAllElements()) {
      if (!elt->getArgumentInterfaceType()) {
        continue;
      }
      if (elt->isIndirect()) {
        continue;
      }
      // Although one might assume enums have a fields count of 1
      // Which holds true for current uses of this code
      // (we shouldn't expand enums)
      // Number of fields > 1 as "future proof" for this heuristic:
      // In case it is used by a pass that tries to explode enums.
      auto payloadTy = Ty.getEnumElementType(elt, Module);
      fieldsCount = 0;
      countNumberOfInnerFields(fieldsCount, Module, payloadTy);
      if (fieldsCount > maxEnumCount) {
        maxEnumCount = fieldsCount;
      }
    }
    fieldsCount = fieldsCountBefore + maxEnumCount;
    return;
  }
}

unsigned TypeConverter::countNumberOfFields(SILType Ty) {
  auto Iter = TypeFields.find(Ty);
  if (Iter != TypeFields.end()) {
    return std::max(Iter->second, 1U);
  }
  unsigned fieldsCount = 0;
  countNumberOfInnerFields(fieldsCount, M, Ty);
  TypeFields[Ty] = fieldsCount;
  return std::max(fieldsCount, 1U);
}
