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
#include "swift/Basic/Fallthrough.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "clang/AST/Type.h"
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
  if (VarDecl *var = dyn_cast<VarDecl>(decl)) {
    switch (var->getStorageKind()) {
    case VarDecl::StoredWithTrivialAccessors:
      llvm_unreachable("stored local variable with trivial accessors?");

    case VarDecl::InheritedWithObservers:
      llvm_unreachable("inherited local variable?");

    case VarDecl::Computed:
      llvm_unreachable("computed captured property should have been lowered "
                       "away");

    case VarDecl::StoredWithObservers:
    case VarDecl::Addressed:
    case VarDecl::AddressedWithTrivialAccessors:
    case VarDecl::AddressedWithObservers:
    case VarDecl::ComputedWithMutableAddress:
      // Computed captures should have been lowered away.
      assert(capture.isDirect()
             && "computed captured property should have been lowered away");

      // If captured directly, the variable is captured by box or pointer.
      assert(var->hasStorage());
      return capture.isNoEscape() ?
        CaptureKind::StorageAddress : CaptureKind::Box;

    case VarDecl::Stored:
      // If this is a non-address-only stored 'let' constant, we can capture it
      // by value.  If it is address-only, then we can't load it, so capture it
      // by its address (like a var) instead.
      if (var->isLet() && !getTypeLowering(var->getType()).isAddressOnly())
        return CaptureKind::Constant;

      if (var->getType()->is<InOutType>()) {
        return CaptureKind::StorageAddress;
      }

      // If we're capturing into a non-escaping closure, we can generally just
      // capture the address of the value as no-escape.
      return capture.isNoEscape() ?
        CaptureKind::StorageAddress : CaptureKind::Box;
    }
    llvm_unreachable("bad storage kind");
  }
  
  // "Captured" local types require no context.
  if (isa<TypeAliasDecl>(decl) || isa<GenericTypeParamDecl>(decl) ||
      isa<AssociatedTypeDecl>(decl))
    return CaptureKind::None;
  
  llvm_unreachable("function-like captures should have been lowered away");
}

enum class LoweredTypeKind {
  /// Trivial and loadable.
  Trivial,

  /// A reference type.
  Reference,

  /// An aggregate type that contains references (potentially recursively).
  AggWithReference,

  /// Non-trivial and not loadable.
  AddressOnly,

  /// Trivial and not loadable.
  TrivialAddressOnly
};

static LoweredTypeKind classifyType(CanType type, SILModule &M,
                                    CanGenericSignature sig,
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
    //   RetTy handleAddressOnly(CanType);
    //   RetTy handleReference(CanType);
    //   RetTy handleTrivial(CanType);
    //   RetTy handleTrivialAddressOnly(CanType);
    // In addition, if it does not override visitTupleType
    // and visitAnyStructType, it should also implement:
    //   RetTy handleAggWithReference(CanType);

#define IMPL(TYPE, LOWERING)                            \
    RetTy visit##TYPE##Type(Can##TYPE##Type type) {     \
      return asImpl().handle##LOWERING(type);        \
    }

    IMPL(BuiltinInteger, Trivial)
    IMPL(BuiltinFloat, Trivial)
    IMPL(BuiltinRawPointer, Trivial)
    IMPL(BuiltinNativeObject, Reference)
    IMPL(BuiltinBridgeObject, Reference)
    IMPL(BuiltinUnknownObject, Reference)
    IMPL(BuiltinUnsafeValueBuffer, AddressOnly)
    IMPL(BuiltinVector, Trivial)
    IMPL(Class, Reference)
    IMPL(BoundGenericClass, Reference)
    IMPL(AnyMetatype, Trivial)
    IMPL(Module, Trivial)
    
    RetTy visitAnyFunctionType(CanAnyFunctionType type) {
      switch (type->getRepresentation()) {
      case AnyFunctionType::Representation::Swift:
      case AnyFunctionType::Representation::Block:
        return asImpl().handleReference(type);
      case AnyFunctionType::Representation::CFunctionPointer:
      case AnyFunctionType::Representation::Thin:
        return asImpl().handleTrivial(type);
      }
      llvm_unreachable("bad function representation");
    }
    
    RetTy visitSILFunctionType(CanSILFunctionType type) {
      if (type->getExtInfo().hasContext())
        return asImpl().handleReference(type);
      return asImpl().handleTrivial(type);
    }

#undef IMPL

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
        auto &mod = *M.getSwiftModule();
        if (genericSig->requiresClass(type, mod)) {
          return asImpl().handleReference(type);
        } else if (genericSig->isConcreteType(type, mod)) {
          return asImpl().visit(genericSig->getConcreteType(type, mod)
                                    ->getCanonicalType());
        } else {
          return asImpl().handleAddressOnly(type);
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

    RetTy visitUnmanagedStorageType(CanUnmanagedStorageType type) {
      return asImpl().handleTrivial(type);
    }

    bool hasNativeReferenceCounting(CanType type) {
      if (type->isTypeParameter()) {
        auto &mod = *M.getSwiftModule();
        auto signature = getGenericSignature();
        assert(signature && "dependent type without generic signature?!");

        if (auto concreteType = signature->getConcreteType(type, mod))
          return hasNativeReferenceCounting(concreteType->getCanonicalType());

        assert(signature->requiresClass(type, mod));

        // If we have a superclass bound, recurse on that.  This should
        // always terminate: even if we allow
        //   <T, U: T, V: U, ...>
        // at some point the type-checker should prove acyclic-ness.
        auto bound = signature->getSuperclassBound(type, mod);
        if (bound) {
          return hasNativeReferenceCounting(bound->getCanonicalType());
        }

        // Ask whether Builtin.UnknownObject uses native reference counting.
        auto &ctx = M.getASTContext();
        return ctx.TheUnknownObjectType->
                 usesNativeReferenceCounting(ResilienceExpansion::Maximal);
      }

      // FIXME: resilience
      return type->usesNativeReferenceCounting(ResilienceExpansion::Maximal);
    }

    RetTy visitUnownedStorageType(CanUnownedStorageType type) {
      // FIXME: avoid this duplication of the behavior of isLoadable.
      if (hasNativeReferenceCounting(type.getReferentType())) {
        return asImpl().visitLoadableUnownedStorageType(type);
      } else {
        return asImpl().visitAddressOnlyUnownedStorageType(type);
      }
    }

    RetTy visitLoadableUnownedStorageType(CanUnownedStorageType type) {
      return asImpl().handleReference(type);
    }

    RetTy visitAddressOnlyUnownedStorageType(CanUnownedStorageType type) {
      return asImpl().handleAddressOnly(type);
    }

    RetTy visitWeakStorageType(CanWeakStorageType type) {
      return asImpl().handleAddressOnly(type);
    }

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
          return asImpl().handleTrivialAddressOnly(type);
        }

        if (LayoutInfo->isRefCountedObject())
          return asImpl().handleReference(type);
      }
      return asImpl().handleAddressOnly(type);
    }

    RetTy visitExistentialType(CanType type) {
      switch (SILType::getPrimitiveObjectType(type)
                .getPreferredExistentialRepresentation(M)) {
      case ExistentialRepresentation::None:
        llvm_unreachable("not an existential type?!");
      // Opaque existentials are address-only.
      case ExistentialRepresentation::Opaque:
        return asImpl().handleAddressOnly(type);
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
      bool hasReference = false;
      for (auto eltType : type.getElementTypes()) {
        switch (classifyType(eltType, M, Sig, Expansion)) {
        case LoweredTypeKind::Trivial:
          continue;
        case LoweredTypeKind::TrivialAddressOnly:
          return asImpl().handleTrivialAddressOnly(type);
        case LoweredTypeKind::AddressOnly:
          return asImpl().handleAddressOnly(type);
        case LoweredTypeKind::Reference:
        case LoweredTypeKind::AggWithReference:
          hasReference = true;
          continue;
        }
        llvm_unreachable("bad type classification");
      }

      if (hasReference)
        return asImpl().handleAggWithReference(type);
      return asImpl().handleTrivial(type);
    }

    RetTy visitDynamicSelfType(CanDynamicSelfType type) {
      return this->visit(type.getSelfType());
    }
    
    RetTy visitSILBlockStorageType(CanSILBlockStorageType type) {
      // Should not be loaded.
      return asImpl().handleAddressOnly(type);
    }

    RetTy visitSILBoxType(CanSILBoxType type) {
      // Should not be loaded.
      return asImpl().handleReference(type);
    }
  };

  class TypeClassifier :
      public TypeClassifierBase<TypeClassifier, LoweredTypeKind> {
  public:
    TypeClassifier(SILModule &M, CanGenericSignature Sig,
                   ResilienceExpansion Expansion)
        : TypeClassifierBase(M, Sig, Expansion) {}

    LoweredTypeKind handleReference(CanType type) {
      return LoweredTypeKind::Reference;
    }
    LoweredTypeKind handleAggWithReference(CanType type) {
      return LoweredTypeKind::AggWithReference;
    }
    LoweredTypeKind
    handleTrivial(CanType type) {
      return LoweredTypeKind::Trivial;
    }

    LoweredTypeKind
    handleTrivialAddressOnly(CanType type) {
      return LoweredTypeKind::TrivialAddressOnly;
    }

    LoweredTypeKind handleAddressOnly(CanType type) {
      return LoweredTypeKind::AddressOnly;
    }

    LoweredTypeKind visitAnyEnumType(CanType type, EnumDecl *D) {
      // We have to look through optionals here without grabbing the
      // type lowering because the way that optionals are reabstracted
      // can trip recursion checks if we try to build a lowered type.
      if (D->classifyAsOptionalType()) {
        return visit(type.getAnyOptionalObjectType());
      }

      // Consult the type lowering.
      type = getSubstitutedTypeForTypeLowering(type);
      auto &lowering = M.Types.getTypeLowering(type);
      return handleClassificationFromLowering(type, lowering);
    }

    LoweredTypeKind visitAnyStructType(CanType type, StructDecl *D) {
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
          .getGenericEnvironment(*M.getSwiftModule())
          ->mapTypeIntoContext(type)
          ->getCanonicalType();
      }

      return type;
    }

    LoweredTypeKind handleClassificationFromLowering(CanType type,
                                           const TypeLowering &lowering) {
      if (lowering.isAddressOnly())
        return handleAddressOnly(type);
      if (lowering.isTrivial())
        return handleTrivial(type);
      return handleAggWithReference(type);
    }
  };
} // end anonymous namespace

static LoweredTypeKind classifyType(CanType type, SILModule &M,
                                    CanGenericSignature sig,
                                    ResilienceExpansion expansion) {
  return TypeClassifier(M, sig, expansion).visit(type);
}

/// True if the type, or the referenced type of an address
/// type, is address-only.  For example, it could be a resilient struct or
/// something of unknown size.
bool SILType::isAddressOnly(CanType type, SILModule &M,
                            CanGenericSignature sig,
                            ResilienceExpansion expansion) {
  return classifyType(type, M, sig, expansion)
      == LoweredTypeKind::AddressOnly;
}

namespace {
  /// A class for types that can be loaded and stored in SIL.
  /// This always include loadable types, but can include address-only types if
  /// opaque values are passed by value.
  class LoadableTypeLowering : public TypeLowering {
  protected:
    LoadableTypeLowering(SILType type, IsTrivial_t isTrivial,
                         IsAddressOnly_t isAddressOnly,
                         IsReferenceCounted_t isRefCounted)
      : TypeLowering(type, isTrivial, isAddressOnly, isRefCounted) {}

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

  /// A class for trivial, loadable types.
  class TrivialTypeLowering final : public LoadableTypeLowering {
  public:
    TrivialTypeLowering(SILType type)
      : LoadableTypeLowering(type, IsTrivial, IsNotAddressOnly,
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

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc, SILValue value,
                                 LoweringStyle loweringStyle) const override {
      // Trivial
    }

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  LoweringStyle style) const override {
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
                                   IsAddressOnly_t isAddressOnly,
                                   IsReferenceCounted_t isRefCounted)
      : LoadableTypeLowering(type, IsNotTrivial, isAddressOnly, isRefCounted) {}

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
                                       IsNotAddressOnly,
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
      B.createRetainValue(loc, value, Atomicity::Atomic);
      return value;
    }

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue aggValue,
                                  LoweringStyle style) const override {
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

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc,
                                 SILValue aggValue,
                                 LoweringStyle loweringStyle) const override {
      SimpleOperationTy Fn;

      switch(loweringStyle) {
      case LoweringStyle::Shallow:
        Fn = &TypeLowering::emitDestroyValue;
        break;
      case LoweringStyle::Deep:
        Fn = &TypeLowering::emitLoweredDestroyValueDeep;
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
                                       IsNotAddressOnly,
                                       IsNotReferenceCounted) {}

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      if (B.getFunction().hasQualifiedOwnership())
        return B.createCopyValue(loc, value);
      B.createRetainValue(loc, value, Atomicity::Atomic);
      return value;
    }

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  LoweringStyle style) const override {
      if (B.getFunction().hasQualifiedOwnership())
        return B.createCopyValue(loc, value);
      B.createRetainValue(loc, value, Atomicity::Atomic);
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
                                 LoweringStyle style) const override {
      assert(style != LoweringStyle::Shallow &&
             "This method should never be called when performing a shallow "
             "destroy value.");
      if (B.getFunction().hasQualifiedOwnership()) {
        B.createDestroyValue(loc, value);
        return;
      }
      B.emitReleaseValueAndFold(loc, value);
    }
  };

  class LeafLoadableTypeLowering : public NonTrivialLoadableTypeLowering {
  public:
    LeafLoadableTypeLowering(SILType type,
                             IsAddressOnly_t isAddressOnly,
                             IsReferenceCounted_t isRefCounted)
      : NonTrivialLoadableTypeLowering(type, isAddressOnly, isRefCounted) {}

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  LoweringStyle style) const override {
      return emitCopyValue(B, loc, value);
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc, SILValue value,
                                 LoweringStyle style) const override {
      emitDestroyValue(B, loc, value);
    }
  };

  /// A class for reference types, which are all non-trivial but still
  /// loadable.
  class ReferenceTypeLowering : public LeafLoadableTypeLowering {
  public:
    ReferenceTypeLowering(SILType type)
      : LeafLoadableTypeLowering(type, IsNotAddressOnly, IsReferenceCounted) {}

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      if (isa<FunctionRefInst>(value))
        return value;

      if (B.getFunction().hasQualifiedOwnership())
        return B.createCopyValue(loc, value);

      B.createStrongRetain(loc, value, Atomicity::Atomic);
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
  class LoadableUnownedTypeLowering final : public LeafLoadableTypeLowering {
  public:
    LoadableUnownedTypeLowering(SILType type)
      : LeafLoadableTypeLowering(type, IsNotAddressOnly, IsReferenceCounted) {}

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      if (B.getFunction().hasQualifiedOwnership())
        return B.createCopyValue(loc, value);

      B.createUnownedRetain(loc, value, Atomicity::Atomic);
      return value;
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      if (B.getFunction().hasQualifiedOwnership()) {
        B.createDestroyValue(loc, value);
        return;
      }
      B.createUnownedRelease(loc, value, Atomicity::Atomic);
    }
  };

  /// A class for non-trivial, address-only types.
  class AddressOnlyTypeLowering : public TypeLowering {
  public:
    AddressOnlyTypeLowering(SILType type)
      : TypeLowering(type, IsNotTrivial, IsAddressOnly, IsNotReferenceCounted)
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
      B.emitDestroyAddrAndFold(loc, addr);
    }

    void emitDestroyRValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      B.emitDestroyAddrAndFold(loc, value);
    }

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      llvm_unreachable("type is not loadable!");
    }

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  LoweringStyle style) const override {
      llvm_unreachable("type is not loadable!");
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      llvm_unreachable("type is not loadable!");
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc, SILValue value,
                                 LoweringStyle style) const override {
      llvm_unreachable("type is not loadable!");
    }
  };

  /// A class for Builtin.UnsafeValueBuffer.  The only purpose here is
  /// to catch obviously broken attempts to copy or destroy the buffer.
  class UnsafeValueBufferTypeLowering : public AddressOnlyTypeLowering {
  public:
    UnsafeValueBufferTypeLowering(SILType type)
      : AddressOnlyTypeLowering(type) {}

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
    OpaqueValueTypeLowering(SILType type)
      : LeafLoadableTypeLowering(type, IsAddressOnly, IsReferenceCounted) {}

    // --- Same as LoadableTypeLowering.
    void emitDestroyAddress(SILBuilder &B, SILLocation loc,
                            SILValue addr) const override {
      llvm_unreachable("destroy address");
    }

    void emitDestroyRValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      llvm_unreachable("destroy value");
    }

    void emitCopyInto(SILBuilder &B, SILLocation loc,
                      SILValue src, SILValue dest, IsTake_t isTake,
                      IsInitialization_t isInit) const override {
      llvm_unreachable("copy into");
    }

    // --- Same as NonTrivialLoadableTypeLowering

    SILValue emitLoadOfCopy(SILBuilder &B, SILLocation loc,
                            SILValue addr, IsTake_t isTake) const override {
      llvm_unreachable("load copy");
    }

    void emitStoreOfCopy(SILBuilder &B, SILLocation loc,
                         SILValue newValue, SILValue addr,
                         IsInitialization_t isInit) const override {
      llvm_unreachable("store copy");
    }

    void emitStore(SILBuilder &B, SILLocation loc, SILValue value,
                   SILValue addr, StoreOwnershipQualifier qual) const override {
      llvm_unreachable("store");
    }

    SILValue emitLoad(SILBuilder &B, SILLocation loc, SILValue addr,
                      LoadOwnershipQualifier qual) const override {
      llvm_unreachable("store");
    }

    // --- Same as LeafLoadableTypeLowering.

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  LoweringStyle style) const override {
      llvm_unreachable("lowered copy");
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc, SILValue value,
                                 LoweringStyle style) const override {
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

  /// A class for trivial, address-only types.
  class AddressOnlyTrivialTypeLowering : public TypeLowering {
  public:
    AddressOnlyTrivialTypeLowering(SILType type)
      : TypeLowering(type, IsTrivial, IsAddressOnly, IsNotReferenceCounted)
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
    }

    void emitDestroyRValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
    }

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      llvm_unreachable("type is not loadable!");
    }

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  LoweringStyle style) const override {
      llvm_unreachable("type is not loadable!");
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      llvm_unreachable("type is not loadable!");
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc, SILValue value,
                                 LoweringStyle style) const override {
      llvm_unreachable("type is not loadable!");
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

    const TypeLowering *
    handleTrivialAddressOnly(CanType type) {
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC, Dependent) AddressOnlyTrivialTypeLowering(silType);
    }

    const TypeLowering *handleReference(CanType type) {
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC, Dependent) ReferenceTypeLowering(silType);
    }

    const TypeLowering *handleAddressOnly(CanType type) {
      if (SILModuleConventions(M).useLoweredAddresses()) {
        auto silType = SILType::getPrimitiveAddressType(type);
        return new (TC, Dependent) AddressOnlyTypeLowering(silType);
      }
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC, Dependent) OpaqueValueTypeLowering(silType);
    }

    const TypeLowering *
    visitLoadableUnownedStorageType(CanUnownedStorageType type) {
      return new (TC, Dependent) LoadableUnownedTypeLowering(
                                  SILType::getPrimitiveObjectType(type));
    }

    const TypeLowering *
    visitBuiltinUnsafeValueBufferType(CanBuiltinUnsafeValueBufferType type) {
      auto silType = SILType::getPrimitiveAddressType(type);
      return new (TC, Dependent) UnsafeValueBufferTypeLowering(silType);
    }

    const TypeLowering *visitTupleType(CanTupleType tupleType) {
      bool hasOnlyTrivialChildren = true;

      for (auto eltType : tupleType.getElementTypes()) {
        auto &lowering = TC.getTypeLowering(eltType);
        if (lowering.isAddressOnly())
          return handleAddressOnly(tupleType);
        hasOnlyTrivialChildren &= lowering.isTrivial();
      }

      if (hasOnlyTrivialChildren)
        return handleTrivial(tupleType);

      return new (TC, Dependent) LoadableTupleTypeLowering(tupleType);
    }

    const TypeLowering *visitAnyStructType(CanType structType, StructDecl *D) {

      // For now, if the type does not have a fixed layout in all resilience
      // domains, we will treat it as address-only in SIL.
      if (!D->hasFixedLayout(M.getSwiftModule(), Expansion))
        return handleAddressOnly(structType);

      // Classify the type according to its stored properties.
      bool trivial = true;
      for (auto field : D->getStoredProperties()) {
        auto substFieldType =
          structType->getTypeOfMember(D->getModuleContext(), field, nullptr);
        switch (classifyType(substFieldType->getCanonicalType(),
                             M, Sig, Expansion)) {
        case LoweredTypeKind::TrivialAddressOnly:
        case LoweredTypeKind::AddressOnly:
          return handleAddressOnly(structType);
        case LoweredTypeKind::AggWithReference:
        case LoweredTypeKind::Reference:
          trivial = false;
          break;
        case LoweredTypeKind::Trivial:
          break;
        }
      }

      if (trivial)
        return handleTrivial(structType);
      return new (TC, Dependent) LoadableStructTypeLowering(structType);
    }
        
    const TypeLowering *visitAnyEnumType(CanType enumType, EnumDecl *D) {
      // For now, if the type does not have a fixed layout in all resilience
      // domains, we will treat it as address-only in SIL.
      if (!D->hasFixedLayout(M.getSwiftModule(), Expansion))
        return handleAddressOnly(enumType);

      // If the whole enum is indirect, we lower it as if all payload
      // cases were indirect. This means a fixed-layout indirect enum
      // is always loadable and nontrivial. A resilient indirect enum
      // is still address only, because we don't know how many bits
      // are used for the discriminator.
      if (D->isIndirect()) {
        return new (TC, Dependent) LoadableEnumTypeLowering(enumType);
      }

      // If any of the enum elements have address-only data, the enum is
      // address-only.
      bool trivial = true;
      for (auto elt : D->getAllElements()) {
        // No-payload elements do not affect address-only-ness.
        if (!elt->getArgumentInterfaceType())
          continue;

        // Indirect elements make the type nontrivial, but don't affect
        // address-only-ness.
        if (elt->isIndirect()) {
          trivial = false;
          continue;
        }
        
        auto substEltType = enumType->getTypeOfMember(
                              D->getModuleContext(), elt,
                              elt->getArgumentInterfaceType())
          ->getCanonicalType();
        
        switch (classifyType(substEltType, M, Sig, Expansion)) {
        case LoweredTypeKind::TrivialAddressOnly:
        case LoweredTypeKind::AddressOnly:
          return handleAddressOnly(enumType);
        case LoweredTypeKind::AggWithReference:
        case LoweredTypeKind::Reference:
          trivial = false;
          break;
        case LoweredTypeKind::Trivial:
          break;
        }
        
      }
      if (trivial)
        return handleTrivial(enumType);
      return new (TC, Dependent) LoadableEnumTypeLowering(enumType);
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
    CanType mappedType = ti.second->getLoweredType().getSwiftRValueType();
    if (srcType == mappedType || isa<InOutType>(srcType))
      ti.second->~TypeLowering();
  }
}

void *TypeLowering::operator new(size_t size, TypeConverter &tc,
                                 IsDependent_t dependent) {
  return dependent
    ? tc.DependentBPA.Allocate(size, alignof(TypeLowering&))
    : tc.IndependentBPA.Allocate(size, alignof(TypeLowering&));
}

const TypeLowering *TypeConverter::find(TypeKey k) {
  if (!k.isCacheable()) return nullptr;

  auto &Types = k.isDependent() ? DependentTypes : IndependentTypes;
  auto ck = k.getCachingKey();
  auto found = Types.find(ck);
  if (found == Types.end())
    return nullptr;

  assert(found->second && "type recursion not caught in Sema");
  return found->second;
}

void TypeConverter::insert(TypeKey k, const TypeLowering *tl) {
  if (!k.isCacheable()) return;

  auto &Types = k.isDependent() ? DependentTypes : IndependentTypes;

  Types[k.getCachingKey()] = tl;
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

    assert(!isa<LValueType>(substEltType) &&
           "lvalue types cannot exist in function signatures");

    CanType loweredSubstEltType;
    if (auto substLV = dyn_cast<InOutType>(substEltType)) {
      SILType silType = tc.getLoweredType(origType.getLValueObjectType(),
                                          substLV.getObjectType());
      loweredSubstEltType = CanInOutType::get(silType.getSwiftRValueType());

    } else {
      // If the original type was an archetype, use that archetype as
      // the original type of the element --- the actual archetype
      // doesn't matter, just the abstraction pattern.
      SILType silType = tc.getLoweredType(origEltType, substEltType);
      loweredSubstEltType = silType.getSwiftRValueType();
    }

    changed = (changed || substEltType != loweredSubstEltType);

    auto &substElt = substType->getElement(i);
    loweredElts.push_back(substElt.getWithType(loweredSubstEltType));
  }
  
  if (!changed) return substType;

  // Because we're transforming an existing tuple, the weird corner
  // case where TupleType::get does not return a TupleType can't happen.
  return cast<TupleType>(CanType(TupleType::get(loweredElts, tc.Context)));
}

static CanType getLoweredOptionalType(TypeConverter &tc,
                                      AbstractionPattern origType,
                                      CanType substType,
                                      CanType substObjectType,
                                      OptionalTypeKind optKind) {
  assert(substType.getAnyOptionalObjectType() == substObjectType);

  CanType loweredObjectType =
    tc.getLoweredType(origType.getAnyOptionalObjectType(), substObjectType)
      .getSwiftRValueType();

  // If the object type didn't change, we don't have to rebuild anything.
  if (loweredObjectType == substObjectType && optKind == OTK_Optional) {
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
      .getSwiftRValueType();

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
  
  assert((!key.isDependent() || CurGenericContext)
         && "dependent type outside of generic context?!");
  
  if (auto existing = find(key))
    return *existing;

  // inout types are a special case for lowering, because they get
  // completely removed and represented as 'address' SILTypes.
  if (isa<InOutType>(substType)) {
    // Derive SILType for InOutType from the object type.
    CanType substObjectType = substType.getLValueOrInOutObjectType();
    AbstractionPattern origObjectType = origType.getLValueObjectType();

    SILType loweredType = getLoweredType(origObjectType, substObjectType)
      .getAddressType();

    auto *theInfo = new (*this, key.isDependent())
      TrivialTypeLowering(loweredType);
    insert(key, theInfo);
    return *theInfo;
  }

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
    AbstractionPattern(CurGenericContext, loweredSubstType);
  auto loweredKey = getTypeKey(origTypeForCaching, loweredSubstType);

  auto &lowering = getTypeLoweringForLoweredType(loweredKey);
  insert(key, &lowering);
  return lowering;
}

CanSILFunctionType TypeConverter::getSILFunctionType(
    AbstractionPattern origType,
    CanFunctionType substFnType,
    unsigned uncurryLevel) {
  // First, lower at the AST level by uncurrying and substituting
  // bridged types.
  auto substLoweredType =
    getLoweredASTFunctionType(substFnType, 0, None);

  AbstractionPattern origLoweredType = [&] {
    if (origType.isExactType(substFnType)) {
      return AbstractionPattern(origType.getGenericSignature(),
                                substLoweredType);
    } else if (origType.isTypeParameter()) {
      return origType;
    } else {
      auto origFnType = cast<AnyFunctionType>(origType.getType());
      return AbstractionPattern(origType.getGenericSignature(),
                                getLoweredASTFunctionType(origFnType,
                                                          0, None));
    }
  }();

  return getNativeSILFunctionType(M, origLoweredType, substLoweredType);
}

CanType TypeConverter::getLoweredRValueType(AbstractionPattern origType,
                                            CanType substType) {
  // AST function types are turned into SIL function types:
  //   - the type is uncurried as desired
  //   - types are turned into their unbridged equivalents, depending
  //     on the abstract CC
  //   - ownership conventions are deduced
  if (auto substFnType = dyn_cast<FunctionType>(substType))
    return getSILFunctionType(origType, substFnType, /*uncurryLevel=*/0);

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
  OptionalTypeKind optKind;
  if (auto substObjectType = substType.getAnyOptionalObjectType(optKind)) {
    return getLoweredOptionalType(*this, origType, substType,
                                  substObjectType, optKind);
  }

  // The Swift type directly corresponds to the lowered type.
  return substType;
}

const TypeLowering &TypeConverter::getTypeLowering(SILType type) {
  auto loweredType = type.getSwiftRValueType();
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
  return CanFunctionType::get(TupleType::getEmpty(C), C.TheRawPointerType);
}

/// Get the type of a global variable getter function.
static CanAnyFunctionType getGlobalGetterType(CanType varType) {
  ASTContext &C = varType->getASTContext();
  return CanFunctionType::get(TupleType::getEmpty(C), varType);
}

/// Get the type of a default argument generator, () -> T.
static CanAnyFunctionType getDefaultArgGeneratorInterfaceType(
                                                     TypeConverter &TC,
                                                     AbstractFunctionDecl *AFD,
                                                     unsigned DefaultArgIndex,
                                                     ASTContext &context) {
  auto resultTy = AFD->getDefaultArg(DefaultArgIndex).second;
  assert(resultTy && "Didn't find default argument?");

  // The result type might be written in terms of type parameters
  // that have been made fully concrete.
  CanType canResultTy;
  if (auto *sig = AFD->getGenericSignature())
    canResultTy = sig->getCanonicalTypeInContext(resultTy,
                                                 *TC.M.getSwiftModule());
  else
    canResultTy = resultTy->getCanonicalType();

  // Get the generic signature from the surrounding context.
  auto funcInfo = TC.getConstantInfo(SILDeclRef(AFD));
  CanGenericSignature sig;
  if (auto genTy = funcInfo.FormalInterfaceType->getAs<GenericFunctionType>())
    sig = genTy->getGenericSignature()->getCanonicalSignature();

  if (sig) {
    return cast<GenericFunctionType>(
        GenericFunctionType::get(sig,
                                 TupleType::getEmpty(context),
                                 canResultTy,
                                 AnyFunctionType::ExtInfo())
            ->getCanonicalType());
  }
  
  return CanFunctionType::get(TupleType::getEmpty(context), canResultTy);
}

/// Get the type of a stored property initializer, () -> T.
static CanAnyFunctionType getStoredPropertyInitializerInterfaceType(
                                                     TypeConverter &TC,
                                                     VarDecl *VD,
                                                     ASTContext &context) {
  auto *DC = VD->getDeclContext();
  CanType resultTy =
      DC->mapTypeOutOfContext(VD->getParentInitializer()->getType())
          ->getCanonicalType();
  GenericSignature *sig = DC->getGenericSignatureOfContext();

  if (sig)
    return CanGenericFunctionType::get(sig->getCanonicalSignature(),
                                       TupleType::getEmpty(context),
                                       resultTy,
                                       GenericFunctionType::ExtInfo());
  
  return CanFunctionType::get(TupleType::getEmpty(context), resultTy);
}

/// Get the type of a destructor function.
static CanAnyFunctionType getDestructorInterfaceType(DestructorDecl *dd,
                                                     bool isDeallocating,
                                                     ASTContext &C,
                                                     bool isForeign) {
  auto classType = dd->getDeclContext()->getDeclaredInterfaceType()
                     ->getCanonicalType();

  assert((!isForeign || isDeallocating)
         && "There are no foreign destroying destructors");
  AnyFunctionType::ExtInfo extInfo =
            AnyFunctionType::ExtInfo(FunctionType::Representation::Thin,
                                     /*throws*/ false);
  if (isForeign)
    extInfo = extInfo
      .withSILRepresentation(SILFunctionTypeRepresentation::ObjCMethod);
  else
    extInfo = extInfo
      .withSILRepresentation(SILFunctionTypeRepresentation::Method);

  CanType resultTy = isDeallocating? TupleType::getEmpty(C)->getCanonicalType()
                                   : C.TheNativeObjectType;

  auto sig = dd->getDeclContext()->getGenericSignatureOfContext();
  if (sig)
    return cast<GenericFunctionType>(
      GenericFunctionType::get(sig, classType, resultTy, extInfo)
      ->getCanonicalType());
  return CanFunctionType::get(classType, resultTy, extInfo);
}

/// Retrieve the type of the ivar initializer or destroyer method for
/// a class.
static CanAnyFunctionType getIVarInitDestroyerInterfaceType(ClassDecl *cd,
                                                            bool isObjC,
                                                            ASTContext &ctx,
                                                            bool isDestroyer) {
  auto classType = cd->getDeclaredInterfaceType()->getCanonicalType();

  auto emptyTupleTy = TupleType::getEmpty(ctx)->getCanonicalType();
  CanType resultType = isDestroyer? emptyTupleTy : classType;
  auto extInfo = AnyFunctionType::ExtInfo(FunctionType::Representation::Thin,
                                          /*throws*/ false);
  extInfo = extInfo
    .withSILRepresentation(isObjC? SILFunctionTypeRepresentation::ObjCMethod
                           : SILFunctionTypeRepresentation::Method);

  resultType = CanFunctionType::get(emptyTupleTy, resultType, extInfo);
  auto sig = cd->getGenericSignatureOfContext();
  if (sig)
    return CanGenericFunctionType::get(sig->getCanonicalSignature(),
                                       classType, resultType,
                                       extInfo);
  return CanFunctionType::get(classType, resultType, extInfo);
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
TypeConverter::getEffectiveGenericSignature(AnyFunctionRef fn,
                                            CaptureInfo captureInfo) {
  auto dc = fn.getAsDeclContext();

  if (dc->getParent()->isLocalContext() &&
      !captureInfo.hasGenericParamCaptures())
    return nullptr;

  if (auto sig = dc->getGenericSignatureOfContext()) {
    if (sig->areAllParamsConcrete())
      return nullptr;
    return sig->getCanonicalSignature();
  }

  return nullptr;
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

  // If we don't have any local captures (including function captures),
  // there's no context to apply.
  if (!hasLoweredLocalCaptures(theClosure)) {
    if (!genericSig)
      return CanFunctionType::get(funcType.getInput(),
                                  funcType.getResult(),
                                  innerExtInfo);
    
    return CanGenericFunctionType::get(genericSig,
                                       funcType.getInput(),
                                       funcType.getResult(),
                                       innerExtInfo);
  }

  // Strip the generic signature off the inner type; we will add it to the
  // outer type with captures.
  funcType = CanFunctionType::get(funcType.getInput(),
                                  funcType.getResult(),
                                  innerExtInfo);

  // Add an extra empty tuple level to represent the captures. We'll append the
  // lowered capture types here.
  auto extInfo = AnyFunctionType::ExtInfo(FunctionType::Representation::Thin,
                                          /*throws*/ false);

  if (genericSig) {
    return CanGenericFunctionType::get(genericSig,
                                       Context.TheEmptyTupleType, funcType,
                                       extInfo);
  }
  
  return CanFunctionType::get(Context.TheEmptyTupleType, funcType, extInfo);
}

CanAnyFunctionType TypeConverter::makeConstantInterfaceType(SILDeclRef c) {
  ValueDecl *vd = c.loc.dyn_cast<ValueDecl *>();

  switch (c.kind) {
  case SILDeclRef::Kind::Func: {
    if (auto *ACE = c.loc.dyn_cast<AbstractClosureExpr *>()) {
      // FIXME: Closures could have an interface type computed by Sema.
      auto funcTy = cast<AnyFunctionType>(ACE->getType()->getCanonicalType());
      funcTy = cast<AnyFunctionType>(
          ACE->mapTypeOutOfContext(funcTy)
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
    return getDestructorInterfaceType(cast<DestructorDecl>(vd),
                             c.kind == SILDeclRef::Kind::Deallocator,
                             Context,
                             c.isForeign);
  
  case SILDeclRef::Kind::GlobalAccessor: {
    VarDecl *var = cast<VarDecl>(vd);
    assert(var->hasStorage() &&
           "constant ref to computed global var");
    return getGlobalAccessorType(var->getInterfaceType()->getCanonicalType());
  }
  case SILDeclRef::Kind::GlobalGetter: {
    VarDecl *var = cast<VarDecl>(vd);
    assert(var->hasStorage() &&
           "constant ref to computed global var");
    return getGlobalGetterType(var->getInterfaceType()->getCanonicalType());
  }
  case SILDeclRef::Kind::DefaultArgGenerator:
    return getDefaultArgGeneratorInterfaceType(*this,
                                               cast<AbstractFunctionDecl>(vd),
                                               c.defaultArgIndex, Context);
  case SILDeclRef::Kind::StoredPropertyInitializer:
    return getStoredPropertyInitializerInterfaceType(*this,
                                                     cast<VarDecl>(vd),
                                                     Context);
  case SILDeclRef::Kind::IVarInitializer:
    return getIVarInitDestroyerInterfaceType(cast<ClassDecl>(vd),
                                             c.isForeign, Context, false);
  case SILDeclRef::Kind::IVarDestroyer:
    return getIVarInitDestroyerInterfaceType(cast<ClassDecl>(vd),
                                             c.isForeign, Context, true);
  }

  llvm_unreachable("Unhandled SILDeclRefKind in switch.");
}

/// Get the generic environment for an entity.
GenericEnvironment *
TypeConverter::getConstantGenericEnvironment(SILDeclRef c) {
  ValueDecl *vd = c.loc.dyn_cast<ValueDecl *>();
  
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
  case SILDeclRef::Kind::GlobalGetter: {
    return vd->getDeclContext()->getGenericEnvironmentOfContext();
  }
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

  // Remove a layer of l-value type if present.
  if (auto substLVType = dyn_cast<LValueType>(substType))
    substType = substLVType.getObjectType();

  // Look through reference storage on the original type.
  auto origRefType = origType.getAs<ReferenceStorageType>();
  if (origRefType) {
    origType = origType.getReferenceStorageReferentType();
    substType = substType.getReferenceStorageReferent();
  }

  SILType silSubstType = getLoweredType(origType, substType).getAddressType();
  substType = silSubstType.getSwiftRValueType();

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
  
  // GenericFunctionTypes shouldn't nest.
  assert(DependentTypes.empty() && "already in generic context?!");
  assert(!CurGenericContext && "already in generic context!");

  CurGenericContext = sig;
}

void TypeConverter::popGenericContext(CanGenericSignature sig) {
  // If the generic signature is empty, this is a no-op.
  if (!sig)
    return;

  assert(CurGenericContext == sig && "unpaired push/pop");
  
  // Erase our cached TypeLowering objects and associated mappings for dependent
  // types.
  // Resetting the DependentBPA will deallocate but not run the destructor of
  // the dependent TypeLowerings.
  for (auto &ti : DependentTypes) {
    // Destroy only the unique entries.
    CanType srcType = ti.first.OrigType;
    if (!srcType) continue;
    CanType mappedType = ti.second->getLoweredType().getSwiftRValueType();
    if (srcType == mappedType || isa<LValueType>(srcType))
      ti.second->~TypeLowering();
  }
  DependentTypes.clear();
  DependentBPA.Reset();
  CurGenericContext = nullptr;
}

ProtocolDispatchStrategy
TypeConverter::getProtocolDispatchStrategy(ProtocolDecl *P) {
  // AnyObject has no requirements (other than the object being a class), so
  // needs no method dispatch.
  if (auto known = P->getKnownProtocolKind()) {
    if (*known == KnownProtocolKind::AnyObject)
      return ProtocolDispatchStrategy::Empty;
  }
  
  // Otherwise, ObjC protocols use ObjC method dispatch, and Swift protocols
  // use witness tables.
  
  if (P->isObjC())
    return ProtocolDispatchStrategy::ObjC;
  
  return ProtocolDispatchStrategy::Swift;
}

CanSILFunctionType TypeConverter::
getMaterializeForSetCallbackType(AbstractStorageDecl *storage,
                                 CanGenericSignature genericSig,
                                 Type selfType) {
  auto &ctx = M.getASTContext();

  // Get lowered formal types for callback parameters.
  auto selfMetatypeType = MetatypeType::get(selfType,
                                            MetatypeRepresentation::Thick);

  {
    GenericContextScope scope(*this, genericSig);

    // If 'self' is a metatype, make it @thin or @thick as needed, but not inside
    // selfMetatypeType.
    if (auto metatype = selfType->getAs<MetatypeType>()) {
      if (!metatype->hasRepresentation())
        selfType = getLoweredType(metatype).getSwiftRValueType();
    }
  }

  CanType canSelfType;
  CanType canSelfMetatypeType;
  if (genericSig) {
    canSelfType = genericSig->getCanonicalTypeInContext(
        selfType, *M.getSwiftModule());
    canSelfMetatypeType = genericSig->getCanonicalTypeInContext(
        selfMetatypeType, *M.getSwiftModule());
  } else {
    canSelfType = selfType->getCanonicalType();
    canSelfMetatypeType = selfMetatypeType->getCanonicalType();
  }

  // Create the SILFunctionType for the callback.
  SILParameterInfo params[] = {
    { ctx.TheRawPointerType, ParameterConvention::Direct_Unowned },
    { ctx.TheUnsafeValueBufferType, ParameterConvention::Indirect_Inout },
    { canSelfType, ParameterConvention::Indirect_Inout },
    { canSelfMetatypeType, ParameterConvention::Direct_Unowned },
  };
  ArrayRef<SILResultInfo> results = {};
  auto extInfo = 
    SILFunctionType::ExtInfo()
      .withRepresentation(SILFunctionTypeRepresentation::Thin);

  if (genericSig && genericSig->areAllParamsConcrete())
    genericSig = nullptr;

  return SILFunctionType::get(genericSig, extInfo,
                   /*callee*/ ParameterConvention::Direct_Unowned,
                              params, results, None, ctx);
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
  llvm::SetVector<CapturedValue> captures;

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
        switch (capturedVar->getStorageKind()) {
        case VarDecl::StoredWithTrivialAccessors:
          llvm_unreachable("stored local variable with trivial accessors?");

        case VarDecl::InheritedWithObservers:
          llvm_unreachable("inherited local variable?");

        case VarDecl::StoredWithObservers:
        case VarDecl::Addressed:
        case VarDecl::AddressedWithTrivialAccessors:
        case VarDecl::AddressedWithObservers:
        case VarDecl::ComputedWithMutableAddress:
          // Directly capture storage if we're supposed to.
          if (capture.isDirect())
            goto capture_value;

          // Otherwise, transitively capture the accessors.
          SWIFT_FALLTHROUGH;

        case VarDecl::Computed: {
          collectFunctionCaptures(capturedVar->getGetter());
          if (auto setter = capturedVar->getSetter())
            collectFunctionCaptures(setter);
          continue;
        }

        case VarDecl::Stored: {
          // We can always capture the storage in these cases.
          Type captureType = capturedVar->getType();
          if (auto *metatypeType = captureType->getAs<MetatypeType>())
            captureType = metatypeType->getInstanceType();

          if (auto *selfType = captureType->getAs<DynamicSelfType>()) {
            captureType = selfType->getSelfType();

            // We're capturing a 'self' value with dynamic 'Self' type;
            // handle it specially.
            if (!selfCapture &&
                captureType->getClassOrBoundGenericClass()) {
              selfCapture = capture;
              continue;
            }
          }

          // Otherwise just fall through.
          goto capture_value;
        }
        }
      }
      
    capture_value:
      // Collect non-function captures.
      captures.insert(capture);
    }
  };
  collectFunctionCaptures(fn);

  // If we captured the dynamic 'Self' type and we have a 'self' value also,
  // add it as the final capture. Otherwise, add a fake hidden capture for
  // the dynamic 'Self' metatype.
  if (selfCapture.hasValue()) {
    captures.insert(*selfCapture);
  } else if (capturesDynamicSelf) {
    selfCapture = CapturedValue::getDynamicSelfMetadata();
    captures.insert(*selfCapture);
  }

  // Cache the uniqued set of transitive captures.
  auto inserted = LoweredCaptures.insert({fn, CaptureInfo()});
  assert(inserted.second && "already in map?!");
  auto &cachedCaptures = inserted.first->second;
  cachedCaptures.setGenericParamCaptures(capturesGenericParams);
  cachedCaptures.setDynamicSelfType(capturesDynamicSelf);
  cachedCaptures.setCaptures(Context.AllocateCopy(captures));
  
  return cachedCaptures;
}

/// Given that type1 is known to be a subtype of type2, check if the two
/// types have the same calling convention representation.
TypeConverter::ABIDifference
TypeConverter::checkForABIDifferences(SILType type1, SILType type2) {
  // Unwrap optionals, but remember that we did.
  bool type1WasOptional = false;
  bool type2WasOptional = false;
  if (auto object = type1.getAnyOptionalObjectType()) {
    type1WasOptional = true;
    type1 = object;
  }
  if (auto object = type2.getAnyOptionalObjectType()) {
    type2WasOptional = true;
    type2 = object;
  }

  // Forcing IUOs always requires a thunk.
  if (type1WasOptional && !type2WasOptional)
    return ABIDifference::NeedsThunk;
  
  // Except for the above case, we should not be making a value less optional.
  
  // If we're introducing a level of optionality, only certain types are
  // ABI-compatible -- check below.
  bool optionalityChange = (!type1WasOptional && type2WasOptional);

  // If the types are identical and there was no optionality change,
  // we're done.
  if (type1 == type2 && !optionalityChange)
    return ABIDifference::Trivial;
  
  // Classes, class-constrained archetypes, and pure-ObjC existential types
  // all have single retainable pointer representation; optionality change
  // is allowed.
  if ((type1.getSwiftRValueType()->mayHaveSuperclass() ||
       type1.getSwiftRValueType()->isObjCExistentialType()) &&
      (type2.getSwiftRValueType()->mayHaveSuperclass() ||
       type2.getSwiftRValueType()->isObjCExistentialType()))
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
                               result2.getSILStorageType())
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
                               error2.getSILStorageType())
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
                               param2.getSILStorageType())
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

void canonicalizeSubstitutions(MutableArrayRef<Substitution> subs) {
  for (auto &sub : subs) {
    sub = Substitution(sub.getReplacement()->getCanonicalType(),
                       sub.getConformances());
  }
}

CanSILBoxType
TypeConverter::getInterfaceBoxTypeForCapture(ValueDecl *captured,
                                             CanType loweredInterfaceType,
                                             bool isMutable) {
  auto &C = M.getASTContext();
  CanGenericSignature signature;
  if (auto sig = captured->getDeclContext()->getGenericSignatureOfContext())
    signature = sig->getCanonicalSignature();
  
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
  SmallVector<Substitution, 4> genericArgs;
  signature->getSubstitutions(
    [&](SubstitutableType* type) -> Type {
      return signature->getCanonicalTypeInContext(type,
                                                  *M.getSwiftModule());
    },
    [](Type depTy, Type replacementTy, ProtocolType *conformedTy)
    -> ProtocolConformanceRef {
      return ProtocolConformanceRef(conformedTy->getDecl());
    },
    genericArgs);
  canonicalizeSubstitutions(genericArgs);
  
  auto boxTy = SILBoxType::get(C, layout, genericArgs);
#ifndef NDEBUG
  // FIXME: Map the box type out of context when asserting the field so
  // we don't need to push a GenericContextScope (which really ought to die).
  auto loweredContextType = loweredInterfaceType;
  auto contextBoxTy = boxTy;
  if (signature) {
    auto env = signature.getGenericEnvironment(*M.getSwiftModule());
    loweredContextType = env->mapTypeIntoContext(loweredContextType)
                            ->getCanonicalType();
    contextBoxTy = cast<SILBoxType>(
      env->mapTypeIntoContext(contextBoxTy)
         ->getCanonicalType());
  }
  assert(contextBoxTy->getLayout()->getFields().size() == 1
         && contextBoxTy->getFieldType(M, 0).getSwiftRValueType()
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
    if (auto homeSig = captured->getDeclContext()
                               ->getGenericSignatureOfContext()) {
      loweredInterfaceType = homeSig->getCanonicalTypeInContext(
        env->mapTypeOutOfContext(loweredInterfaceType),
        *M.getSwiftModule());
    }
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
