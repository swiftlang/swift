//===--- TypeLowering.cpp - Type information for SILGen -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "libsil"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
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
}

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
  AddressOnly
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
      } else {
        return asImpl().handleAddressOnly(type);
      }
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
    RetTy visitAnyEnumType(CanType type, EnumDecl *D) {
      // If we're using a generic signature different from
      // M.Types.getCurGenericContext(), we have to map the
      // type into context first because the rest of type
      // lowering doesn't have a generic signature plumbed
      // through.
      if (Sig && type->hasTypeParameter()) {
        auto builder = M.getASTContext().getOrCreateArchetypeBuilder(
            Sig, M.getSwiftModule());
        type = builder->substDependentType(type)->getCanonicalType();
      }

      // Consult the type lowering.
      auto &lowering = M.Types.getTypeLowering(type);
      return handleClassificationFromLowering(type, lowering);
    }
    
    RetTy handleClassificationFromLowering(CanType type,
                                           const TypeLowering &lowering) {
      if (lowering.isAddressOnly())
        return asImpl().handleAddressOnly(type);
      if (lowering.isTrivial())
        return asImpl().handleTrivial(type);
      return asImpl().handleAggWithReference(type);
    }

    // Structs depend on their physical fields.
    RetTy visitStructType(CanStructType type) {
      return asImpl().visitAnyStructType(type, type->getDecl());
    }
    RetTy visitBoundGenericStructType(CanBoundGenericStructType type) {
      return asImpl().visitAnyStructType(type, type->getDecl());
    }

    RetTy visitAnyStructType(CanType type, StructDecl *D) {
      // If we're using a generic signature different from
      // M.Types.getCurGenericContext(), we have to map the
      // type into context first because the rest of type
      // lowering doesn't have a generic signature plumbed
      // through.
      if (Sig && type->hasTypeParameter()) {
        auto builder = M.getASTContext().getOrCreateArchetypeBuilder(
            Sig, M.getSwiftModule());
        type = builder->substDependentType(type)->getCanonicalType();
      }

      // Consult the type lowering.  This means we implicitly get
      // caching, but that type lowering needs to override this case.
      auto &lowering = M.Types.getTypeLowering(type);
      return handleClassificationFromLowering(type, lowering);
    }
    
    // Tuples depend on their elements.
    RetTy visitTupleType(CanTupleType type) {
      bool hasReference = false;
      for (auto eltType : type.getElementTypes()) {
        switch (classifyType(eltType, M, Sig, Expansion)) {
        case LoweredTypeKind::Trivial:
          continue;
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
    LoweredTypeKind handleTrivial(CanType type) {
      return LoweredTypeKind::Trivial;
    }
    LoweredTypeKind handleAddressOnly(CanType type) {
      return LoweredTypeKind::AddressOnly;
    }
  };
}

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
  /// A class for loadable types.
  class LoadableTypeLowering : public TypeLowering {
  protected:
    LoadableTypeLowering(SILType type, IsTrivial_t isTrivial,
                         IsReferenceCounted_t isRefCounted)
      : TypeLowering(type, isTrivial, IsNotAddressOnly, isRefCounted) {}

  public:
    void emitDestroyAddress(SILBuilder &B, SILLocation loc,
                            SILValue addr) const override {
      SILValue value = B.createLoad(loc, addr);
      emitReleaseValue(B, loc, value);
    }

    void emitDestroyRValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      emitReleaseValue(B, loc, value);
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
      : LoadableTypeLowering(type, IsTrivial, IsNotReferenceCounted) {}

    SILValue emitLoadOfCopy(SILBuilder &B, SILLocation loc, SILValue addr,
                            IsTake_t isTake) const override {
      return B.createLoad(loc, addr);
    }

    void emitStoreOfCopy(SILBuilder &B, SILLocation loc,
                         SILValue value, SILValue addr,
                         IsInitialization_t isInit) const override {
      B.createStore(loc, value, addr);
    }

    void emitDestroyAddress(SILBuilder &B, SILLocation loc,
                            SILValue addr) const override {
      // Trivial
    }

    void emitLoweredReleaseValue(SILBuilder &B, SILLocation loc,
                                 SILValue value,
                                 LoweringStyle loweringStyle) const override {
      // Trivial
    }

    void emitLoweredRetainValue(SILBuilder &B, SILLocation loc,
                              SILValue value,
                              LoweringStyle style) const override {
      // Trivial
    }

    void emitRetainValue(SILBuilder &B, SILLocation loc,
                       SILValue value) const override {
      // Trivial
    }

    void emitReleaseValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      // Trivial
    }
  };

  class NonTrivialLoadableTypeLowering : public LoadableTypeLowering {
  public:
    NonTrivialLoadableTypeLowering(SILType type,
                                   IsReferenceCounted_t isRefCounted)
      : LoadableTypeLowering(type, IsNotTrivial, isRefCounted) {}

    SILValue emitLoadOfCopy(SILBuilder &B, SILLocation loc,
                            SILValue addr, IsTake_t isTake) const override {
      SILValue value = B.createLoad(loc, addr);
      if (!isTake) emitRetainValue(B, loc, value);
      return value;
    }

    void emitStoreOfCopy(SILBuilder &B, SILLocation loc,
                         SILValue newValue, SILValue addr,
                         IsInitialization_t isInit) const override {
      SILValue oldValue;
      if (!isInit) oldValue = B.createLoad(loc, addr);
      B.createStore(loc, newValue, addr);
      if (!isInit) emitReleaseValue(B, loc, oldValue);
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

    void emitRetainValue(SILBuilder &B, SILLocation loc,
                       SILValue aggValue) const override {
      B.createRetainValue(loc, aggValue, Atomicity::Atomic);
    }

    void emitLoweredRetainValue(SILBuilder &B, SILLocation loc,
                              SILValue aggValue,
                              LoweringStyle style) const override {
      for (auto &child : getChildren(B.getModule())) {
        auto &childLowering = child.getLowering();
        SILValue childValue = asImpl().emitRValueProject(B, loc, aggValue,
                                                         child.getIndex(),
                                                         childLowering);
        if (!childLowering.isTrivial()) {
          childLowering.emitLoweredCopyChildValue(B, loc, childValue, style);
        }
      }
    }

    void emitReleaseValue(SILBuilder &B, SILLocation loc,
                          SILValue aggValue) const override {
      B.emitReleaseValueAndFold(loc, aggValue);
    }

    void emitLoweredReleaseValue(SILBuilder &B, SILLocation loc,
                                 SILValue aggValue,
                                 LoweringStyle loweringStyle) const override {
      SimpleOperationTy Fn;

      switch(loweringStyle) {
      case LoweringStyle::Shallow:
        Fn = &TypeLowering::emitReleaseValue;
        break;
      case LoweringStyle::Deep:
        Fn = &TypeLowering::emitLoweredReleaseValueDeep;
        break;
      case LoweringStyle::DeepNoEnum:
        Fn = &TypeLowering::emitLoweredReleaseValueDeepNoEnum;
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
                              ArrayRef<SILValue> values) const {
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
                              ArrayRef<SILValue> values) const {
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
    /// A non-trivial case of the enum.
    class NonTrivialElement {
      /// The non-trivial element.
      EnumElementDecl *element;
      
      /// Its type lowering.
      const TypeLowering *lowering;
      
    public:
      NonTrivialElement(EnumElementDecl *element, const TypeLowering &lowering)
        : element(element), lowering(&lowering) {}
      
      const TypeLowering &getLowering() const { return *lowering; }
      EnumElementDecl *getElement() const { return element; }
    };
    
  private:

    using SimpleOperationTy = void (*)(SILBuilder &B, SILLocation loc, SILValue value,
                                       const TypeLowering &valueLowering,
                                       SILBasicBlock *dest);

    /// Emit a value semantics operation for each nontrivial case of the enum.
    template <typename T>
    void ifNonTrivialElement(SILBuilder &B, SILLocation loc, SILValue value,
                             const T &operation) const {
      SmallVector<std::pair<EnumElementDecl*,SILBasicBlock*>, 4> nonTrivialBBs;
      
      auto &M = B.getFunction().getModule();

      // Create all the blocks up front, so we can set up our switch_enum.
      auto nonTrivialElts = getNonTrivialElements(M);
      for (auto &elt : nonTrivialElts) {
        auto bb = new (M) SILBasicBlock(&B.getFunction());
        auto argTy = elt.getLowering().getLoweredType();
        new (M) SILArgument(bb, argTy);
        nonTrivialBBs.push_back({elt.getElement(), bb});
      }

      // If we are appending to the end of a block being constructed, then we
      // create a new basic block to continue cons'ing up code.  If we're
      // emitting this operation into the middle of existing code, we split the
      // block.
      SILBasicBlock *doneBB = B.splitBlockForFallthrough();
      B.createSwitchEnum(loc, value, doneBB, nonTrivialBBs);
      
      for (size_t i = 0; i < nonTrivialBBs.size(); ++i) {
        SILBasicBlock *bb = nonTrivialBBs[i].second;
        const TypeLowering &lowering = nonTrivialElts[i].getLowering();
        B.emitBlock(bb);
        operation(B, loc, bb->getBBArgs()[0], lowering, doneBB);
      }
      
      B.emitBlock(doneBB);
    }
    
    /// A reference to the lazily-allocated array of non-trivial enum cases.
    mutable ArrayRef<NonTrivialElement> NonTrivialElements = {};
    
  public:
    LoadableEnumTypeLowering(CanType type)
      : NonTrivialLoadableTypeLowering(SILType::getPrimitiveObjectType(type),
                                       IsNotReferenceCounted)
    {
    }
    
    ArrayRef<NonTrivialElement> getNonTrivialElements(SILModule &M) const {
      SILType silTy = getLoweredType();
      EnumDecl *enumDecl = silTy.getEnumOrBoundGenericEnum();
      assert(enumDecl);
      
      if (NonTrivialElements.data() == nullptr) {
        SmallVector<NonTrivialElement, 4> elts;
        
        for (auto elt : enumDecl->getAllElements()) {
          if (!elt->hasArgumentType()) continue;
          SILType substTy = silTy.getEnumElementType(elt, M);
          elts.push_back(NonTrivialElement{elt,
                                     M.Types.getTypeLowering(substTy)});
        }
        
        auto isDependent = IsDependent_t(silTy.hasTypeParameter());
        
        auto buf = operator new(sizeof(NonTrivialElement) * elts.size(),
                                M.Types, isDependent);
        memcpy(buf, elts.data(), sizeof(NonTrivialElement) * elts.size());
        NonTrivialElements = {reinterpret_cast<NonTrivialElement*>(buf),
                              elts.size()};
      }
      return NonTrivialElements;
    }

    void emitRetainValue(SILBuilder &B, SILLocation loc,
                       SILValue value) const override {
      B.createRetainValue(loc, value, Atomicity::Atomic);
    }

    void emitLoweredRetainValue(SILBuilder &B, SILLocation loc,
                              SILValue value,
                              LoweringStyle style) const override {
      if (style == LoweringStyle::Shallow ||
          style == LoweringStyle::DeepNoEnum) {
        B.createRetainValue(loc, value, Atomicity::Atomic);
      } else {
        ifNonTrivialElement(B, loc, value,
          [&](SILBuilder &B, SILLocation loc, SILValue child,
              const TypeLowering &childLowering, SILBasicBlock *dest) {
            childLowering.emitLoweredCopyChildValue(B, loc, child, style);
            B.createBranch(loc, dest);
          });
      }
    }
    
    void emitReleaseValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      B.emitReleaseValueAndFold(loc, value);
    }

    void emitLoweredReleaseValue(SILBuilder &B, SILLocation loc,
                                 SILValue value,
                                 LoweringStyle style) const override {
      assert(style != LoweringStyle::Shallow &&
             "This method should never be called when performing a shallow "
             "destroy value.");
      if (style == LoweringStyle::DeepNoEnum)
        B.emitReleaseValueAndFold(loc, value);
      else
        ifNonTrivialElement(B, loc, value,
          [&](SILBuilder &B, SILLocation loc, SILValue child,
             const TypeLowering &childLowering, SILBasicBlock *dest) {
             childLowering.emitLoweredDestroyChildValue(B, loc, child, style);
             B.createBranch(loc, dest);
          });
    }
  };

  class LeafLoadableTypeLowering : public NonTrivialLoadableTypeLowering {
  public:
    LeafLoadableTypeLowering(SILType type, IsReferenceCounted_t isRefCounted)
      : NonTrivialLoadableTypeLowering(type, isRefCounted) {}

    void emitLoweredRetainValue(SILBuilder &B, SILLocation loc,
                              SILValue value,
                              LoweringStyle style) const override {
      emitRetainValue(B, loc, value);
    }

    void emitLoweredReleaseValue(SILBuilder &B, SILLocation loc,
                                 SILValue value,
                                 LoweringStyle style) const override {
      emitReleaseValue(B, loc, value);
    }
  };

  /// A class for reference types, which are all non-trivial but still
  /// loadable.
  class ReferenceTypeLowering : public LeafLoadableTypeLowering {
  public:
    ReferenceTypeLowering(SILType type)
      : LeafLoadableTypeLowering(type, IsReferenceCounted) {}

    void emitRetainValue(SILBuilder &B, SILLocation loc,
                       SILValue value) const override {
      if (!isa<FunctionRefInst>(value))
        B.createStrongRetain(loc, value, Atomicity::Atomic);
    }

    void emitReleaseValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      B.emitStrongReleaseAndFold(loc, value);
    }
  };

  /// A type lowering for loadable @unowned types.
  class LoadableUnownedTypeLowering final : public LeafLoadableTypeLowering {
  public:
    LoadableUnownedTypeLowering(SILType type)
      : LeafLoadableTypeLowering(type, IsReferenceCounted) {}

    void emitRetainValue(SILBuilder &B, SILLocation loc,
                       SILValue value) const override {
      B.createUnownedRetain(loc, value, Atomicity::Atomic);
    }

    void emitReleaseValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
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

    void emitDestroyAddress(SILBuilder &B, SILLocation loc,
                            SILValue addr) const override {
      B.emitDestroyAddrAndFold(loc, addr);
    }

    void emitDestroyRValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      B.emitDestroyAddrAndFold(loc, value);
    }

    void emitRetainValue(SILBuilder &B, SILLocation loc,
                       SILValue value) const override {
      llvm_unreachable("type is not loadable!");
    }

    void emitLoweredRetainValue(SILBuilder &B, SILLocation loc,
                              SILValue value,
                              LoweringStyle style) const override {
      llvm_unreachable("type is not loadable!");
    }

    void emitReleaseValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      llvm_unreachable("type is not loadable!");
    }

    void emitLoweredReleaseValue(SILBuilder &B, SILLocation loc,
                                 SILValue value,
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

  /// Build the appropriate TypeLowering subclass for the given type.
  class LowerType
    : public TypeClassifierBase<LowerType, const TypeLowering *>
  {
    TypeConverter &TC;
    CanType OrigType;
    IsDependent_t Dependent;
  public:
    LowerType(TypeConverter &TC, CanType OrigType,
              CanGenericSignature Sig, ResilienceExpansion Expansion,
              IsDependent_t Dependent)
      : TypeClassifierBase(TC.M, Sig, Expansion),
        TC(TC), OrigType(OrigType),
        Dependent(Dependent) {}
        
    const TypeLowering *handleTrivial(CanType type) {
      auto silType = SILType::getPrimitiveObjectType(OrigType);
      return new (TC, Dependent) TrivialTypeLowering(silType);
    }
  
    const TypeLowering *handleReference(CanType type) {
      auto silType = SILType::getPrimitiveObjectType(OrigType);
      return new (TC, Dependent) ReferenceTypeLowering(silType);
    }

    const TypeLowering *handleAddressOnly(CanType type) {
      auto silType = SILType::getPrimitiveAddressType(OrigType);
      return new (TC, Dependent) AddressOnlyTypeLowering(silType);
    }

    /// @unowned is basically like a reference type lowering except
    /// it manipulates unowned reference counts instead of strong.
    const TypeLowering *visitUnownedStorageType(CanUnownedStorageType type) {
      // Lower 'Self' as if it were the base type.
      if (auto dynamicSelfType
            = dyn_cast<DynamicSelfType>(type.getReferentType())) {
        auto unownedBaseType = CanUnownedStorageType::get(
                                                dynamicSelfType.getSelfType());

        return LowerType(TC, unownedBaseType, Sig, Expansion, Dependent)
          .visit(unownedBaseType);
      }

      return this->TypeClassifierBase::visitUnownedStorageType(type);
    }

    const TypeLowering *
    visitLoadableUnownedStorageType(CanUnownedStorageType type) {
      return new (TC, Dependent) LoadableUnownedTypeLowering(
                                     SILType::getPrimitiveObjectType(OrigType));      
    }

    const TypeLowering *visitUnmanagedStorageType(
                                          CanUnmanagedStorageType type) {
      if (auto dynamicSelfType
            = dyn_cast<DynamicSelfType>(type.getReferentType())) {
        auto unmanagedBaseType = CanUnmanagedStorageType::get(
                                                dynamicSelfType.getSelfType());

        return LowerType(TC, unmanagedBaseType, Sig, Expansion, Dependent)
          .visit(unmanagedBaseType);
      }

      return this->TypeClassifierBase::visitUnmanagedStorageType(type);
    }

    const TypeLowering *visitWeakStorageType(CanWeakStorageType type) {
      OptionalTypeKind OTK;
      auto objectType = type.getReferentType().getAnyOptionalObjectType(OTK);
      if (auto dynamicSelfType = dyn_cast<DynamicSelfType>(objectType)) {
        auto optBaseType = OptionalType::get(OTK, dynamicSelfType.getSelfType())
          ->getCanonicalType();
        auto weakBaseType = CanWeakStorageType::get(optBaseType);

        return LowerType(TC, weakBaseType, Sig, Expansion, Dependent)
          .visit(weakBaseType);
      }

      return this->TypeClassifierBase::visitWeakStorageType(type);
    }

    const TypeLowering *
    visitBuiltinUnsafeValueBufferType(CanBuiltinUnsafeValueBufferType type) {
      auto silType = SILType::getPrimitiveAddressType(OrigType);
      return new (TC, Dependent) UnsafeValueBufferTypeLowering(silType);
    }

    const TypeLowering *visitTupleType(CanTupleType tupleType) {
      typedef LoadableTupleTypeLowering::Child Child;
      SmallVector<Child, 8> childElts;
      bool hasOnlyTrivialChildren = true;

      unsigned i = 0;
      for (auto eltType : tupleType.getElementTypes()) {
        auto &lowering = TC.getTypeLowering(eltType);
        if (lowering.isAddressOnly())
          return handleAddressOnly(tupleType);
        hasOnlyTrivialChildren &= lowering.isTrivial();
        childElts.push_back(Child(i, lowering));
        ++i;
      }

      if (hasOnlyTrivialChildren)
        return handleTrivial(tupleType);

      return new (TC, Dependent) LoadableTupleTypeLowering(OrigType);
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
      return new (TC, Dependent) LoadableStructTypeLowering(OrigType);
    }
        
    const TypeLowering *visitAnyEnumType(CanType enumType, EnumDecl *D) {

      // For now, if the type does not have a fixed layout in all resilience
      // domains, we will treat it as address-only in SIL.
      if (!D->hasFixedLayout(M.getSwiftModule(), Expansion))
        return handleAddressOnly(enumType);
      
      // Lower Self? as if it were Whatever? and Self! as if it were Whatever!.
      if (auto genericEnum = dyn_cast<BoundGenericEnumType>(enumType)) {
        if (auto dynamicSelfType =
              dyn_cast<DynamicSelfType>(genericEnum.getGenericArgs()[0])) {
          if (auto optKind = D->classifyAsOptionalType()) {
            CanType selfType = dynamicSelfType.getSelfType();
            selfType = OptionalType::get(optKind, selfType)->getCanonicalType();

            // Remove the DynamicSelfType from OrigType, too.
            if (OrigType == enumType) {
              OrigType = selfType;
            } else {
              CanType origDynamicSelfType =
                cast<BoundGenericEnumType>(OrigType).getGenericArgs()[0];
              CanType origSelfType =
                cast<DynamicSelfType>(origDynamicSelfType).getSelfType();
              OrigType =
                OptionalType::get(optKind, origSelfType)->getCanonicalType();
            }

            return visitAnyEnumType(selfType, D);
          }
        }
      }

      // If the whole enum is indirect, we lower it as if all payload
      // cases were indirect. This means a fixed-layout indirect enum
      // is always loadable and nontrivial. A resilient indirect enum
      // is still address only, because we don't know how many bits
      // are used for the discriminator.
      if (D->isIndirect()) {
        return new (TC, Dependent) LoadableEnumTypeLowering(OrigType);
      }

      // If any of the enum elements have address-only data, the enum is
      // address-only.
      bool trivial = true;
      for (auto elt : D->getAllElements()) {
        // No-payload elements do not affect address-only-ness.
        if (!elt->hasArgumentType())
          continue;

        // Indirect elements make the type nontrivial, but don't affect
        // address-only-ness.
        if (elt->isIndirect()) {
          trivial = false;
          continue;
        }
        
        auto substEltType = enumType->getTypeOfMember(
                              D->getModuleContext(),
                              elt, nullptr,
                              elt->getArgumentInterfaceType())
          ->getCanonicalType();
        
        switch (classifyType(substEltType->getCanonicalType(),
                             M, Sig, Expansion)) {
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
      return new (TC, Dependent) LoadableEnumTypeLowering(OrigType);
    }

    const TypeLowering *visitDynamicSelfType(CanDynamicSelfType type) {
      return LowerType(TC, cast<DynamicSelfType>(OrigType).getSelfType(), 
                       Sig, Expansion, Dependent)
               .visit(type.getSelfType());
    }

  };
}

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
    ? tc.DependentBPA.Allocate(size, alignof(TypeLowering))
    : tc.IndependentBPA.Allocate(size, alignof(TypeLowering));
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

#ifndef NDEBUG
/// Is this type a lowered type?
static bool isLoweredType(CanType type) {
  if (isa<LValueType>(type) || isa<InOutType>(type))
    return false;
  if (isa<AnyFunctionType>(type))
    return false;
  if (auto tuple = dyn_cast<TupleType>(type)) {
    for (auto elt : tuple.getElementTypes())
      if (!isLoweredType(elt))
        return false;
    return true;
  }
  if (auto meta = dyn_cast<AnyMetatypeType>(type)) {
    return meta->hasRepresentation();
  }
  return true;
}
#endif

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

CanSILFunctionType
TypeConverter::getSILFunctionType(AbstractionPattern origType,
                                  CanAnyFunctionType substType,
                                  unsigned uncurryLevel) {
  return getLoweredType(origType, substType, uncurryLevel)
           .castTo<SILFunctionType>();
}

const TypeLowering &
TypeConverter::getTypeLowering(AbstractionPattern origType,
                               Type origSubstType,
                               unsigned uncurryLevel) {
  CanType substType = origSubstType->getCanonicalType();
  auto key = getTypeKey(origType, substType, uncurryLevel);
  
  assert((!key.isDependent() || CurGenericContext)
         && "dependent type outside of generic context?!");
  
  if (auto existing = find(key))
    return *existing;
  
  // Static metatypes are unitary and can optimized to a "thin" empty
  // representation if the type also appears as a static metatype in the
  // original abstraction pattern.
  if (auto substMeta = dyn_cast<MetatypeType>(substType)) {
    SILType loweredTy;
    
    // If the metatype has already been lowered, it will already carry its
    // representation.
    if (substMeta->hasRepresentation()) {
      loweredTy = SILType::getPrimitiveObjectType(substMeta);
    } else {
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
      // If this is a DynamicSelf metatype, turn it into a metatype of the
      // underlying self type.
      if (auto dynamicSelf = dyn_cast<DynamicSelfType>(instanceType)) {
        instanceType = dynamicSelf.getSelfType();
      }
      
      // Regardless of thinness, metatypes are always trivial.
      auto thinnedTy = CanMetatypeType::get(instanceType, repr);
      loweredTy = SILType::getPrimitiveObjectType(thinnedTy);
    }
    
    auto *theInfo
      = new (*this, key.isDependent()) TrivialTypeLowering(loweredTy);
    insert(key, theInfo);
    return *theInfo;
  }

  // Give existential metatypes @thick representation by default.
  if (auto existMetatype = dyn_cast<ExistentialMetatypeType>(substType)) {
    CanType loweredType;
    if (existMetatype->hasRepresentation()) {
      loweredType = existMetatype;
    } else {
      loweredType =
        CanExistentialMetatypeType::get(existMetatype.getInstanceType(),
                                        MetatypeRepresentation::Thick);
    }

    auto loweredTy = SILType::getPrimitiveObjectType(loweredType);
    auto theInfo
      = new (*this, key.isDependent()) TrivialTypeLowering(loweredTy);
    insert(key, theInfo);
    return *theInfo;
  }

  // AST function types are turned into SIL function types:
  //   - the type is uncurried as desired
  //   - types are turned into their unbridged equivalents, depending
  //     on the abstract CC
  //   - ownership conventions are deduced
  if (auto substFnType = dyn_cast<AnyFunctionType>(substType)) {
    // First, lower at the AST level by uncurrying and substituting
    // bridged types.
    auto substLoweredType =
      getLoweredASTFunctionType(substFnType, uncurryLevel, None);

    AbstractionPattern origLoweredType = [&] {
      if (origType.isExactType(substType)) {
        return AbstractionPattern(origType.getGenericSignature(),
                                  substLoweredType);
      } else if (origType.isTypeParameter()) {
        return origType;
      } else {
        auto origFnType = cast<AnyFunctionType>(origType.getType());
        return AbstractionPattern(origType.getGenericSignature(),
                                  getLoweredASTFunctionType(origFnType,
                                                            uncurryLevel,
                                                            None));
      }
    }();
    TypeKey loweredKey = getTypeKey(origLoweredType, substLoweredType, 0);

    // If the uncurrying/unbridging process changed the type, re-check
    // the cache and add a cache entry for the curried type.
    if (substLoweredType != substType) {
      auto &typeInfo = getTypeLoweringForLoweredFunctionType(loweredKey);
      insert(key, &typeInfo);
      return typeInfo;
    }

    // If it didn't, use the standard logic.
    return getTypeLoweringForUncachedLoweredFunctionType(loweredKey);
  }

  assert(uncurryLevel == 0);

  // inout types are a special case for lowering, because they get
  // completely removed and represented as 'address' SILTypes.
  if (isa<InOutType>(substType)) {
    // Derive SILType for InOutType from the object type.
    CanType substObjectType = substType.getLValueOrInOutObjectType();
    AbstractionPattern origObjectType = origType.getLValueObjectType();

    SILType loweredType = getLoweredType(origObjectType, substObjectType,
                                         uncurryLevel).getAddressType();

    auto *theInfo = new (*this, key.isDependent())
      TrivialTypeLowering(loweredType);
    insert(key, theInfo);
    return *theInfo;
  }

  // We need to lower function and metatype types within tuples.
  if (auto substTupleType = dyn_cast<TupleType>(substType)) {
    auto loweredType = getLoweredTupleType(*this, origType, substTupleType);

    // If that changed anything, check for a lowering at the lowered
    // type.
    if (loweredType != substType) {
      TypeKey loweredKey = getTypeKey(origType, loweredType, 0);
      auto &lowering = getTypeLoweringForLoweredType(loweredKey);
      insert(key, &lowering);
      return lowering;
    }

    // Okay, the lowered type didn't change anything from the subst type.
    // Fall out into the normal path.
  }

  // The Swift type directly corresponds to the lowered type; don't
  // re-check the cache.
  return getTypeLoweringForUncachedLoweredType(key);
}

const TypeLowering &TypeConverter::getTypeLowering(SILType type) {
  auto loweredType = type.getSwiftRValueType();
  auto key = getTypeKey(AbstractionPattern(getCurGenericContext(), loweredType),
                        loweredType, 0);

  return getTypeLoweringForLoweredType(key);
}

const TypeLowering &
TypeConverter::getTypeLoweringForLoweredType(TypeKey key) {
  auto type = key.SubstType;
  assert(isLoweredType(type) && "type is not lowered!");
  (void)type;

  // Re-using uncurry level 0 is reasonable because our uncurrying
  // transforms are idempotent at this level.  This means we don't
  // need a ton of redundant entries in the map.
  if (auto existing = find(key))
    return *existing;

  return getTypeLoweringForUncachedLoweredType(key);
}

const TypeLowering &
TypeConverter::getTypeLoweringForLoweredFunctionType(TypeKey key) {
  assert(isa<AnyFunctionType>(key.SubstType));
  assert(key.UncurryLevel == 0);

  // Check for an existing mapping for the key.
  if (auto existing = find(key))
    return *existing;

  // Okay, we didn't find one; go ahead and produce a SILFunctionType.
  return getTypeLoweringForUncachedLoweredFunctionType(key);
}

const TypeLowering &TypeConverter::
getTypeLoweringForUncachedLoweredFunctionType(TypeKey key) {
  assert(isa<AnyFunctionType>(key.SubstType));
  assert(key.UncurryLevel == 0);

#ifndef NDEBUG
  // Catch recursions.
  insert(key, nullptr);
#endif

  // Generic functions aren't first-class values and shouldn't end up lowered
  // through this interface.
  assert(!isa<PolymorphicFunctionType>(key.SubstType)
         && !isa<GenericFunctionType>(key.SubstType));

  // Construct the SILFunctionType.
  CanType silFnType = getNativeSILFunctionType(M, key.OrigType,
                                       cast<AnyFunctionType>(key.SubstType));

  // Do a cached lookup under yet another key, just so later lookups
  // using the SILType will find the same TypeLowering object.
  auto loweredKey = getTypeKey(key.OrigType, silFnType, 0);
  auto &lowering = getTypeLoweringForLoweredType(loweredKey);
  insert(key, &lowering);
  return lowering;
}

/// Do type-lowering for a lowered type which is not already in the cache.
const TypeLowering &
TypeConverter::getTypeLoweringForUncachedLoweredType(TypeKey key) {
  assert(!find(key) && "re-entrant or already cached");
  assert(isLoweredType(key.SubstType) && "didn't lower out l-value type?");

#ifndef NDEBUG
  // Catch reentrancy bugs.
  insert(key, nullptr);
#endif

  CanType contextType = key.SubstType;
  // FIXME: Get expansion from SILFunction
  auto *theInfo = LowerType(*this, key.SubstType,
                            CanGenericSignature(),
                            ResilienceExpansion::Minimal,
                            key.isDependent()).visit(contextType);

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
  auto resultTy = AFD->getDefaultArg(DefaultArgIndex).second->getCanonicalType();
  assert(resultTy && "Didn't find default argument?");
  
  // Get the generic signature from the surrounding context.
  auto funcInfo = TC.getConstantInfo(SILDeclRef(AFD));
  CanGenericSignature sig;
  if (auto genTy = funcInfo.FormalInterfaceType->getAs<GenericFunctionType>()) {
    sig = genTy->getGenericSignature()->getCanonicalSignature();
    resultTy = ArchetypeBuilder::mapTypeOutOfContext(
        TC.M.getSwiftModule(),
        funcInfo.ContextGenericParams,
        resultTy)->getCanonicalType();
  }
  
  if (sig)
    return CanGenericFunctionType::get(sig,
                                       TupleType::getEmpty(context),
                                       resultTy,
                                       AnyFunctionType::ExtInfo());
  
  return CanFunctionType::get(TupleType::getEmpty(context), resultTy);
}

/// Get the type of a stored property initializer, () -> T.
static CanAnyFunctionType getStoredPropertyInitializerInterfaceType(
                                                     TypeConverter &TC,
                                                     VarDecl *VD,
                                                     ASTContext &context) {
  auto *DC = VD->getDeclContext();
  CanType resultTy =
      ArchetypeBuilder::mapTypeOutOfContext(
          DC, VD->getParentInitializer()->getType())
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

GenericParamList *
TypeConverter::getEffectiveGenericParams(AnyFunctionRef fn,
                                         CaptureInfo captureInfo) {
  auto dc = fn.getAsDeclContext();

  if (dc->getParent()->isLocalContext() &&
      !captureInfo.hasGenericParamCaptures())
    return nullptr;

  return dc->getGenericParamsOfContext();
}

CanGenericSignature
TypeConverter::getEffectiveGenericSignature(AnyFunctionRef fn,
                                            CaptureInfo captureInfo) {
  auto dc = fn.getAsDeclContext();

  if (dc->getParent()->isLocalContext() &&
      !captureInfo.hasGenericParamCaptures())
    return nullptr;

  if (auto sig = dc->getGenericSignatureOfContext())
    return sig->getCanonicalSignature();

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
  if (!theClosure.getCaptureInfo().hasLocalCaptures()) {
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

/// Replace any DynamicSelf types with their underlying Self type.
static Type replaceDynamicSelfWithSelf(Type t) {
  return t.transform([](Type type) -> Type {
    if (auto dynamicSelf = type->getAs<DynamicSelfType>())
      return dynamicSelf->getSelfType();
    return type;
  });
}

/// Replace any DynamicSelf types with their underlying Self type.
static CanType replaceDynamicSelfWithSelf(CanType t) {
  if (!t->hasDynamicSelfType())
    return t;
  return replaceDynamicSelfWithSelf(Type(t))->getCanonicalType();
}

CanAnyFunctionType TypeConverter::makeConstantInterfaceType(SILDeclRef c) {
  ValueDecl *vd = c.loc.dyn_cast<ValueDecl *>();

  switch (c.kind) {
  case SILDeclRef::Kind::Func: {
    if (auto *ACE = c.loc.dyn_cast<AbstractClosureExpr *>()) {
      // FIXME: Closures could have an interface type computed by Sema.
      auto funcTy = cast<AnyFunctionType>(ACE->getType()->getCanonicalType());
      funcTy = cast<AnyFunctionType>(
          ArchetypeBuilder::mapTypeOutOfContext(ACE->getParent(), funcTy)
              ->getCanonicalType());
      return getFunctionInterfaceTypeWithCaptures(funcTy, ACE);
    }

    FuncDecl *func = cast<FuncDecl>(vd);
    auto funcTy = cast<AnyFunctionType>(
                                  func->getInterfaceType()->getCanonicalType());
    funcTy = cast<AnyFunctionType>(replaceDynamicSelfWithSelf(funcTy));
    return getFunctionInterfaceTypeWithCaptures(funcTy, func);
  }

  case SILDeclRef::Kind::Allocator:
  case SILDeclRef::Kind::EnumElement:
    return cast<AnyFunctionType>(vd->getInterfaceType()->getCanonicalType());
  
  case SILDeclRef::Kind::Initializer:
    return cast<AnyFunctionType>(cast<ConstructorDecl>(vd)
                           ->getInitializerInterfaceType()->getCanonicalType());
  
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
}

/// Get the context generic parameters for an entity.
std::pair<GenericParamList *, GenericParamList *>
TypeConverter::getConstantContextGenericParams(SILDeclRef c) {
  ValueDecl *vd = c.loc.dyn_cast<ValueDecl *>();
  
  /// Get the function generic params, including outer params.
  switch (c.kind) {
  case SILDeclRef::Kind::Func: {
    if (auto *ACE = c.getAbstractClosureExpr()) {
      auto captureInfo = getLoweredLocalCaptures(ACE);

      // Closures are currently never natively generic.
      return {getEffectiveGenericParams(ACE, captureInfo), nullptr};
    }
    FuncDecl *func = cast<FuncDecl>(vd);
    auto captureInfo = getLoweredLocalCaptures(func);

    return {getEffectiveGenericParams(func, captureInfo),
            func->getGenericParams()};
  }
  case SILDeclRef::Kind::EnumElement: {
    auto eltDecl = cast<EnumElementDecl>(vd);
    return {
      eltDecl->getDeclContext()->getGenericParamsOfContext(),
      nullptr
    };
  }
  case SILDeclRef::Kind::Allocator:
  case SILDeclRef::Kind::Initializer:
  case SILDeclRef::Kind::Destroyer:
  case SILDeclRef::Kind::Deallocator: {
    auto *afd = cast<AbstractFunctionDecl>(vd);
    return {afd->getGenericParamsOfContext(), afd->getGenericParams()};
  }
  case SILDeclRef::Kind::GlobalAccessor:
  case SILDeclRef::Kind::GlobalGetter: {
    return {
      cast<VarDecl>(vd)->getDeclContext()->getGenericParamsOfContext(),
      nullptr,
    };
  }
  case SILDeclRef::Kind::IVarInitializer:
  case SILDeclRef::Kind::IVarDestroyer:
    return {cast<ClassDecl>(vd)->getGenericParamsOfContext(), nullptr};
  case SILDeclRef::Kind::DefaultArgGenerator:
    // Use the context generic parameters of the original declaration.
    return getConstantContextGenericParams(SILDeclRef(c.getDecl()));
  case SILDeclRef::Kind::StoredPropertyInitializer:
    // Use the context generic parameters of the containing type.
    return {
      c.getDecl()->getDeclContext()->getGenericParamsOfContext(),
      nullptr,
    };
  }
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

  SILType silSubstType = getLoweredType(origType, substType).getAddressType();
  substType = silSubstType.getSwiftRValueType();

  // Fast path: if the unsubstituted type from the variable equals the
  // substituted type from the l-value, there's nothing to do.
  if (origType.isExactType(substType))
    return silSubstType;

  // Type substitution preserves structural type structure, and the
  // type-of-reference is only different in the outermost structural
  // types.  So, basically, we just need to undo the changes made by
  // getTypeOfReference and then reapply them on the substituted type.

  // The only really significant manipulation there is with @weak and
  // @unowned.
  if (auto refType = origType.getAs<ReferenceStorageType>()) {
    substType = CanType(ReferenceStorageType::get(substType,
                                                  refType->getOwnership(),
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

  // Create the SILFunctionType for the callback.
  SILParameterInfo params[] = {
    { ctx.TheRawPointerType, ParameterConvention::Direct_Unowned },
    { ctx.TheUnsafeValueBufferType, ParameterConvention::Indirect_Inout },
    { selfType->getCanonicalType(), ParameterConvention::Indirect_Inout },
    { selfMetatypeType->getCanonicalType(), ParameterConvention::Direct_Unowned },
  };
  ArrayRef<SILResultInfo> results = {};
  auto extInfo = 
    SILFunctionType::ExtInfo()
      .withRepresentation(SILFunctionTypeRepresentation::Thin);

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
          Type captureType;
          if (auto *selfType = capturedVar->getType()->getAs<DynamicSelfType>()) {
            captureType = selfType->getSelfType();
            if (auto *metatypeType = captureType->getAs<MetatypeType>())
              captureType = metatypeType->getInstanceType();

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
TypeConverter::checkForABIDifferences(CanType type1, CanType type2) {
  assert(!isa<AnyFunctionType>(type1) && !isa<AnyFunctionType>(type2) &&
         "cannot compare unlowered types for ABI compatibility");

  // Unwrap optionals, but remember that we did.
  OptionalTypeKind OTK1, OTK2;

  {
    // Get the lowered optional payload types.
    AbstractionPattern opaque = AbstractionPattern::getOpaque();

    CanType object1 = type1.getAnyOptionalObjectType(OTK1);
    if (OTK1 != OTK_None)
      type1 = getLoweredType(opaque, object1).getSwiftRValueType();

    CanType object2 = type2.getAnyOptionalObjectType(OTK2);
    if (OTK2 != OTK_None)
      type2 = getLoweredType(opaque, object2).getSwiftRValueType();
  }
  
  // Forcing IUOs always requires a thunk.
  if (OTK1 == OTK_ImplicitlyUnwrappedOptional && OTK2 == OTK_None)
    return ABIDifference::NeedsThunk;
  
  // Except for the above case, we should not be making a value less optional.
  assert(OTK1 == OTK_None || OTK2 != OTK_None);
  
  // If we're introducing a level of optionality, only certain types are
  // ABI-compatible -- check below.
  bool optionalityChange = (OTK1 == OTK_None && OTK2 != OTK_None);

  // If the types are identical and there was no optionality change,
  // we're done.
  if (type1 == type2 && !optionalityChange)
    return ABIDifference::Trivial;
  
  // Classes, class-constrained archetypes, and pure-ObjC existential types
  // all have single retainable pointer representation; optionality change
  // is allowed.
  if ((type1->mayHaveSuperclass() || type1->isObjCExistentialType()) &&
      (type2->mayHaveSuperclass() || type2->isObjCExistentialType()))
    return ABIDifference::Trivial;

  // Function parameters are ABI compatible if their differences are
  // trivial.
  if (auto fnTy1 = dyn_cast<SILFunctionType>(type1)) {
    if (auto fnTy2 = dyn_cast<SILFunctionType>(type2)) {
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
  if (auto meta1 = dyn_cast<MetatypeType>(type1)) {
    if (auto meta2 = dyn_cast<MetatypeType>(type2)) {
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
  if (auto meta1 = dyn_cast<ExistentialMetatypeType>(type1)) {
    if (auto meta2 = dyn_cast<ExistentialMetatypeType>(type2)) {
      if (meta1->getRepresentation() == meta2->getRepresentation() &&
          meta1->getRepresentation() == MetatypeRepresentation::ObjC)
        return ABIDifference::Trivial;
    }
  }

  // Tuple types are ABI-compatible if their elements are.
  if (!optionalityChange) {
    if (auto tuple1 = dyn_cast<TupleType>(type1)) {
      if (auto tuple2 = dyn_cast<TupleType>(type2)) {
        if (tuple1->getNumElements() != tuple2->getNumElements())
          return ABIDifference::NeedsThunk;
        
        for (unsigned i = 0, e = tuple1->getNumElements(); i < e; i++) {
          if (checkForABIDifferences(tuple1.getElementType(i),
                                     tuple2.getElementType(i))
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

  if (fnTy1->getNumAllResults() != fnTy2->getNumAllResults())
    return ABIDifference::NeedsThunk;

  // If we don't have a context but the other type does, we'll return
  // ABIDifference::ThinToThick below.
  if (fnTy1->getExtInfo().hasContext() &&
      fnTy1->getCalleeConvention() != fnTy2->getCalleeConvention())
    return ABIDifference::NeedsThunk;

  for (unsigned i : indices(fnTy1->getAllResults())) {
    auto result1 = fnTy1->getAllResults()[i];
    auto result2 = fnTy2->getAllResults()[i];

    if (result1.getConvention() != result2.getConvention())
      return ABIDifference::NeedsThunk;

    if (checkForABIDifferences(result1.getType(), result2.getType())
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

    if (checkForABIDifferences(error1.getType(), error2.getType())
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

    if (checkForABIDifferences(param1.getType(), param2.getType())
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
