//===--- TypeLowering.cpp - Type information for SILGen ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace Lowering;

namespace {
  /// A CRTP type visitor for deciding whether the metatype for a type
  /// has trivial representation.
  struct HasTrivialMetatype : CanTypeVisitor<HasTrivialMetatype, bool> {
    /// Class metatypes have non-trivial representation due to the
    /// possibility of subclassing.
    bool visitClassType(CanClassType type) {
      return false;
    }
    bool visitBoundGenericClassType(CanBoundGenericClassType type) {
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

    /// Existential metatypes have non-trivial representation because
    /// they can refer to an arbitrary metatype. Everything else is trivial.
    bool visitType(CanType type) {
      return !type->isExistentialType();
    }
  };
}

/// Does the metatype for the given type have a trivial representation?
static bool hasTrivialMetatype(CanType instanceType) {
  return HasTrivialMetatype().visit(instanceType);
}

static CanType getKnownType(Optional<CanType> &cacheSlot,
                            ASTContext &C,
                            StringRef moduleName,
                            StringRef typeName) {
  CanType t = cacheSlot.cache([&] {
    Optional<UnqualifiedLookup> lookup
      = UnqualifiedLookup::forModuleAndName(C, moduleName, typeName);
    if (!lookup)
      return CanType();
    if (TypeDecl *typeDecl = lookup->getSingleTypeResult()) {
      assert(typeDecl->getDeclaredType() &&
             "bridged type must be type-checked");
      return typeDecl->getDeclaredType()->getCanonicalType();
    }
    return CanType();
  });

  assert(t && "bridging type not found but we got past the clang importer?!");
  
  DEBUG(llvm::dbgs() << "Bridging type " << moduleName << '.' << typeName
          << " mapped to ";
        if (t)
          t->print(llvm::dbgs());
        else
          llvm::dbgs() << "<null>";
        llvm::dbgs() << '\n');
  return t;
}

#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType) \
  CanType TypeConverter::get##BridgedType##Type() {         \
    return getKnownType(BridgedType##Ty, M.getASTContext(), \
                        #BridgedModule, #BridgedType);      \
  }                                                         \
  CanType TypeConverter::get##NativeType##Type() {          \
    return getKnownType(NativeType##Ty, M.getASTContext(),  \
                        #NativeModule, #NativeType);        \
  }
#include "swift/SIL/BridgedTypes.def"

CaptureKind Lowering::getDeclCaptureKind(ValueDecl *capture) {
  if (VarDecl *var = dyn_cast<VarDecl>(capture)) {
    if (var->isComputed())
      return var->isSettable()? CaptureKind::GetterSetter : CaptureKind::Getter;

    if (var->isLet())
      return CaptureKind::Constant;

    return CaptureKind::Box;
  }
  
  // "Captured" local typealiases require no context.
  // FIXME: Is this true for dependent typealiases?
  if (isa<TypeAliasDecl>(capture))
    return CaptureKind::None;
  
  return CaptureKind::LocalFunction;
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

static LoweredTypeKind classifyType(CanType type, SILModule &M);

namespace {
  /// A CRTP helper class for doing things that depends on type
  /// classification.
  template <class Impl, class RetTy>
  class TypeClassifierBase : public CanTypeVisitor<Impl, RetTy> {
    SILModule &M;
    Impl &asImpl() { return *static_cast<Impl*>(this); }
  protected:
    TypeClassifierBase(SILModule &M) : M(M) {}

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
    IMPL(BuiltinObjectPointer, Reference)
    IMPL(BuiltinObjCPointer, Reference)
    IMPL(BuiltinVector, Trivial)
    IMPL(Class, Reference)
    IMPL(BoundGenericClass, Reference)
    IMPL(Metatype, Trivial)
    IMPL(AnyFunction, Reference)
    IMPL(SILFunction, Reference)
    IMPL(Array, AddressOnly) // who knows?
    IMPL(Module, Trivial)

#undef IMPL

    RetTy visitLValueType(CanLValueType type) {
      llvm_unreachable("shouldn't get an l-value type here");
    }
    RetTy visitInOutType(CanInOutType type) {
      llvm_unreachable("shouldn't get an @inout type here");
    }

    // Delegate dependent types to their context archetypes.

    RetTy visitDependent(CanType type) {
      CanType contextType = M.Types.getArchetypes().substDependentType(type)
        ->getCanonicalType();
      return asImpl().visit(contextType);
    }
    
    RetTy visitGenericTypeParamType(CanGenericTypeParamType type) {
      return asImpl().visitDependent(type);
    }
    RetTy visitDependentMemberType(CanDependentMemberType type) {
      return asImpl().visitDependent(type);
    }

    RetTy visitUnownedStorageType(CanUnownedStorageType type) {
      return asImpl().handleReference(type);
    }

    RetTy visitWeakStorageType(CanWeakStorageType type) {
      return asImpl().handleAddressOnly(type);
    }

    // These types are address-only unless they're class-constrained.
    template <class T> RetTy visitAbstracted(T type) {
      if (type->requiresClass()) {
        return asImpl().handleReference(type);
      } else {
        return asImpl().handleAddressOnly(type);        
      }
    }
    RetTy visitArchetypeType(CanArchetypeType type) {
      return visitAbstracted(type);
    }
    RetTy visitProtocolType(CanProtocolType type) {
      return visitAbstracted(type);
    }
    RetTy visitProtocolCompositionType(CanProtocolCompositionType type) {
      return visitAbstracted(type);
    }

    // Enums depend on their enumerators.
    RetTy visitEnumType(CanEnumType type) {
      return asImpl().visitAnyEnumType(type, type->getDecl());
    }
    RetTy visitBoundGenericEnumType(CanBoundGenericEnumType type) {
      return asImpl().visitAnyEnumType(type, type->getDecl());
    }
    RetTy visitAnyEnumType(CanType type, EnumDecl *D) {
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
      // Consult the type lowering.  This means we implicitly get
      // caching, but that type lowering needs to override this case.
      auto &lowering = M.Types.getTypeLowering(type);
      return handleClassificationFromLowering(type, lowering);
    }
    
    // Tuples depend on their elements.
    RetTy visitTupleType(CanTupleType type) {
      bool hasReference = false;
      for (auto eltType : type.getElementTypes()) {
        switch (classifyType(eltType, M)) {
        case LoweredTypeKind::Trivial:
          continue;
        case LoweredTypeKind::AddressOnly:
          return LoweredTypeKind::AddressOnly;
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
  };

  class TypeClassifier :
      public TypeClassifierBase<TypeClassifier, LoweredTypeKind> {
  public:
    TypeClassifier(SILModule &M) : TypeClassifierBase(M) {}

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

static LoweredTypeKind classifyType(CanType type, SILModule &M) {
  return TypeClassifier(M).visit(type);
}

/// True if the type, or the referenced type of an address
/// type, is address-only.  For example, it could be a resilient struct or
/// something of unknown size.
bool SILType::isAddressOnly(CanType type, SILModule &M) {
  return classifyType(type, M) == LoweredTypeKind::AddressOnly;
}

namespace {
  /// A class for loadable types.
  class LoadableTypeLowering : public TypeLowering {
  protected:
    LoadableTypeLowering(SILType type, IsTrivial_t isTrivial)
      : TypeLowering(type, isTrivial, IsNotAddressOnly) {}

  public:
    void emitDestroyAddress(SILBuilder &B, SILLocation loc,
                            SILValue addr) const override {
      SILValue value = B.createLoad(loc, addr);
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
      : LoadableTypeLowering(type, IsTrivial) {}

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

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc,
                                 SILValue value,
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
    NonTrivialLoadableTypeLowering(SILType type)
      : LoadableTypeLowering(type, IsNotTrivial) {}

    SILValue emitLoadOfCopy(SILBuilder &B, SILLocation loc,
                            SILValue addr, IsTake_t isTake) const override {
      SILValue value = B.createLoad(loc, addr);
      if (!isTake) value = emitCopyValue(B, loc, value);
      return value;
    }

    void emitStoreOfCopy(SILBuilder &B, SILLocation loc,
                         SILValue newValue, SILValue addr,
                         IsInitialization_t isInit) const override {
      SILValue oldValue;
      if (!isInit) oldValue = B.createLoad(loc, addr);
      B.createStore(loc, newValue, addr);
      if (!isInit) emitDestroyValue(B, loc, oldValue);
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

    /// The number of children of this aggregate. The data follows the instance
    /// data.
    unsigned NumChildren;

  protected:
    LoadableAggTypeLowering(SILType type, ArrayRef<Child> children)
        : NonTrivialLoadableTypeLowering(type),
          NumChildren(children.size()) {
      memcpy(reinterpret_cast<Child*>(&asImpl()+1),
             children.data(),
             NumChildren * sizeof(Child));
    }

  public:
    static const Impl *create(TypeConverter &TC, IsDependent_t dependent,
                              CanType type, ArrayRef<Child> children) {
      size_t bufferSize =
        sizeof(Impl) + sizeof(Child) * children.size();
      void *buffer = operator new(bufferSize, TC, dependent);
      auto silType = SILType::getPrimitiveObjectType(type);
      return ::new(buffer) Impl(silType, children);
    }

    ArrayRef<Child> getChildren() const {
      auto buffer = reinterpret_cast<const Child*>(&asImpl() + 1);
      return ArrayRef<Child>(buffer, NumChildren);
    }

    template <class T>
    void forEachNonTrivialChild(SILBuilder &B, SILLocation loc,
                                SILValue aggValue,
                                const T &operation) const {
      for (auto &child : getChildren()) {
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
                           SILValue aggValue) const override {
      return B.createCopyValue(loc, aggValue);
    }

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue aggValue,
                                  LoweringStyle style) const override {
      // Types with non-trivial copy semantics will need to override this.
      bool hasDifference = false;
      SmallVector<SILValue, 4> copiedChildren;

      for (auto &child : getChildren()) {
        auto &childLowering = child.getLowering();
        SILValue childValue = asImpl().emitRValueProject(B, loc, aggValue,
                                                         child.getIndex(),
                                                         childLowering);
        if (!childLowering.isTrivial()) {
          SILValue copiedChildValue =
            childLowering.emitLoweredCopyChildValue(B, loc, childValue, style);
          hasDifference |= (copiedChildValue != childValue);
          childValue = copiedChildValue;
        }

        copiedChildren.push_back(childValue);
      }

      if (hasDifference) {
        return asImpl().Impl::rebuildAggregate(B, loc, copiedChildren);
      } else {
        return aggValue;
      }
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue aggValue) const override {
      B.createDestroyValue(loc, aggValue);
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
      case LoweringStyle::DeepNoEnum:
        Fn = &TypeLowering::emitLoweredDestroyValueDeepNoEnum;
        break;
      }

      forEachNonTrivialChild(B, loc, aggValue, Fn);
    }
  };

  /// A lowering for loadable but non-trivial tuple types.
  class LoadableTupleTypeLowering final
      : public LoadableAggTypeLowering<LoadableTupleTypeLowering, unsigned> {
  public:
    LoadableTupleTypeLowering(SILType type,
                              ArrayRef<Child> children)
      : LoadableAggTypeLowering(type, children) {}

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
  };

  /// A lowering for loadable but non-trivial struct types.
  class LoadableStructTypeLowering final
      : public LoadableAggTypeLowering<LoadableStructTypeLowering, VarDecl*> {
  public:
    LoadableStructTypeLowering(SILType type,
                               ArrayRef<Child> children)
      : LoadableAggTypeLowering(type, children) {}

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
    /// The number of tail-allocated NonTrivialElements following this object.
    unsigned numNonTrivial;
    
    LoadableEnumTypeLowering(SILType type,
                             ArrayRef<NonTrivialElement> nonTrivial)
      : NonTrivialLoadableTypeLowering(type), numNonTrivial(nonTrivial.size())
    {
      memcpy(reinterpret_cast<NonTrivialElement*>(this+1), nonTrivial.data(),
             numNonTrivial * sizeof(NonTrivialElement));
    }

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
      for (auto &elt : getNonTrivialElements()) {
        auto bb = new (M) SILBasicBlock(&B.getFunction());
        auto argTy = elt.getLowering().getLoweredType();
        new (M) SILArgument(argTy, bb);
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
        const TypeLowering &lowering = getNonTrivialElements()[i].getLowering();
        B.emitBlock(bb);
        operation(B, loc, bb->getBBArgs()[0], lowering, doneBB);
      }
      
      B.emitBlock(doneBB);
    }
    
  public:
    static const LoadableEnumTypeLowering *create(TypeConverter &TC,
                                       IsDependent_t dependent,
                                       CanType type,
                                       ArrayRef<NonTrivialElement> nonTrivial) {
      void *buffer
        = operator new(sizeof(LoadableEnumTypeLowering)
                         + sizeof(NonTrivialElement) * nonTrivial.size(),
                       TC, dependent);
      
      auto silTy = SILType::getPrimitiveObjectType(type);
      
      return ::new (buffer) LoadableEnumTypeLowering(silTy, nonTrivial);
    }
    
    ArrayRef<NonTrivialElement> getNonTrivialElements() const {
      auto buffer = reinterpret_cast<const NonTrivialElement*>(this+1);
      return ArrayRef<NonTrivialElement>(buffer, numNonTrivial);
    }

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      return B.createCopyValue(loc, value);
    }

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  LoweringStyle style) const override {
      if (style == LoweringStyle::Shallow ||
          style == LoweringStyle::DeepNoEnum) {
        return B.createCopyValue(loc, value);
      } else {
        ifNonTrivialElement(B, loc, value,
          [&](SILBuilder &B, SILLocation loc, SILValue child,
              const TypeLowering &childLowering, SILBasicBlock *dest) {
            SILValue copiedChild =
              childLowering.emitLoweredCopyChildValue(B, loc, child, style);
            B.createBranch(loc, dest, copiedChild);
          });
        return new (B.getFunction().getModule())
          SILArgument(value.getType(), B.getInsertionBB());
      }
    }
    
    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      B.createDestroyValue(loc, value);
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc,
                                 SILValue value,
                                 LoweringStyle style) const override {
      assert(style != LoweringStyle::Shallow &&
             "This method should never be called when performing a shallow "
             "destroy value.");
      if (style == LoweringStyle::DeepNoEnum)
        B.createDestroyValue(loc, value);
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
    LeafLoadableTypeLowering(SILType type)
      : NonTrivialLoadableTypeLowering(type) {}

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value,
                                  LoweringStyle style) const override {
      return emitCopyValue(B, loc, value);
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc,
                                 SILValue value,
                                 LoweringStyle style) const override {
      emitDestroyValue(B, loc, value);
    }
  };

  /// A class for reference types, which are all non-trivial but still
  /// loadable.
  class ReferenceTypeLowering : public LeafLoadableTypeLowering {
  public:
    ReferenceTypeLowering(SILType type) : LeafLoadableTypeLowering(type) {}

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      if (!isa<FunctionRefInst>(value))
        B.createStrongRetain(loc, value);
      return value;
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      B.emitStrongRelease(loc, value);
    }
  };

  /// A type lowering for [unowned] types.
  class UnownedTypeLowering final : public LeafLoadableTypeLowering {
  public:
    UnownedTypeLowering(SILType type) : LeafLoadableTypeLowering(type) {}

    SILValue emitCopyValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      B.createUnownedRetain(loc, value);
      return value;
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      B.createUnownedRelease(loc, value);
    }
  };

  /// A class for non-trivial, address-only types.
  class AddressOnlyTypeLowering : public TypeLowering {
  public:
    AddressOnlyTypeLowering(SILType type)
      : TypeLowering(type, IsNotTrivial, IsAddressOnly) {}

  public:
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
      B.emitDestroyAddr(loc, addr);
    }

    void emitDestroyRValue(SILBuilder &B, SILLocation loc,
                           SILValue value) const override {
      B.emitDestroyAddr(loc, value);
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

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc,
                                 SILValue value,
                                 LoweringStyle style) const override {
      llvm_unreachable("type is not loadable!");
    }
  };

  /// Build the appropriate TypeLowering subclass for the given type.
  class LowerType :
      public TypeClassifierBase<LowerType, const TypeLowering *> {
    TypeConverter &TC;
    IsDependent_t Dependent;
  public:
    LowerType(TypeConverter &TC, IsDependent_t Dependent)
      : TypeClassifierBase(TC.M), TC(TC), Dependent(Dependent) {}

    const TypeLowering *handleTrivial(CanType type) {
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC, Dependent) TrivialTypeLowering(silType);
    }
  
    const TypeLowering *handleReference(CanType type) {
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC, Dependent) ReferenceTypeLowering(silType);
    }

    const TypeLowering *handleAddressOnly(CanType type) {
      auto silType = SILType::getPrimitiveAddressType(type);
      return new (TC, Dependent) AddressOnlyTypeLowering(silType);
    }

    /// [unowned] is basically like a reference type lowering except
    /// it manipulates unowned reference counts instead of strong.
    const TypeLowering *visitUnownedStorageType(CanUnownedStorageType type) {
      return new (TC, Dependent) UnownedTypeLowering(
                                      SILType::getPrimitiveObjectType(type));
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
      return LoadableTupleTypeLowering::create(TC, Dependent,
                                               tupleType, childElts);
    }

    const TypeLowering *visitAnyStructType(CanType structType, StructDecl *D) {
      typedef LoadableStructTypeLowering::Child Child;
      SmallVector<Child, 8> childFields;

      // For consistency, if it's anywhere resilient, we need to treat the type
      // as resilient in SIL.
      if (TC.isAnywhereResilient(D))
        return handleAddressOnly(structType);

      bool hasOnlyTrivialChildren = true;
      for (auto field : D->getStoredProperties()) {
        auto origFieldType = AbstractionPattern(field->getType());
        auto substFieldType =
          structType->getTypeOfMember(D->getModuleContext(), field, nullptr);
        auto &lowering = TC.getTypeLowering(origFieldType, substFieldType);
        if (lowering.isAddressOnly())
          return handleAddressOnly(structType);
        hasOnlyTrivialChildren &= lowering.isTrivial();
        childFields.push_back(Child(field, lowering));
      }

      if (hasOnlyTrivialChildren)
        return handleTrivial(structType);
      return LoadableStructTypeLowering::create(TC, Dependent,
                                                structType, childFields);
    }
        
    const TypeLowering *visitAnyEnumType(CanType enumType, EnumDecl *D) {
      // For consistency, if it's anywhere resilient, we need to treat the type
      // as resilient in SIL.
      if (TC.isAnywhereResilient(D))
        return handleAddressOnly(enumType);
      
      typedef LoadableEnumTypeLowering::NonTrivialElement NonTrivialElement;
      SmallVector<NonTrivialElement, 8> nonTrivialElts;
      
      // If any of the enum elements have address-only data, the enum is
      // address-only.
      for (auto elt : D->getAllElements()) {
        // No-payload elements do not affect address-only-ness.
        if (!elt->hasArgumentType())
          continue;
        
        auto eltType = enumType->getTypeOfMember(D->getModuleContext(),
                                                  elt, nullptr,
                                                  elt->getArgumentType())
          ->getCanonicalType();
        auto &lowering = TC.getTypeLowering(eltType);
        if (lowering.isAddressOnly())
          return handleAddressOnly(enumType);
        if (!lowering.isTrivial())
          nonTrivialElts.push_back(NonTrivialElement(elt, lowering));        
      }
      if (nonTrivialElts.empty())
        return handleTrivial(enumType);
      return LoadableEnumTypeLowering::create(TC, Dependent,
                                              enumType, nonTrivialElts);
    }
        
    // For dependent types, lower based on the context archetype we created,
    // but preserve the original type in the lowering.
    
    const TypeLowering *visitGenericTypeParamType(CanGenericTypeParamType type){
      assert(Dependent && "generic type param type not dependent?!");

      // The parameter is address-only unless our context requirements make it
      // class-constrained.
      if (TC.getArchetypes().getArchetype(type)->requiresClass())
        return handleReference(type);
      else
        return handleAddressOnly(type);
    }
        
    const TypeLowering *visitDependentMemberType(CanDependentMemberType type) {
      assert(Dependent && "dependent member type not dependent?!");
      
      // The parameter is address-only unless our context requirements make it
      // class-constrained.
      CanType contextType = TC.getArchetypes().substDependentType(type)
        ->getCanonicalType();
      
      if (contextType->hasReferenceSemantics())
        return handleReference(type);
      else
        return handleAddressOnly(type);
    }
  };
}

TypeConverter::TypeConverter(SILModule &m)
  : M(m), Context(m.getASTContext()) {
}

TypeConverter::~TypeConverter() {
  assert(!GenericArchetypes.hasValue() && "generic context was never popped?!");

  // The bump pointer allocator destructor will deallocate but not destroy all
  // our independent TypeLowerings.
  for (auto &ti : IndependentTypes) {
    // Destroy only the unique entries.
    CanType srcType = CanType(ti.first.OrigType);
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
  auto &Types = k.isDependent() ? DependentTypes : IndependentTypes;
  auto found = Types.find(k);
  if (found == Types.end())
    return nullptr;
  // In debug builds we place a null placeholder in the hashtable to catch
  // reentrancy bugs.
  assert(found->second && "reentered TypeLowering");
  return found->second;
}

void TypeConverter::insert(TypeKey k, const TypeLowering *tl) {
  auto &Types = k.isDependent() ? DependentTypes : IndependentTypes;
  Types[k] = tl;
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
  if (auto meta = dyn_cast<MetatypeType>(type)) {
    return meta->hasThin();
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

    auto &substElt = substType->getFields()[i];
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
  
  assert(!key.isDependent() || GenericArchetypes.hasValue()
         && "dependent type outside of generic context?!");
  
  if (auto existing = find(key))
    return *existing;
  
  // Static metatypes are unitary and can optimized to a "thin" empty
  // representation if the type also appears as a static metatype in the
  // original abstraction pattern.
  if (auto substMeta = dyn_cast<MetatypeType>(substType)) {
    bool isThin;
    
    auto origMeta = dyn_cast<MetatypeType>(origType.getAsType());
    if (!origMeta) {
      // If the metatype matches a dependent type, it cannot be thin.
      assert((isa<SubstitutableType>(origType.getAsType())
              || isa<DependentMemberType>(origType.getAsType()))
         && "metatype matches in position that isn't a dependent type "
            "or metatype?!");
      isThin = false;
    } else {
      // Otherwise, we're thin if the metatype is thinnable both substituted and
      // in the abstraction pattern.
      isThin = hasTrivialMetatype(substMeta.getInstanceType())
        && hasTrivialMetatype(origMeta.getInstanceType());
    }
    
    // Regardless of thinness, metatypes are always trivial.
    auto thinnedTy = CanMetatypeType::get(substMeta.getInstanceType(),
                                          isThin, substMeta->getASTContext());
    auto loweredTy = SILType::getPrimitiveObjectType(thinnedTy);
    auto *theInfo
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
      getLoweredASTFunctionType(substFnType, uncurryLevel);

    AbstractionPattern origLoweredType;
    if (substType == origType.getAsType()) {
      origLoweredType = AbstractionPattern(substLoweredType);
    } else if (origType.isOpaque()) {
      origLoweredType = origType;
    } else {
      auto origFnType = cast<AnyFunctionType>(origType.getAsType());
      origLoweredType =
        AbstractionPattern(getLoweredASTFunctionType(origFnType, uncurryLevel));
    }
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

  // @inout types are a special case for lowering, because they get
  // completely removed and represented as 'address' SILTypes.
  if (auto substInOutType = dyn_cast<InOutType>(substType)) {
    // Derive SILType for InOutType from the object type.
    CanType substObjectType = substInOutType.getObjectType();
    AbstractionPattern origObjectType = origType.getLValueObjectType();

    SILType loweredType = getLoweredType(origObjectType, substObjectType,
                                         uncurryLevel).getAddressType();

    auto *theInfo = new (*this, key.isDependent()) TrivialTypeLowering(loweredType);
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
  auto key = getTypeKey(AbstractionPattern(loweredType), loweredType, 0);

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
  // Catch reentrancy bugs.
  insert(key, nullptr);
#endif

  // Construct the SILFunctionType.
  CanType silFnType = getNativeSILFunctionType(M, key.OrigType,
                                       cast<AnyFunctionType>(key.SubstType));

  // Do a cached lookup under yet another key, just so later lookups
  // using the SILType will find the same TypeLowering object.
  auto loweredKey = getTypeKey(AbstractionPattern(key.OrigType), silFnType, 0);
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

  auto *theInfo = LowerType(*this, key.isDependent()).visit(key.SubstType);
  insert(key, theInfo);
  return *theInfo;
}

/// Get the type of a global variable accessor function, () -> RawPointer.
static CanAnyFunctionType getGlobalAccessorType(CanType varType) {
  ASTContext &C = varType->getASTContext();
  return CanFunctionType::get(TupleType::getEmpty(C), C.TheRawPointerType);
}

/// Get the type of a default argument generator, () -> T.
static CanAnyFunctionType getDefaultArgGeneratorType(AbstractFunctionDecl *AFD,
                                                     unsigned DefaultArgIndex,
                                                     ASTContext &context) {
  auto resultTy = AFD->getDefaultArg(DefaultArgIndex).second->getCanonicalType();
  assert(resultTy && "Didn't find default argument?");
  return CanFunctionType::get(TupleType::getEmpty(context), resultTy);
}

/// Get the type of a destructor function.
static CanAnyFunctionType getDestructorType(DestructorDecl *dd,
                                            bool isDeallocating,
                                            ASTContext &C,
                                            bool isForeign) {
  auto classType = dd->getDeclContext()->getDeclaredTypeInContext()
                     ->getCanonicalType();

  if (isForeign) {
    assert(isDeallocating && "There are no foreign destroying destructors");
    auto extInfo = AnyFunctionType::ExtInfo(AbstractCC::ObjCMethod,
                                            /*thin*/ true,
                                            /*noreturn*/ false);
    return CanFunctionType::get(classType,
                                TupleType::getEmpty(C)->getCanonicalType(),
                                extInfo);
  }

  auto extInfo = AnyFunctionType::ExtInfo(AbstractCC::Method,
                                          /*thin*/ true,
                                          /*noreturn*/ false);
  CanType resultTy = isDeallocating? TupleType::getEmpty(C)->getCanonicalType()
                                   : CanType(C.TheObjectPointerType);

  auto selfType = classType;
  if (auto params = dd->getDeclContext()->getGenericParamsOfContext())
    return CanPolymorphicFunctionType::get(selfType, resultTy, params, extInfo);

  return CanFunctionType::get(selfType, resultTy, extInfo);
}

/// Retrieve the type of the ivar initializer or destroyer method for
/// a class.
static CanAnyFunctionType getIVarInitDestroyerType(ClassDecl *cd, 
                                                   bool isObjC,
                                                   ASTContext &ctx) {
  auto classType = cd->getDeclaredTypeInContext()->getCanonicalType();

  auto emptyTupleTy = TupleType::getEmpty(ctx)->getCanonicalType();
  auto extInfo = AnyFunctionType::ExtInfo(isObjC? AbstractCC::ObjCMethod
                                                : AbstractCC::Method,
                                          /*thin*/ true,
                                          /*noreturn*/ false);
  
  auto resultType = CanFunctionType::get(emptyTupleTy, emptyTupleTy, extInfo);
  if (auto params = cd->getGenericParams())
    return CanPolymorphicFunctionType::get(classType, resultType, params, 
                                           extInfo);
  return CanFunctionType::get(classType, resultType, extInfo);
}

GenericParamList *
TypeConverter::getEffectiveGenericParamsForContext(DeclContext *dc) {

  // FIXME: This is a clunky way of uncurrying nested type parameters from
  // a function context.
  if (auto func = dyn_cast<AbstractFunctionDecl>(dc)) {
    return getConstantFunctionType(SILDeclRef(func))->getGenericParams();
  }

  if (auto closure = dyn_cast<AbstractClosureExpr>(dc)) {
    return getConstantFunctionType(SILDeclRef(closure))->getGenericParams();
  }

  return dc->getGenericParamsOfContext();
}

CanAnyFunctionType
TypeConverter::getFunctionTypeWithCaptures(CanAnyFunctionType funcType,
                                           ArrayRef<ValueDecl*> captures,
                                           DeclContext *parentContext) {
  // Capture generic parameters from the enclosing context.
  GenericParamList *genericParams
    = getEffectiveGenericParamsForContext(parentContext);;

  if (captures.empty()) {
    if (!genericParams)
      return getThinFunctionType(funcType);

    auto extInfo = AnyFunctionType::ExtInfo(AbstractCC::Freestanding,
                                            /*thin*/ true,
                                            /*noreturn*/ false);

    return CanPolymorphicFunctionType::get(funcType.getInput(),
                                           funcType.getResult(),
                                           genericParams, extInfo);

  }

  SmallVector<TupleTypeElt, 8> inputFields;

  for (ValueDecl *capture : captures) {
    // A capture of a 'var' or '@inout' variable is done with the underlying
    // object type.
    auto captureType =
      capture->getType()->getLValueOrInOutObjectType()->getCanonicalType();

    switch (getDeclCaptureKind(capture)) {
    case CaptureKind::None:
      break;
        
    case CaptureKind::LocalFunction:
      // Local functions are captured by value.
      inputFields.push_back(TupleTypeElt(captureType));
      break;
    case CaptureKind::GetterSetter: {
      // Capture the setter and getter closures.
      Type setterTy;
      if (auto subscript = dyn_cast<SubscriptDecl>(capture))
        setterTy = subscript->getSetterType();
      else
        setterTy = cast<VarDecl>(capture)->getSetterType();
      inputFields.push_back(TupleTypeElt(setterTy));
      SWIFT_FALLTHROUGH;
    }
    case CaptureKind::Getter: {
      // Capture the getter closure.
      Type getterTy;
      if (auto subscript = dyn_cast<SubscriptDecl>(capture))
        getterTy = subscript->getGetterType();
      else
        getterTy = cast<VarDecl>(capture)->getGetterType();

      inputFields.push_back(TupleTypeElt(getterTy));
      break;
    }
    case CaptureKind::Constant:
      if (!getTypeLowering(captureType).isAddressOnly()) {
        // Capture the value directly, unless it is address only.
        inputFields.push_back(TupleTypeElt(captureType));
        break;
      }
      SWIFT_FALLTHROUGH;
    case CaptureKind::Box: {
      // Capture the owning ObjectPointer and the address of the value.
      inputFields.push_back(Context.TheObjectPointerType);
      auto lvType = CanInOutType::get(captureType);
      inputFields.push_back(TupleTypeElt(lvType));
      break;
    }
    }
  }
  
  CanType capturedInputs =
    TupleType::get(inputFields, Context)->getCanonicalType();

  auto extInfo = AnyFunctionType::ExtInfo(AbstractCC::Freestanding,
                                          /*thin*/ true,
                                          /*noreturn*/ false);

  if (genericParams)
    return CanPolymorphicFunctionType::get(capturedInputs, funcType,
                                           genericParams, extInfo);
  
  return CanFunctionType::get(capturedInputs, funcType, extInfo);
}

template <class T>
static CanAnyFunctionType getAccessorType(T *decl, SILDeclRef c) {
  auto fnType =
    (c.kind == SILDeclRef::Kind::Getter ? decl->getGetterType()
                                        : decl->getSetterType());
  return cast<AnyFunctionType>(fnType->getCanonicalType());
}

CanAnyFunctionType
TypeConverter::getConstantFormalTypeWithoutCaptures(SILDeclRef c) {
  return makeConstantType(c, /*withCaptures*/ false);
}

CanAnyFunctionType TypeConverter::makeConstantType(SILDeclRef c,
                                                   bool withCaptures) {
  ValueDecl *vd = c.loc.dyn_cast<ValueDecl *>();

  switch (c.kind) {
  case SILDeclRef::Kind::Func: {
    SmallVector<ValueDecl*, 4> captures;
    if (auto *ACE = c.loc.dyn_cast<AbstractClosureExpr *>()) {
      auto funcTy = cast<AnyFunctionType>(ACE->getType()->getCanonicalType());
      if (!withCaptures) return funcTy;
      ACE->getCaptureInfo().getLocalCaptures(captures);
      return getFunctionTypeWithCaptures(funcTy, captures, ACE->getParent());
    }

    FuncDecl *func = cast<FuncDecl>(vd);
    auto funcTy = cast<AnyFunctionType>(func->getType()->getCanonicalType());
    if (!withCaptures) return funcTy;
    func->getCaptureInfo().getLocalCaptures(captures);
    return getFunctionTypeWithCaptures(funcTy, captures,
                                       func->getDeclContext());
  }

  case SILDeclRef::Kind::Getter:
  case SILDeclRef::Kind::Setter: {
    if (SubscriptDecl *sd = dyn_cast<SubscriptDecl>(vd)) {
      return getAccessorType(sd, c);
    }

    VarDecl *var = cast<VarDecl>(c.getDecl());
    auto accessorMethodType = getAccessorType(var, c);
    if (!withCaptures) return accessorMethodType;
    
    // If this is a local variable, its property methods may be closures.
    if (var->isComputed()) {
      FuncDecl *property = c.kind == SILDeclRef::Kind::Getter
        ? var->getGetter()
        : var->getSetter();
      SmallVector<ValueDecl*, 4> LocalCaptures;
      property->getCaptureInfo().getLocalCaptures(LocalCaptures);
      return getFunctionTypeWithCaptures(accessorMethodType, LocalCaptures,
                                         var->getDeclContext());
    }
    return accessorMethodType;
  }
      
  case SILDeclRef::Kind::Allocator:
  case SILDeclRef::Kind::EnumElement:
    return cast<AnyFunctionType>(vd->getType()->getCanonicalType());
  
  case SILDeclRef::Kind::Initializer:
    return cast<AnyFunctionType>(cast<ConstructorDecl>(vd)
                                   ->getInitializerType()->getCanonicalType());
  
  case SILDeclRef::Kind::Destroyer:
  case SILDeclRef::Kind::Deallocator:
    return getDestructorType(cast<DestructorDecl>(vd), 
                             c.kind == SILDeclRef::Kind::Deallocator,
                             Context,
                             c.isForeign);
  
  case SILDeclRef::Kind::GlobalAccessor: {
    VarDecl *var = cast<VarDecl>(vd);
    assert(!var->isComputed() && "constant ref to computed global var");
    return getGlobalAccessorType(var->getType()->getCanonicalType());
  }
  case SILDeclRef::Kind::DefaultArgGenerator: {
    return getDefaultArgGeneratorType(cast<AbstractFunctionDecl>(vd),
                                      c.defaultArgIndex, Context);
  }
  case SILDeclRef::Kind::IVarDestroyer: {
    return getIVarInitDestroyerType(cast<ClassDecl>(vd), c.isForeign, Context);
  }
  }
}

Type TypeConverter::getLoweredBridgedType(Type t, AbstractCC cc) {  
  switch (cc) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod:
    // No bridging needed for native CCs.
    return t;
  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    // Map native types back to bridged types.
#define BRIDGE_TYPE(BridgedModule,BridgedType, NativeModule,NativeType) \
    if (t->isEqual(get##NativeType##Type()))                         \
      return get##BridgedType##Type();
#include "swift/SIL/BridgedTypes.def"
    return t;
  }
}

SILType TypeConverter::getSubstitutedStorageType(ValueDecl *value,
                                                 Type lvalueType) {
  // The l-value type is the result of applying substitutions to
  // the type-of-reference.  Essentially, we want to apply those
  // same substitutions to value->getType().

  // Canonicalize and lower the l-value's object type.
  CanType origType = value->getType()->getCanonicalType();
  CanType substType
    = cast<LValueType>(lvalueType->getCanonicalType()).getObjectType();
  SILType silSubstType
    = getLoweredType(AbstractionPattern(origType), substType).getAddressType();
  substType = silSubstType.getSwiftRValueType();

  // Fast path: if the unsubstituted type from the variable equals the
  // substituted type from the l-value, there's nothing to do.
  if (origType == substType)
    return silSubstType;

  // Type substitution preserves structural type structure, and the
  // type-of-reference is only different in the outermost structural
  // types.  So, basically, we just need to undo the changes made by
  // getTypeOfReference and then reapply them on the substituted type.

  // The only really significant manipulation there is with [weak] and
  // [unowned].
  if (auto refType = dyn_cast<ReferenceStorageType>(origType)) {
    // Strip Optional<> off of [weak] types.
    if (isa<WeakStorageType>(refType))
      substType = cast<BoundGenericType>(substType).getGenericArgs()[0];
    substType = CanType(ReferenceStorageType::get(substType,
                                                  refType->getOwnership(),
                                                  Context));
    return SILType::getPrimitiveType(substType, SILValueCategory::Address);
  }

  return silSubstType;
}

void TypeConverter::pushGenericContext(
                               ArrayRef<GenericTypeParamType *> genericParams,
                               ArrayRef<Requirement> requirements) {
  // GenericFunctionTypes shouldn't nest.
  assert(!GenericArchetypes.hasValue() && "already in generic context?!");
  assert(DependentTypes.empty() && "already in generic context?!");
  
  // If the generic signature is empty, this is a no-op.
  if (genericParams.empty() && requirements.empty())
    return;
  
  // Prepare the ArchetypeBuilder with the generic signature.
  GenericArchetypes.emplace(*M.getSwiftModule(), M.getASTContext().Diags);
  if (GenericArchetypes->addGenericSignature(genericParams, requirements))
    llvm_unreachable("error adding generic signature to archetype builder?!");
  GenericArchetypes->assignArchetypes();
}

void TypeConverter::popGenericContext() {
  if (!GenericArchetypes.hasValue())
    return;
  
  // Erase our cached TypeLowering objects and associated mappings for dependent
  // types.
  // Resetting the DependentBPA will deallocate but not run the destructor of
  // the dependent TypeLowerings.
  for (auto &ti : DependentTypes) {
    // Destroy only the unique entries.
    CanType srcType = CanType(ti.first.OrigType);
    CanType mappedType = ti.second->getLoweredType().getSwiftRValueType();
    if (srcType == mappedType || isa<LValueType>(srcType))
      ti.second->~TypeLowering();
  }
  DependentTypes.clear();
  DependentBPA.Reset();
  GenericArchetypes.reset();
}
