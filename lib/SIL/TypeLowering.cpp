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

#if 0  // FIXME.
    // Self is always capture by value, never by address.  Even in the case when
    // self can be reassigned within a init by a super.init() call, it cannot be
    // live in a closure.
    if (var->isImplicit() && var->getName().str() == "self")
      return CaptureKind::Constant;
#endif

    return CaptureKind::Box;
  }
  
  // "Captured" local typealiases require no context.
  // FIXME: Is this true for dependent typealiases?
  if (isa<TypeAliasDecl>(capture))
    return CaptureKind::None;
  
  return CaptureKind::Constant;
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
    IMPL(MetaType, Trivial)
    IMPL(AnyFunction, Reference)
    IMPL(SILFunction, Reference)
    IMPL(Array, AddressOnly) // who knows?
    IMPL(Module, Trivial)

#undef IMPL

    RetTy visitLValueType(CanLValueType type) {
      llvm_unreachable("shouldn't get an l-value type here");
    }

    RetTy visitGenericTypeParamType(CanGenericTypeParamType type) {
      llvm_unreachable("shouldn't get a generic type parameter type here");
    }

    RetTy visitDependentMemberType(CanDependentMemberType type) {
      llvm_unreachable("shouldn't get a dependent member type here");
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
                                 SILValue value, bool deep) const override {
      // Trivial
    }

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value, bool deep) const override {
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
    /// A non-trivial child of this aggregate type.
    class NonTrivialChild {
      /// The index of this child, used to project it out.
      IndexType Index;

      /// The aggregate's type lowering.
      const TypeLowering *Lowering;
    public:
      NonTrivialChild(IndexType index, const TypeLowering &lowering)
        : Index(index), Lowering(&lowering) {}
      const TypeLowering &getLowering() const { return *Lowering; }
      IndexType getIndex() const { return Index; }
    };

  private:
    const Impl &asImpl() const { return static_cast<const Impl&>(*this); }
    Impl &asImpl() { return static_cast<Impl&>(*this); }

    /// The number of non-trivial children of this aggregate.
    /// The data follows the instance data.
    unsigned NumNonTrivial;

  protected:
    LoadableAggTypeLowering(SILType type, ArrayRef<NonTrivialChild> nonTrivial)
        : NonTrivialLoadableTypeLowering(type),
          NumNonTrivial(nonTrivial.size()) {
      memcpy(reinterpret_cast<NonTrivialChild*>(&asImpl()+1),
             nonTrivial.data(),
             NumNonTrivial * sizeof(NonTrivialChild));
    }

  public:
    static const Impl *create(TypeConverter &TC, CanType type,
                              ArrayRef<NonTrivialChild> nonTrivial) {
      size_t bufferSize =
        sizeof(Impl) + sizeof(NonTrivialChild) * nonTrivial.size();
      void *buffer = operator new(bufferSize, TC);
      auto silType = SILType::getPrimitiveObjectType(type);
      return ::new(buffer) Impl(silType, nonTrivial);
    }

    ArrayRef<NonTrivialChild> getNonTrivialChildren() const {
      auto buffer = reinterpret_cast<const NonTrivialChild*>(&asImpl() + 1);
      return ArrayRef<NonTrivialChild>(buffer, NumNonTrivial);
    }

    template <class T>
    void forEachNonTrivialChild(SILBuilder &B, SILLocation loc,
                                SILValue aggValue,
                                const T &operation) const {
      for (auto &child : getNonTrivialChildren()) {
        auto &childLowering = child.getLowering();
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
                                  SILValue aggValue, bool deep) const override {
      // Types with non-trivial copy semantics will need to override this.
      bool hasDifference = false;
      SmallVector<SILValue, 4> copiedChildren;

      forEachNonTrivialChild(B, loc, aggValue,
        [&](SILBuilder &B, SILLocation loc, IndexType index,
            SILValue childValue, const TypeLowering &childLowering) {
          SILValue copiedChildValue =
            childLowering.emitLoweredCopyChildValue(B, loc, childValue, deep);
          hasDifference |= (copiedChildValue != childValue);
          copiedChildren.push_back(copiedChildValue);
        });

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
                                 SILValue aggValue, bool deep) const override {
      forEachNonTrivialChild(B, loc, aggValue,
                             deep ? &TypeLowering::emitLoweredDestroyValueDeep
                                  : &TypeLowering::emitDestroyValue);
    }
  };

  /// A lowering for loadable but non-trivial tuple types.
  class LoadableTupleTypeLowering final
      : public LoadableAggTypeLowering<LoadableTupleTypeLowering, unsigned> {
  public:
    LoadableTupleTypeLowering(SILType type,
                              ArrayRef<NonTrivialChild> nonTrivial)
      : LoadableAggTypeLowering(type, nonTrivial) {}

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
                               ArrayRef<NonTrivialChild> nonTrivial)
      : LoadableAggTypeLowering(type, nonTrivial) {}

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
    
    using SimpleOperationTy = 
      void(*)(SILBuilder &B, SILLocation loc, SILValue value,
              const TypeLowering &valueLowering, SILBasicBlock *dest,
              bool deep);

    /// Emit a value semantics operation for each nontrivial case of the enum.
    void ifNonTrivialElement(SILBuilder &B, SILLocation loc, SILValue value,
                             SimpleOperationTy operation, bool deep) const {
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
        operation(B, loc, bb->getBBArgs()[0], lowering, doneBB, deep);
      }
      
      B.emitBlock(doneBB);
    }
    
  public:
    static const LoadableEnumTypeLowering *create(TypeConverter &TC,
                                       CanType type,
                                       ArrayRef<NonTrivialElement> nonTrivial) {
      void *buffer
        = operator new(sizeof(LoadableEnumTypeLowering)
                         + sizeof(NonTrivialElement) * nonTrivial.size(),
                       TC);
      
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
                                  SILValue value, bool deep) const override {
      ifNonTrivialElement(B, loc, value,
        [](SILBuilder &B, SILLocation loc, SILValue child,
           const TypeLowering &childLowering, SILBasicBlock *dest, bool deep) {
          SILValue copiedChild =
            childLowering.emitLoweredCopyChildValue(B, loc, child, deep);
          B.createBranch(loc, dest, copiedChild);
      }, deep);
      return new (B.getFunction().getModule())
               SILArgument(value.getType(), B.getInsertionBB());
    }
    
    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      B.createDestroyValue(loc, value);
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc,
                                 SILValue value, bool deep) const override {
      ifNonTrivialElement(B, loc, value,
        [](SILBuilder &B, SILLocation loc, SILValue child,
           const TypeLowering &childLowering, SILBasicBlock *dest, bool deep) {
          childLowering.emitLoweredDestroyChildValue(B, loc, child, deep);
          B.createBranch(loc, dest);
      }, deep);
    }
  };

  class LeafLoadableTypeLowering : public NonTrivialLoadableTypeLowering {
  public:
    LeafLoadableTypeLowering(SILType type)
      : NonTrivialLoadableTypeLowering(type) {}

    SILValue emitLoweredCopyValue(SILBuilder &B, SILLocation loc,
                                  SILValue value, bool deep) const override {
      return emitCopyValue(B, loc, value);
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc,
                                 SILValue value, bool deep) const override {
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
                                  SILValue value, bool deep) const override {
      llvm_unreachable("type is not loadable!");
    }

    void emitDestroyValue(SILBuilder &B, SILLocation loc,
                          SILValue value) const override {
      llvm_unreachable("type is not loadable!");
    }

    void emitLoweredDestroyValue(SILBuilder &B, SILLocation loc,
                                 SILValue value, bool deep) const override {
      llvm_unreachable("type is not loadable!");
    }
  };

  /// Build the appropriate TypeLowering subclass for the given type.
  class LowerType :
      public TypeClassifierBase<LowerType, const TypeLowering *> {
    TypeConverter &TC;
  public:
    LowerType(TypeConverter &TC) : TypeClassifierBase(TC.M), TC(TC) {}

    const TypeLowering *handleTrivial(CanType type) {
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC) TrivialTypeLowering(silType);
    }
  
    const TypeLowering *handleReference(CanType type) {
      auto silType = SILType::getPrimitiveObjectType(type);
      return new (TC) ReferenceTypeLowering(silType);
    }

    const TypeLowering *handleAddressOnly(CanType type) {
      auto silType = SILType::getPrimitiveAddressType(type);
      return new (TC) AddressOnlyTypeLowering(silType);
    }

    /// [unowned] is basically like a reference type lowering except
    /// it manipulates unowned reference counts instead of strong.
    const TypeLowering *visitUnownedStorageType(CanUnownedStorageType type) {
      return new (TC) UnownedTypeLowering(
                                      SILType::getPrimitiveObjectType(type));
    }

    const TypeLowering *visitTupleType(CanTupleType tupleType) {
      typedef LoadableTupleTypeLowering::NonTrivialChild NonTrivialChild;
      SmallVector<NonTrivialChild, 8> nonTrivialElts;

      unsigned i = 0;
      for (auto eltType : tupleType.getElementTypes()) {
        auto &lowering = TC.getTypeLowering(eltType);
        if (lowering.isAddressOnly())
          return handleAddressOnly(tupleType);
        if (!lowering.isTrivial()) {
          nonTrivialElts.push_back(NonTrivialChild(i, lowering));
        }
        ++i;
      }

      if (nonTrivialElts.empty())
        return handleTrivial(tupleType);
      return LoadableTupleTypeLowering::create(TC, tupleType, nonTrivialElts);
    }

    const TypeLowering *visitAnyStructType(CanType structType, StructDecl *D) {
      typedef LoadableStructTypeLowering::NonTrivialChild NonTrivialChild;
      SmallVector<NonTrivialChild, 8> nonTrivialFields;

      // For consistency, if it's anywhere resilient, we need to treat the type
      // as resilient in SIL.
      if (TC.isAnywhereResilient(D))
        return handleAddressOnly(structType);

      for (auto field : D->getStoredProperties()) {
        auto origFieldType = AbstractionPattern(field->getType());
        auto substFieldType =
          structType->getTypeOfMember(D->getModuleContext(), field, nullptr);
        auto &lowering = TC.getTypeLowering(origFieldType, substFieldType);
        if (lowering.isAddressOnly())
          return handleAddressOnly(structType);
        if (!lowering.isTrivial()) {
          nonTrivialFields.push_back(NonTrivialChild(field, lowering));
        }
      }

      if (nonTrivialFields.empty())
        return handleTrivial(structType);
      return LoadableStructTypeLowering::create(TC, structType,
                                                nonTrivialFields);
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
      return LoadableEnumTypeLowering::create(TC, enumType, nonTrivialElts);
    }
  };
}

TypeConverter::TypeConverter(SILModule &m)
  : M(m), Context(m.getASTContext()) {
}

TypeConverter::~TypeConverter() {
  // The bump pointer allocator destructor will deallocate but not destroy all
  // our TypeLowerings.
  for (auto &ti : Types) {
    // Destroy only the unique entries.
    CanType srcType = CanType(ti.first.OrigType);
    CanType mappedType = ti.second->getLoweredType().getSwiftRValueType();
    if (srcType == mappedType || isa<LValueType>(srcType))
      ti.second->~TypeLowering();
  }
}

void *TypeLowering::operator new(size_t size, TypeConverter &tc) {
  return tc.TypeLoweringBPA.Allocate(size, alignof(TypeLowering));
}

#ifndef NDEBUG
/// Is this type a lowered type?
static bool isLoweredType(CanType type) {
  if (isa<LValueType>(type))
    return false;
  if (isa<AnyFunctionType>(type))
    return false;
  if (auto tuple = dyn_cast<TupleType>(type)) {
    for (auto elt : tuple.getElementTypes())
      if (!isLoweredType(elt))
        return false;
    return true;
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

    CanType loweredSubstEltType;
    if (auto substLV = dyn_cast<LValueType>(substEltType)) {
      SILType silType = tc.getLoweredType(origType.getLValueObjectType(),
                                          substLV.getObjectType());
      loweredSubstEltType = CanType(LValueType::get(silType.getSwiftRValueType(),
                                                    substLV->getQualifiers(),
                                                    tc.Context));
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
  auto existing = Types.find(key);
  if (existing != Types.end()) {
    assert(existing->second && "reentered getTypeLowering");
    return *existing->second;
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
      Types[key] = &typeInfo;
      return typeInfo;
    }

    // If it didn't, use the standard logic.
    return getTypeLoweringForUncachedLoweredFunctionType(loweredKey);
  }

  assert(uncurryLevel == 0);

  // L-value types are a special case for lowering, because they get
  // completely removed and represented as 'address' SILTypes.
  if (auto substLValueType = dyn_cast<LValueType>(substType)) {
    // Derive SILType for LValueType from the object type.
    CanType substObjectType = substLValueType.getObjectType();
    AbstractionPattern origObjectType = origType.getLValueObjectType();

    SILType loweredType = getLoweredType(origObjectType, substObjectType,
                                         uncurryLevel).getAddressType();

    auto *theInfo = new (*this) TrivialTypeLowering(loweredType);
    Types[key] = theInfo;
    return *theInfo;
  }

  // We need to lower function types within tuples.
  if (auto substTupleType = dyn_cast<TupleType>(substType)) {
    auto loweredType = getLoweredTupleType(*this, origType, substTupleType);

    // If that changed anything, check for a lowering at the lowered
    // type.
    if (loweredType != substType) {
      TypeKey loweredKey = getTypeKey(origType, loweredType, 0);
      auto &lowering = getTypeLoweringForLoweredType(loweredKey);
      Types[key] = &lowering;
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
  auto existing = Types.find(key);
  if (existing != Types.end()) {
    assert(existing->second && "reentered getTypeLowering");
    return *existing->second;
  }

  return getTypeLoweringForUncachedLoweredType(key);
}

const TypeLowering &
TypeConverter::getTypeLoweringForLoweredFunctionType(TypeKey key) {
  assert(isa<AnyFunctionType>(key.SubstType));
  assert(key.UncurryLevel == 0);

  // Check for an existing mapping for the key.
  auto existing = Types.find(key);
  if (existing != Types.end()) {
    assert(existing->second && "reentered type-lowering");
    return *existing->second;
  }

  // Okay, we didn't find one; go ahead and produce a SILFunctionType.
  return getTypeLoweringForUncachedLoweredFunctionType(key);
}

const TypeLowering &TypeConverter::
getTypeLoweringForUncachedLoweredFunctionType(TypeKey key) {
  assert(isa<AnyFunctionType>(key.SubstType));
  assert(key.UncurryLevel == 0);

#ifndef NDEBUG
  // Catch reentrancy bugs.
  Types[key] = nullptr;
#endif

  // Construct the SILFunctionType.
  CanType silFnType = getNativeSILFunctionType(M, key.OrigType,
                                       cast<AnyFunctionType>(key.SubstType));

  // Do a cached lookup under yet another key, just so later lookups
  // using the SILType will find the same TypeLowering object.
  auto loweredKey = getTypeKey(AbstractionPattern(key.OrigType), silFnType, 0);
  auto &lowering = getTypeLoweringForLoweredType(loweredKey);
  Types[key] = &lowering;
  return lowering;
}

/// Do type-lowering for a lowered type which is not already in the cache.
const TypeLowering &
TypeConverter::getTypeLoweringForUncachedLoweredType(TypeKey key) {
  assert(!Types.count(key) && "re-entrant or already cached");
  assert(isLoweredType(key.SubstType) && "didn't lower out l-value type?");

#ifndef NDEBUG
  // Catch reentrancy bugs.
  Types[key] = nullptr;
#endif

  auto *theInfo = LowerType(*this).visit(key.SubstType);
  Types[key] = theInfo;
  return *theInfo;
}

/// Get the type of the 'self' parameter for methods of a type.
CanType TypeConverter::getMethodSelfType(CanType selfType) const {
  if (selfType->hasReferenceSemantics()) {
    return selfType;
  } else {
    return CanType(LValueType::get(selfType, LValueType::Qual::DefaultForType,
                                   Context));
  }
}

/// Get the type of a global variable accessor function, () -> RawPointer.
static CanAnyFunctionType getGlobalAccessorType(CanType varType,
                                                ASTContext &C) {
  return CanFunctionType::get(TupleType::getEmpty(C), C.TheRawPointerType, C);
}

/// Get the type of a default argument generator, () -> T.
static CanAnyFunctionType getDefaultArgGeneratorType(AbstractFunctionDecl *AFD,
                                                     unsigned DefaultArgIndex,
                                                     ASTContext &context) {
  auto resultTy = AFD->getDefaultArg(DefaultArgIndex).second->getCanonicalType();
  assert(resultTy && "Didn't find default argument?");
  return CanFunctionType::get(TupleType::getEmpty(context), resultTy, context);
}

/// Get the type of a destructor function, This -> ().
static CanAnyFunctionType getDestroyingDestructorType(ClassDecl *cd,
                                                      ASTContext &C) {
  auto classType = cd->getDeclaredTypeInContext()->getCanonicalType();

  auto extInfo = AnyFunctionType::ExtInfo(AbstractCC::Method,
                                          /*thin*/ true,
                                          /*noreturn*/ false);

  if (cd->getGenericParams())
    return CanPolymorphicFunctionType::get(classType,
                                           CanType(C.TheObjectPointerType),
                                           cd->getGenericParams(),
                                           extInfo,
                                           C);

  return CanFunctionType::get(classType, CanType(C.TheObjectPointerType),
                              extInfo, C);
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
                                           genericParams, extInfo,
                                           Context);

  }

  SmallVector<TupleTypeElt, 8> inputFields;

  for (ValueDecl *capture : captures) {
    // FIXME: should this be the type-of-rvalue?
    auto captureType = capture->getType()->getRValueType()->getCanonicalType();

    switch (getDeclCaptureKind(capture)) {
    case CaptureKind::None:
      break;
        
    case CaptureKind::Constant:
      // Constants are captured by value.
      assert(!capture->isReferencedAsLValue() &&
             "constant capture is an lvalue?!");
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
    case CaptureKind::Box:
      // Capture the owning ObjectPointer and the address of the value.
      assert(capture->isReferencedAsLValue() &&
             "lvalue capture not an lvalue?!");
      inputFields.push_back(Context.TheObjectPointerType);
      auto lvType = CanLValueType::get(captureType,
                                       LValueType::Qual::DefaultForType,
                                       Context);
      inputFields.push_back(TupleTypeElt(lvType));
      break;
    }
  }
  
  CanType capturedInputs =
    TupleType::get(inputFields, Context)->getCanonicalType();

  auto extInfo = AnyFunctionType::ExtInfo(AbstractCC::Freestanding,
                                          /*thin*/ true,
                                          /*noreturn*/ false);

  if (genericParams)
    return CanPolymorphicFunctionType::get(capturedInputs, funcType,
                                           genericParams, extInfo,
                                           Context);
  
  return CanFunctionType::get(capturedInputs, funcType, extInfo, Context);
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
    return getDestroyingDestructorType(cast<ClassDecl>(vd), Context);
  
  case SILDeclRef::Kind::GlobalAccessor: {
    VarDecl *var = cast<VarDecl>(vd);
    assert(!var->isComputed() && "constant ref to computed global var");
    return getGlobalAccessorType(var->getType()->getCanonicalType(), Context);
  }
  case SILDeclRef::Kind::DefaultArgGenerator: {
    return getDefaultArgGeneratorType(cast<AbstractFunctionDecl>(vd),
                                      c.defaultArgIndex, Context);
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
  CanType substType =
    cast<LValueType>(lvalueType->getCanonicalType()).getObjectType();
  SILType silSubstType = getLoweredType(substType).getAddressType();
  substType = silSubstType.getSwiftRValueType();

  // Fast path: if the unsubstituted type from the variable equals the
  // substituted type from the l-value, there's nothing to do.
  CanType valueType = value->getType()->getCanonicalType();
  if (valueType == substType)
    return silSubstType;

  // Type substitution preserves structural type structure, and the
  // type-of-reference is only different in the outermost structural
  // types.  So, basically, we just need to undo the changes made by
  // getTypeOfReference and then reapply them on the substituted type.

  // The only really significant manipulation there is with [weak] and
  // [unowned].
  if (auto refType = dyn_cast<ReferenceStorageType>(valueType)) {
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

SILType SILType::getFieldType(VarDecl *field, SILModule &M) const {
  assert(field->getDeclContext() == getNominalOrBoundGenericNominal());
  auto origFieldTy = AbstractionPattern(field->getType());
  auto substFieldTy =
    getSwiftRValueType()->getTypeOfMember(field->getModuleContext(),
                                          field, nullptr);
  auto loweredTy = M.Types.getLoweredType(origFieldTy, substFieldTy);
  if (isAddress() || getClassOrBoundGenericClass() != nullptr) {
    return loweredTy.getAddressType();
  } else {
    return loweredTy.getObjectType();
  }
}

SILType SILType::getEnumElementType(EnumElementDecl *elt, SILModule &M) const {
  assert(elt->getDeclContext() == getEnumOrBoundGenericEnum());
  assert(elt->hasArgumentType());
  auto origEltTy = elt->getArgumentType();
  auto substEltTy =
    getSwiftRValueType()->getTypeOfMember(elt->getModuleContext(),
                                          elt, nullptr, origEltTy);
  auto loweredTy =
    M.Types.getLoweredType(AbstractionPattern(origEltTy), substEltTy);
  return SILType(loweredTy.getSwiftRValueType(), getCategory());
}
