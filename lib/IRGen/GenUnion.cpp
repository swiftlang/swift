//===--- GenUnion.cpp - Swift IR Generation For 'union' Types -------------===//
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
//
//  This file implements IR generation for algebraic data types (ADTs,
//  or 'union' types) in Swift.  This includes creating the IR type as
//  well as emitting the basic access operations.
//
//  The current scheme is that all such types with are represented
//  with an initial word indicating the variant, followed by a union
//  of all the possibilities.  This is obviously completely acceptable
//  to everyone and will not benefit from further refinement.
//
//  As a completely unimportant premature optimization, we do emit
//  types with only a single variant as simple structs wrapping that
//  variant.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/Optional.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"

#include "LoadableTypeInfo.h"
#include "GenProto.h"
#include "GenType.h"
#include "GenUnion.h"
#include "IRGenDebugInfo.h"
#include "IRGenModule.h"
#include "ScalarTypeInfo.h"

using namespace swift;
using namespace irgen;

namespace {
  class UnionImplStrategy;
  
  /// An abstract base class for TypeInfo implementations of union types.
  // FIXME: not always loadable or even fixed-size!
  class UnionTypeInfo : public LoadableTypeInfo {
  public:
    // FIXME: Derive spare bits from element layout.
    UnionTypeInfo(llvm::StructType *T, Size S, llvm::BitVector SB,
                  Alignment A, IsPOD_t isPOD)
      : LoadableTypeInfo(T, S, std::move(SB), A, isPOD) {}

    llvm::StructType *getStorageType() const {
      return cast<llvm::StructType>(TypeInfo::getStorageType());
    }

    bool isIndirectArgument(ExplosionKind kind) const override {
      // FIXME!
      return false;
    }

    void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                              Address dest) const override {
      // FIXME!
      initialize(IGF, params, dest);
    }

    virtual void emitInjectionFunctionBody(IRGenFunction &IGF,
                                           UnionElementDecl *elt,
                                           Explosion &params) const = 0;
    
    virtual void emitSwitch(IRGenFunction &IGF,
                            Explosion &value,
                            ArrayRef<std::pair<UnionElementDecl*,
                                     llvm::BasicBlock*>> dests,
                            llvm::BasicBlock *defaultDest) const = 0;
    
    /// Given an incomplete UnionTypeInfo, completes layout of the storage type
    /// and calculates its size and alignment.
    virtual void layoutUnionType(TypeConverter &TC, UnionDecl *theUnion,
                                 UnionImplStrategy &strategy) = 0;
  };

  /// A UnionTypeInfo implementation which has a single case with a payload, and
  /// one or more additional no-payload cases.
  class SinglePayloadUnionTypeInfo : public UnionTypeInfo {
    UnionElementDecl *PayloadElement = nullptr;
    // FIXME - non-fixed payloads
    const FixedTypeInfo *PayloadTypeInfo = nullptr;
    unsigned ExtraTagBitCount = 0;
  public:
    /// FIXME: Spare bits from the payload not exhausted by the extra
    /// inhabitants we used.
    SinglePayloadUnionTypeInfo(llvm::StructType *T, Size S, Alignment A,
                              IsPOD_t isPOD)
      : UnionTypeInfo(T, S, {}, A, isPOD) {}
    
    void getSchema(ExplosionSchema &schema) const {
      schema.add(ExplosionSchema::Element::forScalar(
                                         getStorageType()->getElementType(0)));
      if (ExtraTagBitCount > 0)
        schema.add(ExplosionSchema::Element::forScalar(
                                         getStorageType()->getElementType(1)));
    }
    
    unsigned getExplosionSize(ExplosionKind kind) const {
      return ExtraTagBitCount > 0 ? 2 : 1;
    }
    
    void load(IRGenFunction &IGF, Address addr, Explosion &e) const {
      // FIXME
    }
    
    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {
      // FIXME
    }
    
    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {
      // FIXME
    }
    
    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src) const {
      // FIXME
    }
    
    void assignWithTake(IRGenFunction &IGF, Address dest, Address src) const {
      // FIXME
    }
    
    void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {
      // FIXME
    }
    
    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      // FIXME
    }
    
    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      // FIXME
    }
    
    void destroy(IRGenFunction &IGF, Address addr) const {
      // FIXME
    }
    
    llvm::Value *packUnionPayload(IRGenFunction &IGF, Explosion &src,
                                  unsigned bitWidth) const override {
      assert(isComplete());
      
      PackUnionPayload pack(IGF, bitWidth);
      // Pack payload.
      pack.add(src.claimNext());
      
      // Pack tag bits, if any.
      if (ExtraTagBitCount > 0) {
        unsigned extraTagOffset
          = PayloadTypeInfo->getFixedSize().getValueInBits();
        
        pack.addAtOffset(src.claimNext(), extraTagOffset);
      }
      
      return pack.get();
    }
    
    void unpackUnionPayload(IRGenFunction &IGF, llvm::Value *outerPayload,
                            Explosion &dest) const override {
      assert(isComplete());
      
      UnpackUnionPayload unpack(IGF, outerPayload);
      
      // Unpack our inner payload.
      dest.add(unpack.claim(getStorageType()->getElementType(0)));
      
      // Unpack our extra tag bits, if any.
      if (ExtraTagBitCount > 0) {
        unsigned extraTagOffset
          = PayloadTypeInfo->getFixedSize().getValueInBits();
        
        dest.add(unpack.claimAtOffset(getStorageType()->getElementType(1),
                                      extraTagOffset));
      }
    }
    
    void emitSwitch(IRGenFunction &IGF,
                    Explosion &value,
                    ArrayRef<std::pair<UnionElementDecl*,
                                       llvm::BasicBlock*>> dests,
                    llvm::BasicBlock *defaultDest) const override {
      // FIXME
      IGF.Builder.CreateUnreachable();
    }
    
  private:
    // Get the index of a union element among the non-payload cases.
    unsigned getSimpleElementTagIndex(UnionElementDecl *elt) const {
      assert(elt != PayloadElement && "is payload element");
      unsigned i = 0;
      // FIXME: linear search
      for (auto *unionElt : elt->getParentUnion()->getAllElements()) {
        if (elt == unionElt)
          return i;
        if (unionElt != PayloadElement)
          ++i;
      }
      llvm_unreachable("element was not a member of union");
    }
    
    void addExtraTagBitValue(IRGenFunction &IGF,
                             Explosion &out, unsigned value) const {
      if (ExtraTagBitCount > 0) {
        llvm::Value *zeroTagBits
          = llvm::ConstantInt::get(IGF.IGM.getLLVMContext(),
                                   APInt(ExtraTagBitCount, value));
        out.add(zeroTagBits);
        return;
      }
      assert(value == 0 && "setting non-zero extra tag value with no tag bits");
    }
    
  public:
    void emitInjectionFunctionBody(IRGenFunction &IGF,
                                   UnionElementDecl *elt,
                                   Explosion &params) const {
      // The payload case gets its native representation. If there are extra
      // tag bits, set them to zero.
      unsigned payloadSize = PayloadTypeInfo->getFixedSize().getValueInBits();
      
      if (elt == PayloadElement) {
        Explosion unionExplosion(ExplosionKind::Minimal);
        
        // FIXME
        auto &loadablePayloadTI = cast<LoadableTypeInfo>(*PayloadTypeInfo);
        llvm::Value *payload = loadablePayloadTI.packUnionPayload(IGF, params,
                                                                  payloadSize);
        unionExplosion.add(payload);
        
        addExtraTagBitValue(IGF, unionExplosion, 0);
        IGF.emitScalarReturn(unionExplosion);
        return;
      }
      
      // Non-payload cases use extra inhabitants, if any, or are discriminated
      // by setting the tag bits.
      unsigned tagIndex = getSimpleElementTagIndex(elt);
      unsigned numExtraInhabitants
        = PayloadTypeInfo->getFixedExtraInhabitantCount();
      llvm::Value *payload;
      unsigned extraTagValue;
      if (tagIndex < numExtraInhabitants) {
        payload = PayloadTypeInfo->getFixedExtraInhabitantValue(IGF.IGM,
                                                                payloadSize,
                                                                tagIndex);
        extraTagValue = 0;
      } else {
        tagIndex -= numExtraInhabitants;
        
        // Factor the extra tag value from the payload value.
        unsigned payloadValue;
        if (payloadSize >= 32) {
          payloadValue = tagIndex;
          extraTagValue = 1U;
        } else {
          payloadValue = tagIndex & ((1U << payloadSize) - 1U);
          extraTagValue = (tagIndex >> payloadSize) + 1U;
        }
        
        payload = llvm::ConstantInt::get(IGF.IGM.getLLVMContext(),
                                         APInt(payloadSize, payloadValue));
      }

      Explosion unionExplosion(ExplosionKind::Minimal);
      unionExplosion.add(payload);
      addExtraTagBitValue(IGF, unionExplosion, extraTagValue);
      IGF.emitScalarReturn(unionExplosion);
    }
    
    void layoutUnionType(TypeConverter &TC, UnionDecl *theUnion,
                         UnionImplStrategy &strategy) override;
  };

  /// A UnionTypeInfo implementation which has multiple payloads.
  class MultiPayloadUnionTypeInfo : public UnionTypeInfo {
    bool HasExtraTagBits = false;
    
    // The spare bits shared by all payloads, if any.
    // Invariant: The size of the bit
    // vector is the size of the payload in bits, rounded up to a byte boundary.
    llvm::BitVector CommonSpareBits;
  public:
    /// FIXME: Spare bits common to aggregate elements and not used for tags.
    MultiPayloadUnionTypeInfo(llvm::StructType *T, Size S, Alignment A,
                           IsPOD_t isPOD)
      : UnionTypeInfo(T, S, {}, A, isPOD) {}

    void getSchema(ExplosionSchema &schema) const {
      schema.add(ExplosionSchema::Element::forAggregate(getStorageType(),
                                                        getFixedAlignment()));
    }

    unsigned getExplosionSize(ExplosionKind kind) const {
      return HasExtraTagBits ? 2 : 1;
    }

    void load(IRGenFunction &IGF, Address addr, Explosion &e) const {
      // FIXME
    }

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {
      // FIXME
    }

    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {
      // FIXME
    }

    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src) const {
      // FIXME
    }

    void assignWithTake(IRGenFunction &IGF, Address dest, Address src) const {
      // FIXME
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {
      // FIXME
    }

    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      // FIXME
    }

    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      // FIXME
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      // FIXME
    }
    
    void emitSwitch(IRGenFunction &IGF,
                    Explosion &value,
                    ArrayRef<std::pair<UnionElementDecl*,
                                       llvm::BasicBlock*>> dests,
                    llvm::BasicBlock *defaultDest) const override {
      // FIXME
      IGF.Builder.CreateUnreachable();
    }
    
    llvm::Value *packUnionPayload(IRGenFunction &IGF, Explosion &src,
                                  unsigned bitWidth) const override {
      assert(isComplete());
      
      PackUnionPayload pack(IGF, bitWidth);
      // Pack the payload.
      pack.add(src.claimNext());
      // Pack the extra bits, if any.
      if (HasExtraTagBits) {
        pack.addAtOffset(src.claimNext(), CommonSpareBits.size());
      }
      return pack.get();
    }
    
    void unpackUnionPayload(IRGenFunction &IGF, llvm::Value *outerPayload,
                            Explosion &dest) const override {
      assert(isComplete());

      UnpackUnionPayload unpack(IGF, outerPayload);
      // Unpack the payload.
      dest.add(unpack.claim(getStorageType()->getElementType(0)));
      // Unpack the extra bits, if any.
      if (HasExtraTagBits) {
        dest.add(unpack.claimAtOffset(getStorageType()->getElementType(1),
                                      CommonSpareBits.size()));
      }
    }
    
    void emitInjectionFunctionBody(IRGenFunction &IGF,
                                   UnionElementDecl *elt,
                                   Explosion &params) const {
      // FIXME
      params.markClaimed(params.size());
      IGF.Builder.CreateRetVoid();
    }
    
    void layoutUnionType(TypeConverter &TC, UnionDecl *theUnion,
                         UnionImplStrategy &strategy) override;
  };

  /// A UnionTypeInfo implementation for singleton unions.
  class SingletonUnionTypeInfo : public UnionTypeInfo {
  public:
    static Address getSingletonAddress(IRGenFunction &IGF, Address addr) {
      llvm::Value *singletonAddr =
        IGF.Builder.CreateStructGEP(addr.getAddress(), 0);
      return Address(singletonAddr, addr.getAlignment());
    }

    /// The type info of the singleton member, or null if it carries no data.
    const LoadableTypeInfo *Singleton;

    SingletonUnionTypeInfo(llvm::StructType *T, Size S, Alignment A,
                           IsPOD_t isPOD)
      : UnionTypeInfo(T, S, {}, A, isPOD), Singleton(nullptr) {}

    void getSchema(ExplosionSchema &schema) const {
      assert(isComplete());
      if (Singleton) Singleton->getSchema(schema);
    }

    unsigned getExplosionSize(ExplosionKind kind) const {
      assert(isComplete());
      if (!Singleton) return 0;
      return Singleton->getExplosionSize(kind);
    }

    void load(IRGenFunction &IGF, Address addr, Explosion &e) const {
      if (!Singleton) return;
      Singleton->load(IGF, getSingletonAddress(IGF, addr), e);
    }

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {
      if (!Singleton) return;
      Singleton->loadAsTake(IGF, getSingletonAddress(IGF, addr), e);
    }

    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {
      if (!Singleton) return;
      Singleton->assign(IGF, e, getSingletonAddress(IGF, addr));
    }

    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src) const {
      if (!Singleton) return;
      dest = getSingletonAddress(IGF, dest);
      src = getSingletonAddress(IGF, src);
      Singleton->assignWithCopy(IGF, dest, src);
    }

    void assignWithTake(IRGenFunction &IGF, Address dest, Address src) const {
      if (!Singleton) return;
      dest = getSingletonAddress(IGF, dest);
      src = getSingletonAddress(IGF, src);
      Singleton->assignWithTake(IGF, dest, src);
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {
      if (!Singleton) return;
      Singleton->initialize(IGF, e, getSingletonAddress(IGF, addr));
    }

    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      if (Singleton) Singleton->reexplode(IGF, src, dest);
    }

    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      if (Singleton) Singleton->copy(IGF, src, dest);
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      if (Singleton && !isPOD(ResilienceScope::Local))
        Singleton->destroy(IGF, getSingletonAddress(IGF, addr));
    }
    
    llvm::Value *packUnionPayload(IRGenFunction &IGF,
                                  Explosion &in,
                                  unsigned bitWidth) const override {
      if (Singleton)
        return Singleton->packUnionPayload(IGF, in, bitWidth);
      return PackUnionPayload::getEmpty(IGF.IGM, bitWidth);
    }
    
    void unpackUnionPayload(IRGenFunction &IGF,
                            llvm::Value *payload,
                            Explosion &dest) const override {
      if (!Singleton) return;
      Singleton->unpackUnionPayload(IGF, payload, dest);
    }
    
    void emitSwitch(IRGenFunction &IGF,
                    Explosion &value,
                    ArrayRef<std::pair<UnionElementDecl*,
                             llvm::BasicBlock*>> dests,
                    llvm::BasicBlock *defaultDest) const override {
      // No dispatch necessary. Branch straight to the destination.
      // FIXME: Bind argument to the value itself.
      value.claimAll();
      
      assert(dests.size() == 1 && "switch table mismatch");
      IGF.Builder.CreateBr(dests[0].second);
    }

    void emitInjectionFunctionBody(IRGenFunction &IGF,
                                   UnionElementDecl *elt,
                                   Explosion &params) const {
      // If this union carries no data, the function must take no
      // arguments and return void.
      if (!Singleton) {
        IGF.Builder.CreateRetVoid();
        return;
      }

      // Otherwise, package up the result.
      ExplosionSchema schema(params.getKind());
      Singleton->getSchema(schema);
      if (schema.requiresIndirectResult()) {
        Address returnSlot =
          Singleton->getAddressForPointer(params.claimNext());
        initialize(IGF, params, returnSlot);
        IGF.Builder.CreateRetVoid();
      } else {
        IGF.emitScalarReturn(params);
      }
    }
    
    void layoutUnionType(TypeConverter &TC, UnionDecl *theUnion,
                         UnionImplStrategy &) override;
  };

  /// A UnionTypeInfo implementation for unions with no payload.
  class NoPayloadUnionTypeInfo :
    public PODSingleScalarTypeInfo<NoPayloadUnionTypeInfo,UnionTypeInfo> {
  public:
    NoPayloadUnionTypeInfo(llvm::StructType *T, Size S, Alignment A)
      : PODSingleScalarTypeInfo(T, S, {}, A) {}

    llvm::Type *getScalarType() const {
      assert(isComplete());
      return getDiscriminatorType();
    }

    static Address projectScalar(IRGenFunction &IGF, Address addr) {
      return IGF.Builder.CreateStructGEP(addr, 0, Size(0));
    }
      
    llvm::IntegerType *getDiscriminatorType() const {
      llvm::StructType *Struct = getStorageType();
      return cast<llvm::IntegerType>(Struct->getElementType(0));
    }
    
    /// Map the given element to the appropriate value in the
    /// discriminator type.
    llvm::ConstantInt *getDiscriminatorIndex(UnionElementDecl *target) const {
      // FIXME: using a linear search here is fairly ridiculous.
      unsigned index = 0;
      for (auto elt : target->getParentUnion()->getAllElements()) {
        if (elt == target) break;
        index++;
      }
      return llvm::ConstantInt::get(getDiscriminatorType(), index);
    }
    
    void emitSwitch(IRGenFunction &IGF,
                    Explosion &value,
                    ArrayRef<std::pair<UnionElementDecl*,
                             llvm::BasicBlock*>> dests,
                    llvm::BasicBlock *defaultDest) const override {
      llvm::Value *discriminator = value.claimNext();
      
      // Create an unreachable block for the default if the original SIL
      // instruction had none.
      bool unreachableDefault = false;
      if (!defaultDest) {
        unreachableDefault = true;
        defaultDest = llvm::BasicBlock::Create(IGF.IGM.getLLVMContext());
      }
      
      auto *i = IGF.Builder.CreateSwitch(discriminator, defaultDest,
                                         dests.size());
      for (auto &dest : dests)
        i->addCase(getDiscriminatorIndex(dest.first), dest.second);
      
      if (unreachableDefault) {
        IGF.Builder.emitBlock(defaultDest);
        IGF.Builder.CreateUnreachable();
      }
    }

    void emitInjectionFunctionBody(IRGenFunction &IGF,
                                   UnionElementDecl *elt,
                                   Explosion &params) const {
      IGF.Builder.CreateRet(getDiscriminatorIndex(elt));
    }
    
    void layoutUnionType(TypeConverter &TC, UnionDecl *theUnion,
                         UnionImplStrategy &strategy) override;
  };

  bool isObviouslyEmptyType(CanType type) {
    if (auto tuple = dyn_cast<TupleType>(type)) {
      for (auto eltType : tuple.getElementTypes())
        if (!isObviouslyEmptyType(eltType))
          return false;
      return true;
    }
    // Add more cases here?  Meh.
    return false;
  }

  /// An implementation strategy for a union.
  class UnionImplStrategy {
  public:
    enum Kind {
      /// A "union" with a single case.
      Singleton,
      /// An enum-like union whose cases all have empty payloads.
      NoPayload,
      /// A union which is enum-like except for a single case with payload.
      SinglePayload,
      /// A fully general union with more than one case with payload.
      MultiPayload,
    };

  private:
    unsigned NumElements;
    Kind TheKind;
    std::vector<UnionElementDecl *> ElementsWithPayload;

  public:
    explicit UnionImplStrategy(UnionDecl *theUnion) {
      NumElements = 0;

      for (auto elt : theUnion->getAllElements()) {
        NumElements++;

        // Compute whether this gives us an apparent payload.
        Type argType = elt->getArgumentType();
        if (!argType.isNull() &&
            !isObviouslyEmptyType(argType->getCanonicalType())) {
          ElementsWithPayload.push_back(elt);
        }
      }

      assert(NumElements != 0);
      if (NumElements == 1) {
        TheKind = Singleton;
      } else if (ElementsWithPayload.size() > 1) {
        TheKind = MultiPayload;
      } else if (ElementsWithPayload.size() == 1) {
        TheKind = SinglePayload;
      } else {
        TheKind = NoPayload;
      }
    }

    Kind getKind() const { return TheKind; }
    unsigned getNumElements() const { return NumElements; }
    unsigned getNumElementsWithPayload() const {
      return ElementsWithPayload.size();
    }
    
    UnionElementDecl *getOnlyElementWithPayload() const {
      assert(TheKind == SinglePayload &&
             "not a single-payload union");
      assert(ElementsWithPayload.size() == 1 &&
             "single-payload union has multiple payloads?!");
      return ElementsWithPayload[0];
    }
    
    ArrayRef<UnionElementDecl*> getElementsWithPayload() const {
      return ElementsWithPayload;
    }
    
    /// Create a forward declaration for the union.
    UnionTypeInfo *create(llvm::StructType *convertedStruct) const {
      switch (getKind()) {
      case Singleton:
        return new SingletonUnionTypeInfo(convertedStruct,
                                          Size(0), Alignment(0), IsPOD);
      case NoPayload:
        return new NoPayloadUnionTypeInfo(convertedStruct,
                                          Size(0), Alignment(0));
      
      case SinglePayload:
        return new SinglePayloadUnionTypeInfo(convertedStruct,
                                              Size(0), Alignment(0), IsPOD);
      
      case MultiPayload:
        return new MultiPayloadUnionTypeInfo(convertedStruct,
                                             Size(0), Alignment(0), IsPOD);
      }
    }
  };
  
  void SingletonUnionTypeInfo::layoutUnionType(TypeConverter &TC,
                                               UnionDecl *theUnion,
                                               UnionImplStrategy &) {
    Type eltType =
      theUnion->getAllElements().front()->getArgumentType();
    
    llvm::Type *storageType;
    if (eltType.isNull()) {
      storageType = TC.IGM.Int8Ty;
      completeFixed(Size(0), Alignment(1));
      Singleton = nullptr;
    } else {
      const TypeInfo &eltTI
        = TC.getCompleteTypeInfo(eltType->getCanonicalType());
      
      auto &fixedEltTI = cast<FixedTypeInfo>(eltTI); // FIXME
      storageType = eltTI.StorageType;
      completeFixed(fixedEltTI.getFixedSize(),
                    fixedEltTI.getFixedAlignment());
      Singleton = cast<LoadableTypeInfo>(&eltTI); // FIXME
      setPOD(eltTI.isPOD(ResilienceScope::Local));
    }
    
    llvm::Type *body[] = { storageType };
    getStorageType()->setBody(body);
  }
  
  void NoPayloadUnionTypeInfo::layoutUnionType(TypeConverter &TC,
                                               UnionDecl *theUnion,
                                               UnionImplStrategy &strategy) {
    // Since there are no payloads, we need just enough bits to hold a
    // discriminator.
    assert(strategy.getNumElements() > 1 &&
           "singletons should use SingletonUnionTypeInfo");
    unsigned tagBits = llvm::Log2_32(strategy.getNumElements() - 1) + 1;
    auto tagTy = llvm::IntegerType::get(TC.IGM.getLLVMContext(), tagBits);
    // Round the physical size up to the next power of two.
    unsigned tagBytes = (tagBits + 7U)/8U;
    if (!llvm::isPowerOf2_32(tagBytes))
      tagBytes = llvm::NextPowerOf2(tagBytes);
    completeFixed(Size(tagBytes), Alignment(tagBytes));
    
    llvm::Type *body[] = { tagTy };
    getStorageType()->setBody(body);
  }
  
  static void setTaggedUnionBody(IRGenModule &IGM,
                                 llvm::StructType *bodyStruct,
                                 unsigned payloadBits, unsigned extraTagBits) {

    auto payloadUnionTy = llvm::IntegerType::get(IGM.getLLVMContext(),
                                                 payloadBits);

    if (extraTagBits > 0) {
      auto extraTagTy = llvm::IntegerType::get(IGM.getLLVMContext(),
                                               extraTagBits);
      llvm::Type *body[] = { payloadUnionTy, extraTagTy };
      bodyStruct->setBody(body);
    } else {
      llvm::Type *body[] = { payloadUnionTy };
      bodyStruct->setBody(body);
    }
  }
  
  void SinglePayloadUnionTypeInfo::layoutUnionType(TypeConverter &TC,
                                                   UnionDecl *theUnion,
                                                   UnionImplStrategy &strategy){
    // See whether the payload case's type has extra inhabitants.
    unsigned fixedExtraInhabitants = 0;
    unsigned numTags = strategy.getNumElements() - 1;

    PayloadElement = strategy.getOnlyElementWithPayload();
    auto payloadTy = PayloadElement->getArgumentType()
      ->getCanonicalType();
    
    auto &payloadTI = TC.getCompleteTypeInfo(payloadTy);

    PayloadTypeInfo = cast<FixedTypeInfo>(&payloadTI); // FIXME
    fixedExtraInhabitants = PayloadTypeInfo->getFixedExtraInhabitantCount();
    
    // Determine how many tag bits we need. Given N extra inhabitants, we
    // represent the first N tags using those inhabitants. For additional tags,
    // we use discriminator bit(s) to inhabit the full bit size of the payload.
    unsigned tagsWithoutInhabitants = numTags <= fixedExtraInhabitants
      ? 0 : numTags - fixedExtraInhabitants;
    
    if (tagsWithoutInhabitants == 0) {
      ExtraTagBitCount = 0;
    // If the payload size is greater than 32 bits, the calculation would
    // overflow, but one tag bit should suffice. if you have more than 2^32
    // union discriminators you have other problems.
    } else if (PayloadTypeInfo->getFixedSize().getValue() >= 4) {
      ExtraTagBitCount = 1;
    } else {
      unsigned tagsPerTagBitValue =
        1 << PayloadTypeInfo->getFixedSize().getValueInBits();
      unsigned tagBitValues
        = (tagsWithoutInhabitants + (tagsPerTagBitValue-1))/tagsPerTagBitValue;
      ExtraTagBitCount = llvm::Log2_32(tagBitValues-1) + 1;
    }

    // Create the body type.
    setTaggedUnionBody(TC.IGM, getStorageType(),
                       PayloadTypeInfo->getFixedSize().getValueInBits(),
                       ExtraTagBitCount);
    
    // The union has the alignment of the payload. The size includes the added
    // tag bits.
    auto sizeWithTag = PayloadTypeInfo->getFixedSize()
      .roundUpToAlignment(PayloadTypeInfo->getFixedAlignment())
      .getValue();
    sizeWithTag += (ExtraTagBitCount+7U)/8U;
    
    completeFixed(Size(sizeWithTag),
                  PayloadTypeInfo->getFixedAlignment());
  }
  
  void MultiPayloadUnionTypeInfo::layoutUnionType(TypeConverter &TC,
                                                  UnionDecl *theUnion,
                                                  UnionImplStrategy &strategy) {
    // We need tags for each of the payload types, which we may be able to form
    // using spare bits, plus a minimal number of tags with which we can
    // represent the empty cases.
    unsigned numPayloadTags = strategy.getNumElementsWithPayload();
    unsigned numEmptyElements = strategy.getNumElements() - numPayloadTags;
    
    // See if the payload types have any spare bits in common.
    // At the end of the loop CommonSpareBits.size() will be the size (in bits)
    // of the largest payload.
    CommonSpareBits = {};
    Alignment worstAlignment(1);
    for (auto *elt : strategy.getElementsWithPayload()) {
      auto payloadTy = elt->getArgumentType()->getCanonicalType();
      auto &payloadTI = TC.getCompleteTypeInfo(payloadTy);
      auto &fixedPayloadTI = cast<FixedTypeInfo>(payloadTI); // FIXME
      fixedPayloadTI.applyFixedSpareBitsMask(CommonSpareBits);
      if (fixedPayloadTI.getFixedAlignment() > worstAlignment)
        worstAlignment = fixedPayloadTI.getFixedAlignment();
    }
    
    unsigned commonSpareBitCount = CommonSpareBits.count();
    unsigned usedBitCount = CommonSpareBits.size() - commonSpareBitCount;
    
    // We can store tags for the empty elements using the inhabited bits with
    // their own tag(s).
    unsigned numEmptyElementTags;
    if (usedBitCount >= 32) {
      numEmptyElementTags = 1;
    } else {
      unsigned emptyElementsPerTag = 1 << usedBitCount;
      numEmptyElementTags
        = (numEmptyElements + (emptyElementsPerTag-1))/emptyElementsPerTag;
    }
    
    unsigned numTags = numPayloadTags + numEmptyElementTags;
    unsigned numTagBits = llvm::Log2_32(numTags-1) + 1;
    unsigned extraTagBits = numTagBits <= commonSpareBitCount
      ? 0 : numTagBits - commonSpareBitCount;
    
    // Create the type. We need enough bits to store the largest payload plus
    // extra tag bits we need.
    setTaggedUnionBody(TC.IGM, getStorageType(),
                       CommonSpareBits.size(),
                       extraTagBits);
    
    // The union has the worst alignment of its payloads. The size includes the
    // added tag bits.
    auto sizeWithTag = Size((CommonSpareBits.size() + 7U)/8U)
      .roundUpToAlignment(worstAlignment)
      .getValue();
    sizeWithTag += (extraTagBits+7U)/8U;
    
    HasExtraTagBits = extraTagBits > 0;
    completeFixed(Size(sizeWithTag), worstAlignment);
  }
}

const TypeInfo *TypeConverter::convertUnionType(UnionDecl *theUnion) {
  llvm::StructType *convertedStruct = IGM.createNominalType(theUnion);

  // Create a forward declaration for that type.
  auto typeCacheKey = theUnion->getDeclaredType().getPointer();
  addForwardDecl(typeCacheKey, convertedStruct);

  // Compute the implementation strategy.
  UnionImplStrategy strategy(theUnion);

  // Create the TI as a forward declaration and map it in the table.
  UnionTypeInfo *convertedTI = strategy.create(convertedStruct);

  convertedTI->layoutUnionType(*this, theUnion, strategy);
  return convertedTI;
}

/// Emit the injection function for the given element.
static void emitInjectionFunction(IRGenModule &IGM,
                                  llvm::Function *fn,
                                  UnionElementDecl *elt) {
  ExplosionKind explosionKind = ExplosionKind::Minimal;
  IRGenFunction IGF(IGM, explosionKind, fn);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(IGF, fn);

  Explosion explosion = IGF.collectParameters();
  UnionDecl *ood = cast<UnionDecl>(elt->getDeclContext());
  if (ood->getGenericParamsOfContext()) {
    auto polyFn =
      cast<PolymorphicFunctionType>(elt->getType()->getCanonicalType());
    emitPolymorphicParameters(IGF, polyFn, explosion);
  }
  const UnionTypeInfo &unionTI =
    IGM.getFragileTypeInfo(ood->getDeclaredTypeInContext()).as<UnionTypeInfo>();
  unionTI.emitInjectionFunctionBody(IGF, elt, explosion);
}

/// emitUnionDecl - Emit all the declarations associated with this union type.
void IRGenModule::emitUnionDecl(UnionDecl *theUnion) {
  // FIXME: This is mostly copy-paste from emitExtension;
  // figure out how to refactor! 
  for (Decl *member : theUnion->getMembers()) {
    switch (member->getKind()) {
    case DeclKind::Import:
    case DeclKind::TopLevelCode:
    case DeclKind::Protocol:
    case DeclKind::Extension:
    case DeclKind::Destructor:
    case DeclKind::InfixOperator:
    case DeclKind::PrefixOperator:
    case DeclKind::PostfixOperator:
      llvm_unreachable("decl not allowed in struct!");

    // We can't have meaningful initializers for variables; these just show
    // up as part of parsing properties.
    case DeclKind::PatternBinding:
      continue;

    case DeclKind::Subscript:
      // Getter/setter will be handled separately.
      continue;
    case DeclKind::TypeAlias:
      continue;
    case DeclKind::Union:
      emitUnionDecl(cast<UnionDecl>(member));
      continue;
    case DeclKind::Struct:
      emitStructDecl(cast<StructDecl>(member));
      continue;
    case DeclKind::Class:
      emitClassDecl(cast<ClassDecl>(member));
      continue;
    case DeclKind::Var:
      if (cast<VarDecl>(member)->isProperty())
        // Getter/setter will be handled separately.
        continue;
      // FIXME: Will need an implementation here for resilience
      continue;
    case DeclKind::Func:
      emitLocalDecls(cast<FuncDecl>(member));
      continue;
    case DeclKind::Constructor:
      emitLocalDecls(cast<ConstructorDecl>(member));
      continue;
    case DeclKind::UnionElement: {
      UnionElementDecl *elt = cast<UnionElementDecl>(member);
      llvm::Function *fn = getAddrOfInjectionFunction(elt);
      emitInjectionFunction(*this, fn, elt);
      continue;
    }
    }
    llvm_unreachable("bad extension member kind");
  }
}

// FIXME: PackUnionPayload and UnpackUnionPayload need to be endian-aware.

PackUnionPayload::PackUnionPayload(IRGenFunction &IGF, unsigned bitSize)
  : IGF(IGF), bitSize(bitSize)
{}

void PackUnionPayload::add(llvm::Value *v) {
  // First, bitcast to an integer type.
  if (!isa<llvm::IntegerType>(v->getType())) {
    unsigned bitSize = IGF.IGM.DataLayout.getTypeSizeInBits(v->getType());
    auto intTy = llvm::IntegerType::get(IGF.IGM.getLLVMContext(), bitSize);
    v = IGF.Builder.CreateBitCast(v, intTy);
  }
  auto fromTy = cast<llvm::IntegerType>(v->getType());
  
  // If this was the first added value, use it to start our packed value.
  if (!packedValue) {
    // Zero-extend the integer value out to the value size.
    // FIXME: On big-endian, shift out to the value size.
    if (fromTy->getBitWidth() < bitSize) {
      auto toTy = llvm::IntegerType::get(IGF.IGM.getLLVMContext(), bitSize);
      v = IGF.Builder.CreateZExt(v, toTy);
    }
    if (packedBits != 0)
      v = IGF.Builder.CreateShl(v, packedBits);
    packedBits += fromTy->getBitWidth();
    packedValue = v;
    return;
  }
  
  // Otherwise, shift and bitor the value into the existing value.
  v = IGF.Builder.CreateZExt(v, packedValue->getType());
  v = IGF.Builder.CreateShl(v, packedBits);
  packedBits += fromTy->getBitWidth();
  packedValue = IGF.Builder.CreateOr(packedValue, v);
}

void PackUnionPayload::addAtOffset(llvm::Value *v, unsigned bitOffset) {
  packedBits = bitOffset;
  add(v);
}

llvm::Value *PackUnionPayload::get() {
  if (!packedValue)
    packedValue = getEmpty(IGF.IGM, bitSize);
  return packedValue;
}

llvm::Value *PackUnionPayload::getEmpty(IRGenModule &IGM, unsigned bitSize) {
  return llvm::ConstantInt::get(IGM.getLLVMContext(), APInt(bitSize, 0));
}

UnpackUnionPayload::UnpackUnionPayload(IRGenFunction &IGF,
                                       llvm::Value *packedValue)
  : IGF(IGF), packedValue(packedValue)
{}

llvm::Value *UnpackUnionPayload::claim(llvm::Type *ty) {
  // Mask out the bits for the value.
  unsigned bitSize = IGF.IGM.DataLayout.getTypeSizeInBits(ty);
  auto bitTy = llvm::IntegerType::get(IGF.IGM.getLLVMContext(), bitSize);
  llvm::Value *unpacked = IGF.Builder.CreateLShr(packedValue, unpackedBits);
  unpacked = IGF.Builder.CreateTrunc(unpacked, bitTy);

  unpackedBits += bitSize;

  // Bitcast to the destination type.
  return IGF.Builder.CreateBitCast(unpacked, ty);
}

llvm::Value *UnpackUnionPayload::claimAtOffset(llvm::Type *ty,
                                               unsigned bitOffset) {
  unpackedBits = bitOffset;
  return claim(ty);
}

void irgen::emitSwitchLoadableUnionDispatch(IRGenFunction &IGF,
                                  SILType unionTy,
                                  Explosion &unionValue,
                                  ArrayRef<std::pair<UnionElementDecl *,
                                                     llvm::BasicBlock *>> dests,
                                  llvm::BasicBlock *defaultDest) {
  assert(unionTy.getSwiftRValueType()->getUnionOrBoundGenericUnion()
         && "not of a union type");
  auto &unionTI
    = static_cast<const UnionTypeInfo &>(IGF.getFragileTypeInfo(unionTy));
  unionTI.emitSwitch(IGF, unionValue, dests, defaultDest);
}
