//===--- GenEnum.cpp - Swift IR Generation For 'enum' Types -------------===//
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
//  or 'enum' types) in Swift.  This includes creating the IR type as
//  well as emitting the basic access operations.
//
//  The current scheme is that all such types with are represented
//  with an initial word indicating the variant, followed by an enum
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
#include "swift/IRGen/Options.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"

#include "IRGenModule.h"
#include "LoadableTypeInfo.h"
#include "NonFixedTypeInfo.h"
#include "GenMeta.h"
#include "GenProto.h"
#include "GenType.h"
#include "GenEnum.h"
#include "IRGenDebugInfo.h"
#include "ScalarTypeInfo.h"

using namespace swift;
using namespace irgen;

/// An implementation strategy for an enum, which handles how the enum is
/// laid out and how to construct and destructure values inside the enum.
class irgen::EnumImplStrategy {
public:
  struct Element {
    EnumElementDecl *decl;
    const TypeInfo *ti;
  };
  
  enum TypeInfoKind {
    Opaque,   ///< The enum has a NonFixedTypeInfo.
    Fixed,    ///< The enum has a FixedTypeInfo.
    Loadable, ///< The enum has a LoadableTypeInfo.
  };
  
protected:
  std::vector<Element> ElementsWithPayload;
  std::vector<Element> ElementsWithRecursivePayload;
  std::vector<Element> ElementsWithNoPayload;
  const TypeInfo *TI = nullptr;
  TypeInfoKind TIK;
  unsigned NumElements;
  
  EnumImplStrategy(TypeInfoKind tik,
                    unsigned NumElements,
                    std::vector<Element> &&ElementsWithPayload,
                    std::vector<Element> &&ElementsWithRecursivePayload,
                    std::vector<Element> &&ElementsWithNoPayload)
  : ElementsWithPayload(std::move(ElementsWithPayload)),
    ElementsWithRecursivePayload(std::move(ElementsWithRecursivePayload)),
    ElementsWithNoPayload(std::move(ElementsWithNoPayload)),
    TIK(tik),
    NumElements(NumElements)
  {}
  
  /// Save the TypeInfo created for the enum.
  TypeInfo * registerEnumTypeInfo(TypeInfo *mutableTI) {
    TI = mutableTI;
    return mutableTI;
  }
  
  /// Constructs a TypeInfo for an enum of the best possible kind for its
  /// layout, FixedEnumTypeInfo or LoadableEnumTypeInfo.
  TypeInfo *getFixedEnumTypeInfo(llvm::StructType *T, Size S, llvm::BitVector SB,
                                  Alignment A, IsPOD_t isPOD);
  
public:
  virtual ~EnumImplStrategy() { }
  
  /// Construct a layout strategy appropriate to the enum type.
  static EnumImplStrategy *get(TypeConverter &TC,
                                CanType type,
                                EnumDecl *theEnum);
  
  /// Given an incomplete StructType for the enum, completes layout of the
  /// storage type, calculates its size and alignment, and produces the
  /// TypeInfo for the enum.
  virtual TypeInfo *completeEnumTypeLayout(TypeConverter &TC,
                                            CanType type,
                                            EnumDecl *theEnum,
                                            llvm::StructType *enumTy) = 0;
  
  const TypeInfo &getTypeInfo() const {
    assert(TI);
    return *TI;
  }
  
  llvm::StructType *getStorageType() const {
    return cast<llvm::StructType>(getTypeInfo().getStorageType());
  }
  
  IsPOD_t isPOD(ResilienceScope scope) const {
    return getTypeInfo().isPOD(scope);
  }
  
  /// \group Indirect enum operations
  
  /// Project the address of the data for a case. Does not check or modify
  /// the referenced enum value.
  /// Corresponds to the SIL 'enum_data_addr' instruction.
  virtual Address projectDataForStore(IRGenFunction &IGF,
                                      EnumElementDecl *elt,
                                      Address enumAddr) const = 0;
  
  /// Overlay the tag value for a case onto a data value in memory.
  /// Corresponds to the SIL 'inject_enum_addr' instruction.
  virtual void storeTag(IRGenFunction &IGF,
                        EnumElementDecl *elt,
                        Address enumAddr) const = 0;
  
  /// Clears tag bits from within the payload of an enum in memory and
  /// projects the address of the data for a case. Does not check
  /// the referenced enum value.
  /// Performs the block argument binding for a SIL
  /// 'destructive_switch_enum_addr' instruction.
  virtual Address destructiveProjectDataForLoad(IRGenFunction &IGF,
                                                EnumElementDecl *elt,
                                                Address enumAddr) const = 0;
  
  /// Emit a branch on the case contained by an enum explosion.
  /// Performs the branching for a SIL 'destructive_switch_enum_addr'
  /// instruction.
  virtual void emitIndirectSwitch(IRGenFunction &IGF,
                                  Address enumAddr,
                                  ArrayRef<std::pair<EnumElementDecl*,
                                                     llvm::BasicBlock*>> dests,
                                  llvm::BasicBlock *defaultDest) const = 0;
  
  /// \group Loadable enum operations
  
  /// Emit the construction sequence for an enum case into an explosion.
  /// Corresponds to the SIL 'enum' instruction.
  virtual void emitValueInjection(IRGenFunction &IGF,
                                  EnumElementDecl *elt,
                                  Explosion &params,
                                  Explosion &out) const = 0;
  
  /// Emit a branch on the case contained by an enum explosion.
  /// Performs the branching for a SIL 'switch_enum' instruction.
  virtual void emitValueSwitch(IRGenFunction &IGF,
                               Explosion &value,
                               ArrayRef<std::pair<EnumElementDecl*,
                                                  llvm::BasicBlock*>> dests,
                               llvm::BasicBlock *defaultDest) const = 0;
  
  /// Project a case value out of an enum explosion. This does not check that
  /// the explosion actually contains a value of the given case.
  /// Performs the block argument binding for a SIL 'switch_enum'
  /// instruction.
  virtual void emitValueProject(IRGenFunction &IGF,
                                Explosion &inEnum,
                                EnumElementDecl *theCase,
                                Explosion &out) const = 0;
  
  /// \group Delegated TypeInfo operations
  
  virtual void getSchema(ExplosionSchema &schema) const = 0;
  virtual void destroy(IRGenFunction &IGF, Address addr) const = 0;
  
  virtual bool isIndirectArgument(ExplosionKind kind) const {
    return TIK < Loadable;
  }
  
  virtual void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                                    Address dest) const {
    if (TIK >= Loadable)
      return initialize(IGF, params, dest);
    Address src = TI->getAddressForPointer(params.claimNext());
    TI->initializeWithTake(IGF, dest, src);
  }
  
  virtual void assignWithCopy(IRGenFunction &IGF, Address dest,
                              Address src) const = 0;
  virtual void assignWithTake(IRGenFunction &IGF, Address dest,
                              Address src) const = 0;
  virtual void initializeWithCopy(IRGenFunction &IGF, Address dest,
                                  Address src) const = 0;
  virtual void initializeWithTake(IRGenFunction &IGF, Address dest,
                                  Address src) const = 0;
  
  virtual void initializeValueWitnessTable(IRGenFunction &IGF,
                                           llvm::Value *metadata,
                                           llvm::Value *vwtable) const = 0;

  virtual bool mayHaveExtraInhabitants() const = 0;

  virtual llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                               Address src) const = 0;
  virtual void storeExtraInhabitant(IRGenFunction &IGF,
                                    llvm::Value *index,
                                    Address dest) const = 0;
  
  /// \group Delegated FixedTypeInfo operations
  
  virtual unsigned getFixedExtraInhabitantCount() const = 0;
  
  virtual llvm::ConstantInt *
  getFixedExtraInhabitantValue(IRGenModule &IGM,
                               unsigned bits,
                               unsigned index) const = 0;
  
  /// \group Delegated LoadableTypeInfo operations
  
  virtual unsigned getExplosionSize(ExplosionKind kind) const = 0;
  virtual void loadAsCopy(IRGenFunction &IGF, Address addr,
                          Explosion &e) const = 0;
  virtual void loadAsTake(IRGenFunction &IGF, Address addr,
                          Explosion &e) const = 0;
  virtual void assign(IRGenFunction &IGF, Explosion &e,
                      Address addr) const = 0;
  virtual void initialize(IRGenFunction &IGF, Explosion &e,
                          Address addr) const = 0;
  virtual void reexplode(IRGenFunction &IGF, Explosion &src,
                         Explosion &dest) const = 0;
  virtual void copy(IRGenFunction &IGF, Explosion &src,
                    Explosion &dest) const = 0;
  virtual void consume(IRGenFunction &IGF, Explosion &src) const = 0;
  virtual llvm::Value *packEnumPayload(IRGenFunction &IGF,
                                        Explosion &in,
                                        unsigned bitWidth,
                                        unsigned offset) const = 0;
  
  virtual void unpackEnumPayload(IRGenFunction &IGF,
                                  llvm::Value *payload,
                                  Explosion &dest,
                                  unsigned offset) const = 0;
};

namespace {
  /// Implementation strategy for singleton enums, with zero or one cases.
  class SingletonEnumImplStrategy final : public EnumImplStrategy {
    const TypeInfo *getSingleton() const {
      return ElementsWithPayload.empty() ? nullptr : ElementsWithPayload[0].ti;
    }
    
    const FixedTypeInfo *getFixedSingleton() const {
      return cast_or_null<FixedTypeInfo>(getSingleton());
    }
    
    const LoadableTypeInfo *getLoadableSingleton() const {
      return cast_or_null<LoadableTypeInfo>(getSingleton());
    }

    Address getSingletonAddress(IRGenFunction &IGF, Address addr) const {
      return IGF.Builder.CreateBitCast(addr,
                             getSingleton()->getStorageType()->getPointerTo());
    }
    
  public:
    SingletonEnumImplStrategy(TypeInfoKind tik, unsigned NumElements,
                           std::vector<Element> &&WithPayload,
                           std::vector<Element> &&WithRecursivePayload,
                           std::vector<Element> &&WithNoPayload)
      : EnumImplStrategy(tik, NumElements,
                          std::move(WithPayload),
                          std::move(WithRecursivePayload),
                          std::move(WithNoPayload))
    {
      assert(NumElements <= 1);
      assert(ElementsWithPayload.size() <= 1);
    }
    
    TypeInfo *completeEnumTypeLayout(TypeConverter &TC,
                                      CanType type,
                                      EnumDecl *theEnum,
                                      llvm::StructType *enumTy) override;

    void emitSingletonSwitch(IRGenFunction &IGF,
                    ArrayRef<std::pair<EnumElementDecl*,
                                       llvm::BasicBlock*>> dests,
                    llvm::BasicBlock *defaultDest) const {
      // No dispatch necessary. Branch straight to the destination.
      assert(dests.size() <= 1 && "impossible switch table for singleton enum");
      llvm::BasicBlock *dest = dests.size() == 1
        ? dests[0].second : defaultDest;
      IGF.Builder.CreateBr(dest);
    }
    
    void emitValueSwitch(IRGenFunction &IGF,
                         Explosion &value,
                         ArrayRef<std::pair<EnumElementDecl*,
                                            llvm::BasicBlock*>> dests,
                         llvm::BasicBlock *defaultDest) const override {
      value.claim(getExplosionSize(value.getKind()));
      emitSingletonSwitch(IGF, dests, defaultDest);
    }
    
    void emitIndirectSwitch(IRGenFunction &IGF,
                            Address addr,
                            ArrayRef<std::pair<EnumElementDecl*,
                                               llvm::BasicBlock*>> dests,
                            llvm::BasicBlock *defaultDest) const override {
      emitSingletonSwitch(IGF, dests, defaultDest);
    }
    
    void emitValueProject(IRGenFunction &IGF,
                          Explosion &in,
                          EnumElementDecl *theCase,
                          Explosion &out) const override {
      // The projected value is the payload.
      if (getLoadableSingleton())
        getLoadableSingleton()->reexplode(IGF, in, out);
    }

    void emitValueInjection(IRGenFunction &IGF,
                            EnumElementDecl *elt,
                            Explosion &params,
                            Explosion &out) const override {
      // If the element carries no data, neither does the injection.
      // Otherwise, the result is identical.
      if (getLoadableSingleton())
        getLoadableSingleton()->reexplode(IGF, params, out);
    }
    
    Address projectDataForStore(IRGenFunction &IGF,
                                EnumElementDecl *elt,
                                Address enumAddr) const override {
      return getSingletonAddress(IGF, enumAddr);
    }
    
    Address destructiveProjectDataForLoad(IRGenFunction &IGF,
                                          EnumElementDecl *elt,
                                          Address enumAddr) const override {
      return getSingletonAddress(IGF, enumAddr);
    }
    
    void storeTag(IRGenFunction &IGF,
                  EnumElementDecl *elt,
                  Address enumAddr) const override {
      // No tag, nothing to do.
    }
    
    void getSchema(ExplosionSchema &schema) const {
      if (!getSingleton()) return;
      // If the payload is loadable, forward its explosion schema.
      if (TIK >= Loadable)
        return getSingleton()->getSchema(schema);
      // Otherwise, use an indirect aggregate schema with our storage
      // type.
      schema.add(ExplosionSchema::Element::forAggregate(getStorageType(),
                                      getSingleton()->getBestKnownAlignment()));
    }
    
    unsigned getExplosionSize(ExplosionKind kind) const {
      if (!getLoadableSingleton()) return 0;
      return getLoadableSingleton()->getExplosionSize(kind);
    }
    
    void loadAsCopy(IRGenFunction &IGF, Address addr, Explosion &e) const {
      if (!getLoadableSingleton()) return;
      getLoadableSingleton()->loadAsCopy(IGF, getSingletonAddress(IGF, addr),e);
    }

    void loadForSwitch(IRGenFunction &IGF, Address addr, Explosion &e) const {
      // Switching on a singleton does not require a value.
      return;
    }

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {
      if (!getLoadableSingleton()) return;
      getLoadableSingleton()->loadAsTake(IGF, getSingletonAddress(IGF, addr),e);
    }
    
    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {
      if (!getLoadableSingleton()) return;
      getLoadableSingleton()->assign(IGF, e, getSingletonAddress(IGF, addr));
    }
    
    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src) const {
      if (!getSingleton()) return;
      dest = getSingletonAddress(IGF, dest);
      src = getSingletonAddress(IGF, src);
      getSingleton()->assignWithCopy(IGF, dest, src);
    }
    
    void assignWithTake(IRGenFunction &IGF, Address dest, Address src) const {
      if (!getSingleton()) return;
      dest = getSingletonAddress(IGF, dest);
      src = getSingletonAddress(IGF, src);
      getSingleton()->assignWithTake(IGF, dest, src);
    }
    
    void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {
      if (!getLoadableSingleton()) return;
      getLoadableSingleton()->initialize(IGF, e, getSingletonAddress(IGF, addr));
    }
    
    void initializeWithCopy(IRGenFunction &IGF, Address dest, Address src)
    const override {
      if (!getSingleton()) return;
      dest = getSingletonAddress(IGF, dest);
      src = getSingletonAddress(IGF, src);
      getSingleton()->initializeWithCopy(IGF, dest, src);
    }
    
    void initializeWithTake(IRGenFunction &IGF, Address dest, Address src)
    const override {
      if (!getSingleton()) return;
      dest = getSingletonAddress(IGF, dest);
      src = getSingletonAddress(IGF, src);
      getSingleton()->initializeWithTake(IGF, dest, src);
    }
    
    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      if (getLoadableSingleton()) getLoadableSingleton()->reexplode(IGF, src, dest);
    }
    
    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      if (getLoadableSingleton()) getLoadableSingleton()->copy(IGF, src, dest);
    }
    
    void consume(IRGenFunction &IGF, Explosion &src) const {
      if (getLoadableSingleton()) getLoadableSingleton()->consume(IGF, src);
    }
    
    void destroy(IRGenFunction &IGF, Address addr) const {
      if (getSingleton() && !getSingleton()->isPOD(ResilienceScope::Local))
        getSingleton()->destroy(IGF, getSingletonAddress(IGF, addr));
    }
    
    llvm::Value *packEnumPayload(IRGenFunction &IGF,
                                  Explosion &in,
                                  unsigned bitWidth,
                                  unsigned offset) const override {
      if (getLoadableSingleton())
        return getLoadableSingleton()->packEnumPayload(IGF, in,
                                                        bitWidth, offset);
      return PackEnumPayload::getEmpty(IGF.IGM, bitWidth);
    }
    
    void unpackEnumPayload(IRGenFunction &IGF,
                            llvm::Value *payload,
                            Explosion &dest,
                            unsigned offset) const override {
      if (!getLoadableSingleton()) return;
      getLoadableSingleton()->unpackEnumPayload(IGF, payload, dest, offset);
    }
    
    void initializeValueWitnessTable(IRGenFunction &IGF,
                                     llvm::Value *metadata,
                                     llvm::Value *vwtable) const override {
      // Fixed-size enums don't need dynamic witness table initialization.
      if (TIK >= Fixed) return;

      assert(!ElementsWithPayload.empty() &&
             "empty singleton enum should not be dynamic!");

      // Get the value witness table for the element.
      CanType eltTy
       = ElementsWithPayload[0].decl->getArgumentType()->getCanonicalType();
      llvm::Value *eltMetadata = IGF.emitTypeMetadataRef(eltTy);
      llvm::Value *eltVWT
        = IGF.emitValueWitnessTableRefForMetadata(eltMetadata);
      
      Address vwtAddr(vwtable, IGF.IGM.getPointerAlignment());
      Address eltVWTAddr(eltVWT, IGF.IGM.getPointerAlignment());
      
      auto copyWitnessFromElt = [&](ValueWitness witness) -> llvm::Value* {
        Address dest = IGF.Builder.CreateConstArrayGEP(vwtAddr,
                                   unsigned(witness), IGF.IGM.getPointerSize());
        Address src = IGF.Builder.CreateConstArrayGEP(eltVWTAddr,
                                   unsigned(witness), IGF.IGM.getPointerSize());
        auto val = IGF.Builder.CreateLoad(src);
        IGF.Builder.CreateStore(val, dest);
        return val;
      };

      copyWitnessFromElt(ValueWitness::Size);
      auto flags = copyWitnessFromElt(ValueWitness::Flags);
      copyWitnessFromElt(ValueWitness::Stride);
      
      // If the original type had extra inhabitants, carry over its
      // extra inhabitant flags.
      auto xiBB = llvm::BasicBlock::Create(IGF.IGM.getLLVMContext());
      auto noXIBB = llvm::BasicBlock::Create(IGF.IGM.getLLVMContext());
      
      auto xiFlag = IGF.Builder.CreatePtrToInt(flags, IGF.IGM.SizeTy);
      auto xiMask
        = IGF.IGM.getSize(Size(ValueWitnessFlags::Enum_HasExtraInhabitants));
      xiFlag = IGF.Builder.CreateAnd(xiFlag, xiMask);
      auto xiBool = IGF.Builder.CreateICmpNE(xiFlag,
                                             IGF.IGM.getSize(Size(0)));
      IGF.Builder.CreateCondBr(xiBool, xiBB, noXIBB);
      
      IGF.Builder.emitBlock(xiBB);
      copyWitnessFromElt(ValueWitness::ExtraInhabitantFlags);
      IGF.Builder.CreateBr(noXIBB);
      
      IGF.Builder.emitBlock(noXIBB);
    }
    
    bool mayHaveExtraInhabitants() const override {
      // FIXME: Hold off on registering extra inhabitants for dynamic enums
      // until initializeValueWitnessTable handles them.
      if (!getSingleton())
        return false;
      return getSingleton()->mayHaveExtraInhabitants();
    }
    
    llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                         Address src) const override {
      if (!getSingleton()) {
        // Any empty value is a valid value.
        return llvm::ConstantInt::getSigned(IGF.IGM.Int32Ty, -1);
      }

      return getSingleton()->getExtraInhabitantIndex(IGF,
                                                 getSingletonAddress(IGF, src));
    }
    
    void storeExtraInhabitant(IRGenFunction &IGF,
                              llvm::Value *index,
                              Address dest) const override {
      if (!getSingleton()) {
        // Nothing to store for empty singletons.
        return;
      }
      getSingleton()->storeExtraInhabitant(IGF, index,
                                           getSingletonAddress(IGF, dest));
    }
    
    unsigned getFixedExtraInhabitantCount() const override {
      assert(TIK >= Fixed);
      if (!getSingleton())
        return 0;
      return getFixedSingleton()->getFixedExtraInhabitantCount();
    }
    
    llvm::ConstantInt *
    getFixedExtraInhabitantValue(IRGenModule &IGM,
                                 unsigned bits,
                                 unsigned index) const override {
      assert(TIK >= Fixed);
      assert(getSingleton() && "empty singletons have no extra inhabitants");
      return getFixedSingleton()
        ->getFixedExtraInhabitantValue(IGM, bits, index);
    }
  };
  
  /// Implementation strategy for no-payload enums, in other words, 'C-like'
  /// enums where none of the cases have data.
  class NoPayloadEnumImplStrategy final
    : public SingleScalarTypeInfo<NoPayloadEnumImplStrategy,
                                  EnumImplStrategy>
  {
    llvm::IntegerType *getDiscriminatorType() const {
      llvm::StructType *Struct = getStorageType();
      return cast<llvm::IntegerType>(Struct->getElementType(0));
    }
    
    /// Map the given element to the appropriate value in the
    /// discriminator type.
    llvm::ConstantInt *getDiscriminatorIndex(EnumElementDecl *target) const {
      // FIXME: using a linear search here is fairly ridiculous.
      unsigned index = 0;
      for (auto elt : target->getParentEnum()->getAllElements()) {
        if (elt == target) break;
        index++;
      }
      return llvm::ConstantInt::get(getDiscriminatorType(), index);
    }
    
  public:
    NoPayloadEnumImplStrategy(TypeInfoKind tik, unsigned NumElements,
                           std::vector<Element> &&WithPayload,
                           std::vector<Element> &&WithRecursivePayload,
                           std::vector<Element> &&WithNoPayload)
      : SingleScalarTypeInfo(tik, NumElements,
                             std::move(WithPayload),
                             std::move(WithRecursivePayload),
                             std::move(WithNoPayload))
    {
      assert(ElementsWithPayload.empty());
      assert(!ElementsWithNoPayload.empty());
    }
    
    TypeInfo *completeEnumTypeLayout(TypeConverter &TC,
                                      CanType type,
                                      EnumDecl *theEnum,
                                      llvm::StructType *enumTy) override;

    void emitValueSwitch(IRGenFunction &IGF,
                         Explosion &value,
                         ArrayRef<std::pair<EnumElementDecl*,
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
    
    void emitIndirectSwitch(IRGenFunction &IGF,
                            Address addr,
                            ArrayRef<std::pair<EnumElementDecl*,
                                               llvm::BasicBlock*>> dests,
                            llvm::BasicBlock *defaultDest) const override {
      Explosion value(ExplosionKind::Minimal);
      loadAsTake(IGF, addr, value);
      emitValueSwitch(IGF, value, dests, defaultDest);
    }
    
    void emitValueProject(IRGenFunction &IGF,
                          Explosion &in,
                          EnumElementDecl *elt,
                          Explosion &out) const override {
      // All of the cases project an empty explosion.
      in.claim(getExplosionSize(in.getKind()));
    }
    
    void emitValueInjection(IRGenFunction &IGF,
                            EnumElementDecl *elt,
                            Explosion &params,
                            Explosion &out) const {
      out.add(getDiscriminatorIndex(elt));
    }
    
    Address projectDataForStore(IRGenFunction &IGF,
                                EnumElementDecl *elt,
                                Address enumAddr) const override {
      llvm_unreachable("cannot project data for no-payload cases");
    }
    Address destructiveProjectDataForLoad(IRGenFunction &IGF,
                                          EnumElementDecl *elt,
                                          Address enumAddr) const override {
      llvm_unreachable("cannot project data for no-payload cases");
    }
    
    void storeTag(IRGenFunction &IGF, EnumElementDecl *elt, Address enumAddr)
    const override {
      llvm::Value *discriminator = getDiscriminatorIndex(elt);
      Address discriminatorAddr
        = IGF.Builder.CreateStructGEP(enumAddr, 0, Size(0));
      IGF.Builder.CreateStore(discriminator, discriminatorAddr);
    }
    
    void initializeValueWitnessTable(IRGenFunction &IGF,
                                     llvm::Value *metadata,
                                     llvm::Value *vwtable) const override {
      // No-payload enums are always fixed-size so never need dynamic value
      // witness table initialization.
    }
    
    /// \group Extra inhabitants for no-payload enums.

    // No-payload enums have all values above their greatest discriminator
    // value that fit inside their storage size available as extra inhabitants.
    
    bool mayHaveExtraInhabitants() const override {
      return getFixedExtraInhabitantCount() > 0;
    }
    
    unsigned getFixedExtraInhabitantCount() const override {
      unsigned bits = cast<FixedTypeInfo>(TI)->getFixedSize().getValueInBits();
      assert(bits < 32 && "freakishly huge no-payload enum");
      return (1U << bits) - ElementsWithNoPayload.size();
    }
    
    llvm::ConstantInt *
    getFixedExtraInhabitantValue(IRGenModule &IGM,
                                 unsigned bits,
                                 unsigned index) const override {
      unsigned value = index + ElementsWithNoPayload.size();
      return llvm::ConstantInt::get(IGM.getLLVMContext(),
                                    APInt(bits, value));
    }
    
    llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                         Address src) const override {
      auto &C = IGF.IGM.getLLVMContext();

      // Load the value.
      auto payloadTy = llvm::IntegerType::get(C,
                      cast<FixedTypeInfo>(TI)->getFixedSize().getValueInBits());
      src = IGF.Builder.CreateBitCast(src, payloadTy->getPointerTo());
      llvm::Value *val = IGF.Builder.CreateLoad(src);
      
      // Subtract the number of cases.
      val = IGF.Builder.CreateSub(val,
              llvm::ConstantInt::get(payloadTy, ElementsWithNoPayload.size()));
      
      // If signed less than zero, we have a valid value. Otherwise, we have
      // an extra inhabitant.
      auto valid
        = IGF.Builder.CreateICmpSLT(val, llvm::ConstantInt::get(payloadTy, 0));
      val = IGF.Builder.CreateSelect(valid,
                                     llvm::ConstantInt::getSigned(payloadTy, -1),
                                     val);
      
      val = IGF.Builder.CreateZExtOrTrunc(val, IGF.IGM.Int32Ty);
      return val;
    }
    
    void storeExtraInhabitant(IRGenFunction &IGF,
                              llvm::Value *index,
                              Address dest) const override {
      auto &C = IGF.IGM.getLLVMContext();
      auto payloadTy = llvm::IntegerType::get(C,
                      cast<FixedTypeInfo>(TI)->getFixedSize().getValueInBits());
      dest = IGF.Builder.CreateBitCast(dest, payloadTy->getPointerTo());

      index = IGF.Builder.CreateZExtOrTrunc(index, payloadTy);
      index = IGF.Builder.CreateAdd(index,
                llvm::ConstantInt::get(payloadTy, ElementsWithNoPayload.size()));
      IGF.Builder.CreateStore(index, dest);
    }
    
    /// \group Required for SingleScalarTypeInfo
    
    llvm::Type *getScalarType() const {
      return getDiscriminatorType();
    }
    
    static Address projectScalar(IRGenFunction &IGF, Address addr) {
      return IGF.Builder.CreateStructGEP(addr, 0, Size(0));
    }

    void emitScalarRetain(IRGenFunction &IGF, llvm::Value *value) const {}
    void emitScalarRelease(IRGenFunction &IGF, llvm::Value *value) const {}
    
    void initializeWithTake(IRGenFunction &IGF, Address dest, Address src)
    const override {
      // No-payload enums are always POD, so we can always initialize by
      // primitive copy.
      llvm::Value *val = IGF.Builder.CreateLoad(src);
      IGF.Builder.CreateStore(val, dest);
    }
    
    static constexpr IsPOD_t IsScalarPOD = IsPOD;
  };
  
  /// Common base class for enums with one or more cases with data.
  class PayloadEnumImplStrategyBase : public EnumImplStrategy {
  protected:
    // The number of extra tag bits outside of the payload required to
    // discriminate enum cases.
    unsigned ExtraTagBitCount = ~0u;
    // The number of possible values for the extra tag bits that are used.
    // Log2(NumExtraTagValues - 1) + 1 == ExtraTagBitCount
    unsigned NumExtraTagValues = ~0u;

  public:
    PayloadEnumImplStrategyBase(TypeInfoKind tik, unsigned NumElements,
                                 std::vector<Element> &&WithPayload,
                                 std::vector<Element> &&WithRecursivePayload,
                                 std::vector<Element> &&WithNoPayload)
      : EnumImplStrategy(tik, NumElements,
                          std::move(WithPayload),
                          std::move(WithRecursivePayload),
                          std::move(WithNoPayload))
    {
      assert(ElementsWithPayload.size() >= 1);
    }
    
    virtual Size getExtraTagBitOffset() const = 0;
    
    void getSchema(ExplosionSchema &schema) const override {
      if (TIK < Loadable) {
        schema.add(ExplosionSchema::Element::forAggregate(getStorageType(),
                                                  TI->getBestKnownAlignment()));
        return;
      }
      
      schema.add(ExplosionSchema::Element::forScalar(
                                         getStorageType()->getElementType(0)));
      if (ExtraTagBitCount > 0)
        schema.add(ExplosionSchema::Element::forScalar(
                                         getStorageType()->getElementType(1)));
    }
    
    unsigned getExplosionSize(ExplosionKind kind) const override {
      return ExtraTagBitCount > 0 ? 2 : 1;
    }
    
    Address projectPayload(IRGenFunction &IGF, Address addr) const {
      return IGF.Builder.CreateStructGEP(addr, 0, Size(0));
    }

    Address projectExtraTagBits(IRGenFunction &IGF, Address addr) const {
      assert(ExtraTagBitCount > 0 && "does not have extra tag bits");
      // The place LLVM naturally places the extra tag bits isn't necessarily
      // where we want it. E.g., if our layout is as {i72, i1}, then the
      // i72 will likely take up 128 bits of storage and be 64-bit-aligned, when
      // we really want the tag bit to be allocated right after the 72-bit
      // payload.
      addr = IGF.Builder.CreateBitCast(addr, IGF.IGM.Int8PtrTy);
      addr = IGF.Builder.CreateConstByteArrayGEP(addr,
                                                 getExtraTagBitOffset());
      return IGF.Builder.CreateBitCast(addr,
                         getStorageType()->getElementType(1)->getPointerTo());
    }

    void loadForSwitch(IRGenFunction &IGF, Address addr, Explosion &e)
    const {
      assert(TIK >= Fixed);
      e.add(IGF.Builder.CreateLoad(projectPayload(IGF, addr)));
      if (ExtraTagBitCount > 0)
        e.add(IGF.Builder.CreateLoad(projectExtraTagBits(IGF, addr)));
    }

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e)
    const override {
      assert(TIK >= Loadable);
      loadForSwitch(IGF, addr, e);
    }
    
    void loadAsCopy(IRGenFunction &IGF, Address addr, Explosion &e)
    const override {
      assert(TIK >= Loadable);
      Explosion tmp(e.getKind());
      loadAsTake(IGF, addr, tmp);
      copy(IGF, tmp, e);
    }
    
    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const override {
      assert(TIK >= Loadable);
      Explosion old(e.getKind());
      if (!isPOD(ResilienceScope::Local))
        loadAsTake(IGF, addr, old);
      initialize(IGF, e, addr);
      if (!isPOD(ResilienceScope::Local))
        consume(IGF, old);
    }
    
    void initialize(IRGenFunction &IGF, Explosion &e, Address addr)
    const override {
      assert(TIK >= Loadable);
      IGF.Builder.CreateStore(e.claimNext(), projectPayload(IGF, addr));
      if (ExtraTagBitCount > 0)
        IGF.Builder.CreateStore(e.claimNext(), projectExtraTagBits(IGF, addr));
    }
    
    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest)
    const override {
      assert(TIK >= Loadable);
      dest.add(src.claim(getExplosionSize(ExplosionKind::Minimal)));
    }
    
  protected:
    /// Do a primitive copy of the enum from one address to another.
    void emitPrimitiveCopy(IRGenFunction &IGF, Address dest, Address src) const{
      // If the layout is fixed, load and store the fixed-size payload and tag.
      if (TIK >= Fixed) {
        llvm::Value *payload, *extraTag;
        std::tie(payload, extraTag)
          = emitPrimitiveLoadPayloadAndExtraTag(IGF, src);
        emitPrimitiveStorePayloadAndExtraTag(IGF, dest, payload, extraTag);
        return;
      }
      
      // Otherwise, do a memcpy of the dynamic size of the type.
      IGF.Builder.CreateMemCpy(dest.getAddress(), src.getAddress(),
                               TI->getSize(IGF),
                               std::min(dest.getAlignment().getValue(),
                                        src.getAlignment().getValue()));
    }
    
    void emitPrimitiveStorePayloadAndExtraTag(IRGenFunction &IGF, Address dest,
                                              llvm::Value *payload,
                                              llvm::Value *extraTag) const {
      IGF.Builder.CreateStore(payload, projectPayload(IGF, dest));
      if (ExtraTagBitCount > 0)
        IGF.Builder.CreateStore(extraTag, projectExtraTagBits(IGF, dest));
    }
    
    std::pair<llvm::Value*, llvm::Value*>
    getPayloadAndExtraTagFromExplosion(Explosion &src) const {
      llvm::Value *payload = src.claimNext();
      llvm::Value *extraTag = ExtraTagBitCount > 0 ? src.claimNext() : nullptr;
      return {payload, extraTag};
    }
    
    std::pair<llvm::Value*, llvm::Value*>
    emitPrimitiveLoadPayloadAndExtraTag(IRGenFunction &IGF, Address addr) const{
      llvm::Value *payload
        = IGF.Builder.CreateLoad(projectPayload(IGF, addr));
      llvm::Value *extraTag = nullptr;
      if (ExtraTagBitCount > 0)
        extraTag = IGF.Builder.CreateLoad(projectExtraTagBits(IGF, addr));
      return {payload, extraTag};
    }
  };

  class SinglePayloadEnumImplStrategy final
    : public PayloadEnumImplStrategyBase
  {
    EnumElementDecl *getPayloadElement() const {
      return ElementsWithPayload[0].decl;
    }
    
    const TypeInfo &getPayloadTypeInfo() const {
      return *ElementsWithPayload[0].ti;
    }
    const FixedTypeInfo &getFixedPayloadTypeInfo() const {
      return cast<FixedTypeInfo>(*ElementsWithPayload[0].ti);
    }
    const LoadableTypeInfo &getLoadablePayloadTypeInfo() const {
      return cast<LoadableTypeInfo>(*ElementsWithPayload[0].ti);
    }
    
    CanType PayloadTy;
    
    llvm::Value *emitPayloadMetadata(IRGenFunction &IGF) const {
      return IGF.emitTypeMetadataRef(PayloadTy);
    }
    
  public:
    SinglePayloadEnumImplStrategy(TypeInfoKind tik, unsigned NumElements,
                                   std::vector<Element> &&WithPayload,
                                   std::vector<Element> &&WithRecursivePayload,
                                   std::vector<Element> &&WithNoPayload)
      : PayloadEnumImplStrategyBase(tik, NumElements,
                                     std::move(WithPayload),
                                     std::move(WithRecursivePayload),
                                     std::move(WithNoPayload))
    {
      assert(ElementsWithPayload.size() == 1);
    }
    
    /// The payload for a single-payload enum is always placed in front and
    /// will never have interleaved tag bits, so we can just bitcast the enum
    /// address to the payload type for either injection or projection of the
    /// enum.
    Address projectPayloadData(IRGenFunction &IGF, Address addr) const {
      return IGF.Builder.CreateBitCast(addr,
                         getPayloadTypeInfo().getStorageType()->getPointerTo());
    }
    Address projectDataForStore(IRGenFunction &IGF, EnumElementDecl *elt,
                                Address enumAddr) const override {
      assert(elt == getPayloadElement() && "cannot project no-data case");
      return projectPayloadData(IGF, enumAddr);
    }
    Address destructiveProjectDataForLoad(IRGenFunction &IGF,
                                          EnumElementDecl *elt,
                                          Address enumAddr) const override {
      assert(elt == getPayloadElement() && "cannot project no-data case");
      return projectPayloadData(IGF, enumAddr);
    }
    
    Size getExtraTagBitOffset() const override {
      return getFixedPayloadTypeInfo().getFixedSize();
    }
    
    TypeInfo *completeEnumTypeLayout(TypeConverter &TC,
                                      CanType type,
                                      EnumDecl *theEnum,
                                      llvm::StructType *enumTy) override;
  private:
    TypeInfo *completeFixedLayout(TypeConverter &TC,
                                  CanType type,
                                  EnumDecl *theEnum,
                                  llvm::StructType *enumTy);
    TypeInfo *completeDynamicLayout(TypeConverter &TC,
                                    CanType type,
                                    EnumDecl *theEnum,
                                    llvm::StructType *enumTy);
    
  public:
    llvm::Value *packEnumPayload(IRGenFunction &IGF, Explosion &src,
                                  unsigned bitWidth,
                                  unsigned offset) const override {
      PackEnumPayload pack(IGF, bitWidth);
      // Pack payload.
      pack.addAtOffset(src.claimNext(), offset);
      
      // Pack tag bits, if any.
      if (ExtraTagBitCount > 0) {
        unsigned extraTagOffset
          = getFixedPayloadTypeInfo().getFixedSize().getValueInBits() + offset;
        
        pack.addAtOffset(src.claimNext(), extraTagOffset);
      }
      
      return pack.get();
    }
    
    void unpackEnumPayload(IRGenFunction &IGF, llvm::Value *outerPayload,
                            Explosion &dest,
                            unsigned offset) const override {
      UnpackEnumPayload unpack(IGF, outerPayload);
      
      // Unpack our inner payload.
      dest.add(unpack.claimAtOffset(getStorageType()->getElementType(0),
                                    offset));
      
      // Unpack our extra tag bits, if any.
      if (ExtraTagBitCount > 0) {
        unsigned extraTagOffset
          = getFixedPayloadTypeInfo().getFixedSize().getValueInBits() + offset;
        
        dest.add(unpack.claimAtOffset(getStorageType()->getElementType(1),
                                      extraTagOffset));
      }
    }
    
    void emitValueSwitch(IRGenFunction &IGF,
                         Explosion &value,
                         ArrayRef<std::pair<EnumElementDecl*,
                                            llvm::BasicBlock*>> dests,
                         llvm::BasicBlock *defaultDest) const override {
      auto &C = IGF.IGM.getLLVMContext();
      
      // Create a map of the destination blocks for quicker lookup.
      llvm::DenseMap<EnumElementDecl*,llvm::BasicBlock*> destMap(dests.begin(),
                                                                  dests.end());
      // Create an unreachable branch for unreachable switch defaults.
      auto *unreachableBB = llvm::BasicBlock::Create(C);
      
      // If there was no default branch in SIL, use the unreachable branch as
      // the default.
      if (!defaultDest)
        defaultDest = unreachableBB;
      
      auto blockForCase = [&](EnumElementDecl *theCase) -> llvm::BasicBlock* {
        auto found = destMap.find(theCase);
        if (found == destMap.end())
          return defaultDest;
        else
          return found->second;
      };
      
      llvm::Value *payload = value.claimNext();
      llvm::BasicBlock *payloadDest = blockForCase(getPayloadElement());
      unsigned extraInhabitantCount
        = getFixedPayloadTypeInfo().getFixedExtraInhabitantCount();
      
      // If there are extra tag bits, switch over them first.
      SmallVector<llvm::BasicBlock*, 2> tagBitBlocks;
      if (ExtraTagBitCount > 0) {
        llvm::Value *tagBits = value.claimNext();
        
        auto *swi = IGF.Builder.CreateSwitch(tagBits, unreachableBB,
                                             NumExtraTagValues);
        
        // If we have extra inhabitants, we need to check for them in the
        // zero-tag case. Otherwise, we switch directly to the payload case.
        if (extraInhabitantCount > 0) {
          auto bb = llvm::BasicBlock::Create(C);
          tagBitBlocks.push_back(bb);
          swi->addCase(llvm::ConstantInt::get(C,APInt(ExtraTagBitCount,0)), bb);
        } else {
          tagBitBlocks.push_back(payloadDest);
          swi->addCase(llvm::ConstantInt::get(C,APInt(ExtraTagBitCount,0)),
                       payloadDest);
        }
        
        for (unsigned i = 1; i < NumExtraTagValues; ++i) {
          auto bb = llvm::BasicBlock::Create(C);
          tagBitBlocks.push_back(bb);
          swi->addCase(llvm::ConstantInt::get(C,APInt(ExtraTagBitCount,i)), bb);
        }
        
        // Continue by emitting the extra inhabitant dispatch, if any.
        if (extraInhabitantCount > 0)
          IGF.Builder.emitBlock(tagBitBlocks[0]);
      }
      
      auto elements = getPayloadElement()->getParentEnum()->getAllElements();
      auto elti = elements.begin(), eltEnd = elements.end();
      if (*elti == getPayloadElement())
        ++elti;
      
      // Advance the enum element iterator, skipping the payload case.
      auto nextCase = [&]() -> EnumElementDecl* {
        assert(elti != eltEnd);
        auto result = *elti;
        ++elti;
        if (elti != eltEnd && *elti == getPayloadElement())
          ++elti;
        return result;
      };
      
      // If there are no extra tag bits, or they're set to zero, then we either
      // have a payload, or an empty case represented using an extra inhabitant.
      // Check the extra inhabitant cases if we have any.
      unsigned payloadBits
        = getFixedPayloadTypeInfo().getFixedSize().getValueInBits();
      if (extraInhabitantCount > 0) {
        auto *swi = IGF.Builder.CreateSwitch(payload, payloadDest);
        for (unsigned i = 0; i < extraInhabitantCount && elti != eltEnd; ++i) {
          auto v = getFixedPayloadTypeInfo().getFixedExtraInhabitantValue(
                                                       IGF.IGM, payloadBits, i);
          swi->addCase(v, blockForCase(nextCase()));
        }
      }
      
      // We should have handled the payload case either in extra inhabitant
      // or in extra tag dispatch by now.
      assert(IGF.Builder.hasPostTerminatorIP() &&
             "did not handle payload case");
      
      // Handle the cases covered by each tag bit value.
      unsigned casesPerTag = 1 << ExtraTagBitCount;
      for (unsigned i = 1, e = tagBitBlocks.size(); i < e; ++i) {
        assert(elti != eltEnd &&
               "ran out of cases before running out of extra tags?");
        IGF.Builder.emitBlock(tagBitBlocks[i]);
        auto swi = IGF.Builder.CreateSwitch(payload, unreachableBB);
        for (unsigned tag = 0; tag < casesPerTag && elti != eltEnd; ++tag) {
          auto v = llvm::ConstantInt::get(C, APInt(payloadBits, tag));
          swi->addCase(v, blockForCase(nextCase()));
        }
      }
      
      // Delete the unreachable default block if we didn't use it, or emit it
      // if we did.
      if (unreachableBB->use_empty()) {
        delete unreachableBB;
      } else {
        IGF.Builder.emitBlock(unreachableBB);
        IGF.Builder.CreateUnreachable();
      }
    }
    
    void emitDynamicSwitch(IRGenFunction &IGF,
                           Address addr,
                           ArrayRef<std::pair<EnumElementDecl*,
                                              llvm::BasicBlock*>> dests,
                           llvm::BasicBlock *defaultDest) const {
      auto payloadMetadata = emitPayloadMetadata(IGF);
      auto numEmptyCases = llvm::ConstantInt::get(IGF.IGM.Int32Ty,
                                                  ElementsWithNoPayload.size());
      auto opaqueAddr = IGF.Builder.CreateBitCast(addr.getAddress(),
                                                  IGF.IGM.OpaquePtrTy);
      
      // Create a map of the destination blocks for quicker lookup.
      llvm::DenseMap<EnumElementDecl*,llvm::BasicBlock*> destMap(dests.begin(),
                                                                  dests.end());

      // If there was no default branch in SIL, use an unreachable branch as
      // the default.
      llvm::BasicBlock *unreachableBB = nullptr;
      if (!defaultDest) {
        unreachableBB = llvm::BasicBlock::Create(IGF.IGM.getLLVMContext());
        defaultDest = unreachableBB;
      }
      
      // Ask the runtime to find the case index.
      auto caseIndex = IGF.Builder.CreateCall3(
                            IGF.IGM.getGetEnumCaseSinglePayloadFn(),
                            opaqueAddr, payloadMetadata, numEmptyCases);

      // Switch on the index.
      auto *swi = IGF.Builder.CreateSwitch(caseIndex, defaultDest);

      // Add the payload case.
      auto payloadCase = destMap.find(getPayloadElement());
      if (payloadCase != destMap.end())
        swi->addCase(llvm::ConstantInt::getSigned(IGF.IGM.Int32Ty, -1),
                     payloadCase->second);
      
      // Add the empty cases.
      unsigned emptyCaseIndex = 0;
      for (auto &empty : ElementsWithNoPayload) {
        auto emptyCase = destMap.find(empty.decl);
        if (emptyCase != destMap.end())
          swi->addCase(llvm::ConstantInt::get(IGF.IGM.Int32Ty, emptyCaseIndex),
                       emptyCase->second);
        ++emptyCaseIndex;
      }
      
      // Emit the unreachable block, if any.
      if (unreachableBB) {
        IGF.Builder.emitBlock(unreachableBB);
        IGF.Builder.CreateUnreachable();
      }
    }
    
    void emitIndirectSwitch(IRGenFunction &IGF,
                            Address addr,
                            ArrayRef<std::pair<EnumElementDecl*,
                                               llvm::BasicBlock*>> dests,
                            llvm::BasicBlock *defaultDest) const override {
      if (TIK >= Fixed) {
        // Load the fixed-size representation and switch directly.
        Explosion value(ExplosionKind::Minimal);
        loadForSwitch(IGF, addr, value);
        return emitValueSwitch(IGF, value, dests, defaultDest);
      }
      
      // Use the runtime to dynamically switch.
      emitDynamicSwitch(IGF, addr, dests, defaultDest);
    }
    
    void emitValueProject(IRGenFunction &IGF,
                          Explosion &inEnum,
                          EnumElementDecl *theCase,
                          Explosion &out) const override {
      // Only the payload case has anything to project. The other cases are
      // empty.
      if (theCase != getPayloadElement()) {
        inEnum.claim(getExplosionSize(inEnum.getKind()));
        return;
      }
      
      llvm::Value *payload = inEnum.claimNext();
      if (ExtraTagBitCount > 0)
        inEnum.claimNext();
      // FIXME non-loadable payloads
      getLoadablePayloadTypeInfo().unpackEnumPayload(IGF, payload, out, 0);
    }
    
  private:
    // Get the index of an enum element among the non-payload cases.
    unsigned getSimpleElementTagIndex(EnumElementDecl *elt) const {
      assert(elt != getPayloadElement() && "is payload element");
      unsigned i = 0;
      // FIXME: linear search
      for (auto *enumElt : elt->getParentEnum()->getAllElements()) {
        if (elt == enumElt)
          return i;
        if (enumElt != getPayloadElement())
          ++i;
      }
      llvm_unreachable("element was not a member of enum");
    }
    
    // Get the payload and extra tag (if any) parts of the discriminator for
    // a no-data case.
    std::pair<llvm::Value *, llvm::Value *>
    getNoPayloadCaseValue(IRGenFunction &IGF, EnumElementDecl *elt) const {
      assert(elt != getPayloadElement());
      
      unsigned payloadSize
        = getFixedPayloadTypeInfo().getFixedSize().getValueInBits();
      
      // Non-payload cases use extra inhabitants, if any, or are discriminated
      // by setting the tag bits.
      unsigned tagIndex = getSimpleElementTagIndex(elt);
      unsigned numExtraInhabitants
        = getFixedPayloadTypeInfo().getFixedExtraInhabitantCount();
      llvm::Value *payload;
      unsigned extraTagValue;
      if (tagIndex < numExtraInhabitants) {
        payload = getFixedPayloadTypeInfo().getFixedExtraInhabitantValue(
                                               IGF.IGM, payloadSize, tagIndex);
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

      llvm::Value *extraTag = nullptr;
      if (ExtraTagBitCount > 0) {
        extraTag = llvm::ConstantInt::get(IGF.IGM.getLLVMContext(),
                                        APInt(ExtraTagBitCount, extraTagValue));
      } else {
        assert(extraTagValue == 0 &&
               "non-zero extra tag value with no tag bits");
      }
      return {payload, extraTag};
    }
    
  public:
    void emitValueInjection(IRGenFunction &IGF,
                            EnumElementDecl *elt,
                            Explosion &params,
                            Explosion &out) const {
      // The payload case gets its native representation. If there are extra
      // tag bits, set them to zero.
      unsigned payloadSize
        = getFixedPayloadTypeInfo().getFixedSize().getValueInBits();
      
      if (elt == getPayloadElement()) {
        auto &loadablePayloadTI = getLoadablePayloadTypeInfo();
        llvm::Value *payload
          = loadablePayloadTI.packEnumPayload(IGF, params, payloadSize, 0);
        out.add(payload);

        if (ExtraTagBitCount > 0)
          out.add(getZeroExtraTagConstant(IGF.IGM));
        return;
      }
      
      // Non-payload cases use extra inhabitants, if any, or are discriminated
      // by setting the tag bits.
      llvm::Value *payload, *extraTag;
      std::tie(payload, extraTag) = getNoPayloadCaseValue(IGF, elt);
      out.add(payload);
      if (ExtraTagBitCount > 0) {
        assert(extraTag);
        out.add(extraTag);
      }
    }
    
  private:
    /// Emits the test(s) that determine whether the fixed-size enum contains a
    /// payload or an empty case. Emits the basic block for the "true" case and
    /// returns the unemitted basic block for the "false" case.
    llvm::BasicBlock *
    testFixedEnumContainsPayload(IRGenFunction &IGF,
                                  llvm::Value *payload,
                                  llvm::Value *extraBits) const {
      auto *falseBB = llvm::BasicBlock::Create(IGF.IGM.getLLVMContext());
      
      // We only need to apply the payload operation if the enum contains a
      // value of the payload case.
      
      // If we have extra tag bits, they will be zero if we contain a payload.
      if (ExtraTagBitCount > 0) {
        assert(extraBits);
        llvm::Value *zero = llvm::ConstantInt::get(extraBits->getType(), 0);
        llvm::Value *isZero = IGF.Builder.CreateICmp(llvm::CmpInst::ICMP_EQ,
                                                     extraBits, zero);
        
        auto *trueBB = llvm::BasicBlock::Create(IGF.IGM.getLLVMContext());
        IGF.Builder.CreateCondBr(isZero, trueBB, falseBB);
        
        IGF.Builder.emitBlock(trueBB);
      }
      
      // If we used extra inhabitants to represent empty case discriminators,
      // weed them out.
      unsigned numExtraInhabitants
        = getFixedPayloadTypeInfo().getFixedExtraInhabitantCount();
      if (numExtraInhabitants > 0) {
        auto *payloadBB = llvm::BasicBlock::Create(IGF.IGM.getLLVMContext());
        auto *swi = IGF.Builder.CreateSwitch(payload, payloadBB);
        
        auto elements = getPayloadElement()->getParentEnum()->getAllElements();
        unsigned inhabitant = 0;
        for (auto i = elements.begin(), end = elements.end();
             i != end && inhabitant < numExtraInhabitants;
             ++i, ++inhabitant) {
          if (*i == getPayloadElement())
            ++i;
          auto xi = getFixedPayloadTypeInfo().getFixedExtraInhabitantValue(
                      IGF.IGM,
                      getFixedPayloadTypeInfo().getFixedSize().getValueInBits(),
                      inhabitant);
          swi->addCase(xi, falseBB);
        }
        
        IGF.Builder.emitBlock(payloadBB);
      }
      
      return falseBB;
    }
    
    /// Emits the test(s) that determine whether the enum contains a payload
    /// or an empty case. For a fixed-size enum, this does a primitive load
    /// of the representation and calls down to testFixedEnumContainsPayload.
    /// For a dynamic enum, this queries the value witness table of the payload
    /// type. Emits the basic block for the "true" case and
    /// returns the unemitted basic block for the "false" case.
    llvm::BasicBlock *
    testEnumContainsPayload(IRGenFunction &IGF,
                             Address addr) const {
      auto &C = IGF.IGM.getLLVMContext();
      
      if (TIK >= Fixed) {
        llvm::Value *payload, *extraTag;
        std::tie(payload, extraTag)
          = emitPrimitiveLoadPayloadAndExtraTag(IGF, addr);
        return testFixedEnumContainsPayload(IGF, payload, extraTag);
      }

      auto *payloadBB = llvm::BasicBlock::Create(C);
      auto *noPayloadBB = llvm::BasicBlock::Create(C);

      // Look up the metadata for the payload.
      llvm::Value *metadata = emitPayloadMetadata(IGF);

      // Ask the runtime what case we have.
      llvm::Value *opaqueAddr = IGF.Builder.CreateBitCast(addr.getAddress(),
                                                          IGF.IGM.OpaquePtrTy);
      llvm::Value *numCases = llvm::ConstantInt::get(IGF.IGM.Int32Ty,
                                                 ElementsWithNoPayload.size());
      llvm::Value *which = IGF.Builder.CreateCall3(
                                       IGF.IGM.getGetEnumCaseSinglePayloadFn(),
                                       opaqueAddr, metadata, numCases);
      
      // If it's -1 then we have the payload.
      llvm::Value *hasPayload = IGF.Builder.CreateICmpEQ(which,
                             llvm::ConstantInt::getSigned(IGF.IGM.Int32Ty, -1));
      IGF.Builder.CreateCondBr(hasPayload, payloadBB, noPayloadBB);
      
      IGF.Builder.emitBlock(payloadBB);
      return noPayloadBB;
    }
    
  public:
    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest)
    const override {
      assert(TIK >= Loadable);
      if (isPOD(ResilienceScope::Local)) {
        reexplode(IGF, src, dest);
        return;
      }
      
      // Copy the payload, if we have it.
      llvm::Value *payload, *extraTag;
      std::tie(payload, extraTag) = getPayloadAndExtraTagFromExplosion(src);
      
      llvm::BasicBlock *endBB = testFixedEnumContainsPayload(IGF, payload, extraTag);
      
      Explosion payloadValue(ExplosionKind::Minimal);
      Explosion payloadCopy(ExplosionKind::Minimal);
      auto &loadableTI = getLoadablePayloadTypeInfo();
      loadableTI.unpackEnumPayload(IGF, payload, payloadValue, 0);
      loadableTI.copy(IGF, payloadValue, payloadCopy);
      payloadCopy.claimAll(); // FIXME: repack if not bit-identical
      
      IGF.Builder.CreateBr(endBB);
      IGF.Builder.emitBlock(endBB);
      
      // Copy to the new explosion.
      dest.add(payload);
      if (extraTag) dest.add(extraTag);
    }
    
    void consume(IRGenFunction &IGF, Explosion &src) const override {
      assert(TIK >= Loadable);

      if (isPOD(ResilienceScope::Local)) {
        src.claim(getExplosionSize(src.getKind()));
        return;
      }
      
      // Check that we have a payload.
      llvm::Value *payload, *extraTag;
      std::tie(payload, extraTag) = getPayloadAndExtraTagFromExplosion(src);
      
      llvm::BasicBlock *endBB = testFixedEnumContainsPayload(IGF, payload, extraTag);
      
      // If we did, consume it.
      Explosion payloadValue(ExplosionKind::Minimal);
      auto &loadableTI = getLoadablePayloadTypeInfo();
      loadableTI.unpackEnumPayload(IGF, payload, payloadValue, 0);
      loadableTI.consume(IGF, payloadValue);
      
      IGF.Builder.CreateBr(endBB);
      IGF.Builder.emitBlock(endBB);
    }

    void destroy(IRGenFunction &IGF, Address addr) const override {
      if (isPOD(ResilienceScope::Local))
        return;
      
      // Check that there is a payload at the address.
      llvm::BasicBlock *endBB = testEnumContainsPayload(IGF, addr);
      
      // If there is, project and destroy it.
      Address payloadAddr = projectPayloadData(IGF, addr);
      getPayloadTypeInfo().destroy(IGF, payloadAddr);
      
      IGF.Builder.CreateBr(endBB);
      IGF.Builder.emitBlock(endBB);
    }
    
  private:
    llvm::ConstantInt *getZeroExtraTagConstant(IRGenModule &IGM) const {
      assert(TIK >= Fixed && "not fixed layout");
      assert(ExtraTagBitCount > 0 && "no extra tag bits?!");
      return llvm::ConstantInt::get(IGM.getLLVMContext(),
                                    APInt(ExtraTagBitCount, 0));
    }
    
    /// Initialize the extra tag bits, if any, to zero to indicate a payload.
    void emitInitializeExtraTagBitsForPayload(IRGenFunction &IGF,
                                              Address dest) const {
      if (TIK >= Fixed) {
        // We statically know whether we have extra tag bits.
        // Store zero directly to the fixed-layout extra tag field.
        if (ExtraTagBitCount > 0) {
          auto *zeroTag = getZeroExtraTagConstant(IGF.IGM);
          IGF.Builder.CreateStore(zeroTag, projectExtraTagBits(IGF, dest));
        }
        return;
      }
      
      // Ask the runtime to store the tag.
      llvm::Value *opaqueAddr = IGF.Builder.CreateBitCast(dest.getAddress(),
                                                          IGF.IGM.OpaquePtrTy);
      llvm::Value *metadata = emitPayloadMetadata(IGF);
      IGF.Builder.CreateCall4(IGF.IGM.getStoreEnumTagSinglePayloadFn(),
                              opaqueAddr, metadata,
                              llvm::ConstantInt::getSigned(IGF.IGM.Int32Ty, -1),
                              llvm::ConstantInt::get(IGF.IGM.Int32Ty,
                                                 ElementsWithNoPayload.size()));
    }
    
    /// Emit an reassignment sequence from an enum at one address to another.
    void emitIndirectAssign(IRGenFunction &IGF,
       Address dest, Address src,
       void (TypeInfo::*assignData)(IRGenFunction&, Address, Address) const,
       void (TypeInfo::*initializeData)(IRGenFunction&, Address, Address) const)
    const {
      auto &C = IGF.IGM.getLLVMContext();
      
      if (isPOD(ResilienceScope::Local))
        return emitPrimitiveCopy(IGF, dest, src);

      llvm::BasicBlock *endBB = llvm::BasicBlock::Create(C);

      Address destData = projectPayloadData(IGF, dest);
      Address srcData = projectPayloadData(IGF, src);
      // See whether the current value at the destination has a payload.

      llvm::BasicBlock *noDestPayloadBB
        = testEnumContainsPayload(IGF, dest);
      
      // Here, the destination has a payload. Now see if the source also has
      // one.
      llvm::BasicBlock *destNoSrcPayloadBB
        = testEnumContainsPayload(IGF, src);
      
      // Here, both source and destination have payloads. Do the reassignment
      // of the payload in-place.
      (getPayloadTypeInfo().*assignData)(IGF, destData, srcData);
      IGF.Builder.CreateBr(endBB);
      
      // If the destination has a payload but the source doesn't, we can destroy
      // the payload and primitive-store the new no-payload value.
      IGF.Builder.emitBlock(destNoSrcPayloadBB);
      getPayloadTypeInfo().destroy(IGF, destData);
      emitPrimitiveCopy(IGF, dest, src);
      IGF.Builder.CreateBr(endBB);
      
      // Now, if the destination has no payload, check if the source has one.
      IGF.Builder.emitBlock(noDestPayloadBB);
      llvm::BasicBlock *noDestNoSrcPayloadBB
        = testEnumContainsPayload(IGF, src);

      // Here, the source has a payload but the destination doesn't. We can
      // copy-initialize the source over the destination, then primitive-store
      // the zero extra tag (if any).
      (getPayloadTypeInfo().*initializeData)(IGF, destData, srcData);
      emitInitializeExtraTagBitsForPayload(IGF, dest);
      IGF.Builder.CreateBr(endBB);
      
      // If neither destination nor source have payloads, we can just primitive-
      // store the new empty-case value.
      IGF.Builder.emitBlock(noDestNoSrcPayloadBB);
      emitPrimitiveCopy(IGF, dest, src);
      IGF.Builder.CreateBr(endBB);
      
      IGF.Builder.emitBlock(endBB);
    }
    
    /// Emit an initialization sequence, initializing an enum at one address
    /// with another at a different address.
    void emitIndirectInitialize(IRGenFunction &IGF,
      Address dest, Address src,
      void (TypeInfo::*initializeData)(IRGenFunction&, Address, Address) const)
    const {
      auto &C = IGF.IGM.getLLVMContext();
      
      if (isPOD(ResilienceScope::Local))
        return emitPrimitiveCopy(IGF, dest, src);
      
      llvm::BasicBlock *endBB = llvm::BasicBlock::Create(C);

      Address destData = projectPayloadData(IGF, dest);
      Address srcData = projectPayloadData(IGF, src);

      // See whether the source value has a payload.
      llvm::BasicBlock *noSrcPayloadBB
        = testEnumContainsPayload(IGF, src);
      
      // Here, the source value has a payload. Initialize the destination with
      // it, and set the extra tag if any to zero.
      (getPayloadTypeInfo().*initializeData)(IGF, destData, srcData);
      emitInitializeExtraTagBitsForPayload(IGF, dest);
      IGF.Builder.CreateBr(endBB);

      // If the source value has no payload, we can primitive-store the
      // empty-case value.
      IGF.Builder.emitBlock(noSrcPayloadBB);
      emitPrimitiveCopy(IGF, dest, src);
      IGF.Builder.CreateBr(endBB);
      
      IGF.Builder.emitBlock(endBB);
    }
    
  public:
    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src)
    const override {
      emitIndirectAssign(IGF, dest, src,
                         &TypeInfo::assignWithCopy,
                         &TypeInfo::initializeWithCopy);
    }
    
    void assignWithTake(IRGenFunction &IGF, Address dest, Address src)
    const override {
      emitIndirectAssign(IGF, dest, src,
                         &TypeInfo::assignWithTake,
                         &TypeInfo::initializeWithTake);
    }
    
    void initializeWithCopy(IRGenFunction &IGF, Address dest, Address src)
    const override {
      emitIndirectInitialize(IGF, dest, src,
                             &TypeInfo::initializeWithCopy);
    }
    
    void initializeWithTake(IRGenFunction &IGF, Address dest, Address src)
    const override {
      emitIndirectInitialize(IGF, dest, src,
                             &TypeInfo::initializeWithTake);
    }
    
    void storeTag(IRGenFunction &IGF,
                  EnumElementDecl *elt,
                  Address enumAddr) const override {
      if (TIK < Fixed) {
        // If the enum isn't fixed-layout, get the runtime to do this for us.
        llvm::Value *payload = emitPayloadMetadata(IGF);
        llvm::Value *caseIndex;
        if (elt == getPayloadElement()) {
          caseIndex = llvm::ConstantInt::getSigned(IGF.IGM.Int32Ty, -1);
        } else {
          auto found = std::find_if(ElementsWithNoPayload.begin(),
                                    ElementsWithNoPayload.end(),
                                    [&](Element a) { return a.decl == elt; });
          assert(found != ElementsWithNoPayload.end() &&
                 "case not in enum?!");
          unsigned caseIndexVal = found - ElementsWithNoPayload.begin();
          caseIndex = llvm::ConstantInt::get(IGF.IGM.Int32Ty, caseIndexVal);
        }
        
        llvm::Value *numEmptyCases = llvm::ConstantInt::get(IGF.IGM.Int32Ty,
                                                  ElementsWithNoPayload.size());
        
        llvm::Value *opaqueAddr
          = IGF.Builder.CreateBitCast(enumAddr.getAddress(),
                                      IGF.IGM.OpaquePtrTy);
        
        IGF.Builder.CreateCall4(IGF.IGM.getStoreEnumTagSinglePayloadFn(),
                                opaqueAddr, payload, caseIndex, numEmptyCases);
        
        return;
      }
      
      if (elt == getPayloadElement()) {
        // The data occupies the entire payload. If we have extra tag bits,
        // zero them out.
        if (ExtraTagBitCount > 0)
          IGF.Builder.CreateStore(getZeroExtraTagConstant(IGF.IGM),
                                  projectExtraTagBits(IGF, enumAddr));
        return;
      }
      
      // Store the discriminator for the no-payload case.
      llvm::Value *payload, *extraTag;
      std::tie(payload, extraTag) = getNoPayloadCaseValue(IGF, elt);
      
      IGF.Builder.CreateStore(payload, projectPayload(IGF, enumAddr));
      if (ExtraTagBitCount > 0)
        IGF.Builder.CreateStore(extraTag, projectExtraTagBits(IGF, enumAddr));
    }
    
    void initializeValueWitnessTable(IRGenFunction &IGF,
                                     llvm::Value *metadata,
                                     llvm::Value *vwtable) const override
    {
      // Fixed-size enums don't need dynamic witness table initialization.
      if (TIK >= Fixed) return;

      // Ask the runtime to do our layout using the payload metadata and number
      // of empty cases.
      auto payloadMetadata = emitPayloadMetadata(IGF);
      auto emptyCasesVal = llvm::ConstantInt::get(IGF.IGM.Int32Ty,
                                                  ElementsWithNoPayload.size());
      
      IGF.Builder.CreateCall3(
                    IGF.IGM.getInitEnumValueWitnessTableSinglePayloadFn(),
                    vwtable, payloadMetadata, emptyCasesVal);
    }
    
    /// \group Extra inhabitants
    
    // Extra inhabitants from the payload that we didn't use for our empty cases
    // are available to outer enums.
    // FIXME: If we spilled extra tag bits, we could offer spare bits from the
    // tag.
    
    bool mayHaveExtraInhabitants() const override {
      if (TIK >= Fixed)
        return getFixedExtraInhabitantCount() > 0;

      return getPayloadTypeInfo().mayHaveExtraInhabitants();
    }
    
    unsigned getFixedExtraInhabitantCount() const override {
      unsigned payloadXI
        = getFixedPayloadTypeInfo().getFixedExtraInhabitantCount();
      
      unsigned numEmptyCases = ElementsWithNoPayload.size();
      
      if (payloadXI <= numEmptyCases)
        return 0;
      
      return payloadXI - numEmptyCases;
    }
    
    llvm::ConstantInt *
    getFixedExtraInhabitantValue(IRGenModule &IGM,
                                 unsigned bits,
                                 unsigned index) const override {
      return getFixedPayloadTypeInfo()
        .getFixedExtraInhabitantValue(IGM, bits,
                                      index + ElementsWithNoPayload.size());
    }
    
    llvm::Value *
    getExtraInhabitantIndex(IRGenFunction &IGF,
                            Address src) const override {
      auto payload = projectPayloadData(IGF, src);
      llvm::Value *index
        = getPayloadTypeInfo().getExtraInhabitantIndex(IGF, payload);
      
      // Offset the payload extra inhabitant index by the number of inhabitants
      // we used. If less than zero, it's a valid value of the enum type.
      index = IGF.Builder.CreateSub(index,
         llvm::ConstantInt::get(IGF.IGM.Int32Ty, ElementsWithNoPayload.size()));
      auto valid = IGF.Builder.CreateICmpSLT(index,
                                   llvm::ConstantInt::get(IGF.IGM.Int32Ty, 0));
      index = IGF.Builder.CreateSelect(valid,
                              llvm::ConstantInt::getSigned(IGF.IGM.Int32Ty, -1),
                              index);
      return index;
    }
    
    void storeExtraInhabitant(IRGenFunction &IGF,
                              llvm::Value *index,
                              Address dest) const override {
      // Offset the index to skip the extra inhabitants we used.
      index = IGF.Builder.CreateAdd(index,
          llvm::ConstantInt::get(IGF.IGM.Int32Ty, ElementsWithNoPayload.size()));
      
      auto payload = projectPayloadData(IGF, dest);
      getPayloadTypeInfo().storeExtraInhabitant(IGF, index, payload);
    }
  };

  class MultiPayloadEnumImplStrategy final
    : public PayloadEnumImplStrategyBase
  {
    // The spare bits shared by all payloads, if any.
    // Invariant: The size of the bit vector is the size of the payload in bits,
    // rounded up to a byte boundary.
    llvm::BitVector CommonSpareBits;

    // The number of tag values used for no-payload cases.
    unsigned NumEmptyElementTags = ~0u;

  public:
    MultiPayloadEnumImplStrategy(TypeInfoKind tik, unsigned NumElements,
                                  std::vector<Element> &&WithPayload,
                                  std::vector<Element> &&WithRecursivePayload,
                                  std::vector<Element> &&WithNoPayload)
      : PayloadEnumImplStrategyBase(tik, NumElements,
                                     std::move(WithPayload),
                                     std::move(WithRecursivePayload),
                                     std::move(WithNoPayload))
    {
      assert(ElementsWithPayload.size() > 1);
    }
    
    TypeInfo *completeEnumTypeLayout(TypeConverter &TC,
                                      CanType type,
                                      EnumDecl *theEnum,
                                      llvm::StructType *enumTy) override;
    
    Size getExtraTagBitOffset() const override {
      return Size((CommonSpareBits.size()+7U)/8U);
    }
    
  private:
    /// The number of empty cases representable by each tag value.
    /// Equal to the size of the payload minus the spare bits used for tags.
    unsigned getNumCaseBits() const {
      return CommonSpareBits.size() - CommonSpareBits.count();
    }
    
    unsigned getNumCasesPerTag() const {
      unsigned numCaseBits = getNumCaseBits();
      return numCaseBits >= 32
        ? 0x80000000 : 1 << numCaseBits;
    }
    
    /// Extract the payload-discriminating tag from a payload and optional
    /// extra tag value.
    llvm::Value *extractPayloadTag(IRGenFunction &IGF,
                                   llvm::Value *payload,
                                   llvm::Value *extraTagBits) const {
      unsigned numSpareBits = CommonSpareBits.count();
      llvm::Value *tag = nullptr;
      unsigned numTagBits = numSpareBits + ExtraTagBitCount;
      
      // Get the tag bits from spare bits, if any.
      if (numSpareBits > 0) {
        tag = emitGatherSpareBits(IGF, CommonSpareBits, payload, 0, numTagBits);
      }
      
      // Get the extra tag bits, if any.
      if (ExtraTagBitCount > 0) {
        assert(extraTagBits);
        if (!tag) {
          return extraTagBits;
        } else {
          extraTagBits = IGF.Builder.CreateZExt(extraTagBits, tag->getType());
          extraTagBits = IGF.Builder.CreateShl(extraTagBits,
                                               numTagBits - ExtraTagBitCount);
          return IGF.Builder.CreateOr(tag, extraTagBits);
        }
      }
      assert(!extraTagBits);
      return tag;
    }
    
  public:
    void emitValueSwitch(IRGenFunction &IGF,
                         Explosion &value,
                         ArrayRef<std::pair<EnumElementDecl*,
                                            llvm::BasicBlock*>> dests,
                         llvm::BasicBlock *defaultDest) const override {
      auto &C = IGF.IGM.getLLVMContext();
      
      // Create a map of the destination blocks for quicker lookup.
      llvm::DenseMap<EnumElementDecl*,llvm::BasicBlock*> destMap(dests.begin(),
                                                                  dests.end());
      
      // Create an unreachable branch for unreachable switch defaults.
      auto *unreachableBB = llvm::BasicBlock::Create(C);
      
      // If there was no default branch in SIL, use the unreachable branch as
      // the default.
      if (!defaultDest)
        defaultDest = unreachableBB;
      
      auto blockForCase = [&](EnumElementDecl *theCase) -> llvm::BasicBlock* {
        auto found = destMap.find(theCase);
        if (found == destMap.end())
          return defaultDest;
        else
          return found->second;
      };
      
      llvm::Value *payload = value.claimNext();
      llvm::Value *extraTagBits = nullptr;
      if (ExtraTagBitCount > 0)
        extraTagBits = value.claimNext();
      
      // Extract and switch on the tag bits.
      llvm::Value *tag = extractPayloadTag(IGF, payload, extraTagBits);
      unsigned numTagBits
        = cast<llvm::IntegerType>(tag->getType())->getBitWidth();
      
      auto *tagSwitch = IGF.Builder.CreateSwitch(tag, unreachableBB,
                             ElementsWithPayload.size() + NumEmptyElementTags);
      
      // Switch over the tag bits for payload cases.
      unsigned tagIndex = 0;
      for (auto &payloadCasePair : ElementsWithPayload) {
        EnumElementDecl *payloadCase = payloadCasePair.decl;
        tagSwitch->addCase(llvm::ConstantInt::get(C,APInt(numTagBits,tagIndex)),
                           blockForCase(payloadCase));
        ++tagIndex;
      }
      
      // Switch over the no-payload cases.
      unsigned casesPerTag = getNumCasesPerTag();
      
      auto elti = ElementsWithNoPayload.begin(),
           eltEnd = ElementsWithNoPayload.end();
      
      for (unsigned i = 0; i < NumEmptyElementTags; ++i) {
        assert(elti != eltEnd &&
               "ran out of cases before running out of extra tags?");
        auto *tagBB = llvm::BasicBlock::Create(C);
        tagSwitch->addCase(llvm::ConstantInt::get(C,APInt(numTagBits,tagIndex)),
                           tagBB);
        
        // Switch over the cases for this tag.
        IGF.Builder.emitBlock(tagBB);
        auto *caseSwitch = IGF.Builder.CreateSwitch(payload, unreachableBB);
        for (unsigned idx = 0; idx < casesPerTag && elti != eltEnd; ++idx) {
          auto v = interleaveSpareBits(IGF.IGM, CommonSpareBits,
                                       CommonSpareBits.size(),
                                       tagIndex, idx);
          caseSwitch->addCase(v, blockForCase(elti->decl));
          ++elti;
        }
        
        ++tagIndex;
      }
      
      // Delete the unreachable default block if we didn't use it, or emit it
      // if we did.
      if (unreachableBB->use_empty()) {
        delete unreachableBB;
      } else {
        IGF.Builder.emitBlock(unreachableBB);
        IGF.Builder.CreateUnreachable();
      }
    }

    void emitIndirectSwitch(IRGenFunction &IGF,
                            Address addr,
                            ArrayRef<std::pair<EnumElementDecl*,
                            llvm::BasicBlock*>> dests,
                            llvm::BasicBlock *defaultDest) const override {
      if (TIK >= Fixed) {
        // Load the fixed-size representation and switch directly.
        Explosion value(ExplosionKind::Minimal);
        loadForSwitch(IGF, addr, value);
        return emitValueSwitch(IGF, value, dests, defaultDest);
      }
      
      // Use the runtime to dynamically switch.
      llvm_unreachable("dynamic switch for multi-payload enum not implemented");
    }
    
  private:
    void projectPayloadValue(IRGenFunction &IGF,
                        llvm::Value *payload,
                        unsigned payloadTag,
                        const LoadableTypeInfo &payloadTI,
                        Explosion &out) const {
      // If we have spare bits, we have to mask out any set tag bits packed
      // there.
      if (CommonSpareBits.any()) {
        unsigned spareBitCount = CommonSpareBits.count();
        if (spareBitCount < 32)
          payloadTag &= (1U << spareBitCount) - 1U;
        if (payloadTag != 0) {
          APInt mask = ~getAPIntFromBitVector(CommonSpareBits);
          auto maskVal = llvm::ConstantInt::get(IGF.IGM.getLLVMContext(),
                                                mask);
          payload = IGF.Builder.CreateAnd(payload, maskVal);
        }
      }
      
      // Unpack the payload.
      payloadTI.unpackEnumPayload(IGF, payload, out, 0);
    }
    
  public:
    void emitValueProject(IRGenFunction &IGF,
                          Explosion &inValue,
                          EnumElementDecl *theCase,
                          Explosion &out) const override {
      auto foundPayload = std::find_if(ElementsWithPayload.begin(),
                                       ElementsWithPayload.end(),
             [&](const Element &e) { return e.decl == theCase; });
      
      // Non-payload cases project to an empty explosion.
      if (foundPayload == ElementsWithPayload.end()) {
        inValue.claim(getExplosionSize(inValue.getKind()));
        return;
      }
      
      llvm::Value *payload = inValue.claimNext();
      // We don't need the tag bits.
      if (ExtraTagBitCount > 0)
        inValue.claimNext();
      
      // Unpack the payload.
      projectPayloadValue(IGF, payload,
                          foundPayload - ElementsWithPayload.begin(),
                     cast<LoadableTypeInfo>(*foundPayload->ti), out);
    }
    
    llvm::Value *packEnumPayload(IRGenFunction &IGF, Explosion &src,
                                  unsigned bitWidth,
                                  unsigned offset) const override {
      PackEnumPayload pack(IGF, bitWidth);
      // Pack the payload.
      pack.addAtOffset(src.claimNext(), offset);
      // Pack the extra bits, if any.
      if (ExtraTagBitCount > 0) {
        pack.addAtOffset(src.claimNext(), CommonSpareBits.size() + offset);
      }
      return pack.get();
    }
    
    void unpackEnumPayload(IRGenFunction &IGF, llvm::Value *outerPayload,
                            Explosion &dest, unsigned offset) const override {
      UnpackEnumPayload unpack(IGF, outerPayload);
      // Unpack the payload.
      dest.add(unpack.claimAtOffset(getStorageType()->getElementType(0),
                                    offset));
      // Unpack the extra bits, if any.
      if (ExtraTagBitCount > 0) {
        dest.add(unpack.claimAtOffset(getStorageType()->getElementType(1),
                                      CommonSpareBits.size() + offset));
      }
    }
    
  private:
    void emitPayloadInjection(IRGenFunction &IGF,
                              const FixedTypeInfo &payloadTI,
                              Explosion &params, Explosion &out,
                              unsigned tag) const {
      // Pack the payload.
      auto &loadablePayloadTI = cast<LoadableTypeInfo>(payloadTI); // FIXME
      llvm::Value *payload = loadablePayloadTI.packEnumPayload(IGF, params,
                                                    CommonSpareBits.size(), 0);
      
      // If we have spare bits, pack tag bits into them.
      unsigned numSpareBits = CommonSpareBits.count();
      if (numSpareBits > 0) {
        llvm::ConstantInt *tagMask
          = interleaveSpareBits(IGF.IGM, CommonSpareBits,CommonSpareBits.size(),
                                tag, 0);
        payload = IGF.Builder.CreateOr(payload, tagMask);
      }
      
      out.add(payload);
      
      // If we have extra tag bits, pack the remaining tag bits into them.
      if (ExtraTagBitCount > 0) {
        tag >>= numSpareBits;
        auto extra = llvm::ConstantInt::get(IGF.IGM.getLLVMContext(),
                                            APInt(ExtraTagBitCount, tag));
        out.add(extra);
      }
    }
    
    std::pair<llvm::Value*, llvm::Value*>
    getNoPayloadCaseValue(IRGenFunction &IGF, unsigned index) const {
      // Figure out the tag and payload for the empty case.
      unsigned numCaseBits = getNumCaseBits();
      unsigned tag, tagIndex;
      if (numCaseBits >= 32) {
        tag = ElementsWithPayload.size();
        tagIndex = index;
      } else {
        tag = (index >> numCaseBits) + ElementsWithPayload.size();
        tagIndex = index & ((1 << numCaseBits) - 1);
      }
      
      llvm::Value *payload;
      llvm::Value *extraTag = nullptr;
      unsigned numSpareBits = CommonSpareBits.count();
      if (numSpareBits > 0) {
        // If we have spare bits, pack tag bits into them.
        payload = interleaveSpareBits(IGF.IGM,
                                      CommonSpareBits, CommonSpareBits.size(),
                                      tag, tagIndex);
      } else {
        // Otherwise the payload is just the index.
        payload = llvm::ConstantInt::get(IGF.IGM.getLLVMContext(),
                                         APInt(CommonSpareBits.size(), tagIndex));
      }
      
      // If we have extra tag bits, pack the remaining tag bits into them.
      if (ExtraTagBitCount > 0) {
        tag >>= numSpareBits;
        extraTag = llvm::ConstantInt::get(IGF.IGM.getLLVMContext(),
                                            APInt(ExtraTagBitCount, tag));
      }
      return {payload, extraTag};
    }
    
    void emitNoPayloadInjection(IRGenFunction &IGF, Explosion &out,
                                unsigned index) const {
      llvm::Value *payload, *extraTag;
      std::tie(payload, extraTag) = getNoPayloadCaseValue(IGF, index);
      out.add(payload);
      if (ExtraTagBitCount > 0) {
        assert(extraTag);
        out.add(extraTag);
      }
    }
    
    void forNontrivialPayloads(IRGenFunction &IGF,
               llvm::Value *payload, llvm::Value *extraTagBits,
               std::function<void (unsigned, const TypeInfo &)> f) const
    {
      auto *endBB = llvm::BasicBlock::Create(IGF.IGM.getLLVMContext());
      
      llvm::Value *tag = extractPayloadTag(IGF, payload, extraTagBits);
      auto *swi = IGF.Builder.CreateSwitch(tag, endBB);
      auto *tagTy = cast<llvm::IntegerType>(tag->getType());
      
      // Handle nontrivial tags.
      unsigned tagIndex = 0;
      for (auto &payloadCasePair : ElementsWithPayload) {
        auto &payloadTI = *payloadCasePair.ti;
        
        // Trivial payloads don't need any work.
        if (payloadTI.isPOD(ResilienceScope::Local)) {
          ++tagIndex;
          continue;
        }
        
        // Unpack and handle nontrivial payloads.
        auto *caseBB = llvm::BasicBlock::Create(IGF.IGM.getLLVMContext());
        swi->addCase(llvm::ConstantInt::get(tagTy, tagIndex), caseBB);
        
        IGF.Builder.emitBlock(caseBB);
        f(tagIndex, payloadTI);
        IGF.Builder.CreateBr(endBB);
        
        ++tagIndex;
      }
      
      IGF.Builder.emitBlock(endBB);
    }
    
  public:
    void emitValueInjection(IRGenFunction &IGF,
                            EnumElementDecl *elt,
                            Explosion &params,
                            Explosion &out) const {
      // See whether this is a payload or empty case we're emitting.
      auto payloadI = std::find_if(ElementsWithPayload.begin(),
                                   ElementsWithPayload.end(),
                               [&](const Element &e) { return e.decl == elt; });
      if (payloadI != ElementsWithPayload.end())
        return emitPayloadInjection(IGF, cast<FixedTypeInfo>(*payloadI->ti),
                                    params, out,
                                    payloadI - ElementsWithPayload.begin());

      auto emptyI = std::find_if(ElementsWithNoPayload.begin(),
                                 ElementsWithNoPayload.end(),
                               [&](const Element &e) { return e.decl == elt; });
      assert(emptyI != ElementsWithNoPayload.end() && "case not in enum");
      emitNoPayloadInjection(IGF, out, emptyI - ElementsWithNoPayload.begin());
    }
    
    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest)
    const override {
      if (isPOD(ResilienceScope::Local)) {
        reexplode(IGF, src, dest);
        return;
      }
      
      llvm::Value *payload = src.claimNext();
      llvm::Value *extraTagBits = ExtraTagBitCount > 0
        ? src.claimNext() : nullptr;
      
      forNontrivialPayloads(IGF, payload, extraTagBits,
        [&](unsigned tagIndex, const TypeInfo &ti) {
          auto &lti = cast<LoadableTypeInfo>(ti);
          Explosion value(ExplosionKind::Minimal);
          projectPayloadValue(IGF, payload, tagIndex, lti, value);

          Explosion tmp(value.getKind());
          lti.copy(IGF, value, tmp);
          tmp.claimAll(); // FIXME: repack if not bit-identical
        });
      
      dest.add(payload);
      if (extraTagBits)
        dest.add(extraTagBits);
    }
    
    void consume(IRGenFunction &IGF, Explosion &src) const override {
      if (isPOD(ResilienceScope::Local)) {
        src.claim(getExplosionSize(src.getKind()));
        return;
      }
      
      llvm::Value *payload = src.claimNext();
      llvm::Value *extraTagBits = ExtraTagBitCount > 0
        ? src.claimNext() : nullptr;
      
      forNontrivialPayloads(IGF, payload, extraTagBits,
        [&](unsigned tagIndex, const TypeInfo &ti) {
          auto &lti = cast<LoadableTypeInfo>(ti);
          Explosion value(ExplosionKind::Minimal);
          projectPayloadValue(IGF, payload, tagIndex, lti, value);

          lti.consume(IGF, value);
        });
    }
  
  private:
    /// Emit an reassignment sequence from an enum at one address to another.
    void emitIndirectAssign(IRGenFunction &IGF,
                            Address dest, Address src, IsTake_t isTake) const {
      auto &C = IGF.IGM.getLLVMContext();
      
      if (isPOD(ResilienceScope::Local))
        return emitPrimitiveCopy(IGF, dest, src);
      
      // If the enum is loadable, it's better to do this directly using values,
      // so we don't need to RMW tag bits in place.
      if (TI->isLoadable()) {
        Explosion tmpSrc(ExplosionKind::Minimal),
                  tmpOld(ExplosionKind::Minimal);
        if (isTake)
          loadAsTake(IGF, src, tmpSrc);
        else
          loadAsCopy(IGF, src, tmpSrc);
        
        loadAsTake(IGF, dest, tmpOld);
        initialize(IGF, tmpSrc, dest);
        consume(IGF, tmpOld);
        return;
      }
      
      auto *endBB = llvm::BasicBlock::Create(C);
      
      // Sanity-check whether the source and destination alias.
      llvm::Value *alias = IGF.Builder.CreateICmpEQ(dest.getAddress(),
                                                    src.getAddress());
      auto *noAliasBB = llvm::BasicBlock::Create(C);
      IGF.Builder.CreateCondBr(alias, endBB, noAliasBB);
      IGF.Builder.emitBlock(noAliasBB);
      
      // Destroy the old value.
      destroy(IGF, dest);
      
      // Reinitialize with the new value.
      emitIndirectInitialize(IGF, dest, src, isTake);
      
      IGF.Builder.CreateBr(endBB);
      IGF.Builder.emitBlock(endBB);
    }
    
    void emitIndirectInitialize(IRGenFunction &IGF,
                              Address dest, Address src, IsTake_t isTake) const{
      auto &C = IGF.IGM.getLLVMContext();

      if (isPOD(ResilienceScope::Local))
        return emitPrimitiveCopy(IGF, dest, src);
      
      // If the enum is loadable, it's better to do this directly using values,
      // so we don't need to RMW tag bits in place.
      if (TI->isLoadable()) {
        Explosion tmpSrc(ExplosionKind::Minimal);
        if (isTake)
          loadAsTake(IGF, src, tmpSrc);
        else
          loadAsCopy(IGF, src, tmpSrc);
        initialize(IGF, tmpSrc, dest);
        return;
      }

      llvm::Value *payload, *extraTagBits;
      std::tie(payload, extraTagBits)
        = emitPrimitiveLoadPayloadAndExtraTag(IGF, src);

      auto *endBB = llvm::BasicBlock::Create(C);

      /// Switch out nontrivial payloads.
      auto *trivialBB = llvm::BasicBlock::Create(C);
      llvm::Value *tag = extractPayloadTag(IGF, payload, extraTagBits);
      auto *swi = IGF.Builder.CreateSwitch(tag, trivialBB);
      auto *tagTy = cast<llvm::IntegerType>(tag->getType());
      
      unsigned tagIndex = 0;
      for (auto &payloadCasePair : ElementsWithPayload) {
        auto &payloadTI = *payloadCasePair.ti;
        // Trivial payloads can all share the default path.
        if (payloadTI.isPOD(ResilienceScope::Local)) {
          ++tagIndex;
          continue;
        }
        
        // For nontrivial payloads, we need to copy/take the payload using its
        // value semantics.
        auto *caseBB = llvm::BasicBlock::Create(C);
        swi->addCase(llvm::ConstantInt::get(tagTy, tagIndex), caseBB);
        IGF.Builder.emitBlock(caseBB);

        // Temporarily clear the tag bits from the source so we can use the
        // data.
        preparePayloadForLoad(IGF, src, tagIndex);
        
        // Do the take/copy of the payload.
        Address srcData = IGF.Builder.CreateBitCast(src,
                                payloadTI.getStorageType()->getPointerTo());
        Address destData = IGF.Builder.CreateBitCast(dest,
                                  payloadTI.getStorageType()->getPointerTo());

        if (isTake) {
          payloadTI.initializeWithTake(IGF, destData, srcData);
          // We don't need to preserve the old value.
        } else {
          payloadTI.initializeWithCopy(IGF, destData, srcData);
          // Replant the tag bits, if any, in the source.
          storePayloadTag(IGF, src, tagIndex);
        }
        
        // Plant spare bit tag bits, if any, into the new value.
        storePayloadTag(IGF, dest, tagIndex);
        IGF.Builder.CreateBr(endBB);
        
        ++tagIndex;
      }
      
      // For trivial payloads (including no-payload cases), we can just
      // primitive-store to the destination.
      IGF.Builder.emitBlock(trivialBB);
      emitPrimitiveStorePayloadAndExtraTag(IGF, dest, payload, extraTagBits);
      IGF.Builder.CreateBr(endBB);
      
      IGF.Builder.emitBlock(endBB);
    }
    
  public:
    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src)
    const override {
      emitIndirectAssign(IGF, dest, src, IsNotTake);
    }
    
    void assignWithTake(IRGenFunction &IGF, Address dest, Address src)
    const override {
      emitIndirectAssign(IGF, dest, src, IsTake);
    }
    
    void initializeWithCopy(IRGenFunction &IGF, Address dest, Address src)
    const override {
      emitIndirectInitialize(IGF, dest, src, IsNotTake);
    }
    
    void initializeWithTake(IRGenFunction &IGF, Address dest, Address src)
    const override {
      emitIndirectInitialize(IGF, dest, src, IsTake);
    }
    
  private:
    /// Clear any tag bits stored in the payload area of the given address.
    void preparePayloadForLoad(IRGenFunction &IGF, Address enumAddr,
                               unsigned tagIndex) const {
      // If the case has non-zero tag bits stored in spare bits, we need to
      // mask them out before the data can be read.
      unsigned numSpareBits = CommonSpareBits.count();
      if (numSpareBits > 0) {
        unsigned spareTagBits = numSpareBits >= 32
          ? tagIndex : tagIndex & ((1U << numSpareBits) - 1U);
        
        if (spareTagBits != 0) {
          Address payloadAddr = projectPayload(IGF, enumAddr);
          llvm::Value *payloadBits = IGF.Builder.CreateLoad(payloadAddr);
          auto *spareBitMask = llvm::ConstantInt::get(IGF.IGM.getLLVMContext(),
                                                      ~getAPIntFromBitVector(CommonSpareBits));
          payloadBits = IGF.Builder.CreateAnd(payloadBits, spareBitMask);
          IGF.Builder.CreateStore(payloadBits, payloadAddr);
        }
      }
    }
    
    void destroy(IRGenFunction &IGF, Address addr) const override {
      if (isPOD(ResilienceScope::Local))
        return;
      
      // If loadable, it's better to do this directly to the value than
      // in place, so we don't need to RMW out the tag bits in memory.
      if (TI->isLoadable()) {
        Explosion tmp(ExplosionKind::Minimal);
        loadAsTake(IGF, addr, tmp);
        consume(IGF, tmp);
        return;
      }
      
      llvm::Value *payload, *extraTagBits;
      std::tie(payload, extraTagBits)
        = emitPrimitiveLoadPayloadAndExtraTag(IGF, addr);
      
      forNontrivialPayloads(IGF, payload, extraTagBits,
        [&](unsigned tagIndex, const TypeInfo &ti) {
          // Clear tag bits out of the payload area, if any.
          preparePayloadForLoad(IGF, addr, tagIndex);
          // Destroy the data.
          Address dataAddr = IGF.Builder.CreateBitCast(addr,
                                        ti.getStorageType()->getPointerTo());
          ti.destroy(IGF, dataAddr);
        });
    }
    
    Address projectDataForStore(IRGenFunction &IGF,
                                EnumElementDecl *elt,
                                Address enumAddr) const override {
      auto payloadI = std::find_if(ElementsWithPayload.begin(),
                                   ElementsWithPayload.end(),
         [&](const Element &e) { return e.decl == elt; });

      assert(payloadI != ElementsWithPayload.end() &&
             "cannot project a no-payload case");
      
      // Payloads are all placed at the beginning of the value.
      return IGF.Builder.CreateBitCast(enumAddr,
                               payloadI->ti->getStorageType()->getPointerTo());
    }

  private:
    void storePayloadTag(IRGenFunction &IGF,
                         Address enumAddr, unsigned index) const {
      // If the tag has spare bits, we need to mask them into the
      // payload area.
      unsigned numSpareBits = CommonSpareBits.count();
      if (numSpareBits > 0) {
        unsigned spareTagBits = numSpareBits >= 32
          ? index : index & ((1U << numSpareBits) - 1U);

        // Mask the spare bits into the payload area.
        Address payloadAddr = projectPayload(IGF, enumAddr);
        llvm::Value *payloadBits = IGF.Builder.CreateLoad(payloadAddr);
        auto *spareBitMask = llvm::ConstantInt::get(IGF.IGM.getLLVMContext(),
                                      ~getAPIntFromBitVector(CommonSpareBits));
        llvm::Value *tagBitMask
          = interleaveSpareBits(IGF.IGM, CommonSpareBits, CommonSpareBits.size(),
                                spareTagBits, 0);
        payloadBits = IGF.Builder.CreateAnd(payloadBits, spareBitMask);
        if (spareTagBits != 0)
          payloadBits = IGF.Builder.CreateOr(payloadBits, tagBitMask);
        IGF.Builder.CreateStore(payloadBits, payloadAddr);
      }

      // Initialize the extra tag bits, if we have them.
      if (ExtraTagBitCount > 0) {
        unsigned extraTagBits = index >> numSpareBits;
        auto *extraTagValue = llvm::ConstantInt::get(IGF.IGM.getLLVMContext(),
                                        APInt(ExtraTagBitCount, extraTagBits));
        IGF.Builder.CreateStore(extraTagValue,
                                projectExtraTagBits(IGF, enumAddr));
      }
    }
    
    void storeNoPayloadTag(IRGenFunction &IGF, Address enumAddr,
                           unsigned index) const {
      // We can just primitive-store the representation for the empty case.
      llvm::Value *payload, *extraTag;
      std::tie(payload, extraTag) = getNoPayloadCaseValue(IGF, index);
      IGF.Builder.CreateStore(payload, projectPayload(IGF, enumAddr));
      if (ExtraTagBitCount > 0) {
        assert(extraTag);
        IGF.Builder.CreateStore(extraTag, projectExtraTagBits(IGF, enumAddr));
      }
    }
    
  public:
    void storeTag(IRGenFunction &IGF,
                  EnumElementDecl *elt,
                  Address enumAddr) const override {
      // See whether this is a payload or empty case we're emitting.
      auto payloadI = std::find_if(ElementsWithPayload.begin(),
                                   ElementsWithPayload.end(),
                               [&](const Element &e) { return e.decl == elt; });
      if (payloadI != ElementsWithPayload.end())
        return storePayloadTag(IGF,
                               enumAddr,
                               payloadI - ElementsWithPayload.begin());
      
      auto emptyI = std::find_if(ElementsWithNoPayload.begin(),
                                 ElementsWithNoPayload.end(),
                               [&](const Element &e) { return e.decl == elt; });
      assert(emptyI != ElementsWithNoPayload.end() && "case not in enum");
      storeNoPayloadTag(IGF, enumAddr, emptyI - ElementsWithNoPayload.begin());
    }
    
    Address destructiveProjectDataForLoad(IRGenFunction &IGF,
                                          EnumElementDecl *elt,
                                          Address enumAddr) const override {
      auto payloadI = std::find_if(ElementsWithPayload.begin(),
                               ElementsWithPayload.end(),
                               [&](const Element &e) { return e.decl == elt; });
      
      assert(payloadI != ElementsWithPayload.end() &&
             "cannot project a no-payload case");
      
      unsigned index = payloadI - ElementsWithPayload.begin();
      
      preparePayloadForLoad(IGF, enumAddr, index);
      
      // Payloads are all placed at the beginning of the value.
      return IGF.Builder.CreateBitCast(enumAddr,
                               payloadI->ti->getStorageType()->getPointerTo());
    }

    void initializeValueWitnessTable(IRGenFunction &IGF,
                                     llvm::Value *metadata,
                                     llvm::Value *vwtable) const override {
      // FIXME
    }
    
    /// \group Extra inhabitants
    
    // TODO
    
    bool mayHaveExtraInhabitants() const override { return false; }
    
    llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                         Address src) const override {
      llvm_unreachable("extra inhabitants for multi-payload enums not implemented");
    }
    
    void storeExtraInhabitant(IRGenFunction &IGF,
                              llvm::Value *index,
                              Address dest) const override {
      llvm_unreachable("extra inhabitants for multi-payload enums not implemented");
    }
    
    unsigned getFixedExtraInhabitantCount() const override {
      return 0;
    }
    
    llvm::ConstantInt *
    getFixedExtraInhabitantValue(IRGenModule &IGM,
                                 unsigned bits,
                                 unsigned index) const override {
      llvm_unreachable("extra inhabitants for multi-payload enums not implemented");
    }
  };
} // end anonymous namespace

EnumImplStrategy *EnumImplStrategy::get(TypeConverter &TC,
                                          CanType type,
                                          EnumDecl *theEnum)
{
  unsigned numElements = 0;
  TypeInfoKind tik = Loadable;
  std::vector<Element> elementsWithPayload;
  std::vector<Element> elementsWithRecursivePayload;
  std::vector<Element> elementsWithNoPayload;
  
  for (auto elt : theEnum->getAllElements()) {
    numElements++;
    
    // Compute whether this gives us an apparent payload or dynamic layout.
    // Note that we do *not* apply substitutions from a bound generic instance
    // yet. We want all instances of a generic enum to share an implementation
    // strategy.
    Type argType = elt->getArgumentType();
    if (argType.isNull()) {
      elementsWithNoPayload.push_back({elt, nullptr});
      continue;
    }
    auto *argTI = TC.tryGetCompleteTypeInfo(argType->getCanonicalType());
    if (!argTI) {
      elementsWithRecursivePayload.push_back({elt, nullptr});
      continue;
    }

    auto loadableArgTI = dyn_cast<LoadableTypeInfo>(argTI);
    if (loadableArgTI
        && loadableArgTI->getExplosionSize(ExplosionKind::Minimal) == 0) {
      elementsWithNoPayload.push_back({elt, nullptr});
    } else {
      // *Now* apply the substitutions and get the type info for the instance's
      // payload type, since we know this case carries an apparent payload in
      // the generic case.
      auto *substArgTI = argTI;
      if (type->is<BoundGenericType>()) {
        Type substArgTy = type->getTypeOfMember(theEnum->getModuleContext(),
                                                elt, nullptr,
                                                elt->getArgumentType());
        substArgTI = &TC.getCompleteTypeInfo(substArgTy->getCanonicalType());
      }
      
      elementsWithPayload.push_back({elt, substArgTI});
      if (!substArgTI->isFixedSize())
        tik = Opaque;
      else if (!substArgTI->isLoadable() && tik > Fixed)
        tik = Fixed;
    }
  }

  // FIXME recursive enums
  if (!elementsWithRecursivePayload.empty()) {
    TC.IGM.unimplemented(theEnum->getLoc(), "recursive enum layout");
    exit(1);
  }
  
  assert(numElements == elementsWithPayload.size()
           + elementsWithRecursivePayload.size()
           + elementsWithNoPayload.size()
         && "not all elements accounted for");
  
  if (numElements <= 1)
    return new SingletonEnumImplStrategy(tik, numElements,
                                    std::move(elementsWithPayload),
                                    std::move(elementsWithRecursivePayload),
                                    std::move(elementsWithNoPayload));
  if (elementsWithPayload.size() > 1)
    return new MultiPayloadEnumImplStrategy(tik, numElements,
                                    std::move(elementsWithPayload),
                                    std::move(elementsWithRecursivePayload),
                                    std::move(elementsWithNoPayload));
  if (elementsWithPayload.size() == 1)
    return new SinglePayloadEnumImplStrategy(tik, numElements,
                                    std::move(elementsWithPayload),
                                    std::move(elementsWithRecursivePayload),
                                    std::move(elementsWithNoPayload));

  return new NoPayloadEnumImplStrategy(tik, numElements,
                                      std::move(elementsWithPayload),
                                      std::move(elementsWithRecursivePayload),
                                      std::move(elementsWithNoPayload));
}

namespace {
  /// Common base template for enum type infos.
  template<typename BaseTypeInfo>
  class EnumTypeInfoBase : public BaseTypeInfo {
  public:
    EnumImplStrategy &Strategy;
    
    template<typename...AA>
    EnumTypeInfoBase(EnumImplStrategy &strategy, AA &&...args)
      : BaseTypeInfo(std::forward<AA>(args)...), Strategy(strategy) {}
    
    llvm::StructType *getStorageType() const {
      return cast<llvm::StructType>(TypeInfo::getStorageType());
    }
    
    /// \group Methods delegated to the EnumImplStrategy
    
    void getSchema(ExplosionSchema &s) const override {
      return Strategy.getSchema(s);
    }
    void destroy(IRGenFunction &IGF, Address addr) const override {
      return Strategy.destroy(IGF, addr);
    }
    bool isIndirectArgument(ExplosionKind kind) const override {
      return Strategy.isIndirectArgument(kind);
    }
    void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                              Address dest) const override {
      return Strategy.initializeFromParams(IGF, params, dest);
    }
    void initializeWithCopy(IRGenFunction &IGF, Address dest,
                            Address src) const override {
      return Strategy.initializeWithCopy(IGF, dest, src);
    }
    void initializeWithTake(IRGenFunction &IGF, Address dest,
                            Address src) const override {
      return Strategy.initializeWithTake(IGF, dest, src);
    }
    void assignWithCopy(IRGenFunction &IGF, Address dest,
                        Address src) const override {
      return Strategy.assignWithCopy(IGF, dest, src);
    }
    void assignWithTake(IRGenFunction &IGF, Address dest,
                        Address src) const override {
      return Strategy.assignWithTake(IGF, dest, src);
    }
    virtual void initializeValueWitnessTable(IRGenFunction &IGF,
                                         llvm::Value *metadata,
                                         llvm::Value *vwtable) const override {
      return Strategy.initializeValueWitnessTable(IGF, metadata, vwtable);
    }
    bool mayHaveExtraInhabitants() const override {
      return Strategy.mayHaveExtraInhabitants();
    }
    llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                         Address src) const override {
      return Strategy.getExtraInhabitantIndex(IGF, src);
    }
    void storeExtraInhabitant(IRGenFunction &IGF,
                              llvm::Value *index,
                              Address dest) const override {
      return Strategy.storeExtraInhabitant(IGF, index, dest);
    }
  };
  
  /// TypeInfo for fixed-layout, address-only enum types.
  class FixedEnumTypeInfo : public EnumTypeInfoBase<FixedTypeInfo> {
  public:
    FixedEnumTypeInfo(EnumImplStrategy &strategy,
                       llvm::StructType *T, Size S, llvm::BitVector SB,
                       Alignment A, IsPOD_t isPOD)
      : EnumTypeInfoBase(strategy, T, S, std::move(SB), A, isPOD) {}
    
    /// \group Methods delegated to the EnumImplStrategy

    unsigned getFixedExtraInhabitantCount() const override {
      return Strategy.getFixedExtraInhabitantCount();
    }

    llvm::ConstantInt *getFixedExtraInhabitantValue(IRGenModule &IGM,
                                                    unsigned bits,
                                                    unsigned index)
    const override {
      return Strategy.getFixedExtraInhabitantValue(IGM, bits, index);
    }
  };

  /// TypeInfo for loadable enum types.
  class LoadableEnumTypeInfo : public EnumTypeInfoBase<LoadableTypeInfo> {
  public:
    // FIXME: Derive spare bits from element layout.
    LoadableEnumTypeInfo(EnumImplStrategy &strategy,
                          llvm::StructType *T, Size S, llvm::BitVector SB,
                          Alignment A, IsPOD_t isPOD)
      : EnumTypeInfoBase(strategy, T, S, std::move(SB), A, isPOD) {}

    unsigned getExplosionSize(ExplosionKind kind) const override {
      return Strategy.getExplosionSize(kind);
    }
    void loadAsCopy(IRGenFunction &IGF, Address addr,
                    Explosion &e) const override {
      return Strategy.loadAsCopy(IGF, addr, e);
    }
    void loadAsTake(IRGenFunction &IGF, Address addr,
                    Explosion &e) const override {
      return Strategy.loadAsTake(IGF, addr, e);
    }
    void assign(IRGenFunction &IGF, Explosion &e,
                Address addr) const override {
      return Strategy.assign(IGF, e, addr);
    }
    void initialize(IRGenFunction &IGF, Explosion &e,
                    Address addr) const override {
      return Strategy.initialize(IGF, e, addr);
    }
    void reexplode(IRGenFunction &IGF, Explosion &src,
                   Explosion &dest) const override {
      return Strategy.reexplode(IGF, src, dest);
    }
    void copy(IRGenFunction &IGF, Explosion &src,
              Explosion &dest) const override {
      return Strategy.copy(IGF, src, dest);
    }
    void consume(IRGenFunction &IGF, Explosion &src) const override {
      return Strategy.consume(IGF, src);
    }
    llvm::Value *packEnumPayload(IRGenFunction &IGF,
                                  Explosion &in,
                                  unsigned bitWidth,
                                  unsigned offset) const override {
      return Strategy.packEnumPayload(IGF, in, bitWidth, offset);
    }
    void unpackEnumPayload(IRGenFunction &IGF,
                            llvm::Value *payload,
                            Explosion &dest,
                            unsigned offset) const override {
      return Strategy.unpackEnumPayload(IGF, payload, dest, offset);
    }
    unsigned getFixedExtraInhabitantCount() const override {
      return Strategy.getFixedExtraInhabitantCount();
    }
    
    llvm::ConstantInt *getFixedExtraInhabitantValue(IRGenModule &IGM,
                                                    unsigned bits,
                                                    unsigned index)
    const override {
      return Strategy.getFixedExtraInhabitantValue(IGM, bits, index);
    }
  };
  
  /// TypeInfo for dynamically-sized enum types.
  class NonFixedEnumTypeInfo
    : public EnumTypeInfoBase<WitnessSizedTypeInfo<NonFixedEnumTypeInfo>>
  {
    CanType EnumType;
  public:
    NonFixedEnumTypeInfo(EnumImplStrategy &strategy,
                          llvm::Type *irTy, CanType enumTy,
                          Alignment align, IsPOD_t pod)
      : EnumTypeInfoBase(strategy, irTy, align, pod), EnumType(enumTy) {}
    
    llvm::Value *getMetadataRef(IRGenFunction &IGF) const {
      return IGF.emitTypeMetadataRef(EnumType);
    }
    
    llvm::Value *getValueWitnessTable(IRGenFunction &IGF) const {
      auto metadata = getMetadataRef(IGF);
      return IGF.emitValueWitnessTableRefForMetadata(metadata);
    }

  };
  
  static const EnumImplStrategy &getEnumImplStrategy(IRGenModule &IGM,
                                                       SILType ty) {
    assert(ty.getEnumOrBoundGenericEnum() && "not an enum");
    auto *ti = &IGM.getTypeInfo(ty);
    if (auto *loadableTI = dyn_cast<LoadableTypeInfo>(ti))
      return loadableTI->as<LoadableEnumTypeInfo>().Strategy;
    if (auto *fti = dyn_cast<FixedTypeInfo>(ti))
      return fti->as<FixedEnumTypeInfo>().Strategy;
    return ti->as<NonFixedEnumTypeInfo>().Strategy;
  }
} // end anonymous namespace

TypeInfo *
EnumImplStrategy::getFixedEnumTypeInfo(llvm::StructType *T, Size S,
                                    llvm::BitVector SB,
                                    Alignment A, IsPOD_t isPOD) {
  TypeInfo *mutableTI;
  switch (TIK) {
  case Opaque:
    llvm_unreachable("not valid");
  case Fixed:
    mutableTI = new FixedEnumTypeInfo(*this, T, S, SB, A, isPOD);
    break;
  case Loadable:
    mutableTI = new LoadableEnumTypeInfo(*this, T, S, SB, A, isPOD);
    break;
  }
  TI = mutableTI;
  return mutableTI;
}

namespace {
  TypeInfo *
  SingletonEnumImplStrategy::completeEnumTypeLayout(TypeConverter &TC,
                                                   CanType type,
                                                   EnumDecl *theEnum,
                                                   llvm::StructType *enumTy) {
    if (ElementsWithPayload.empty()) {
      enumTy->setBody(ArrayRef<llvm::Type*>{}, /*isPacked*/ true);
      return registerEnumTypeInfo(new LoadableEnumTypeInfo(*this, enumTy,
                                                             Size(0), {},
                                                             Alignment(1),
                                                             IsPOD));
    } else {
      const TypeInfo &eltTI = *getSingleton();

      // Use the singleton element's storage type if fixed-size.
      if (eltTI.isFixedSize()) {
        llvm::Type *body[] = { eltTI.StorageType };
        enumTy->setBody(body, /*isPacked*/ true);
      } else {
        enumTy->setBody(ArrayRef<llvm::Type*>{}, /*isPacked*/ true);
      }
      
      if (TIK <= Opaque) {
        return registerEnumTypeInfo(new NonFixedEnumTypeInfo(*this, enumTy,
                                         type,
                                         eltTI.getBestKnownAlignment(),
                                         eltTI.isPOD(ResilienceScope::Local)));
      } else {
        auto &fixedEltTI = cast<FixedTypeInfo>(eltTI);
        return getFixedEnumTypeInfo(enumTy,
                                fixedEltTI.getFixedSize(),
                                fixedEltTI.getSpareBits(),
                                fixedEltTI.getFixedAlignment(),
                                fixedEltTI.isPOD(ResilienceScope::Local));
      }
    }
  }
  
  TypeInfo *
  NoPayloadEnumImplStrategy::completeEnumTypeLayout(TypeConverter &TC,
                                                    CanType type,
                                                    EnumDecl *theEnum,
                                                    llvm::StructType *enumTy) {
    // Since there are no payloads, we need just enough bits to hold a
    // discriminator.
    unsigned tagBits = llvm::Log2_32(ElementsWithNoPayload.size() - 1) + 1;
    auto tagTy = llvm::IntegerType::get(TC.IGM.getLLVMContext(), tagBits);
    // Round the physical size up to the next power of two.
    unsigned tagBytes = (tagBits + 7U)/8U;
    if (!llvm::isPowerOf2_32(tagBytes))
      tagBytes = llvm::NextPowerOf2(tagBytes);
    Size tagSize(tagBytes);
    
    llvm::Type *body[] = { tagTy };
    enumTy->setBody(body, /*isPacked*/true);
    
    // Unused tag bits in the physical size can be used as spare bits.
    // TODO: We can use all values greater than the largest discriminator as
    // extra inhabitants, not just those made available by spare bits.
    llvm::BitVector spareBits(tagBits, false);
    spareBits.resize(tagSize.getValueInBits(), true);
    
    return registerEnumTypeInfo(new LoadableEnumTypeInfo(*this,
                                     enumTy, tagSize, std::move(spareBits),
                                     Alignment(tagBytes), IsPOD));
  }
  
  static void setTaggedEnumBody(IRGenModule &IGM,
                                 llvm::StructType *bodyStruct,
                                 unsigned payloadBits, unsigned extraTagBits) {

    auto payloadEnumTy = llvm::IntegerType::get(IGM.getLLVMContext(),
                                                 payloadBits);

    if (extraTagBits > 0) {
      auto extraTagTy = llvm::IntegerType::get(IGM.getLLVMContext(),
                                               extraTagBits);
      llvm::Type *body[] = { payloadEnumTy, extraTagTy };
      bodyStruct->setBody(body, /*isPacked*/true);
    } else {
      llvm::Type *body[] = { payloadEnumTy };
      bodyStruct->setBody(body, /*isPacked*/true);
    }
  }
  
  TypeInfo *SinglePayloadEnumImplStrategy::completeFixedLayout(
                                      TypeConverter &TC,
                                      CanType type,
                                      EnumDecl *theEnum,
                                      llvm::StructType *enumTy) {
    // See whether the payload case's type has extra inhabitants.
    unsigned fixedExtraInhabitants = 0;
    unsigned numTags = ElementsWithNoPayload.size();
    
    auto &payloadTI = getFixedPayloadTypeInfo(); // FIXME non-fixed payload
    fixedExtraInhabitants = payloadTI.getFixedExtraInhabitantCount();
    
    // Determine how many tag bits we need. Given N extra inhabitants, we
    // represent the first N tags using those inhabitants. For additional tags,
    // we use discriminator bit(s) to inhabit the full bit size of the payload.
    unsigned tagsWithoutInhabitants = numTags <= fixedExtraInhabitants
      ? 0 : numTags - fixedExtraInhabitants;
    
    if (tagsWithoutInhabitants == 0) {
      ExtraTagBitCount = 0;
      NumExtraTagValues = 0;
    // If the payload size is greater than 32 bits, the calculation would
    // overflow, but one tag bit should suffice. if you have more than 2^32
    // enum discriminators you have other problems.
    } else if (payloadTI.getFixedSize().getValue() >= 4) {
      ExtraTagBitCount = 1;
      NumExtraTagValues = 2;
    } else {
      unsigned tagsPerTagBitValue =
        1 << payloadTI.getFixedSize().getValueInBits();
      NumExtraTagValues
        = (tagsWithoutInhabitants+(tagsPerTagBitValue-1))/tagsPerTagBitValue+1;
      ExtraTagBitCount = llvm::Log2_32(NumExtraTagValues-1) + 1;
    }

    // Create the body type.
    setTaggedEnumBody(TC.IGM, enumTy,
                       payloadTI.getFixedSize().getValueInBits(),
                       ExtraTagBitCount);
    
    // The enum has the alignment of the payload. The size includes the added
    // tag bits.
    auto sizeWithTag = payloadTI.getFixedSize()
      .roundUpToAlignment(payloadTI.getFixedAlignment())
      .getValue();
    sizeWithTag += (ExtraTagBitCount+7U)/8U;
    
    /// FIXME: Spare bits.
    return getFixedEnumTypeInfo(enumTy, Size(sizeWithTag), {},
                            payloadTI.getFixedAlignment(),
                            payloadTI.isPOD(ResilienceScope::Component));
  }
  
  TypeInfo *SinglePayloadEnumImplStrategy::completeDynamicLayout(
                                                  TypeConverter &TC,
                                                  CanType type,
                                                  EnumDecl *theEnum,
                                                  llvm::StructType *enumTy) {
    // The body is runtime-dependent, so we can't put anything useful here
    // statically.
    enumTy->setBody(ArrayRef<llvm::Type*>{}, /*isPacked*/true);

    // Layout has to be done when the value witness table is instantiated,
    // during initializeValueWitnessTable.
    return registerEnumTypeInfo(new NonFixedEnumTypeInfo(*this, enumTy, type,
                       getPayloadTypeInfo().getBestKnownAlignment(),
                       getPayloadTypeInfo().isPOD(ResilienceScope::Component)));
  }
  
  TypeInfo *
  SinglePayloadEnumImplStrategy::completeEnumTypeLayout(TypeConverter &TC,
                                                  CanType type,
                                                  EnumDecl *theEnum,
                                                  llvm::StructType *enumTy) {
    PayloadTy = type->getTypeOfMember(theEnum->getModuleContext(),
                                      getPayloadElement(),
                                      nullptr,
                                      getPayloadElement()->getArgumentType())
      ->getCanonicalType();
    
    if (TIK >= Fixed)
      return completeFixedLayout(TC, type, theEnum, enumTy);
    return completeDynamicLayout(TC, type, theEnum, enumTy);
  }
  
  TypeInfo *
  MultiPayloadEnumImplStrategy::completeEnumTypeLayout(TypeConverter &TC,
                                                  CanType type,
                                                  EnumDecl *theEnum,
                                                  llvm::StructType *enumTy) {
    // TODO Dynamic layout for multi-payload enums.
    if (!TC.IGM.Opts.EnableDynamicValueTypeLayout && TIK < Fixed) {
      TC.IGM.unimplemented(theEnum->getLoc(),
                           "non-fixed multi-payload enum layout");
      exit(1);
    }

    // We need tags for each of the payload types, which we may be able to form
    // using spare bits, plus a minimal number of tags with which we can
    // represent the empty cases.
    unsigned numPayloadTags = ElementsWithPayload.size();
    unsigned numEmptyElements = ElementsWithNoPayload.size();
    
    // See if the payload types have any spare bits in common.
    // At the end of the loop CommonSpareBits.size() will be the size (in bits)
    // of the largest payload.
    CommonSpareBits = {};
    Alignment worstAlignment(1);
    IsPOD_t isPOD = IsPOD;
    for (auto &elt : ElementsWithPayload) {
      auto &fixedPayloadTI = cast<FixedTypeInfo>(*elt.ti); // FIXME
      fixedPayloadTI.applyFixedSpareBitsMask(CommonSpareBits);
      if (fixedPayloadTI.getFixedAlignment() > worstAlignment)
        worstAlignment = fixedPayloadTI.getFixedAlignment();
      if (!fixedPayloadTI.isPOD(ResilienceScope::Component))
        isPOD = IsNotPOD;
    }
    
    unsigned commonSpareBitCount = CommonSpareBits.count();
    unsigned usedBitCount = CommonSpareBits.size() - commonSpareBitCount;
    
    // Determine how many tags we need to accommodate the empty cases, if any.
    if (ElementsWithNoPayload.empty()) {
      NumEmptyElementTags = 0;
    } else {
      // We can store tags for the empty elements using the inhabited bits with
      // their own tag(s).
      if (usedBitCount >= 32) {
        NumEmptyElementTags = 1;
      } else {
        unsigned emptyElementsPerTag = 1 << usedBitCount;
        NumEmptyElementTags
          = (numEmptyElements + (emptyElementsPerTag-1))/emptyElementsPerTag;
      }
    }
    
    unsigned numTags = numPayloadTags + NumEmptyElementTags;
    unsigned numTagBits = llvm::Log2_32(numTags-1) + 1;
    ExtraTagBitCount = numTagBits <= commonSpareBitCount
      ? 0 : numTagBits - commonSpareBitCount;
    NumExtraTagValues = numTags >> commonSpareBitCount;
    
    // Create the type. We need enough bits to store the largest payload plus
    // extra tag bits we need.
    setTaggedEnumBody(TC.IGM, enumTy,
                       CommonSpareBits.size(),
                       ExtraTagBitCount);
    
    // The enum has the worst alignment of its payloads. The size includes the
    // added tag bits.
    auto sizeWithTag = Size((CommonSpareBits.size() + 7U)/8U)
      .roundUpToAlignment(worstAlignment)
      .getValue();
    sizeWithTag += (ExtraTagBitCount+7U)/8U;
    
    return getFixedEnumTypeInfo(enumTy, Size(sizeWithTag), {},
                            worstAlignment, isPOD);
  }
}

const TypeInfo *TypeConverter::convertEnumType(CanType type,
                                                EnumDecl *theEnum) {
  llvm::StructType *convertedStruct = IGM.createNominalType(theEnum);

  // Create a forward declaration for that type.
  addForwardDecl(type.getPointer(), convertedStruct);

  // Determine the implementation strategy.
  EnumImplStrategy *strategy = EnumImplStrategy::get(*this, type, theEnum);

  // Create the TI.
  return strategy->completeEnumTypeLayout(*this, type,
                                           theEnum, convertedStruct);
}

/// emitEnumDecl - Emit all the declarations associated with this enum type.
void IRGenModule::emitEnumDecl(EnumDecl *theEnum) {
  emitEnumMetadata(*this, theEnum);
  
  // FIXME: This is mostly copy-paste from emitExtension;
  // figure out how to refactor! 
  for (Decl *member : theEnum->getMembers()) {
    switch (member->getKind()) {
    case DeclKind::Import:
    case DeclKind::TopLevelCode:
    case DeclKind::Protocol:
    case DeclKind::Extension:
    case DeclKind::Destructor:
    case DeclKind::InfixOperator:
    case DeclKind::PrefixOperator:
    case DeclKind::PostfixOperator:
      llvm_unreachable("decl not allowed in enum!");

    // We can't have meaningful initializers for variables; these just show
    // up as part of parsing properties.
    case DeclKind::PatternBinding:
      continue;

    case DeclKind::Subscript:
      // Getter/setter will be handled separately.
      continue;
    case DeclKind::TypeAlias:
    case DeclKind::AssociatedType:
    case DeclKind::GenericTypeParam:
      continue;
    case DeclKind::Enum:
      emitEnumDecl(cast<EnumDecl>(member));
      continue;
    case DeclKind::Struct:
      emitStructDecl(cast<StructDecl>(member));
      continue;
    case DeclKind::Class:
      emitClassDecl(cast<ClassDecl>(member));
      continue;
    case DeclKind::Var:
      if (cast<VarDecl>(member)->isComputed())
        // Getter/setter will be handled separately.
        continue;
      continue;
    case DeclKind::Func:
      emitLocalDecls(cast<FuncDecl>(member));
      continue;
    case DeclKind::Constructor:
      emitLocalDecls(cast<ConstructorDecl>(member));
      continue;
        
    case DeclKind::EnumCase:
    case DeclKind::EnumElement:
      // Lowered in SIL.
      continue;
    }
    llvm_unreachable("bad extension member kind");
  }
}

// FIXME: PackEnumPayload and UnpackEnumPayload need to be endian-aware.

PackEnumPayload::PackEnumPayload(IRGenFunction &IGF, unsigned bitSize)
  : IGF(IGF), bitSize(bitSize)
{}

void PackEnumPayload::add(llvm::Value *v) {
  // First, bitcast to an integer type.
  if (isa<llvm::PointerType>(v->getType())) {
    v = IGF.Builder.CreatePtrToInt(v, IGF.IGM.SizeTy);
  } else if (!isa<llvm::IntegerType>(v->getType())) {
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

void PackEnumPayload::addAtOffset(llvm::Value *v, unsigned bitOffset) {
  packedBits = bitOffset;
  add(v);
}

void PackEnumPayload::combine(llvm::Value *v) {
  if (!packedValue)
    packedValue = v;
  else
    packedValue = IGF.Builder.CreateOr(packedValue, v);
}

llvm::Value *PackEnumPayload::get() {
  if (!packedValue)
    packedValue = getEmpty(IGF.IGM, bitSize);
  return packedValue;
}

llvm::Value *PackEnumPayload::getEmpty(IRGenModule &IGM, unsigned bitSize) {
  return llvm::ConstantInt::get(IGM.getLLVMContext(), APInt(bitSize, 0));
}

UnpackEnumPayload::UnpackEnumPayload(IRGenFunction &IGF,
                                       llvm::Value *packedValue)
  : IGF(IGF), packedValue(packedValue)
{}

llvm::Value *UnpackEnumPayload::claim(llvm::Type *ty) {
  // Mask out the bits for the value.
  unsigned bitSize = IGF.IGM.DataLayout.getTypeSizeInBits(ty);
  auto bitTy = llvm::IntegerType::get(IGF.IGM.getLLVMContext(), bitSize);
  llvm::Value *unpacked = unpackedBits == 0
    ? packedValue
    : IGF.Builder.CreateLShr(packedValue, unpackedBits);
  if (bitSize < cast<llvm::IntegerType>(packedValue->getType())->getBitWidth())
    unpacked = IGF.Builder.CreateTrunc(unpacked, bitTy);

  unpackedBits += bitSize;

  // Bitcast to the destination type.
  if (isa<llvm::PointerType>(ty))
    return IGF.Builder.CreateIntToPtr(unpacked, ty);
  return IGF.Builder.CreateBitCast(unpacked, ty);
}

llvm::Value *UnpackEnumPayload::claimAtOffset(llvm::Type *ty,
                                               unsigned bitOffset) {
  unpackedBits = bitOffset;
  return claim(ty);
}

void irgen::emitSwitchLoadableEnumDispatch(IRGenFunction &IGF,
                                  SILType enumTy,
                                  Explosion &enumValue,
                                  ArrayRef<std::pair<EnumElementDecl *,
                                                     llvm::BasicBlock *>> dests,
                                  llvm::BasicBlock *defaultDest) {
  getEnumImplStrategy(IGF.IGM, enumTy)
    .emitValueSwitch(IGF, enumValue, dests, defaultDest);
}

void irgen::emitSwitchAddressOnlyEnumDispatch(IRGenFunction &IGF,
                                  SILType enumTy,
                                  Address enumAddr,
                                  ArrayRef<std::pair<EnumElementDecl *,
                                                     llvm::BasicBlock *>> dests,
                                  llvm::BasicBlock *defaultDest) {
  auto &strategy = getEnumImplStrategy(IGF.IGM, enumTy);
  strategy.emitIndirectSwitch(IGF, enumAddr, dests, defaultDest);
}

void irgen::emitInjectLoadableEnum(IRGenFunction &IGF, SILType enumTy,
                                    EnumElementDecl *theCase,
                                    Explosion &data,
                                    Explosion &out) {
  getEnumImplStrategy(IGF.IGM, enumTy)
    .emitValueInjection(IGF, theCase, data, out);
}

void irgen::emitProjectLoadableEnum(IRGenFunction &IGF, SILType enumTy,
                                     Explosion &inEnumValue,
                                     EnumElementDecl *theCase,
                                     Explosion &out) {
  getEnumImplStrategy(IGF.IGM, enumTy)
    .emitValueProject(IGF, inEnumValue, theCase, out);
}

Address irgen::emitProjectEnumAddressForStore(IRGenFunction &IGF,
                                               SILType enumTy,
                                               Address enumAddr,
                                               EnumElementDecl *theCase) {
  return getEnumImplStrategy(IGF.IGM, enumTy)
    .projectDataForStore(IGF, theCase, enumAddr);
}

Address irgen::emitDestructiveProjectEnumAddressForLoad(IRGenFunction &IGF,
                                                   SILType enumTy,
                                                   Address enumAddr,
                                                   EnumElementDecl *theCase) {
  return getEnumImplStrategy(IGF.IGM, enumTy)
    .destructiveProjectDataForLoad(IGF, theCase, enumAddr);
}

void irgen::emitStoreEnumTagToAddress(IRGenFunction &IGF,
                                       SILType enumTy,
                                       Address enumAddr,
                                       EnumElementDecl *theCase) {
  getEnumImplStrategy(IGF.IGM, enumTy)
    .storeTag(IGF, theCase, enumAddr);
}

APInt irgen::getAPIntFromBitVector(const llvm::BitVector &bits) {
  SmallVector<llvm::integerPart, 2> parts;
  
  for (unsigned i = 0; i < bits.size();) {
    llvm::integerPart part = 0UL;
    for (llvm::integerPart bit = 1; bit != 0 && i < bits.size();
         ++i, bit <<= 1) {
      if (bits[i])
        part |= bit;
    }
    parts.push_back(part);
  }
  
  return APInt(bits.size(), parts);
}

/// Gather spare bits into the low bits of a smaller integer value.
llvm::Value *irgen::emitGatherSpareBits(IRGenFunction &IGF,
                                        const llvm::BitVector &spareBitMask,
                                        llvm::Value *spareBits,
                                        unsigned resultLowBit,
                                        unsigned resultBitWidth) {
  auto destTy
    = llvm::IntegerType::get(IGF.IGM.getLLVMContext(), resultBitWidth);
  unsigned usedBits = resultLowBit;
  llvm::Value *result = nullptr;
  
  for (int i = spareBitMask.find_first(); i != -1;
       i = spareBitMask.find_next(i)) {
    assert(i >= 0);
    unsigned u = i;
    assert(u >= (usedBits - resultLowBit) &&
           "used more bits than we've processed?!");
    
    // Shift the bits into place.
    llvm::Value *newBits;
    if (u > usedBits)
      newBits = IGF.Builder.CreateLShr(spareBits, u - usedBits);
    else if (u < usedBits)
      newBits = IGF.Builder.CreateShl(spareBits, usedBits - u);
    else
      newBits = spareBits;
    newBits = IGF.Builder.CreateZExtOrTrunc(newBits, destTy);
    
    // See how many consecutive bits we have.
    unsigned numBits = 1;
    ++u;
    for (unsigned e = spareBitMask.size(); u < e && spareBitMask[u]; ++u)
      ++numBits;
    
    // Mask out the selected bits.
    auto val = APInt::getAllOnesValue(numBits);
    if (numBits < resultBitWidth)
      val = val.zext(resultBitWidth);
    val = val.shl(usedBits);
    auto *mask = llvm::ConstantInt::get(IGF.IGM.getLLVMContext(), val);
    newBits = IGF.Builder.CreateAnd(newBits, mask);
    
    // Accumulate the result.
    if (result)
      result = IGF.Builder.CreateOr(result, newBits);
    else
      result = newBits;
    
    usedBits += numBits;
    i = u;
  }
  
  return result;
}

/// Scatter spare bits from the low bits of an integer value.
llvm::Value *irgen::emitScatterSpareBits(IRGenFunction &IGF,
                                         const llvm::BitVector &spareBitMask,
                                         llvm::Value *packedBits,
                                         unsigned packedLowBit) {
  auto destTy
    = llvm::IntegerType::get(IGF.IGM.getLLVMContext(), spareBitMask.size());
  llvm::Value *result = nullptr;
  unsigned usedBits = packedLowBit;
  
  // Expand the packed bits to the destination type.
  packedBits = IGF.Builder.CreateZExtOrTrunc(packedBits, destTy);
  
  for (int i = spareBitMask.find_first(); i != -1;
       i = spareBitMask.find_next(i)) {
    assert(i >= 0);
    unsigned u = i, startBit = u;
    assert(u >= usedBits - packedLowBit
           && "used more bits than we've processed?!");
    
    // Shift the selected bits into place.
    llvm::Value *newBits;
    if (u > usedBits)
      newBits = IGF.Builder.CreateShl(packedBits, u - usedBits);
    else if (u < usedBits)
      newBits = IGF.Builder.CreateLShr(packedBits, usedBits - u);
    else
      newBits = packedBits;
    
    // See how many consecutive bits we have.
    unsigned numBits = 1;
    ++u;
    for (unsigned e = spareBitMask.size(); u < e && spareBitMask[u]; ++u)
      ++numBits;
    
    // Mask out the selected bits.
    auto val = APInt::getAllOnesValue(numBits);
    if (numBits < spareBitMask.size())
      val = val.zext(spareBitMask.size());
    val = val.shl(startBit);
    auto mask = llvm::ConstantInt::get(IGF.IGM.getLLVMContext(), val);
    newBits = IGF.Builder.CreateAnd(newBits, mask);
    
    // Accumulate the result.
    if (result)
      result = IGF.Builder.CreateOr(result, newBits);
    else
      result = newBits;
    
    usedBits += numBits;
    i = u;
  }
  
  return result;
}

/// Interleave the occupiedValue and spareValue bits, taking a bit from one
/// or the other at each position based on the spareBits mask.
llvm::ConstantInt *
irgen::interleaveSpareBits(IRGenModule &IGM, const llvm::BitVector &spareBits,
                           unsigned bits,
                           unsigned spareValue, unsigned occupiedValue) {
  // FIXME: endianness.
  SmallVector<llvm::integerPart, 2> valueParts;
  valueParts.push_back(0);
  
  llvm::integerPart valueBit = 1;
  auto advanceValueBit = [&]{
    valueBit <<= 1;
    if (valueBit == 0) {
      valueParts.push_back(0);
      valueBit = 1;
    }
  };
  
  for (unsigned i = 0, e = spareBits.size();
       (occupiedValue || spareValue) && i < e;
       ++i, advanceValueBit()) {
    if (spareBits[i]) {
      if (spareValue & 1)
        valueParts.back() |= valueBit;
      spareValue >>= 1;
    } else {
      if (occupiedValue & 1)
        valueParts.back() |= valueBit;
      occupiedValue >>= 1;
    }
  }
  
  // Create the value.
  llvm::APInt value(bits, valueParts);
  return llvm::ConstantInt::get(IGM.getLLVMContext(), value);
}
