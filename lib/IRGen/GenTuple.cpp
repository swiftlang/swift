//===--- GenTuple.cpp - Swift IR Generation For Tuple Types ---------------===//
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
//  This file implements IR generation for tuple types in Swift.  This
//  includes creating the IR type as  well as emitting the primitive access
//  operations.
//
//  It is assumed in several places in IR-generation that the
//  explosion schema of a tuple type is always equal to the appended
//  explosion schemas of the component types.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Pattern.h"
#include "swift/SIL/SILType.h"
#include "llvm/IR/DerivedTypes.h"

#include "GenHeap.h"
#include "GenSequential.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"
#include "IndirectTypeInfo.h"
#include "NonFixedTypeInfo.h"

#include "GenTuple.h"

using namespace swift;
using namespace irgen;

namespace {
  class TupleFieldInfo : public SequentialField<TupleFieldInfo> {
  public:
    TupleFieldInfo(unsigned index, StringRef name, const TypeInfo &type)
      : SequentialField(type), Index(index), Name(name)
    {}

    /// The field index.
    const unsigned Index;
    const StringRef Name;

    StringRef getFieldName() const {
      return Name;
    }
    
    const TupleTypeElt &getField(SILType T) const {
      auto tup = T.castTo<TupleType>();
      
      return tup->getFields()[Index];
    }
    
    SILType getType(IRGenModule&, SILType t) const {
      return t.getTupleElementType(Index);
    }
  };

  /// Adapter for tuple types.
  template <class Impl, class Base>
  class TupleTypeInfoBase
      : public SequentialTypeInfo<Impl, Base, TupleFieldInfo> {
    typedef SequentialTypeInfo<Impl, Base, TupleFieldInfo> super;

  protected:
    template <class... As>
    TupleTypeInfoBase(As &&...args) : super(std::forward<As>(args)...) {}

    using super::asImpl;

  public:
    /// Given a full tuple explosion, project out a single element.
    void projectElementFromExplosion(IRGenFunction &IGF,
                                     Explosion &tuple,
                                     unsigned fieldNo,
                                     Explosion &out) const {
      const TupleFieldInfo &field = asImpl().getFields()[fieldNo];

      // If the field requires no storage, there's nothing to do.
      if (field.isEmpty())
        return IGF.emitFakeExplosion(field.getTypeInfo(), out);
  
      // Otherwise, project from the base.
      auto fieldRange = field.getProjectionRange();
      ArrayRef<llvm::Value *> element = tuple.getRange(fieldRange.first,
                                                       fieldRange.second);
      out.add(element);
    }

    /// Given the address of a tuple, project out the address of a
    /// single element.
    Address projectElementAddress(IRGenFunction &IGF,
                                  Address tuple,
                                  SILType T,
                                  unsigned fieldNo) const {
      const TupleFieldInfo &field = asImpl().getFields()[fieldNo];
      if (field.isEmpty())
        return field.getTypeInfo().getUndefAddress();

      auto offsets = asImpl().getNonFixedOffsets(IGF, T);
      return field.projectAddress(IGF, tuple, offsets);
    }

    bool isIndirectArgument() const override {
      llvm_unreachable("unexploded tuple as argument?");
    }
    void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                              Address src, SILType T) const override {
      llvm_unreachable("unexploded tuple as argument?");
    }

    // For now, just use extra inhabitants from the first element.
    // FIXME: generalize
    bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
      if (asImpl().getFields().empty()) return false;
      return asImpl().getFields()[0].getTypeInfo().mayHaveExtraInhabitants(IGM);
    }

    // This is dead code in NonFixedTupleTypeInfo.
    unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const {
      if (asImpl().getFields().empty()) return 0;
      auto &eltTI = cast<FixedTypeInfo>(asImpl().getFields()[0].getTypeInfo());
      return eltTI.getFixedExtraInhabitantCount(IGM);
    }

    // This is dead code in NonFixedTupleTypeInfo.
    llvm::ConstantInt *getFixedExtraInhabitantValue(IRGenModule &IGM,
                                                    unsigned bits,
                                                    unsigned index) const {
      auto &eltTI = cast<FixedTypeInfo>(asImpl().getFields()[0].getTypeInfo());
      return eltTI.getFixedExtraInhabitantValue(IGM, bits, index);
    }

    // This is dead code in NonFixedTupleTypeInfo.
    llvm::Value *maskFixedExtraInhabitant(IRGenFunction &IGF,
                                          llvm::Value *tupleValue) const {
      // Truncate down to the width of the element, mask it recursively,
      // and then zext back out to the payload size.
      auto &eltTI = cast<FixedTypeInfo>(asImpl().getFields()[0].getTypeInfo());
      unsigned eltWidth = eltTI.getFixedSize().getValueInBits();
      auto eltTy = llvm::IntegerType::get(IGF.IGM.getLLVMContext(), eltWidth);
      auto eltValue = IGF.Builder.CreateTrunc(tupleValue, eltTy);      
      eltValue = eltTI.maskFixedExtraInhabitant(IGF, eltValue);
      return IGF.Builder.CreateZExt(eltValue, tupleValue->getType());
    }
        
    // This is dead code in NonFixedTupleTypeInfo.
    SpareBitVector getFixedExtraInhabitantMask(IRGenModule &IGM) const {
      if (asImpl().getFields().empty())
        return {};
      
      const FixedTypeInfo &fieldTI
        = cast<FixedTypeInfo>(asImpl().getFields()[0].getTypeInfo());
      
      SpareBitVector mask;
      auto firstFieldSize = fieldTI.getFixedSize().getValueInBits();
      mask.appendSetBits(firstFieldSize);
      mask.appendClearBits(asImpl().getFixedSize().getValueInBits()
                             - firstFieldSize);
      return mask;
    }

    llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                         Address tupleAddr,
                                         SILType tupleType) const override {
      Address eltAddr =
        asImpl().projectElementAddress(IGF, tupleAddr, tupleType, 0);
      auto &elt = asImpl().getFields()[0];
      return elt.getTypeInfo().getExtraInhabitantIndex(IGF, eltAddr,
                                               elt.getType(IGF.IGM, tupleType));
    }
  
    void storeExtraInhabitant(IRGenFunction &IGF,
                              llvm::Value *index,
                              Address tupleAddr,
                              SILType tupleType) const override {
      Address eltAddr =
        asImpl().projectElementAddress(IGF, tupleAddr, tupleType, 0);
      auto &elt = asImpl().getFields()[0];
      elt.getTypeInfo().storeExtraInhabitant(IGF, index, eltAddr,
                                             elt.getType(IGF.IGM, tupleType));
    }
  };

  /// Type implementation for loadable tuples.
  class LoadableTupleTypeInfo :
      public TupleTypeInfoBase<LoadableTupleTypeInfo, LoadableTypeInfo> {
  public:
    // FIXME: Spare bits between tuple elements.
    LoadableTupleTypeInfo(ArrayRef<TupleFieldInfo> fields,
                          unsigned explosionSize,
                          llvm::Type *ty,
                          Size size, SpareBitVector &&spareBits,
                          Alignment align, IsPOD_t isPOD)
      : TupleTypeInfoBase(fields, explosionSize,
                          ty, size, std::move(spareBits), align, isPOD)
      {}

    llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF) const {
      return None;
    }
    llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF, SILType T) const {
      return None;
    }
  };

  /// Type implementation for fixed-size but non-loadable tuples.
  class FixedTupleTypeInfo :
      public TupleTypeInfoBase<FixedTupleTypeInfo,
                               IndirectTypeInfo<FixedTupleTypeInfo,
                                                FixedTypeInfo>>
  {
  public:
    // FIXME: Spare bits between tuple elements.
    FixedTupleTypeInfo(ArrayRef<TupleFieldInfo> fields, llvm::Type *ty,
                       Size size, SpareBitVector &&spareBits, Alignment align,
                       IsPOD_t isPOD, IsBitwiseTakable_t isBT)
      : TupleTypeInfoBase(fields, ty, size, std::move(spareBits), align,
                          isPOD, isBT)
    {}

    llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF) const {
      return None;
    }
    llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF, SILType T) const {
      return None;
    }
  };

  /// An accessor for the non-fixed offsets for a tuple type.
  class TupleNonFixedOffsets : public NonFixedOffsetsImpl {
    // TODO: Should be a SILType.
    SILType TheType;
  public:
    TupleNonFixedOffsets(SILType type) : TheType(type) {
      assert(TheType.is<TupleType>());
    }

    llvm::Value *getOffsetForIndex(IRGenFunction &IGF, unsigned index) {
      // Fetch the metadata as a tuple type.  We cache this because
      // we might repeatedly need the bitcast.
      auto metadata = IGF.emitTypeMetadataRefForLayout(TheType);
      auto asTuple = IGF.Builder.CreateBitCast(metadata,
                                               IGF.IGM.TupleTypeMetadataPtrTy);

      llvm::Value *indices[] = {
        IGF.IGM.getSize(Size(0)),                   // (*tupleType)
        llvm::ConstantInt::get(IGF.IGM.Int32Ty, 3), //   .Elements
        IGF.IGM.getSize(Size(index)),               //     [index]
        llvm::ConstantInt::get(IGF.IGM.Int32Ty, 1)  //       .Offset
      };
      auto slot = IGF.Builder.CreateInBoundsGEP(asTuple, indices);

      return IGF.Builder.CreateLoad(slot, IGF.IGM.getPointerAlignment(),
                                    metadata->getName()
                                      + "." + Twine(index) + ".offset");
    }
  };

  /// Type implementation for non-fixed-size tuples.
  class NonFixedTupleTypeInfo :
      public TupleTypeInfoBase<NonFixedTupleTypeInfo,
                               WitnessSizedTypeInfo<NonFixedTupleTypeInfo>>
  {
  public:
    NonFixedTupleTypeInfo(ArrayRef<TupleFieldInfo> fields, llvm::Type *T,
                          Alignment minAlign, IsPOD_t isPOD,
                          IsBitwiseTakable_t isBT)
      : TupleTypeInfoBase(fields, T, minAlign, isPOD, isBT) {}

    TupleNonFixedOffsets getNonFixedOffsets(IRGenFunction &IGF,
                                            SILType T) const {
      return TupleNonFixedOffsets(T);
    }

    void initializeMetadata(IRGenFunction &IGF,
                            llvm::Value *metadata,
                            llvm::Value *vwtable,
                            SILType T) const override {
      // Tuple value witness tables are instantiated by the runtime along with
      // their metadata. We should never try to initialize one in the compiler.
      llvm_unreachable("initializing value witness table for tuple?!");
    }
  };

  class TupleTypeBuilder :
      public SequentialTypeBuilder<TupleTypeBuilder, TupleFieldInfo,
                                   TupleTypeElt> {
    SILType TheTuple;

  public:
    TupleTypeBuilder(IRGenModule &IGM, SILType theTuple)
      : SequentialTypeBuilder(IGM), TheTuple(theTuple) {}

    FixedTupleTypeInfo *createFixed(ArrayRef<TupleFieldInfo> fields,
                                    StructLayout &&layout) {
      return FixedTupleTypeInfo::create(fields, layout.getType(),
                                        layout.getSize(),
                                        std::move(layout.getSpareBits()),
                                        layout.getAlignment(),
                                        layout.isKnownPOD(),
                                        layout.isKnownBitwiseTakable());
    }

    LoadableTupleTypeInfo *createLoadable(ArrayRef<TupleFieldInfo> fields,
                                          StructLayout &&layout,
                                          unsigned explosionSize) {
      return LoadableTupleTypeInfo::create(fields, explosionSize,
                                           layout.getType(), layout.getSize(),
                                           std::move(layout.getSpareBits()),
                                           layout.getAlignment(),
                                           layout.isKnownPOD());
    }

    NonFixedTupleTypeInfo *createNonFixed(ArrayRef<TupleFieldInfo> fields,
                                          StructLayout &&layout) {
      return NonFixedTupleTypeInfo::create(fields, layout.getType(),
                                           layout.getAlignment(),
                                           layout.isKnownPOD(),
                                           layout.isKnownBitwiseTakable());
    }

    TupleFieldInfo getFieldInfo(unsigned index,
                                const TupleTypeElt &field,
                                const TypeInfo &fieldTI) {
      StringRef name = field.hasName() ? field.getName().str() : "elt";
      return TupleFieldInfo(index, name, fieldTI);
    }

    SILType getType(const TupleTypeElt &field) {
      // We know we're working with a lowered type here.
      return SILType::getPrimitiveObjectType(CanType(field.getType()));
    }

    StructLayout performLayout(ArrayRef<const TypeInfo *> fieldTypes) {
      return StructLayout(IGM, LayoutKind::NonHeapObject,
                          LayoutStrategy::Universal, fieldTypes);
    }
  };
}

const TypeInfo *TypeConverter::convertTupleType(TupleType *tuple) {
  TupleTypeBuilder builder(IGM, SILType::getPrimitiveAddressType(CanType(tuple)));
  return builder.layout(tuple->getFields());
}

/// A convenient macro for delegating an operation to all of the
/// various tuple implementations.
#define FOR_TUPLE_IMPL(IGF, type, op, ...) do {                      \
  auto &tupleTI = IGF.getTypeInfo(type);                             \
  if (isa<LoadableTypeInfo>(tupleTI)) {                              \
    return tupleTI.as<LoadableTupleTypeInfo>().op(IGF, __VA_ARGS__); \
  } else if (isa<FixedTypeInfo>(tupleTI)) {                          \
    return tupleTI.as<FixedTupleTypeInfo>().op(IGF, __VA_ARGS__);    \
  } else {                                                           \
    return tupleTI.as<NonFixedTupleTypeInfo>().op(IGF, __VA_ARGS__); \
  }                                                                  \
} while(0)

void irgen::projectTupleElementFromExplosion(IRGenFunction &IGF,
                                             SILType tupleType,
                                             Explosion &tuple,
                                             unsigned fieldNo,
                                             Explosion &out) {
  FOR_TUPLE_IMPL(IGF, tupleType, projectElementFromExplosion,
                 tuple, fieldNo, out);
}

Address irgen::projectTupleElementAddress(IRGenFunction &IGF,
                                          Address tuple,
                                          SILType tupleType,
                                          unsigned fieldNo) {
  FOR_TUPLE_IMPL(IGF, tupleType, projectElementAddress, tuple,
                 tupleType, fieldNo);
}
