//===--- GenTuple.cpp - Swift IR Generation For Tuple Types ---------------===//
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
//
//  This file implements IR generation for tuple types in Swift.  This
//  includes creating the IR type as well as emitting the primitive access
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
#include "GenRecord.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"
#include "IndirectTypeInfo.h"
#include "NonFixedTypeInfo.h"

#include "GenTuple.h"

#pragma clang diagnostic ignored "-Winconsistent-missing-override"

using namespace swift;
using namespace irgen;

namespace {
  class TupleFieldInfo : public RecordField<TupleFieldInfo> {
  public:
    TupleFieldInfo(unsigned index, StringRef name, const TypeInfo &type)
      : RecordField(type), Index(index), Name(name)
    {}

    /// The field index.
    const unsigned Index;
    const StringRef Name;

    StringRef getFieldName() const {
      return Name;
    }
    
    const TupleTypeElt &getField(SILType T) const {
      auto tup = T.castTo<TupleType>();
      
      return tup->getElement(Index);
    }
    
    SILType getType(IRGenModule&, SILType t) const {
      return t.getTupleElementType(Index);
    }
  };
  
  /// Project a tuple offset from a tuple metadata structure.
  static llvm::Value *loadTupleOffsetFromMetadata(IRGenFunction &IGF,
                                                  llvm::Value *metadata,
                                                  unsigned index) {
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

  /// Adapter for tuple types.
  template <class Impl, class Base>
  class TupleTypeInfoBase
      : public RecordTypeInfo<Impl, Base, TupleFieldInfo> {
    using super = RecordTypeInfo<Impl, Base, TupleFieldInfo>;

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

    /// Return the statically-known offset of the given element.
    Optional<Size> getFixedElementOffset(IRGenModule &IGM,
                                         unsigned fieldNo) const {
      const TupleFieldInfo &field = asImpl().getFields()[fieldNo];
      switch (field.getKind()) {
      case ElementLayout::Kind::Empty:
      case ElementLayout::Kind::Fixed:
        return field.getFixedByteOffset();
      case ElementLayout::Kind::InitialNonFixedSize:
        return Size(0);
      case ElementLayout::Kind::NonFixed:
        return None;
      }
      llvm_unreachable("bad element layout kind");
    }

    Optional<unsigned> getElementStructIndex(IRGenModule &IGM,
                                             unsigned fieldNo) const {
      const TupleFieldInfo &field = asImpl().getFields()[fieldNo];
      if (field.isEmpty())
        return None;
      return field.getStructIndex();
    }

    void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                              Address src, SILType T,
                              bool isOutlined) const override {
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
    APInt getFixedExtraInhabitantValue(IRGenModule &IGM,
                                       unsigned bits,
                                       unsigned index) const {
      auto &eltTI = cast<FixedTypeInfo>(asImpl().getFields()[0].getTypeInfo());
      return eltTI.getFixedExtraInhabitantValue(IGM, bits, index);
    }
        
    // This is dead code in NonFixedTupleTypeInfo.
    APInt getFixedExtraInhabitantMask(IRGenModule &IGM) const {
      if (asImpl().getFields().empty())
        return APInt();
      
      const FixedTypeInfo &fieldTI
        = cast<FixedTypeInfo>(asImpl().getFields()[0].getTypeInfo());
      auto size = asImpl().getFixedSize().getValueInBits();
      
      if (fieldTI.isKnownEmpty(ResilienceExpansion::Maximal))
        return APInt(size, 0);
      
      APInt firstMask = fieldTI.getFixedExtraInhabitantMask(IGM);
      return firstMask.zextOrTrunc(size);
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
    
    void verify(IRGenTypeVerifierFunction &IGF,
                llvm::Value *metadata,
                SILType tupleType) const override {
      auto fields = asImpl().getFields();
      for (unsigned i : indices(fields)) {
        const TupleFieldInfo &field = fields[i];
        switch (field.getKind()) {
        case ElementLayout::Kind::Fixed: {
          // Check that the fixed layout matches the layout in the tuple
          // metadata.
          auto fixedOffset = field.getFixedByteOffset();
          
          auto runtimeOffset = loadTupleOffsetFromMetadata(IGF, metadata, i);

          IGF.verifyValues(metadata, runtimeOffset,
                     IGF.IGM.getSize(fixedOffset),
                     llvm::Twine("offset of tuple element ") + llvm::Twine(i));
          break;
        }
        
        case ElementLayout::Kind::Empty:
        case ElementLayout::Kind::InitialNonFixedSize:
        case ElementLayout::Kind::NonFixed:
          continue;
        }
      }
    }
  };

  /// Type implementation for loadable tuples.
  class LoadableTupleTypeInfo final :
      public TupleTypeInfoBase<LoadableTupleTypeInfo, LoadableTypeInfo> {
  public:
    // FIXME: Spare bits between tuple elements.
    LoadableTupleTypeInfo(ArrayRef<TupleFieldInfo> fields,
                          unsigned explosionSize,
                          llvm::Type *ty,
                          Size size, SpareBitVector &&spareBits,
                          Alignment align, IsPOD_t isPOD,
                          IsFixedSize_t alwaysFixedSize)
      : TupleTypeInfoBase(fields, explosionSize,
                          ty, size, std::move(spareBits), align, isPOD,
                          alwaysFixedSize)
      {}

    void addToAggLowering(IRGenModule &IGM, SwiftAggLowering &lowering,
                          Size offset) const override {
      for (auto &field : getFields()) {
        auto fieldOffset = offset + field.getFixedByteOffset();
        cast<LoadableTypeInfo>(field.getTypeInfo())
          .addToAggLowering(IGM, lowering, fieldOffset);
      }
    }

    llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF) const {
      return None;
    }
    llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF, SILType T) const {
      return None;
    }
  };

  /// Type implementation for fixed-size but non-loadable tuples.
  class FixedTupleTypeInfo final :
      public TupleTypeInfoBase<FixedTupleTypeInfo,
                               IndirectTypeInfo<FixedTupleTypeInfo,
                                                FixedTypeInfo>>
  {
  public:
    // FIXME: Spare bits between tuple elements.
    FixedTupleTypeInfo(ArrayRef<TupleFieldInfo> fields, llvm::Type *ty,
                       Size size, SpareBitVector &&spareBits, Alignment align,
                       IsPOD_t isPOD, IsBitwiseTakable_t isBT,
                       IsFixedSize_t alwaysFixedSize)
      : TupleTypeInfoBase(fields, ty, size, std::move(spareBits), align,
                          isPOD, isBT, alwaysFixedSize)
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

    llvm::Value *getOffsetForIndex(IRGenFunction &IGF, unsigned index) override {
      // Fetch the metadata as a tuple type.  We cache this because
      // we might repeatedly need the bitcast.
      auto metadata = IGF.emitTypeMetadataRefForLayout(TheType);
      return loadTupleOffsetFromMetadata(IGF, metadata, index);
    }
  };

  /// Type implementation for non-fixed-size tuples.
  class NonFixedTupleTypeInfo final :
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
  };

  class TupleTypeBuilder :
      public RecordTypeBuilder<TupleTypeBuilder, TupleFieldInfo,
                               TupleTypeElt> {
    SILType TheTuple;

  public:
    TupleTypeBuilder(IRGenModule &IGM, SILType theTuple)
      : RecordTypeBuilder(IGM), TheTuple(theTuple) {}

    FixedTupleTypeInfo *createFixed(ArrayRef<TupleFieldInfo> fields,
                                    StructLayout &&layout) {
      return FixedTupleTypeInfo::create(fields, layout.getType(),
                                        layout.getSize(),
                                        std::move(layout.getSpareBits()),
                                        layout.getAlignment(),
                                        layout.isPOD(),
                                        layout.isBitwiseTakable(),
                                        layout.isAlwaysFixedSize());
    }

    LoadableTupleTypeInfo *createLoadable(ArrayRef<TupleFieldInfo> fields,
                                          StructLayout &&layout,
                                          unsigned explosionSize) {
      return LoadableTupleTypeInfo::create(fields, explosionSize,
                                           layout.getType(), layout.getSize(),
                                           std::move(layout.getSpareBits()),
                                           layout.getAlignment(),
                                           layout.isPOD(),
                                           layout.isAlwaysFixedSize());
    }

    NonFixedTupleTypeInfo *createNonFixed(ArrayRef<TupleFieldInfo> fields,
                                          StructLayout &&layout) {
      return NonFixedTupleTypeInfo::create(fields, layout.getType(),
                                           layout.getAlignment(),
                                           layout.isPOD(),
                                           layout.isBitwiseTakable());
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
      return StructLayout(IGM, CanType(), LayoutKind::NonHeapObject,
                          LayoutStrategy::Universal, fieldTypes);
    }
  };
} // end anonymous namespace

const TypeInfo *TypeConverter::convertTupleType(TupleType *tuple) {
  TupleTypeBuilder builder(IGM, SILType::getPrimitiveAddressType(CanType(tuple)));
  return builder.layout(tuple->getElements());
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
} while (0)

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

Optional<Size> irgen::getFixedTupleElementOffset(IRGenModule &IGM,
                                                 SILType tupleType,
                                                 unsigned fieldNo) {
  // Macro happens to work with IGM, too.
  FOR_TUPLE_IMPL(IGM, tupleType, getFixedElementOffset, fieldNo);
}

Optional<unsigned> irgen::getPhysicalTupleElementStructIndex(IRGenModule &IGM,
                                                             SILType tupleType,
                                                             unsigned fieldNo) {
  FOR_TUPLE_IMPL(IGM, tupleType, getElementStructIndex, fieldNo);
}
