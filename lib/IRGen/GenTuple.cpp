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
#include "swift/Basic/Optional.h"
#include "llvm/IR/DerivedTypes.h"

#include "ASTVisitor.h"
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
    
    const TupleTypeElt &getField(CanType t) const {
      auto tup = cast<TupleType>(t);
      
      return tup->getFields()[Index];
    }
    
    CanType getType(IRGenModule&, CanType t) const {
      auto tup = cast<TupleType>(t);
      return tup.getElementType(Index);
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
      assert(tuple.getKind() == out.getKind());

      const TupleFieldInfo &field = asImpl().getFields()[fieldNo];

      // If the field requires no storage, there's nothing to do.
      if (field.isEmpty())
        return IGF.emitFakeExplosion(field.getTypeInfo(), out);
  
      // Otherwise, project from the base.
      auto fieldRange = field.getProjectionRange(out.getKind());
      ArrayRef<llvm::Value *> element = tuple.getRange(fieldRange.first,
                                                       fieldRange.second);
      out.add(element);
    }

    /// Given the address of a tuple, project out the address of a
    /// single element.
    Address projectElementAddress(IRGenFunction &IGF,
                                  Address tuple,
                                  CanType T,
                                  unsigned fieldNo) const {
      const TupleFieldInfo &field = asImpl().getFields()[fieldNo];
      if (field.isEmpty())
        return field.getTypeInfo().getUndefAddress();

      auto offsets = asImpl().getNonFixedOffsets(IGF, T);
      return field.projectAddress(IGF, tuple, offsets);
    }

    bool isIndirectArgument(ExplosionKind kind) const override {
      llvm_unreachable("unexploded tuple as argument?");
    }
    void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                              Address src, CanType T) const override {
      llvm_unreachable("unexploded tuple as argument?");
    }
  };

  /// Type implementation for loadable tuples.
  class LoadableTupleTypeInfo :
      public TupleTypeInfoBase<LoadableTupleTypeInfo, LoadableTypeInfo> {
  public:
    // FIXME: Spare bits between tuple elements.
    LoadableTupleTypeInfo(unsigned numFields, llvm::Type *ty,
                          Size size, llvm::BitVector spareBits,
                          Alignment align, IsPOD_t isPOD)
      : TupleTypeInfoBase(numFields, ty, size, std::move(spareBits), align,
                          isPOD)
      {}

    Nothing_t getNonFixedOffsets(IRGenFunction &IGF) const { return Nothing; }
    Nothing_t getNonFixedOffsets(IRGenFunction &IGF,
                                 CanType T) const { return Nothing; }
    
    // FIXME: Suppress use of extra inhabitants for single-payload enum layout
    // until we're ready to handle the runtime logic for exporting extra
    // inhabitants through tuple metadata.
    bool mayHaveExtraInhabitants(IRGenModule&) const override { return false; }
    unsigned getFixedExtraInhabitantCount(IRGenModule&) const override {
      return 0;
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
    FixedTupleTypeInfo(unsigned numFields, llvm::Type *ty,
                       Size size, llvm::BitVector spareBits, Alignment align,
                       IsPOD_t isPOD)
      : TupleTypeInfoBase(numFields, ty, size, std::move(spareBits), align,
                          isPOD)
    {}

    Nothing_t getNonFixedOffsets(IRGenFunction &IGF) const { return Nothing; }
    Nothing_t getNonFixedOffsets(IRGenFunction &IGF,
                                 CanType T) const { return Nothing; }
    
    // FIXME: Suppress use of extra inhabitants for single-payload enum layout
    // until we're ready to handle the runtime logic for exporting extra
    // inhabitants through tuple metadata.
    bool mayHaveExtraInhabitants(IRGenModule&) const override { return false; }
    unsigned getFixedExtraInhabitantCount(IRGenModule&) const override {
      return 0;
    }
  };

  /// An accessor for the non-fixed offsets for a tuple type.
  class TupleNonFixedOffsets : public NonFixedOffsetsImpl {
    CanType TheType;
  public:
    TupleNonFixedOffsets(CanType type) : TheType(type) {
      assert(isa<TupleType>(TheType));
    }

    llvm::Value *getOffsetForIndex(IRGenFunction &IGF, unsigned index) {
      // Fetch the metadata as a tuple type.  We cache this because
      // we might repeatedly need the bitcast.
      auto metadata = IGF.emitTypeMetadataRef(TheType);
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
    NonFixedTupleTypeInfo(unsigned numFields, llvm::Type *T,
                          Alignment minAlign, IsPOD_t isPOD)
      : TupleTypeInfoBase(numFields, T, minAlign, isPOD) {}

    TupleNonFixedOffsets getNonFixedOffsets(IRGenFunction &IGF,
                                            CanType T) const {
      return TupleNonFixedOffsets(T);
    }

    void initializeMetadata(IRGenFunction &IGF,
                            llvm::Value *metadata,
                            llvm::Value *vwtable,
                            CanType T) const override {
      // Tuple value witness tables are instantiated by the runtime along with
      // their metadata. We should never try to initialize one in the compiler.
      llvm_unreachable("initializing value witness table for tuple?!");
    }
  };

  class TupleTypeBuilder :
      public SequentialTypeBuilder<TupleTypeBuilder, TupleFieldInfo,
                                   TupleTypeElt> {
    CanType TheTuple;

  public:
    TupleTypeBuilder(IRGenModule &IGM, CanType theTuple)
      : SequentialTypeBuilder(IGM), TheTuple(theTuple) {}

    FixedTupleTypeInfo *createFixed(ArrayRef<TupleFieldInfo> fields,
                                    const StructLayout &layout) {
      return create<FixedTupleTypeInfo>(fields, layout.getType(),
                                        layout.getSize(),
                                        layout.getSpareBits(),
                                        layout.getAlignment(),
                                        layout.isKnownPOD());
    }

    LoadableTupleTypeInfo *createLoadable(ArrayRef<TupleFieldInfo> fields,
                                          const StructLayout &layout) {
      return create<LoadableTupleTypeInfo>(fields, layout.getType(),
                                           layout.getSize(),
                                           layout.getSpareBits(),
                                           layout.getAlignment(),
                                           layout.isKnownPOD());
    }

    NonFixedTupleTypeInfo *createNonFixed(ArrayRef<TupleFieldInfo> fields,
                                          const StructLayout &layout) {
      return create<NonFixedTupleTypeInfo>(fields, layout.getType(),
                                           layout.getAlignment(),
                                           layout.isKnownPOD());
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
  TupleTypeBuilder builder(IGM, CanType(tuple));
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
                 tupleType.getSwiftRValueType(), fieldNo);
}
