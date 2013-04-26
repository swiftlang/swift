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
#include "swift/Basic/Optional.h"
#include "llvm/IR/DerivedTypes.h"

#include "ASTVisitor.h"
#include "GenHeap.h"
#include "GenSequential.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"
#include "NonFixedTypeInfo.h"

#include "GenTuple.h"

using namespace swift;
using namespace irgen;

namespace {
  class TupleFieldInfo : public SequentialField<TupleFieldInfo> {
  public:
    TupleFieldInfo(const TupleTypeElt &field, const TypeInfo &type)
      : SequentialField(type), Field(field) {}

    /// The field.
    const TupleTypeElt &Field;

    StringRef getFieldName() const {
      if (Field.hasName())
        return Field.getName().str();
      return "elt";
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
    OwnedAddress projectElementAddress(IRGenFunction &IGF,
                                       OwnedAddress tuple,
                                       unsigned fieldNo) const {
      const TupleFieldInfo &field = asImpl().getFields()[fieldNo];
      if (field.isEmpty())
        return {field.getTypeInfo().getUndefAddress(), nullptr};

      auto offsets = asImpl().getNonFixedOffsets(IGF);
      Address fieldAddr = field.projectAddress(IGF, tuple.getAddress(),
                                               offsets);
      return {fieldAddr, tuple.getOwner()};
    }
  };

  /// Type implementation for fixed-size tuples.
  class FixedTupleTypeInfo :
      public TupleTypeInfoBase<FixedTupleTypeInfo, FixedTypeInfo> {
  public:
    FixedTupleTypeInfo(unsigned numFields, llvm::Type *ty,
                       Size size, Alignment align, IsPOD_t isPOD)
      : TupleTypeInfoBase(numFields, ty, size, align, isPOD) {}

    Nothing_t getNonFixedOffsets(IRGenFunction &IGF) const { return Nothing; }
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
                               WitnessSizedTypeInfo<NonFixedTupleTypeInfo> > {

    CanType TheType;
  public:
    NonFixedTupleTypeInfo(unsigned numFields, llvm::Type *T, CanType type,
                          Alignment minAlign, IsPOD_t isPOD)
      : TupleTypeInfoBase(numFields, T, minAlign, isPOD), TheType(type) {}

    TupleNonFixedOffsets getNonFixedOffsets(IRGenFunction &IGF) const {
      return TupleNonFixedOffsets(TheType);
    }

    llvm::Value *getMetadataRef(IRGenFunction &IGF) const {
      return IGF.emitTypeMetadataRef(TheType);
    }

    llvm::Value *getValueWitnessTable(IRGenFunction &IGF) const {
      auto metadata = getMetadataRef(IGF);
      return IGF.emitValueWitnessTableRefForMetadata(metadata);
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
                                        layout.getSize(), layout.getAlignment(),
                                        layout.isKnownPOD());
    }

    NonFixedTupleTypeInfo *createNonFixed(ArrayRef<TupleFieldInfo> fields,
                                          const StructLayout &layout) {
      return create<NonFixedTupleTypeInfo>(fields, layout.getType(), TheTuple,
                                           layout.getAlignment(),
                                           layout.isKnownPOD());
    }

    TupleFieldInfo getFieldInfo(const TupleTypeElt &field,
                                const TypeInfo &fieldTI) {
      return TupleFieldInfo(field, fieldTI);
    }

    Type getType(const TupleTypeElt &field) { return field.getType(); }

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
  auto &tupleTI = IGF.getFragileTypeInfo(type);                      \
  if (isa<FixedTypeInfo>(tupleTI)) {                                 \
    return tupleTI.as<FixedTupleTypeInfo>().op(IGF, __VA_ARGS__);    \
  } else {                                                           \
    return tupleTI.as<NonFixedTupleTypeInfo>().op(IGF, __VA_ARGS__); \
  }                                                                  \
} while(0)

void irgen::projectTupleElementFromExplosion(IRGenFunction &IGF,
                                             CanType tupleType,
                                             Explosion &tuple,
                                             unsigned fieldNo,
                                             Explosion &out) {
  FOR_TUPLE_IMPL(IGF, tupleType, projectElementFromExplosion,
                 tuple, fieldNo, out);
}

OwnedAddress irgen::projectTupleElementAddress(IRGenFunction &IGF,
                                               OwnedAddress tuple,
                                               CanType tupleType,
                                               unsigned fieldNo) {
  FOR_TUPLE_IMPL(IGF, tupleType, projectElementAddress,
                 tuple, fieldNo);
}

/// Emit a string literal, either as a C string pointer or as a (pointer, size)
/// tuple.
// FIXME: Why is this here?
void swift::irgen::emitStringLiteral(IRGenFunction &IGF,
                                     StringRef string,
                                     bool includeSize,
                                     Explosion &out) {
  auto ptr = IGF.IGM.getAddrOfGlobalString(string);
  out.add(ptr);
  if (includeSize)
    out.add(IGF.Builder.getInt64(string.size()));
}
