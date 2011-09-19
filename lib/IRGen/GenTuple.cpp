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
//  Currently we do no optimization of tuples.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Target/TargetData.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "RValue.h"

using namespace swift;
using namespace irgen;

namespace {
  /// A class describing the IR layout for a particular field.
  struct TupleFieldInfo {
    TupleFieldInfo(const TupleTypeElt &Field, const TypeInfo &FieldInfo)
      : Field(Field), FieldInfo(FieldInfo) {}

    /// The field.
    const TupleTypeElt &Field;

    /// The TypeInfo for the field.
    const TypeInfo &FieldInfo;

    /// The offset of this field into the storage type.
    Size StorageOffset;

    /// The index of this field into the storage type, or NoStorage
    /// if the field requires no storage.
    unsigned StorageIndex : 24;
    enum : unsigned { NoStorage = 0xFFFFFFU };

    /// The range of this field within the scalars for the tuple.
    unsigned ScalarBegin : 4;
    unsigned ScalarEnd : 4;

    /// Given an l-value for the base address, produce an l-value for
    /// this field.
    /// TODO: this is only actually meaningful for certain possible
    /// representations of tuples;  a bit-packing representation
    /// would not be adequate here.
    LValue getElementPtr(IRGenFunction &IGF, const LValue &LV) const {
      llvm::Value *Addr =
        IGF.Builder.CreateStructGEP(LV.getAddress(), StorageIndex);
      Alignment Alignment = LV.getAlignment().alignmentAtOffset(StorageOffset);
      return LValue::forAddress(Addr, Alignment);
    }
  };

  /// An abstract base class for tuple types.
  class TupleTypeInfo : public TypeInfo {
    /// The number of TupleFieldInfos for this type.  Equal to the
    /// number of fields in the tuple.  The actual data is stored
    /// after the TypeInfo.
    unsigned NumFields : 31;
    unsigned IsAggregate : 1;

    TupleFieldInfo *getFieldInfoStorage();
    const TupleFieldInfo *getFieldInfoStorage() const {
      return const_cast<TupleTypeInfo*>(this)->getFieldInfoStorage();
    }

  public:
    TupleTypeInfo(llvm::Type *T, Size S, Alignment A,
                  bool IsAggregate, ArrayRef<TupleFieldInfo> Fields)
      : TypeInfo(T, S, A), NumFields(Fields.size()), IsAggregate(IsAggregate) {

      TupleFieldInfo *FieldStorage = getFieldInfoStorage();
      for (unsigned I = 0, E = Fields.size(); I != E; ++I)
        new(&FieldStorage[I]) TupleFieldInfo(Fields[I]);
    }

    ArrayRef<TupleFieldInfo> getFieldInfos() const {
      return llvm::makeArrayRef(getFieldInfoStorage(), NumFields);
    }
  };

  /// A TypeInfo implementation for tuples that are broken down into scalars.
  class ScalarTupleTypeInfo : public TupleTypeInfo {
    RValueSchema Schema;
    
    ScalarTupleTypeInfo(llvm::Type *T, Size S, Alignment A,
                        ArrayRef<TupleFieldInfo> Fields,
                        const RValueSchema &Schema)
      : TupleTypeInfo(T, S, A, /*agg*/ false, Fields), Schema(Schema) {}

  public:
    static ScalarTupleTypeInfo *create(llvm::Type *T, Size S, Alignment A,
                                       ArrayRef<llvm::Type*> ScalarTypes,
                                       ArrayRef<TupleFieldInfo> FieldInfos) {
      void *Storage = operator new(sizeof(ScalarTupleTypeInfo) +
                                   sizeof(TupleFieldInfo) * FieldInfos.size());
      return new(Storage) ScalarTupleTypeInfo(T, S, A, FieldInfos,
                               RValueSchema::forScalars(ScalarTypes));
    }

    RValueSchema getSchema() const {
      return Schema;
    }

    RValue load(IRGenFunction &IGF, const LValue &LV) const {
      SmallVector<llvm::Value*, RValue::MaxScalars> Scalars;

      // Load by loading the elements.
      for (const TupleFieldInfo &Field : getFieldInfos()) {
        assert(Scalars.size() == Field.ScalarBegin);

        // Skip fields that contribute no scalars.
        if (Field.ScalarBegin == Field.ScalarEnd) continue;

        // Load the field and extract the scalars.
        LValue FieldLV = Field.getElementPtr(IGF, LV);
        RValue FieldRV = Field.FieldInfo.load(IGF, FieldLV);
        Scalars.append(FieldRV.getScalars().begin(),
                       FieldRV.getScalars().end());

        assert(Scalars.size() == Field.ScalarEnd);
      }

      return RValue::forScalars(Scalars);
    }

    void store(IRGenFunction &IGF, const RValue &RV, const LValue &LV) const {
      assert(RV.isScalar());

      for (const TupleFieldInfo &Field : getFieldInfos()) {
        // Skip fields that contribute no scalars.
        if (Field.ScalarBegin == Field.ScalarEnd) continue;

        // Project out the appropriate r-value.
        ArrayRef<llvm::Value*> Scalars = 
          RV.getScalars().slice(Field.ScalarBegin,
                                Field.ScalarEnd - Field.ScalarBegin);
        RValue FieldRV = RValue::forScalars(Scalars);

        // Write the extracted r-value into a projected l-value.
        LValue FieldLV = Field.getElementPtr(IGF, LV);
        Field.FieldInfo.store(IGF, FieldRV, FieldLV);
      }
    }
  };

  /// A TypeInfo implementation for tuples that are passed around as
  /// aggregates.
  class AggregateTupleTypeInfo : public TupleTypeInfo {
    AggregateTupleTypeInfo(llvm::Type *T, Size S, Alignment A,
                           ArrayRef<TupleFieldInfo> Fields)
      : TupleTypeInfo(T, S, A, /*agg*/ true, Fields) {}

  public:
    static AggregateTupleTypeInfo *create(llvm::StructType *T,
                                          Size S, Alignment A,
                                          ArrayRef<TupleFieldInfo> FieldInfos) {
      void *Storage = operator new(sizeof(AggregateTupleTypeInfo) +
                                   sizeof(TupleFieldInfo) * FieldInfos.size());
      return new(Storage) AggregateTupleTypeInfo(T, S, A, FieldInfos);
    }

    llvm::StructType *getStorageType() const {
      return cast<llvm::StructType>(TypeInfo::getStorageType());
    }

    RValueSchema getSchema() const {
      return RValueSchema::forAggregate(getStorageType(), StorageAlignment);
    }

    RValue load(IRGenFunction &IGF, const LValue &LV) const {
      // FIXME
      return RValue();
    }

    void store(IRGenFunction &CGF, const RValue &RV, const LValue &LV) const {
      // FIXME
    }
  };
}

TupleFieldInfo *TupleTypeInfo::getFieldInfoStorage() {
  void *Ptr;
  if (IsAggregate)
    Ptr = static_cast<AggregateTupleTypeInfo*>(this)+1;
  else
    Ptr = static_cast<ScalarTupleTypeInfo*>(this)+1;
  return reinterpret_cast<TupleFieldInfo*>(Ptr);
}

const TypeInfo *
TypeConverter::convertTupleType(IRGenModule &IGM, TupleType *T) {
  SmallVector<TupleFieldInfo, 8> FieldInfos;
  SmallVector<llvm::Type*, 8> ScalarTypes;
  SmallVector<llvm::Type*, 8> StorageTypes;
  FieldInfos.reserve(T->Fields.size());
  StorageTypes.reserve(T->Fields.size());

  bool HasAggregateField = false;

  Size StorageSize;
  Alignment StorageAlignment;

  // TODO: rearrange the tuple for optimal packing.
  for (const TupleTypeElt &Field : T->Fields) {
    const TypeInfo &TInfo = getFragileTypeInfo(IGM, Field.Ty);
    assert(TInfo.isComplete());

    FieldInfos.push_back(TupleFieldInfo(Field, TInfo));
    TupleFieldInfo &FieldInfo = FieldInfos.back();

    // Ignore zero-sized fields.
    if (TInfo.StorageSize.isZero()) {
      FieldInfo.StorageIndex = TupleFieldInfo::NoStorage;
      FieldInfo.ScalarBegin = FieldInfo.ScalarEnd = ScalarTypes.size();
      continue;
    }

    if (!HasAggregateField) {
      RValueSchema Schema = TInfo.getSchema();
      if (Schema.isAggregate()) {
        HasAggregateField = true;
      } else {
        FieldInfo.ScalarBegin = ScalarTypes.size();
        ScalarTypes.append(Schema.getScalarTypes().begin(),
                           Schema.getScalarTypes().end());
        FieldInfo.ScalarEnd = ScalarTypes.size();
      }
    }

    StorageAlignment = std::max(StorageAlignment, TInfo.StorageAlignment);

    // If the current tuple size isn't a multiple of the tuple
    // alignment, we need padding.
    if (Size OffsetFromAlignment = StorageSize % StorageAlignment) {
      unsigned PaddingRequired
        = StorageAlignment.getValue() - OffsetFromAlignment.getValue();

      // We don't actually need to uglify the IR unless the natural
      // alignment of the IR type for the field isn't good enough.
      Alignment FieldIRAlignment(
          IGM.TargetData.getABITypeAlignment(TInfo.StorageType));
      assert(FieldIRAlignment <= TInfo.StorageAlignment);
      if (FieldIRAlignment != TInfo.StorageAlignment) {
        StorageTypes.push_back(llvm::ArrayType::get(IGM.Int8Ty,
                                                    PaddingRequired));
      }

      // Regardless, the storage size goes up.
      StorageSize += Size(PaddingRequired);
    }

    FieldInfo.StorageIndex = StorageTypes.size();
    FieldInfo.StorageOffset = StorageSize;
    StorageTypes.push_back(TInfo.getStorageType());
    StorageSize += TInfo.StorageSize;
  }

  // If the tuple requires no storage at all, just use i8.  Most
  // clients will just ignore zero-size types, but those that care can
  // have a sensible one-byte type.
  if (StorageSize.isZero()) {
    assert(ScalarTypes.empty());
    assert(StorageTypes.empty());
    return ScalarTupleTypeInfo::create(IGM.Int8Ty, Size(0), Alignment(1),
                                       ScalarTypes, FieldInfos);
  }

  // Otherwise, build a new, structural type.
  llvm::StructType *Converted
    = llvm::StructType::get(IGM.getLLVMContext(), StorageTypes);

  // If the tuple has no aggregate fields, and the number of scalars
  // doesn't exceed the maximum, pass it as scalars.
  if (!HasAggregateField && ScalarTypes.size() <= RValueSchema::MaxScalars) {
    return ScalarTupleTypeInfo::create(Converted, StorageSize, StorageAlignment,
                                       ScalarTypes, FieldInfos);
  }

  // Otherwise, pass the entire thing as an aggregate.  This is a bit
  // wasteful; we could really pass some of the fields as aggregates
  // and some as scalars.
  return AggregateTupleTypeInfo::create(Converted, StorageSize,
                                        StorageAlignment, FieldInfos);
}

/// Emit a tuple literal expression.
RValue IRGenFunction::emitTupleExpr(TupleExpr *Tuple, const TypeInfo &TI) {
  // Extract information about the tuple.  Note that type-checking
  // should ensure that the literal's elements are exactly parallel
  // with the field infos.
  const TupleTypeInfo &TInfo = static_cast<const TupleTypeInfo&>(TI);
  ArrayRef<TupleFieldInfo> FieldInfos = TInfo.getFieldInfos();
  RValueSchema Schema = TInfo.getSchema();

  // Set up for the emission.
  llvm::SmallVector<llvm::Value*, RValue::MaxScalars> Scalars;
  LValue Aggregate;
  if (Schema.isAggregate()) {
    Aggregate = createFullExprAlloca(Schema.getAggregateType(),
                                     Schema.getAggregateAlignment(),
                                     "tuple-literal");
  }

  // Emit all the sub-expressions.
  for (unsigned I = 0, E = Tuple->getNumElements(); I != E; ++I) {
    const TupleFieldInfo &FieldInfo = FieldInfos[I];
    RValue Field = emitRValue(Tuple->getElement(I), FieldInfo.FieldInfo);

    // If the outer schema is scalar, then all the element schemas
    // are, too, and we should just their scalars into field scalars.
    if (Schema.isScalar()) {
      assert(Field.isScalar());
      Scalars.append(Field.getScalars().begin(), Field.getScalars().end());

    // The reverse is not necessarily true, so we need to store an
    // arbitrary r-value into our temporary.
    } else {
      LValue FieldLV = FieldInfo.getElementPtr(*this, Aggregate);
      FieldInfo.FieldInfo.store(*this, Field, FieldLV);
    }
  }

  // Finally, construct the temporary.
  if (Schema.isAggregate()) {
    return RValue::forAggregate(Aggregate.getAddress());
  } else {
    return RValue::forScalars(Scalars);
  }
}

RValue IRGenFunction::emitTupleElementRValue(TupleElementExpr *E,
                                             const TypeInfo &TI) {
  unimplemented(E->getLoc(), "tuple elements are unimplemented");
  return emitFakeRValue(TI);
}

LValue IRGenFunction::emitTupleElementLValue(TupleElementExpr *E,
                                             const TypeInfo &TI) {
  unimplemented(E->getLoc(), "tuple elements are unimplemented");
  return emitFakeLValue(TI);
}

RValue IRGenFunction::emitTupleShuffleExpr(TupleShuffleExpr *E,
                                           const TypeInfo &TI) {
  unimplemented(E->getLoc(), "tuple shuffles are unimplemented");
  return emitFakeRValue(TI);
}
