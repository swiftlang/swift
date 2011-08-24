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
#include "llvm/DerivedTypes.h"
#include "llvm/Target/TargetData.h"

#include "GenType.h"
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

    /// The index of this field into the storage type, or NoStorage
    /// if the field requires no storage.
    unsigned StorageIndex : 24;
    enum : unsigned { NoStorage = 0xFFFFFFU };

    /// The range of this field within the scalars for the tuple.
    unsigned ScalarBegin : 4;
    unsigned ScalarEnd : 4;
  };

  /// An abstract base class for tuple types.
  class TupleTypeInfo : public TypeInfo {
    /// The number of TupleFieldInfos for this type.  Equal to the
    /// number of fields in the tuple.  The actual data is stored
    /// after the TypeInfo.
    unsigned NumFields : 31;
    unsigned IsAggregate : 1;

    TupleFieldInfo *getFieldInfos();
    const TupleFieldInfo *getFieldInfos() const {
      return const_cast<TupleTypeInfo*>(this)->getFieldInfos();
    }

  public:
    TupleTypeInfo(llvm::Type *T, Size S, Alignment A,
                  bool IsAggregate, ArrayRef<TupleFieldInfo> Fields)
      : TypeInfo(T, S, A), NumFields(Fields.size()), IsAggregate(IsAggregate) {

      TupleFieldInfo *FieldStorage = getFieldInfos();
      for (unsigned I = 0, E = Fields.size(); I != E; ++I)
        new(&FieldStorage[I]) TupleFieldInfo(Fields[I]);
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
      // FIXME
      return RValue();
    }

    void store(IRGenFunction &CGF, const RValue &RV, const LValue &LV) const {
      // FIXME
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

TupleFieldInfo *TupleTypeInfo::getFieldInfos() {
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
    // alignment, and the field's required alignment is more than its
    // IR preferred alignment, we need padding.
    if (Size OffsetFromAlignment = StorageSize % StorageAlignment) {
      Alignment FieldIRAlignment(
          IGM.TargetData.getABITypeAlignment(TInfo.StorageType));
      assert(FieldIRAlignment <= TInfo.StorageAlignment);
      if (FieldIRAlignment != TInfo.StorageAlignment) {
        unsigned PaddingRequired
          = StorageAlignment.getValue() - OffsetFromAlignment.getValue();
        StorageTypes.push_back(llvm::ArrayType::get(IGM.Int8Ty,
                                                    PaddingRequired));
        StorageSize += Size(PaddingRequired);
      }
    }

    FieldInfo.StorageIndex = StorageTypes.size();
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
