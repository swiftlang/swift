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
#include "swift/Basic/Optional.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Target/TargetData.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "RValue.h"
#include "Explosion.h"

using namespace swift;
using namespace irgen;

static StringRef getFieldName(const TupleTypeElt &field) {
  if (!field.Name.empty())
    return field.Name.str();
  return "elt";
}

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

    bool hasStorage() const { return StorageIndex != NoStorage; }

    /// The range of this field within the scalars for the tuple.
    unsigned ScalarBegin : 4;
    unsigned ScalarEnd : 4;
    unsigned MaximalBegin : 16;
    unsigned MaximalEnd : 16;
    unsigned MinimalBegin : 16;
    unsigned MinimalEnd : 16;
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

    /// Perform an l-value projection of a member of this tuple.
    static Address projectAddress(IRGenFunction &IGF, Address tuple,
                                  const TupleFieldInfo &field) {
      if (!field.hasStorage())
        return tuple;

      llvm::Value *addr = tuple.getAddress();
      addr = IGF.Builder.CreateStructGEP(addr, field.StorageIndex,
                     addr->getName() + "." + getFieldName(field.Field));

      // Compute the adjusted alignment.
      Alignment align =
        tuple.getAlignment().alignmentAtOffset(field.StorageOffset);

      return Address(addr, align);
    }

    /// Perform an exploded r-value projection.
    static void projectExplosion(Explosion &inner, const TupleFieldInfo &field,
                                 Explosion &outer) {
      assert(inner.getKind() == outer.getKind());
      switch (inner.getKind()) {
      case ExplosionKind::Maximal:
        outer.add(inner.getRange(field.MaximalBegin, field.MaximalEnd));
        return;

      case ExplosionKind::Minimal:
        outer.add(inner.getRange(field.MinimalBegin, field.MinimalEnd));
        return;
      }
      llvm_unreachable("bad explosion kind");
    }

    /// Perform an r-value projection of a member of this tuple.
    virtual RValue projectRValue(IRGenFunction &IGF, const RValue &tuple,
                                 const TupleFieldInfo &field) const = 0;

    /// Form a tuple from a bunch of r-values.
    virtual RValue implode(IRGenFunction &IGF, Explosion &explosion) const = 0;

    void getExplosionSchema(ExplosionSchema &schema) const {
      for (auto &fieldInfo : getFieldInfos()) {
        fieldInfo.FieldInfo.getExplosionSchema(schema);
      }
    }

    void loadExplosion(IRGenFunction &IGF, Address addr, Explosion &e) const {
      for (auto &field : getFieldInfos()) {
        // Ignore fields that don't have storage.
        if (!field.hasStorage()) continue;

        Address fieldAddr = projectAddress(IGF, addr, field);
        field.FieldInfo.loadExplosion(IGF, fieldAddr, e);
      }
    }

    void storeExplosion(IRGenFunction &IGF, Explosion &e, Address addr) const {
      for (auto &field : getFieldInfos()) {
        // Ignore fields that don't have storage.
        if (!field.hasStorage()) continue;

        Address fieldAddr = projectAddress(IGF, addr, field);
        field.FieldInfo.storeExplosion(IGF, e, fieldAddr);
      }
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

    unsigned getNumScalars() const {
      return Schema.getScalarTypes().size();
    }

    unsigned getExplosionSize(ExplosionKind kind) const {
      return getNumScalars();
    }

    RValueSchema getSchema() const {
      return Schema;
    }

    RValue projectRValue(IRGenFunction &IGF, const RValue &tuple,
                         const TupleFieldInfo &field) const {
      ArrayRef<llvm::Value*> scalars = tuple.getScalars();
      return RValue::forScalars(scalars.slice(field.ScalarBegin,
                                       field.ScalarEnd - field.ScalarBegin));
    }

    RValue implode(IRGenFunction &IGF, Explosion &elements) const {
      return RValue::forScalars(elements.claim(getNumScalars()));
    }

    RValue load(IRGenFunction &IGF, Address addr) const {
      SmallVector<llvm::Value*, RValue::MaxScalars> scalars;

      // Load by loading the elements.
      for (const TupleFieldInfo &field : getFieldInfos()) {
        assert(scalars.size() == field.ScalarBegin);

        // Skip fields that contribute no scalars.
        if (field.ScalarBegin == field.ScalarEnd) continue;

        // Load the field and extract the scalars.
        Address fieldAddr = projectAddress(IGF, addr, field);
        RValue fieldRV = field.FieldInfo.load(IGF, fieldAddr);
        scalars.append(fieldRV.getScalars().begin(),
                       fieldRV.getScalars().end());

        assert(scalars.size() == field.ScalarEnd);
      }

      return RValue::forScalars(scalars);
    }

    void store(IRGenFunction &IGF, const RValue &RV, Address addr) const {
      assert(RV.isScalar());

      for (const TupleFieldInfo &field : getFieldInfos()) {
        // Skip fields that contribute no scalars.
        if (field.ScalarBegin == field.ScalarEnd) continue;

        // Project out the appropriate r-value.
        ArrayRef<llvm::Value*> scalars = 
          RV.getScalars().slice(field.ScalarBegin,
                                field.ScalarEnd - field.ScalarBegin);
        RValue fieldRV = RValue::forScalars(scalars);

        // Write the extracted r-value into a projected l-value.
        Address fieldAddr = projectAddress(IGF, addr, field);
        field.FieldInfo.store(IGF, fieldRV, fieldAddr);
      }
    }
  };

  /// A TypeInfo implementation for tuples that are passed around as
  /// aggregates.
  class AggregateTupleTypeInfo : public TupleTypeInfo {
    unsigned MaximalExplosionSize;
    unsigned MinimalExplosionSize;

    AggregateTupleTypeInfo(llvm::Type *T, Size S, Alignment A,
                           ArrayRef<TupleFieldInfo> Fields,
                           unsigned maximalExplosionSize,
                           unsigned minimalExplosionSize)
      : TupleTypeInfo(T, S, A, /*agg*/ true, Fields),
        MaximalExplosionSize(maximalExplosionSize),
        MinimalExplosionSize(minimalExplosionSize) {
    }

  public:
    static AggregateTupleTypeInfo *create(llvm::StructType *T,
                                          Size S, Alignment A,
                                          ArrayRef<TupleFieldInfo> FieldInfos,
                                          unsigned maximalExplosionSize,
                                          unsigned minimalExplosionSize) {
      void *Storage = operator new(sizeof(AggregateTupleTypeInfo) +
                                   sizeof(TupleFieldInfo) * FieldInfos.size());
      return new(Storage) AggregateTupleTypeInfo(T, S, A, FieldInfos,
                                                 maximalExplosionSize,
                                                 minimalExplosionSize);
    }

    unsigned getExplosionSize(ExplosionKind kind) const {
      switch (kind) {
      case ExplosionKind::Maximal: return MaximalExplosionSize;
      case ExplosionKind::Minimal: return MinimalExplosionSize;
      }
      llvm_unreachable("bad explosion kind");
    }

    llvm::StructType *getStorageType() const {
      return cast<llvm::StructType>(TypeInfo::getStorageType());
    }

    RValueSchema getSchema() const {
      return RValueSchema::forAggregate(getStorageType(), StorageAlignment);
    }

    /// Given an r-value of this type, form an address referring to
    /// the temporary.
    Address getAddressForAggregateRValue(const RValue &rvalue) const {
      assert(rvalue.isAggregate());

      // The alignment of a temporary is always the alignment of the type.
      return Address(rvalue.getAggregateAddress(), StorageAlignment);
    }

    RValue projectRValue(IRGenFunction &IGF, const RValue &tuple,
                         const TupleFieldInfo &field) const {
      assert(tuple.isAggregate());
      Address tupleAddr = getAddressForAggregateRValue(tuple);
      Address fieldAddr = projectAddress(IGF, tupleAddr, field);

      // If we need an aggregate, we've already got one.
      if (field.FieldInfo.getSchema().isAggregate()) {
        assert(field.FieldInfo.StorageAlignment <= fieldAddr.getAlignment());
        return RValue::forAggregate(fieldAddr.getAddress());
      }

      // Otherwise, we need to load the scalars out.
      return field.FieldInfo.load(IGF, fieldAddr);
    }

    RValue implode(IRGenFunction &IGF, Explosion &explosion) const {
      Address temp = IGF.createFullExprAlloca(StorageType, StorageAlignment,
                                              "tuple-implode");

      for (const TupleFieldInfo &field : getFieldInfos()) {
        Address fieldAddr = projectAddress(IGF, temp, field);
        field.FieldInfo.storeExplosion(IGF, explosion, fieldAddr);
      }

      return RValue::forAggregate(temp.getAddress());
    }

    RValue load(IRGenFunction &IGF, Address addr) const {
      Address temp = IGF.createFullExprAlloca(StorageType, StorageAlignment,
                                             "lvalue-load");
      // FIXME: a memcpy isn't right if any of the fields require
      // special logic for loads or stores.
      IGF.emitMemCpy(temp.getAddress(), addr.getAddress(), StorageSize,
                     std::min(StorageAlignment, addr.getAlignment()));
      return RValue::forAggregate(temp.getAddress());
    }

    void store(IRGenFunction &IGF, const RValue &RV, Address addr) const {
      // FIXME: a memcpy isn't right if any of the fields require
      // special logic for loads or stores.
      IGF.emitMemCpy(addr.getAddress(), RV.getAggregateAddress(), StorageSize,
                     std::min(StorageAlignment, addr.getAlignment()));
    }
  };
}

static const TupleTypeInfo &getAsTupleTypeInfo(const TypeInfo &typeInfo) {
  // It'd be nice to get some better verification than this.
#ifdef __GXX_RTTI
  assert(dynamic_cast<const TupleTypeInfo*>(&typeInfo));
#endif

  return typeInfo.as<TupleTypeInfo>();
}

static const TupleTypeInfo &getAsTupleTypeInfo(IRGenFunction &IGF, Type type) {
  assert(type->is<TupleType>());
  return getAsTupleTypeInfo(IGF.getFragileTypeInfo(type));
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

  unsigned maximalExplosionSize = 0, minimalExplosionSize = 0;

  bool HasAggregateField = false;

  Size StorageSize;
  Alignment StorageAlignment;

  // TODO: rearrange the tuple for optimal packing.
  for (const TupleTypeElt &Field : T->Fields) {
    const TypeInfo &TInfo = getFragileTypeInfo(IGM, Field.Ty);
    assert(TInfo.isComplete());

    FieldInfos.push_back(TupleFieldInfo(Field, TInfo));
    TupleFieldInfo &FieldInfo = FieldInfos.back();

    FieldInfo.MaximalBegin = maximalExplosionSize;
    maximalExplosionSize += TInfo.getExplosionSize(ExplosionKind::Maximal);
    FieldInfo.MaximalEnd = maximalExplosionSize;

    FieldInfo.MinimalBegin = minimalExplosionSize;
    minimalExplosionSize += TInfo.getExplosionSize(ExplosionKind::Minimal);
    FieldInfo.MinimalEnd = minimalExplosionSize;

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
    assert(ScalarTypes.size() == maximalExplosionSize);
    assert(ScalarTypes.size() == minimalExplosionSize);
    return ScalarTupleTypeInfo::create(Converted, StorageSize, StorageAlignment,
                                       ScalarTypes, FieldInfos);
  }

  // Otherwise, pass the entire thing as an aggregate.  This is a bit
  // wasteful; we could really pass some of the fields as aggregates
  // and some as scalars.
  return AggregateTupleTypeInfo::create(Converted, StorageSize,
                                        StorageAlignment, FieldInfos,
                                        maximalExplosionSize,
                                        minimalExplosionSize);
}

void IRGenFunction::emitExplodedTupleLiteral(TupleExpr *E,
                                             Explosion &explosion) {
  // Emit all the sub-expressions.
  for (Expr *elt : E->getElements()) {
    emitExplodedRValue(elt, explosion);
  }
}

namespace {
  class TupleElement : public PhysicalPathComponent {
    const TupleFieldInfo &Field;

  public:
    TupleElement(const TupleFieldInfo &field)
      : PhysicalPathComponent(sizeof(TupleElement)), Field(field) {}

    Address offset(IRGenFunction &IGF, Address addr) const {
      return TupleTypeInfo::projectAddress(IGF, addr, Field);
    }
  };
}

void IRGenFunction::emitExplodedTupleElement(TupleElementExpr *E,
                                             Explosion &explosion) {
  Expr *tuple = E->getBase();
  const TupleTypeInfo &tupleType = getAsTupleTypeInfo(*this, tuple->getType());

  const TupleFieldInfo &field =
    tupleType.getFieldInfos()[E->getFieldNumber()];

  // If the field requires no storage, there's nothing to do.
  if (!field.hasStorage()) {
    // Emit the base in case it has side-effects.
    emitIgnored(tuple);
    return emitFakeExplosion(field.FieldInfo, explosion);
  }

  // If we can emit the base as an l-value, we can avoid a lot
  // of unnecessary work.
  if (Optional<Address> tupleAddr = tryEmitAsAddress(tuple, tupleType)) {
    Address addr = tupleType.projectAddress(*this, tupleAddr.getValue(), field);
    return field.FieldInfo.loadExplosion(*this, addr, explosion);
  }

  // Otherwise, emit the base as an r-value and project.
  Explosion tupleExplosion(explosion.getKind());
  emitExplodedRValue(tuple, tupleExplosion);
  return tupleType.projectExplosion(tupleExplosion, field, explosion);
}

/// Try to emit a tuple-element reference expression as an address.
Optional<Address>
IRGenFunction::tryEmitTupleElementAsAddress(TupleElementExpr *E) {
  Expr *tuple = E->getBase();
  const TupleTypeInfo &tupleType = getAsTupleTypeInfo(*this, tuple->getType());

  // This is contigent exclusively on whether we can emit an address
  // for the tuple.
  Optional<Address> tupleAddr = tryEmitAsAddress(tuple, tupleType);
  if (!tupleAddr) return Nothing;

  // We succeeded;  now just GEP down.
  const TupleFieldInfo &field =
    tupleType.getFieldInfos()[E->getFieldNumber()];
  if (!field.hasStorage()) return Address();

  return tupleType.projectAddress(*this, tupleAddr.getValue(), field);
}

LValue IRGenFunction::emitTupleElementLValue(TupleElementExpr *E,
                                             const TypeInfo &fieldType) {
  // Emit the base l-value.
  Expr *tuple = E->getBase();
  const TupleTypeInfo &tupleType = getAsTupleTypeInfo(*this, tuple->getType());
  LValue tupleLV = emitLValue(tuple, tupleType);

  const TupleFieldInfo &field =
    tupleType.getFieldInfos()[E->getFieldNumber()];

  // If the field requires no storage, there's nothing to do.
  if (!field.hasStorage()) {
    return tupleLV; // as good as anything
  }

  // Project.
  tupleLV.push<TupleElement>(field);
  return tupleLV;
}

/// Emit a tuple shuffle in exploded form.
/// emitExplodedTupleShuffleExpr - Emit a tuple-shuffle expression
/// as an exploded r-value.
void IRGenFunction::emitExplodedTupleShuffle(TupleShuffleExpr *E,
                                             Explosion &outerTupleExplosion) {
  Expr *innerTuple = E->getSubExpr();
  const TupleTypeInfo &innerTupleType =
    getAsTupleTypeInfo(*this, innerTuple->getType());

  // Emit the inner tuple.  We prefer to emit it as an address.
  Explosion innerTupleExplosion(outerTupleExplosion.getKind());
  Address innerTupleAddr;
  if (Optional<Address> addr = tryEmitAsAddress(innerTuple, innerTupleType)) {
    innerTupleAddr = addr.getValue();
  } else {
    emitExplodedRValue(innerTuple, innerTupleExplosion);
  }

  llvm::ArrayRef<TupleTypeElt> outerFields =
    E->getType()->castTo<TupleType>()->Fields;

  auto shuffleIndexIterator = E->getElementMapping().begin();
  for (const TupleTypeElt &outerField : outerFields) {
    int shuffleIndex = *shuffleIndexIterator++;

    // If the shuffle index is -1, we're supposed to use the default value.
    if (shuffleIndex == -1) {
      assert(outerField.Init && "no default initializer for field!");
      emitExplodedRValue(outerField.Init, outerTupleExplosion);
      continue;
    }

    // Otherwise, we need to map from a different tuple.
    assert(shuffleIndex >= 0 &&
           (unsigned) shuffleIndex < outerFields.size());

    const TupleFieldInfo &innerField
      = innerTupleType.getFieldInfos()[(unsigned) shuffleIndex];

    // If we're loading from an l-value, project from that.
    if (innerTupleAddr.isValid()) {
      Address elementAddr = innerTupleType.projectAddress(*this, innerTupleAddr,
                                                          innerField);
      innerField.FieldInfo.loadExplosion(*this, elementAddr,
                                         outerTupleExplosion);

    // Otherwise, project the r-value down.
    } else {
      innerTupleType.projectExplosion(innerTupleExplosion, innerField,
                                      outerTupleExplosion);
    }
  }
}
