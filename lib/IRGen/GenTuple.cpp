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
#include "swift/AST/Expr.h"
#include "swift/Basic/Optional.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Target/TargetData.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "Explosion.h"

using namespace swift;
using namespace irgen;

static StringRef getFieldName(const TupleTypeElt &field) {
  if (field.hasName())
    return field.getName().str();
  return "elt";
}

namespace {
  /// A class describing the IR layout for a particular field.
  struct TupleFieldInfo {
    TupleFieldInfo(const TupleTypeElt &field, const TypeInfo &type)
      : Field(field), Type(type) {}

    /// The field.
    const TupleTypeElt &Field;

    /// The TypeInfo for the field.
    const TypeInfo &Type;

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

  /// Layout information for tuple types.
  class TupleTypeInfo : public TypeInfo {
    /// The number of TupleFieldInfos for this type.  Equal to the
    /// number of fields in the tuple.  The actual data is stored
    /// after the TypeInfo.
    unsigned NumFields;
    unsigned MaximalExplosionSize;
    unsigned MinimalExplosionSize;

    TupleFieldInfo *getFieldInfoStorage() {
      return reinterpret_cast<TupleFieldInfo*>(this+1);
    }
    const TupleFieldInfo *getFieldInfoStorage() const {
      return const_cast<TupleTypeInfo*>(this)->getFieldInfoStorage();
    }

    TupleTypeInfo(llvm::Type *T, Size S, Alignment A,
                  unsigned maximalSize, unsigned minimalSize,
                  ArrayRef<TupleFieldInfo> fields)
      : TypeInfo(T, S, A), NumFields(fields.size()),
        MaximalExplosionSize(maximalSize), MinimalExplosionSize(minimalSize) {

      TupleFieldInfo *storage = getFieldInfoStorage();
      for (unsigned i = 0, e = fields.size(); i != e; ++i)
        new(&storage[i]) TupleFieldInfo(fields[i]);
    }

  public:
    static TupleTypeInfo *create(llvm::Type *storageType,
                                 Size size, Alignment align,
                                 unsigned maximalSize, unsigned minimalSize,
                                 ArrayRef<TupleFieldInfo> fields) {
      void *buffer = new char[sizeof(TupleTypeInfo)
                                + fields.size() * sizeof(TupleFieldInfo)];
      return new(buffer) TupleTypeInfo(storageType, size, align,
                                       maximalSize, minimalSize, fields);
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

    unsigned getExplosionSize(ExplosionKind level) const {
      switch (level) {
      case ExplosionKind::Minimal: return MinimalExplosionSize;
      case ExplosionKind::Maximal: return MaximalExplosionSize;
      }
      llvm_unreachable("bad explosion level");
    }

    void getSchema(ExplosionSchema &schema) const {
      for (auto &fieldInfo : getFieldInfos()) {
        fieldInfo.Type.getSchema(schema);
      }
    }

    void load(IRGenFunction &IGF, Address addr, Explosion &e) const {
      for (auto &field : getFieldInfos()) {
        // Ignore fields that don't have storage.
        if (!field.hasStorage()) continue;

        Address fieldAddr = projectAddress(IGF, addr, field);
        field.Type.load(IGF, fieldAddr, e);
      }
    }

    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {
      for (auto &field : getFieldInfos()) {
        // Ignore fields that don't have storage.
        if (!field.hasStorage()) continue;

        Address fieldAddr = projectAddress(IGF, addr, field);
        field.Type.assign(IGF, e, fieldAddr);
      }
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {
      for (auto &field : getFieldInfos()) {
        // Ignore fields that don't have storage.
        if (!field.hasStorage()) continue;

        Address fieldAddr = projectAddress(IGF, addr, field);
        field.Type.initialize(IGF, e, fieldAddr);
      }
    }

    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      for (auto &field : getFieldInfos())
        field.Type.reexplode(IGF, src, dest);
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

const TypeInfo *
TypeConverter::convertTupleType(IRGenModule &IGM, TupleType *T) {
  SmallVector<TupleFieldInfo, 8> fieldInfos;
  SmallVector<llvm::Type*, 8> storageTypes;
  fieldInfos.reserve(T->getFields().size());
  storageTypes.reserve(T->getFields().size());

  unsigned maximalExplosionSize = 0, minimalExplosionSize = 0;

  Size storageSize;
  Alignment storageAlignment(1);

  // TODO: rearrange the tuple for optimal packing.
  for (const TupleTypeElt &field : T->getFields()) {
    const TypeInfo &fieldTI = getFragileTypeInfo(IGM, field.getType());
    assert(fieldTI.isComplete());

    fieldInfos.push_back(TupleFieldInfo(field, fieldTI));
    TupleFieldInfo &fieldInfo = fieldInfos.back();

    fieldInfo.MaximalBegin = maximalExplosionSize;
    maximalExplosionSize += fieldTI.getExplosionSize(ExplosionKind::Maximal);
    fieldInfo.MaximalEnd = maximalExplosionSize;

    fieldInfo.MinimalBegin = minimalExplosionSize;
    minimalExplosionSize += fieldTI.getExplosionSize(ExplosionKind::Minimal);
    fieldInfo.MinimalEnd = minimalExplosionSize;

    // Ignore zero-sized fields.
    if (fieldTI.StorageSize.isZero()) {
      fieldInfo.StorageIndex = TupleFieldInfo::NoStorage;
      continue;
    }

    storageAlignment = std::max(storageAlignment, fieldTI.StorageAlignment);

    // If the current tuple size isn't a multiple of the tuple
    // alignment, we need padding.
    if (Size offsetFromAlignment = storageSize % storageAlignment) {
      unsigned paddingRequired
        = storageAlignment.getValue() - offsetFromAlignment.getValue();

      // We don't actually need to uglify the IR unless the natural
      // alignment of the IR type for the field isn't good enough.
      Alignment fieldIRAlignment(
          IGM.TargetData.getABITypeAlignment(fieldTI.StorageType));
      assert(fieldIRAlignment <= fieldTI.StorageAlignment);
      if (fieldIRAlignment != fieldTI.StorageAlignment) {
        storageTypes.push_back(llvm::ArrayType::get(IGM.Int8Ty,
                                                    paddingRequired));
      }

      // Regardless, the storage size goes up.
      storageSize += Size(paddingRequired);
    }

    fieldInfo.StorageIndex = storageTypes.size();
    fieldInfo.StorageOffset = storageSize;
    storageTypes.push_back(fieldTI.getStorageType());
    storageSize += fieldTI.StorageSize;
  }

  // If the tuple requires no storage at all, just use i8.  Most
  // clients will just ignore zero-size types, but those that care can
  // have a sensible one-byte type.
  llvm::Type *convertedTy;
  if (storageSize.isZero()) {
    convertedTy = IGM.Int8Ty;
  } else {
    convertedTy = llvm::StructType::get(IGM.getLLVMContext(), storageTypes);
  }

  return TupleTypeInfo::create(convertedTy, storageSize, storageAlignment,
                               maximalExplosionSize, minimalExplosionSize,
                               fieldInfos);
}

void IRGenFunction::emitTupleLiteral(TupleExpr *E, Explosion &explosion) {
  // Emit all the sub-expressions.
  for (Expr *elt : E->getElements()) {
    emitRValue(elt, explosion);
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

void IRGenFunction::emitTupleElement(TupleElementExpr *E,
                                     Explosion &explosion) {
  // If we're doing an l-value projection, this is straightforward.
  if (E->getType()->is<LValueType>())
    return emitLValueAsScalar(emitTupleElementLValue(E), explosion);

  Expr *tuple = E->getBase();
  const TupleTypeInfo &tupleType = getAsTupleTypeInfo(*this, tuple->getType());

  const TupleFieldInfo &field =
    tupleType.getFieldInfos()[E->getFieldNumber()];

  // If the field requires no storage, there's nothing to do.
  if (!field.hasStorage()) {
    // Emit the base in case it has side-effects.
    emitIgnored(tuple);
    return emitFakeExplosion(field.Type, explosion);
  }

  // If we can emit the base as an l-value, we can avoid a lot
  // of unnecessary work.
  if (Optional<Address> tupleAddr = tryEmitAsAddress(tuple, tupleType)) {
    Address addr = tupleType.projectAddress(*this, tupleAddr.getValue(), field);
    return field.Type.load(*this, addr, explosion);
  }

  // Otherwise, emit the base as an r-value and project.
  Explosion tupleExplosion(explosion.getKind());
  emitRValue(tuple, tupleExplosion);
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

LValue IRGenFunction::emitTupleElementLValue(TupleElementExpr *E) {
  assert(E->getType()->is<LValueType>());

  // Emit the base l-value.
  Expr *tuple = E->getBase();
  LValue tupleLV = emitLValue(tuple);

  Type tupleType = tuple->getType()->castTo<LValueType>()->getObjectType();
  const TupleTypeInfo &tupleTI = getAsTupleTypeInfo(*this, tupleType);
  const TupleFieldInfo &field =
    tupleTI.getFieldInfos()[E->getFieldNumber()];

  // If the field requires no storage, there's nothing to do.
  if (!field.hasStorage()) {
    return tupleLV; // as good as anything
  }

  // Project.
  tupleLV.add<TupleElement>(field);
  return tupleLV;
}

/// emitTupleShuffle - Emit a tuple-shuffle expression
/// as an exploded r-value.
void IRGenFunction::emitTupleShuffle(TupleShuffleExpr *E,
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
    emitRValue(innerTuple, innerTupleExplosion);
  }

  llvm::ArrayRef<TupleTypeElt> outerFields =
    E->getType()->castTo<TupleType>()->getFields();

  auto shuffleIndexIterator = E->getElementMapping().begin();
  for (const TupleTypeElt &outerField : outerFields) {
    int shuffleIndex = *shuffleIndexIterator++;

    // If the shuffle index is -1, we're supposed to use the default value.
    if (shuffleIndex == -1) {
      assert(outerField.hasInit() && "no default initializer for field!");
      emitRValue(outerField.getInit(), outerTupleExplosion);
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
      innerField.Type.load(*this, elementAddr, outerTupleExplosion);

    // Otherwise, project the r-value down.
    } else {
      innerTupleType.projectExplosion(innerTupleExplosion, innerField,
                                      outerTupleExplosion);
    }
  }
}
