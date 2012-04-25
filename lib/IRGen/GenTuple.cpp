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
#include "swift/AST/Pattern.h"
#include "swift/Basic/Optional.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Target/TargetData.h"

#include "ASTVisitor.h"
#include "GenInit.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "Explosion.h"

#include "GenTuple.h"

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

    unsigned IsPOD : 1;

    /// The index of this field into the storage type, or NoStorage
    /// if the field requires no storage.
    unsigned StorageIndex : 23;
    enum : unsigned { NoStorage = 0x7FFFFFU };

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

    TupleTypeInfo(llvm::Type *T, Size size, Alignment align, IsPOD_t isPOD,
                  unsigned maximalSize, unsigned minimalSize,
                  ArrayRef<TupleFieldInfo> fields)
      : TypeInfo(T, size, align, isPOD), NumFields(fields.size()),
        MaximalExplosionSize(maximalSize), MinimalExplosionSize(minimalSize) {

      TupleFieldInfo *storage = getFieldInfoStorage();
      for (unsigned i = 0, e = fields.size(); i != e; ++i)
        new(&storage[i]) TupleFieldInfo(fields[i]);
    }

  public:
    static TupleTypeInfo *create(llvm::Type *storageType,
                                 Size size, Alignment align, IsPOD_t isPOD,
                                 unsigned maximalSize, unsigned minimalSize,
                                 ArrayRef<TupleFieldInfo> fields) {
      void *buffer = new char[sizeof(TupleTypeInfo)
                                + fields.size() * sizeof(TupleFieldInfo)];
      return new(buffer) TupleTypeInfo(storageType, size, align, isPOD,
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

    /// Get the range of explosion indices associated with a given field.
    static std::pair<unsigned, unsigned>
    getFieldProjectionRange(const TupleFieldInfo &field, ExplosionKind kind) {
      switch (kind) {
      case ExplosionKind::Maximal:
        return std::make_pair(field.MaximalBegin, field.MaximalEnd);

      case ExplosionKind::Minimal:
        return std::make_pair(field.MinimalBegin, field.MinimalEnd);
      }
      llvm_unreachable("bad explosion kind!");
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

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {
      for (auto &field : getFieldInfos()) {
        // Ignore fields that don't have storage.
        if (!field.hasStorage()) continue;

        Address fieldAddr = projectAddress(IGF, addr, field);
        field.Type.loadAsTake(IGF, fieldAddr, e);
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

    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      for (auto &field : getFieldInfos())
        field.Type.copy(IGF, src, dest);
    }

    void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      for (auto &field : getFieldInfos())
        field.Type.manage(IGF, src, dest);
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      for (auto &field : getFieldInfos())
        if (!field.Type.isPOD(ResilienceScope::Local))
          field.Type.destroy(IGF, projectAddress(IGF, addr, field));
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

  IsPOD_t pod = IsPOD;
  Size storageSize;
  Alignment storageAlignment(1);

  // TODO: rearrange the tuple for optimal packing.
  for (const TupleTypeElt &field : T->getFields()) {
    const TypeInfo &fieldTI = getFragileTypeInfo(IGM, field.getType());
    assert(fieldTI.isComplete());

    pod &= fieldTI.isPOD(ResilienceScope::Local);

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

  return TupleTypeInfo::create(convertedTy, storageSize, storageAlignment, pod,
                               maximalExplosionSize, minimalExplosionSize,
                               fieldInfos);
}

void swift::irgen::emitTupleLiteral(IRGenFunction &IGF, TupleExpr *E,
                                    Explosion &explosion) {
 for (Expr *elt : E->getElements())
   if (!elt) {
     IGF.unimplemented(E->getLoc(), "tuple default element");
     IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()),
                           explosion);
     return;
   }

  // Emit all the sub-expressions.
  for (Expr *elt : E->getElements())
    IGF.emitRValue(elt, explosion);
}

namespace {
  class TupleElement : public PhysicalPathComponent {
    const TupleFieldInfo &Field;

  public:
    TupleElement(const TupleFieldInfo &field) : Field(field) {}

    OwnedAddress offset(IRGenFunction &IGF, OwnedAddress addr) const {
      Address project = TupleTypeInfo::projectAddress(IGF, addr, Field);
      return OwnedAddress(project, addr.getOwner());
    }
  };
}

void swift::irgen::emitTupleElement(IRGenFunction &IGF, TupleElementExpr *E,
                                    Explosion &explosion) {
  // If we're doing an l-value projection, this is straightforward.
  if (LValueType *lv = E->getType()->getAs<LValueType>())
    return IGF.emitLValueAsScalar(emitTupleElementLValue(IGF, E),
                                  lv->isHeap() ? OnHeap : NotOnHeap,
                                  explosion);

  Expr *tuple = E->getBase();
  const TupleTypeInfo &tupleType = getAsTupleTypeInfo(IGF, tuple->getType());

  const TupleFieldInfo &field =
    tupleType.getFieldInfos()[E->getFieldNumber()];

  // If the field requires no storage, there's nothing to do.
  if (!field.hasStorage()) {
    // Emit the base in case it has side-effects.
    IGF.emitIgnored(tuple);
    return IGF.emitFakeExplosion(field.Type, explosion);
  }

  // If we can emit the base as an l-value, we can avoid a lot
  // of unnecessary work.
  if (Optional<Address> tupleAddr = IGF.tryEmitAsAddress(tuple, tupleType)) {
    Address addr = tupleType.projectAddress(IGF, tupleAddr.getValue(), field);
    return field.Type.load(IGF, addr, explosion);
  }

  // Otherwise, emit the base as an r-value and project.
  Explosion tupleExplosion(explosion.getKind());
  IGF.emitRValue(tuple, tupleExplosion);

  auto fieldRange =
    tupleType.getFieldProjectionRange(field, explosion.getKind());

  // Ignore up to the start of the range.
  tupleExplosion.ignoreAndDestroy(IGF, fieldRange.first);

  // Transfer the correct range.
  tupleExplosion.transferInto(explosion, fieldRange.second - fieldRange.first);

  // Ignore everything else.
  tupleExplosion.ignoreAndDestroy(IGF, tupleExplosion.size());
}

/// Try to emit a tuple-element reference expression as an address.
Optional<Address>
swift::irgen::tryEmitTupleElementAsAddress(IRGenFunction &IGF,
                                           TupleElementExpr *E) {
  Expr *tuple = E->getBase();

  // There are two kinds of TupleElementExprs; ones where the input is an
  // lvalue, and ones where the input is an rvalue.  Either way, we just
  // want to tryEmitAsAddress on the operand and GEP into it.
  CanType TT = tuple->getType()->getCanonicalType();
  if (!isa<TupleType>(TT))
    TT = cast<LValueType>(TT)->getObjectType()->getCanonicalType();

  const TupleTypeInfo &tupleType = getAsTupleTypeInfo(IGF, TT);

  // This is contigent exclusively on whether we can emit an address
  // for the tuple.
  Optional<Address> tupleAddr = IGF.tryEmitAsAddress(tuple, tupleType);
  if (!tupleAddr) return Nothing;

  // We succeeded;  now just GEP down.
  const TupleFieldInfo &field =
    tupleType.getFieldInfos()[E->getFieldNumber()];
  if (!field.hasStorage()) return Address();

  return tupleType.projectAddress(IGF, tupleAddr.getValue(), field);
}

LValue swift::irgen::emitTupleElementLValue(IRGenFunction &IGF,
                                            TupleElementExpr *E) {
  assert(E->getType()->is<LValueType>());

  // Emit the base l-value.
  Expr *tuple = E->getBase();
  LValue tupleLV = IGF.emitLValue(tuple);

  Type tupleType = tuple->getType()->castTo<LValueType>()->getObjectType();
  const TupleTypeInfo &tupleTI = getAsTupleTypeInfo(IGF, tupleType);
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
void swift::irgen::emitTupleShuffle(IRGenFunction &IGF, TupleShuffleExpr *E,
                                    Explosion &outerTupleExplosion) {
  Expr *innerTuple = E->getSubExpr();
  const TupleTypeInfo &innerTupleType =
    getAsTupleTypeInfo(IGF, innerTuple->getType());

  // Emit the inner tuple.  We prefer to emit it as an address.
  Explosion innerTupleExplosion(outerTupleExplosion.getKind());
  Address innerTupleAddr;
  if (Optional<Address> addr
        = IGF.tryEmitAsAddress(innerTuple, innerTupleType)) {
    innerTupleAddr = addr.getValue();
  } else {
    IGF.emitRValue(innerTuple, innerTupleExplosion);
  }

  llvm::ArrayRef<TupleTypeElt> outerFields =
    E->getType()->castTo<TupleType>()->getFields();

  auto shuffleIndexIterator = E->getElementMapping().begin();
  for (const TupleTypeElt &outerField : outerFields) {
    int shuffleIndex = *shuffleIndexIterator++;

    // If the shuffle index is -1, we're supposed to use the default value.
    if (shuffleIndex == -1) {
      assert(outerField.hasInit() && "no default initializer for field!");
      IGF.emitRValue(outerField.getInit(), outerTupleExplosion);
      continue;
    }

    // Otherwise, we need to map from a different tuple.
    assert(shuffleIndex >= 0 &&
           (unsigned) shuffleIndex < outerFields.size());

    const TupleFieldInfo &innerField
      = innerTupleType.getFieldInfos()[(unsigned) shuffleIndex];

    // If we're loading from an l-value, project from that.
    if (innerTupleAddr.isValid()) {
      Address elementAddr = innerTupleType.projectAddress(IGF, innerTupleAddr,
                                                          innerField);
      innerField.Type.load(IGF, elementAddr, outerTupleExplosion);

    // Otherwise, project the r-value down.
    } else {
      // Get the range of elements and project those down.
      auto fieldRange =
        innerTupleType.getFieldProjectionRange(innerField,
                                               innerTupleExplosion.getKind());
      outerTupleExplosion.add(innerTupleExplosion.getRange(fieldRange.first,
                                                           fieldRange.second));
    }
  }

  // Tuple shuffles always use everything from the inner tuple.
  innerTupleExplosion.markClaimed(innerTupleExplosion.size());
}

namespace {
  /// A visitor for initializing a pattern from an address.
  struct InitPatternFromAddress
      : irgen::PatternVisitor<InitPatternFromAddress> {
    IRGenFunction &IGF;
    Initialization &I;
    Address SrcAddr;

    InitPatternFromAddress(IRGenFunction &IGF, Initialization &I, Address addr)
      : IGF(IGF), I(I), SrcAddr(addr) {}

    void visitAnyPattern(AnyPattern *P) {
      // No need to copy anything out.
    }

    void visitNamedPattern(NamedPattern *P) {
      VarDecl *var = P->getDecl();

      const TypeInfo &fieldTI = IGF.getFragileTypeInfo(var->getType());
      Address destAddr = I.emitVariable(IGF, var, fieldTI);
      fieldTI.initializeWithCopy(IGF, destAddr, SrcAddr);

      // The validity of marking this after the initialization comes from
      // the assumption that initializeWithCopy is atomic w.r.t.
      // exceptions and control flow.
      I.markInitialized(IGF, I.getObjectForDecl(var));
    }

    void visitTuplePattern(TuplePattern *P) {
      visitTuplePattern(P, getAsTupleTypeInfo(IGF, P->getType()));
    }

    void visitTuplePattern(TuplePattern *P, const TupleTypeInfo &tupleTI) {
      Address srcTupleAddr = SrcAddr;
      for (unsigned i = 0, e = P->getNumFields(); i != e; ++i) {
        auto &field = tupleTI.getFieldInfos()[i];
        if (!field.hasStorage()) continue;

        // Get the element pattern, skipping obviously ignored ones.
        Pattern *fieldP =
          P->getFields()[i].getPattern()->getSemanticsProvidingPattern();
        if (isa<AnyPattern>(fieldP)) continue;

        // Otherwise, change the source address and recurse on each field.
        SrcAddr = tupleTI.projectAddress(IGF, srcTupleAddr, field);
        visit(fieldP);
      }
    }
  };
}

/// Emit an initializer for a tuple pattern.
void swift::irgen::emitTuplePatternInitFromAddress(IRGenFunction &IGF,
                                                   Initialization &I,
                                                   Address addr,
                                                   TuplePattern *P,
                                                   const TypeInfo &TI) {
  const TupleTypeInfo &tupleTI = getAsTupleTypeInfo(TI);

  // If we can emit the initializer as an address, we can project
  // and copy directly.
  InitPatternFromAddress(IGF, I, addr).visitTuplePattern(P, tupleTI);
}
