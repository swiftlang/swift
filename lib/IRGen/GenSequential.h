//===--- GenSequential.h - IR generation for sequential types ---*- C++ -*-===//
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
//  This file provides some common code for emitting sequential types.
//  A sequential type is something like a tuple or a struct.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENSEQUENTIAL_H
#define SWIFT_IRGEN_GENSEQUENTIAL_H

#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"
#include "FixedTypeInfo.h"
#include "TypeInfo.h"
#include "StructLayout.h"

namespace swift {
namespace irgen {

template <class, class, class> class SequentialTypeBuilder;

/// A field of a sequential type.
template <class FieldImpl> class SequentialField {
  const TypeInfo &FieldTI;

  template <class, class, class> friend class SequentialTypeBuilder;

  Size StorageOffset;

  /// IsPOD - is this element known to be POD with ResilienceScope::Local?
  unsigned IsPOD : 1;

  /// IsEmpty - is this element known to be empty with ResilienceScope::Local?
  unsigned IsEmpty : 1;

  /// StorageIndex - the index to use with getelementptr in order to
  /// access this element.
  unsigned StorageIndex : 30;

  /// MaximalBegin/MaximalEnd - the range of explosion indexes for
  /// this element, under a maximal explosion
  unsigned MaximalBegin : 16;
  unsigned MaximalEnd : 16;

  /// MinimalBegin/MinimalEnd - the range of explosion indexes for
  /// this element, under a minimal explosion
  unsigned MinimalBegin : 16;
  unsigned MinimalEnd : 16;

protected:
  SequentialField(const TypeInfo &elementTI) : FieldTI(elementTI) {}

  const FieldImpl *asImpl() const {
    return static_cast<const FieldImpl*>(this);
  }
public:
  const TypeInfo &getTypeInfo() const { return FieldTI; }

  bool isEmpty() const {
    return IsEmpty;
  }

  bool isPOD() const {
    return IsPOD;
  }

  Address projectAddress(IRGenFunction &IGF, Address seq) const {
    if (IsEmpty) return seq;

    llvm::Value *addr = seq.getAddress();
    return IGF.Builder.CreateStructGEP(seq, StorageIndex, StorageOffset,
                           addr->getName() + "." + asImpl()->getFieldName());
  }  

  std::pair<unsigned, unsigned> getProjectionRange(ExplosionKind kind) const {
    switch (kind) {
    case ExplosionKind::Maximal:
      return std::make_pair(MaximalBegin, MaximalEnd);
    case ExplosionKind::Minimal:
      return std::make_pair(MinimalBegin, MinimalEnd);
    }
    llvm_unreachable("bad explosion kind!");
  }
};

/// A metaprogrammed TypeInfo implementation for sequential types.
template <class Impl, class FieldImpl_>
class SequentialTypeInfo : public FixedTypeInfo { // FIXME: not true!
public:
  typedef FieldImpl_ FieldImpl;

private:
  const unsigned NumFields;
  unsigned MaximalExplosionSize : 16;
  unsigned MinimalExplosionSize : 16;

  const FieldImpl *getFieldsBuffer() const {
    return reinterpret_cast<const FieldImpl*>(static_cast<const Impl*>(this)+1);
  }
  FieldImpl *getFieldsBuffer() {
    return reinterpret_cast<FieldImpl*>(static_cast<Impl*>(this)+1);
  }

  template <class, class, class> friend class SequentialTypeBuilder;

protected:
  SequentialTypeInfo(llvm::Type *ty, unsigned numFields)
    : FixedTypeInfo(ty, Size(0), Alignment(0), IsPOD), NumFields(numFields) {
    assert(!isComplete());
  }

public:
  ArrayRef<FieldImpl> getFields() const {
    return ArrayRef<FieldImpl>(getFieldsBuffer(), NumFields);
  }

  unsigned getExplosionSize(ExplosionKind level) const {
    switch (level) {
    case ExplosionKind::Minimal: return MinimalExplosionSize;
    case ExplosionKind::Maximal: return MaximalExplosionSize;
    }
    llvm_unreachable("bad explosion level");
  }

  /// The standard schema is just all the fields jumbled together.
  void getSchema(ExplosionSchema &schema) const {
    for (auto &field : getFields()) {
      field.getTypeInfo().getSchema(schema);
    }
  }

  void load(IRGenFunction &IGF, Address addr, Explosion &out) const {
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address fieldAddr = field.projectAddress(IGF, addr);
      field.getTypeInfo().load(IGF, fieldAddr, out);
    }
  }

  void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &out) const {
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address fieldAddr = field.projectAddress(IGF, addr);
      field.getTypeInfo().loadAsTake(IGF, fieldAddr, out);
    }
  }

  void loadUnmanaged(IRGenFunction &IGF, Address addr, Explosion &out) const {
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;
      
      Address fieldAddr = field.projectAddress(IGF, addr);
      field.getTypeInfo().loadUnmanaged(IGF, fieldAddr, out);
    }
  }
  
  void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address fieldAddr = field.projectAddress(IGF, addr);
      field.getTypeInfo().assign(IGF, e, fieldAddr);
    }
  }

  void assignWithCopy(IRGenFunction &IGF, Address dest, Address src) const {
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address destField = field.projectAddress(IGF, dest);
      Address srcField = field.projectAddress(IGF, src);
      field.getTypeInfo().assignWithCopy(IGF, destField, srcField);
    }
  }

  void assignWithTake(IRGenFunction &IGF, Address dest, Address src) const {
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address destField = field.projectAddress(IGF, dest);
      Address srcField = field.projectAddress(IGF, src);
      field.getTypeInfo().assignWithTake(IGF, destField, srcField);
    }
  }

  void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;
      
      Address fieldAddr = field.projectAddress(IGF, addr);
      field.getTypeInfo().initialize(IGF, e, fieldAddr);
    }
  }

  void initializeWithCopy(IRGenFunction &IGF, Address dest,
                          Address src) const {
    // If we're POD, use the generic routine.
    if (isPOD(ResilienceScope::Local))
      return FixedTypeInfo::initializeWithCopy(IGF, dest, src);

    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address destField = field.projectAddress(IGF, dest);
      Address srcField = field.projectAddress(IGF, src);
      field.getTypeInfo().initializeWithCopy(IGF, destField, srcField);
    }
  }

  void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
    for (auto &field : getFields())
      field.getTypeInfo().reexplode(IGF, src, dest);
  }

  void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
    for (auto &field : getFields())
      field.getTypeInfo().copy(IGF, src, dest);
  }

  void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
    for (auto &field : getFields())
      field.getTypeInfo().manage(IGF, src, dest);
  }

  void retain(IRGenFunction &IGF, Explosion &e) const {
    llvm_unreachable("not retainable");
  }

  void release(IRGenFunction &IGF, Explosion &e) const {
    llvm_unreachable("not releasable");
  }
  
  void destroy(IRGenFunction &IGF, Address addr) const {
    for (auto &field : getFields())
      if (!field.isPOD())
        field.getTypeInfo().destroy(IGF, field.projectAddress(IGF, addr));
  }
};

/// A builder of sequential types.
///
/// Required for a full implementation:
///   TypeInfoImpl *construct(void *buffer, ArrayRef<ASTField> fields);
///   FieldImpl getFieldInfo(const ASTField &field, const TypeInfo &fieldTI);
///   Type getType(const ASTField &field);
///   void performLayout(ArrayRef<const TypeInfo *> fieldTypes);
///     - should call recordLayout with the layout
template <class BuilderImpl, class TypeInfoImpl, class ASTField>
class SequentialTypeBuilder {

public:
  typedef typename TypeInfoImpl::FieldImpl FieldImpl;

private:
  TypeInfoImpl *SeqTI;

protected:
  IRGenModule &IGM;
  SequentialTypeBuilder(IRGenModule &IGM) : SeqTI(nullptr), IGM(IGM) {}

  BuilderImpl *asImpl() { return static_cast<BuilderImpl*>(this); }

  void recordLayout(StructLayout &layout, llvm::Type *StorageType) {
    SeqTI->StorageType = StorageType;
    SeqTI->completeFixed(layout.getSize(), layout.getAlignment());

    FieldImpl *nextFieldInfo = SeqTI->getFieldsBuffer();    
    for (auto &fieldLayout : layout.getElements()) {
      FieldImpl &field = *nextFieldInfo++;
      field.StorageOffset = fieldLayout.ByteOffset;
      field.StorageIndex = fieldLayout.StructIndex;
    }
  }

public:
  static void completeEmpty(TypeInfoImpl *seqTI) {
    seqTI->completeFixed(Size(0), Alignment(1));
    assert(seqTI->isComplete());
  }

  TypeInfoImpl *create(llvm::ArrayRef<ASTField> astFields) {
    void *buffer = ::operator new(sizeof(TypeInfoImpl) +
                                  astFields.size() * sizeof(FieldImpl));
    return SeqTI = asImpl()->construct(buffer, astFields);
  }

  TypeInfoImpl *complete(llvm::ArrayRef<ASTField> astFields) {
    assert(SeqTI && "no allocated type info!");
    assert(!SeqTI->isComplete() && "completing type twice!");

    assert(astFields.size() == SeqTI->NumFields);

    if (astFields.empty()) {
      SeqTI->setPOD(IsPOD);
      SeqTI->MaximalExplosionSize = 0;
      SeqTI->MinimalExplosionSize = 0;
      asImpl()->completeEmpty(SeqTI);
      return SeqTI;
    }

    SmallVector<const TypeInfo *, 8> fieldTypesForLayout;
    fieldTypesForLayout.reserve(astFields.size());

    FieldImpl *nextFieldInfo = SeqTI->getFieldsBuffer();
    unsigned maximalExplosionSize = 0, minimalExplosionSize = 0;
    IsPOD_t seqIsPOD = IsPOD;
    Size storageSize;
    Alignment storageAlignment(1);

    for (auto &astField : astFields) {
      // Compute the field's type info.
      const TypeInfo &fieldTI =
        IGM.getFragileTypeInfo(asImpl()->getType(astField));
      assert(fieldTI.isComplete());
      fieldTypesForLayout.push_back(&fieldTI);

      FieldImpl &fieldInfo = *::new((void*) nextFieldInfo++)
                        FieldImpl(asImpl()->getFieldInfo(astField, fieldTI));

      // Record POD-ness.
      IsPOD_t isPOD = fieldTI.isPOD(ResilienceScope::Local);
      fieldInfo.IsPOD = bool(isPOD);
      seqIsPOD &= isPOD;

      fieldInfo.MaximalBegin = maximalExplosionSize;
      maximalExplosionSize += fieldTI.getExplosionSize(ExplosionKind::Maximal);
      fieldInfo.MaximalEnd = maximalExplosionSize;

      fieldInfo.MinimalBegin = minimalExplosionSize;
      minimalExplosionSize += fieldTI.getExplosionSize(ExplosionKind::Minimal);
      fieldInfo.MinimalEnd = minimalExplosionSize;

      bool isEmpty = fieldTI.isEmpty(ResilienceScope::Local);
      fieldInfo.IsEmpty = isEmpty;
    }

    SeqTI->setPOD(seqIsPOD);
    SeqTI->MaximalExplosionSize = maximalExplosionSize;
    SeqTI->MinimalExplosionSize = minimalExplosionSize;

    asImpl()->performLayout(fieldTypesForLayout);

    assert(SeqTI->isComplete());
    return SeqTI;
  }  
};

} // end namespace irgen
} // end namespace swift

#endif
