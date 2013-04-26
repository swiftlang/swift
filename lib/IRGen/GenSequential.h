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
  ElementLayout Layout;

  template <class, class, class> friend class SequentialTypeBuilder;

  /// MaximalBegin/MaximalEnd - the range of explosion indexes for
  /// this element, under a maximal explosion
  unsigned MaximalBegin : 16;
  unsigned MaximalEnd : 16;

  /// MinimalBegin/MinimalEnd - the range of explosion indexes for
  /// this element, under a minimal explosion
  unsigned MinimalBegin : 16;
  unsigned MinimalEnd : 16;

protected:
  explicit SequentialField(const TypeInfo &elementTI)
    : Layout(ElementLayout::getIncomplete(elementTI)) {}

  const FieldImpl *asImpl() const {
    return static_cast<const FieldImpl*>(this);
  }
public:
  const TypeInfo &getTypeInfo() const { return Layout.getType(); }

  void completeFrom(const ElementLayout &layout) {
    Layout.completeFrom(layout);
  }

  bool isEmpty() const {
    return Layout.isEmpty();
  }

  IsPOD_t isPOD() const {
    return Layout.isPOD();
  }

  Address projectAddress(IRGenFunction &IGF, Address seq,
                         NonFixedOffsets offsets) const {
    return Layout.project(IGF, seq, offsets, "." + asImpl()->getFieldName());
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
template <class Impl, class Base, class FieldImpl_>
class SequentialTypeInfo : public Base {
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
  const Impl &asImpl() const { return *static_cast<const Impl*>(this); }

  template <class... As> 
  SequentialTypeInfo(unsigned numFields, As&&...args)
    : Base(std::forward<As>(args)...), NumFields(numFields) {}

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
    auto offsets = asImpl().getNonFixedOffsets(IGF);
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address fieldAddr = field.projectAddress(IGF, addr, offsets);
      field.getTypeInfo().load(IGF, fieldAddr, out);
    }
  }

  void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &out) const {
    auto offsets = asImpl().getNonFixedOffsets(IGF);
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address fieldAddr = field.projectAddress(IGF, addr, offsets);
      field.getTypeInfo().loadAsTake(IGF, fieldAddr, out);
    }
  }
  
  void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {
    auto offsets = asImpl().getNonFixedOffsets(IGF);
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address fieldAddr = field.projectAddress(IGF, addr, offsets);
      field.getTypeInfo().assign(IGF, e, fieldAddr);
    }
  }

  void assignWithCopy(IRGenFunction &IGF, Address dest, Address src) const {
    auto offsets = asImpl().getNonFixedOffsets(IGF);
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address destField = field.projectAddress(IGF, dest, offsets);
      Address srcField = field.projectAddress(IGF, src, offsets);
      field.getTypeInfo().assignWithCopy(IGF, destField, srcField);
    }
  }

  void assignWithTake(IRGenFunction &IGF, Address dest, Address src) const {
    auto offsets = asImpl().getNonFixedOffsets(IGF);
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address destField = field.projectAddress(IGF, dest, offsets);
      Address srcField = field.projectAddress(IGF, src, offsets);
      field.getTypeInfo().assignWithTake(IGF, destField, srcField);
    }
  }

  void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {
    auto offsets = asImpl().getNonFixedOffsets(IGF);
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;
      
      Address fieldAddr = field.projectAddress(IGF, addr, offsets);
      field.getTypeInfo().initialize(IGF, e, fieldAddr);
    }
  }

  void initializeWithCopy(IRGenFunction &IGF, Address dest,
                          Address src) const {
    // If we're POD, use the generic routine.
    if (this->isPOD(ResilienceScope::Local) && Base::isFixedSize()) {
      return cast<FixedTypeInfo>(this)->
               FixedTypeInfo::initializeWithCopy(IGF, dest, src);
    }

    auto offsets = asImpl().getNonFixedOffsets(IGF);
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address destField = field.projectAddress(IGF, dest, offsets);
      Address srcField = field.projectAddress(IGF, src, offsets);
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

  void retain(IRGenFunction &IGF, Explosion &e) const {
    llvm_unreachable("not retainable");
  }

  void release(IRGenFunction &IGF, Explosion &e) const {
    llvm_unreachable("not releasable");
  }
  
  void destroy(IRGenFunction &IGF, Address addr) const {
    auto offsets = asImpl().getNonFixedOffsets(IGF);
    for (auto &field : getFields()) {
      if (field.isPOD()) continue;

      field.getTypeInfo().destroy(IGF, field.projectAddress(IGF, addr, offsets));
    }
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
template <class BuilderImpl, class FieldImpl, class ASTField>
class SequentialTypeBuilder {
protected:
  IRGenModule &IGM;
  SequentialTypeBuilder(IRGenModule &IGM) : IGM(IGM) {}

  BuilderImpl *asImpl() { return static_cast<BuilderImpl*>(this); }

public:
  /// Allocate and initialize a type info of the given type.
  template <class T, class... As>
  T *create(ArrayRef<FieldImpl> fields, As &&...args) {
    void *buffer =
      ::operator new(sizeof(T) + fields.size() * sizeof(FieldImpl));
    T *type = new(buffer) T(fields.size(), std::forward<As>(args)...);
    std::uninitialized_copy(fields.begin(), fields.end(), type->getFieldsBuffer());
    return type;
  }

  TypeInfo *layout(llvm::ArrayRef<ASTField> astFields) {
    SmallVector<FieldImpl, 8> fields;
    SmallVector<const TypeInfo *, 8> fieldTypesForLayout;
    fields.reserve(astFields.size());
    fieldTypesForLayout.reserve(astFields.size());

    unsigned maximalExplosionSize = 0, minimalExplosionSize = 0;
    for (auto &astField : astFields) {
      // Compute the field's type info.
      auto &fieldTI = IGM.getFragileTypeInfo(asImpl()->getType(astField));
      assert(fieldTI.isComplete());
      fieldTypesForLayout.push_back(&fieldTI);

      fields.push_back(FieldImpl(asImpl()->getFieldInfo(astField, fieldTI)));

      auto &fieldInfo = fields.back();
      fieldInfo.MaximalBegin = maximalExplosionSize;
      maximalExplosionSize += fieldTI.getExplosionSize(ExplosionKind::Maximal);
      fieldInfo.MaximalEnd = maximalExplosionSize;

      fieldInfo.MinimalBegin = minimalExplosionSize;
      minimalExplosionSize += fieldTI.getExplosionSize(ExplosionKind::Minimal);
      fieldInfo.MinimalEnd = minimalExplosionSize;
    }

    // Perform layout and fill in the fields.
    StructLayout layout = asImpl()->performLayout(fieldTypesForLayout);
    for (unsigned i = 0, e = fields.size(); i != e; ++i) {
      fields[i].completeFrom(layout.getElements()[i]);
    }

    // Create the type info.

    if (layout.isFixedLayout()) {
      auto seqTI = asImpl()->createFixed(fields, layout);
      seqTI->MaximalExplosionSize = maximalExplosionSize;
      seqTI->MinimalExplosionSize = minimalExplosionSize;
      return seqTI;
    }

    auto seqTI = asImpl()->createNonFixed(fields, layout);
    seqTI->MaximalExplosionSize = maximalExplosionSize;
    seqTI->MinimalExplosionSize = minimalExplosionSize;
    return seqTI;
  }  
};

} // end namespace irgen
} // end namespace swift

#endif
