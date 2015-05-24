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
#include "GenEnum.h"
#include "LoadableTypeInfo.h"
#include "TypeInfo.h"
#include "StructLayout.h"

namespace swift {
namespace irgen {

template <class, class, class> class SequentialTypeBuilder;

/// A field of a sequential type.
template <class FieldImpl> class SequentialField {
  ElementLayout Layout;

  template <class, class, class> friend class SequentialTypeBuilder;

  /// Begin/End - the range of explosion indexes for this element
  unsigned Begin : 16;
  unsigned End : 16;

protected:
  explicit SequentialField(const TypeInfo &elementTI)
    : Layout(ElementLayout::getIncomplete(elementTI)) {}

  explicit SequentialField(const ElementLayout &layout,
                           unsigned begin, unsigned end)
    : Layout(layout), Begin(begin), End(end) {}

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
  
  ElementLayout::Kind getKind() const {
    return Layout.getKind();
  }
  
  Size getFixedByteOffset() const {
    return Layout.getByteOffset();
  }

  std::pair<unsigned, unsigned> getProjectionRange() const {
    return {Begin, End};
  }
};

/// A metaprogrammed TypeInfo implementation for sequential types.
template <class Impl, class Base, class FieldImpl_,
          bool IsLoadable = std::is_base_of<LoadableTypeInfo, Base>::value>
class SequentialTypeInfoImpl : public Base {
public:
  typedef FieldImpl_ FieldImpl;

private:
  const unsigned NumFields;

  const FieldImpl *getFieldsBuffer() const {
    return reinterpret_cast<const FieldImpl*>(static_cast<const Impl*>(this)+1);
  }
  FieldImpl *getFieldsBuffer() {
    return reinterpret_cast<FieldImpl*>(static_cast<Impl*>(this)+1);
  }

protected:
  const Impl &asImpl() const { return *static_cast<const Impl*>(this); }

  template <class... As> 
  SequentialTypeInfoImpl(ArrayRef<FieldImpl> fields, As&&...args)
      : Base(std::forward<As>(args)...), NumFields(fields.size()) {
    std::uninitialized_copy(fields.begin(), fields.end(),
                            getFieldsBuffer());
  }

public:
  /// Allocate and initialize a type info of this type.
  template <class... As>
  static Impl *create(ArrayRef<FieldImpl> fields, As &&...args) {
    void *buffer =
      ::operator new(sizeof(Impl) + fields.size() * sizeof(FieldImpl));
    return new(buffer) Impl(fields, std::forward<As>(args)...);
  }

  ArrayRef<FieldImpl> getFields() const {
    return ArrayRef<FieldImpl>(getFieldsBuffer(), NumFields);
  }

  /// The standard schema is just all the fields jumbled together.
  void getSchema(ExplosionSchema &schema) const override {
    for (auto &field : getFields()) {
      field.getTypeInfo().getSchema(schema);
    }
  }

  void assignWithCopy(IRGenFunction &IGF, Address dest,
                      Address src, SILType T) const override {
    auto offsets = asImpl().getNonFixedOffsets(IGF, T);
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address destField = field.projectAddress(IGF, dest, offsets);
      Address srcField = field.projectAddress(IGF, src, offsets);
      field.getTypeInfo().assignWithCopy(IGF, destField, srcField,
                                         field.getType(IGF.IGM, T));
    }
  }

  void assignWithTake(IRGenFunction &IGF, Address dest,
                      Address src, SILType T) const override {
    auto offsets = asImpl().getNonFixedOffsets(IGF, T);
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address destField = field.projectAddress(IGF, dest, offsets);
      Address srcField = field.projectAddress(IGF, src, offsets);
      field.getTypeInfo().assignWithTake(IGF, destField, srcField,
                                         field.getType(IGF.IGM, T));
    }
  }

  void initializeWithCopy(IRGenFunction &IGF,
                          Address dest, Address src,
                          SILType T) const override {
    // If we're POD, use the generic routine.
    if (this->isPOD(ResilienceScope::Local) && isa<LoadableTypeInfo>(this)) {
      return cast<LoadableTypeInfo>(this)->
               LoadableTypeInfo::initializeWithCopy(IGF, dest, src, T);
    }

    auto offsets = asImpl().getNonFixedOffsets(IGF, T);
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address destField = field.projectAddress(IGF, dest, offsets);
      Address srcField = field.projectAddress(IGF, src, offsets);
      field.getTypeInfo().initializeWithCopy(IGF, destField, srcField,
                                             field.getType(IGF.IGM, T));
    }
  }
  
  void initializeWithTake(IRGenFunction &IGF,
                          Address dest, Address src,
                          SILType T) const override {
    // If we're bitwise-takable, use memcpy.
    if (this->isBitwiseTakable(ResilienceScope::Local)) {
      IGF.Builder.CreateMemCpy(dest.getAddress(), src.getAddress(),
                 asImpl().Impl::getSize(IGF, T),
                 std::min(dest.getAlignment(), src.getAlignment()).getValue());
      return;
    }
    
    auto offsets = asImpl().getNonFixedOffsets(IGF, T);
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;
      
      Address destField = field.projectAddress(IGF, dest, offsets);
      Address srcField = field.projectAddress(IGF, src, offsets);
      field.getTypeInfo().initializeWithTake(IGF, destField, srcField,
                                             field.getType(IGF.IGM, T));
    }
  }

  void destroy(IRGenFunction &IGF, Address addr, SILType T) const override {
    auto offsets = asImpl().getNonFixedOffsets(IGF, T);
    for (auto &field : getFields()) {
      if (field.isPOD()) continue;

      field.getTypeInfo().destroy(IGF, field.projectAddress(IGF, addr, offsets),
                                  field.getType(IGF.IGM, T));
    }
  }
};

template <class Impl, class Base, class FieldImpl_,
          bool IsLoadable = std::is_base_of<LoadableTypeInfo, Base>::value>
class SequentialTypeInfo;

/// An implementation of SequentialTypeInfo for non-loadable types. 
template <class Impl, class Base, class FieldImpl>
class SequentialTypeInfo<Impl, Base, FieldImpl, /*IsLoadable*/ false>
    : public SequentialTypeInfoImpl<Impl, Base, FieldImpl> {
  typedef SequentialTypeInfoImpl<Impl, Base, FieldImpl> super;
protected:
  template <class... As> 
  SequentialTypeInfo(As&&...args) : super(std::forward<As>(args)...) {}
};

/// An implementation of SequentialTypeInfo for loadable types. 
template <class Impl, class Base, class FieldImpl>
class SequentialTypeInfo<Impl, Base, FieldImpl, /*IsLoadable*/ true>
    : public SequentialTypeInfoImpl<Impl, Base, FieldImpl> {
  typedef SequentialTypeInfoImpl<Impl, Base, FieldImpl> super;

  unsigned ExplosionSize : 16;

protected:
  using super::asImpl;

  template <class... As> 
  SequentialTypeInfo(ArrayRef<FieldImpl> fields, unsigned explosionSize,
                     As &&...args)
    : super(fields, std::forward<As>(args)...),
      ExplosionSize(explosionSize) {}

private:
  template <void (LoadableTypeInfo::*Op)(IRGenFunction &IGF,
                                         Address addr,
                                         Explosion &out) const>
  void forAllFields(IRGenFunction &IGF, Address addr, Explosion &out) const {
    auto offsets = asImpl().getNonFixedOffsets(IGF);
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address fieldAddr = field.projectAddress(IGF, addr, offsets);
      (cast<LoadableTypeInfo>(field.getTypeInfo()).*Op)(IGF, fieldAddr, out);
    }
  }

  template <void (LoadableTypeInfo::*Op)(IRGenFunction &IGF,
                                         Explosion &in,
                                         Address addr) const>
  void forAllFields(IRGenFunction &IGF, Explosion &in, Address addr) const {
    auto offsets = asImpl().getNonFixedOffsets(IGF);
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address fieldAddr = field.projectAddress(IGF, addr, offsets);
      (cast<LoadableTypeInfo>(field.getTypeInfo()).*Op)(IGF, in, fieldAddr);
    }
  }

public:
  using super::getFields;

  void loadAsCopy(IRGenFunction &IGF, Address addr,
                  Explosion &out) const override {
    forAllFields<&LoadableTypeInfo::loadAsCopy>(IGF, addr, out);
  }

  void loadAsTake(IRGenFunction &IGF, Address addr,
                  Explosion &out) const override {
    forAllFields<&LoadableTypeInfo::loadAsTake>(IGF, addr, out);
  }
  
  void assign(IRGenFunction &IGF, Explosion &e, Address addr) const override {
    forAllFields<&LoadableTypeInfo::assign>(IGF, e, addr);
  }

  void initialize(IRGenFunction &IGF, Explosion &e,
                  Address addr) const override {
    forAllFields<&LoadableTypeInfo::initialize>(IGF, e, addr);
  }

  unsigned getExplosionSize() const override {
    return ExplosionSize;
  }

  void reexplode(IRGenFunction &IGF, Explosion &src,
                 Explosion &dest) const override {
    for (auto &field : getFields())
      cast<LoadableTypeInfo>(field.getTypeInfo()).reexplode(IGF, src, dest);
  }

  void copy(IRGenFunction &IGF, Explosion &src,
            Explosion &dest) const override {
    for (auto &field : getFields())
      cast<LoadableTypeInfo>(field.getTypeInfo()).copy(IGF, src, dest);
  }
      
  void consume(IRGenFunction &IGF, Explosion &src) const override {
    for (auto &field : getFields())
      cast<LoadableTypeInfo>(field.getTypeInfo()).consume(IGF, src);
  }

  void fixLifetime(IRGenFunction &IGF, Explosion &src) const override {
    for (auto &field : getFields())
      cast<LoadableTypeInfo>(field.getTypeInfo()).fixLifetime(IGF, src);
  }
  
  llvm::Value *packEnumPayload(IRGenFunction &IGF, Explosion &src,
                                unsigned bitWidth,
                                unsigned startOffset) const override {
    PackEnumPayload pack(IGF, bitWidth);
    for (auto &field : getFields()) {
      if (field.getKind() != ElementLayout::Kind::Empty) {
        unsigned offset = field.getFixedByteOffset().getValueInBits()
          + startOffset;
        llvm::Value *subValue = cast<LoadableTypeInfo>(field.getTypeInfo())
          .packEnumPayload(IGF, src, bitWidth, offset);
        pack.combine(subValue);
      }
    }
    return pack.get();
  }
  
  void unpackEnumPayload(IRGenFunction &IGF, llvm::Value *payload,
                          Explosion &dest, unsigned startOffset) const override{
    for (auto &field : getFields()) {
      if (field.getKind() != ElementLayout::Kind::Empty) {
        unsigned offset = field.getFixedByteOffset().getValueInBits()
          + startOffset;
        cast<LoadableTypeInfo>(field.getTypeInfo())
          .unpackEnumPayload(IGF, payload, dest, offset);
      }
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
  TypeInfo *layout(ArrayRef<ASTField> astFields) {
    SmallVector<FieldImpl, 8> fields;
    SmallVector<const TypeInfo *, 8> fieldTypesForLayout;
    fields.reserve(astFields.size());
    fieldTypesForLayout.reserve(astFields.size());

    bool loadable = true;

    unsigned explosionSize = 0;
    for (unsigned i : indices(astFields)) {
      auto &astField = astFields[i];
      // Compute the field's type info.
      auto &fieldTI = IGM.getTypeInfo(asImpl()->getType(astField));
      assert(fieldTI.isComplete());
      fieldTypesForLayout.push_back(&fieldTI);

      fields.push_back(FieldImpl(asImpl()->getFieldInfo(i, astField, fieldTI)));

      auto loadableFieldTI = dyn_cast<LoadableTypeInfo>(&fieldTI);
      if (!loadableFieldTI) {
        loadable = false;
        continue;
      }

      auto &fieldInfo = fields.back();
      fieldInfo.Begin = explosionSize;
      explosionSize += loadableFieldTI->getExplosionSize();
      fieldInfo.End = explosionSize;
    }

    // Perform layout and fill in the fields.
    StructLayout layout = asImpl()->performLayout(fieldTypesForLayout);
    for (unsigned i = 0, e = fields.size(); i != e; ++i) {
      fields[i].completeFrom(layout.getElements()[i]);
    }

    // Create the type info.
    if (loadable) {
      assert(layout.isFixedLayout());
      return asImpl()->createLoadable(fields, std::move(layout), explosionSize);
    } else if (layout.isFixedLayout()) {
      return asImpl()->createFixed(fields, std::move(layout));
    } else {
      return asImpl()->createNonFixed(fields, std::move(layout));
    }
  }  
};

} // end namespace irgen
} // end namespace swift

#endif
