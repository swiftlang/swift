//===--- GenRecord.h - IR generation for record types -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file provides some common code for emitting record types.
//  A record type is something like a tuple or a struct.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENRECORD_H
#define SWIFT_IRGEN_GENRECORD_H

#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"
#include "GenEnum.h"
#include "LoadableTypeInfo.h"
#include "TypeInfo.h"
#include "StructLayout.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {
namespace irgen {

template <class, class, class> class RecordTypeBuilder;

/// A field of a record type.
template <class FieldImpl> class RecordField {
  ElementLayout Layout;

  template <class, class, class> friend class RecordTypeBuilder;

  /// Begin/End - the range of explosion indexes for this element
  unsigned Begin : 16;
  unsigned End : 16;

protected:
  explicit RecordField(const TypeInfo &elementTI)
    : Layout(ElementLayout::getIncomplete(elementTI)) {}

  explicit RecordField(const ElementLayout &layout,
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

  unsigned getStructIndex() const { return Layout.getStructIndex(); }

  unsigned getNonFixedElementIndex() const {
    return Layout.getNonFixedElementIndex();
  }

  std::pair<unsigned, unsigned> getProjectionRange() const {
    return {Begin, End};
  }
};

/// A metaprogrammed TypeInfo implementation for record types.
template <class Impl, class Base, class FieldImpl_,
          bool IsLoadable = std::is_base_of<LoadableTypeInfo, Base>::value>
class RecordTypeInfoImpl : public Base,
    private llvm::TrailingObjects<Impl, FieldImpl_> {
  friend class llvm::TrailingObjects<Impl, FieldImpl_>;

public:
  typedef FieldImpl_ FieldImpl;

private:
  const unsigned NumFields;

protected:
  const Impl &asImpl() const { return *static_cast<const Impl*>(this); }

  template <class... As> 
  RecordTypeInfoImpl(ArrayRef<FieldImpl> fields, As&&...args)
      : Base(std::forward<As>(args)...), NumFields(fields.size()) {
    std::uninitialized_copy(fields.begin(), fields.end(),
                            this->template getTrailingObjects<FieldImpl>());
  }

public:
  /// Allocate and initialize a type info of this type.
  template <class... As>
  static Impl *create(ArrayRef<FieldImpl> fields, As &&...args) {
    size_t size = Impl::template totalSizeToAlloc<FieldImpl>(fields.size());
    void *buffer = ::operator new(size);
    return new(buffer) Impl(fields, std::forward<As>(args)...);
  }

  ArrayRef<FieldImpl> getFields() const {
    return {this->template getTrailingObjects<FieldImpl>(), NumFields};
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
    if (this->isPOD(ResilienceExpansion::Maximal) &&
        isa<LoadableTypeInfo>(this)) {
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
    if (this->isBitwiseTakable(ResilienceExpansion::Maximal)) {
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
          bool IsFixedSize = std::is_base_of<FixedTypeInfo, Base>::value,
          bool IsLoadable = std::is_base_of<LoadableTypeInfo, Base>::value>
class RecordTypeInfo;

/// An implementation of RecordTypeInfo for non-fixed-size types
/// (but not resilient ones where we don't know the complete set of
/// stored properties).
///
/// Override the buffer operations to just delegate to the unique
/// non-empty field, if there is one.
template <class Impl, class Base, class FieldImpl>
class RecordTypeInfo<Impl, Base, FieldImpl,
                     /*IsFixedSize*/ false, /*IsLoadable*/ false>
    : public RecordTypeInfoImpl<Impl, Base, FieldImpl> {
  typedef RecordTypeInfoImpl<Impl, Base, FieldImpl> super;

  /// The index+1 of the unique non-empty field, or zero if there is none.
  unsigned UniqueNonEmptyFieldIndexPlusOne;
protected:
  template <class... As>
  RecordTypeInfo(ArrayRef<FieldImpl> fields, As&&...args)
      : super(fields, std::forward<As>(args)...) {

    // Look for a unique non-empty field.
    UniqueNonEmptyFieldIndexPlusOne = findUniqueNonEmptyField(fields);
  }

public:
  using super::getStorageType;
  Address allocateBuffer(IRGenFunction &IGF, Address buffer,
                         SILType type) const override {
    if (auto field = getUniqueNonEmptyField()) {
      Address address =
        field->getTypeInfo().allocateBuffer(IGF, buffer,
                                            field->getType(IGF.IGM, type));
      return IGF.Builder.CreateElementBitCast(address, getStorageType());
    } else {
      return super::allocateBuffer(IGF, buffer, type);
    }
  }

  Address projectBuffer(IRGenFunction &IGF, Address buffer,
                        SILType type) const override {
    if (auto field = getUniqueNonEmptyField()) {
      Address address =
        field->getTypeInfo().projectBuffer(IGF, buffer,
                                           field->getType(IGF.IGM, type));
      return IGF.Builder.CreateElementBitCast(address, getStorageType());
    } else {
      return super::projectBuffer(IGF, buffer, type);
    }
  }

  void destroyBuffer(IRGenFunction &IGF, Address buffer,
                     SILType type) const override {
    if (auto field = getUniqueNonEmptyField()) {
      field->getTypeInfo().destroyBuffer(IGF, buffer,
                                         field->getType(IGF.IGM, type));
    } else {
      super::destroyBuffer(IGF, buffer, type);
    }
  }

  void deallocateBuffer(IRGenFunction &IGF, Address buffer,
                        SILType type) const override {
    if (auto field = getUniqueNonEmptyField()) {
      field->getTypeInfo().deallocateBuffer(IGF, buffer,
                                            field->getType(IGF.IGM, type));
    } else {
      super::deallocateBuffer(IGF, buffer, type);
    }
  }

  Address initializeBufferWithTake(IRGenFunction &IGF,
                                   Address destBuffer,
                                   Address srcAddr,
                                   SILType type) const override {
    if (auto field = getUniqueNonEmptyField()) {
      auto &fieldTI = field->getTypeInfo();
      Address srcFieldAddr =
        IGF.Builder.CreateElementBitCast(srcAddr, fieldTI.getStorageType());
      Address fieldResult =
        fieldTI.initializeBufferWithTake(IGF, destBuffer, srcFieldAddr,
                                         field->getType(IGF.IGM, type));
      return IGF.Builder.CreateElementBitCast(fieldResult, getStorageType());
    } else {
      return super::initializeBufferWithTake(IGF, destBuffer, srcAddr, type);
    }
  }

  Address initializeBufferWithCopy(IRGenFunction &IGF,
                                   Address destBuffer,
                                   Address srcAddr,
                                   SILType type) const override {
    if (auto field = getUniqueNonEmptyField()) {
      auto &fieldTI = field->getTypeInfo();
      Address srcFieldAddr =
        IGF.Builder.CreateElementBitCast(srcAddr, fieldTI.getStorageType());
      Address fieldResult =
        fieldTI.initializeBufferWithCopy(IGF, destBuffer, srcFieldAddr,
                                         field->getType(IGF.IGM, type));
      return IGF.Builder.CreateElementBitCast(fieldResult, getStorageType());
    } else {
      return super::initializeBufferWithCopy(IGF, destBuffer, srcAddr, type);
    }
  }

  Address initializeBufferWithTakeOfBuffer(IRGenFunction &IGF,
                                           Address destBuffer,
                                           Address srcBuffer,
                                           SILType type) const override {
    if (auto field = getUniqueNonEmptyField()) {
      auto &fieldTI = field->getTypeInfo();
      Address fieldResult =
        fieldTI.initializeBufferWithTakeOfBuffer(IGF, destBuffer, srcBuffer,
                                                 field->getType(IGF.IGM, type));
      return IGF.Builder.CreateElementBitCast(fieldResult, getStorageType());
    } else {
      return super::initializeBufferWithTakeOfBuffer(IGF, destBuffer,
                                                     srcBuffer, type);
    }
  }

  Address initializeBufferWithCopyOfBuffer(IRGenFunction &IGF,
                                           Address destBuffer,
                                           Address srcBuffer,
                                           SILType type) const override {
    if (auto field = getUniqueNonEmptyField()) {
      auto &fieldTI = field->getTypeInfo();
      Address fieldResult =
        fieldTI.initializeBufferWithCopyOfBuffer(IGF, destBuffer, srcBuffer,
                                                 field->getType(IGF.IGM, type));
      return IGF.Builder.CreateElementBitCast(fieldResult, getStorageType());
    } else {
      return super::initializeBufferWithCopyOfBuffer(IGF, destBuffer,
                                                     srcBuffer, type);
    }
  }

private:
  /// Scan the given field info
  static unsigned findUniqueNonEmptyField(ArrayRef<FieldImpl> fields) {
    unsigned result = 0;
    for (auto &field : fields) {
      // Ignore empty fields.
      if (field.isEmpty()) continue;

      // If we've already found an index, then there isn't a
      // unique non-empty field.
      if (result) return 0;

      result = (&field - fields.data()) + 1;
    }

    return result;
  }

  const FieldImpl *getUniqueNonEmptyField() const {
    if (UniqueNonEmptyFieldIndexPlusOne) {
      return &this->getFields()[UniqueNonEmptyFieldIndexPlusOne - 1];
    } else {
      return nullptr;
    }
  }
};

/// An implementation of RecordTypeInfo for non-loadable types. 
template <class Impl, class Base, class FieldImpl>
class RecordTypeInfo<Impl, Base, FieldImpl,
                     /*IsFixedSize*/ true, /*IsLoadable*/ false>
    : public RecordTypeInfoImpl<Impl, Base, FieldImpl> {
  typedef RecordTypeInfoImpl<Impl, Base, FieldImpl> super;
protected:
  template <class... As> 
  RecordTypeInfo(As&&...args) : super(std::forward<As>(args)...) {}
};

/// An implementation of RecordTypeInfo for loadable types. 
template <class Impl, class Base, class FieldImpl>
class RecordTypeInfo<Impl, Base, FieldImpl,
                     /*IsFixedSize*/ true, /*IsLoadable*/ true>
    : public RecordTypeInfoImpl<Impl, Base, FieldImpl> {
  typedef RecordTypeInfoImpl<Impl, Base, FieldImpl> super;

  unsigned ExplosionSize : 16;

protected:
  using super::asImpl;

  template <class... As> 
  RecordTypeInfo(ArrayRef<FieldImpl> fields, unsigned explosionSize,
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

  template <void (LoadableTypeInfo::*Op)(IRGenFunction &IGF, Address addr,
                                         Explosion &out, Atomicity atomicity) const>
  void forAllFields(IRGenFunction &IGF, Address addr, Explosion &out,
                    Atomicity atomicity) const {
    auto offsets = asImpl().getNonFixedOffsets(IGF);
    for (auto &field : getFields()) {
      if (field.isEmpty()) continue;

      Address fieldAddr = field.projectAddress(IGF, addr, offsets);
      (cast<LoadableTypeInfo>(field.getTypeInfo()).*Op)(IGF, fieldAddr, out,
                                                        atomicity);
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
            Explosion &dest, Atomicity atomicity) const override {
    for (auto &field : getFields())
      cast<LoadableTypeInfo>(field.getTypeInfo())
          .copy(IGF, src, dest, atomicity);
  }

  void consume(IRGenFunction &IGF, Explosion &src,
               Atomicity atomicity) const override {
    for (auto &field : getFields())
      cast<LoadableTypeInfo>(field.getTypeInfo())
          .consume(IGF, src, atomicity);
  }

  void fixLifetime(IRGenFunction &IGF, Explosion &src) const override {
    for (auto &field : getFields())
      cast<LoadableTypeInfo>(field.getTypeInfo()).fixLifetime(IGF, src);
  }
  
  void packIntoEnumPayload(IRGenFunction &IGF,
                           EnumPayload &payload,
                           Explosion &src,
                           unsigned startOffset) const override {
    for (auto &field : getFields()) {
      if (field.getKind() != ElementLayout::Kind::Empty) {
        unsigned offset = field.getFixedByteOffset().getValueInBits()
          + startOffset;
        cast<LoadableTypeInfo>(field.getTypeInfo())
          .packIntoEnumPayload(IGF, payload, src, offset);
      }
    }
  }
  
  void unpackFromEnumPayload(IRGenFunction &IGF, const EnumPayload &payload,
                             Explosion &dest, unsigned startOffset)
                            const override {
    for (auto &field : getFields()) {
      if (field.getKind() != ElementLayout::Kind::Empty) {
        unsigned offset = field.getFixedByteOffset().getValueInBits()
          + startOffset;
        cast<LoadableTypeInfo>(field.getTypeInfo())
          .unpackFromEnumPayload(IGF, payload, dest, offset);
      }
    }
  }
};

/// A builder of record types.
///
/// Required for a full implementation:
///   TypeInfoImpl *construct(void *buffer, ArrayRef<ASTField> fields);
///   FieldImpl getFieldInfo(const ASTField &field, const TypeInfo &fieldTI);
///   Type getType(const ASTField &field);
///   void performLayout(ArrayRef<const TypeInfo *> fieldTypes);
///     - should call recordLayout with the layout
template <class BuilderImpl, class FieldImpl, class ASTField>
class RecordTypeBuilder {
protected:
  IRGenModule &IGM;
  RecordTypeBuilder(IRGenModule &IGM) : IGM(IGM) {}

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
