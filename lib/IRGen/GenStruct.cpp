//===--- GenStruct.cpp - Swift IR Generation For 'struct' Types -----------===//
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
//  This file implements IR generation for struct types.
//
//===----------------------------------------------------------------------===//

#include "GenStruct.h"

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/SILModule.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecordLayout.h"
#include "clang/CodeGen/SwiftCallingConv.h"

#include "GenMeta.h"
#include "GenRecord.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "IndirectTypeInfo.h"
#include "MemberAccessStrategy.h"
#include "NonFixedTypeInfo.h"
#include "ResilientTypeInfo.h"
#include "StructMetadataVisitor.h"
#include "MetadataLayout.h"

#pragma clang diagnostic ignored "-Winconsistent-missing-override"

using namespace swift;
using namespace irgen;

/// The kinds of TypeInfos implementing struct types.
enum class StructTypeInfoKind {
  LoadableStructTypeInfo,
  FixedStructTypeInfo,
  ClangRecordTypeInfo,
  NonFixedStructTypeInfo,
  ResilientStructTypeInfo
};

static StructTypeInfoKind getStructTypeInfoKind(const TypeInfo &type) {
  return (StructTypeInfoKind) type.getSubclassKind();
}

namespace {
  class StructFieldInfo : public RecordField<StructFieldInfo> {
  public:
    StructFieldInfo(VarDecl *field, const TypeInfo &type)
      : RecordField(type), Field(field) {}

    /// The field.
    VarDecl * const Field;

    StringRef getFieldName() const {
      return Field->getName().str();
    }
    
    SILType getType(IRGenModule &IGM, SILType T) const {
      return T.getFieldType(Field, IGM.getSILModule(),
                            IGM.getMaximalTypeExpansionContext());
    }
  };

  /// A field-info implementation for fields of Clang types.
  class ClangFieldInfo : public RecordField<ClangFieldInfo> {
  public:
    ClangFieldInfo(VarDecl *swiftField, const ElementLayout &layout,
                   unsigned explosionBegin, unsigned explosionEnd)
      : RecordField(layout, explosionBegin, explosionEnd),
        Field(swiftField) {}

    VarDecl * const Field;

    StringRef getFieldName() const {
      if (Field) return Field->getName().str();
      return "<unimported>";
    }

    SILType getType(IRGenModule &IGM, SILType T) const {
      if (Field)
        return T.getFieldType(Field, IGM.getSILModule(),
                              IGM.getMaximalTypeExpansionContext());

      // The Swift-field-less cases use opaque storage, which is
      // guaranteed to ignore the type passed to it.
      return {};
    }
  };

  /// A common base class for structs.
  template <class Impl, class Base, class FieldInfoType = StructFieldInfo>
  class StructTypeInfoBase :
     public RecordTypeInfo<Impl, Base, FieldInfoType> {
    using super = RecordTypeInfo<Impl, Base, FieldInfoType>;
  protected:
    template <class... As>
    StructTypeInfoBase(StructTypeInfoKind kind, As &&...args)
      : super(std::forward<As>(args)...) {
      super::setSubclassKind((unsigned) kind);
    }

    using super::asImpl;

  public:
    const FieldInfoType &getFieldInfo(VarDecl *field) const {
      // FIXME: cache the physical field index in the VarDecl.
      for (auto &fieldInfo : asImpl().getFields()) {
        if (fieldInfo.Field == field)
          return fieldInfo;
      }
      llvm_unreachable("field not in struct?");
    }

    /// Given a full struct explosion, project out a single field.
    void projectFieldFromExplosion(IRGenFunction &IGF,
                                   Explosion &in,
                                   VarDecl *field,
                                   Explosion &out) const {
      auto &fieldInfo = getFieldInfo(field);

      // If the field requires no storage, there's nothing to do.
      if (fieldInfo.isEmpty())
        return;
  
      // Otherwise, project from the base.
      auto fieldRange = fieldInfo.getProjectionRange();
      auto elements = in.getRange(fieldRange.first, fieldRange.second);
      out.add(elements);
    }
       
    /// Given the address of a struct value, project out the address of a
    /// single field.
    Address projectFieldAddress(IRGenFunction &IGF,
                                Address addr,
                                SILType T,
                                const FieldInfoType &field) const {
      return asImpl().projectFieldAddress(IGF, addr, T, field.Field);
    }

    /// Given the address of a struct value, project out the address of a
    /// single field.
    Address projectFieldAddress(IRGenFunction &IGF,
                                Address addr,
                                SILType T,
                                VarDecl *field) const {
      auto &fieldInfo = getFieldInfo(field);
      if (fieldInfo.isEmpty())
        return fieldInfo.getTypeInfo().getUndefAddress();

      auto offsets = asImpl().getNonFixedOffsets(IGF, T);
      return fieldInfo.projectAddress(IGF, addr, offsets);
    }

    /// Return the constant offset of a field as a Int32Ty, or nullptr if the
    /// field is not at a fixed offset.
    llvm::Constant *getConstantFieldOffset(IRGenModule &IGM,
                                           VarDecl *field) const {
      auto &fieldInfo = getFieldInfo(field);
      if (fieldInfo.hasFixedByteOffset()) {
        return llvm::ConstantInt::get(
            IGM.Int32Ty, fieldInfo.getFixedByteOffset().getValue());
      }
      return nullptr;
    }

    MemberAccessStrategy getFieldAccessStrategy(IRGenModule &IGM,
                                             SILType T, VarDecl *field) const {
      auto &fieldInfo = getFieldInfo(field);
      switch (fieldInfo.getKind()) {
      case ElementLayout::Kind::Fixed:
      case ElementLayout::Kind::Empty:
      case ElementLayout::Kind::EmptyTailAllocatedCType:
        return MemberAccessStrategy::getDirectFixed(
                                               fieldInfo.getFixedByteOffset());
      case ElementLayout::Kind::InitialNonFixedSize:
        return MemberAccessStrategy::getDirectFixed(Size(0));
      case ElementLayout::Kind::NonFixed:
        return asImpl().getNonFixedFieldAccessStrategy(IGM, T, fieldInfo);
      }
      llvm_unreachable("bad field layout kind");
    }

    unsigned getFieldIndex(IRGenModule &IGM, VarDecl *field) const {
      auto &fieldInfo = getFieldInfo(field);
      return fieldInfo.getStructIndex();
    }

    Optional<unsigned> getFieldIndexIfNotEmpty(IRGenModule &IGM,
                                               VarDecl *field) const {
      auto &fieldInfo = getFieldInfo(field);
      if (fieldInfo.isEmpty())
        return None;
      return fieldInfo.getStructIndex();
    }

    bool isSingleRetainablePointer(ResilienceExpansion expansion,
                                   ReferenceCounting *rc) const override {
      auto fields = asImpl().getFields();
      if (fields.size() != 1)
        return false;
      return fields[0].getTypeInfo().isSingleRetainablePointer(expansion, rc);
    }
    
    void verify(IRGenTypeVerifierFunction &IGF,
                llvm::Value *metadata,
                SILType structType) const override {
      // Check that constant field offsets we know match
      for (auto &field : asImpl().getFields()) {
        switch (field.getKind()) {
        case ElementLayout::Kind::Fixed: {
          // We know the offset at compile time. See whether there's also an
          // entry for this field in the field offset vector.
          class FindOffsetOfFieldOffsetVector
            : public StructMetadataScanner<FindOffsetOfFieldOffsetVector> {
          public:
            VarDecl *FieldToFind;
            Size AddressPoint = Size::invalid();
            Size FieldOffset = Size::invalid();

            FindOffsetOfFieldOffsetVector(IRGenModule &IGM, VarDecl *Field)
                : StructMetadataScanner<FindOffsetOfFieldOffsetVector>(
                      IGM, cast<StructDecl>(Field->getDeclContext())),
                  FieldToFind(Field) {}

            void noteAddressPoint() {
              AddressPoint = this->NextOffset;
            }

            void addFieldOffset(VarDecl *Field) {
              if (Field == FieldToFind) {
                FieldOffset = this->NextOffset;
              }
              StructMetadataScanner<
                  FindOffsetOfFieldOffsetVector>::addFieldOffset(Field);
            }
          };
          
          FindOffsetOfFieldOffsetVector scanner(IGF.IGM, field.Field);
          scanner.layout();
          
          if (scanner.FieldOffset == Size::invalid()
              || scanner.AddressPoint == Size::invalid())
            continue;
          
          // Load the offset from the field offset vector and ensure it matches
          // the compiler's idea of the offset.
          auto metadataBytes =
            IGF.Builder.CreateBitCast(metadata, IGF.IGM.Int8PtrTy);
          auto fieldOffsetPtr =
            IGF.Builder.CreateInBoundsGEP(metadataBytes,
                  IGF.IGM.getSize(scanner.FieldOffset - scanner.AddressPoint));
          fieldOffsetPtr =
            IGF.Builder.CreateBitCast(fieldOffsetPtr,
                                      IGF.IGM.Int32Ty->getPointerTo());
          llvm::Value *fieldOffset =
            IGF.Builder.CreateLoad(fieldOffsetPtr, Alignment(4));
          fieldOffset = IGF.Builder.CreateZExtOrBitCast(fieldOffset,
                                                        IGF.IGM.SizeTy);
          
          IGF.verifyValues(metadata, fieldOffset,
                       IGF.IGM.getSize(field.getFixedByteOffset()),
                       Twine("offset of struct field ") + field.getFieldName());
          break;
        }
        case ElementLayout::Kind::Empty:
        case ElementLayout::Kind::EmptyTailAllocatedCType:
        case ElementLayout::Kind::InitialNonFixedSize:
        case ElementLayout::Kind::NonFixed:
          continue;
        }
      }
    }
  };
  
  /// A type implementation for loadable record types imported from Clang.
  class ClangRecordTypeInfo final :
    public StructTypeInfoBase<ClangRecordTypeInfo, LoadableTypeInfo,
                              ClangFieldInfo> {
    const clang::RecordDecl *ClangDecl;
  public:
    ClangRecordTypeInfo(ArrayRef<ClangFieldInfo> fields,
                        unsigned explosionSize,
                        llvm::Type *storageType, Size size,
                        SpareBitVector &&spareBits, Alignment align,
                        const clang::RecordDecl *clangDecl)
      : StructTypeInfoBase(StructTypeInfoKind::ClangRecordTypeInfo,
                           fields, explosionSize,
                           storageType, size, std::move(spareBits),
                           align, IsPOD, IsFixedSize),
        ClangDecl(clangDecl)
    {
    }

    void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                              Address addr, SILType T,
                              bool isOutlined) const override {
      ClangRecordTypeInfo::initialize(IGF, params, addr, isOutlined);
    }

    void addToAggLowering(IRGenModule &IGM, SwiftAggLowering &lowering,
                          Size offset) const override {
      lowering.addTypedData(ClangDecl, offset.asCharUnits());
    }

    llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF) const {
      return None;
    }
    llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF, SILType T) const {
      return None;
    }
    MemberAccessStrategy
    getNonFixedFieldAccessStrategy(IRGenModule &IGM, SILType T,
                                   const ClangFieldInfo &field) const {
      llvm_unreachable("non-fixed field in Clang type?");
    }
  };

  /// A type implementation for loadable struct types.
  class LoadableStructTypeInfo final
      : public StructTypeInfoBase<LoadableStructTypeInfo, LoadableTypeInfo> {
  public:
    LoadableStructTypeInfo(ArrayRef<StructFieldInfo> fields,
                           unsigned explosionSize,
                           llvm::Type *storageType, Size size,
                           SpareBitVector &&spareBits,
                           Alignment align, IsPOD_t isPOD,
                           IsFixedSize_t alwaysFixedSize)
      : StructTypeInfoBase(StructTypeInfoKind::LoadableStructTypeInfo,
                           fields, explosionSize,
                           storageType, size, std::move(spareBits),
                           align, isPOD, alwaysFixedSize)
    {}

    void addToAggLowering(IRGenModule &IGM, SwiftAggLowering &lowering,
                          Size offset) const override {
      for (auto &field : getFields()) {
        auto fieldOffset = offset + field.getFixedByteOffset();
        cast<LoadableTypeInfo>(field.getTypeInfo())
          .addToAggLowering(IGM, lowering, fieldOffset);
      }
    }

    void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                              Address addr, SILType T,
                              bool isOutlined) const override {
      LoadableStructTypeInfo::initialize(IGF, params, addr, isOutlined);
    }
    llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF) const {
      return None;
    }
    llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF, SILType T) const {
      return None;
    }
    MemberAccessStrategy
    getNonFixedFieldAccessStrategy(IRGenModule &IGM, SILType T,
                                   const StructFieldInfo &field) const {
      llvm_unreachable("non-fixed field in loadable type?");
    }
  };

  /// A type implementation for non-loadable but fixed-size struct types.
  class FixedStructTypeInfo final
      : public StructTypeInfoBase<FixedStructTypeInfo,
                                  IndirectTypeInfo<FixedStructTypeInfo,
                                                   FixedTypeInfo>> {
  public:
    // FIXME: Spare bits between struct members.
    FixedStructTypeInfo(ArrayRef<StructFieldInfo> fields, llvm::Type *T,
                        Size size, SpareBitVector &&spareBits,
                        Alignment align, IsPOD_t isPOD, IsBitwiseTakable_t isBT,
                        IsFixedSize_t alwaysFixedSize)
      : StructTypeInfoBase(StructTypeInfoKind::FixedStructTypeInfo,
                           fields, T, size, std::move(spareBits), align,
                           isPOD, isBT, alwaysFixedSize)
    {}
    llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF) const {
      return None;
    }
    llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF, SILType T) const {
      return None;
    }
    MemberAccessStrategy
    getNonFixedFieldAccessStrategy(IRGenModule &IGM, SILType T,
                                   const StructFieldInfo &field) const {
      llvm_unreachable("non-fixed field in fixed struct?");
    }
  };
  
  /// Accessor for the non-fixed offsets of a struct type.
  class StructNonFixedOffsets : public NonFixedOffsetsImpl {
    SILType TheStruct;
  public:
    StructNonFixedOffsets(SILType type) : TheStruct(type) {
      assert(TheStruct.getStructOrBoundGenericStruct());
    }
    
    llvm::Value *getOffsetForIndex(IRGenFunction &IGF, unsigned index) override {
      // TODO: do this with StructMetadataLayout::getFieldOffset

      // Get the field offset vector from the struct metadata.
      llvm::Value *metadata = IGF.emitTypeMetadataRefForLayout(TheStruct);
      Address fieldVector = emitAddressOfFieldOffsetVector(IGF, metadata,
                                    TheStruct.getStructOrBoundGenericStruct());
      
      // Grab the indexed offset.
      fieldVector = IGF.Builder.CreateConstArrayGEP(fieldVector, index,
                                                    IGF.IGM.getPointerSize());
      return IGF.Builder.CreateLoad(fieldVector);
    }

    MemberAccessStrategy getFieldAccessStrategy(IRGenModule &IGM,
                                                unsigned nonFixedIndex) {
      auto start =
        IGM.getMetadataLayout(TheStruct.getStructOrBoundGenericStruct())
          .getFieldOffsetVectorOffset();

      // FIXME: Handle resilience
      auto indirectOffset = start.getStatic() +
        (IGM.getPointerSize() * nonFixedIndex);

      return MemberAccessStrategy::getIndirectFixed(indirectOffset,
                               MemberAccessStrategy::OffsetKind::Bytes_Word);
    }
  };

  /// A type implementation for non-fixed struct types.
  class NonFixedStructTypeInfo final
      : public StructTypeInfoBase<NonFixedStructTypeInfo,
                                  WitnessSizedTypeInfo<NonFixedStructTypeInfo>>
  {
  public:
    NonFixedStructTypeInfo(ArrayRef<StructFieldInfo> fields,
                           FieldsAreABIAccessible_t fieldsAccessible,
                           llvm::Type *T,
                           Alignment align,
                           IsPOD_t isPOD, IsBitwiseTakable_t isBT,
                           IsABIAccessible_t structAccessible)
      : StructTypeInfoBase(StructTypeInfoKind::NonFixedStructTypeInfo,
                           fields, fieldsAccessible,
                           T, align, isPOD, isBT, structAccessible) {
    }

    // We have an indirect schema.
    void getSchema(ExplosionSchema &s) const override {
      s.add(ExplosionSchema::Element::forAggregate(getStorageType(),
                                                   getBestKnownAlignment()));
    }

    StructNonFixedOffsets
    getNonFixedOffsets(IRGenFunction &IGF, SILType T) const {
      return StructNonFixedOffsets(T);
    }

    MemberAccessStrategy
    getNonFixedFieldAccessStrategy(IRGenModule &IGM, SILType T,
                                   const StructFieldInfo &field) const {
      return StructNonFixedOffsets(T).getFieldAccessStrategy(IGM,
                                              field.getNonFixedElementIndex());
    }

    llvm::Value *getEnumTagSinglePayload(IRGenFunction &IGF,
                                         llvm::Value *numEmptyCases,
                                         Address structAddr,
                                         SILType structType,
                                         bool isOutlined) const override {
      // If we're not emitting the value witness table's implementation,
      // just call that.
      if (!isOutlined) {
        return emitGetEnumTagSinglePayloadCall(IGF, structType, numEmptyCases,
                                               structAddr);
      }

      return emitGetEnumTagSinglePayloadGenericCall(IGF, structType, *this,
                                                    numEmptyCases, structAddr,
        [this,structType](IRGenFunction &IGF, Address structAddr,
                          llvm::Value *structNumXI) {
          return withExtraInhabitantProvidingField(IGF, structAddr, structType,
                                                   structNumXI, IGF.IGM.Int32Ty,
            [&](const FieldImpl &field, llvm::Value *numXI) -> llvm::Value* {
              Address fieldAddr = asImpl().projectFieldAddress(
                                           IGF, structAddr, structType, field);
              auto fieldTy = field.getType(IGF.IGM, structType);
              return field.getTypeInfo()
                          .getExtraInhabitantTagDynamic(IGF, fieldAddr, fieldTy,
                                                     numXI, /*outlined*/ false);
            });
        });
    }

    void storeEnumTagSinglePayload(IRGenFunction &IGF,
                                   llvm::Value *whichCase,
                                   llvm::Value *numEmptyCases,
                                   Address structAddr,
                                   SILType structType,
                                   bool isOutlined) const override {
      // If we're not emitting the value witness table's implementation,
      // just call that.
      if (!isOutlined) {
        return emitStoreEnumTagSinglePayloadCall(IGF, structType, whichCase,
                                                 numEmptyCases, structAddr);
      }

      emitStoreEnumTagSinglePayloadGenericCall(IGF, structType, *this,
                                               whichCase, numEmptyCases,
                                               structAddr,
        [this,structType](IRGenFunction &IGF, Address structAddr,
                          llvm::Value *tag, llvm::Value *structNumXI) {
          withExtraInhabitantProvidingField(IGF, structAddr, structType,
                                            structNumXI, IGF.IGM.VoidTy,
            [&](const FieldImpl &field, llvm::Value *numXI) -> llvm::Value* {
              Address fieldAddr = asImpl().projectFieldAddress(
                                           IGF, structAddr, structType, field);
              auto fieldTy = field.getType(IGF.IGM, structType);
              field.getTypeInfo()
                   .storeExtraInhabitantTagDynamic(IGF, tag, fieldAddr, fieldTy,
                                                   /*outlined*/ false);
              return nullptr;
            });
        });
    }
  };

  class StructTypeBuilder :
    public RecordTypeBuilder<StructTypeBuilder, StructFieldInfo, VarDecl*> {

    llvm::StructType *StructTy;
    CanType TheStruct;
  public:
    StructTypeBuilder(IRGenModule &IGM, llvm::StructType *structTy,
                      CanType type) :
      RecordTypeBuilder(IGM), StructTy(structTy), TheStruct(type) {
    }

    LoadableStructTypeInfo *createLoadable(ArrayRef<StructFieldInfo> fields,
                                           StructLayout &&layout,
                                           unsigned explosionSize) {
      return LoadableStructTypeInfo::create(fields,
                                            explosionSize,
                                            layout.getType(),
                                            layout.getSize(),
                                            std::move(layout.getSpareBits()),
                                            layout.getAlignment(),
                                            layout.isPOD(),
                                            layout.isAlwaysFixedSize());
    }

    FixedStructTypeInfo *createFixed(ArrayRef<StructFieldInfo> fields,
                                     StructLayout &&layout) {
      return FixedStructTypeInfo::create(fields, layout.getType(),
                                         layout.getSize(),
                                         std::move(layout.getSpareBits()),
                                         layout.getAlignment(),
                                         layout.isPOD(),
                                         layout.isBitwiseTakable(),
                                         layout.isAlwaysFixedSize());
    }

    NonFixedStructTypeInfo *createNonFixed(ArrayRef<StructFieldInfo> fields,
                                     FieldsAreABIAccessible_t fieldsAccessible,
                                           StructLayout &&layout) {
      auto structAccessible = IsABIAccessible_t(
        IGM.getSILModule().isTypeMetadataAccessible(TheStruct));
      return NonFixedStructTypeInfo::create(fields, fieldsAccessible,
                                            layout.getType(),
                                            layout.getAlignment(),
                                            layout.isPOD(),
                                            layout.isBitwiseTakable(),
                                            structAccessible);
    }

    StructFieldInfo getFieldInfo(unsigned index,
                                 VarDecl *field, const TypeInfo &fieldTI) {
      return StructFieldInfo(field, fieldTI);
    }

    SILType getType(VarDecl *field) {
      assert(field->getDeclContext() == TheStruct->getAnyNominal());
      auto silType = SILType::getPrimitiveAddressType(TheStruct);
      return silType.getFieldType(
          field, IGM.getSILModule(),
          IGM.getMaximalTypeExpansionContext());
    }

    StructLayout performLayout(ArrayRef<const TypeInfo *> fieldTypes) {
      return StructLayout(IGM, TheStruct->getAnyNominal(),
                          LayoutKind::NonHeapObject,
                          LayoutStrategy::Optimal, fieldTypes, StructTy);
    }
  };

/// A class for lowering Clang records.
class ClangRecordLowering {
  IRGenModule &IGM;
  StructDecl *SwiftDecl;
  SILType SwiftType;
  const clang::RecordDecl *ClangDecl;
  const clang::ASTContext &ClangContext;
  const clang::ASTRecordLayout &ClangLayout;
  const Size TotalStride;
  const Alignment TotalAlignment;
  SpareBitVector SpareBits;

  SmallVector<llvm::Type *, 8> LLVMFields;
  SmallVector<ClangFieldInfo, 8> FieldInfos;
  Size NextOffset = Size(0);
  unsigned NextExplosionIndex = 0;
public:
  ClangRecordLowering(IRGenModule &IGM, StructDecl *swiftDecl,
                      const clang::RecordDecl *clangDecl,
                      SILType swiftType)
    : IGM(IGM), SwiftDecl(swiftDecl), SwiftType(swiftType),
      ClangDecl(clangDecl), ClangContext(clangDecl->getASTContext()),
      ClangLayout(ClangContext.getASTRecordLayout(clangDecl)),
      TotalStride(Size(ClangLayout.getSize().getQuantity())),
      TotalAlignment(IGM.getCappedAlignment(
                                       Alignment(ClangLayout.getAlignment()))) {
  }

  void collectRecordFields() {
    if (ClangDecl->isUnion()) {
      collectUnionFields();
    } else {
      collectStructFields();
    }
  }

  const TypeInfo *createTypeInfo(llvm::StructType *llvmType) {
    llvmType->setBody(LLVMFields, /*packed*/ true);
    return ClangRecordTypeInfo::create(FieldInfos, NextExplosionIndex,
                                       llvmType, TotalStride,
                                       std::move(SpareBits), TotalAlignment,
                                       ClangDecl);
  }

private:
  /// Collect all the fields of a union.
  void collectUnionFields() {
    addOpaqueField(Size(0), TotalStride);
  }

  static bool isImportOfClangField(VarDecl *swiftField,
                                   const clang::FieldDecl *clangField) {
    assert(swiftField->hasClangNode());
    return (swiftField->getClangNode().castAsDecl() == clangField);
  }

  void collectStructFields() {
    auto cfi = ClangDecl->field_begin(), cfe = ClangDecl->field_end();
    auto swiftProperties = SwiftDecl->getStoredProperties();
    auto sfi = swiftProperties.begin(), sfe = swiftProperties.end();

    while (cfi != cfe) {
      const clang::FieldDecl *clangField = *cfi++;

      // Bitfields are currently never mapped, but that doesn't mean
      // we don't have to copy them.
      if (clangField->isBitField()) {
        // Collect all of the following bitfields.
        unsigned bitStart =
          ClangLayout.getFieldOffset(clangField->getFieldIndex());
        unsigned bitEnd = bitStart + clangField->getBitWidthValue(ClangContext);

        while (cfi != cfe && (*cfi)->isBitField()) {
          clangField = *cfi++;
          unsigned nextStart =
            ClangLayout.getFieldOffset(clangField->getFieldIndex());
          assert(nextStart >= bitEnd && "laying out bit-fields out of order?");

          // In a heuristic effort to reduce the number of weird-sized
          // fields, whenever we see a bitfield starting on a 32-bit
          // boundary, start a new storage unit.
          if (nextStart % 32 == 0) {
            addOpaqueBitField(bitStart, bitEnd);
            bitStart = nextStart;
          }

          bitEnd = nextStart + clangField->getBitWidthValue(ClangContext);
        }

        addOpaqueBitField(bitStart, bitEnd);
        continue;
      }

      VarDecl *swiftField;
      if (sfi != sfe) {
        swiftField = *sfi;
        if (isImportOfClangField(swiftField, clangField)) {
          ++sfi;
        } else {
          swiftField = nullptr;
        }
      } else {
        swiftField = nullptr;
      }

      // Try to position this field.  If this fails, it's because we
      // didn't lay out padding correctly.
      addStructField(clangField, swiftField);
    }

    assert(sfi == sfe && "more Swift fields than there were Clang fields?");

    // We never take advantage of tail padding, because that would prevent
    // us from passing the address of the object off to C, which is a pretty
    // likely scenario for imported C types.
    assert(NextOffset <= TotalStride);
    assert(SpareBits.size() <= TotalStride.getValueInBits());
    if (NextOffset < TotalStride) {
      addPaddingField(TotalStride);
    }
  }

  /// Place the next struct field at its appropriate offset.
  void addStructField(const clang::FieldDecl *clangField,
                      VarDecl *swiftField) {
    unsigned fieldOffset = ClangLayout.getFieldOffset(clangField->getFieldIndex());
    assert(!clangField->isBitField());
    Size offset(fieldOffset / 8);

    // If we have a Swift import of this type, use our lowered information.
    if (swiftField) {
      auto &fieldTI = cast<LoadableTypeInfo>(IGM.getTypeInfo(
          SwiftType.getFieldType(swiftField, IGM.getSILModule(),
                                 IGM.getMaximalTypeExpansionContext())));
      addField(swiftField, offset, fieldTI);
      return;
    }

    // Otherwise, add it as an opaque blob.
    auto fieldSize = ClangContext.getTypeSizeInChars(clangField->getType());
    return addOpaqueField(offset, Size(fieldSize.getQuantity()));
  }

  /// Add opaque storage for bitfields spanning the given range of bits.
  void addOpaqueBitField(unsigned bitBegin, unsigned bitEnd) {
    assert(bitBegin <= bitEnd);

    // No need to add storage for zero-width bitfields.
    if (bitBegin == bitEnd) return;

    // Round up to an even number of bytes.
    assert(bitBegin % 8 == 0);
    Size offset = Size(bitBegin / 8);
    Size byteLength = Size((bitEnd - bitBegin + 7) / 8);

    addOpaqueField(offset, byteLength);
  }

  /// Add opaque storage at the given offset.
  void addOpaqueField(Size offset, Size fieldSize) {
    // No need to add storage for zero-size fields (e.g. incomplete array
    // decls).
    if (fieldSize.isZero()) return;

    auto &opaqueTI = IGM.getOpaqueStorageTypeInfo(fieldSize, Alignment(1));
    addField(nullptr, offset, opaqueTI);
  }

  /// Add storage for an (optional) Swift field at the given offset.
  void addField(VarDecl *swiftField, Size offset,
                const LoadableTypeInfo &fieldType) {
    assert(offset >= NextOffset && "adding fields out of order");

    // Add a padding field if required.
    if (offset != NextOffset)
      addPaddingField(offset);

    addFieldInfo(swiftField, fieldType);
  }

  /// Add information to track a value field at the current offset.
  void addFieldInfo(VarDecl *swiftField, const LoadableTypeInfo &fieldType) {
    unsigned explosionSize = fieldType.getExplosionSize();
    unsigned explosionBegin = NextExplosionIndex;
    NextExplosionIndex += explosionSize;
    unsigned explosionEnd = NextExplosionIndex;

    ElementLayout layout = ElementLayout::getIncomplete(fieldType);
    auto isEmpty = fieldType.isKnownEmpty(ResilienceExpansion::Maximal);
    if (isEmpty)
      layout.completeEmptyTailAllocatedCType(
          fieldType.isPOD(ResilienceExpansion::Maximal), NextOffset);
    else
      layout.completeFixed(fieldType.isPOD(ResilienceExpansion::Maximal),
                           NextOffset, LLVMFields.size());

    FieldInfos.push_back(
           ClangFieldInfo(swiftField, layout, explosionBegin, explosionEnd));
    
    if (!isEmpty) {
      LLVMFields.push_back(fieldType.getStorageType());
      NextOffset += fieldType.getFixedSize();
      SpareBits.append(fieldType.getSpareBits());
    }
  }

  /// Add padding to get up to the given offset.
  void addPaddingField(Size offset) {
    assert(offset > NextOffset);
    Size count = offset - NextOffset;
    LLVMFields.push_back(llvm::ArrayType::get(IGM.Int8Ty, count.getValue()));
    NextOffset = offset;
    SpareBits.appendSetBits(count.getValueInBits());
  }
};

} // end anonymous namespace

/// A convenient macro for delegating an operation to all of the
/// various struct implementations.
#define FOR_STRUCT_IMPL(IGF, type, op, ...) do {                       \
  auto &structTI = IGF.getTypeInfo(type);                              \
  switch (getStructTypeInfoKind(structTI)) {                           \
  case StructTypeInfoKind::ClangRecordTypeInfo:                        \
    return structTI.as<ClangRecordTypeInfo>().op(IGF, __VA_ARGS__);    \
  case StructTypeInfoKind::LoadableStructTypeInfo:                     \
    return structTI.as<LoadableStructTypeInfo>().op(IGF, __VA_ARGS__); \
  case StructTypeInfoKind::FixedStructTypeInfo:                        \
    return structTI.as<FixedStructTypeInfo>().op(IGF, __VA_ARGS__);    \
  case StructTypeInfoKind::NonFixedStructTypeInfo:                     \
    return structTI.as<NonFixedStructTypeInfo>().op(IGF, __VA_ARGS__); \
  case StructTypeInfoKind::ResilientStructTypeInfo:                    \
    llvm_unreachable("resilient structs are opaque");                  \
  }                                                                    \
  llvm_unreachable("bad struct type info kind!");                      \
} while (0)

Address irgen::projectPhysicalStructMemberAddress(IRGenFunction &IGF,
                                                  Address base,
                                                  SILType baseType,
                                                  VarDecl *field) {
  FOR_STRUCT_IMPL(IGF, baseType, projectFieldAddress, base,
                  baseType, field);
}

void irgen::projectPhysicalStructMemberFromExplosion(IRGenFunction &IGF,
                                                     SILType baseType,
                                                     Explosion &base,
                                                     VarDecl *field,
                                                     Explosion &out) {
  FOR_STRUCT_IMPL(IGF, baseType, projectFieldFromExplosion, base, field, out);
}

llvm::Constant *irgen::emitPhysicalStructMemberFixedOffset(IRGenModule &IGM,
                                                           SILType baseType,
                                                           VarDecl *field) {
  FOR_STRUCT_IMPL(IGM, baseType, getConstantFieldOffset, field);
}

MemberAccessStrategy
irgen::getPhysicalStructMemberAccessStrategy(IRGenModule &IGM,
                                             SILType baseType, VarDecl *field) {
  FOR_STRUCT_IMPL(IGM, baseType, getFieldAccessStrategy, baseType, field);
}

Optional<unsigned> irgen::getPhysicalStructFieldIndex(IRGenModule &IGM,
                                                      SILType baseType,
                                                      VarDecl *field) {
  FOR_STRUCT_IMPL(IGM, baseType, getFieldIndexIfNotEmpty, field);
}

void IRGenModule::emitStructDecl(StructDecl *st) {
  if (!IRGen.hasLazyMetadata(st)) {
    emitStructMetadata(*this, st);
    emitFieldDescriptor(st);
  }

  emitNestedTypeDecls(st->getMembers());
}

void IRGenModule::maybeEmitOpaqueTypeDecl(OpaqueTypeDecl *opaque) {
  if (IRGen.Opts.EnableAnonymousContextMangledNames) {
    // If we're emitting anonymous context mangled names for debuggability,
    // then emit all opaque type descriptors and make them runtime-discoverable
    // so that remote ast/mirror can recover them.
    addRuntimeResolvableType(opaque);
    if (IRGen.hasLazyMetadata(opaque))
      IRGen.noteUseOfOpaqueTypeDescriptor(opaque);
    else
      emitOpaqueTypeDecl(opaque);
  } else if (!IRGen.hasLazyMetadata(opaque)) {
    emitOpaqueTypeDecl(opaque);
  }
}

namespace {
  /// A type implementation for resilient struct types. This is not a
  /// StructTypeInfoBase at all, since we don't know anything about
  /// the struct's fields.
  class ResilientStructTypeInfo
      : public ResilientTypeInfo<ResilientStructTypeInfo>
  {
  public:
    ResilientStructTypeInfo(llvm::Type *T, IsABIAccessible_t abiAccessible)
      : ResilientTypeInfo(T, abiAccessible) {
      setSubclassKind((unsigned) StructTypeInfoKind::ResilientStructTypeInfo);
    }
  };
} // end anonymous namespace

const TypeInfo *
TypeConverter::convertResilientStruct(IsABIAccessible_t abiAccessible) {
  llvm::Type *storageType = IGM.OpaquePtrTy->getElementType();
  return new ResilientStructTypeInfo(storageType, abiAccessible);
}

const TypeInfo *TypeConverter::convertStructType(TypeBase *key, CanType type,
                                                 StructDecl *D){
  // All resilient structs have the same opaque lowering, since they are
  // indistinguishable as values --- except that we have to track
  // ABI-accessibility.
  if (IGM.isResilient(D, ResilienceExpansion::Maximal)) {
    auto structAccessible =
      IsABIAccessible_t(IGM.getSILModule().isTypeMetadataAccessible(type));
    return &getResilientStructTypeInfo(structAccessible);
  }

  // Create the struct type.
  auto ty = IGM.createNominalType(type);

  // Register a forward declaration before we look at any of the child types.
  addForwardDecl(key);

  // Use different rules for types imported from C.
  if (D->hasClangNode()) {
    const clang::Decl *clangDecl = D->getClangNode().getAsDecl();
    assert(clangDecl && "Swift struct from an imported C macro?");

    if (auto clangRecord = dyn_cast<clang::RecordDecl>(clangDecl)) {
      ClangRecordLowering lowering(IGM, D, clangRecord,
                                   SILType::getPrimitiveObjectType(type));
      lowering.collectRecordFields();
      return lowering.createTypeInfo(ty);

    } else if (isa<clang::EnumDecl>(clangDecl)) {
      // Fall back to Swift lowering for the enum's representation as a struct.
      assert(D->getStoredProperties().size() == 1 &&
             "Struct representation of a Clang enum should wrap one value");
    } else if (clangDecl->hasAttr<clang::SwiftNewtypeAttr>()) {
      // Fall back to Swift lowering for the underlying type's
      // representation as a struct member.
      assert(D->getStoredProperties().size() == 1 &&
             "Struct representation of a swift_newtype should wrap one value");
    } else {
      llvm_unreachable("Swift struct represents unexpected imported type");
    }
  }

  // Collect all the fields from the type.
  SmallVector<VarDecl*, 8> fields;
  for (VarDecl *VD : D->getStoredProperties())
    fields.push_back(VD);

  // Build the type.
  StructTypeBuilder builder(IGM, ty, type);
  return builder.layout(fields);
}
