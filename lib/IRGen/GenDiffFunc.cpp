//===- GenDiffFunc.cpp - Swift IR Generation For @differentiable Functions ===//
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
// SWIFT_ENABLE_TENSORFLOW

#include "swift/AST/Decl.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "llvm/IR/DerivedTypes.h"

#include "Explosion.h"
#include "GenHeap.h"
#include "GenRecord.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "IndirectTypeInfo.h"
#include "NonFixedTypeInfo.h"

#pragma clang diagnostic ignored "-Winconsistent-missing-override"

using namespace swift;
using namespace irgen;

using LegacyDiffFuncIndex =
    std::pair<AutoDiffFunctionExtractInst::Extractee, unsigned>;

namespace {
class LegacyDiffFuncFieldInfo final
    : public RecordField<LegacyDiffFuncFieldInfo> {
public:
  /// The field index.
  const LegacyDiffFuncIndex Index;

  /// The parameter indices.
  AutoDiffIndexSubset *ParameterIndices;

  LegacyDiffFuncFieldInfo(LegacyDiffFuncIndex index,
                          AutoDiffIndexSubset *ParameterIndices,
                          const TypeInfo &type)
      : RecordField(type), Index(index), ParameterIndices(ParameterIndices) {}

  std::string getFieldName() const {
    auto extractee = std::get<0>(Index);
    auto differentiationOrder = std::get<1>(Index);
    switch (extractee) {
    case AutoDiffFunctionExtractInst::Extractee::Original:
      return "original";
    case AutoDiffFunctionExtractInst::Extractee::JVP:
      return "jvp_" + llvm::itostr(differentiationOrder);
    case AutoDiffFunctionExtractInst::Extractee::VJP:
      return "vjp_" + llvm::itostr(differentiationOrder);
    }
  }

  SILType getType(IRGenModule &IGM, SILType t) const {
    auto fnTy = t.castTo<SILFunctionType>();
    auto origFnTy = fnTy->getWithoutDifferentiability();
    if (std::get<0>(Index) == AutoDiffFunctionExtractInst::Extractee::Original)
      return SILType::getPrimitiveObjectType(origFnTy);
    auto differentiationOrder = std::get<1>(Index);
    auto kind = *std::get<0>(Index).getExtracteeAsAssociatedFunction();
    auto assocTy = origFnTy->getAutoDiffAssociatedFunctionType(
        ParameterIndices, /*resultIndex*/ 0, differentiationOrder, kind,
        IGM.getSILModule(), LookUpConformanceInModule(IGM.getSwiftModule()));
    return SILType::getPrimitiveObjectType(assocTy);
  }
};

class LegacyDiffFuncTypeInfo final
    : public RecordTypeInfo<LegacyDiffFuncTypeInfo, LoadableTypeInfo,
                            LegacyDiffFuncFieldInfo> {
  using super =
      RecordTypeInfo<LegacyDiffFuncTypeInfo, LoadableTypeInfo,
                     LegacyDiffFuncFieldInfo>;

public:
  LegacyDiffFuncTypeInfo(ArrayRef<LegacyDiffFuncFieldInfo> fields,
                         unsigned explosionSize,
                         llvm::Type *ty, Size size, SpareBitVector &&spareBits,
                         Alignment align, IsPOD_t isPOD,
                         IsFixedSize_t alwaysFixedSize)
      : super(fields, explosionSize, ty, size, std::move(spareBits), align,
              isPOD, alwaysFixedSize) {}

  Address projectFieldAddress(IRGenFunction &IGF, Address addr, SILType T,
                              const LegacyDiffFuncFieldInfo &field) const {
    return field.projectAddress(IGF, addr, getNonFixedOffsets(IGF, T));
  }

  void initializeFromParams(IRGenFunction &IGF, Explosion &params, Address src,
                            SILType T, bool isOutlined) const override {
    llvm_unreachable("unexploded @differentiable function as argument?");
  }

  void addToAggLowering(IRGenModule &IGM, SwiftAggLowering &lowering,
                        Size offset) const override {
    for (auto &field : getFields()) {
      auto fieldOffset = offset + field.getFixedByteOffset();
      cast<LoadableTypeInfo>(field.getTypeInfo())
          .addToAggLowering(IGM, lowering, fieldOffset);
    }
  }

  llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF) const { return None; }
  llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF, SILType T) const {
    return None;
  }
};

class LegacyDiffFuncTypeBuilder
    : public RecordTypeBuilder<LegacyDiffFuncTypeBuilder,
                               LegacyDiffFuncFieldInfo,
                               LegacyDiffFuncIndex> {

  SILFunctionType *origFnTy;
  AutoDiffIndexSubset *parameterIndices;

public:
  LegacyDiffFuncTypeBuilder(IRGenModule &IGM, SILFunctionType *fnTy)
      : RecordTypeBuilder(IGM), origFnTy(fnTy->getWithoutDifferentiability()),
        parameterIndices(fnTy->getDifferentiationParameterIndices()) {
    assert(fnTy->isDifferentiable());
  }

  TypeInfo *createFixed(ArrayRef<LegacyDiffFuncFieldInfo> fields,
                        StructLayout &&layout) {
    llvm_unreachable("@differentiable functions are always loadable");
  }

  LegacyDiffFuncTypeInfo *createLoadable(
      ArrayRef<LegacyDiffFuncFieldInfo> fields, StructLayout &&layout,
      unsigned explosionSize) {
    return LegacyDiffFuncTypeInfo::create(
        fields, explosionSize, layout.getType(), layout.getSize(),
        std::move(layout.getSpareBits()), layout.getAlignment(), layout.isPOD(),
        layout.isAlwaysFixedSize());
  }

  TypeInfo *createNonFixed(ArrayRef<LegacyDiffFuncFieldInfo> fields,
                           FieldsAreABIAccessible_t fieldsAccessible,
                           StructLayout &&layout) {
    llvm_unreachable("@differentiable functions are always loadable");
  }

  LegacyDiffFuncFieldInfo getFieldInfo(
      unsigned index, LegacyDiffFuncIndex field, const TypeInfo &fieldTI) {
    return LegacyDiffFuncFieldInfo(field, parameterIndices, fieldTI);
  }

  SILType getType(LegacyDiffFuncIndex field) {
    if (std::get<0>(field) == AutoDiffFunctionExtractInst::Extractee::Original)
      return SILType::getPrimitiveObjectType(origFnTy->getCanonicalType());
    auto differentiationOrder = std::get<1>(field);
    auto kind = *std::get<0>(field).getExtracteeAsAssociatedFunction();
    auto assocTy = origFnTy->getAutoDiffAssociatedFunctionType(
        parameterIndices, /*resultIndex*/ 0, differentiationOrder, kind,
        IGM.getSILModule(), LookUpConformanceInModule(IGM.getSwiftModule()));
    return SILType::getPrimitiveObjectType(assocTy);
  }

  StructLayout performLayout(ArrayRef<const TypeInfo *> fieldTypes) {
    return StructLayout(IGM, /*decl=*/nullptr, LayoutKind::NonHeapObject,
                        LayoutStrategy::Universal, fieldTypes);
  }
};
} // end anonymous namespace

const TypeInfo *
TypeConverter::convertLegacyDifferentiableFunctionType(SILFunctionType *type) {
  assert(type->isDifferentiable());
  LegacyDiffFuncTypeBuilder builder(IGM, type);
  SmallVector<LegacyDiffFuncIndex, 3> fields;
  fields.push_back(
      std::make_pair(AutoDiffFunctionExtractInst::Extractee::Original, 0));
  fields.push_back(
      std::make_pair(AutoDiffFunctionExtractInst::Extractee::JVP, 1));
  fields.push_back(
      std::make_pair(AutoDiffFunctionExtractInst::Extractee::VJP, 1));
  return builder.layout(fields);
}

// New differnetiable function type.

namespace {
class DiffFuncFieldInfo final : public RecordField<DiffFuncFieldInfo> {
public:
  DiffFuncFieldInfo(unsigned index, const TypeInfo &type)
      : RecordField(type), Index(index) {}

  /// The field index.
  const unsigned Index;

  std::string getFieldName() const {
    if (Index == 0)
      return "original";
    return "assoc_" + llvm::utostr(Index);
  }

  SILType getType(IRGenModule &IGM, SILType t) const {
    auto diffFnTy = t.castTo<SILDifferentiableFunctionType>();
    if (Index == 0)
      return SILType::getPrimitiveObjectType(
          diffFnTy->getOriginalFunctionType());
    switch (diffFnTy->getRepresentationKind()) {
    case DifferentiabilityRepresentationKind::Normal:
      assert((int)Index <= diffFnTy->getMaxOrder());
      return SILType::getPrimitiveObjectType(
          diffFnTy->getAssociatedFunctionType(
              AutoDiffAssociatedFunctionKind::JVP, Index));
    case DifferentiabilityRepresentationKind::Linear:
      assert(Index == 1);
      return SILType::getPrimitiveObjectType(diffFnTy->getPullbackType());
    }
  }
};

class DiffFuncTypeInfo final
    : public RecordTypeInfo<DiffFuncTypeInfo, LoadableTypeInfo,
                            DiffFuncFieldInfo> {
  using super =
    RecordTypeInfo<DiffFuncTypeInfo, LoadableTypeInfo,
                   DiffFuncFieldInfo>;

public:
  DiffFuncTypeInfo(ArrayRef<DiffFuncFieldInfo> fields,
                   unsigned explosionSize,
                   llvm::Type *ty, Size size, SpareBitVector &&spareBits,
                   Alignment align, IsPOD_t isPOD,
                   IsFixedSize_t alwaysFixedSize)
    : super(fields, explosionSize, ty, size, std::move(spareBits), align,
            isPOD, alwaysFixedSize) {}

  Address projectFieldAddress(IRGenFunction &IGF, Address addr, SILType T,
                              const DiffFuncFieldInfo &field) const {
    return field.projectAddress(IGF, addr, getNonFixedOffsets(IGF, T));
  }

  void initializeFromParams(IRGenFunction &IGF, Explosion &params, Address src,
                            SILType T, bool isOutlined) const override {
    llvm_unreachable("unexploded @differentiable function as argument?");
  }

  void addToAggLowering(IRGenModule &IGM, SwiftAggLowering &lowering,
                        Size offset) const override {
    for (auto &field : getFields()) {
      auto fieldOffset = offset + field.getFixedByteOffset();
      cast<LoadableTypeInfo>(field.getTypeInfo())
          .addToAggLowering(IGM, lowering, fieldOffset);
    }
  }

  llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF) const { return None; }
  llvm::NoneType getNonFixedOffsets(IRGenFunction &IGF, SILType T) const {
    return None;
  }
};

class DiffFuncTypeBuilder
    : public RecordTypeBuilder<DiffFuncTypeBuilder, DiffFuncFieldInfo,
                               unsigned> {
  SILDifferentiableFunctionType *type;

  public:
  DiffFuncTypeBuilder(IRGenModule &IGM, SILDifferentiableFunctionType *diffFnTy)
      : RecordTypeBuilder(IGM), type(diffFnTy) {
  }

  TypeInfo *createFixed(ArrayRef<DiffFuncFieldInfo> fields,
                        StructLayout &&layout) {
    llvm_unreachable("@differentiable functions are always loadable");
  }

  DiffFuncTypeInfo *createLoadable(
    ArrayRef<DiffFuncFieldInfo> fields, StructLayout &&layout,
    unsigned explosionSize) {
    return DiffFuncTypeInfo::create(
      fields, explosionSize, layout.getType(), layout.getSize(),
      std::move(layout.getSpareBits()), layout.getAlignment(), layout.isPOD(),
      layout.isAlwaysFixedSize());
  }

  TypeInfo *createNonFixed(ArrayRef<DiffFuncFieldInfo> fields,
                           FieldsAreABIAccessible_t fieldsAccessible,
                           StructLayout &&layout) {
    llvm_unreachable("@differentiable functions are always loadable");
  }

  DiffFuncFieldInfo getFieldInfo(unsigned, unsigned index,
                                 const TypeInfo &fieldTI) {
    return DiffFuncFieldInfo(index, fieldTI);
  }

  SILType getType(unsigned index) {
    if (index == 0)
      return SILType::getPrimitiveObjectType(type->getOriginalFunctionType());
    switch (type->getRepresentationKind()) {
    case DifferentiabilityRepresentationKind::Normal:
      assert((int)index <= type->getMaxOrder());
      return SILType::getPrimitiveObjectType(
          type->getAssociatedFunctionType(
              AutoDiffAssociatedFunctionKind::JVP, index));
    case DifferentiabilityRepresentationKind::Linear:
      assert(index == 1);
      return SILType::getPrimitiveObjectType(type->getPullbackType());
    }
  }

  StructLayout performLayout(ArrayRef<const TypeInfo *> fieldTypes) {
    return StructLayout(IGM, /*decl=*/nullptr, LayoutKind::NonHeapObject,
                        LayoutStrategy::Universal, fieldTypes);
  }
};
} // end anonymous namespace

const TypeInfo *TypeConverter::convertDifferentiableFunctionType(
  SILDifferentiableFunctionType *type) {
  DiffFuncTypeBuilder builder(IGM, type);
  return builder.layout({0, 1});
}
