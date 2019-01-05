//===--- GenDiffFunc.cpp - Swift IR Generation For @autodiff Functions ---===//
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

using DiffFuncIndex =
    std::pair<AutoDiffFunctionExtractInst::Extractee, unsigned>;

namespace {
class DiffFuncFieldInfo final : public RecordField<DiffFuncFieldInfo> {
public:
  DiffFuncFieldInfo(DiffFuncIndex index, const TypeInfo &type)
      : RecordField(type), Index(index) {}

  /// The field index.
  const DiffFuncIndex Index;

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
    auto extInfo = fnTy->getExtInfo();
    auto nondiffExtInfo = extInfo.withDifferentiable(false);
    auto origFnTy = fnTy->getWithExtInfo(nondiffExtInfo);
    if (std::get<0>(Index) == AutoDiffFunctionExtractInst::Extractee::Original)
      return SILType::getPrimitiveObjectType(origFnTy);
    auto differentiationOrder = std::get<1>(Index);
    auto kind = *std::get<0>(Index).getExtracteeAsAssociatedFunction();
    auto assocTy = origFnTy->getAutoDiffAssociatedFunctionType(
        SmallBitVector(origFnTy->getNumParameters(), true), /*resultIndex*/ 0,
        differentiationOrder, kind, IGM.getSILModule(),
        LookUpConformanceInModule(IGM.getSwiftModule()));
    return SILType::getPrimitiveObjectType(assocTy);
  }
};

class DiffFuncTypeInfo final
    : public RecordTypeInfo<DiffFuncTypeInfo, LoadableTypeInfo,
                            DiffFuncFieldInfo> {
  using super =
      RecordTypeInfo<DiffFuncTypeInfo, LoadableTypeInfo, DiffFuncFieldInfo>;

public:
  DiffFuncTypeInfo(ArrayRef<DiffFuncFieldInfo> fields, unsigned explosionSize,
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
    llvm_unreachable("unexploded @autodiff function as argument?");
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
                               DiffFuncIndex> {

  SILFunctionType *origFnTy;

public:
  DiffFuncTypeBuilder(IRGenModule &IGM, SILFunctionType *fnTy)
      : RecordTypeBuilder(IGM) {
    assert(fnTy->isDifferentiable());
    auto extInfo = fnTy->getExtInfo();
    auto nondiffExtInfo = extInfo.withDifferentiable(false);
    origFnTy = fnTy->getWithExtInfo(nondiffExtInfo);
  }

  TypeInfo *createFixed(ArrayRef<DiffFuncFieldInfo> fields,
                        StructLayout &&layout) {
    llvm_unreachable("@autodiff functions are always loadable");
  }

  DiffFuncTypeInfo *createLoadable(ArrayRef<DiffFuncFieldInfo> fields,
                                   StructLayout &&layout,
                                   unsigned explosionSize) {
    return DiffFuncTypeInfo::create(
        fields, explosionSize, layout.getType(), layout.getSize(),
        std::move(layout.getSpareBits()), layout.getAlignment(), layout.isPOD(),
        layout.isAlwaysFixedSize());
  }

  TypeInfo *createNonFixed(ArrayRef<DiffFuncFieldInfo> fields,
                           FieldsAreABIAccessible_t fieldsAccessible,
                           StructLayout &&layout) {
    llvm_unreachable("@autodiff functions are always loadable");
  }

  DiffFuncFieldInfo getFieldInfo(unsigned index, DiffFuncIndex field,
                                 const TypeInfo &fieldTI) {
    return DiffFuncFieldInfo(field, fieldTI);
  }

  SILType getType(DiffFuncIndex field) {
    if (std::get<0>(field) == AutoDiffFunctionExtractInst::Extractee::Original)
      return SILType::getPrimitiveObjectType(origFnTy->getCanonicalType());
    auto differentiationOrder = std::get<1>(field);
    auto kind = *std::get<0>(field).getExtracteeAsAssociatedFunction();
    auto assocTy = origFnTy->getAutoDiffAssociatedFunctionType(
        SmallBitVector(origFnTy->getNumParameters(), true), /*resultIndex*/ 0,
        differentiationOrder, kind, IGM.getSILModule(),
        LookUpConformanceInModule(IGM.getSwiftModule()));
    return SILType::getPrimitiveObjectType(assocTy);
  }

  StructLayout performLayout(ArrayRef<const TypeInfo *> fieldTypes) {
    return StructLayout(IGM, /*decl=*/nullptr, LayoutKind::NonHeapObject,
                        LayoutStrategy::Universal, fieldTypes);
  }
};
} // end anonymous namespace

const TypeInfo *
TypeConverter::convertDifferentiableFunctionType(SILFunctionType *type) {
  assert(type->isDifferentiable());
  DiffFuncTypeBuilder builder(IGM, type);
  SmallVector<DiffFuncIndex, 3> fields;
  fields.push_back(
      std::make_pair(AutoDiffFunctionExtractInst::Extractee::Original, 0));
  fields.push_back(
      std::make_pair(AutoDiffFunctionExtractInst::Extractee::JVP, 1));
  fields.push_back(
      std::make_pair(AutoDiffFunctionExtractInst::Extractee::VJP, 1));
  return builder.layout(fields);
}
