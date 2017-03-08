//===--- ConstantBuilder.h - IR generation for constant structs -*- C++ -*-===//
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
//  This file implements IR generation of constant packed LLVM structs.
//===----------------------------------------------------------------------===//

#include "swift/AST/Mangle.h"
#include "swift/ABI/MetadataValues.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "clang/CodeGen/ConstantInitBuilder.h"

#include "Address.h"
#include "IRGenModule.h"
#include "IRGenFunction.h"

namespace swift {
namespace irgen {

class ConstantAggregateBuilderBase;
class ConstantStructBuilder;
class ConstantArrayBuilder;
class ConstantInitBuilder;

struct ConstantInitBuilderTraits {
  using InitBuilder = ConstantInitBuilder;
  using AggregateBuilderBase = ConstantAggregateBuilderBase;
  using ArrayBuilder = ConstantArrayBuilder;
  using StructBuilder = ConstantStructBuilder;
};

/// A Swift customization of Clang's ConstantInitBuilder.
class ConstantInitBuilder
    : public clang::CodeGen::ConstantInitBuilderTemplateBase<
                                                    ConstantInitBuilderTraits> {
public:
  IRGenModule &IGM;
  ConstantInitBuilder(IRGenModule &IGM)
    : ConstantInitBuilderTemplateBase(IGM.getClangCGM()),
      IGM(IGM) {}
};

class ConstantAggregateBuilderBase
       : public clang::CodeGen::ConstantAggregateBuilderBase {
  using super = clang::CodeGen::ConstantAggregateBuilderBase;
protected:
  ConstantAggregateBuilderBase(ConstantInitBuilder &builder,
                               ConstantAggregateBuilderBase *parent)
    : super(builder, parent) {}

  ConstantInitBuilder &getBuilder() const {
    return static_cast<ConstantInitBuilder&>(Builder);
  }
  IRGenModule &IGM() const { return getBuilder().IGM; }

public:
  void addInt16(uint16_t value) {
    addInt(IGM().Int16Ty, value);
  }

  void addInt32(uint32_t value) {
    addInt(IGM().Int32Ty, value);
  }

  void addRelativeAddressOrNull(llvm::Constant *target) {
    if (target) {
      addRelativeAddress(target);
    } else {
      addInt(IGM().RelativeAddressTy, 0);
    }
  }

  void addRelativeAddress(llvm::Constant *target) {
    addRelativeOffset(IGM().RelativeAddressTy, target);
  }

  /// Add a tagged relative reference to the given address.  The direct
  /// target must be defined within the current image, but it might be
  /// a "GOT-equivalent", i.e. a pointer to an external object; if so,
  /// set the low bit of the offset to indicate that this is true.
  void addRelativeAddress(ConstantReference reference) {
    addTaggedRelativeOffset(IGM().RelativeAddressTy,
                            reference.getValue(),
                            unsigned(reference.isIndirect()));
  }

  void addFarRelativeAddress(llvm::Constant *target) {
    addRelativeOffset(IGM().FarRelativeAddressTy, target);
  }

  void addFarRelativeAddress(ConstantReference reference) {
    addTaggedRelativeOffset(IGM().FarRelativeAddressTy,
                            reference.getValue(),
                            unsigned(reference.isIndirect()));
  }

  Size getNextOffsetFromGlobal() const {
    return Size(super::getNextOffsetFromGlobal().getQuantity());
  }
};

class ConstantArrayBuilder
    : public clang::CodeGen::ConstantArrayBuilderTemplateBase<
                                                    ConstantInitBuilderTraits> {
public:
  template <class... As>
  ConstantArrayBuilder(As &&... args)
    : ConstantArrayBuilderTemplateBase(std::forward<As>(args)...) {}
};

class ConstantStructBuilder
    : public clang::CodeGen::ConstantStructBuilderTemplateBase<
                                                    ConstantInitBuilderTraits> {
public:
  template <class... As>
  ConstantStructBuilder(As &&... args)
    : ConstantStructBuilderTemplateBase(std::forward<As>(args)...) {}
};

} // end namespace irgen
} // end namespace swift
